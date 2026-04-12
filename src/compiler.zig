const std = @import("std");
const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const virtual_machine = @import("vm.zig");
const logging = @import("logging.zig");
const memory = @import("memory.zig");
const constants = @import("constants.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "Compiler",
    });
}

const Precedence = enum(u8) {
    Lowest = 1,
    Assign,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equals,
    LessGreater,
    Shift,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
};

const FunctionType = enum {
    Function,
    Script,
};

pub const Compiler = struct {
    gpa: std.mem.Allocator,
    vm: *virtual_machine.VirtualMachine,
    locals: [UINT8_COUNT]Local,
    local_count: u32,
    scope_depth: u32,
    function: ?*object.ObjFunction,
    type: FunctionType,
    enclosing: ?*Compiler,
    upvalues: [UINT8_COUNT]Upvalue,
    indent: usize,
    ctx: struct { suppress_pop: bool },

    const Local = struct {
        name: []const u8,
        depth: ?u32,
        is_captured: bool,
    };

    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    const UINT8_COUNT = std.math.maxInt(u8) + 1;

    pub fn init(
        self: *Compiler,
        vm: *virtual_machine.VirtualMachine,
        gpa: std.mem.Allocator,
        func_type: FunctionType,
        enclosing: ?*Compiler,
        indent: usize,
        name: ?[]const u8,
    ) !void {
        self.gpa = gpa;
        self.vm = vm;
        self.locals = undefined;
        self.scope_depth = 0;
        self.local_count = 0;
        self.type = func_type;
        self.enclosing = enclosing;
        self.upvalues = undefined;
        self.indent = indent;
        self.function = null;
        self.ctx = .{ .suppress_pop = false };

        // Set current_compiler BEFORE any allocations that could trigger GC
        vm.current_compiler = self;

        self.function = try object.allocateFunction(vm);

        if (name) |func_name| {
            const obj = try object.copyString(vm, func_name);
            self.function.?.name = obj.asString();
        }

        const local = &self.locals[self.local_count];
        self.local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        local.name = "";
    }

    fn getFunctionName(self: *Compiler) []const u8 {
        return if (self.function.?.name) |name| name.chars else "script";
    }

    fn log(self: *Compiler, comptime format: []const u8, args: anytype) void {
        if (!constants.debug_print_steps) return;
        const name = self.getFunctionName();
        logging.log(
            format,
            args,
            .{ .indent = self.indent, .module = name },
        );
    }

    pub fn compile(self: *Compiler, program: ast.Program) !*object.ObjFunction {
        for (program.items) |stmt| {
            self.compileStatement(stmt) catch |err| {
                self.currentChunk().disassemble("<error>");
                return err;
            };
        }
        return try self.endCompiler();
    }

    fn endCompiler(self: *Compiler) !*object.ObjFunction {
        try self.emitReturn();
        self.vm.current_compiler = self.enclosing;
        const function = self.function;
        if (comptime constants.debug_disassemble) {
            const name = self.getFunctionName();
            logDebug("Finished compiling {s}", .{name});
            self.currentChunk().disassemble(name);
        }
        return function.?;
    }

    fn emitReturn(self: *Compiler) !void {
        try self.emitOpCode(.Null);
        try self.emitOpCode(.Return);
    }

    fn defineVariable(self: *Compiler, global: usize) !void {
        if (self.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        try self.emitOpCode(.DefineGlobal);
        try self.emitU24(global);
    }

    fn markInitialized(self: *Compiler) void {
        if (self.scope_depth == 0) return;
        self.locals[self.local_count - 1].depth = self.scope_depth;
    }

    fn getVariable(self: *Compiler, name: []const u8) !usize {
        try self.declareVariable(name);
        if (self.scope_depth > 0) return 0;
        return try self.identifierConstant(name);
    }

    fn declareVariable(self: *Compiler, name: []const u8) !void {
        if (self.scope_depth == 0) return;

        if (self.local_count > 0) {
            var i = self.local_count;
            while (i > 0) : (i -= 1) {
                const local = self.locals[i - 1];
                if (local.depth != null and local.depth.? < self.scope_depth) {
                    break;
                }
                if (Compiler.identifiersEqual(name, local.name)) {
                    return errorAt("Already a variable with this name in this scope.");
                }
            }
        }

        try self.addLocal(name);
    }

    fn addLocal(self: *Compiler, name: []const u8) !void {
        if (self.local_count == std.math.maxInt(u8) + 1) {
            return errorAt("Too many local variables in block.");
        }
        const local = &self.locals[self.local_count];
        self.local_count += 1;
        local.name = name;
        local.depth = null;
        local.is_captured = false;
    }

    fn identifierConstant(self: *Compiler, name: []const u8) !usize {
        const obj = try object.copyString(self.vm, name);
        return try self.makeConstant(value.wrapObj(obj));
    }

    fn emitLoop(self: *Compiler, loop_start: usize) !void {
        try self.emitOpCode(.Loop);

        const offset = self.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) {
            return errorAt("Loop body too large.");
        }

        try self.emitU16(offset);
    }

    fn emitJump(self: *Compiler, instruction: chunk.OpCode) !usize {
        try self.emitOpCode(instruction);
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return self.currentChunk().code.items.len - 2;
    }

    fn patchJump(self: *Compiler, offset: usize) !void {
        const cur_chunk = self.currentChunk();
        const jump = cur_chunk.code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            return errorAt("Too much code to jump over.");
        }
        const bytes = chunk.indexToU16(jump);
        cur_chunk.code.items[offset] = bytes[0];
        cur_chunk.code.items[offset + 1] = bytes[1];
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) !u8 {
        const upvalue_count = self.function.?.upvalue_count;

        for (0..upvalue_count) |i| {
            const upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(i);
            }
        }

        if (upvalue_count == UINT8_COUNT) {
            return errorAt("Too many closure variables in function.");
        }

        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        const retVal = self.function.?.upvalue_count;
        self.function.?.upvalue_count += 1;
        return retVal;
    }

    fn resolveUpvalue(self: *Compiler, name: []const u8) !?u8 {
        if (self.enclosing == null) return null;

        const enclosing = self.enclosing.?;
        const local = try enclosing.resolveLocal(name);
        if (local) |index| {
            enclosing.locals[index].is_captured = true;
            return try self.addUpvalue(@intCast(index), true);
        }

        const upvalue = try self.enclosing.?.resolveUpvalue(name);
        if (upvalue) |index| return try self.addUpvalue(index, false);

        return null;
    }

    fn blockStatement(self: *Compiler) anyerror!void {
        self.log("block statement", .{});
        defer self.log("end block statement", .{});

        while (!self.check(.RBrace) and !self.check(.Eof)) {
            try self.declaration();
        }
        try self.consume(.RBrace, "Expect '}' after block.");
        try self.consume(.NewLine, "Expect '\n' after block.");
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) !void {
        self.scope_depth -= 1;
        while (self.local_count > 0 and
            self.locals[self.local_count - 1].depth != null and
            self.locals[self.local_count - 1].depth.? > self.scope_depth)
        {
            if (self.locals[self.local_count - 1].is_captured) {
                try self.emitOpCode(.CloseUpvalue);
            } else {
                try self.emitOpCode(.Pop);
            }
            self.local_count -= 1;
        }
    }

    fn resolveLocal(self: *Compiler, name: []const u8) !?usize {
        if (self.local_count == 0) return null;
        var i = self.local_count;
        while (i > 0) : (i -= 1) {
            const local = &self.locals[i - 1];
            if (Compiler.identifiersEqual(name, local.name)) {
                if (local.depth == null) {
                    return errorAt("Can't read local variable in its own initializer.");
                }
                return i - 1;
            }
        }
        return null;
    }

    fn identifiersEqual(a: []const u8, b: []const u8) bool {
        if (a.len != b.len) return false;
        return std.mem.eql(u8, a, b);
    }

    fn emitConstant(self: *Compiler, val: value.Value) !void {
        try self.currentChunk().writeConstant(self.gpa, val, 0);
    }

    fn makeConstant(self: *Compiler, val: value.Value) !usize {
        return try self.currentChunk().addConstant(self.gpa, val);
    }

    fn currentChunk(self: *Compiler) *chunk.Chunk {
        return &self.function.?.chunk;
    }

    fn emitBytes(self: *Compiler, first: chunk.OpCode, second: u8) !void {
        try self.emitOpCode(first);
        try self.emitByte(second);
    }

    fn emitOpByte(self: *Compiler, op_byte: chunk.OpByte) !void {
        try self.currentChunk().write(self.gpa, op_byte, 0);
    }

    fn emitByte(self: *Compiler, byte: u8) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Byte = byte }, 0);
    }

    fn emitU24(self: *Compiler, index: usize) !void {
        const bytes = chunk.indexToU24(index);
        try self.emitByte(bytes[0]);
        try self.emitByte(bytes[1]);
        try self.emitByte(bytes[2]);
    }

    fn emitU16(self: *Compiler, index: usize) !void {
        const bytes = chunk.indexToU16(index);
        try self.emitByte(bytes[0]);
        try self.emitByte(bytes[1]);
    }

    fn emitOpCode(self: *Compiler, op_code: chunk.OpCode) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = op_code }, 0);
    }

    fn emitOpCodes(self: *Compiler, a: chunk.OpCode, b: chunk.OpCode) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = a }, 0);
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = b }, 0);
    }

    fn errorAt(message: []const u8) anyerror {
        // std.debug.print("[line {d}] Error", .{line});
        // std.debug.print(" at '{s}'", .{name});
        // std.debug.print(": {s}\n", .{message});
        std.debug.print("{s}", .{message});
        return error.CompileError;
    }

    fn compileStatement(self: *Compiler, stmt: ast.Statement) !void {
        switch (stmt) {
            .VarDeclaration => |val| {
                const global = try self.getVariable(val.name);
                try self.compileExpression(val.expression);
                try self.defineVariable(global);
            },
            .Block => |val| {
                self.beginScope();
                for (val.items) |item| {
                    try self.compileStatement(item);
                }
                try self.endScope();
            },
            .Assignment => |val| {
                switch (val.target) {
                    .Identifier => |name| {
                        try self.compileExpression(val.expression);
                        if (try self.resolveLocal(name)) |local| {
                            try self.emitOpCode(.SetLocal);
                            try self.emitU24(local);
                        } else if (try self.resolveUpvalue(name)) |upvalue| {
                            try self.emitOpCode(.SetUpvalue);
                            try self.emitByte(upvalue);
                        } else {
                            const constant = try self.identifierConstant(name);
                            try self.emitOpCode(.SetGlobal);
                            try self.emitU24(constant);
                        }
                    },
                    .Index => |index| {
                        try self.compileExpression(index.left);
                        try self.compileExpression(index.index);
                        try self.compileExpression(val.expression);
                        try self.emitOpCode(.IndexSet);
                        try self.emitOpCode(.Pop);
                    },
                }
            },
            .For => |val| {
                self.beginScope();
                if (val.expression.* != .Range) {
                    return errorAt("Only range expressions are supported for now.");
                }

                try self.compileExpression(val.expression.Range.start);
                try self.compileExpression(val.expression.Range.end);

                try self.declareVariable(val.capture);
                self.markInitialized();

                const increment_var_index = self.local_count - 1;

                // a dummy local for the right side of the range
                try self.addLocal("");
                self.markInitialized();

                const loop_start = self.currentChunk().code.items.len;
                const exit_jump = try self.emitJump(.JumpIfGreaterOrEq);
                const local_count = self.local_count;

                for (val.body.items) |item| {
                    try self.compileStatement(item);
                }

                try self.emitOpCode(.GetLocal);
                try self.emitU24(increment_var_index);
                try self.emitConstant(value.wrapInt(1));
                try self.emitOpCode(.Add);
                try self.emitOpCode(.SetLocal);
                try self.emitU24(increment_var_index);

                var i = self.local_count;
                while (i > local_count) {
                    try self.emitOpCode(.Pop);
                    i -= 1;
                }

                try self.emitLoop(loop_start);

                try self.patchJump(exit_jump);

                // we already popped the locals on the last iteration and now
                // need to reduce the local array accordingly
                while (self.local_count > local_count) {
                    self.local_count -= 1;
                }

                try self.endScope();
            },
            .While => |val| {
                const loop_start = self.currentChunk().code.items.len;
                try self.compileExpression(val.expression);

                const exit_jump = try self.emitJump(.JumpIfFalse);
                try self.emitOpCode(.Pop);

                for (val.body.items) |item| {
                    try self.compileStatement(item);
                }

                try self.emitLoop(loop_start);

                try self.patchJump(exit_jump);
                try self.emitOpCode(.Pop);
            },
            .Return => |val| {
                if (self.type == .Script) {
                    return errorAt("Can't return from top-level code.");
                }
                try self.compileExpression(val);
                try self.emitOpCode(.Return);
            },
            .Expression => |val| {
                try self.compileExpression(val);
                if (!self.ctx.suppress_pop) {
                    try self.emitOpCode(.Pop);
                }
            },
        }
    }

    fn compileExpression(self: *Compiler, expr: *const ast.Expression) anyerror!void {
        switch (expr.*) {
            .Identifier => |name| {
                if (try self.resolveLocal(name)) |local| {
                    try self.emitOpCode(.GetLocal);
                    try self.emitU24(local);
                } else if (try self.resolveUpvalue(name)) |upvalue| {
                    try self.emitOpCode(.GetUpvalue);
                    try self.emitByte(upvalue);
                } else {
                    const constant = try self.identifierConstant(name);
                    try self.emitOpCode(.GetGlobal);
                    try self.emitU24(constant);
                }
            },
            .String => |val| {
                try self.emitConstant(value.wrapObj(try object.copyString(self.vm, val)));
            },
            .Integer => |val| {
                try self.emitConstant(value.wrapInt(val));
            },
            .Float => |val| {
                try self.emitConstant(value.wrapFloat(val));
            },
            .Boolean => |val| {
                if (val) try self.emitOpCode(.True) else try self.emitOpCode(.False);
            },
            .Infix => |val| {
                try self.compileExpression(val.left);

                if (val.operator == .And) {
                    const end_jump = try self.emitJump(.JumpIfFalse);
                    try self.emitOpCode(.Pop);
                    try self.compileExpression(val.right);
                    try self.patchJump(end_jump);
                } else if (val.operator == .Or) {
                    const else_jump = try self.emitJump(.JumpIfFalse);
                    const end_jump = try self.emitJump(.Jump);
                    try self.patchJump(else_jump);
                    try self.emitOpCode(.Pop);
                    try self.compileExpression(val.right);
                    try self.patchJump(end_jump);
                } else {
                    try self.compileExpression(val.right);
                    switch (val.operator) {
                        .Plus => try self.emitOpCode(.Add),
                        .Minus => try self.emitOpCode(.Subtract),
                        .Asterisk => try self.emitOpCode(.Multiply),
                        .Slash => try self.emitOpCode(.Divide),
                        .Eq => try self.emitOpCode(.Equal),
                        .NotEq => try self.emitOpCodes(.Equal, .Not),
                        .Gt => try self.emitOpCode(.Greater),
                        .GtOrEq => try self.emitOpCodes(.Less, .Not),
                        .Lt => try self.emitOpCode(.Less),
                        .LtOrEq => try self.emitOpCodes(.Greater, .Not),
                        else => unreachable,
                    }
                }
            },
            .Prefix => |val| {
                try self.compileExpression(val.expression);
                switch (val.operator) {
                    .Minus => try self.emitOpCode(.Negate),
                    .Bang => try self.emitOpCode(.Not),
                    else => unreachable,
                }
            },
            .Function => |val| {
                self.indent += 1;

                var new_compiler: Compiler = undefined;
                try new_compiler.init(
                    self.vm,
                    self.gpa,
                    .Function,
                    self,
                    self.indent,
                    val.name,
                );

                new_compiler.vm.current_compiler = &new_compiler;
                new_compiler.beginScope();

                for (val.params.items) |param| {
                    switch (param) {
                        .Positional => |name| {
                            new_compiler.function.?.arity += 1;
                            if (new_compiler.function.?.arity > 255) {
                                return errorAt("Can't have more than 255 parameters.");
                            }
                            try new_compiler.declareVariable(name);
                            const constant = if (new_compiler.scope_depth > 0) 0 else try new_compiler.identifierConstant(name);
                            try new_compiler.defineVariable(constant);
                        },
                        .Default => unreachable,
                    }
                }

                switch (val.body) {
                    .Block => |block| {
                        for (block.items) |stmt| {
                            try new_compiler.compileStatement(stmt);
                        }
                    },
                    .Expression => unreachable,
                }

                const function = try new_compiler.endCompiler();

                if (function.upvalue_count > 0) {
                    const constant = try self.makeConstant(value.wrapObj(&function.obj));
                    try self.emitOpCode(.Closure);
                    try self.emitU24(constant);
                } else {
                    try self.emitConstant(value.wrapObj(&function.obj));
                }

                for (0..function.upvalue_count) |i| {
                    try self.emitByte(@intFromBool(new_compiler.upvalues[i].is_local));
                    try self.emitByte(new_compiler.upvalues[i].index);
                }

                self.indent -= 1;
            },
            .Call => |val| {
                const count = val.args.items.len;
                if (count == 255) {
                    return errorAt("Can't have more than 255 arguments");
                }
                try self.compileExpression(val.function);
                for (val.args.items) |arg| {
                    switch (arg) {
                        .Positional => |positional| try self.compileExpression(positional),
                        .Named => unreachable,
                    }
                }
                try self.emitBytes(.Call, @intCast(count));
            },
            .Range => unreachable,
            .List => |val| {
                var index = val.items.len;
                while (index > 0) {
                    index -= 1;
                    try self.compileExpression(val.items[index]);
                }
                try self.emitConstant(value.wrapObj(try object.allocateList(self.vm)));
                try self.emitOpCode(.ListInit);
                try self.emitU24(val.items.len);
            },
            .Table => |val| {
                var index = val.items.len;
                while (index > 0) {
                    index -= 1;
                    try self.compileExpression(val.items[index].value);
                    try self.compileExpression(val.items[index].key);
                }
                try self.emitConstant(value.wrapObj(try object.allocateTable(self.vm)));
                try self.emitOpCode(.TableInit);
                try self.emitU24(val.items.len);
            },
            .Index => |val| {
                try self.compileExpression(val.left);
                try self.compileExpression(val.index);
                try self.emitOpCode(.IndexGet);
            },
            .Match => |val| {
                const has_target = val.target != null;
                const instruction: chunk.OpCode = if (has_target) .JumpIfNotEq else .JumpIfFalse;

                if (val.target) |target| {
                    try self.compileExpression(target);
                }

                if (val.body == .Single) {
                    try self.compileExpression(val.body.Single.pattern);

                    const then_jump = try self.emitJump(instruction);

                    try self.emitOpCode(.Pop);
                    if (has_target) try self.emitOpCode(.Pop);
                    try self.compileMatchArmBody(val.body.Single.body);

                    const end_jump = try self.emitJump(.Jump);

                    try self.patchJump(then_jump);

                    try self.emitOpCode(.Pop);
                    if (has_target) try self.emitOpCode(.Pop);

                    try self.emitOpCode(.Null);

                    try self.patchJump(end_jump);

                    return;
                }

                var end_jumps: std.ArrayList(usize) = .{};
                errdefer end_jumps.deinit(self.gpa);

                const arms = val.body.Multiple.items;
                for (arms) |arm| {
                    if (arm.pattern.* == .Identifier and std.mem.eql(u8, arm.pattern.Identifier, "_")) {
                        break;
                    }

                    try self.compileExpression(arm.pattern);

                    const then_jump = try self.emitJump(instruction);

                    try self.emitOpCode(.Pop);
                    if (has_target) try self.emitOpCode(.Pop);
                    try self.compileMatchArmBody(arm.body);

                    try end_jumps.append(self.gpa, try self.emitJump(.Jump));

                    try self.patchJump(then_jump);
                    try self.emitOpCode(.Pop);
                }

                const last_arm: ?ast.MatchArm = if (arms.len > 0) arms[arms.len - 1] else null;
                var else_arm: ?ast.MatchArm = null;
                if (last_arm) |arm| {
                    if (arm.pattern.* == .Identifier and std.mem.eql(u8, arm.pattern.Identifier, "_")) {
                        else_arm = arm;
                    }
                }
                if (else_arm) |arm| {
                    if (has_target) try self.emitOpCode(.Pop);
                    try self.compileMatchArmBody(arm.body);
                } else {
                    if (has_target) try self.emitOpCode(.Pop);
                    try self.emitOpCode(.Null);
                }

                for (end_jumps.items) |jump| {
                    try self.patchJump(jump);
                }

                end_jumps.deinit(self.gpa);
            },
            .Null => {
                try self.emitOpCode(.Null);
            },
        }
    }

    fn compileMatchArmBody(self: *Compiler, body: ast.Statement) !void {
        self.ctx.suppress_pop = true;
        defer self.ctx.suppress_pop = false;

        try self.compileStatement(body);

        switch (body) {
            .Expression => {},
            else => try self.emitOpCode(.Null),
        }
    }
};
