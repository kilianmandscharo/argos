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
    local_count: u8,
    scope_depth: u32,
    function: ?*object.ObjFunction,
    type: FunctionType,
    enclosing: ?*Compiler,
    upvalues: [UINT8_COUNT]Upvalue,
    indent: usize,

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
            self.compileStatement(stmt, false) catch |err| {
                self.currentChunk().disassemble("<error>");
                return err;
            };
        }
        try self.emitOp(.Null);
        return try self.endCompiler();
    }

    fn endCompiler(self: *Compiler) !*object.ObjFunction {
        try self.emitOp(.Return);
        self.vm.current_compiler = self.enclosing;
        const function = self.function;
        if (comptime constants.debug_disassemble) {
            const name = self.getFunctionName();
            logDebug("Finished compiling {s}", .{name});
            self.currentChunk().disassemble(name);
        }
        return function.?;
    }

    fn defineVariable(self: *Compiler, global: u16) !void {
        if (self.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        try self.emitOpU16(.DefineGlobal, global);
    }

    fn markInitialized(self: *Compiler) void {
        if (self.scope_depth == 0) return;
        self.locals[self.local_count - 1].depth = self.scope_depth;
    }

    fn getVariable(self: *Compiler, name: scanner.Token) !u16 {
        try self.declareVariable(name);
        if (self.scope_depth > 0) return 0;
        return try self.identifierConstant(name.data);
    }

    fn declareVariable(self: *Compiler, name: scanner.Token) !void {
        if (self.scope_depth == 0) return;

        if (self.local_count > 0) {
            var i = self.local_count;
            while (i > 0) : (i -= 1) {
                const local = self.locals[i - 1];
                if (local.depth != null and local.depth.? < self.scope_depth) {
                    break;
                }
                if (Compiler.identifiersEqual(name.data, local.name)) {
                    return self.errorAt(name, "Already a variable with this name in this scope.");
                }
            }
        }

        try self.addLocal(name);
    }

    fn addLocal(self: *Compiler, name: scanner.Token) !void {
        if (self.local_count == std.math.maxInt(u8) + 1) {
            return self.errorAt(name, "Too many local variables in block.");
        }
        const local = &self.locals[self.local_count];
        self.local_count += 1;
        local.name = name.data;
        local.depth = null;
        local.is_captured = false;
    }

    fn identifierConstant(self: *Compiler, name: []const u8) !u16 {
        const obj = try object.copyString(self.vm, name);
        return try self.makeConstant(value.wrapObj(obj));
    }

    fn emitLoop(self: *Compiler, loop_start: usize) !void {
        try self.emitOp(.Loop);

        const offset = self.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) {
            // TODO: pass corrent token
            return self.errorAt(scanner.Token.dummy(), "Loop body too large.");
        }

        try self.emitU16(offset);
    }

    fn emitJump(self: *Compiler, instruction: chunk.OpCode) !usize {
        try self.emitOp(instruction);
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return self.currentChunk().code.items.len - 2;
    }

    fn patchJump(self: *Compiler, offset: usize) !void {
        const cur_chunk = self.currentChunk();
        const jump = cur_chunk.code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            // TODO: pass corrent token
            return self.errorAt(scanner.Token.dummy(), "Too much code to jump over.");
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
            return self.errorAt("Too many closure variables in function.");
        }

        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        const retVal = self.function.?.upvalue_count;
        self.function.?.upvalue_count += 1;
        return retVal;
    }

    fn resolveUpvalue(self: *Compiler, name: scanner.Token) !?u8 {
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
                try self.emitOp(.SwapCloseUpvalue);
            } else {
                try self.emitOp(.SwapPop);
            }
            self.local_count -= 1;
        }
    }

    fn resolveLocal(self: *Compiler, name: scanner.Token) !?u8 {
        if (self.local_count == 0) return null;
        var i = self.local_count;
        while (i > 0) {
            i -= 1;
            const local = &self.locals[i];
            if (Compiler.identifiersEqual(name.data, local.name)) {
                if (local.depth == null) {
                    return self.errorAt(name, "Can't read local variable in its own initializer.");
                }
                return i;
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

    fn makeConstant(self: *Compiler, val: value.Value) !u16 {
        return try self.currentChunk().addConstant(self.gpa, val);
    }

    fn currentChunk(self: *Compiler) *chunk.Chunk {
        return &self.function.?.chunk;
    }

    fn emitOpU8(self: *Compiler, first: chunk.OpCode, second: u8) !void {
        try self.emitOp(first);
        try self.emitByte(second);
    }

    fn emitOpU16(self: *Compiler, first: chunk.OpCode, second: u16) !void {
        try self.emitOp(first);
        try self.emitU16(second);
    }

    fn emitByte(self: *Compiler, byte: u8) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Byte = byte }, 0);
    }

    fn emitU16(self: *Compiler, index: usize) !void {
        const bytes = chunk.indexToU16(index);
        try self.emitByte(bytes[0]);
        try self.emitByte(bytes[1]);
    }

    fn emitOp(self: *Compiler, op: chunk.OpCode) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = op }, 0);
    }

    fn emitOpCodes(self: *Compiler, a: chunk.OpCode, b: chunk.OpCode) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = a }, 0);
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = b }, 0);
    }

    fn errorAt(self: *Compiler, token: scanner.Token, message: []const u8) anyerror {
        token.printError(message, &self.vm.script_context, "Compiler");
        return error.CompileError;
    }

    fn compileStatement(self: *Compiler, stmt: ast.Statement, suppress_pop: bool) !void {
        switch (stmt) {
            .VarDeclaration => |val| {
                const global = try self.getVariable(val.name);
                try self.compileExpression(val.expression);
                try self.defineVariable(global);
            },
            .Assignment => |val| {
                switch (val.target) {
                    .Identifier => |name| {
                        try self.compileExpression(val.expression);
                        if (try self.resolveLocal(name)) |local| {
                            try self.emitOpU8(.SetLocal, local);
                        } else if (try self.resolveUpvalue(name)) |upvalue| {
                            try self.emitOp(.SetUpvalue);
                            try self.emitByte(upvalue);
                        } else {
                            const constant = try self.identifierConstant(name.data);
                            try self.emitOpU16(.SetGlobal, constant);
                        }
                    },
                    .Index => |index| {
                        try self.compileExpression(index.left);
                        try self.compileExpression(index.index);
                        try self.compileExpression(val.expression);
                        try self.emitOp(.IndexSet);
                        try self.emitOp(.Pop);
                    },
                }
            },
            .For => |val| {
                self.beginScope();
                if (val.expression.data != .Range) {
                    unreachable;
                }

                try self.compileExpression(val.expression.data.Range.start);
                try self.compileExpression(val.expression.data.Range.end);

                try self.declareVariable(val.capture);
                self.markInitialized();

                const increment_var_index = self.local_count - 1;

                // a dummy local for the right side of the range
                try self.addLocal(scanner.Token.dummy());
                self.markInitialized();

                const loop_start = self.currentChunk().code.items.len;
                const exit_jump = try self.emitJump(.JumpIfGreaterOrEq);
                const local_count = self.local_count;

                try self.compileStatement(val.body.*, false);

                try self.emitOpU8(.GetLocal, increment_var_index);
                try self.emitConstant(value.wrapInt(1));
                try self.emitOp(.Add);
                try self.emitOpU8(.SetLocal, increment_var_index);

                var i = self.local_count;
                while (i > local_count) {
                    try self.emitOp(.Pop);
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
                try self.emitOp(.Pop);

                try self.compileStatement(val.body.*, false);

                try self.emitLoop(loop_start);

                try self.patchJump(exit_jump);
                try self.emitOp(.Pop);
            },
            .Return => |val| {
                if (self.type == .Script) {
                    return self.errorAt(val.token, "Can't return from top-level code.");
                }
                try self.compileExpression(val);
                try self.emitOp(.Return);
            },
            .Expression => |val| {
                try self.compileExpression(val);
                if (!suppress_pop) {
                    try self.emitOp(.Pop);
                }
            },
        }
    }

    fn compileExpression(self: *Compiler, expr: *const ast.Expression) anyerror!void {
        switch (expr.data) {
            .Identifier => |name| {
                if (try self.resolveLocal(expr.token)) |local| {
                    try self.emitOpU8(.GetLocal, local);
                } else if (try self.resolveUpvalue(expr.token)) |upvalue| {
                    try self.emitOpU8(.GetUpvalue, upvalue);
                } else {
                    const constant = try self.identifierConstant(name);
                    try self.emitOpU16(.GetGlobal, constant);
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
                if (val) try self.emitOp(.True) else try self.emitOp(.False);
            },
            .Infix => |val| {
                try self.compileExpression(val.left);

                if (val.operator == .And) {
                    const end_jump = try self.emitJump(.JumpIfFalse);
                    try self.emitOp(.Pop);
                    try self.compileExpression(val.right);
                    try self.patchJump(end_jump);
                } else if (val.operator == .Or) {
                    const else_jump = try self.emitJump(.JumpIfFalse);
                    const end_jump = try self.emitJump(.Jump);
                    try self.patchJump(else_jump);
                    try self.emitOp(.Pop);
                    try self.compileExpression(val.right);
                    try self.patchJump(end_jump);
                } else {
                    try self.compileExpression(val.right);
                    switch (val.operator) {
                        .Plus => try self.emitOp(.Add),
                        .Minus => try self.emitOp(.Subtract),
                        .Asterisk => try self.emitOp(.Multiply),
                        .Slash => try self.emitOp(.Divide),
                        .Eq => try self.emitOp(.Equal),
                        .NotEq => try self.emitOpCodes(.Equal, .Not),
                        .Gt => try self.emitOp(.Greater),
                        .GtOrEq => try self.emitOpCodes(.Less, .Not),
                        .Lt => try self.emitOp(.Less),
                        .LtOrEq => try self.emitOpCodes(.Greater, .Not),
                        .Ampersand => try self.emitOp(.BitwiseAnd),
                        .Pipe => try self.emitOp(.BitwiseOr),
                        .Caret => try self.emitOp(.BitwiseXor),
                        .LeftShift => try self.emitOp(.LeftShift),
                        .RightShift => try self.emitOp(.RightShift),
                        .Percent => try self.emitOp(.Mod),
                        else => unreachable,
                    }
                }
            },
            .Prefix => |val| {
                try self.compileExpression(val.expression);
                switch (val.operator) {
                    .Minus => try self.emitOp(.Negate),
                    .Bang => try self.emitOp(.Not),
                    .Tilde => try self.emitOp(.BitwiseNot),
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
                                // TODO: pass corrent token
                                return self.errorAt(scanner.Token.dummy(), "Can't have more than 255 parameters.");
                            }
                            try new_compiler.declareVariable(name);
                            const constant = if (new_compiler.scope_depth > 0) 0 else try new_compiler.identifierConstant(name.data);
                            try new_compiler.defineVariable(constant);
                        },
                        .Default => unreachable,
                    }
                }

                try new_compiler.compileExpression(val.body);

                const function = try new_compiler.endCompiler();

                if (function.upvalue_count > 0) {
                    const constant = try self.makeConstant(value.wrapObj(&function.obj));
                    try self.emitOpU16(.Closure, constant);
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
                    return self.errorAt(val.function.token, "Can't have more than 255 arguments");
                }
                try self.compileExpression(val.function);
                for (val.args.items) |arg| {
                    switch (arg) {
                        .Positional => |positional| try self.compileExpression(positional),
                        .Named => unreachable,
                    }
                }
                try self.emitOpU8(.Call, @intCast(count));
            },
            .Range => unreachable,
            .List => |val| {
                var index = val.items.len;
                while (index > 0) {
                    index -= 1;
                    try self.compileExpression(val.items[index]);
                }
                try self.emitConstant(value.wrapObj(try object.allocateList(self.vm)));
                try self.emitOpU16(.ListInit, @intCast(val.items.len));
            },
            .Table => |val| {
                var index = val.items.len;
                while (index > 0) {
                    index -= 1;
                    try self.compileExpression(val.items[index].value);
                    try self.compileExpression(val.items[index].key);
                }
                try self.emitConstant(value.wrapObj(try object.allocateTable(self.vm)));
                try self.emitOpU16(.TableInit, @intCast(val.items.len));
            },
            .Index => |val| {
                try self.compileExpression(val.left);
                try self.compileExpression(val.index);
                try self.emitOp(.IndexGet);
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

                    try self.emitOp(.Pop);
                    if (has_target) try self.emitOp(.Pop);
                    try self.compileExpression(val.body.Single.body);

                    const end_jump = try self.emitJump(.Jump);

                    try self.patchJump(then_jump);

                    try self.emitOp(.Pop);
                    if (has_target) try self.emitOp(.Pop);

                    try self.emitOp(.Null);

                    try self.patchJump(end_jump);

                    return;
                }

                var end_jumps: std.ArrayList(usize) = .{};
                errdefer end_jumps.deinit(self.gpa);

                const arms = val.body.Multiple.items;
                for (arms) |arm| {
                    if (arm.pattern.data == .Identifier and std.mem.eql(u8, arm.pattern.data.Identifier, "_")) {
                        break;
                    }

                    try self.compileExpression(arm.pattern);

                    const then_jump = try self.emitJump(instruction);

                    try self.emitOp(.Pop);
                    if (has_target) try self.emitOp(.Pop);
                    try self.compileExpression(arm.body);

                    try end_jumps.append(self.gpa, try self.emitJump(.Jump));

                    try self.patchJump(then_jump);
                    try self.emitOp(.Pop);
                }

                const last_arm: ?ast.MatchArm = if (arms.len > 0) arms[arms.len - 1] else null;
                var else_arm: ?ast.MatchArm = null;
                if (last_arm) |arm| {
                    if (arm.pattern.data == .Identifier and std.mem.eql(u8, arm.pattern.data.Identifier, "_")) {
                        else_arm = arm;
                    }
                }
                if (else_arm) |arm| {
                    if (has_target) try self.emitOp(.Pop);
                    try self.compileExpression(arm.body);
                } else {
                    if (has_target) try self.emitOp(.Pop);
                    try self.emitOp(.Null);
                }

                for (end_jumps.items) |jump| {
                    try self.patchJump(jump);
                }

                end_jumps.deinit(self.gpa);
            },
            .Null => {
                try self.emitOp(.Null);
            },
            .Block => |val| {
                self.beginScope();
                for (val.items, 0..) |item, i| {
                    const is_last = i == val.items.len - 1;
                    try self.compileStatement(item, is_last);
                    if (is_last and item != .Expression) {
                        try self.emitOp(.Null);
                    }
                }
                try self.endScope();
            },
        }
    }
};
