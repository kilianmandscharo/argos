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

const TypeInfo = enum {
    Float,
    Int,
    Bool,
    Unit,
    String,
    Function,
    List,
    Unknown,
};

const VariableInfo = struct {
    is_used: bool,
    is_captured: bool,
    token: scanner.Token,
    depth: u32,
};

const Upvalue = struct {
    index: u8,
    is_local: bool,
};

const UINT8_COUNT = std.math.maxInt(u8) + 1;

const Function = struct {
    locals: std.ArrayList(VariableInfo),
    upvalues: std.ArrayList(Upvalue),
    scope_depth: u32,
    function: *object.ObjFunction,
    index: u8,
    compiler: *const Compiler,

    pub fn init(vm: *virtual_machine.VirtualMachine) !@This() {
        return .{
            .locals = .empty,
            .upvalues = .empty,
            .scope_depth = 0,
            .function = try object.allocateFunction(vm),
        };
    }

    fn getLocal(self: *Function, index: u8) *VariableInfo {
        return &self.locals.items[index];
    }

    pub fn addLocal(self: *Function, name: *const scanner.Token) !void {
        var i = self.locals.items.len;
        while (i > 0) {
            i -= 1;
            const local = self.locals.items[i];
            if (local.depth < self.scope_depth) {
                break;
            }
            if (std.mem.eql(u8, name.data, local.token.data)) {
                return self.compiler.errorAt(name, "already a variable with this name in this scope");
            }
        }
        if (self.locals.items.len == UINT8_COUNT) {
            return self.compiler.errorAt(name, "too many local variables in function");
        }
        try self.locals.append(self.compiler.arena, .{
            .is_used = false,
            .is_captured = false,
            .token = name.*,
            .depth = self.scope_depth,
        });
    }

    pub fn resolveLocal(self: *Function, name: *const scanner.Token) ?u8 {
        var i = self.locals.items.len;
        while (i > 0) {
            i -= 1;
            const local = &self.locals.items[i];
            if (std.mem.eql(u8, name.data, local.token.data)) {
                return @intCast(i);
            }
        }
        return null;
    }

    fn getUpvalue(self: *Function, index: u8) *VariableInfo {
        return &self.locals.items[self.upvalues.items[index].index];
    }

    pub fn addUpvalue(self: *Function, name: *const scanner.Token, index: u8, is_local: bool) !u8 {
        for (self.upvalues.items, 0..) |upvalue, i| {
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(i);
            }
        }
        if (self.upvalues.items.len == UINT8_COUNT) {
            return self.compiler.errorAt(name, "too many closure variables in function");
        }
        try self.upvalues.append(self.arena, .{
            .is_local = is_local,
            .index = index,
        });
        return @intCast(self.upvalues.items.len - 1);
    }

    fn resolveUpvalue(self: *Function, name: *const scanner.Token) !?u8 {
        if (self.getParent()) |parent| {
            if (parent.resolveLocal(name)) |index| {
                parent.getLocal(index).is_captured = true;
                return try self.addUpvalue(name, index, true);
            }
            if (try parent.resolveUpvalue(name)) |index| {
                return try self.addUpvalue(name, index, false);
            }
        }
        return null;
    }

    fn getParent(self: Function) ?*Function {
        if (self.index == 0) return null;
        return self.compiler.functions.items[self.index - 1];
    }
};

pub const Compiler = struct {
    vm: *virtual_machine.VirtualMachine,
    gpa: std.mem.Allocator,
    arena: std.mem.Allocator,
    functions: std.ArrayList(Function),

    pub fn init(
        vm: *virtual_machine.VirtualMachine,
        gpa: std.mem.Allocator,
        arena: std.mem.Allocator,
    ) !Compiler {
        const compiler: Compiler = .{
            .vm = vm,
            .gpa = gpa,
            .arena = arena,
            .functions = .empty,
        };

        const function = try Function.init(vm);
        try function.locals.append(arena, .{
            .is_used = true,
            .is_captured = false,
            .token = scanner.Token.dummy(),
            .depth = 0,
        });

        try compiler.functions.append(arena, function);

        return compiler;
    }

    pub fn initNewScope(
        self: *Compiler,
        name: ?[]const u8,
    ) !*Compiler {
        const new_compiler = try self.arena.create(Compiler);
        new_compiler.* = .{
            .vm = self.vm,
            .enclosing = self,
            .gpa = self.gpa,
            .arena = self.arena,
            .function = null,
            .type = .Function,
            .locals = .empty,
            .upvalues = .empty,
            .scope_depth = 0,
            .globals = self.globals,
            .expression_types = self.expression_types,
        };

        new_compiler.function = try object.allocateFunction(new_compiler.vm);

        if (name) |func_name| {
            const obj = try object.copyString(new_compiler.vm, func_name);
            new_compiler.getFunction().name = obj.asString();
        }

        try new_compiler.locals.append(new_compiler.arena, .{
            .type_info = .Unknown,
            .is_const = false,
            .is_used = true,
            .is_captured = false,
            .token = scanner.Token.dummy(),
            .depth = 0,
        });

        return new_compiler;
    }

    fn getFunction(self: *Compiler) *Function {
        return &self.functions.items[self.functions.items.len - 1];
    }

    fn getFunctionName(self: *Compiler) []const u8 {
        return if (self.getFunction().name) |name| name.chars else "script";
    }

    pub fn compile(self: *Compiler, program: ast.Program) !*object.ObjFunction {
        for (program.items) |stmt| {
            self.compileStatement(stmt, false) catch |err| {
                if (constants.debug_compiler) {
                    self.currentChunk().disassemble("<error>");
                }
                return err;
            };
        }
        try self.emitOp(.Null);
        return try self.endCompiler();
    }

    fn endCompiler(self: *Compiler) !*object.ObjFunction {
        // TODO: if a function has a return statement there will be 2 return
        // instructions
        try self.emitOp(.Return);
        const function = self.getFunction();
        if (comptime constants.debug_disassemble) {
            const name = self.getFunctionName();
            logDebug("Finished compiling {s}", .{name});
            self.currentChunk().disassemble(name);
        }
        return function;
    }

    fn identifierConstant(self: *Compiler, name: []const u8) !u16 {
        const obj = try object.copyString(self.vm, name);
        return try self.makeConstant(value.wrapObj(obj));
    }

    fn emitLoop(self: *Compiler, loop_start: usize) !void {
        try self.emitOp(.Loop);

        const offset = self.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) {
            // TODO: pass correct token
            return self.errorAt(&scanner.Token.dummy(), "Loop body too large.");
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
            // TODO: pass correct token
            return self.errorAt(&scanner.Token.dummy(), "Too much code to jump over.");
        }
        const bytes = chunk.indexToU16(jump);
        cur_chunk.code.items[offset] = bytes[0];
        cur_chunk.code.items[offset + 1] = bytes[1];
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) !void {
        self.scope_depth -= 1;
        while (self.locals.items.len > 0 and
            self.locals.items[self.locals.items.len - 1].depth > self.scope_depth)
        {
            if (self.locals.items[self.locals.items.len - 1].is_captured) {
                try self.emitOp(.SwapCloseUpvalue);
            } else {
                try self.emitOp(.SwapPop);
            }
            _ = self.locals.pop();
        }
    }

    fn emitConstant(self: *Compiler, val: value.Value) !void {
        try self.currentChunk().writeConstant(self.gpa, val, 0);
    }

    fn makeConstant(self: *Compiler, val: value.Value) !u16 {
        return try self.currentChunk().addConstant(self.gpa, val);
    }

    fn currentChunk(self: *Compiler) *chunk.Chunk {
        return self.getFunction().function.chunk;
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

    fn errorAt(self: *Compiler, token: *const scanner.Token, message: []const u8) anyerror {
        token.printError(message, &self.vm.script_context, "Compile");
        return error.CompileError;
    }

    fn compileExpression(self: *Compiler, expr: *const ast.Expression) anyerror!void {
        switch (expr.data) {
            .Declaration => |declaration| {
                try self.compileExpression(declaration.right);
                try self.getFunction().declareLocal(&declaration.left);
            },
            .Identifier => {
                if (self.getFunction().resolveLocal(&expr.token)) |index| {
                    try self.emitOpU8(.GetLocal, index);
                } else if (try self.getFunction().resolveUpvalue(&expr.token)) |index| {
                    try self.emitOpU8(.GetUpvalue, index);
                } else {
                    return self.errorAt(&expr.token, "unknown identifier");
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
            .Binary => |val| {
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
                        .Plus => {
                            const left_type = try self.resolveExpression(val.left);
                            const right_type = try self.resolveExpression(val.right);
                            if (left_type == .Int and right_type == .Int) {
                                try self.emitOp(.AddInt);
                            } else {
                                try self.emitOp(.Add);
                            }
                        },
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
                        else => @panic("unknown operator"),
                    }
                }
            },
            .Unary => |val| {
                try self.compileExpression(val.expression);
                switch (val.operator) {
                    .Minus => try self.emitOp(.Negate),
                    .Bang => try self.emitOp(.Not),
                    .Tilde => try self.emitOp(.BitwiseNot),
                    else => unreachable,
                }
            },
            .Function => |val| {
                var new_compiler = try self.initNewScope(val.name);
                new_compiler.beginScope();

                for (val.params.items) |*param| {
                    switch (param.*) {
                        .Positional => |*name| {
                            new_compiler.function.?.arity += 1;
                            if (new_compiler.function.?.arity > 255) {
                                // TODO: pass correct token
                                return self.errorAt(&scanner.Token.dummy(), "Can't have more than 255 parameters.");
                            }
                            try new_compiler.declareLocalVar(name, .Unknown);
                        },
                        .Default => unreachable,
                    }
                }

                try new_compiler.compileExpression(val.body);

                const function = try new_compiler.endCompiler();

                if (new_compiler.upvalues.items.len > 0) {
                    const constant = try self.makeConstant(value.wrapObj(&function.obj));
                    try self.emitOpU16(.Closure, constant);
                } else {
                    try self.emitConstant(value.wrapObj(&function.obj));
                }

                for (new_compiler.upvalues.items) |upvalue| {
                    try self.emitByte(@intFromBool(upvalue.is_local));
                    try self.emitByte(upvalue.index);
                }
            },
            .Call => |val| {
                const count = val.args.items.len;
                if (count == 255) {
                    return self.errorAt(&val.function.token, "Can't have more than 255 arguments");
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
            .List => |val| {
                var index = val.items.len;
                while (index > 0) {
                    index -= 1;
                    try self.compileExpression(val.items[index]);
                }
                try self.emitConstant(value.wrapObj(try object.allocateList(self.vm)));
                try self.emitOpU16(.ListInit, @intCast(val.items.len));
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

                var end_jumps: std.ArrayList(usize) = .empty;
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
            .Unit => {
                try self.emitOp(.Null);
            },
            .Grouping => |val| {
                self.beginScope();
                for (val.items, 0..) |item, i| {
                    const is_last = i == val.items.len - 1;
                    try self.compileStatement(item, is_last);
                    if (is_last and item != .Expression and item != .Return) {
                        try self.emitOp(.Null);
                    }
                }
                try self.endScope();
            },
        }
    }

    // fn updateVarInfo(var_info: *VariableInfo, new_type: TypeInfo) void {
    //     if (var_info.type_info == new_type) return;
    //     var_info.type_info = .Unknown;
    // }
    //
    // fn resolveExpression(self: *Compiler, expr: *const ast.Expression) !TypeInfo {
    //     if (self.expression_types.get(expr)) |t| return t;
    //
    //     const t: TypeInfo = switch (expr.data) {
    //         .Identifier => blk: {
    //             if (self.resolveLocal(&expr.token)) |index| {
    //                 break :blk self.getLocal(index).type_info;
    //             }
    //             if (try self.resolveUpvalue(&expr.token)) |index| {
    //                 break :blk self.getUpvalue(index).type_info;
    //             }
    //             if (self.globals.get(expr.token.data)) |var_info| {
    //                 break :blk var_info.type_info;
    //             }
    //             return self.errorAt(&expr.token, "identifier not found");
    //         },
    //         .String => .String,
    //         .Integer => .Int,
    //         .Float => .Float,
    //         .Boolean => .Bool,
    //         .Infix => .Unknown,
    //         .Prefix => .Unknown,
    //         .Function => .Unknown,
    //         .Call => .Unknown,
    //         .Range => .Unknown,
    //         .List => .List,
    //         .Table => .Table,
    //         .Index => .Unknown,
    //         .Match => .Unknown,
    //         .Null => .Null,
    //         .Block => .Unknown,
    //     };
    //
    //     try self.expression_types.put(self.arena, expr, t);
    //
    //     return t;
    // }
};
