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

    pub fn init(vm: *virtual_machine.VirtualMachine, name: ?[]const u8) !@This() {
        const function = try object.allocateFunction(vm);
        function.name = name;
        return .{
            .locals = .empty,
            .upvalues = .empty,
            .scope_depth = 0,
            .function = function,
        };
    }

    fn getName(self: *Function) []const u8 {
        if (self.index == 0) return "script";
        if (self.function.name) |name| return name;
        return "anonymous";
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

    fn beginScope(self: *Function) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Function) !void {
        self.scope_depth -= 1;
        var i = self.locals.items.len;
        while (i > 0 and self.locals.items[i - 1].depth > self.scope_depth) {
            i -= 1;
            if (self.locals.items[i].is_captured) {
                try self.compiler.emitOp(.SwapCloseUpvalue);
            } else {
                try self.compiler.emitOp(.SwapPop);
            }
            _ = self.locals.pop();
        }
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
        try compiler.addFunction(null);
        return compiler;
    }

    pub fn addFunction(
        self: *Compiler,
        name: ?[]const u8,
    ) !*Function {
        const function = try Function.init(self.vm, name);
        try function.locals.append(self.arena, .{
            .is_used = true,
            .is_captured = false,
            .token = scanner.Token.dummy(),
            .depth = 0,
        });
        try self.functions.append(self.arena, function);
        return &function;
    }

    fn getFunction(self: *Compiler) *Function {
        return &self.functions.items[self.functions.items.len - 1];
    }

    pub fn compile(self: *Compiler, program: ast.Program) !*object.ObjFunction {
        for (program.items) |expr| {
            self.compileExpression(expr) catch |err| {
                if (constants.debug_compiler) {
                    self.currentChunk().disassemble("<error>");
                }
                return err;
            };
        }

        // TODO: does that make sense?
        try self.emitOp(.Unit);
        try self.emitOp(.Return);

        const function = self.functions.pop().?;
        if (comptime constants.debug_disassemble) {
            const name = function.getName();
            logDebug("Finished compiling {s}", .{name});
            function.function.chunk.disassemble(name);
        }

        return function.function;
    }

    fn identifierConstant(self: *Compiler, name: []const u8) !u16 {
        const obj = try object.copyString(self.vm, name);
        return try self.makeConstant(value.wrapObj(obj));
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
            .String => |string| {
                try self.emitConstant(value.wrapObj(try object.copyString(self.vm, string)));
            },
            .Integer => |integer| {
                try self.emitConstant(value.wrapInt(integer));
            },
            .Float => |float| {
                try self.emitConstant(value.wrapFloat(float));
            },
            .Boolean => |boolean| {
                if (boolean) try self.emitOp(.True) else try self.emitOp(.False);
            },
            .Unit => {
                try self.emitOp(.Unit);
            },
            .Unary => |unary| {
                try self.compileExpression(unary.right);
                switch (unary.operator) {
                    .Minus => try self.emitOp(.Negate),
                    .Bang => try self.emitOp(.Not),
                    .Tilde => try self.emitOp(.BitwiseNot),
                    else => unreachable,
                }
            },
            .Binary => |binary| {
                try self.compileExpression(binary.left);

                if (binary.operator == .And) {
                    const end_jump = try self.emitJump(.JumpIfFalse);
                    try self.emitOp(.Pop);
                    try self.compileExpression(binary.right);
                    try self.patchJump(end_jump);
                } else if (binary.operator == .Or) {
                    const else_jump = try self.emitJump(.JumpIfFalse);
                    const end_jump = try self.emitJump(.Jump);
                    try self.patchJump(else_jump);
                    try self.emitOp(.Pop);
                    try self.compileExpression(binary.right);
                    try self.patchJump(end_jump);
                } else {
                    try self.compileExpression(binary.right);
                    switch (binary.operator) {
                        .Plus => {
                            const left_type = try self.resolveExpression(binary.left);
                            const right_type = try self.resolveExpression(binary.right);
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
            .Grouping => |grouping| {
                const function = self.getFunction();
                function.beginScope();
                for (grouping.items) |item| {
                    try self.compileExpression(item);
                    // TODO: what to do?
                }
                try function.endScope();
            },
            .Function => |function| {
                var new_function = try self.addFunction(function.name);

                for (function.params.items) |*param| {
                    switch (param.*) {
                        .Positional => |*name| {
                            new_function.function.?.arity += 1;
                            if (new_function.function.?.arity > 255) {
                                // TODO: pass correct token
                                return self.errorAt(&scanner.Token.dummy(), "can't have more than 255 parameters");
                            }
                            try new_function.addLocal(name);
                        },
                        .Default => @panic("not implemented"),
                    }
                }

                try self.compileExpression(function.body);
                try self.emitOp(.Return);

                if (comptime constants.debug_disassemble) {
                    const name = new_function.getName();
                    logDebug("Finished compiling {s}", .{name});
                    new_function.function.chunk.disassemble(name);
                }

                if (new_function.upvalues.items.len > 0) {
                    const constant = try self.makeConstant(value.wrapObj(&new_function.function.obj));
                    try self.emitOpU16(.Closure, constant);
                } else {
                    try self.emitConstant(value.wrapObj(&new_function.function.obj));
                }

                for (new_function.upvalues.items) |upvalue| {
                    try self.emitByte(@intFromBool(upvalue.is_local));
                    try self.emitByte(upvalue.index);
                }

                _ = self.functions.pop();
            },
            .Call => |call| {
                const count = call.args.items.len;
                if (count == 255) {
                    return self.errorAt(&call.function.token, "can't have more than 255 arguments");
                }
                try self.compileExpression(call.function);
                for (call.args.items) |arg| {
                    try self.compileExpression(arg);
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
            .Match => |match| {
                try self.compileExpression(match.target);

                var end_jumps: std.ArrayList(usize) = .empty;
                errdefer end_jumps.deinit(self.gpa);

                var found_else = false;

                for (match.arms.items) |arm| {
                    if (arm.pattern.data == .Identifier and std.mem.eql(u8, arm.pattern.data.Identifier, "_")) {
                        try self.emitOp(.Pop);
                        try self.compileExpression(arm.body);
                        found_else = true;
                        break;
                    }

                    try self.compileExpression(arm.pattern);

                    const then_jump = try self.emitJump(.JumpIfNotEq);

                    try self.emitOp(.Pop);
                    try self.emitOp(.Pop);

                    try self.compileExpression(arm.body);

                    try end_jumps.append(self.gpa, try self.emitJump(.Jump));

                    try self.patchJump(then_jump);
                    try self.emitOp(.Pop);
                }

                if (!found_else) {
                    try self.emitOp(.Pop);
                    // TODO: does that make sense?
                    try self.emitOp(.Unit);
                }

                for (end_jumps.items) |jump| {
                    try self.patchJump(jump);
                }

                end_jumps.deinit(self.gpa);
            },
            .If => |ifExpr| {
                var end_jumps: std.ArrayList(usize) = .empty;
                errdefer end_jumps.deinit(self.gpa);

                for (ifExpr.arms.items) |arm| {
                    try self.compileExpression(arm.condition);

                    const then_jump = try self.emitJump(.JumpIfFalse);

                    try self.emitOp(.Pop);
                    try self.compileExpression(arm.then);

                    try end_jumps.append(self.gpa, try self.emitJump(.Jump));

                    try self.patchJump(then_jump);
                    try self.emitOp(.Pop);
                }

                if (ifExpr.alt) |alt| {
                    try self.compileExpression(alt.then);
                }

                for (end_jumps.items) |jump| {
                    try self.patchJump(jump);
                }

                end_jumps.deinit(self.gpa);
            },
        }
    }
};
