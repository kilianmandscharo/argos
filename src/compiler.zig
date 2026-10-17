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
const native = @import("native.zig");

pub const Function = struct {
    locals: std.ArrayList(VariableInfo),
    upvalues: std.ArrayList(Upvalue),
    scope_depth: u32,
    function: *object.ObjFunction,
    index: u8,
    compiler: *Compiler,

    const UINT8_COUNT = std.math.maxInt(u8) + 1;

    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    const VariableInfo = struct {
        is_used: bool,
        is_captured: bool,
        token: scanner.Token,
        depth: u32,
    };

    pub fn init(vm: *virtual_machine.VirtualMachine, compiler: *Compiler, index: u8, name: ?scanner.Token) !*@This() {
        const function = try object.allocateFunction(vm);
        if (name) |token| {
            const obj = try object.copyString(vm, token.data);
            function.name = obj.asString();
        }
        const owned = try compiler.arena.create(Function);
        owned.* = .{
            .locals = .empty,
            .upvalues = .empty,
            .scope_depth = 0,
            .function = function,
            .index = index,
            .compiler = compiler,
        };
        return owned;
    }

    fn getName(self: *const Function) []const u8 {
        if (self.function.name) |name| return name.chars;
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
        try self.upvalues.append(self.compiler.arena, .{
            .is_local = is_local,
            .index = index,
        });
        self.function.upvalue_count += 1;
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
        if (comptime constants.debug_compiler) {
            const name = self.getName();
            self.compiler.log("new scope depth for function {s} = {d}", .{ name, self.scope_depth });
        }
    }

    fn endScope(self: *Function) !void {
        self.scope_depth -= 1;

        if (comptime constants.debug_compiler) {
            const name = self.getName();
            self.compiler.log("new scope depth for function {s} = {d}", .{ name, self.scope_depth });
        }

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
    functions: std.ArrayList(*Function),
    globals: std.StringHashMapUnmanaged(*object.Obj),
    debug_indent: usize = 0,

    pub fn init(
        vm: *virtual_machine.VirtualMachine,
        gpa: std.mem.Allocator,
        arena: std.mem.Allocator,
    ) !Compiler {
        var compiler: Compiler = .{
            .vm = vm,
            .gpa = gpa,
            .arena = arena,
            .functions = .empty,
            .globals = .empty,
        };
        try compiler.initNativeFunctions();
        return compiler;
    }

    fn log(self: *const Compiler, comptime fmt: []const u8, args: anytype) void {
        logging.log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = .Debug,
            .indent = self.debug_indent,
            .module = "Compiler",
        });
    }

    pub fn initNativeFunctions(self: *Compiler) !void {
        for (native.natives) |item| {
            try self.globals.put(self.arena, item.name, try object.allocateNative(self.vm, item.function));
        }
    }

    pub fn addFunction(
        self: *Compiler,
        name: ?scanner.Token,
    ) !*Function {
        const index = self.functions.items.len;
        var function = try Function.init(self.vm, self, @intCast(index), name);
        try function.locals.append(self.arena, .{
            .is_used = true,
            .is_captured = false,
            .token = if (name) |token| token else scanner.Token.dummy(),
            .depth = 0,
        });
        try self.functions.append(self.arena, function);
        return self.functions.items[index];
    }

    fn getFunction(self: *Compiler) *Function {
        return self.functions.items[self.functions.items.len - 1];
    }

    pub fn compile(self: *Compiler, program: *const ast.Expression) !*object.ObjFunction {
        try self.compileExpression(program);
        const function = self.functions.pop().?;
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
        return &self.getFunction().function.chunk;
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
        if (comptime constants.debug_compiler) {
            self.log("compiling {s}", .{@tagName(expr.data)});
            self.debug_indent += 1;
        }

        switch (expr.data) {
            .Declaration => |declaration| {
                try self.compileExpression(declaration.right);
                try self.getFunction().addLocal(&declaration.left);
                try self.emitOp(.Unit);
            },
            .Identifier => {
                if (self.getFunction().resolveLocal(&expr.token)) |index| {
                    try self.emitOpU8(.GetLocal, index);
                } else if (try self.getFunction().resolveUpvalue(&expr.token)) |index| {
                    try self.emitOpU8(.GetUpvalue, index);
                } else if (self.globals.get(expr.token.data)) |func_obj| {
                    try self.emitConstant(value.wrapObj(func_obj));
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
                        .Cons => try self.emitOp(.Cons),
                        else => @panic("unknown operator"),
                    }
                }
            },
            .Grouping => |grouping| {
                var function = self.getFunction();
                if (comptime constants.debug_compiler) {
                    self.log("entering grouping in fn {s}", .{function.getName()});
                }
                function.beginScope();
                for (grouping.items, 0..) |item, i| {
                    try self.compileExpression(item);
                    const is_last = i == grouping.items.len - 1;
                    if (!is_last) {
                        try self.emitOp(.Pop);
                    }
                }
                if (comptime constants.debug_compiler) {
                    self.log("leaving grouping for fn {s}", .{function.getName()});
                }
                try function.endScope();
            },
            .Function => |function| {
                var new_function = try self.addFunction(function.name);

                if (comptime constants.debug_compiler) {
                    self.log("added new function, current function stack:", .{});
                    for (self.functions.items) |func| {
                        self.log("{s}", .{func.getName()});
                    }
                }

                new_function.beginScope();

                for (function.params.items) |*param| {
                    switch (param.*) {
                        .Positional => |*name| {
                            new_function.function.arity += 1;
                            if (new_function.function.arity > 255) {
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
                try new_function.endScope();

                if (comptime constants.debug_disassemble) {
                    const name = new_function.getName();
                    self.log("Finished compiling {s}", .{name});
                    new_function.function.chunk.disassemble(name);
                }

                if (new_function.index > 0) {
                    const compiled_function = self.functions.pop().?;

                    if (compiled_function.upvalues.items.len > 0) {
                        const constant = try self.makeConstant(value.wrapObj(&compiled_function.function.obj));
                        try self.emitOpU16(.Closure, constant);
                    } else {
                        try self.emitConstant(value.wrapObj(&compiled_function.function.obj));
                    }

                    for (compiled_function.upvalues.items) |upvalue| {
                        try self.emitByte(@intFromBool(upvalue.is_local));
                        try self.emitByte(upvalue.index);
                    }
                } else {
                    try self.emitConstant(value.wrapObj(&new_function.function.obj));
                }
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
                for (val.items) |item| {
                    try self.compileExpression(item);
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
                        try self.compileExpression(arm.expression);
                        found_else = true;
                        break;
                    }

                    try self.compileExpression(arm.pattern);

                    const then_jump = try self.emitJump(.JumpIfNotEq);

                    try self.emitOp(.Pop);
                    try self.emitOp(.Pop);

                    try self.compileExpression(arm.expression);

                    try end_jumps.append(self.gpa, try self.emitJump(.Jump));

                    try self.patchJump(then_jump);
                    try self.emitOp(.Pop);
                }

                if (!found_else) {
                    try self.emitOp(.Pop);
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
                    try self.compileExpression(alt);
                } else {
                    try self.emitOp(.Unit);
                }

                for (end_jumps.items) |jump| {
                    try self.patchJump(jump);
                }

                end_jumps.deinit(self.gpa);
            },
        }

        if (comptime constants.debug_compiler) {
            self.debug_indent -= 1;
        }
    }
};
