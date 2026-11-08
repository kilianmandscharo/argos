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
    tail_recursive: bool,

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

    pub fn init(vm: *virtual_machine.VirtualMachine, compiler: *Compiler, index: u8, name: ?scanner.Token, tail_recursive: bool) !*@This() {
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
            .tail_recursive = tail_recursive,
        };
        return owned;
    }

    fn getDisplayName(self: *const Function) []const u8 {
        return self.getName() orelse "anonymous";
    }

    fn getName(self: *const Function) ?[]const u8 {
        return if (self.function.name) |name| name.chars else null;
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
            const name = self.getDisplayName();
            self.compiler.log("new scope depth for function {s} = {d}", .{ name, self.scope_depth });
        }
    }

    fn endScope(self: *Function) void {
        self.scope_depth -= 1;
        if (comptime constants.debug_compiler) {
            const name = self.getDisplayName();
            self.compiler.log("new scope depth for function {s} = {d}", .{ name, self.scope_depth });
        }
    }

    fn cleanupLocals(self: *Function) !void {
        var i = self.locals.items.len;
        const locals = self.locals.items;
        while (i > 0 and locals[i - 1].depth > self.scope_depth) {
            i -= 1;
            if (locals[i].is_captured) {
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
    type_map: TypeChecker.TypeMap,

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
            .type_map = .empty,
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
        tail_recursive: bool,
    ) !*Function {
        const index = self.functions.items.len;
        var function = try Function.init(self.vm, self, @intCast(index), name, tail_recursive);
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
        var type_checker = TypeChecker.init(self.arena, &self.vm.script_context);
        defer type_checker.deinit();

        const result = try type_checker.checkTypes(program);
        self.type_map = result.type_map;

        try self.compileExpression(program);
        const function = self.functions.pop().?;
        return function.function;
    }

    fn getType(self: *Compiler, expr: *const ast.Expression) *const Type {
        return self.type_map.get(expr).?;
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
                    .Minus => {
                        switch (self.getType(unary.right).Const) {
                            .Integer => try self.emitOp(.NegateInt),
                            .Float => try self.emitOp(.NegateFloat),
                            else => return error.InvalidTypeForNegate,
                        }
                    },
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
                        .Plus => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOp(.AddInt),
                            .Float => try self.emitOp(.AddFloat),
                            .String => try self.emitOp(.Concat),
                            else => return error.InvalidTypeForAdd,
                        },
                        .Minus => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOp(.SubtractInt),
                            .Float => try self.emitOp(.SubtractFloat),
                            else => return error.InvalidTypeForSubtract,
                        },
                        .Asterisk => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOp(.MultiplyInt),
                            .Float => try self.emitOp(.MultiplyFloat),
                            else => return error.InvalidTypeForMultiply,
                        },
                        .Slash => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOp(.DivideInt),
                            .Float => try self.emitOp(.DivideFloat),
                            else => return error.InvalidTypeForDivide,
                        },
                        .Eq => try self.emitOp(.Equal),
                        .NotEq => try self.emitOpCodes(.Equal, .Not),
                        .Gt => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOp(.GreaterInt),
                            .Float => try self.emitOp(.GreaterFloat),
                            else => return error.InvalidTypeForGreater,
                        },
                        .GtOrEq => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOpCodes(.LessInt, .Not),
                            .Float => try self.emitOpCodes(.LessFloat, .Not),
                            else => return error.InvalidTypeForGreaterOrEqual,
                        },
                        .Lt => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOp(.LessInt),
                            .Float => try self.emitOp(.LessFloat),
                            else => return error.InvalidTypeForLess,
                        },
                        .LtOrEq => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOpCodes(.GreaterInt, .Not),
                            .Float => try self.emitOpCodes(.GreaterFloat, .Not),
                            else => return error.InvalidTypeForLessOrEqual,
                        },
                        .Ampersand => try self.emitOp(.BitwiseAnd),
                        .Pipe => try self.emitOp(.BitwiseOr),
                        .Caret => try self.emitOp(.BitwiseXor),
                        .LeftShift => try self.emitOp(.LeftShift),
                        .RightShift => try self.emitOp(.RightShift),
                        .Percent => switch (self.getType(binary.left).Const) {
                            .Integer => try self.emitOp(.ModInt),
                            .Float => try self.emitOp(.ModFloat),
                            else => return error.InvalidTypeForMod,
                        },
                        .Cons => try self.emitOp(.Cons),
                        else => @panic("unknown operator"),
                    }
                }
            },
            .Grouping => |grouping| {
                var function = self.getFunction();
                if (comptime constants.debug_compiler) {
                    self.log("entering grouping in fn {s}", .{function.getDisplayName()});
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
                    self.log("leaving grouping for fn {s}", .{function.getDisplayName()});
                }

                function.endScope();
                try function.cleanupLocals();
            },
            .Function => |function| {
                std.debug.print("{f}\n", .{self.type_map.get(expr).?});

                var new_function = try self.addFunction(function.name, isTailRecursive(&function));

                if (comptime constants.debug_compiler) {
                    self.log("added new function, current function stack:", .{});
                    for (self.functions.items) |func| {
                        self.log("{s}", .{func.getDisplayName()});
                    }
                }

                new_function.beginScope();

                for (function.params.items) |*param| {
                    switch (param.*) {
                        .Positional => |*name| {
                            new_function.function.arity += 1;
                            if (new_function.function.arity > 255) {
                                return self.errorAt(name, "can't have more than 255 parameters");
                            }
                            try new_function.addLocal(name);
                        },
                        .Default => @panic("not implemented"),
                    }
                }

                try self.compileExpression(function.body);
                try self.emitOp(.Return);

                new_function.endScope();

                if (comptime constants.debug_disassemble) {
                    const name = new_function.getDisplayName();
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

                const current_function = self.getFunction();
                if (isRecursiveCall(current_function, &call) and current_function.tail_recursive) {
                    for (call.args.items) |arg| {
                        try self.compileExpression(arg);
                    }
                    try self.emitOpU8(.TailCall, @intCast(count));
                } else {
                    try self.compileExpression(call.function);
                    for (call.args.items) |arg| {
                        try self.compileExpression(arg);
                    }
                    try self.emitOpU8(.Call, @intCast(count));
                }
            },
            .List => |list| {
                for (list.items) |item| {
                    try self.compileExpression(item);
                }
                const new_list = try object.allocateList(self.vm);
                try self.emitConstant(value.wrapObj(&new_list.obj));
                try self.emitOpU16(.ListInit, @intCast(list.items.len));
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

                    if (arm.pattern.data == .List and arm.pattern.data.List.items.len == 0) {
                        try self.emitOp(.IsEmpty);
                        const jump = try self.emitJump(.JumpIfFalse);

                        try self.emitOp(.Pop);
                        try self.emitOp(.Pop);

                        try self.compileExpression(arm.expression);

                        try end_jumps.append(self.gpa, try self.emitJump(.Jump));

                        try self.patchJump(jump);
                        try self.emitOp(.Pop);

                        continue;
                    }

                    if (arm.pattern.data == .Binary and arm.pattern.data.Binary.operator == .Cons) {
                        const cons = arm.pattern.data.Binary;
                        if (cons.left.data != .Identifier) {
                            return self.errorAt(&arm.pattern.token, "expect identifier on left side of cons pattern");
                        }
                        if (cons.right.data != .Identifier) {
                            return self.errorAt(&arm.pattern.token, "expect identifier on right side of cons pattern");
                        }

                        try self.emitOp(.IsEmpty);
                        try self.emitOp(.Not);
                        const jump = try self.emitJump(.JumpIfFalse);

                        try self.emitOp(.Pop);
                        try self.emitOp(.CloneAndPopFirst);

                        const currentFunction = self.getFunction();
                        currentFunction.beginScope();
                        try currentFunction.addLocal(&cons.left.token);
                        try currentFunction.addLocal(&cons.right.token);
                        try self.compileExpression(arm.expression);
                        currentFunction.endScope();
                        try currentFunction.cleanupLocals();

                        try end_jumps.append(self.gpa, try self.emitJump(.Jump));

                        try self.patchJump(jump);
                        try self.emitOp(.Pop);

                        continue;
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

    pub fn isRecursiveCall(function: *const Function, call: *const ast.Call) bool {
        if (function.getName()) |name| {
            if (call.function.data != .Identifier) return false;
            return std.mem.eql(u8, name, call.function.data.Identifier);
        }
        return false;
    }

    pub fn isTailRecursive(function: *const ast.Function) bool {
        if (function.name) |function_name| {
            return checkTail(function_name.data, function.body, true);
        }
        return false;
    }

    fn checkTail(function_name: []const u8, expr: *const ast.Expression, in_tail: bool) bool {
        return switch (expr.data) {
            .Call => |call| {
                for (call.args.items) |arg| {
                    if (!checkTail(function_name, arg, false)) return false;
                }
                if (call.function.data == .Identifier and
                    std.mem.eql(u8, call.function.data.Identifier, function_name))
                {
                    return in_tail;
                }
                return true;
            },
            .Declaration => |declaration| {
                return checkTail(function_name, declaration.right, false);
            },
            .Unary => |unary| {
                return checkTail(function_name, unary.right, false);
            },
            .Binary => |binary| {
                return checkTail(function_name, binary.left, false) and
                    checkTail(function_name, binary.right, false);
            },
            .Grouping => |grouping| {
                for (grouping.items, 0..) |item, i| {
                    const tail = if (i == grouping.items.len - 1) in_tail else false;
                    if (!checkTail(function_name, item, tail)) return false;
                }
                return true;
            },
            .List => |list| {
                for (list.items) |item| {
                    if (!checkTail(function_name, item, false)) return false;
                }
                return true;
            },
            .Match => |match| {
                if (!checkTail(function_name, match.target, false)) return false;
                for (match.arms.items) |arm| {
                    if (!checkTail(function_name, arm.pattern, false)) return false;
                    if (!checkTail(function_name, arm.expression, in_tail)) return false;
                }
                return true;
            },
            .If => |ifExpr| {
                for (ifExpr.arms.items) |arm| {
                    if (!checkTail(function_name, arm.condition, false)) return false;
                    if (!checkTail(function_name, arm.then, in_tail)) return false;
                }
                if (ifExpr.alt) |alt| {
                    if (!checkTail(function_name, alt, in_tail)) return false;
                }
                return true;
            },
            .Function => |function| {
                return checkTail(function_name, function.body, false);
            },
            .Identifier,
            .String,
            .Integer,
            .Float,
            .Boolean,
            .Unit,
            => return true,
        };
    }
};

pub const Type = union(enum) {
    Var: *Var,
    Const: Const,
    Func: Func,
    List: *const Type,

    pub fn prune(self: *const Type) *const Type {
        switch (self.*) {
            .Var => |v| {
                const link = v.link orelse return self;
                const t = link.prune();
                v.link = t;
                return t;
            },
            else => return self,
        }
    }

    pub fn hasInstance(self: *const Type, class: Class) bool {
        return switch (class) {
            .Plus => switch (self.*) {
                .Const => |c| c == .Integer or c == .Float or c == .String,
                else => false,
            },
            .Num => switch (self.*) {
                .Const => |c| c == .Integer or c == .Float,
                else => false,
            },
            .Show => switch (self.*) {
                .Const => true,
                else => false,
            },
            .Eq => switch (self.*) {
                .Const => true,
                else => false,
            },
            .Ord => switch (self.*) {
                .Const => |c| c == .Integer or c == .Float,
                else => false,
            },
        };
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        const resolved = self.prune();
        switch (resolved.*) {
            .Const => |c| try writer.print("{s}", .{@tagName(c)}),
            .Var => |v| {
                try writer.print("t{d}", .{v.id});
                if (v.classes.count() > 0) {
                    try writer.print(" {{", .{});
                    var first = true;
                    var it = v.classes.iterator();
                    while (it.next()) |class| {
                        if (!first) try writer.print(", ", .{});
                        first = false;
                        try writer.print("{s}", .{@tagName(class)});
                    }
                    try writer.print("}}", .{});
                }
            },
            .List => |l| {
                try writer.print("[", .{});
                try l.format(writer);
                try writer.print("]", .{});
            },
            .Func => |f| {
                try writer.print("(", .{});
                for (f.from, 0..) |param, i| {
                    try param.format(writer);
                    if (i != f.from.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(") -> ", .{});
                try f.to.format(writer);
            },
        }
    }
};

const Func = struct {
    from: []const *const Type,
    to: *const Type,
};

const Const = enum {
    Unit,
    String,
    Integer,
    Float,
    Boolean,
};

pub const Class = enum { Plus, Num, Show, Eq, Ord };

pub const Var = struct {
    id: u32,
    link: ?*const Type = null,
    classes: std.EnumSet(Class),
};

const Scheme = union(enum) {
    Mono: *const Type,
    Poly: struct {
        vars: std.ArrayList(*Var),
        t: *const Type,
    },
};

const Env = struct {
    store: std.StringHashMapUnmanaged(Scheme),
    outer: ?*Env,

    pub fn init(arena: std.mem.Allocator, outer: ?*Env) !*Env {
        const env = try arena.create(Env);
        env.* = .{
            .outer = outer,
            .store = .{},
        };
        return env;
    }

    pub fn get(self: *Env, key: []const u8) ?Scheme {
        if (self.store.get(key)) |val| {
            return val;
        }
        if (self.outer) |outer| {
            return outer.get(key);
        }
        return null;
    }

    pub fn set(self: *Env, arena: std.mem.Allocator, key: []const u8, val: Scheme) !void {
        try self.store.put(arena, key, val);
    }
};

pub const TypeChecker = struct {
    arena: std.mem.Allocator,
    scratch_arena: std.heap.ArenaAllocator,
    current_var_id: u32 = 0,
    type_map: TypeMap,
    script_context: *virtual_machine.ScriptContext,

    const VarSet = std.AutoArrayHashMapUnmanaged(*Var, void);
    const TypeMap = std.AutoArrayHashMapUnmanaged(*const ast.Expression, *const Type);

    pub fn init(arena: std.mem.Allocator, script_context: *virtual_machine.ScriptContext) @This() {
        return .{
            .arena = arena,
            .scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .type_map = .empty,
            .script_context = script_context,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        self.scratch_arena.deinit();
    }

    pub inline fn scratch(self: *TypeChecker) std.mem.Allocator {
        return self.scratch_arena.allocator();
    }

    pub fn c(k: Const) *const Type {
        return switch (k) {
            .Unit => &.{ .Const = .Unit },
            .String => &.{ .Const = .String },
            .Integer => &.{ .Const = .Integer },
            .Float => &.{ .Const = .Float },
            .Boolean => &.{ .Const = .Boolean },
        };
    }

    fn newVar(self: *TypeChecker) !*Var {
        const new_var = try self.arena.create(Var);
        new_var.* = .{
            .id = self.current_var_id,
            .classes = .empty,
        };
        self.current_var_id += 1;
        return new_var;
    }

    pub fn checkTypes(self: *TypeChecker, expr: *const ast.Expression) !struct { root: *const Type, type_map: TypeMap } {
        const env = try Env.init(self.scratch(), null);

        try env.set(self.arena, "@assert", .{
            .Mono = try self.function(&.{c(.Boolean)}, c(.Unit)),
        });

        try env.set(self.arena, "@print", .{
            .Mono = try self.function(&.{try self.variable()}, c(.Unit)),
        });

        try env.set(self.arena, "@len", .{
            .Mono = try self.function(&.{try self.genericList()}, c(.Integer)),
        });

        try env.set(self.arena, "@nth", .{
            .Mono = try self.function(&.{ try self.genericList(), c(.Integer) }, c(.Integer)),
        });

        const root = try self.infer(env, expr);

        var it = self.type_map.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.* = entry.value_ptr.*.prune();
        }

        return .{ .root = root, .type_map = self.type_map };
    }

    pub fn variable(self: *TypeChecker) !*const Type {
        const v = try self.newVar();
        return try self.createType(.{ .Var = v });
    }

    pub fn list(self: *TypeChecker, t: *const Type) !*const Type {
        return try self.createType(.{ .List = t });
    }

    pub fn genericList(self: *TypeChecker) !*const Type {
        return try self.createType(.{ .List = try self.variable() });
    }

    pub fn function(self: *TypeChecker, from: []const *const Type, to: *const Type) !*const Type {
        return try self.createType(.{ .Func = .{ .from = from, .to = to } });
    }

    pub fn createType(self: *TypeChecker, t: Type) !*const Type {
        const allocated = try self.arena.create(Type);
        allocated.* = t;
        return allocated;
    }

    fn copy(self: *TypeChecker, freshVars: std.AutoHashMapUnmanaged(*Var, *const Type), t: *const Type) !*const Type {
        const resolved = t.prune();
        return switch (resolved.*) {
            .Var => |v| {
                return freshVars.get(v) orelse resolved;
            },
            .List => |l| {
                return try self.list(try self.copy(freshVars, l));
            },
            .Func => |f| {
                var from: std.ArrayList(*const Type) = .empty;
                for (f.from) |param| {
                    try from.append(self.arena, try self.copy(freshVars, param));
                }
                const to = try self.copy(freshVars, f.to);
                return try self.function(from.items, to);
            },
            else => resolved,
        };
    }

    fn instantiate(self: *TypeChecker, scheme: Scheme) !*const Type {
        return switch (scheme) {
            .Mono => |t| {
                return try self.copy(.empty, t);
            },
            .Poly => |s| {
                var fresh_vars: std.AutoHashMapUnmanaged(*Var, *const Type) = .empty;
                for (s.vars.items) |v| {
                    const fresh_var = try self.variable();
                    fresh_var.Var.classes = v.classes;
                    try fresh_vars.put(self.scratch(), v, fresh_var);
                }
                return try self.copy(fresh_vars, s.t.prune());
            },
        };
    }

    fn freeVars(self: *TypeChecker, out: *VarSet, t: *const Type) !void {
        switch (t.prune().*) {
            .Var => |v| {
                try out.put(self.scratch(), v, {});
            },
            .Func => |f| {
                for (f.from) |param| {
                    try self.freeVars(out, param);
                }
                try self.freeVars(out, f.to);
            },
            .List => |l| try self.freeVars(out, l),
            .Const => {},
        }
    }

    fn freeVarsFromEnv(self: *TypeChecker, env: *Env, out: *VarSet) !void {
        var current: ?*const Env = env;
        while (current) |e| : (current = e.outer) {
            var it = e.store.valueIterator();
            while (it.next()) |scheme| {
                switch (scheme.*) {
                    .Mono => |m| try self.freeVars(out, m),
                    .Poly => |p| {
                        var tmp: VarSet = .empty;
                        try self.freeVars(&tmp, p.t);
                        for (p.vars.items) |v| _ = tmp.swapRemove(v);
                        for (tmp.keys()) |v| try out.put(self.scratch(), v, {});
                    },
                }
            }
        }
    }

    fn generalize(self: *TypeChecker, env: *Env, t: *const Type) !Scheme {
        var varsFromEnv: VarSet = .empty;
        try self.freeVarsFromEnv(env, &varsFromEnv);

        var freeVarsFromType: VarSet = .empty;
        try self.freeVars(&freeVarsFromType, t);

        var vars: std.ArrayList(*Var) = .empty;

        for (freeVarsFromType.keys()) |v| {
            if (!varsFromEnv.contains(v)) {
                try vars.append(self.scratch(), v);
            }
        }

        return if (vars.items.len > 0) .{ .Poly = .{ .vars = vars, .t = t } } else .{ .Mono = t };
    }

    fn occurs(v: *Var, t: *const Type) bool {
        switch (t.prune().*) {
            .Var => |other| return other == v,
            .Func => |f| {
                for (f.from) |param| {
                    if (occurs(v, param)) return true;
                }
                return occurs(v, f.to);
            },
            .List => |l| return occurs(v, l),
            else => return false,
        }
    }

    fn constrain(t: *const Type, class: Class) !void {
        const resolved = t.prune();
        switch (resolved.*) {
            .Var => |v| v.classes.insert(class),
            else => if (!resolved.hasInstance(class)) return error.MissingInstance,
        }
    }

    fn unify(type_a: *const Type, type_b: *const Type) !void {
        const a = type_a.prune();
        const b = type_b.prune();

        if (comptime constants.debug_type_checker) {
            logging.log("Unify {f} with {f}", .{ a, b }, .{});
        }

        if (a.* == .Var and b.* == .Var and a.Var == b.Var) return;

        if (a.* == .Var) {
            if (occurs(a.Var, b)) return error.InfiniteType;
            switch (b.*) {
                .Var => |other| other.classes.setUnion(a.Var.classes),
                else => {
                    var it = a.Var.classes.iterator();
                    while (it.next()) |class| if (!b.hasInstance(class)) return error.MissingInstance;
                },
            }
            a.Var.link = b;
        } else if (b.* == .Var) {
            try unify(b, a);
        } else if (a.* == .Const and b.* == .Const and @backingInt(a.Const) == @backingInt(b.Const)) {
            return;
        } else if (a.* == .Func and b.* == .Func) {
            if (a.Func.from.len != b.Func.from.len) return error.ArityMismatch;
            for (0..a.Func.from.len) |i| {
                try unify(a.Func.from[i], b.Func.from[i]);
            }
            try unify(a.Func.to, b.Func.to);
        } else if (a.* == .List and b.* == .List) {
            try unify(a.List, b.List);
        } else {
            return error.CantUnify;
        }
    }

    fn infer(self: *TypeChecker, env: *Env, expr: *const ast.Expression) anyerror!*const Type {
        if (comptime constants.debug_type_checker) {
            logging.log("infer {s}", .{@tagName(expr.data)}, .{});
        }
        const t = try self.inferType(env, expr);
        try self.type_map.put(self.arena, expr, t);
        return t;
    }

    fn inferType(self: *TypeChecker, env: *Env, expr: *const ast.Expression) !*const Type {
        return switch (expr.data) {
            .Call => |call| {
                const f = try self.infer(env, call.function);
                var args: std.ArrayList(*const Type) = .empty;
                for (call.args.items) |arg| try args.append(self.arena, try self.infer(env, arg));
                const r = try self.variable();
                try unify(f, try self.function(args.items, r));
                return r.prune();
            },
            .Declaration => |declaration| {
                const t = try self.infer(env, declaration.right);
                const scheme = try self.generalize(env, t);
                try env.set(self.scratch(), declaration.left.data, scheme);
                return c(.Unit);
            },
            .Unary => |unary| {
                const t = try self.infer(env, unary.right);
                switch (unary.operator) {
                    .Minus => {
                        try constrain(t, .Num);
                        return t;
                    },
                    .Bang => {
                        try unify(t, c(.Boolean));
                        return c(.Boolean);
                    },
                    .Tilde => {
                        try unify(t, c(.Integer));
                        return c(.Integer);
                    },
                    else => return error.InvalidUnaryOperator,
                }
            },
            .Binary => |binary| {
                const lt = try self.infer(env, binary.left);
                const rt = try self.infer(env, binary.right);
                switch (binary.operator) {
                    .And, .Or => {
                        try unify(lt, c(.Boolean));
                        try unify(rt, c(.Boolean));
                        return c(.Boolean);
                    },
                    .Plus => {
                        try unify(lt, rt);
                        try constrain(lt, .Plus);
                        return lt;
                    },
                    .Minus, .Asterisk, .Slash, .Percent => {
                        try unify(lt, rt);
                        try constrain(lt, .Num);
                        return lt;
                    },
                    .Eq, .NotEq => {
                        try unify(lt, rt);
                        try constrain(lt, .Eq);
                        return c(.Boolean);
                    },
                    .Gt, .GtOrEq, .Lt, .LtOrEq => {
                        try unify(lt, rt);
                        try constrain(lt, .Ord);
                        return c(.Boolean);
                    },
                    .Ampersand, .Pipe, .Caret, .LeftShift, .RightShift => {
                        try unify(lt, c(.Integer));
                        try unify(rt, c(.Integer));
                        return c(.Integer);
                    },
                    .Cons => {
                        const list_t = try self.list(lt);
                        try unify(rt, list_t);
                        return list_t;
                    },
                    else => return error.InvalidBinaryOperator,
                }
            },
            .Grouping => |grouping| {
                const inner_env = try Env.init(self.scratch(), env);
                for (grouping.items[0 .. grouping.items.len - 1]) |e| {
                    _ = try self.infer(inner_env, e);
                }
                return try self.infer(inner_env, grouping.items[grouping.items.len - 1]);
            },
            .List => |l| {
                if (l.items.len == 0) return try self.genericList();
                const t = try self.infer(env, l.items[0]);
                for (l.items[1..l.items.len]) |e| {
                    try unify(t, try self.infer(env, e));
                }
                return try self.list(t);
            },
            .Match => |match| {
                const target_t = try self.infer(env, match.target);
                const result = try self.variable();
                for (match.arms.items) |arm| {
                    switch (arm.pattern.data) {
                        .Identifier => |name| {
                            if (std.mem.eql(u8, name, "_")) {
                                try unify(try self.infer(env, arm.expression), result);
                                continue;
                            }
                            return error.NotSupported;
                        },
                        .Binary => |binary| {
                            switch (binary.operator) {
                                .Cons => {
                                    try unify(target_t, try self.genericList());
                                    const inner_env = try Env.init(self.scratch(), env);
                                    if (!std.mem.eql(u8, binary.left.data.Identifier, "_")) {
                                        const fresh_var = try self.variable();
                                        try unify(fresh_var, target_t.List);
                                        try inner_env.set(self.scratch(), binary.left.data.Identifier, .{ .Mono = fresh_var });
                                    }
                                    if (!std.mem.eql(u8, binary.right.data.Identifier, "_")) {
                                        const fresh_var = try self.variable();
                                        try unify(fresh_var, target_t);
                                        try inner_env.set(self.scratch(), binary.right.data.Identifier, .{ .Mono = fresh_var });
                                    }
                                    const t = try self.infer(inner_env, arm.expression);
                                    try unify(t, result);
                                },
                                else => return error.NotSupported,
                            }
                        },
                        else => {
                            try unify(target_t, try self.infer(env, arm.pattern));
                            try unify(try self.infer(env, arm.expression), result);
                        },
                    }
                }
                return result;
            },
            .If => |ifExpr| {
                const result = try self.variable();
                for (ifExpr.arms.items) |arm| {
                    try unify(try self.infer(env, arm.condition), c(.Boolean));
                    try unify(try self.infer(env, arm.then), result);
                }
                if (ifExpr.alt) |alt| {
                    try unify(try self.infer(env, alt), result);
                } else {
                    try unify(result, c(.Unit));
                }
                return result;
            },
            .Identifier => |name| {
                if (env.get(name)) |scheme| {
                    return try self.instantiate(scheme);
                }
                expr.token.printError("unbound var", self.script_context, "TypeChecker");
                return error.UnboundVar;
            },
            .Function => |f| {
                var inner_env = try Env.init(self.scratch(), env);
                var params: std.ArrayList(*const Type) = .empty;

                for (f.params.items) |param| {
                    switch (param) {
                        .Positional => |name| {
                            const fresh_var = try self.variable();
                            const scheme: Scheme = .{
                                .Mono = fresh_var,
                            };
                            try inner_env.set(self.scratch(), name.data, scheme);
                            try params.append(self.arena, fresh_var);
                        },
                        .Default => @panic("not implemented"),
                    }
                }

                return try self.function(params.items, try self.infer(inner_env, f.body));
            },
            .String => c(.String),
            .Integer => c(.Integer),
            .Float => c(.Float),
            .Boolean => c(.Boolean),
            .Unit => c(.Unit),
        };
    }
};
