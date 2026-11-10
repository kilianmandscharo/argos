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
const hm = @import("hm.zig");

pub const Function = struct {
    locals: std.ArrayList(Local),
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

    const Local = struct {
        is_captured: bool,
        name: []const u8,
        depth: u32,
    };

    pub fn init(vm: *virtual_machine.VirtualMachine, compiler: *Compiler, index: u8, func_name: ?[]const u8, tail_recursive: bool) !*@This() {
        const function = try object.allocateFunction(vm);
        if (func_name) |name| {
            const obj = try object.copyString(vm, name);
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
        return self.getName() orelse "lambda";
    }

    fn getName(self: *const Function) ?[]const u8 {
        return if (self.function.name) |name| name.chars else null;
    }

    fn getLocal(self: *Function, index: u8) *Local {
        return &self.locals.items[index];
    }

    pub fn addLocal(self: *Function, name: []const u8) !void {
        if (self.locals.items.len == UINT8_COUNT) {
            return error.TooManyVariables;
        }
        try self.locals.append(self.compiler.arena, .{
            .is_captured = false,
            .name = name,
            .depth = self.scope_depth,
        });
    }

    pub fn resolveLocal(self: *Function, name: []const u8) ?u8 {
        var i = self.locals.items.len;
        while (i > 0) {
            i -= 1;
            const local = &self.locals.items[i];
            if (std.mem.eql(u8, name, local.name)) {
                return @intCast(i);
            }
        }
        return null;
    }

    fn getUpvalue(self: *Function, index: u8) *Local {
        return &self.locals.items[self.upvalues.items[index].index];
    }

    pub fn addUpvalue(self: *Function, index: u8, is_local: bool) !u8 {
        for (self.upvalues.items, 0..) |upvalue, i| {
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(i);
            }
        }
        if (self.upvalues.items.len == UINT8_COUNT) {
            return error.TooManyUpvalues;
        }
        try self.upvalues.append(self.compiler.arena, .{
            .is_local = is_local,
            .index = index,
        });
        self.function.upvalue_count += 1;
        return @intCast(self.upvalues.items.len - 1);
    }

    fn resolveUpvalue(self: *Function, name: []const u8) !?u8 {
        if (self.getParent()) |parent| {
            if (parent.resolveLocal(name)) |index| {
                parent.getLocal(index).is_captured = true;
                return try self.addUpvalue(index, true);
            }
            if (try parent.resolveUpvalue(name)) |index| {
                return try self.addUpvalue(index, false);
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
    type_map: hm.TypeChecker.TypeMap,
    var_map: std.AutoHashMapUnmanaged(*const hm.Type, *const hm.Type),
    scratch_arena: std.heap.ArenaAllocator,

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
            .var_map = .empty,
            .scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
        };
        try compiler.initNativeFunctions();
        return compiler;
    }

    pub fn deinit(self: *Compiler) void {
        self.scratch_arena.deinit();
    }

    pub inline fn scratch(self: *Compiler) std.mem.Allocator {
        return self.scratch_arena.allocator();
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
        name: ?[]const u8,
        tail_recursive: bool,
    ) !*Function {
        const index = self.functions.items.len;
        var function = try Function.init(self.vm, self, @intCast(index), name, tail_recursive);
        try function.locals.append(self.arena, .{
            .is_captured = false,
            .name = name orelse "",
            .depth = 0,
        });
        try self.functions.append(self.arena, function);
        return self.functions.items[index];
    }

    fn getFunction(self: *Compiler) *Function {
        return self.functions.items[self.functions.items.len - 1];
    }

    pub fn compile(self: *Compiler, program: *const ast.Expression) !*object.ObjFunction {
        var type_checker = hm.TypeChecker.init(self.arena, &self.vm.script_context);
        defer type_checker.deinit();

        const result = try type_checker.checkTypes(program);
        self.type_map = result.type_map;

        try self.compileExpression(program);
        const function = self.functions.pop().?;
        return function.function;
    }

    fn getType(self: *Compiler, expr: *const ast.Expression) *const hm.Type {
        if (self.type_map.get(expr)) |t| {
            return switch (t.*) {
                .Var => self.var_map.get(t).?.prune(),
                else => t,
            };
        }
        @panic("type not found");
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
            return error.JumpTooLarge;
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

    fn compileIdentifier(self: *Compiler, name: []const u8) !void {
        if (self.getFunction().resolveLocal(name)) |index| {
            try self.emitOpU8(.GetLocal, index);
        } else if (try self.getFunction().resolveUpvalue(name)) |index| {
            try self.emitOpU8(.GetUpvalue, index);
        } else if (self.globals.get(name)) |func_obj| {
            try self.emitConstant(value.wrapObj(func_obj));
        } else {
            @panic("type checker messed up");
        }
    }

    fn compileFunction(self: *Compiler, function: *const ast.Function, tail_recursive: bool) !void {
        var new_function = try self.addFunction(if (function.name) |name| name.data else null, tail_recursive);

        if (comptime constants.debug_compiler) {
            self.log("added new function, current function stack:", .{});
            for (self.functions.items) |func| {
                self.log("{s}", .{func.getDisplayName()});
            }
        }

        new_function.beginScope();

        for (function.params.items) |*param| {
            switch (param.*) {
                .Positional => |*param_name| {
                    new_function.function.arity += 1;
                    if (new_function.function.arity > 255) {
                        return self.errorAt(param_name, "can't have more than 255 parameters");
                    }
                    try new_function.addLocal(param_name.data);
                },
                .Default => |*default_param| {
                    new_function.function.arity += 1;
                    if (new_function.function.arity > 255) {
                        return self.errorAt(&default_param.name, "can't have more than 255 parameters");
                    }
                    try new_function.addLocal(default_param.name.data);
                },
            }
        }

        try self.compileExpression(function.body);
        try self.emitOp(.Return);

        new_function.endScope();

        if (comptime constants.debug_disassemble) {
            const func_name = new_function.getDisplayName();
            self.log("Finished compiling {s}", .{func_name});
            new_function.function.chunk.disassemble(func_name);
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
    }

    fn getGenericFunctionName(self: *Compiler, name: []const u8, t: *const hm.Type) ![]const u8 {
        var buf: std.Io.Writer.Allocating = .init(self.scratch());
        try buf.writer.print("{s}", .{name});
        for (t.Func.from) |param| {
            try buf.writer.print("_{f}", .{param.t()});
        }
        return buf.written();
    }

    fn compileArgs(self: *Compiler, function: *const ast.Function, args: []*const ast.Expression) !void {
        for (function.params.items, 0..) |param, i| {
            switch (param) {
                .Positional => try self.compileExpression(args[i]),
                .Default => |default_param| {
                    if (i >= args.len) {
                        try self.compileExpression(default_param.value);
                    } else {
                        try self.compileExpression(args[i]);
                    }
                },
            }
        }
    }

    fn compileExpression(self: *Compiler, expr: *const ast.Expression) anyerror!void {
        if (comptime constants.debug_compiler) {
            self.log("compiling {s}", .{@tagName(expr.data)});
            self.debug_indent += 1;
        }

        switch (expr.data) {
            .Declaration => |declaration| {
                if (declaration.right.data != .Function) {
                    try self.compileExpression(declaration.right);
                    try self.getFunction().addLocal(declaration.left.data);
                    try self.emitOp(.Unit);
                    return;
                }

                const function = declaration.right.data.Function;
                const func_t = self.type_map.get(declaration.right).?.Func;
                const tail_recursive = isTailRecursive(&function);

                if (!func_t.is_generic) {
                    try self.compileFunction(&function, tail_recursive);
                    try self.getFunction().addLocal(declaration.left.data);
                    try self.emitOp(.Unit);
                    return;
                }

                for (func_t.resolved.items) |resolved_t| {
                    for (0..func_t.from.len) |i| {
                        if (func_t.from[i].t().* == .Var) {
                            switch (resolved_t.Func.from[i]) {
                                .Positional => |t| try self.var_map.put(self.scratch(), func_t.from[i].t(), t),
                                .Default => try self.var_map.put(
                                    self.scratch(),
                                    func_t.from[i].t(),
                                    self.getType(function.params.items[i].Default.value),
                                ),
                            }
                        }
                    }

                    try self.compileFunction(&function, tail_recursive);
                    self.var_map.clearRetainingCapacity();

                    try self.getFunction().addLocal(try self.getGenericFunctionName(function.name.?.data, resolved_t));
                }

                try self.emitOp(.Unit);
            },
            .Identifier => {
                try self.compileIdentifier(expr.token.data);
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
                try self.compileFunction(&function, false);
            },
            .Call => |call| {
                const count = call.args.items.len;
                if (count == 255) {
                    return self.errorAt(&call.function.token, "can't have more than 255 arguments");
                }

                const func_t = self.getType(call.function);

                const current_function = self.getFunction();
                if (isRecursiveCall(current_function, &call) and current_function.tail_recursive) {
                    for (call.args.items) |arg| {
                        try self.compileExpression(arg);
                    }
                    try self.emitOpU8(.TailCall, @intCast(count));
                } else {
                    if (func_t.Func.is_generic) {
                        try self.compileIdentifier(try self.getGenericFunctionName(call.function.data.Identifier, func_t));
                    } else {
                        try self.compileExpression(call.function);
                    }
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
                        try currentFunction.addLocal(cons.left.token.data);
                        try currentFunction.addLocal(cons.right.token.data);
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
