const std = @import("std");
const chunk = @import("chunk.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const compiler = @import("compiler.zig");
const scanner = @import("scanner.zig");
const native = @import("native.zig");
const logging = @import("logging.zig");
const constants = @import("constants.zig");
const parser = @import("parser.zig");

const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "VirtualMachine",
        .color = .Blue,
    });
}

const CallFrame = struct {
    function: *object.ObjFunction,
    upvalues: ?[]?*object.ObjUpvalue,
    ip: usize,
    slot: usize,
};

const GlobalContext = struct {
    pub fn hash(_: @This(), key: *object.ObjString) u64 {
        return key.hash;
    }

    pub fn eql(_: @This(), a: *object.ObjString, b: *object.ObjString) bool {
        return a == b;
    }
};

const StringContext = struct {
    pub fn hash(_: @This(), key: *object.ObjString) u64 {
        return key.hash;
    }

    pub fn eql(_: @This(), a: *object.ObjString, b: *object.ObjString) bool {
        return std.mem.eql(u8, a.chars, b.chars);
    }
};

pub const TableGlobals = std.HashMapUnmanaged(*object.ObjString, value.Value, StringContext, 80);

pub const VirtualMachine = struct {
    gpa: std.mem.Allocator,
    stack: [constants.stack_max]value.Value,
    stack_top: usize,
    frames: [constants.stack_max]CallFrame,
    frame_count: usize,
    frame: *CallFrame,
    strings: std.HashMapUnmanaged(*object.ObjString, void, StringContext, 80),
    globals: TableGlobals,
    objects: ?*object.Obj,
    open_upvalues: ?*object.ObjUpvalue,
    current_compiler: ?*compiler.Compiler,
    gray_stack: std.ArrayList(*object.Obj),
    bytes_allocated: usize,
    next_gc: usize,

    pub fn init(gpa: std.mem.Allocator) !VirtualMachine {
        if (comptime constants.debug_trace_execution) {
            logDebug("Init vm...", .{});
        }

        var vm = VirtualMachine{
            .gpa = gpa,
            .stack = undefined,
            .stack_top = 0,
            .frames = undefined,
            .frame = undefined,
            .frame_count = 0,
            .strings = .{},
            .globals = .{},
            .objects = null,
            .open_upvalues = null,
            .current_compiler = null,
            .gray_stack = .{},
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
        };

        try vm.defineNative("clock", native.clockNative);
        try vm.defineNative("print", native.printNative);
        try vm.defineNative("assert", native.assertNative);

        if (comptime constants.debug_trace_execution) {
            logDebug("Vm initialized.", .{});
        }

        return vm;
    }

    pub fn deinit(self: *VirtualMachine) void {
        var head = self.objects;
        while (head) |obj| {
            const next = obj.next;
            obj.deinit(self);
            head = next;
        }
        self.strings.deinit(self.gpa);
        self.globals.deinit(self.gpa);
        self.gray_stack.deinit(self.gpa);
    }

    pub fn findString(self: *VirtualMachine, chars: []const u8, hash: u64) ?*object.Obj {
        var tmp = object.ObjString{
            .chars = chars,
            .hash = hash,
        };
        if (self.strings.getKey(&tmp)) |string_object| {
            return &string_object.obj;
        }
        return null;
    }

    fn resetStack(self: *VirtualMachine) void {
        self.stack_top = 0;
        self.frame_count = 0;
    }

    inline fn push(self: *VirtualMachine, val: value.Value) void {
        self.stack[self.stack_top] = val;
        self.stack_top += 1;
    }

    inline fn pop(self: *VirtualMachine) value.Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    inline fn peek(self: *VirtualMachine, distance: usize) value.Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    inline fn swapInPlace(self: *VirtualMachine, val: value.Value, distance: usize) void {
        self.stack[self.stack_top - 1 - distance] = val;
    }

    pub fn interpret(self: *VirtualMachine, source: []const u8) !InterpretResult {
        if (comptime constants.debug_trace_execution) {
            logDebug("Starting pre-compilation...", .{});
        }
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

        const ast = try parser.createAst(arena.allocator(), source);

        var c: compiler.Compiler = undefined;
        try compiler.Compiler.init(&c, self, self.gpa, .Script, null, 0, null);
        if (comptime constants.debug_trace_execution) {
            logDebug("Pre-compilation finished.", .{});
        }

        self.current_compiler = &c;

        if (comptime constants.debug_trace_execution) {
            logDebug("Compiling...", .{});
        }
        const function = try c.compile(ast);
        arena.deinit();
        if (comptime constants.debug_trace_execution) {
            logDebug("Compilation finished.", .{});
        }

        if (comptime constants.debug_trace_execution) {
            logDebug("Setting up global function...", .{});
        }

        self.push(value.wrapObj(&function.obj));
        try self.call(function, 0, null);
        self.frame = &self.frames[self.frame_count - 1];

        if (comptime constants.debug_trace_execution) {
            logDebug("Global function set up.", .{});
        }

        if (comptime constants.debug_trace_execution) {
            logDebug("Running byte code...", .{});
        }
        _ = self.run() catch {
            return .RuntimeError;
        };
        if (comptime constants.debug_trace_execution) {
            logDebug("Script finished.", .{});
        }

        return .Ok;
    }

    pub inline fn readByte(self: *VirtualMachine) u8 {
        const instruction = self.frame.function.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return instruction;
    }

    pub fn runtimeError(self: *VirtualMachine, comptime format: []const u8, args: anytype) anyerror {
        std.debug.print(format, args);
        std.debug.print("\n", .{});
        var i: i32 = @as(i32, @intCast(self.frame_count)) - 1;
        while (i >= 0) : (i -= 1) {
            const frame = self.frames[@intCast(i)];
            const function = frame.function;
            const instruction = frame.ip - 1;
            std.debug.print("[line {d}] in ", .{function.chunk.lines.items[instruction]});
            if (function.name) |name| {
                std.debug.print("{s}()\n", .{name.chars});
            } else {
                std.debug.print("script\n", .{});
            }
        }

        self.resetStack();
        return error.RuntimeError;
    }

    fn defineNative(self: *VirtualMachine, name: []const u8, function: object.NativeFn) !void {
        self.push(value.wrapObj(try object.copyString(self, name)));
        self.push(value.wrapObj(try object.allocateNative(self, function)));
        try self.globals.put(self.gpa, self.stack[0].asObj().asString(), self.stack[1]);
        _ = self.pop();
        _ = self.pop();
    }

    inline fn readU16(self: *VirtualMachine) usize {
        const code = self.frame.function.chunk.code.items;
        const ip = self.frame.ip;
        const val = chunk.u16ToIndex(code[ip], code[ip + 1]);
        self.frame.ip += 2;
        return val;
    }

    inline fn readConstant(self: *VirtualMachine) value.Value {
        return self.frame.function.chunk.constants.items[self.readU16()];
    }

    inline fn readString(self: *VirtualMachine) *object.ObjString {
        const constant = self.readConstant();
        return constant.Obj.asString();
    }

    inline fn getSlot(self: *VirtualMachine, slot: usize) value.Value {
        return self.stack[self.frame.slot + slot];
    }

    inline fn setSlot(self: *VirtualMachine, slot: usize, val: value.Value) void {
        self.stack[self.frame.slot + slot] = val;
    }

    fn callValue(self: *VirtualMachine, callee: value.Value, argCount: u8) !void {
        if (callee != .Obj) {
            return self.runtimeError("Can't call value of type '{s}'", .{callee.getType()});
        }
        const obj = callee.Obj;
        switch (obj.type) {
            .Closure => {
                const closure = obj.asClosure();
                try self.call(closure.function, argCount, closure.upvalues);
            },
            .Function => try self.call(obj.asFunction(), argCount, null),
            .NativeFn => {
                const native_fn = obj.asNative();
                const start = self.stack_top - argCount;
                const result = try native_fn.function(self, argCount, self.stack[start..self.stack_top]);
                self.stack_top -= argCount + 1;
                self.push(result);
            },
            else => return self.runtimeError("Can't call object of type '{s}'", .{obj.getType()}),
        }
    }

    fn captureUpvalue(self: *VirtualMachine, local: *value.Value) !*object.ObjUpvalue {
        var prev_upvalue: ?*object.ObjUpvalue = null;
        var upvalue = self.open_upvalues;
        while (upvalue != null and @intFromPtr(upvalue.?.location) > @intFromPtr(local)) {
            prev_upvalue = upvalue;
            upvalue = upvalue.?.next;
        }

        if (upvalue != null and upvalue.?.location == local) return upvalue.?;

        const created_upvalue = try object.allocateUpvalue(self, local);
        created_upvalue.next = upvalue;

        if (prev_upvalue) |prev| {
            prev.next = created_upvalue;
        } else {
            self.open_upvalues = created_upvalue;
        }

        return created_upvalue;
    }

    fn closeUpvalues(self: *VirtualMachine, last: *value.Value) void {
        while (self.open_upvalues != null and @intFromPtr(self.open_upvalues.?.location) >= @intFromPtr(last)) {
            const upvalue = self.open_upvalues.?;
            upvalue.closed = upvalue.location.*;
            upvalue.location = &upvalue.closed.?;
            self.open_upvalues = upvalue.next;
        }
    }

    fn call(self: *VirtualMachine, function: *object.ObjFunction, argCount: u8, upvalues: ?[]?*object.ObjUpvalue) !void {
        if (argCount != function.arity) {
            return self.runtimeError("Expected {d} arguments but got {d}", .{ function.arity, argCount });
        }
        if (self.frame_count == constants.frames_max) {
            return self.runtimeError("Stack overflow.", .{});
        }
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.function = function;
        frame.upvalues = upvalues;
        frame.ip = 0;
        frame.slot = self.stack_top - argCount - 1;
    }

    fn resolveListIndex(self: *VirtualMachine, len: usize, index: i64) !usize {
        const signed_len = @as(i64, @intCast(len));
        const resolved = if (index < 0) signed_len + index else index;
        if (resolved < 0 or resolved >= signed_len) {
            return self.runtimeError("Index {d} out of bounds", .{index});
        }
        return @intCast(resolved);
    }

    pub fn run(self: *VirtualMachine) !void {
        var frame = &self.frames[self.frame_count - 1];
        var ip = frame.ip;

        while (true) {
            if (comptime constants.debug_trace_execution) {
                frame.ip = ip;
                std.debug.print("          ", .{});
                for (0..self.stack_top) |index| {
                    std.debug.print("[ {f} ]", .{self.stack[index]});
                }
                std.debug.print("\n", .{});
                _ = frame.function.chunk.disassembleInstruction(frame.ip);
            }

            const instruction = frame.function.chunk.code.items[ip];
            ip += 1;

            switch (@as(chunk.OpCode, @enumFromInt(instruction))) {
                .Constant => {
                    const idx = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    self.push(frame.function.chunk.constants.items[idx]);
                },
                .Null => self.push(value.valueNull()),
                .True => self.push(value.wrapBool(true)),
                .False => self.push(value.wrapBool(false)),
                .Add => {
                    if (self.peek(0).isObjType(.String) and self.peek(1).isObjType(.String)) {
                        try self.concatenate();
                    } else {
                        const b = self.pop();
                        const a = self.peek(0);
                        self.swapInPlace(try self.add(a, b), 0);
                    }
                },
                .Subtract => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.subtract(a, b), 0);
                },
                .Multiply => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.multiply(a, b), 0);
                },
                .Divide => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.divide(a, b), 0);
                },
                .Negate => {
                    const val = self.peek(0);
                    self.swapInPlace(try self.negate(val), 0);
                },
                .Not => {
                    const val = self.peek(0);
                    self.swapInPlace(value.wrapBool(isFalsey(val)), 0);
                },
                .Equal => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(value.wrapBool(try isEqual(a, b)), 0);
                },
                .Greater => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(value.wrapBool(try self.greater(a, b)), 0);
                },
                .Less => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(value.wrapBool(try self.less(a, b)), 0);
                },
                .BitwiseAnd => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.bitwiseInfix(a, b, .BitwiseAnd), 0);
                },
                .BitwiseOr => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.bitwiseInfix(a, b, .BitwiseOr), 0);
                },
                .BitwiseXor => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.bitwiseInfix(a, b, .BitwiseXor), 0);
                },
                .BitwiseNot => {
                    const val = self.peek(0);
                    if (val != .Int) {
                        return self.runtimeError("Invalid type in bitwise not: {s}", .{val.getType()});
                    }
                    self.swapInPlace(value.wrapInt(~val.Int), 0);
                },
                .Mod => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.mod(a, b), 0);
                },
                .LeftShift => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.bitwiseInfix(a, b, .LeftShift), 0);
                },
                .RightShift => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try self.bitwiseInfix(a, b, .RightShift), 0);
                },
                .Pop => {
                    _ = self.pop();
                },
                .DefineGlobal => {
                    const name_idx = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    const name = frame.function.chunk.constants.items[name_idx].Obj.asString();
                    try self.globals.put(self.gpa, name, self.peek(0));
                    _ = self.pop();
                },
                .GetGlobal => {
                    const name_idx = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    const name = frame.function.chunk.constants.items[name_idx].Obj.asString();
                    if (self.globals.get(name)) |val| {
                        self.push(val);
                    } else {
                        frame.ip = ip;
                        return self.runtimeError("Undefined variable '{s}'", .{name.chars});
                    }
                },
                .SetGlobal => {
                    const name_idx = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    const name = frame.function.chunk.constants.items[name_idx].Obj.asString();
                    if (self.globals.getPtr(name)) |val_ptr| {
                        val_ptr.* = self.pop();
                    } else {
                        frame.ip = ip;
                        return self.runtimeError("Undefined variable '{s}'", .{name.chars});
                    }
                },
                .GetLocal => {
                    const slot = frame.function.chunk.code.items[ip];
                    ip += 1;
                    self.push(self.stack[frame.slot + slot]);
                },
                .SetLocal => {
                    const slot = frame.function.chunk.code.items[ip];
                    ip += 1;
                    self.stack[frame.slot + slot] = self.pop();
                },
                .JumpIfFalse => {
                    const offset = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    if (isFalsey(self.peek(0))) ip += offset;
                },
                .JumpIfNotEq => {
                    const offset = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    if (!try isEqual(self.peek(1), self.peek(0))) {
                        ip += offset;
                    }
                },
                .JumpIfGreaterOrEq => {
                    const offset = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    if (!try self.less(self.peek(1), self.peek(0))) {
                        ip += offset;
                    }
                },
                .Jump => {
                    const offset = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    ip += offset;
                },
                .Loop => {
                    const offset = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    ip -= offset;
                },
                .Call => {
                    const arg_count = frame.function.chunk.code.items[ip];
                    ip += 1;
                    // sync ip before call since callValue may trigger GC or errors
                    frame.ip = ip;
                    try self.callValue(self.peek(arg_count), arg_count);
                    frame = &self.frames[self.frame_count - 1];
                    ip = frame.ip;
                },
                .Closure => {
                    const fn_idx = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    const function = frame.function.chunk.constants.items[fn_idx].asObj().asFunction();
                    const closure = try object.allocateClosure(self, function);
                    self.push(value.wrapObj(&closure.obj));

                    for (0..closure.upvalues.len) |i| {
                        const is_local = frame.function.chunk.code.items[ip] == 1;
                        ip += 1;
                        const index = frame.function.chunk.code.items[ip];
                        ip += 1;
                        if (is_local) {
                            closure.upvalues[i] = try self.captureUpvalue(&self.stack[frame.slot + index]);
                        } else {
                            closure.upvalues[i] = frame.upvalues.?[index];
                        }
                    }
                },
                .GetUpvalue => {
                    const slot = frame.function.chunk.code.items[ip];
                    ip += 1;
                    self.push(frame.upvalues.?[slot].?.location.*);
                },
                .SetUpvalue => {
                    const slot = frame.function.chunk.code.items[ip];
                    ip += 1;
                    frame.upvalues.?[slot].?.location.* = self.pop();
                },
                .CloseUpvalue => {
                    self.closeUpvalues(&self.stack[self.stack_top - 1]);
                    _ = self.pop();
                },
                .ListInit => {
                    const val = self.pop();
                    const list = val.asObj().asList();
                    const count = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    for (0..count) |_| {
                        try list.data.append(self.gpa, self.pop());
                    }
                    self.push(val);
                },
                .TableInit => {
                    const val = self.pop();
                    const table = val.asObj().asTable();
                    const count = chunk.u16ToIndex(
                        frame.function.chunk.code.items[ip],
                        frame.function.chunk.code.items[ip + 1],
                    );
                    ip += 2;
                    for (0..count) |_| {
                        try table.data.put(
                            self.gpa,
                            self.pop().asObj().asString(),
                            self.pop(),
                        );
                    }
                    self.push(val);
                },
                .IndexGet => {
                    const index = self.pop();
                    switch (index) {
                        .Int => |number_index| {
                            const val = self.pop();
                            if (!val.isObjType(.List)) {
                                frame.ip = ip;
                                return self.runtimeError(
                                    "Invalid left side in index expression: {s}",
                                    .{val.getType()},
                                );
                            }
                            const list_obj = val.asObj().asList();
                            const list_index = try self.resolveListIndex(list_obj.data.items.len, number_index);
                            self.push(list_obj.data.items[list_index]);
                        },
                        .Obj => |key| {
                            if (key.type != .String) {
                                frame.ip = ip;
                                return self.runtimeError(
                                    "Invalid index type in index expression: {s}",
                                    .{index.Obj.getType()},
                                );
                            }
                            const val = self.pop();
                            if (!val.isObjType(.Table)) {
                                frame.ip = ip;
                                return self.runtimeError(
                                    "Invalid left side in index expression: {s}",
                                    .{val.getType()},
                                );
                            }
                            const table_obj = val.asObj().asTable();
                            if (table_obj.data.get(key.asString())) |table_val| {
                                self.push(table_val);
                            } else {
                                self.push(value.valueNull());
                            }
                        },
                        else => {
                            frame.ip = ip;
                            return self.runtimeError(
                                "Invalid index type in index expression: {s}",
                                .{index.getType()},
                            );
                        },
                    }
                },
                .IndexSet => {
                    const val = self.pop();
                    const index = self.pop();

                    switch (index) {
                        .Int => |number_index| {
                            const list = self.peek(0);
                            if (!list.isObjType(.List)) {
                                frame.ip = ip;
                                return self.runtimeError(
                                    "Invalid left side in index expression: {s}",
                                    .{list.getType()},
                                );
                            }
                            const list_obj = list.asObj().asList();
                            const list_index = try self.resolveListIndex(list_obj.data.items.len, number_index);
                            list_obj.data.items[list_index] = val;
                        },
                        .Obj => |key| {
                            if (key.type != .String) {
                                frame.ip = ip;
                                return self.runtimeError(
                                    "Invalid index type in index expression: {s}",
                                    .{index.Obj.getType()},
                                );
                            }
                            const table = self.peek(0);
                            if (!table.isObjType(.Table)) {
                                frame.ip = ip;
                                return self.runtimeError(
                                    "Invalid left side in index expression: {s}",
                                    .{table.getType()},
                                );
                            }
                            const table_obj = table.asObj().asTable();
                            try table_obj.data.put(self.gpa, key.asString(), val);
                        },
                        else => {
                            frame.ip = ip;
                            return self.runtimeError(
                                "Invalid index type in index expression: {s}",
                                .{index.getType()},
                            );
                        },
                    }
                },
                .Return => {
                    const result = self.pop();
                    self.closeUpvalues(&self.stack[frame.slot]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return;
                    }
                    self.stack_top = frame.slot;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                    ip = frame.ip;
                },
            }
        }
    }

    fn mod(self: *VirtualMachine, a: value.Value, b: value.Value) !value.Value {
        switch (a) {
            .Int => |left| {
                switch (b) {
                    .Int => |right| return value.wrapInt(@rem(left, right)),
                    .Float => |right| return value.wrapFloat(@rem(@as(f64, @floatFromInt(left)), right)),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            .Float => |left| {
                switch (b) {
                    .Int => |right| return value.wrapFloat(@rem(left, @as(f64, @floatFromInt(right)))),
                    .Float => |right| return value.wrapFloat(@rem(left, right)),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            else => return self.runtimeError("Operand must be a number", .{}),
        }
    }

    fn bitwiseInfix(self: *VirtualMachine, a: value.Value, b: value.Value, op: chunk.OpCode) !value.Value {
        if (a != .Int) {
            return self.runtimeError("Invalid left side in operation {s}: {s}", .{ @tagName(op), a.getType() });
        }
        if (b != .Int) {
            return self.runtimeError("Invalid right side in operation {s}: {s}", .{ @tagName(op), b.getType() });
        }
        return switch (op) {
            .BitwiseAnd => value.wrapInt(a.Int & b.Int),
            .BitwiseOr => value.wrapInt(a.Int | b.Int),
            .BitwiseXor => value.wrapInt(a.Int ^ b.Int),
            .LeftShift => value.wrapInt(a.Int << @intCast(b.Int)),
            .RightShift => value.wrapInt(a.Int >> @intCast(b.Int)),
            else => unreachable,
        };
    }

    fn concatenate(self: *VirtualMachine) !void {
        const left = self.peek(1).asObj().asString().chars;
        const right = self.peek(0).asObj().asString().chars;

        const data: [2][]const u8 = .{ left, right };
        const chars = try std.mem.concat(self.gpa, u8, &data);
        errdefer self.gpa.free(chars);

        const hash = std.hash.Wyhash.hash(0, chars);
        const interned = self.findString(chars, hash);

        var result: value.Value = undefined;

        if (interned) |string_object| {
            self.gpa.free(chars);
            result = value.wrapObj(string_object);
        } else {
            result = value.wrapObj(try object.allocateString(self, chars, hash));
        }

        _ = self.pop();
        self.swapInPlace(result, 0);
    }

    fn add(self: *VirtualMachine, a: value.Value, b: value.Value) !value.Value {
        switch (a) {
            .Int => |left| {
                switch (b) {
                    .Int => |right| return value.wrapInt(left + right),
                    .Float => |right| return value.wrapFloat(@as(f64, @floatFromInt(left)) + right),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            .Float => |left| {
                switch (b) {
                    .Int => |right| return value.wrapFloat(left + @as(f64, @floatFromInt(right))),
                    .Float => |right| return value.wrapFloat(left + right),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            else => return self.runtimeError("Operand must be a number", .{}),
        }
    }

    fn greater(self: *VirtualMachine, a: value.Value, b: value.Value) !bool {
        switch (a) {
            .Int => |left| {
                switch (b) {
                    .Int => |right| return left > right,
                    .Float => |right| return @as(f64, @floatFromInt(left)) > right,
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            .Float => |left| {
                switch (b) {
                    .Int => |right| return left > @as(f64, @floatFromInt(right)),
                    .Float => |right| return left > right,
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            else => return self.runtimeError("Operand must be a number", .{}),
        }
    }

    fn less(self: *VirtualMachine, a: value.Value, b: value.Value) !bool {
        switch (a) {
            .Int => |left| {
                switch (b) {
                    .Int => |right| return left < right,
                    .Float => |right| return @as(f64, @floatFromInt(left)) < right,
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            .Float => |left| {
                switch (b) {
                    .Int => |right| return left < @as(f64, @floatFromInt(right)),
                    .Float => |right| return left < right,
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            else => return self.runtimeError("Operand must be a number", .{}),
        }
    }

    fn subtract(self: *VirtualMachine, a: value.Value, b: value.Value) !value.Value {
        switch (a) {
            .Int => |left| {
                switch (b) {
                    .Int => |right| return value.wrapInt(left - right),
                    .Float => |right| return value.wrapFloat(@as(f64, @floatFromInt(left)) - right),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            .Float => |left| {
                switch (b) {
                    .Int => |right| return value.wrapFloat(left - @as(f64, @floatFromInt(right))),
                    .Float => |right| return value.wrapFloat(left - right),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            else => return self.runtimeError("Operand must be a number", .{}),
        }
    }

    fn multiply(self: *VirtualMachine, a: value.Value, b: value.Value) !value.Value {
        switch (a) {
            .Int => |left| {
                switch (b) {
                    .Int => |right| return value.wrapInt(left * right),
                    .Float => |right| return value.wrapFloat(@as(f64, @floatFromInt(left)) * right),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            .Float => |left| {
                switch (b) {
                    .Float => |right| return value.wrapFloat(left * right),
                    .Int => |right| return value.wrapFloat(left * @as(f64, @floatFromInt(right))),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            else => return self.runtimeError("Operand must be a number", .{}),
        }
    }

    fn divide(self: *VirtualMachine, a: value.Value, b: value.Value) !value.Value {
        switch (a) {
            .Int => |left| {
                switch (b) {
                    .Int => |right| return value.wrapFloat(@as(f64, @floatFromInt(left)) / @as(f64, @floatFromInt(right))),
                    .Float => |right| return value.wrapFloat(@as(f64, @floatFromInt(left)) / right),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            .Float => |left| {
                switch (b) {
                    .Float => |right| return value.wrapFloat(left / right),
                    .Int => |right| return value.wrapFloat(left / @as(f64, @floatFromInt(right))),
                    else => return self.runtimeError("Right operand must be a number.", .{}),
                }
            },
            else => return self.runtimeError("Operand must be a number", .{}),
        }
    }

    fn negate(self: *VirtualMachine, val: value.Value) !value.Value {
        return switch (val) {
            .Float => |float| value.wrapFloat(-float),
            .Int => |int| value.wrapInt(-int),
            else => return self.runtimeError("Operand must be a number.", .{}),
        };
    }

    fn isFalsey(val: value.Value) bool {
        return val == .Null or (val == .Bool and !val.Bool);
    }

    fn isEqual(a: value.Value, b: value.Value) !bool {
        if (a == .Int and b == .Float) {
            return @as(f64, @floatFromInt(a.Int)) == b.Float;
        }
        if (a == .Float and b == .Int) {
            return a.Float == @as(f64, @floatFromInt(b.Int));
        }
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
            return false;
        }
        switch (a) {
            .Int => return a.Int == b.Int,
            .Float => return a.Float == b.Float,
            .Bool => return a.Bool == b.Bool,
            .Null => return true,
            .Obj => |obj| {
                if (obj.type != b.Obj.type) return false;
                const a_string = obj.asString();
                const b_string = b.Obj.asString();
                return a_string == b_string;
            },
        }
    }
};
