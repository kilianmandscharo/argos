const std = @import("std");
const chunk_module = @import("chunk.zig");
const value_module = @import("value.zig");
const object_module = @import("object.zig");
const compiler_module = @import("compiler.zig");

const ObjString = object_module.ObjString;
const Obj = object_module.Obj;
const allocateString = object_module.allocateString;

const Value = value_module.Value;
const wrapFloat = value_module.wrapFloat;
const wrapInt = value_module.wrapInt;
const wrapBool = value_module.wrapBool;
const wrapObj = value_module.wrapObj;
const valueNull = value_module.valueNull;

const OpCode = chunk_module.OpCode;
const Chunk = chunk_module.Chunk;
const bytesToIndex = chunk_module.bytesToIndex;

const Compiler = compiler_module.Compiler;

const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

const DEBUG_PRINT_CODE = true;
const DEBUG_TRACE_EXECUTION = true;
const STACK_MAX = 256;

const GlobalContext = struct {
    pub fn hash(_: @This(), key: *ObjString) u64 {
        return key.hash;
    }

    pub fn eql(_: @This(), a: *ObjString, b: *ObjString) bool {
        return a == b;
    }
};

const StringContext = struct {
    pub fn hash(_: @This(), key: *ObjString) u64 {
        return key.hash;
    }

    pub fn eql(_: @This(), a: *ObjString, b: *ObjString) bool {
        return std.mem.eql(u8, a.chars, b.chars);
    }
};

pub const VirtualMachine = struct {
    chunk: *Chunk,
    ip: usize,
    stack: [STACK_MAX]Value,
    stack_top: usize,
    gpa: std.mem.Allocator,
    objects: ?*Obj,
    strings: std.HashMapUnmanaged(*ObjString, void, StringContext, 80),
    globals: std.HashMapUnmanaged(*ObjString, Value, StringContext, 80),

    pub fn init(gpa: std.mem.Allocator) VirtualMachine {
        return VirtualMachine{
            .chunk = undefined,
            .ip = 0,
            .stack = undefined,
            .stack_top = 0,
            .gpa = gpa,
            .objects = null,
            .strings = .{},
            .globals = .{},
        };
    }

    pub fn deinit(self: *VirtualMachine) void {
        var obj = self.objects;
        while (obj) |object| {
            const next = object.next;
            object.deinit(self.gpa);
            obj = next;
        }
        self.strings.deinit(self.gpa);
        self.globals.deinit(self.gpa);
    }

    pub fn findString(self: *VirtualMachine, chars: []const u8, hash: u64) ?*Obj {
        var tmp = ObjString{
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
    }

    fn push(self: *VirtualMachine, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VirtualMachine) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VirtualMachine, distance: usize) Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    fn swapInPlace(self: *VirtualMachine, value: Value, distance: usize) void {
        self.stack[self.stack_top - 1 - distance] = value;
    }

    pub fn interpret(self: *VirtualMachine, chunk: *Chunk, source: []const u8) !InterpretResult {
        var compiler = Compiler.init(self, self.gpa);
        try compiler.compile(chunk, source);
        self.chunk = chunk;
        if (comptime DEBUG_PRINT_CODE) chunk.disassemble();
        return try self.run();
    }

    pub inline fn readByte(self: *VirtualMachine) u8 {
        const instruction = self.chunk.code.items[self.ip];
        self.ip += 1;
        return instruction;
    }

    fn runtimeError(self: *VirtualMachine, comptime format: []const u8, args: anytype) anyerror {
        const line = self.chunk.lines.items[self.ip - 1];
        std.debug.print(format, args);
        std.debug.print("\n[line {d}] in script\n", .{line});
        self.resetStack();
        return error.RuntimeError;
    }

    fn readIndex(self: *VirtualMachine) usize {
        return bytesToIndex(self.readByte(), self.readByte(), self.readByte());
    }

    fn readConstant(self: *VirtualMachine) Value {
        const index = bytesToIndex(self.readByte(), self.readByte(), self.readByte());
        return self.chunk.constants.items[index];
    }

    fn readString(self: *VirtualMachine) *ObjString {
        const constant = self.readConstant();
        return constant.Obj.asString();
    }

    pub fn run(self: *VirtualMachine) !InterpretResult {
        while (true) {
            if (comptime DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (0..self.stack_top) |index| {
                    std.debug.print("[ {f} ]", .{self.stack[index]});
                }
                std.debug.print("\n", .{});
                _ = self.chunk.disassembleInstruction(self.ip);
            }
            const instruction = self.readByte();
            switch (@as(OpCode, @enumFromInt(instruction))) {
                .Constant => self.push(self.readConstant()),
                .Null => self.push(valueNull()),
                .True => self.push(wrapBool(true)),
                .False => self.push(wrapBool(false)),
                .Add => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try add(self, a, b), 0);
                },
                .Subtract => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try subtract(self, a, b), 0);
                },
                .Multiply => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try multiply(self, a, b), 0);
                },
                .Divide => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try divide(self, a, b), 0);
                },
                .Negate => {
                    const value = self.peek(0);
                    self.swapInPlace(try negate(self, value), 0);
                },
                .Not => {
                    const value = self.peek(0);
                    self.swapInPlace(wrapBool(isFalsey(value)), 0);
                },
                .Equal => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(wrapBool(try isEqual(self, a, b)), 0);
                },
                .Greater => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try greater(self, a, b), 0);
                },
                .Less => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(try less(self, a, b), 0);
                },
                .Print => {
                    const value = self.pop();
                    std.debug.print("{f}\n", .{value});
                },
                .Assert => {
                    const value = self.pop();
                    if (value != .Bool) return self.runtimeError("Expected Boolean, got {s}", .{value.getType()});
                    if (!value.Bool) return self.runtimeError("Assertion failed", .{});
                },
                .Pop => {
                    _ = self.pop();
                },
                .DefineGlobal => {
                    const name = self.readString();
                    try self.globals.put(self.gpa, name, self.peek(0));
                    _ = self.pop();
                },
                .GetGlobal => {
                    const name = self.readString();
                    if (self.globals.get(name)) |value| {
                        self.push(value);
                    } else {
                        return self.runtimeError("Undefined variable '{s}'", .{name.chars});
                    }
                },
                .SetGlobal => {
                    const name = self.readString();
                    if (self.globals.getPtr(name)) |val_ptr| {
                        val_ptr.* = self.peek(0);
                    } else {
                        return self.runtimeError("Undefined variable '{s}'", .{name.chars});
                    }
                },
                .GetLocal => {
                    const slot = self.readIndex();
                    self.push(self.stack[slot]);
                },
                .SetLocal => {
                    const slot = self.readIndex();
                    self.stack[slot] = self.peek(0);
                },
                .Return => {
                    return .Ok;
                },
            }
        }
    }
};

fn greater(vm: *VirtualMachine, a: Value, b: Value) !Value {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return wrapBool(left > right),
                .Float => |right| return wrapBool(@as(f64, @floatFromInt(left)) > right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Int => |right| return wrapBool(left > @as(f64, @floatFromInt(right))),
                .Float => |right| return wrapBool(left > right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        else => return vm.runtimeError("Operand must be a number", .{}),
    }
}

fn less(vm: *VirtualMachine, a: Value, b: Value) !Value {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return wrapBool(left < right),
                .Float => |right| return wrapBool(@as(f64, @floatFromInt(left)) < right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Int => |right| return wrapBool(left < @as(f64, @floatFromInt(right))),
                .Float => |right| return wrapBool(left < right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        else => return vm.runtimeError("Operand must be a number", .{}),
    }
}

fn subtract(vm: *VirtualMachine, a: Value, b: Value) !Value {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return wrapInt(left - right),
                .Float => |right| return wrapFloat(@as(f64, @floatFromInt(left)) - right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Int => |right| return wrapFloat(left - @as(f64, @floatFromInt(right))),
                .Float => |right| return wrapFloat(left - right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        else => return vm.runtimeError("Operand must be a number", .{}),
    }
}

fn add(vm: *VirtualMachine, a: Value, b: Value) !Value {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return wrapInt(left + right),
                .Float => |right| return wrapFloat(@as(f64, @floatFromInt(left)) + right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Int => |right| return wrapFloat(left + @as(f64, @floatFromInt(right))),
                .Float => |right| return wrapFloat(left + right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Obj => |left| {
            switch (b) {
                .Obj => |right| {
                    if (!left.isString() or !right.isString()) {
                        return vm.runtimeError("Both operands must be strings", .{});
                    }
                    return try concatenateStrings(vm, left, right);
                },
                else => return vm.runtimeError("Right operand must be an Obj.", .{}),
            }
        },
        else => return vm.runtimeError("Operand must be a number", .{}),
    }
}

fn multiply(vm: *VirtualMachine, a: Value, b: Value) !Value {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return wrapInt(left * right),
                .Float => |right| return wrapFloat(@as(f64, @floatFromInt(left)) * right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Float => |right| return wrapFloat(left * right),
                .Int => |right| return wrapFloat(left * @as(f64, @floatFromInt(right))),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        else => return vm.runtimeError("Operand must be a number", .{}),
    }
}

fn divide(vm: *VirtualMachine, a: Value, b: Value) !Value {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return wrapFloat(@as(f64, @floatFromInt(left)) / @as(f64, @floatFromInt(right))),
                .Float => |right| return wrapFloat(@as(f64, @floatFromInt(left)) / right),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Float => |right| return wrapFloat(left / right),
                .Int => |right| return wrapFloat(left / @as(f64, @floatFromInt(right))),
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        else => return vm.runtimeError("Operand must be a number", .{}),
    }
}

fn negate(vm: *VirtualMachine, value: Value) !Value {
    return switch (value) {
        .Float => |val| wrapFloat(-val),
        .Int => |val| wrapInt(-val),
        else => return vm.runtimeError("Operand must be a number.", .{}),
    };
}

fn isFalsey(value: Value) bool {
    return value == .Null or (value == .Bool and !value.Bool);
}

fn isEqual(vm: *VirtualMachine, a: Value, b: Value) !bool {
    if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
        return vm.runtimeError("Can't compare {s} and {s}", .{ a.getType(), b.getType() });
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

fn concatenateStrings(vm: *VirtualMachine, left: *Obj, right: *Obj) !Value {
    const left_string = left.asString().chars;
    const right_string = right.asString().chars;

    const data: [2][]const u8 = .{ left_string, right_string };
    const chars = try std.mem.concat(vm.gpa, u8, &data);
    errdefer vm.gpa.free(chars);

    const hash = std.hash.Wyhash.hash(0, chars);
    const interned = vm.findString(chars, hash);

    if (interned) |string_object| {
        vm.gpa.free(chars);
        return wrapObj(string_object);
    }

    return wrapObj(try allocateString(vm, chars, hash));
}
