const std = @import("std");
const chunk_module = @import("chunk.zig");
const value_module = @import("value.zig");
const object_module = @import("object.zig");
const compiler_module = @import("compiler.zig");
const scanner_module = @import("scanner.zig");

const Scanner = scanner_module.Scanner;

const ObjString = object_module.ObjString;
const ObjFunction = object_module.ObjFunction;
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
const u24ToIndex = chunk_module.u24ToIndex;
const u16ToIndex = chunk_module.u16ToIndex;

const Compiler = compiler_module.Compiler;
const Parser = compiler_module.Parser;

const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

const DEBUG_TRACE_EXECUTION = true;

const FRAMES_MAX = 64;
const STACK_MAX = std.math.maxInt(u8) * FRAMES_MAX;

const CallFrame = struct {
    function: *ObjFunction,
    ip: usize,
    slot: usize,
};

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
    gpa: std.mem.Allocator,
    stack: [STACK_MAX]Value,
    stack_top: usize,
    frames: [FRAMES_MAX]CallFrame,
    frame_count: usize,
    frame: *CallFrame,
    strings: std.HashMapUnmanaged(*ObjString, void, StringContext, 80),
    globals: std.HashMapUnmanaged(*ObjString, Value, StringContext, 80),
    objects: ?*Obj,

    pub fn init(gpa: std.mem.Allocator) VirtualMachine {
        return VirtualMachine{
            .gpa = gpa,
            .stack = undefined,
            .stack_top = 0,
            .frames = undefined,
            .frame = undefined,
            .frame_count = 0,
            .strings = .{},
            .globals = .{},
            .objects = null,
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
        self.frame_count = 0;
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

    pub fn interpret(self: *VirtualMachine, source: []const u8) !InterpretResult {
        var scanner = Scanner.init(source);
        var parser = Parser.init(&scanner);
        var compiler = try Compiler.init(self, self.gpa, &parser, .Script, null);

        const function = try compiler.compile();

        self.push(wrapObj(&function.obj));
        try self.call(function, 0);

        _ = self.run() catch {
            return .RuntimeError;
        };
        return .Ok;
    }

    pub inline fn readByte(self: *VirtualMachine) u8 {
        const instruction = self.frame.function.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return instruction;
    }

    fn runtimeError(self: *VirtualMachine, comptime format: []const u8, args: anytype) anyerror {
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

    fn readU24(self: *VirtualMachine) usize {
        return u24ToIndex(self.readByte(), self.readByte(), self.readByte());
    }

    fn readU16(self: *VirtualMachine) usize {
        return u16ToIndex(self.readByte(), self.readByte());
    }

    fn readConstant(self: *VirtualMachine) Value {
        return self.frame.function.chunk.constants.items[self.readU24()];
    }

    fn readString(self: *VirtualMachine) *ObjString {
        const constant = self.readConstant();
        return constant.Obj.asString();
    }

    inline fn getSlot(self: *VirtualMachine, slot: usize) Value {
        return self.stack[self.frame.slot + slot];
    }

    inline fn setSlot(self: *VirtualMachine, slot: usize, value: Value) void {
        self.stack[self.frame.slot + slot] = value;
    }

    fn callValue(self: *VirtualMachine, callee: Value, argCount: u8) !void {
        if (!callee.isObjType(.Function)) {
            return self.runtimeError("Can only call functions.", .{});
        }
        try self.call(callee.asObj().asFunction(), argCount);
    }

    fn call(self: *VirtualMachine, function: *ObjFunction, argCount: u8) !void {
        if (argCount != function.arity) {
            return self.runtimeError("Expected {d} arguments but got {d}", .{ function.arity, argCount });
        }
        if (self.frame_count == FRAMES_MAX) {
            return self.runtimeError("Stack overflow.", .{});
        }
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.function = function;
        frame.ip = 0;
        frame.slot = self.stack_top - argCount - 1;
    }

    pub fn run(self: *VirtualMachine) !void {
        self.frame = &self.frames[self.frame_count - 1];

        while (true) {
            if (comptime DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (0..self.stack_top) |index| {
                    std.debug.print("[ {f} ]", .{self.stack[index]});
                }
                std.debug.print("\n", .{});
                _ = self.frame.function.chunk.disassembleInstruction(self.frame.ip);
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
                    self.swapInPlace(wrapBool(try isEqual(a, b)), 0);
                },
                .Greater => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(wrapBool(try greater(self, a, b)), 0);
                },
                .Less => {
                    const b = self.pop();
                    const a = self.peek(0);
                    self.swapInPlace(wrapBool(try less(self, a, b)), 0);
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
                    const slot = self.readU24();
                    self.push(self.getSlot(slot));
                },
                .SetLocal => {
                    const slot = self.readU24();
                    self.setSlot(slot, self.peek(0));
                },
                .JumpIfFalse => {
                    const offset = self.readU16();
                    if (isFalsey(self.peek(0))) self.frame.ip += offset;
                },
                .JumpIfNotEq => {
                    const offset = self.readU16();
                    if (!try isEqual(self.peek(1), self.peek(0))) {
                        self.frame.ip += offset;
                    }
                },
                .JumpIfGreaterOrEq => {
                    const offset = self.readU16();
                    if (!try less(self, self.peek(1), self.peek(0))) {
                        self.frame.ip += offset;
                    }
                },
                .Jump => {
                    const offset = self.readU16();
                    self.frame.ip += offset;
                },
                .Loop => {
                    const offset = self.readU16();
                    self.frame.ip -= offset;
                },
                .Call => {
                    const arg_count = self.readByte();
                    try self.callValue(self.peek(arg_count), arg_count);
                    self.frame = &self.frames[self.frame_count - 1];
                },
                .Return => {
                    const result = self.pop();
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return;
                    }
                    self.stack_top = self.frame.slot;
                    self.push(result);
                    self.frame = &self.frames[self.frame_count - 1];
                },
            }
        }
    }
};

fn greater(vm: *VirtualMachine, a: Value, b: Value) !bool {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return left > right,
                .Float => |right| return @as(f64, @floatFromInt(left)) > right,
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Int => |right| return left > @as(f64, @floatFromInt(right)),
                .Float => |right| return left > right,
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        else => return vm.runtimeError("Operand must be a number", .{}),
    }
}

fn less(vm: *VirtualMachine, a: Value, b: Value) !bool {
    switch (a) {
        .Int => |left| {
            switch (b) {
                .Int => |right| return left < right,
                .Float => |right| return @as(f64, @floatFromInt(left)) < right,
                else => return vm.runtimeError("Right operand must be a number.", .{}),
            }
        },
        .Float => |left| {
            switch (b) {
                .Int => |right| return left < @as(f64, @floatFromInt(right)),
                .Float => |right| return left < right,
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

fn isEqual(a: Value, b: Value) !bool {
    // TODO: should we really return false for 3 == 3.0?
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
