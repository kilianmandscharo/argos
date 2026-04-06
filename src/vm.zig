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
    closure: *object.ObjClosure,
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
        logDebug("Init vm...", .{});

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

        logDebug("Vm initialized.", .{});

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

    fn push(self: *VirtualMachine, val: value.Value) void {
        self.stack[self.stack_top] = val;
        self.stack_top += 1;
    }

    fn pop(self: *VirtualMachine) value.Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    fn peek(self: *VirtualMachine, distance: usize) value.Value {
        return self.stack[self.stack_top - 1 - distance];
    }

    fn swapInPlace(self: *VirtualMachine, val: value.Value, distance: usize) void {
        self.stack[self.stack_top - 1 - distance] = val;
    }

    pub fn interpret(self: *VirtualMachine, source: []const u8) !InterpretResult {
        logDebug("Starting pre-compilation...", .{});
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

        const ast = try parser.createAst(arena.allocator(), source);

        var c: compiler.Compiler = undefined;
        try compiler.Compiler.init(&c, self, self.gpa, .Script, null, 0);
        logDebug("Pre-compilation finished.", .{});

        self.current_compiler = &c;

        logDebug("Compiling...", .{});
        const function = try c.compile(ast);
        arena.deinit();
        logDebug("Compilation finished.", .{});

        logDebug("Setting up global function...", .{});
        self.push(value.wrapObj(&function.obj));
        const closure = try object.allocateClosure(self, function);
        _ = self.pop();
        self.push(value.wrapObj(&closure.obj));
        try self.call(closure, 0);
        logDebug("Global function set up.", .{});

        logDebug("Running byte code...", .{});
        _ = self.run() catch {
            return .RuntimeError;
        };
        logDebug("Script finished.", .{});

        return .Ok;
    }

    pub inline fn readByte(self: *VirtualMachine) u8 {
        const instruction = self.frame.closure.function.chunk.code.items[self.frame.ip];
        self.frame.ip += 1;
        return instruction;
    }

    fn runtimeError(self: *VirtualMachine, comptime format: []const u8, args: anytype) anyerror {
        std.debug.print(format, args);
        std.debug.print("\n", .{});
        var i: i32 = @as(i32, @intCast(self.frame_count)) - 1;
        while (i >= 0) : (i -= 1) {
            const frame = self.frames[@intCast(i)];
            const function = frame.closure.function;
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

    fn readU24(self: *VirtualMachine) usize {
        return chunk.u24ToIndex(self.readByte(), self.readByte(), self.readByte());
    }

    fn readU16(self: *VirtualMachine) usize {
        return chunk.u16ToIndex(self.readByte(), self.readByte());
    }

    fn readConstant(self: *VirtualMachine) value.Value {
        return self.frame.closure.function.chunk.constants.items[self.readU24()];
    }

    fn readString(self: *VirtualMachine) *object.ObjString {
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
        switch (callee) {
            .Obj => |obj| {
                switch (obj.type) {
                    .Closure => try self.call(obj.asClosure(), argCount),
                    .NativeFn => {
                        const native_fn = obj.asNative();
                        const start = self.stack_top - argCount;
                        const result = native_fn.function(argCount, self.stack[start..self.stack_top]);
                        self.stack_top -= argCount + 1;
                        self.push(result);
                    },
                    else => return self.runtimeError("Can't call object of type '{s}'", .{obj.getType()}),
                }
            },
            else => return self.runtimeError("Can't call value of type '{s}'", .{callee.getType()}),
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

    fn call(self: *VirtualMachine, closure: *object.ObjClosure, argCount: u8) !void {
        if (argCount != closure.function.arity) {
            return self.runtimeError("Expected {d} arguments but got {d}", .{ closure.function.arity, argCount });
        }
        if (self.frame_count == constants.frames_max) {
            return self.runtimeError("Stack overflow.", .{});
        }
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.closure = closure;
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
        self.frame = &self.frames[self.frame_count - 1];

        while (true) {
            if (comptime constants.debug_trace_execution) {
                std.debug.print("          ", .{});
                for (0..self.stack_top) |index| {
                    std.debug.print("[ {f} ]", .{self.stack[index]});
                }
                std.debug.print("\n", .{});
                _ = self.frame.closure.function.chunk.disassembleInstruction(self.frame.ip);
            }
            const instruction = self.readByte();
            switch (@as(chunk.OpCode, @enumFromInt(instruction))) {
                .Constant => self.push(self.readConstant()),
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
                .Print => {
                    const val = self.pop();
                    std.debug.print("{f}\n", .{val});
                },
                .Assert => {
                    const val = self.pop();
                    if (val != .Bool) return self.runtimeError("Expected Boolean, got {s}", .{val.getType()});
                    if (!val.Bool) return self.runtimeError("Assertion failed", .{});
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
                    if (self.globals.get(name)) |val| {
                        self.push(val);
                    } else {
                        return self.runtimeError("Undefined variable '{s}'", .{name.chars});
                    }
                },
                .SetGlobal => {
                    const name = self.readString();
                    if (self.globals.getPtr(name)) |val_ptr| {
                        val_ptr.* = self.pop();
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
                    self.setSlot(slot, self.pop());
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
                    if (!try self.less(self.peek(1), self.peek(0))) {
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
                .Closure => {
                    const function = self.readConstant().asObj().asFunction();
                    const closure = try object.allocateClosure(self, function);
                    self.push(value.wrapObj(&closure.obj));

                    for (0..closure.upvalues.len) |i| {
                        const is_local = self.readByte() == 1;
                        const index = self.readByte();
                        if (is_local) {
                            const slot = self.frame.slot + index;
                            closure.upvalues[i] = try self.captureUpvalue(&self.stack[slot]);
                        } else {
                            closure.upvalues[i] = self.frame.closure.upvalues[index];
                        }
                    }
                },
                .GetUpvalue => {
                    const slot = self.readByte();
                    self.push(self.frame.closure.upvalues[slot].?.location.*);
                },
                .SetUpvalue => {
                    const slot = self.readByte();
                    self.frame.closure.upvalues[slot].?.location.* = self.pop();
                },
                .CloseUpvalue => {
                    self.closeUpvalues(&self.stack[self.stack_top - 1]);
                    _ = self.pop();
                },
                .ListInit => {
                    const val = self.pop();
                    const list = val.asObj().asList();
                    const count = self.readU24();
                    // TODO: iterating twice is not ideal
                    for (self.stack_top - count..self.stack_top) |i| {
                        try list.data.append(self.gpa, self.stack[i]);
                    }
                    for (0..count) |_| {
                        _ = self.pop();
                    }
                    self.push(val);
                },
                .ListGet => {
                    const index = self.pop();
                    if (index != .Int) {
                        return self.runtimeError(
                            "Invalid index type in list index expression: {s}",
                            .{index.getType()},
                        );
                    }
                    const list = self.pop();
                    if (!list.isObjType(.List)) {
                        return self.runtimeError(
                            "Invalid left side in index expression: {s}",
                            .{list.getType()},
                        );
                    }
                    const list_obj = list.asObj().asList();
                    const list_index = try self.resolveListIndex(list_obj.data.items.len, index.Int);
                    self.push(list_obj.data.items[list_index]);
                },
                .ListSet => {
                    const val = self.pop();
                    const index = self.pop();
                    if (index != .Int) {
                        return self.runtimeError(
                            "Invalid index type in list index expression: {s}",
                            .{index.getType()},
                        );
                    }
                    const list = self.peek(0);
                    if (!list.isObjType(.List)) {
                        return self.runtimeError(
                            "Invalid left side in index expression: {s}",
                            .{list.getType()},
                        );
                    }
                    const list_obj = list.asObj().asList();
                    const list_index = try self.resolveListIndex(list_obj.data.items.len, index.Int);
                    list_obj.data.items[list_index] = val;
                },
                .Return => {
                    const result = self.pop();
                    self.closeUpvalues(&self.stack[self.frame.slot]);
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
};
