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
const memory = @import("memory.zig");

const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

pub const ScriptContext = struct {
    lines: std.ArrayList(scanner.Line),
    source: []const u8,
    file_name: []const u8,
    static_arena: std.mem.Allocator,

    pub fn appendLine(self: *ScriptContext, l: scanner.Line) !void {
        try self.lines.append(self.static_arena, l);
    }
};

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "VirtualMachine",
        .color = .Blue,
    });
}

const CallFrame = struct {
    function: *object.ObjFunction,
    upvalues: ?[]*object.ObjUpvalue,
    ip: usize,
    stack_offset: usize,
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

pub fn run(io: std.Io, gpa: std.mem.Allocator, static_arena: std.mem.Allocator, file_name: []const u8, source: []const u8) !InterpretResult {
    if (comptime constants.debug_trace_execution) logDebug("Starting pre-compilation...", .{});

    var script_context = ScriptContext{
        .file_name = file_name,
        .source = source,
        .lines = .empty,
        .static_arena = static_arena,
    };

    var ast_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    const ast = try parser.createAst(ast_arena.allocator(), &script_context);

    var vm = try static_arena.create(VirtualMachine);
    var cm = try static_arena.create(compiler.Compiler);

    var runtime_allocator = memory.RuntimeAllocator.init(gpa, vm, cm);
    const gc_alloc = runtime_allocator.allocator();

    vm.* = VirtualMachine.init(io, gpa, static_arena, &runtime_allocator, gc_alloc);
    cm.* = try compiler.Compiler.init(&script_context, &runtime_allocator, gc_alloc);

    defer vm.deinit();

    if (comptime constants.debug_trace_execution) logDebug("Compiling...", .{});

    const function = try cm.compile(ast);
    cm.deinit();
    ast_arena.deinit();

    if (comptime constants.debug_trace_execution) logDebug("Compilation finished.", .{});

    vm.push(value.wrapObj(&function.obj));
    try vm.call(function, 0, null);

    if (comptime constants.debug_trace_execution) logDebug("Running byte code...", .{});
    _ = vm.run() catch return .RuntimeError;
    if (comptime constants.debug_trace_execution) logDebug("Script finished.", .{});

    if (comptime constants.vm_test_mode) {
        if (vm.stack_top != 0) {
            logging.log("Stack not empty at end of program. {d} values remaining\n", .{vm.stack_top}, .{ .messageLevel = .Error });
            for (0..vm.stack_top) |i| {
                logging.log("  [{d}] {f}\n", .{ i, vm.stack[i] }, .{ .messageLevel = .Error });
            }
            return .RuntimeError;
        }
    }

    return .Ok;
}

pub const VirtualMachine = struct {
    io: std.Io,
    gpa: std.mem.Allocator,
    static_arena: std.mem.Allocator,
    stack: [constants.stack_max]value.Value,
    stack_top: usize,
    frames: [constants.frames_max]CallFrame,
    frame_count: usize,
    strings: std.HashMapUnmanaged(*object.ObjString, void, StringContext, 80),
    open_upvalues: ?*object.ObjUpvalue,
    runtime_allocator: *memory.RuntimeAllocator,
    gc_alloc: std.mem.Allocator,

    pub fn init(
        io: std.Io,
        gpa: std.mem.Allocator,
        static_arena: std.mem.Allocator,
        runtime_allocator: *memory.RuntimeAllocator,
        gc_alloc: std.mem.Allocator,
    ) VirtualMachine {
        return .{
            .io = io,
            .gpa = gpa,
            .static_arena = static_arena,
            .stack = undefined,
            .stack_top = 0,
            .frames = undefined,
            .frame_count = 0,
            .strings = .empty,
            .open_upvalues = null,
            .runtime_allocator = runtime_allocator,
            .gc_alloc = gc_alloc,
        };
    }

    pub fn deinit(self: *VirtualMachine) void {
        self.strings.deinit(self.gpa);
        self.runtime_allocator.deinit();
    }

    pub fn putString(self: *VirtualMachine, s: *object.ObjString) !void {
        try self.strings.put(self.gpa, s, {});
    }

    pub fn findString(self: *VirtualMachine, chars: []const u8, hash: u64) ?*object.Obj {
        var tmp = object.ObjString{ .chars = chars, .hash = hash };
        if (self.strings.getKey(&tmp)) |string_object| return &string_object.obj;
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

    inline fn peekTop(self: *VirtualMachine) value.Value {
        return self.peek(0);
    }

    inline fn swapInPlace(self: *VirtualMachine, val: value.Value, distance: usize) void {
        self.stack[self.stack_top - 1 - distance] = val;
    }

    inline fn swapTop(self: *VirtualMachine, val: value.Value) void {
        self.swapInPlace(val, 0);
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

    fn callValue(self: *VirtualMachine, callee: value.Value, argCount: u8) !void {
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
                const result = try native_fn.function(self, self.stack[start..self.stack_top]);
                self.stack_top -= argCount + 1;
                self.push(result);
            },
            else => return self.runtimeError("Can't call object of type '{s}'", .{obj.getType()}),
        }
    }

    fn captureUpvalue(self: *VirtualMachine, local: *value.Value) !*object.ObjUpvalue {
        var link: *?*object.ObjUpvalue = &self.open_upvalues;
        while (link.*) |u| {
            if (u.location == local) return u;
            if (@intFromPtr(u.location) < @intFromPtr(local)) break;
            link = &u.next;
        }
        const created = try self.runtime_allocator.allocateUpvalue(local);
        created.next = link.*;
        link.* = created;
        return created;
    }

    fn closeUpvalues(self: *VirtualMachine, last: *value.Value) void {
        while (self.open_upvalues) |u| {
            if (@intFromPtr(u.location) < @intFromPtr(last)) return;
            u.closed = u.location.*;
            u.location = &u.closed.?;
            self.open_upvalues = u.next;
        }
    }

    fn call(self: *VirtualMachine, function: *object.ObjFunction, argCount: u8, upvalues: ?[]*object.ObjUpvalue) !void {
        if (self.frame_count == constants.frames_max) {
            return self.runtimeError("stack overflow", .{});
        }
        const frame = &self.frames[self.frame_count];
        self.frame_count += 1;
        frame.function = function;
        frame.upvalues = upvalues;
        frame.ip = 0;
        frame.stack_offset = self.stack_top - argCount - 1;
    }

    pub inline fn readU16(ip: *usize, frame: *const CallFrame) usize {
        const idx = chunk.u16ToIndex(
            frame.function.chunk.code.items[ip.*],
            frame.function.chunk.code.items[ip.* + 1],
        );
        ip.* += 2;
        return idx;
    }

    pub inline fn readByte(ip: *usize, frame: *const CallFrame) u8 {
        const b = frame.function.chunk.code.items[ip.*];
        ip.* += 1;
        return b;
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

            const instruction = readByte(&ip, frame);

            switch (@as(chunk.OpCode, @fromBackingInt(@intCast(instruction)))) {
                .Constant => {
                    const idx = readU16(&ip, frame);
                    self.push(frame.function.chunk.constants.items[idx]);
                },
                .Unit => self.push(value.valueUnit()),
                .True => self.push(value.wrapBool(true)),
                .False => self.push(value.wrapBool(false)),
                .Cons => {
                    const b = self.pop();
                    const a = self.peekTop();
                    if (!b.isObjType(.List)) {
                        return self.runtimeError("invalid right side in cons operation: {s}", .{b.getType()});
                    }
                    const new_list = value.wrapObj(try b.asObj().asList().cloneAndPrepend(self.runtime_allocator, a));
                    self.swapTop(new_list);
                },
                .AddInt => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapInt(a.Int + b.Int));
                },
                .AddFloat => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapFloat(a.Float + b.Float));
                },
                .SubtractInt => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapInt(a.Int - b.Int));
                },
                .SubtractFloat => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapFloat(a.Float - b.Float));
                },
                .Concat => {
                    const result = try self.concatenate();
                    _ = self.pop();
                    self.swapTop(result);
                },
                .MultiplyInt => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapInt(a.Int * b.Int));
                },
                .MultiplyFloat => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapFloat(a.Float * b.Float));
                },
                .DivideInt => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapInt(@divTrunc(a.Int, b.Int)));
                },
                .DivideFloat => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapFloat(a.Float / b.Float));
                },
                .NegateInt => {
                    const val = self.peekTop();
                    self.swapTop(value.wrapInt(-val.Int));
                },
                .NegateFloat => {
                    const val = self.peekTop();
                    self.swapTop(value.wrapFloat(-val.Float));
                },
                .Not => {
                    const val = self.peekTop();
                    self.swapTop(value.wrapBool(isFalsey(val)));
                },
                .Equal => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapBool(try isEqual(a, b)));
                },
                .GreaterInt => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapBool(a.Int > b.Int));
                },
                .GreaterFloat => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapBool(a.Float > b.Float));
                },
                .GreaterString => {
                    @panic("not implemented");
                },
                .LessInt => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapBool(a.Int < b.Int));
                },
                .LessFloat => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapBool(a.Float < b.Float));
                },
                .LessString => {
                    @panic("not implemented");
                },
                .BitwiseAnd => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(bitwiseInfix(a, b, .BitwiseAnd));
                },
                .BitwiseOr => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(bitwiseInfix(a, b, .BitwiseOr));
                },
                .BitwiseXor => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(bitwiseInfix(a, b, .BitwiseXor));
                },
                .BitwiseNot => {
                    const val = self.peekTop();
                    if (val != .Int) {
                        return self.runtimeError("Invalid type in bitwise not: {s}", .{val.getType()});
                    }
                    self.swapTop(value.wrapInt(~val.Int));
                },
                .ModInt => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapInt(@rem(a.Int, b.Int)));
                },
                .ModFloat => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(value.wrapFloat(@rem(a.Float, b.Float)));
                },
                .LeftShift => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(bitwiseInfix(a, b, .LeftShift));
                },
                .RightShift => {
                    const b = self.pop();
                    const a = self.peekTop();
                    self.swapTop(bitwiseInfix(a, b, .RightShift));
                },
                .Pop => {
                    _ = self.pop();
                },
                .SwapPop => {
                    self.swapInPlace(self.peekTop(), 1);
                    _ = self.pop();
                },
                .GetLocal => {
                    const slot = readByte(&ip, frame);
                    self.push(self.stack[frame.stack_offset + slot]);
                },
                .JumpIfFalse => {
                    const offset = readU16(&ip, frame);
                    if (isFalsey(self.peekTop())) ip += offset;
                },
                .JumpIfNotEq => {
                    const offset = readU16(&ip, frame);
                    if (!try isEqual(self.peek(1), self.peekTop())) {
                        ip += offset;
                    }
                },
                .IsEmpty => {
                    const val = self.peekTop();
                    if (!val.isObjType(.List)) {
                        return self.runtimeError("can't check value of type {s} for emptiness", .{val.getType()});
                    }
                    self.push(value.wrapBool(val.asObj().asList().isEmpty()));
                },
                .Jump => {
                    const offset = readU16(&ip, frame);
                    ip += offset;
                },
                .Call => {
                    const arg_count = readByte(&ip, frame);
                    frame.ip = ip;
                    try self.callValue(self.peek(arg_count), arg_count);
                    frame = &self.frames[self.frame_count - 1];
                    ip = frame.ip;
                },
                .TailCall => {
                    const arg_count = readByte(&ip, frame);
                    const offset = self.stack_top - frame.stack_offset - 1 - arg_count;
                    for (0..arg_count) |i| {
                        self.swapInPlace(self.peek(i), offset + i);
                    }
                    self.stack_top = frame.stack_offset + 1 + arg_count;
                    frame.ip = 0;
                    ip = 0;
                },
                .Closure => {
                    const function_index = readU16(&ip, frame);
                    const function = frame.function.chunk.constants.items[function_index].asObj().asFunction();

                    // allocate the list first, so when the gc runs during the building of the list it does
                    // not try to mark a half finished list
                    const upvalues = try self.gc_alloc.alloc(*object.ObjUpvalue, function.upvalue_count);
                    for (0..function.upvalue_count) |i| {
                        const is_local = readByte(&ip, frame) == 1;
                        const index = readByte(&ip, frame);
                        if (is_local) {
                            upvalues[i] = try self.captureUpvalue(&self.stack[frame.stack_offset + index]);
                        } else {
                            upvalues[i] = frame.upvalues.?[index];
                        }
                    }

                    const closure = try self.runtime_allocator.allocateClosure(function);
                    closure.upvalues = upvalues;
                    self.push(value.wrapObj(&closure.obj));
                },
                .GetUpvalue => {
                    const index = readByte(&ip, frame);
                    self.push(frame.upvalues.?[index].location.*);
                },
                .SwapCloseUpvalue => {
                    const val = self.pop();
                    self.closeUpvalues(&self.stack[self.stack_top - 1]);
                    _ = self.pop();
                    self.push(val);
                },
                .ListInit => {
                    const val = self.pop();
                    const list = val.asObj().asList();
                    const count = readU16(&ip, frame);
                    for (0..count) |_| {
                        try list.prepend(self.runtime_allocator, self.pop());
                    }
                    self.push(val);
                },
                .Return => {
                    const result = self.pop();
                    self.closeUpvalues(&self.stack[frame.stack_offset]);
                    self.frame_count -= 1;
                    if (self.frame_count == 0) {
                        _ = self.pop();
                        return;
                    }
                    self.stack_top = frame.stack_offset;
                    self.push(result);
                    frame = &self.frames[self.frame_count - 1];
                    ip = frame.ip;
                },
                .CloneAndPopFirst => {
                    const list = self.pop().asObj().asList();
                    const result = try list.cloneAndPopFirst(self.runtime_allocator);
                    self.push(result.value.?);
                    self.push(value.wrapObj(&result.new_list.obj));
                },
            }
        }
    }

    fn bitwiseInfix(a: value.Value, b: value.Value, op: chunk.OpCode) value.Value {
        return switch (op) {
            .BitwiseAnd => value.wrapInt(a.Int & b.Int),
            .BitwiseOr => value.wrapInt(a.Int | b.Int),
            .BitwiseXor => value.wrapInt(a.Int ^ b.Int),
            .LeftShift => value.wrapInt(a.Int << @intCast(b.Int)),
            .RightShift => value.wrapInt(a.Int >> @intCast(b.Int)),
            else => unreachable,
        };
    }

    fn concatenate(self: *VirtualMachine) !value.Value {
        const left = self.peek(1).asObj().asString().chars;
        const right = self.peekTop().asObj().asString().chars;

        const data: [2][]const u8 = .{ left, right };
        const chars = try std.mem.concat(self.gc_alloc, u8, &data);
        errdefer self.gc_alloc.free(chars);

        const hash = std.hash.Wyhash.hash(0, chars);
        const interned = self.findString(chars, hash);

        var result: value.Value = undefined;
        if (interned) |string_object| {
            self.gc_alloc.free(chars);
            result = value.wrapObj(string_object);
        } else {
            result = value.wrapObj(try self.runtime_allocator.allocateString(chars, hash));
        }

        return result;
    }

    fn isFalsey(val: value.Value) bool {
        return val == .Bool and !val.Bool;
    }

    fn isEqual(a: value.Value, b: value.Value) !bool {
        if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
            return false;
        }
        switch (a) {
            .Int => return a.Int == b.Int,
            .Float => return a.Float == b.Float,
            .Bool => return a.Bool == b.Bool,
            .Unit => return true,
            .Obj => |obj| {
                if (obj.type != b.Obj.type) return false;
                const a_string = obj.asString();
                const b_string = b.Obj.asString();
                return a_string == b_string;
            },
        }
    }
};
