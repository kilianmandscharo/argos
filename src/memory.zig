const std = @import("std");
const logging = @import("logging.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const virtual_machine = @import("vm.zig");
const compiler = @import("compiler.zig");

const DEBUG_LOG_GC = true;
const GC_HEAP_GROW_FACTOR = 2;

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "Memory",
    });
}

pub fn collectGarbage(vm: *virtual_machine.VirtualMachine) !void {
    if (comptime DEBUG_LOG_GC) {
        logDebug("-- gc begin", .{});
    }

    const before = vm.bytes_allocated;

    try markRoots(vm);
    try traceReferences(vm);
    tableRemoveWhite(vm);
    sweep(vm);

    vm.next_gc = vm.bytes_allocated * GC_HEAP_GROW_FACTOR;

    if (comptime DEBUG_LOG_GC) {
        logDebug("-- gc end", .{});
        logDebug("collected {d} bytes (from {d} to {d} next at {d})", .{
            before - vm.bytes_allocated, before, vm.bytes_allocated, vm.next_gc,
        });
    }
}

fn sweep(vm: *virtual_machine.VirtualMachine) void {
    var previous: ?*object.Obj = null;
    var current = vm.objects;
    while (current) |obj| {
        if (obj.is_marked) {
            obj.is_marked = false;
            previous = obj;
            current = obj.next;
        } else {
            const unreached = obj;
            current = obj.next;
            if (previous) |prev| {
                prev.next = current;
            } else {
                vm.objects = current;
            }
            unreached.deinit(vm);
        }
    }
}

fn tableRemoveWhite(vm: *virtual_machine.VirtualMachine) void {
    var it = vm.strings.keyIterator();
    while (it.next()) |key| {
        if (!key.*.obj.is_marked) {
            _ = vm.strings.remove(key.*);
        }
    }
}

fn traceReferences(vm: *virtual_machine.VirtualMachine) !void {
    while (vm.gray_stack.pop()) |obj| {
        try blackenObj(vm, obj);
    }
}

fn blackenObj(vm: *virtual_machine.VirtualMachine, obj: *object.Obj) !void {
    if (comptime DEBUG_LOG_GC) {
        logDebug("0x{x} blacken {f}", .{ @intFromPtr(obj), obj });
    }

    switch (obj.type) {
        .Upvalue => {
            if (obj.asUpvalue().closed) |val| {
                try markValue(vm, val);
            }
        },
        .Function => {
            const function = obj.asFunction();
            if (function.name) |name| {
                try markObject(vm, &name.obj);
            }
            try markArray(vm, function.chunk.constants.items);
        },
        .Closure => {
            const closure = obj.asClosure();
            try markObject(vm, &closure.function.obj);
            for (closure.upvalues) |upvalue| {
                if (upvalue) |upval| {
                    try markObject(vm, &upval.obj);
                }
            }
        },
        else => {},
    }
}

fn markArray(vm: *virtual_machine.VirtualMachine, array: []value.Value) !void {
    for (array) |val| {
        try markValue(vm, val);
    }
}

fn markRoots(vm: *virtual_machine.VirtualMachine) !void {
    for (0..vm.stack_top) |i| {
        try markValue(vm, vm.stack[i]);
    }

    for (0..vm.frame_count) |i| {
        try markObject(vm, &vm.frames[i].closure.obj);
    }

    var upvalue = vm.open_upvalues;
    while (upvalue) |val| {
        try markObject(vm, &val.obj);
        upvalue = val.next;
    }

    try markTable(vm);
    try markCompilerRoots(vm);
}

fn markCompilerRoots(vm: *virtual_machine.VirtualMachine) !void {
    var current = vm.current_compiler;
    while (current) |c| {
        try markObject(vm, &c.function.obj);
        current = c.enclosing;
    }
}

fn markValue(vm: *virtual_machine.VirtualMachine, val: value.Value) !void {
    if (val.isObj()) try markObject(vm, val.Obj);
}

pub fn markObject(vm: *virtual_machine.VirtualMachine, obj: *object.Obj) !void {
    if (comptime DEBUG_LOG_GC) {
        logDebug("0x{x} mark {s}", .{ @intFromPtr(obj), obj.getType() });
    }
    obj.is_marked = true;
    try vm.gray_stack.append(vm.gpa, obj);
}

fn markTable(vm: *virtual_machine.VirtualMachine) !void {
    var it = vm.globals.iterator();
    while (it.next()) |entry| {
        try markObject(vm, &entry.key_ptr.*.obj);
        try markValue(vm, entry.value_ptr.*);
    }
}
