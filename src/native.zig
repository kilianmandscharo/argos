const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const virtual_machine = @import("vm.zig");

const Native = struct {
    name: []const u8,
    function: object.NativeFn,
};

pub const natives: []const Native = &.{
    .{
        .name = "@clock",
        .function = clockNative,
    },
    .{
        .name = "@print",
        .function = printNative,
    },
    .{
        .name = "@assert",
        .function = assertNative,
    },
    .{
        .name = "@len",
        .function = lenNative,
    },
    .{
        .name = "@nth",
        .function = nthNative,
    },
};

fn clockNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    _ = arg_count;
    _ = args;
    const now = std.Io.Timestamp.now(vm.io, .real);
    const time: f64 = @floatFromInt(std.Io.Timestamp.toMilliseconds(now));
    const ms_per_s: f64 = @floatFromInt(std.time.ms_per_s);
    return value.wrapFloat(@divExact(time, ms_per_s));
}

fn printNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    if (arg_count != 1) {
        return vm.runtimeError("expected 1 arg, got {d}", .{arg_count});
    }
    const val = args[0];
    std.debug.print("{f}\n", .{val});
    return value.valueUnit();
}

fn assertNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    if (arg_count != 1) {
        return vm.runtimeError("expected 1 arg, got {d}", .{arg_count});
    }
    const val = args[0];
    if (val != .Bool) return vm.runtimeError("expected Boolean, got {s}", .{val.getType()});
    if (!val.Bool) return vm.runtimeError("Assertion failed", .{});
    return value.valueUnit();
}

fn lenNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    if (arg_count != 1) {
        return vm.runtimeError("expected 1 arg, got {d}", .{arg_count});
    }
    const val = args[0];
    if (!val.isObjType(.List)) {
        return vm.runtimeError("expected value of type List, got {s}", .{val.getType()});
    }
    return value.wrapInt(@intCast(val.asObj().asList().len()));
}

fn nthNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    if (arg_count != 2) {
        return vm.runtimeError("expected 2 args, got {d}", .{arg_count});
    }
    const val = args[0];
    if (!val.isObjType(.List)) {
        return vm.runtimeError("expected value of type List, got {s}", .{val.getType()});
    }
    const index = args[1];
    if (index != .Int) {
        return vm.runtimeError("expected value of type Int, got {s}", .{index.getType()});
    }
    var i = index.Int;
    if (i < 0) {
        return vm.runtimeError("expected list index >= 0, got {d}", .{i});
    }
    var it = val.asObj().asList().iterator();
    while (it.next()) |item| {
        if (i == 0) return item.value;
        i -= 1;
    }
    return vm.runtimeError("list index {d} exceeds list length", .{index.Int});
}
