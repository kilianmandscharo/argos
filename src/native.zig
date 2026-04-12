const std = @import("std");
const value = @import("value.zig");
const virtual_machine = @import("vm.zig");

pub fn clockNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    _ = vm;
    _ = arg_count;
    _ = args;
    const time: f64 = @floatFromInt(std.time.milliTimestamp());
    const ms_per_s: f64 = @floatFromInt(std.time.ms_per_s);
    return value.wrapFloat(@divExact(time, ms_per_s));
}

pub fn printNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    if (arg_count != 1) {
        return vm.runtimeError("Expected 1 arg, got {d}.", .{arg_count});
    }
    const val = args[0];
    std.debug.print("{f}\n", .{val});
    return value.valueNull();
}

pub fn assertNative(vm: *virtual_machine.VirtualMachine, arg_count: usize, args: []value.Value) !value.Value {
    if (arg_count != 1) {
        return vm.runtimeError("Expected 1 arg, got {d}.", .{arg_count});
    }
    const val = args[0];
    if (val != .Bool) return vm.runtimeError("Expected Boolean, got {s}", .{val.getType()});
    if (!val.Bool) return vm.runtimeError("Assertion failed", .{});
    return value.valueNull();
}
