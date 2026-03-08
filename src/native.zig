const std = @import("std");
const value_module = @import("value.zig");

const Value = value_module.Value;
const wrapFloat = value_module.wrapFloat;

pub fn clockNative(arg_count: usize, args: []Value) Value {
    _ = arg_count;
    _ = args;
    const time: f64 = @floatFromInt(std.time.milliTimestamp());
    const ms_per_s: f64 = @floatFromInt(std.time.ms_per_s);
    return wrapFloat(@divExact(time, ms_per_s));
}
