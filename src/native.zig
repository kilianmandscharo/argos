const std = @import("std");
const value = @import("value.zig");

pub fn clockNative(arg_count: usize, args: []value.Value) value.Value {
    _ = arg_count;
    _ = args;
    const time: f64 = @floatFromInt(std.time.milliTimestamp());
    const ms_per_s: f64 = @floatFromInt(std.time.ms_per_s);
    return value.wrapFloat(@divExact(time, ms_per_s));
}
