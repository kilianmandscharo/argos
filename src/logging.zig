const std = @import("std");

pub const LogLevel = enum {
    Debug,
    Info,
    Warn,
    Error,
    Fatal,
};

pub const LogColor = enum {
    Blue,
    Green,
    None,
};

pub const LogOptions = struct {
    currentLevel: LogLevel = .Debug,
    messageLevel: LogLevel = .Debug,
    color: LogColor = .None,
    indent: usize = 0,
};

pub fn log(comptime fmt: []const u8, args: anytype, options: LogOptions) void {
    if (@intFromEnum(options.messageLevel) < @intFromEnum(options.currentLevel)) {
        return;
    }
    switch (options.color) {
        .Blue => {
            std.debug.print("\x1b[34m", .{});
        },
        .Green => {
            std.debug.print("\x1b[32m", .{});
        },
        .None => {},
    }
    if (options.indent > 0) {
        for (0..options.indent - 1) |_| {
            std.debug.print("    ", .{});
        }
        std.debug.print("┗━━━", .{});
    }
    std.debug.print("[{s}] ", .{@tagName(options.messageLevel)});
    std.debug.print(fmt, args);
    std.debug.print("\x1b[0m", .{});
}
