const std = @import("std");

pub fn list(comptime T: type, arena: std.mem.Allocator, items: []const T) !std.ArrayListUnmanaged(T) {
    var ret: std.ArrayListUnmanaged(T) = .{};
    for (items) |item| {
        try ret.append(arena, item);
    }
    return ret;
}

fn printColor(color: []const u8, comptime format: []const u8, args: anytype) void {
    std.debug.print("{s}", .{color});
    std.debug.print(format, args);
    std.debug.print("\x1b[0m", .{});
}

fn printHighlight(comptime format: []const u8, args: anytype) void {
    printColor("\x1b[34m", format, args);
}

fn printSuccess(comptime format: []const u8, args: anytype) void {
    printColor("\x1b[32m", format, args);
}

fn printError(comptime format: []const u8, args: anytype) void {
    printColor("\x1b[31m", format, args);
}

pub fn runTests(comptime T: type, name: []const u8, test_cases: []const T, run: fn (arena: std.mem.Allocator, test_case: T) anyerror!void) !void {
    printHighlight("start {s} tests\n", .{name});

    var success: usize = 0;
    var failed: usize = 0;

    for (test_cases) |test_case| {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        const result = run(arena.allocator(), test_case);

        if (result) {
            printSuccess("  > ", .{});
            success += 1;
        } else |err| {
            printError("{any}\n", .{err});
            printError("  > ", .{});
            failed += 1;
        }
        std.debug.print("{s}\n", .{test_case.description});
    }

    if (failed == 0) {
        printSuccess("    > {d} successful, {d} failed\n", .{ success, failed });
    } else {
        printError("    > {d} successful, {d} failed\n", .{
            success,
            failed,
        });
    }
    std.debug.print("\n", .{});

    if (failed > 0) {
        return error.TestFailed;
    }
}
