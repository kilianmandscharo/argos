const std = @import("std");
const compiler = @import("compiler.zig");
const VirtualMachine = compiler.VirtualMachine;

fn repl(_: std.mem.Allocator) !void {
    var stdin_buf: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buf);
    const stdin = &stdin_reader.interface;

    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    const stdout = &stdout_writer.interface;

    while (true) {
        try stdout.writeAll(">>");
        try stdout.flush();

        const input = try stdin.takeDelimiterExclusive('\n');

        try stdout.writeAll(input);
        try stdout.writeAll("\n");
        try stdout.flush();
    }
}

fn runFile(allocator: std.mem.Allocator) !void {
    const file = try std.fs.cwd().openFile("test.argos", .{});
    defer file.close();

    _ = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    // const vm = VirtualMachine.init();

    if (std.os.argv.len == 1) {
        try repl(allocator);
    } else if (std.os.argv.len == 2) {
        try runFile(allocator);
    } else {
        std.debug.print("Usage: argos [path]\n", .{});
        return error.Exit;
    }
}
