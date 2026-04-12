const std = @import("std");
const virtual_machine = @import("vm.zig");
const chunck = @import("chunk.zig");
const constants = @import("constants.zig");

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

fn runFile(allocator: std.mem.Allocator, vm: *virtual_machine.VirtualMachine) !void {
    const file = try std.fs.cwd().openFile("test.argos", .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    vm.interpret(source);
}

pub fn main() !void {
    if (std.os.argv.len != 2) {
        std.debug.print("Usage: {s} <file>\n", .{std.os.argv[0]});
        std.process.exit(1);
    }

    const file_path: []const u8 = std.mem.span(std.os.argv[1]);

    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    var vm = try virtual_machine.VirtualMachine.init(allocator);
    defer vm.deinit();

    _ = vm.interpret(source) catch |err| {
        if (constants.stack_trace_on_error) {
            return err;
        }
    };

    // if (std.os.argv.len == 1) {
    //     try repl(allocator);
    // } else if (std.os.argv.len == 2) {
    // try runFile(allocator, &vm);
    // } else {
    //     std.debug.print("Usage: argos [path]\n", .{});
    //     return error.Exit;
    // }
}
