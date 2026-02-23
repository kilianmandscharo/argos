const std = @import("std");
const vm_module = @import("vm.zig");
const chunck_module = @import("chunk.zig");

const VirtualMachine = vm_module.VirtualMachine;
const Chunk = chunck_module.Chunk;

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

fn runFile(allocator: std.mem.Allocator, vm: *VirtualMachine) !void {
    const file = try std.fs.cwd().openFile("test.argos", .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    vm.interpret(source);
}

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    const file = try std.fs.cwd().openFile("test.argos", .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    var vm = VirtualMachine.init(allocator);
    defer vm.deinit();

    var chunk = Chunk.init();
    defer chunk.deinit(allocator);

    const result = try vm.interpret(&chunk, source);
    std.debug.print("{}\n", .{result});

    // if (std.os.argv.len == 1) {
    //     try repl(allocator);
    // } else if (std.os.argv.len == 2) {
    // try runFile(allocator, &vm);
    // } else {
    //     std.debug.print("Usage: argos [path]\n", .{});
    //     return error.Exit;
    // }
}
