const std = @import("std");
const virtual_machine = @import("vm.zig");
const chunck = @import("chunk.zig");
const constants = @import("constants.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

fn repl(allocator: std.mem.Allocator) !void {
    var stdin_buf: [1024]u8 = undefined;
    var stdin_reader = std.fs.File.stdin().reader(&stdin_buf);
    const stdin = &stdin_reader.interface;

    var stdout_buf: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    const stdout = &stdout_writer.interface;

    while (true) {
        try stdout.writeAll(">> ");
        try stdout.flush();

        const source = try stdin.takeDelimiterExclusive('\n');
        stdin.toss(1);

        var vm = try virtual_machine.VirtualMachine.init(allocator);
        defer vm.deinit();

        _ = vm.interpret(source) catch |err| {
            if (constants.stack_trace_on_error) {
                return err;
            }
        };

        try stdout.writeAll("\n");
        try stdout.flush();
    }
}

fn runFile(allocator: std.mem.Allocator, emit_ast: bool) !void {
    const file_path: []const u8 = std.mem.span(std.os.argv[1]);

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(source);

    if (emit_ast) {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        var stdout_buf: [1024]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
        const stdout = &stdout_writer.interface;

        const program = try parser.createAst(arena.allocator(), source);
        try ast.printProgram(program, stdout);

        try stdout.flush();
    } else {
        var vm = try virtual_machine.VirtualMachine.init(allocator);
        defer vm.deinit();

        _ = vm.interpret(source) catch |err| {
            if (constants.stack_trace_on_error) {
                return err;
            }
        };
    }
}

pub fn main() !void {
    var gpa: std.heap.DebugAllocator(.{}) = .init;
    const allocator = gpa.allocator();

    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    if (std.os.argv.len == 1) {
        try repl(allocator);
    } else if (std.os.argv.len == 2) {
        try runFile(allocator, false);
    } else if (std.os.argv.len == 3 and std.mem.eql(u8, std.mem.span(std.os.argv[2]), "--ast")) {
        try runFile(allocator, true);
    } else {
        std.debug.print("Usage: argos [path]\n", .{});
        return error.Exit;
    }
}
