const std = @import("std");
const vm = @import("vm.zig");
const chunck = @import("chunk.zig");
const constants = @import("constants.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

fn repl(allocator: std.mem.Allocator) !void {
    _ = allocator;
    // var stdin_buf: [1024]u8 = undefined;
    // var stdin_reader = std.fs.File.stdin().reader(&stdin_buf);
    // const stdin = &stdin_reader.interface;
    //
    // var stdout_buf: [1024]u8 = undefined;
    // var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
    // const stdout = &stdout_writer.interface;
    //
    // while (true) {
    //     try stdout.writeAll(">> ");
    //     try stdout.flush();
    //
    //     const source = try stdin.takeDelimiterExclusive('\n');
    //     stdin.toss(1);
    //
    //     var virtual_machine = try vm.VirtualMachine.init(allocator);
    //     defer virtual_machine.deinit();
    //
    //     _ = virtual_machine.interpret("repl", source) catch |err| {
    //         if (constants.stack_trace_on_error) {
    //             return err;
    //         }
    //     };
    //
    //     try stdout.writeAll("\n");
    //     try stdout.flush();
    // }
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

        var script_context = vm.ScriptContext{
            .file_name = file_path,
            .source = source,
            .lines = .{},
        };

        const program: ast.Program = parser.createAst(arena.allocator(), &script_context) catch |err| {
            if (constants.stack_trace_on_error) return err;
            return;
        };
        try ast.printProgram(program, stdout);

        try stdout.flush();
    } else {
        var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
        defer arena.deinit();

        var virtual_machine = try vm.VirtualMachine.init(allocator, arena.allocator());
        defer virtual_machine.deinit();

        _ = virtual_machine.interpret(file_path, source) catch |err| {
            if (constants.stack_trace_on_error) {
                return err;
            }
        };
    }
}

pub fn main() !void {
    if (std.os.argv.len < 2) {
        std.debug.print("Usage: argos [path]\n", .{});
        return error.Exit;
    }

    var gpa: std.heap.DebugAllocator(.{}) = .init;

    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) std.testing.expect(false) catch @panic("TEST FAIL");
    }

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const file_path: []const u8 = std.mem.span(std.os.argv[1]);

    const file = try std.fs.cwd().openFile(file_path, .{});
    defer file.close();

    const source = try file.readToEndAlloc(arena.allocator(), std.math.maxInt(usize));

    if (std.os.argv.len == 2) {
        var virtual_machine = try vm.VirtualMachine.init(gpa.allocator(), arena.allocator());
        defer virtual_machine.deinit();

        _ = virtual_machine.interpret(file_path, source) catch |err| {
            if (constants.stack_trace_on_error) {
                return err;
            }
        };

        return;
    }

    if (std.mem.eql(u8, std.mem.span(std.os.argv[2]), "--ast")) {
        var stdout_buf: [1024]u8 = undefined;
        var stdout_writer = std.fs.File.stdout().writer(&stdout_buf);
        const stdout = &stdout_writer.interface;

        var script_context = vm.ScriptContext{
            .file_name = file_path,
            .source = source,
            .lines = .{},
        };

        const program: ast.Program = parser.createAst(arena.allocator(), &script_context) catch |err| {
            if (constants.stack_trace_on_error) return err;
            return;
        };
        try ast.printProgram(program, stdout);
        try stdout.flush();
    }

    // if (std.mem.eql(u8, std.mem.span(std.os.argv[2]), "--bytecode")) {
    //     var virtual_machine = try vm.VirtualMachine.init(gpa.allocator(), arena.allocator());
    //     defer virtual_machine.deinit();
    //
    //     const function = try virtual_machine.compile(file_path, source);
    //
    //     return;
    // }
}
