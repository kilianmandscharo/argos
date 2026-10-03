const std = @import("std");
const vm = @import("vm.zig");
const chunck = @import("chunk.zig");
const constants = @import("constants.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");

pub fn main(init: std.process.Init) !void {
    const io = init.io;
    const gpa = init.gpa;
    const arena = init.arena;

    const args = try init.minimal.args.toSlice(arena.allocator());

    if (args.len < 2) {
        std.debug.print("Usage: argos [path]\n", .{});
        return error.Exit;
    }

    const file_path: []const u8 = args[1];

    const file = try std.Io.Dir.cwd().openFile(io, file_path, .{});
    defer file.close(io);

    var file_reader = file.reader(io, &.{});
    const source = try file_reader.interface.allocRemaining(arena.allocator(), .unlimited);

    if (args.len == 2) {
        var virtual_machine = try vm.VirtualMachine.init(io, gpa, arena.allocator());
        defer virtual_machine.deinit();

        _ = virtual_machine.interpret(file_path, source) catch |err| {
            if (constants.stack_trace_on_error) {
                return err;
            }
        };

        return;
    }

    if (std.mem.eql(u8, args[2], "--ast")) {
        var script_context = vm.ScriptContext{
            .file_name = file_path,
            .source = source,
            .lines = .empty,
        };

        const program: ast.Program = parser.createAst(arena.allocator(), &script_context) catch |err| {
            if (constants.stack_trace_on_error) return err;
            return;
        };

        var stdout_buffer: [1024]u8 = undefined;
        var stdout_file_writer: std.Io.File.Writer = .init(.stdout(), io, &stdout_buffer);
        const stdout_writer = &stdout_file_writer.interface;
        try ast.printProgram(program, stdout_writer);
        try stdout_writer.flush();
    }

    // if (std.mem.eql(u8, args[2], "--bytecode")) {
    //     var virtual_machine = try vm.VirtualMachine.init(gpa.allocator(), arena.allocator());
    //     defer virtual_machine.deinit();
    //
    //     const function = try virtual_machine.compile(file_path, source);
    //
    //     return;
    // }
}
