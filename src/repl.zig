const std = @import("std");

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const evaluator_module = @import("evaluator.zig");
const Environment = evaluator_module.Environment;
const Evaluator = evaluator_module.Evaluator;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;

pub fn start() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpaAllocator = gpa.allocator();
    defer {
        const status = gpa.deinit();
        if (status != .ok) {
            std.debug.print("Memory leak detected: {}\n", .{status});
        }
    }

    var env = try Environment.init(.{ .gpa = gpaAllocator, .debug = false });
    defer env.deinit();

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

        var arena = std.heap.ArenaAllocator.init(gpaAllocator);
        const arenaAllocator = arena.allocator();
        defer arena.deinit();

        var lexer = try Lexer.init(arenaAllocator, input);
        var parser = try Parser.init(&lexer, .{ .arena = arenaAllocator, .debug = false });
        const program = try parser.parseProgram();

        var evaluator = Evaluator.init(.{ .gpa = gpaAllocator, .debug = false });
        const result = try evaluator.eval(&program, env, 0);

        try result.format(stdout);
        try stdout.writeAll("\n");
        try stdout.flush();
    }
}
