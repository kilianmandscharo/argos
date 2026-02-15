const std = @import("std");

const lexer_module = @import("lexer.zig");
const Token = lexer_module.Token;
const TokenType = lexer_module.TokenType;
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Statement = parser_module.Statement;
const Expression = parser_module.Expression;
const Operator = parser_module.Operator;
const Parser = parser_module.Parser;

const evaluator_module = @import("evaluator.zig");
const Evaluator = evaluator_module.Evaluator;
const Environment = evaluator_module.Environment;

const Repl = @import("repl.zig");

pub fn main() !void {
    if (std.os.argv.len <= 1) {
        try Repl.start();
    }

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const gpaAllocator = gpa.allocator();

    defer {
        const status = gpa.deinit();
        if (status != .ok) {
            std.debug.print("Memory leak detected: {}\n", .{status});
        }
    }

    const file = try std.fs.cwd().openFile("test.argos", .{});
    defer file.close();

    const content = try file.readToEndAlloc(gpaAllocator, std.math.maxInt(usize));
    defer gpaAllocator.free(content);

    var arena = std.heap.ArenaAllocator.init(gpaAllocator);
    const arenaAllocator = arena.allocator();
    defer arena.deinit();

    var lexer = try Lexer.init(arenaAllocator, content);
    var parser = try Parser.init(&lexer, .{ .arena = arenaAllocator, .debug = false });
    const program = try parser.parseProgram();

    const env = try Environment.init(.{
        .gpa = gpaAllocator,
    });
    defer env.drop(0);

    var evaluator = Evaluator.init(.{ .gpa = gpaAllocator, .debug = false });
    const result = try evaluator.eval(&program, env);
    std.debug.print("==========\n", .{});
    std.debug.print("Result: {f}\n", .{result});
    std.debug.print("==========\n", .{});
}
