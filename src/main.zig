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
const Node = evaluator_module.Node;
const Environment = evaluator_module.Environment;

pub fn main() !void {
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
    var parser = try Parser.init(&lexer, arenaAllocator);
    const program = try parser.parse_program();

    const env = try Environment.init(gpaAllocator);
    defer env.deinit();

    var evaluator = Evaluator.init(gpaAllocator);
    const result = evaluator.eval(Node{ .Program = program }, env);
    std.debug.print("Result: {any}\n", .{result});
}
