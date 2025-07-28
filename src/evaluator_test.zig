const std = @import("std");

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;

const evaluator_module = @import("evaluator.zig");
const Evaluator = evaluator_module.Evaluator;
const Object = evaluator_module.Object;
const Node = evaluator_module.Node;
const Environment = evaluator_module.Environment;

pub fn getResult(arena: std.mem.Allocator, input: []const u8) !Object {
    var lexer = try Lexer.init(arena, input);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram();

    const env = try Environment.init(arena);

    var evaluator = Evaluator.init(arena);
    const result = try evaluator.eval(Node{ .Program = program }, env);

    return result;
}

// TODO: Add descriptions to each test

test "infix expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const TestCase = struct {
        input: []const u8,
        expected_output: Object,
    };

    const test_cases = [_]TestCase{
        .{ .input = "5 + 5", .expected_output = Object{ .Integer = 10 } },
        .{ .input = "5 - 3", .expected_output = Object{ .Integer = 2 } },
        .{ .input = "5 - 11", .expected_output = Object{ .Integer = -6 } },
        .{ .input = "5 * 3", .expected_output = Object{ .Integer = 15 } },
        .{ .input = "-5 * 3", .expected_output = Object{ .Integer = -15 } },
        .{ .input = "5 * -3", .expected_output = Object{ .Integer = -15 } },
        .{ .input = "10 / 5", .expected_output = Object{ .Float = 2 } },
        .{ .input = "1.1 * 4.3", .expected_output = Object{ .Float = 4.73 } },
        .{ .input = "1.1 - 4.2", .expected_output = Object{ .Float = -3.1 } },
        .{ .input = "5.4 - 1.0", .expected_output = Object{ .Float = 4.4 } },
        .{ .input = "1.0 / 4.0", .expected_output = Object{ .Float = 0.25 } },
        .{ .input = "true == true", .expected_output = Object{ .Boolean = true } },
        .{ .input = "true == false", .expected_output = Object{ .Boolean = false } },
        .{ .input = "false == false", .expected_output = Object{ .Boolean = true } },
        .{ .input = "5 == 6", .expected_output = Object{ .Boolean = false } },
        .{ .input = "1.1 == 1.1", .expected_output = Object{ .Boolean = true } },
        .{ .input = "7 < 6", .expected_output = Object{ .Boolean = false } },
        .{ .input = "5.1 > 1.3", .expected_output = Object{ .Boolean = true } },
    };

    for (test_cases) |test_case| {
        const result = try getResult(arena.allocator(), test_case.input);
        std.testing.expectEqual(test_case.expected_output, result) catch |err| {
            std.debug.print("expected {any}, got {any}\n", .{ test_case.expected_output, result });
            return err;
        };
    }
}

test "infix expressions error cases" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const TestCase = struct {
        input: []const u8,
        expected_error: []const u8,
    };

    const test_cases = [_]TestCase{
        .{ .input = "5 + true", .expected_error = "type mismatch: Integer <> Boolean" },
        .{ .input = "5.7 + true", .expected_error = "type mismatch: Float <> Boolean" },
        .{ .input = "false * 30", .expected_error = "type mismatch: Boolean <> Integer" },
        .{ .input = "true * false", .expected_error = "invalid operator '*' for type Boolean" },
    };

    for (test_cases) |test_case| {
        const result = try getResult(arena.allocator(), test_case.input);
        switch (result) {
            .Error => |v| {
                std.testing.expectEqualStrings(test_case.expected_error, v) catch |err| {
                    std.debug.print("expected {s}, got {s}\n", .{ test_case.expected_error, v });
                    return err;
                };
            },
            else => return error.ExpectedError,
        }
    }
}

test "multi line calculation" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const input =
        \\a = 7
        \\b = 10
        \\a * b
    ;

    const expected_output = Object{ .Integer = 70 };

    const result = try getResult(arena.allocator(), input);
    std.testing.expectEqual(expected_output, result) catch |err| {
        std.debug.print("expected {any}, got {any}\n", .{ expected_output, result });
        return err;
    };
}

test "function calls" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const TestCase = struct {
        input: []const u8,
        expected_output: Object,
    };

    const test_cases = [_]TestCase{
        .{
            .input =
            \\fnc square(a) {
            \\    return a * a
            \\}
            \\
            \\square(11)
            ,
            .expected_output = Object{ .Integer = 121 },
        },
        .{
            .input =
            \\fnc square(a) {
            \\    a * a
            \\}
            \\
            \\square(11)
            ,
            .expected_output = Object{ .Integer = 121 },
        },
        .{
            .input =
            \\x = 10
            \\
            \\fnc add(a) {
            \\    a + x
            \\}
            \\
            \\add(5)
            ,
            .expected_output = Object{ .Integer = 15 },
        },
        .{
            .input =
            \\x = 10
            \\
            \\fnc add(a) {
            \\    x = 17
            \\    a + x
            \\}
            \\
            \\add(5)
            ,
            .expected_output = Object{ .Integer = 22 },
        },
        .{
            .input =
            \\a = 10
            \\
            \\fnc subtract(a, b) {
            \\    a - b 
            \\}
            \\
            \\subtract(20, 3)
            ,
            .expected_output = Object{ .Integer = 17 },
        },
        .{
            .input =
            \\fnc callback(x) {
            \\    x * x
            \\}
            \\
            \\fnc test(val, cb) {
            \\    cb(val)
            \\}
            \\
            \\test(15, callback)
            ,
            .expected_output = Object{ .Integer = 225 },
        },
        .{
            .input =
            \\fnc test() {
            \\    fnc inner(y) { y * y }
            \\    inner(10)
            \\}
            \\test()
            ,
            .expected_output = Object{ .Integer = 100 },
        },
        .{
            .input =
            \\fnc adder(val) {
            \\    fnc inner(x) { val + x }
            \\    inner
            \\}
            \\add_five = adder(5)
            \\add_five(10)
            ,
            .expected_output = Object{ .Integer = 15 },
        },
    };

    for (test_cases) |test_case| {
        const result = try getResult(arena.allocator(), test_case.input);
        std.testing.expectEqual(test_case.expected_output, result) catch |err| {
            std.debug.print("expected {any}, got {any}\n", .{ test_case.expected_output, result });
            return err;
        };
    }
}

test "if expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_output: Object,
    };

    const test_cases = [_]TestCase{
        .{
            .description = "single line no else true",
            .input =
            \\if true { 5 }
            ,
            .expected_output = Object{ .Integer = 5 },
        },
        .{
            .description = "single line no else false",
            .input =
            \\if false { 5 }
            ,
            .expected_output = Object.Null,
        },
        .{
            .description = "single line with else true",
            .input =
            \\if true { 5 } else { 7 }
            ,
            .expected_output = Object{ .Integer = 5 },
        },
        .{
            .description = "single line with else false",
            .input =
            \\if false { 5 } else { 7 }
            ,
            .expected_output = Object{ .Integer = 7 },
        },
        .{
            .description = "multi line no else true",
            .input =
            \\if true { 
            \\    5 
            \\}
            ,
            .expected_output = Object{ .Integer = 5 },
        },
        .{
            .description = "multi line no else false",
            .input =
            \\if false { 
            \\    5 
            \\}
            ,
            .expected_output = Object.Null,
        },
        .{
            .description = "multi line with else true",
            .input =
            \\if true { 
            \\    5 
            \\} else {
            \\    7
            \\}
            ,
            .expected_output = Object{ .Integer = 5 },
        },
        .{
            .description = "multi line with else false",
            .input =
            \\if false { 
            \\    5 
            \\} else {
            \\    7
            \\}
            ,
            .expected_output = Object{ .Integer = 7 },
        },
        .{
            .description = "in context",
            .input =
            \\y = 17
            \\fnc calculate(x) { y * x }
            \\result = if y < 20 { 
            \\    calculate(5)
            \\} else {
            \\    calculate(3)
            \\}
            \\result
            ,
            .expected_output = Object{ .Integer = 85 },
        },
    };

    std.debug.print("==if expressions start==\n", .{});
    for (test_cases) |test_case| {
        std.debug.print("  {s}\n", .{test_case.description});
        const result = try getResult(arena.allocator(), test_case.input);
        std.testing.expectEqual(test_case.expected_output, result) catch |err| {
            std.debug.print("expected {any}, got {any}\n", .{ test_case.expected_output, result });
            return err;
        };
    }
    std.debug.print("==if expressions end==\n", .{});
}

test "for expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const TestCase = struct {
        description: []const u8,
        input: []const u8,
        expected_output: Object,
    };

    const test_cases = [_]TestCase{
        .{
            .description = "simple for loop",
            .input =
            \\x = 0
            \\for i in 0..10 {
            \\    x = x + i
            \\}
            \\x
            ,
            .expected_output = Object{ .Integer = 45 },
        },
        .{
            .description = "should return null",
            .input =
            \\x = 0
            \\for i in 0..10 {
            \\    x = x + i
            \\}
            ,
            .expected_output = Object.Null,
        },
    };

    std.debug.print("==for expressions start==\n", .{});
    for (test_cases) |test_case| {
        std.debug.print("  {s}\n", .{test_case.description});
        const result = try getResult(arena.allocator(), test_case.input);
        std.testing.expectEqual(test_case.expected_output, result) catch |err| {
            std.debug.print("expected {any}, got {any}\n", .{ test_case.expected_output, result });
            return err;
        };
    }
    std.debug.print("==for expressions end==\n", .{});
}
