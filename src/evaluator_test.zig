const std = @import("std");

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;

const evaluator_module = @import("evaluator.zig");
const Evaluator = evaluator_module.Evaluator;
const Object = evaluator_module.Object;
const Environment = evaluator_module.Environment;

const test_utils = @import("test_utils.zig");
const runTests = test_utils.runTests;

pub fn getResult(arena: std.mem.Allocator, input: []const u8) !Object {
    var lexer = try Lexer.init(arena, input);
    var parser = try Parser.init(&lexer, arena);
    const program = try parser.parseProgram();

    const env = try Environment.init(arena);

    var evaluator = Evaluator.init(arena);
    const result = try evaluator.eval(&program, env);

    return result;
}

const ObjectTestCase = struct {
    input: []const u8,
    description: []const u8,
    expected_output: Object,
};

const runObjectTest = struct {
    fn runTest(arena: std.mem.Allocator, test_case: ObjectTestCase) anyerror!void {
        const result = try getResult(arena, test_case.input);
        try std.testing.expectEqual(test_case.expected_output, result);
    }
}.runTest;

test "infix expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ObjectTestCase{
        .{ .description = "integer addition", .input = "5 + 5", .expected_output = Object{ .Integer = 10 } },
        .{ .description = "integer subtraction positive", .input = "5 - 3", .expected_output = Object{ .Integer = 2 } },
        .{ .description = "integer subtraction negative", .input = "5 - 11", .expected_output = Object{ .Integer = -6 } },
        .{ .description = "integer multiplication", .input = "5 * 3", .expected_output = Object{ .Integer = 15 } },
        .{ .description = "integer multiplication first multiplier negative", .input = "-5 * 3", .expected_output = Object{ .Integer = -15 } },
        .{ .description = "integer multiplication second multiplier negative", .input = "5 * -3", .expected_output = Object{ .Integer = -15 } },
        .{ .description = "integer division", .input = "10 / 5", .expected_output = Object{ .Float = 2 } },
        .{ .description = "float multiplication", .input = "1.1 * 4.3", .expected_output = Object{ .Float = 4.73 } },
        .{ .description = "float subtraction positive", .input = "5.4 - 1.0", .expected_output = Object{ .Float = 4.4 } },
        .{ .description = "float subtraction negative", .input = "1.1 - 4.2", .expected_output = Object{ .Float = -3.1 } },
        .{ .description = "float division", .input = "1.0 / 4.0", .expected_output = Object{ .Float = 0.25 } },
        .{ .description = "equals bool true and true", .input = "true == true", .expected_output = Object{ .Boolean = true } },
        .{ .description = "equals bool true and false", .input = "true == false", .expected_output = Object{ .Boolean = false } },
        .{ .description = "equals bool false and false", .input = "false == false", .expected_output = Object{ .Boolean = true } },
        .{ .description = "equals integer false", .input = "5 == 6", .expected_output = Object{ .Boolean = false } },
        .{ .description = "equals integer true", .input = "5 == 5", .expected_output = Object{ .Boolean = true } },
        .{ .description = "equals float true", .input = "1.1 == 1.1", .expected_output = Object{ .Boolean = true } },
        .{ .description = "equals float false", .input = "1.1 == 2.1", .expected_output = Object{ .Boolean = false } },
        .{ .description = "less than integer", .input = "7 < 6", .expected_output = Object{ .Boolean = false } },
        .{ .description = "larger than float", .input = "5.1 > 1.3", .expected_output = Object{ .Boolean = true } },
    };

    try runTests(ObjectTestCase, "evaluate infix expression", &test_cases, runObjectTest);
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
    const test_cases = [_]ObjectTestCase{
        .{
            .description = "function call with return",
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
            .description = "function call without return",
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
            .description = "function call outer scope variable",
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
            .description = "function call outer scope variable multi line",
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
            .description = "function call shadow outer scope variable",
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
            .description = "function call with callback function",
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
            .description = "function call with inner function declaration",
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
            .description = "function call higher order function",
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

    try runTests(ObjectTestCase, "evaluate function calls", &test_cases, runObjectTest);
}

test "if expressions" {
    const test_cases = [_]ObjectTestCase{
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

    try runTests(ObjectTestCase, "evaluate if expressions", &test_cases, runObjectTest);
}

test "for expressions" {
    const test_cases = [_]ObjectTestCase{
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
        .{
            .description = "fibonacci",
            .input =
            \\fnc fib(n) {
            \\    a = 0
            \\    b = 1
            \\    for i in 0..n {
            \\        tmp = b
            \\        b = a + b
            \\        a = tmp
            \\    }
            \\    a
            \\}
            \\fib(30)
            ,
            .expected_output = Object{ .Integer = 832040 },
        },
    };

    try runTests(ObjectTestCase, "evaluate for expressions", &test_cases, runObjectTest);
}
