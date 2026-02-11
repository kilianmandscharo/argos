const std = @import("std");

const lexer_module = @import("lexer.zig");
const Lexer = lexer_module.Lexer;

const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;

const evaluator_module = @import("evaluator.zig");
const Evaluator = evaluator_module.Evaluator;
const Object = evaluator_module.Object;
const Environment = evaluator_module.Environment;
const String = evaluator_module.String;

const test_utils = @import("test_utils.zig");
const runTests = test_utils.runTests;

const DEBUG = false;

pub fn assertResult(arena: std.mem.Allocator, input: []const u8, expected: Object) !void {
    var lexer = try Lexer.init(arena, input);
    var parser = try Parser.init(&lexer, .{ .arena = arena, .debug = DEBUG });
    const program = try parser.parseProgram();

    const allocator = std.testing.allocator;

    const env = try Environment.init(.{ .gpa = allocator, .debug = DEBUG });
    defer env.deinit();

    var evaluator = Evaluator.init(.{ .gpa = allocator, .debug = DEBUG });
    const result = try evaluator.eval(&program, env);

    try expectObject(expected, result);
}

const ObjectTestCase = struct {
    input: []const u8,
    description: []const u8,
    expected_output: Object,
    expected_error: ?anyerror = null,
};

fn expectObject(expected: Object, actual: Object) !void {
    const Tag = std.meta.Tag(@TypeOf(expected));

    const expectedTag = @as(Tag, expected);
    const actualTag = @as(Tag, actual);

    try std.testing.expectEqual(expectedTag, actualTag);

    switch (expected) {
        .String => |string| {
            try std.testing.expectEqual(string.ref_count, actual.String.ref_count);
            try std.testing.expectEqualStrings(string.data, actual.String.data);
        },
        else => try std.testing.expectEqual(expected, actual),
    }
}

const runObjectTest = struct {
    fn runTest(arena: std.mem.Allocator, test_case: ObjectTestCase) anyerror!void {
        const result = assertResult(arena, test_case.input, test_case.expected_output);
        if (test_case.expected_error) |expected_error| {
            try std.testing.expectError(expected_error, result);
        } else {
            try result;
        }
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
        .{ .description = "greater than integer", .input = "7 > 6", .expected_output = Object{ .Boolean = true } },
        .{ .description = "less than float", .input = "5.1 < 1.3", .expected_output = Object{ .Boolean = false } },
        .{ .description = "greater than float", .input = "5.1 > 1.3", .expected_output = Object{ .Boolean = true } },
        .{ .description = "less than or equal integer", .input = "7 <= 6", .expected_output = Object{ .Boolean = false } },
        .{ .description = "less than or equal integer equal", .input = "7 <= 7", .expected_output = Object{ .Boolean = true } },
        .{ .description = "greater than or equal integer", .input = "7 >= 6", .expected_output = Object{ .Boolean = true } },
        .{ .description = "greater than or equal integer equal", .input = "7 >= 7", .expected_output = Object{ .Boolean = true } },
        .{ .description = "less than or equal float", .input = "5.1 <= 1.3", .expected_output = Object{ .Boolean = false } },
        .{ .description = "less than or equal float equal", .input = "5.1 <= 5.1", .expected_output = Object{ .Boolean = true } },
        .{ .description = "greater than or equal float", .input = "5.1 >= 1.3", .expected_output = Object{ .Boolean = true } },
        .{ .description = "greater than or equal float equal", .input = "5.1 >= 5.1", .expected_output = Object{ .Boolean = true } },
        .{ .description = "logical or", .input = "true or false", .expected_output = Object{ .Boolean = true } },
        .{ .description = "logical and", .input = "true and false", .expected_output = Object{ .Boolean = false } },
        .{ .description = "bitwise or", .input = "1 | 2", .expected_output = Object{ .Integer = 3 } },
        .{ .description = "bitwise and", .input = "3 & 2", .expected_output = Object{ .Integer = 2 } },
        .{ .description = "bitwise xor", .input = "1 ^ 4", .expected_output = Object{ .Integer = 5 } },
        .{ .description = "mod integer", .input = "33 % 2", .expected_output = Object{ .Integer = 1 } },
        .{ .description = "mod float", .input = "5.5 % 3.1", .expected_output = Object{ .Float = 2.4 } },
        .{ .description = "left shift", .input = "1 << 4", .expected_output = Object{ .Integer = 16 } },
        .{ .description = "right shift", .input = "16 >> 4", .expected_output = Object{ .Integer = 1 } },
    };

    try runTests(ObjectTestCase, "evaluate infix expression", &test_cases, runObjectTest);
}

test "function calls" {
    const test_cases = [_]ObjectTestCase{
        .{
            .description = "function call with return",
            .input =
            \\square = (a) -> {
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
            \\square = (a) -> {
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
            \\add = (a) -> {
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
            \\add = (a) -> {
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
            \\subtract = (a, b) -> {
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
            \\callback = (x) -> {
            \\    x * x
            \\}
            \\
            \\test = (val, cb) -> {
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
            \\test = () -> {
            \\    inner = (y) -> { y * y }
            \\    inner(10)
            \\}
            \\test()
            ,
            .expected_output = Object{ .Integer = 100 },
        },
        .{
            .description = "function call higher order function",
            .input =
            \\adder = (val) -> {
            \\    inner = (x) -> { val + x }
            \\    inner
            \\}
            \\add_five = adder(5)
            \\add_five(10)
            ,
            .expected_output = Object{ .Integer = 15 },
        },
        .{
            .description = "function call without braces",
            .input =
            \\square = (a) -> a * a
            \\square(11)
            ,
            .expected_output = Object{ .Integer = 121 },
        },
        .{
            .description = "function with default args",
            .input =
            \\foo = (a = 1, b = 2) -> a + b
            \\foo()
            ,
            .expected_output = Object{ .Integer = 3 },
        },
        .{
            .description = "function call with keyword arguments",
            .input =
            \\foo = (a, b, c) -> a * (b + c)
            \\foo(c = 1, b = 2, a = 3)
            ,
            .expected_output = Object{ .Integer = 9 },
        },
        .{
            .description = "function call with keyword arguments",
            .input =
            \\foo = (a, b, c) -> a * (b + c)
            \\foo(c = 1, b = 2, a = 3)
            ,
            .expected_output = Object{ .Integer = 9 },
        },
        .{
            .description = "function call with positional and keyword arguments",
            .input =
            \\foo = (a, b, c) -> a * (b + c)
            \\foo(1, c = 10, b = 5)
            ,
            .expected_output = Object{ .Integer = 15 },
        },
        .{
            .description = "function call with positional and keyword arguments and default args",
            .input =
            \\foo = (a = 5, b = 3, c = 1) -> a * (b + c)
            \\foo(3, c = 4)
            ,
            .expected_output = Object{ .Integer = 21 },
        },
        // TODO: how to make sure the string is not deallocated before testing it?
        // .{
        //     .description = "function call with string default args",
        //     .input =
        //     \\foo = (first = "Hello, ", second = "World!") -> first + second
        //     \\foo()
        //     ,
        //     .expected_output = Object{ .String = String{ .data = "Hello, World!", .ref_count = 0 } },
        // },
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
            \\calculate = (x) -> { y * x }
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
            \\fib = (n) -> {
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

test "table" {
    const test_cases = [_]ObjectTestCase{
        .{
            .description = "simple table",
            .input =
            \\table = { a = 1, b = 2, c = 3 }
            \\table["a"]
            ,
            .expected_output = Object{ .Integer = 1 },
        },
        .{
            .description = "index expression in infix",
            .input =
            \\table = { a = 1, b = 2, c = 3 }
            \\table["a"] - table["c"]
            ,
            .expected_output = Object{ .Integer = -2 },
        },
        .{
            .description = "empty table assignment",
            .input =
            \\table = {}
            \\table["a"] = 1
            \\table["a"]
            ,
            .expected_output = Object{ .Integer = 1 },
        },
    };

    try runTests(ObjectTestCase, "evaluate table", &test_cases, runObjectTest);
}

test "array" {
    const test_cases = [_]ObjectTestCase{
        .{
            .description = "simple array",
            .input =
            \\array = [1, 2, 3, 4, 5]
            \\array[3]
            ,
            .expected_output = Object{ .Integer = 4 },
        },
        .{
            .description = "index expression in infix",
            .input =
            \\array = [1, 2, 3, 4, 5]
            \\array[2] * array[4]
            ,
            .expected_output = Object{ .Integer = 15 },
        },
        .{
            .description = "empty array assignment",
            .input =
            \\array = [1, 2, 3]
            \\array[0] = 100
            \\array[0]
            ,
            .expected_output = Object{ .Integer = 100 },
        },
    };

    try runTests(ObjectTestCase, "evaluate array", &test_cases, runObjectTest);
}

test "string" {
    const test_cases = [_]ObjectTestCase{
        .{
            .description = "equals true",
            .input =
            \\"test" == "test"
            ,
            .expected_output = Object{ .Boolean = true },
        },
        .{
            .description = "equals false",
            .input =
            \\"test" == "not test"
            ,
            .expected_output = Object{ .Boolean = false },
        },
        .{
            .description = "not equals true",
            .input =
            \\"test" != "not test"
            ,
            .expected_output = Object{ .Boolean = true },
        },
        .{
            .description = "not equals false",
            .input =
            \\"test" != "test"
            ,
            .expected_output = Object{ .Boolean = false },
        },
    };

    try runTests(ObjectTestCase, "evaluate string", &test_cases, runObjectTest);
}

test "program" {
    const test_cases = [_]ObjectTestCase{
        .{
            .description = "return array from function",
            .input =
            \\test = () -> {
            \\    return [1, 2, 3, 4, 5]
            \\}
            \\array = test()
            \\array[2]
            ,
            .expected_output = Object{ .Integer = 3 },
        },
    };

    try runTests(ObjectTestCase, "evaluate program", &test_cases, runObjectTest);
}

test "memory leaks" {
    const TestCase = struct {
        description: []const u8,
        input: []const u8,
    };

    const run = struct {
        fn runTest(arena: std.mem.Allocator, test_case: TestCase) anyerror!void {
            var lexer = try Lexer.init(arena, test_case.input);
            var parser = try Parser.init(&lexer, .{ .arena = arena, .debug = DEBUG });
            const program = try parser.parseProgram();

            const allocator = std.testing.allocator;

            const env = try Environment.init(.{ .gpa = allocator, .debug = DEBUG });
            defer env.deinit();

            var evaluator = Evaluator.init(.{ .gpa = allocator, .debug = DEBUG });
            const result = try evaluator.eval(&program, env);
            _ = result;
        }
    }.runTest;

    const test_cases = [_]TestCase{
        .{
            .description = "unassigned array on top level",
            .input =
            \\[1, 2, 3, 4, 5]
            ,
        },
        .{
            .description = "unassigned array in index expression on top level",
            .input =
            \\[1, 2, 3, 4, 5][0]
            ,
        },
        .{
            .description = "unassigned array in function body",
            .input =
            \\test = (a, b) -> {
            \\    [1, 2, 3, 4, 5]
            \\    a + b
            \\}
            \\test(1, 2)
            ,
        },
        .{
            .description = "unassigned array returned from function call",
            .input =
            \\test = () -> {
            \\    return [1, 2, 3, 4, 5]
            \\}
            \\test()
            ,
        },
        .{
            .description = "assigned array on top level",
            .input =
            \\array = [1, 2, 3, 4, 5]
            ,
        },
        .{
            .description = "assigned array in function body",
            .input =
            \\test = () -> {
            \\    array = [1, 2, 3, 4, 5]
            \\    return array[0] + array[1]
            \\}
            \\test()
            ,
        },
        .{
            .description = "string literal",
            .input =
            \\"Hello, World!"
            ,
        },
        .{
            .description = "string assignment",
            .input =
            \\s = "Hello, World!"
            ,
        },
        .{
            .description = "array of strings",
            .input =
            \\["a", "b", "c", "d", "e"]
            ,
        },
        .{
            .description = "array of strings assignment",
            .input =
            \\a = ["a", "b", "c", "d", "e"]
            ,
        },
        .{
            .description = "array of array of strings",
            .input =
            \\[["a", "b"], ["c", "d"], ["e", "f"]]
            ,
        },
        .{
            .description = "table literal",
            .input =
            \\{a = 1, b = 2, c = 3}
            ,
        },
        .{
            .description = "table assignment",
            .input =
            \\t = {a = 1, b = 2, c = 3}
            ,
        },
        .{
            .description = "table literal of strings",
            .input =
            \\{a = "a", b = "b", c = "c"}
            ,
        },
        .{
            .description = "string concatenation",
            .input =
            \\"Hello, " + "World!"
            ,
        },
        .{
            .description = "string concatenation with string vars",
            .input =
            \\first = "Hello, "
            \\second = "World!"
            \\first + second
            ,
        },
        .{
            .description = "string concatenation with string vars and assignment",
            .input =
            \\first = "Hello, "
            \\second = "World!"
            \\result = first + second
            ,
        },
        .{
            .description = "string concatenation with string vars and assignment",
            .input =
            \\s = "a"
            \\s
            \\s + "b"
            \\s + "c"
            ,
        },
    };

    try runTests(TestCase, "evaluate memory leaks", &test_cases, run);
}
