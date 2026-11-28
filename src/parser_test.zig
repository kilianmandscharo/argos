const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");
const parser = @import("parser.zig");
const ast = @import("ast.zig");
const vm = @import("vm.zig");

const Test = struct {
    arena: std.mem.Allocator,

    fn e(self: *Test, data: ast.ExpressionData) *const ast.Expression {
        const expr = self.arena.create(ast.Expression) catch @panic("oopsie");
        expr.* = .{ .span = .{ .start = 0, .end = 0 }, .data = data };
        return expr;
    }

    fn Ident(self: *Test, val: []const u8) *const ast.Expression {
        return self.e(.{ .Identifier = val });
    }

    fn Int(self: *Test, val: i64) *const ast.Expression {
        return self.e(.{ .Integer = val });
    }

    fn Float(self: *Test, val: f64) *const ast.Expression {
        return self.e(.{ .Float = val });
    }

    fn String(self: *Test, val: []const u8) *const ast.Expression {
        return self.e(.{ .String = val });
    }

    fn Bool(self: *Test, val: bool) *const ast.Expression {
        return self.e(.{ .Boolean = val });
    }

    fn Unit(self: *Test) *const ast.Expression {
        return self.e(.{ .Unit = {} });
    }

    fn List(self: *Test, values: []const *const ast.Expression) *const ast.Expression {
        return self.e(.{ .List = values });
    }

    fn Record(self: *Test, values: []const ast.RecordItem) *const ast.Expression {
        return self.e(.{ .Record = values });
    }

    fn Grouping(self: *Test, values: []const *const ast.Expression) *const ast.Expression {
        return self.e(.{ .Grouping = values });
    }

    fn Unary(self: *Test, op: scanner.TokenType, right: *const ast.Expression) *const ast.Expression {
        return self.e(.{ .Unary = .{ .operator = op, .right = right } });
    }

    fn Binary(self: *Test, left: *const ast.Expression, op: scanner.TokenType, right: *const ast.Expression) *const ast.Expression {
        return self.e(.{ .Binary = .{ .left = left, .operator = op, .right = right } });
    }

    fn Access(self: *Test, left: *const ast.Expression, right: []const u8) *const ast.Expression {
        return self.e(.{ .Access = .{ .left = left, .right = right } });
    }

    fn Decl(self: *Test, left: []const u8, right: *const ast.Expression) *const ast.Expression {
        return self.e(.{ .Declaration = .{ .left = left, .right = right } });
    }

    fn Call(self: *Test, func: *const ast.Expression, args: []const *const ast.Expression) *const ast.Expression {
        return self.e(.{ .Call = .{ .function = func, .args = args } });
    }

    fn Func(self: *Test, name: ?[]const u8, params: []const ast.FunctionParam, body: *const ast.Expression) *const ast.Expression {
        return self.e(.{ .Function = .{ .name = name, .params = params, .body = body } });
    }

    fn If(self: *Test, arms: []const ast.IfArm, alt: ?*const ast.Expression) *const ast.Expression {
        return self.e(.{ .If = .{ .arms = arms, .alt = alt } });
    }

    fn Match(self: *Test, target: *const ast.Expression, arms: []const ast.MatchArm) *const ast.Expression {
        return self.e(.{ .Match = .{ .target = target, .arms = arms } });
    }
};

fn expectTag(expected: anytype, actual: anytype) !void {
    try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
}

fn expectExpression(expected: *const ast.Expression, actual: *const ast.Expression) anyerror!void {
    try expectExpressionData(expected.data, actual.data);
}

fn expectExpressionData(expected: ast.ExpressionData, actual: ast.ExpressionData) !void {
    try expectTag(expected, actual);

    switch (expected) {
        .Declaration => |declaration| {
            try std.testing.expectEqualStrings(declaration.left, actual.Declaration.left);
            try expectExpression(declaration.right, actual.Declaration.right);
        },
        .Grouping => |expr| {
            try std.testing.expectEqual(expr.len, actual.Grouping.len);
            for (0..expr.len) |i| {
                try expectExpression(expr[i], actual.Grouping[i]);
            }
        },
        .Identifier => |identifier| try std.testing.expectEqualStrings(identifier, actual.Identifier),
        .String => |string| try std.testing.expectEqualStrings(string, actual.String),
        .Integer => |n| try std.testing.expectEqual(n, actual.Integer),
        .Float => |f| try std.testing.expectEqual(f, actual.Float),
        .Boolean => |b| try std.testing.expectEqual(b, actual.Boolean),
        .Unary => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Unary.operator);
            try expectExpression(expr.right, actual.Unary.right);
        },
        .Binary => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Binary.operator);
            try expectExpression(expr.left, actual.Binary.left);
            try expectExpression(expr.right, actual.Binary.right);
        },
        .Function => |func| {
            if (func.name) |name| {
                try std.testing.expectEqualStrings(name, actual.Function.name.?);
            } else {
                try std.testing.expectEqual(null, actual.Function.name);
            }
            try std.testing.expectEqual(func.params.len, actual.Function.params.len);
            for (0..func.params.len) |i| {
                const first = func.params[i];
                const second = actual.Function.params[i];
                try expectTag(first, second);
                if (first == .Required) {
                    try std.testing.expectEqualStrings(first.Required, second.Required);
                } else {
                    try std.testing.expectEqualStrings(first.Default.name, second.Default.name);
                    try expectExpression(first.Default.value, second.Default.value);
                }
            }
            try expectExpression(func.body, actual.Function.body);
        },
        .Call => |expr| {
            try expectExpression(expr.function, actual.Call.function);
            try std.testing.expectEqual(expr.args.len, actual.Call.args.len);
            for (0..expr.args.len) |i| {
                const first = expr.args[i];
                const second = actual.Call.args[i];
                try expectExpression(first, second);
            }
        },
        .Access => |expr| {
            try expectExpression(expr.left, actual.Access.left);
            try std.testing.expectEqualStrings(expr.right, actual.Access.right);
        },
        .List => |expr| {
            try std.testing.expectEqual(expr.len, actual.List.len);
            for (expr, 0..) |item, i| {
                try expectExpression(item, actual.List[i]);
            }
        },
        .Record => |expr| {
            try std.testing.expectEqual(expr.len, actual.Record.len);
            for (expr, 0..) |entry, i| {
                try std.testing.expectEqualStrings(entry.key, actual.Record[i].key);
                try expectExpression(entry.value, actual.Record[i].value);
            }
        },
        .Match => |expr| {
            try expectExpression(expr.target, actual.Match.target);
            try std.testing.expectEqual(expr.arms.len, actual.Match.arms.len);
            for (0..expr.arms.len) |i| {
                const first = expr.arms[i];
                const second = actual.Match.arms[i];
                try expectExpression(first.pattern, second.pattern);
                try expectExpression(first.expression, second.expression);
            }
        },
        .If => |expr| {
            try std.testing.expectEqual(expr.arms.len, actual.If.arms.len);
            for (0..expr.arms.len) |i| {
                const first = expr.arms[i];
                const second = actual.If.arms[i];
                try expectExpression(first.condition, second.condition);
                try expectExpression(first.then, second.then);
            }
            if (expr.alt) |alt| {
                try expectExpression(alt, actual.If.alt.?);
            } else {
                try std.testing.expect(actual.If.alt == null);
            }
        },
        .Unit => {
            try std.testing.expect(actual == .Unit);
        },
    }
}

const ExpressionTestCase = struct {
    description: []const u8,
    input: []const u8,
    expected_expression: ?*const ast.Expression = null,
    expect_error: bool = false,
};

fn runExpressionTest(arena: std.mem.Allocator, test_case: ExpressionTestCase) anyerror!void {
    var ctx: vm.ScriptContext = .{
        .file_name = "scanner_test",
        .source = test_case.input,
        .lines = .empty,
        .static_arena = arena,
    };

    const result = parser.createAst(arena, &ctx);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const program = try result;
        const expressions = program.data.Function.body.data.Grouping;
        try std.testing.expectEqual(expressions.len, 1);
        try expectExpression(test_case.expected_expression.?, expressions[0]);
    }
}

test "expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse identifier",
            .input =
            \\foo
            ,
            .expected_expression = t.Ident("foo"),
        },
        .{
            .description = "should parse string literal",
            .input =
            \\"foo"
            ,
            .expected_expression = t.String("foo"),
        },
        .{
            .description = "should parse integer literal",
            .input =
            \\666
            ,
            .expected_expression = t.Int(666),
        },
        .{
            .description = "should parse float literal",
            .input =
            \\3.1415
            ,
            .expected_expression = t.Float(3.1415),
        },
        .{
            .description = "should parse true",
            .input =
            \\true
            ,
            .expected_expression = t.Bool(true),
        },
        .{
            .description = "should parse false",
            .input =
            \\false
            ,
            .expected_expression = t.Bool(false),
        },
        .{
            .description = "should parse unit",
            .input =
            \\()
            ,
            .expected_expression = t.Unit(),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse expressions",
        &test_cases,
        runExpressionTest,
    );
}

test "unary expressions" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse bang operator with true",
            .input = "!true",
            .expected_expression = t.Unary(.Bang, t.Bool(true)),
        },
        .{
            .description = "should parse bang operator with false",
            .input = "!false",
            .expected_expression = t.Unary(.Bang, t.Bool(false)),
        },
        .{
            .description = "should parse plus operator with integer",
            .input = "+5",
            .expected_expression = t.Unary(.Plus, t.Int(5)),
        },
        .{
            .description = "should parse minus operator with integer",
            .input = "-2",
            .expected_expression = t.Unary(.Minus, t.Int(2)),
        },
        .{
            .description = "should parse tilde operator with integer",
            .input = "~5",
            .expected_expression = t.Unary(.Tilde, t.Int(5)),
        },
        .{
            .description = "should parse plus operator with float",
            .input = "+5.41",
            .expected_expression = t.Unary(.Plus, t.Float(5.41)),
        },
        .{
            .description = "should parse minus operator with float",
            .input = "-2.1234",
            .expected_expression = t.Unary(.Minus, t.Float(2.1234)),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse unary expression",
        &test_cases,
        runExpressionTest,
    );
}

test "binary expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse integer addition",
            .input = "1 + 1",
            .expected_expression = t.Binary(t.Int(1), .Plus, t.Int(1)),
        },
        .{
            .description = "should parse float addition",
            .input = "1.1 + 1.35",
            .expected_expression = t.Binary(t.Float(1.1), .Plus, t.Float(1.35)),
        },
        .{
            .description = "should parse integer subtraction",
            .input = "40 - 22",
            .expected_expression = t.Binary(t.Int(40), .Minus, t.Int(22)),
        },
        .{
            .description = "should parse float subtraction",
            .input = "40.54 - 22.33",
            .expected_expression = t.Binary(t.Float(40.54), .Minus, t.Float(22.33)),
        },
        .{
            .description = "should parse integer multiplication",
            .input = "5 * 66",
            .expected_expression = t.Binary(t.Int(5), .Asterisk, t.Int(66)),
        },
        .{
            .description = "should parse float multiplication",
            .input = "5.3 * 66.5",
            .expected_expression = t.Binary(t.Float(5.3), .Asterisk, t.Float(66.5)),
        },
        .{
            .description = "should parse integer division",
            .input = "6 / 2",
            .expected_expression = t.Binary(t.Int(6), .Slash, t.Int(2)),
        },
        .{
            .description = "should parse float division",
            .input = "6.55 / 2.413",
            .expected_expression = t.Binary(t.Float(6.55), .Slash, t.Float(2.413)),
        },
        .{
            .description = "should parse integer mod",
            .input = "33 % 2",
            .expected_expression = t.Binary(t.Int(33), .Percent, t.Int(2)),
        },
        .{
            .description = "should parse float mod",
            .input = "1.1 % 5.3",
            .expected_expression = t.Binary(t.Float(1.1), .Percent, t.Float(5.3)),
        },
        .{
            .description = "should preserve order of operations without parens",
            .input = "3 * 4 / 3",
            .expected_expression = t.Binary(
                t.Binary(t.Int(3), .Asterisk, t.Int(4)),
                .Slash,
                t.Int(3),
            ),
        },
        .{
            .description = "should respect parentheses in first pos",
            .input = "(3 * 4) / 3",
            .expected_expression = t.Binary(
                t.Binary(t.Int(3), .Asterisk, t.Int(4)),
                .Slash,
                t.Int(3),
            ),
        },
        .{
            .description = "should respect parentheses in second pos",
            .input = "3 * (4 / 3)",
            .expected_expression = t.Binary(
                t.Int(3),
                .Asterisk,
                t.Binary(t.Int(4), .Slash, t.Int(3)),
            ),
        },
        .{
            .description = "should parse integer less than",
            .input = "1 < 5",
            .expected_expression = t.Binary(t.Int(1), .Lt, t.Int(5)),
        },
        .{
            .description = "should parse integer less than or equal",
            .input = "1 <= 5",
            .expected_expression = t.Binary(t.Int(1), .LtOrEq, t.Int(5)),
        },
        .{
            .description = "should parse integer greater than",
            .input = "1 > 5",
            .expected_expression = t.Binary(t.Int(1), .Gt, t.Int(5)),
        },
        .{
            .description = "should parse integer greater than or equal",
            .input = "1 >= 5",
            .expected_expression = t.Binary(t.Int(1), .GtOrEq, t.Int(5)),
        },
        .{
            .description = "should parse integer equals",
            .input = "3 == 3",
            .expected_expression = t.Binary(t.Int(3), .Eq, t.Int(3)),
        },
        .{
            .description = "should parse integer does not equal",
            .input = "3 != 3",
            .expected_expression = t.Binary(t.Int(3), .NotEq, t.Int(3)),
        },
        .{
            .description = "should parse integer bitwise or",
            .input = "3 | 3",
            .expected_expression = t.Binary(t.Int(3), .Pipe, t.Int(3)),
        },
        .{
            .description = "should parse integer bitwise and",
            .input = "3 & 3",
            .expected_expression = t.Binary(t.Int(3), .Ampersand, t.Int(3)),
        },
        .{
            .description = "should parse integer bitwise xor",
            .input = "3 ^ 3",
            .expected_expression = t.Binary(t.Int(3), .Caret, t.Int(3)),
        },
        .{
            .description = "should parse integer shift left",
            .input = "3 << 3",
            .expected_expression = t.Binary(t.Int(3), .LeftShift, t.Int(3)),
        },
        .{
            .description = "should parse integer shift right",
            .input = "3 >> 3",
            .expected_expression = t.Binary(t.Int(3), .RightShift, t.Int(3)),
        },

        // Boolean
        .{
            .description = "should parse boolean equals",
            .input = "true == false",
            .expected_expression = t.Binary(t.Bool(true), .Eq, t.Bool(false)),
        },
        .{
            .description = "should parse boolean does not equal",
            .input = "true != false",
            .expected_expression = t.Binary(t.Bool(true), .NotEq, t.Bool(false)),
        },
        .{
            .description = "should parse boolean logical or",
            .input = "true or false",
            .expected_expression = t.Binary(t.Bool(true), .Or, t.Bool(false)),
        },
        .{
            .description = "should parse boolean logical and",
            .input = "true and false",
            .expected_expression = t.Binary(t.Bool(true), .And, t.Bool(false)),
        },
        .{
            .description = "should parse cons",
            .input = "1 :: []",
            .expected_expression = t.Binary(t.Int(1), .Cons, t.List(&.{})),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse binary expression",
        &test_cases,
        runExpressionTest,
    );
}

test "list literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty list",
            .input =
            \\[]
            ,
            .expected_expression = t.List(&.{}),
        },
        .{
            .description = "empty list with new line",
            .input =
            \\[
            \\
            \\]
            ,
            .expected_expression = t.List(&.{}),
        },
        .{
            .description = "list one line",
            .input =
            \\[1, 2]
            ,
            .expected_expression = t.List(&.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "list one line trailing comma",
            .input =
            \\[1, 2,]
            ,
            .expected_expression = t.List(&.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "list multiple lines",
            .input =
            \\[
            \\    1,
            \\    2
            \\]
            ,
            .expected_expression = t.List(&.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "list multiple lines trailing comma",
            .input =
            \\[
            \\    1,
            \\    2,
            \\]
            ,
            .expected_expression = t.List(&.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "list multiple lines whitespace",
            .input =
            \\[
            \\
            \\    1,
            \\
            \\    2
            \\
            \\]
            ,
            .expected_expression = t.List(&.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "list multiple lines trailing comma whitespace",
            .input =
            \\[
            \\
            \\    1,
            \\
            \\    2,
            \\
            \\]
            ,
            .expected_expression = t.List(&.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "eof",
            .input =
            \\[ 1, 2,
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma",
            .input =
            \\[ 1 2 ]
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma multiple lines",
            .input =
            \\[
            \\    1
            \\    2
            \\]
            ,
            .expect_error = true,
        },
        .{
            .description = "list of lists",
            .input =
            \\[
            \\    [1, 2],
            \\    [3, 4],
            \\]
            ,
            .expected_expression = t.List(&.{
                t.List(&.{ t.Int(1), t.Int(2) }),
                t.List(&.{ t.Int(3), t.Int(4) }),
            }),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse list literal",
        &test_cases,
        runExpressionTest,
    );
}

test "record literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty",
            .input =
            \\{}
            ,
            .expected_expression = t.Record(&.{}),
        },
        .{
            .description = "empty with new line",
            .input =
            \\{
            \\
            \\}
            ,
            .expected_expression = t.Record(&.{}),
        },
        .{
            .description = "one line",
            .input =
            \\{ foo = 1, bar = 2 }
            ,
            .expected_expression = t.Record(&.{
                .{ .key = "bar", .value = t.Int(2) },
                .{ .key = "foo", .value = t.Int(1) },
            }),
        },
        .{
            .description = "one line trailing comma",
            .input =
            \\{ foo = 1, bar = 2, }
            ,
            .expected_expression = t.Record(&.{
                .{ .key = "bar", .value = t.Int(2) },
                .{ .key = "foo", .value = t.Int(1) },
            }),
        },
        .{
            .description = "multiple lines",
            .input =
            \\{
            \\    foo = 1,
            \\    bar = 2
            \\}
            ,
            .expected_expression = t.Record(&.{
                .{ .key = "bar", .value = t.Int(2) },
                .{ .key = "foo", .value = t.Int(1) },
            }),
        },
        .{
            .description = "multiple lines trailing comma",
            .input =
            \\{
            \\    foo = 1,
            \\    bar = 2,
            \\}
            ,
            .expected_expression = t.Record(&.{
                .{ .key = "bar", .value = t.Int(2) },
                .{ .key = "foo", .value = t.Int(1) },
            }),
        },
        .{
            .description = "multiple lines with whitespace",
            .input =
            \\{
            \\
            \\    foo = 1,
            \\
            \\    bar = 2
            \\
            \\}
            ,
            .expected_expression = t.Record(&.{
                .{ .key = "bar", .value = t.Int(2) },
                .{ .key = "foo", .value = t.Int(1) },
            }),
        },
        .{
            .description = "multiple lines with whitespace trailing comma",
            .input =
            \\{
            \\
            \\    foo = 1,
            \\
            \\    bar = 2,
            \\
            \\}
            ,
            .expected_expression = t.Record(&.{
                .{ .key = "bar", .value = t.Int(2) },
                .{ .key = "foo", .value = t.Int(1) },
            }),
        },
        .{
            .description = "eof",
            .input =
            \\{ foo = 1, bar = 2,
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma",
            .input =
            \\{ foo = 1 bar = 2 }
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma multiple lines",
            .input =
            \\{
            \\    foo = 1
            \\    bar = 2
            \\}
            ,
            .expect_error = true,
        },
        .{
            .description = "record of records",
            .input =
            \\{
            \\    foo = { a = 1 },
            \\    bar = { a = 2 },
            \\}
            ,
            .expected_expression = t.Record(&.{
                .{ .key = "bar", .value = t.Record(&.{.{ .key = "a", .value = t.Int(2) }}) },
                .{ .key = "foo", .value = t.Record(&.{.{ .key = "a", .value = t.Int(1) }}) },
            }),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse record literal",
        &test_cases,
        runExpressionTest,
    );
}

test "function call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "no args",
            .input =
            \\test()
            ,
            .expected_expression = t.Call(t.Ident("test"), &.{}),
        },
        .{
            .description = "with args one line",
            .input =
            \\test(1, 2)
            ,
            .expected_expression = t.Call(t.Ident("test"), &.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "with args one line trailing comma",
            .input =
            \\test(1, 2,)
            ,
            .expected_expression = t.Call(t.Ident("test"), &.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "with args multiple lines",
            .input =
            \\test(
            \\    1,
            \\    2
            \\)
            ,
            .expected_expression = t.Call(t.Ident("test"), &.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "with args multiple lines trailing comma",
            .input =
            \\test(
            \\    1,
            \\    2,
            \\)
            ,
            .expected_expression = t.Call(t.Ident("test"), &.{ t.Int(1), t.Int(2) }),
        },
        .{
            .description = "in binary expression",
            .input =
            \\test() + test()
            ,
            .expected_expression = t.Binary(
                t.Call(t.Ident("test"), &.{}),
                .Plus,
                t.Call(t.Ident("test"), &.{}),
            ),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse function call",
        &test_cases,
        runExpressionTest,
    );
}

test "function literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty",
            .input =
            \\() -> ()
            ,
            .expected_expression = t.Func(null, &.{}, t.Unit()),
        },
        .{
            .description = "with expression",
            .input =
            \\(a, b) -> a + b
            ,
            .expected_expression = t.Func(
                null,
                &.{ .{ .Required = "a" }, .{ .Required = "b" } },
                t.Binary(t.Ident("a"), .Plus, t.Ident("b")),
            ),
        },
        .{
            .description = "with grouping",
            .input =
            \\(a, b) -> (
            \\    c = a + b
            \\    a + c
            \\)
            ,
            .expected_expression = t.Func(
                null,
                &.{ .{ .Required = "a" }, .{ .Required = "b" } },
                t.Grouping(&.{
                    t.Decl("c", t.Binary(t.Ident("a"), .Plus, t.Ident("b"))),
                    t.Binary(t.Ident("a"), .Plus, t.Ident("c")),
                }),
            ),
        },
        .{
            .description = "with default args",
            .input =
            \\foo = (a = 11, b = 12) -> a + b
            ,
            .expected_expression = t.Decl("foo", t.Func(
                "foo",
                &.{
                    .{ .Default = .{ .name = "a", .value = t.Int(11) } },
                    .{ .Default = .{ .name = "b", .value = t.Int(12) } },
                },
                t.Binary(t.Ident("a"), .Plus, t.Ident("b")),
            )),
        },
        .{
            .description = "declare function",
            .input =
            \\foo = () -> ()
            ,
            .expected_expression = t.Decl("foo", t.Func("foo", &.{}, t.Unit())),
        },
        .{
            .description = "nested",
            .input =
            \\new = list_map([1], (n) -> n * 2)
            ,
            .expected_expression = t.Decl("new", t.Call(
                t.Ident("list_map"),
                &.{
                    t.List(&.{t.Int(1)}),
                    t.Func(
                        null,
                        &.{.{ .Required = "n" }},
                        t.Binary(t.Ident("n"), .Asterisk, t.Int(2)),
                    ),
                },
            )),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse function literal",
        &test_cases,
        runExpressionTest,
    );
}

test "match expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "one line",
            .input =
            \\match foo with | true -> 42
            ,
            .expected_expression = t.Match(t.Ident("foo"), &.{
                .{ .pattern = t.Bool(true), .expression = t.Int(42) },
            }),
        },
        .{
            .description = "multiple lines",
            .input =
            \\match something with
            \\| "foo" -> 42
            \\| "bar" -> 43
            \\| "baz" -> 44
            ,
            .expected_expression = t.Match(t.Ident("something"), &.{
                .{ .pattern = t.String("foo"), .expression = t.Int(42) },
                .{ .pattern = t.String("bar"), .expression = t.Int(43) },
                .{ .pattern = t.String("baz"), .expression = t.Int(44) },
            }),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse match expression",
        &test_cases,
        runExpressionTest,
    );
}

test "if expression" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "one line",
            .input =
            \\if foo then bar
            ,
            .expected_expression = t.If(&.{
                .{ .condition = t.Ident("foo"), .then = t.Ident("bar") },
            }, null),
        },
        .{
            .description = "with else",
            .input =
            \\if foo then
            \\    bar
            \\else
            \\    baz
            ,
            .expected_expression = t.If(&.{
                .{ .condition = t.Ident("foo"), .then = t.Ident("bar") },
            }, t.Ident("baz")),
        },
        .{
            .description = "with else if",
            .input =
            \\if foo then 42
            \\elif bar then 43
            \\elif baz then 44
            \\else 45
            ,
            .expected_expression = t.If(&.{
                .{ .condition = t.Ident("foo"), .then = t.Int(42) },
                .{ .condition = t.Ident("bar"), .then = t.Int(43) },
                .{ .condition = t.Ident("baz"), .then = t.Int(44) },
            }, t.Int(45)),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse if expression",
        &test_cases,
        runExpressionTest,
    );
}

test "grouping" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "with encasing new lines",
            .input =
            \\(
            \\    a = 1
            \\    b = 1
            \\    a + b
            \\)
            ,
            .expected_expression = t.Grouping(&.{
                t.Decl("a", t.Int(1)),
                t.Decl("b", t.Int(1)),
                t.Binary(t.Ident("a"), .Plus, t.Ident("b")),
            }),
        },
        .{
            .description = "without encasing new lines",
            .input =
            \\(a = 1
            \\ b = 1
            \\ a + b)
            ,
            .expected_expression = t.Grouping(&.{
                t.Decl("a", t.Int(1)),
                t.Decl("b", t.Int(1)),
                t.Binary(t.Ident("a"), .Plus, t.Ident("b")),
            }),
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse grouping",
        &test_cases,
        runExpressionTest,
    );
}

test "access" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();
    var t = Test{ .arena = alloc };

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "identifier on left side",
            .input =
            \\a.foo
            ,
            .expected_expression = t.Access(t.Ident("a"), "foo"),
        },
        .{
            .description = "record on left side",
            .input =
            \\{ a = 1 }.a
            ,
            .expected_expression = t.Access(
                t.Record(&.{.{ .key = "a", .value = t.Int(1) }}),
                "a",
            ),
        },
        .{
            .description = "in binary expression",
            .input =
            \\a.foo + a.bar
            ,
            .expected_expression = t.Binary(
                t.Access(t.Ident("a"), "foo"),
                .Plus,
                t.Access(t.Ident("a"), "bar"),
            ),
        },
        .{
            .description = "invalid right side",
            .input =
            \\a."foo"
            ,
            .expect_error = true,
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse access",
        &test_cases,
        runExpressionTest,
    );
}
