const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");

fn createAst(arena: std.mem.Allocator, source: []const u8) !Program {
    var scan = scanner.Scanner.init(source);
    var parser = try Parser.init(arena, &scan);
    return try parser.parseProgram();
}

fn printProgram(program: Program) void {
    for (program.items) |statement| {
        std.debug.print("{f}\n", .{statement});
    }
}

pub const Parser = struct {
    current: scanner.Token,
    previous: ?scanner.Token,
    scanner: *scanner.Scanner,
    arena: std.mem.Allocator,
    debug_indent: usize = 0,
    ctx: struct { parse_loop_capture: bool },

    pub fn init(arena: std.mem.Allocator, s: *scanner.Scanner) !Parser {
        const current = try s.next();
        if (current.type == .Eof) {
            return error.NoTokens;
        }
        return Parser{
            .scanner = s,
            .current = current,
            .previous = null,
            .arena = arena,
            .ctx = .{ .parse_loop_capture = false },
        };
    }

    fn log(self: *Parser, comptime fmt: []const u8, args: anytype) void {
        logging.log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = .Debug,
            .indent = self.debug_indent,
            .module = "Parser",
        });
    }

    pub fn parseProgram(self: *Parser) !Program {
        var statements: std.ArrayList(Statement) = .{};
        while (self.current.type != .Eof) {
            try self.chopNewlines();
            const statement = try self.parseStatement();
            try statements.append(self.arena, statement);
        }
        return statements;
    }

    fn parseStatement(self: *Parser) anyerror!Statement {
        try self.chopNewlines();
        if (try self.match(.Print)) {
            return try self.printStatement();
        } else if (try self.match(.Assert)) {
            return try self.assertStatement();
        } else if (try self.match(.While)) {
            return try self.whileStatement();
        } else if (try self.match(.For)) {
            return try self.forStatement();
        } else if (try self.match(.Return)) {
            return try self.returnStatement();
        } else if (try self.match(.LBrace)) {
            return try self.blockStatement();
        } else if (try self.match(.Let)) {
            return try self.varDeclaration();
        } else {
            return try self.expressionStatement();
        }
    }

    fn printStatement(self: *Parser) !Statement {
        self.log("print statement", .{});
        defer self.log("end print statement", .{});

        try self.consume(.LParen, "Expect '(' after print.");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after print expression.");

        _ = try self.match(.NewLine);

        return Statement{ .Print = expression };
    }

    fn assertStatement(self: *Parser) !Statement {
        self.log("assert statement", .{});
        defer self.log("end assert statement", .{});

        try self.consume(.LParen, "Expect '(' after assert.");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after assert expression.");

        _ = try self.match(.NewLine);

        return Statement{ .Assert = expression };
    }

    fn whileStatement(self: *Parser) !Statement {
        self.log("while statement", .{});
        defer self.log("end while statement", .{});

        try self.consume(.LParen, "Expect '(' after 'while'");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after condition");
        try self.consume(.LBrace, "Expect '{' after while condition");
        const body = try self.blockStatement();

        return Statement{
            .While = .{ .expression = expression, .body = body.Block },
        };
    }

    fn forStatement(self: *Parser) !Statement {
        self.log("for statement", .{});
        defer self.log("end for statement", .{});

        try self.consume(.LParen, "Expect '(' after 'for'");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after range");

        try self.consume(.Pipe, "Expect '|' after loop range");
        self.ctx.parse_loop_capture = true;

        const capture = try self.parseExpression();
        if (capture.* != .Identifier) {
            return self.errorAtPrevious("Expect identifier in loop capture");
        }
        var index: ?[]const u8 = null;
        if (!self.check(.Pipe)) {
            const second_capture = try self.parseExpression();
            if (second_capture.* != .Identifier) {
                return self.errorAtPrevious("Expect identifier in loop capture");
            }
            index = second_capture.Identifier;
        }

        try self.consume(.Pipe, "Expect '|' after for loop capture");
        self.ctx.parse_loop_capture = false;

        try self.consume(.LBrace, "Expect '{' after loop capture.");
        const body = try self.blockStatement();

        return Statement{
            .For = .{
                .expression = expression,
                .capture = capture.Identifier,
                .index = index,
                .body = body.Block,
            },
        };
    }

    fn returnStatement(self: *Parser) !Statement {
        self.log("return statement", .{});
        defer self.log("end return statement", .{});

        if (self.isLineEnd()) {
            const expression = try self.arena.create(Expression);
            expression.* = Expression.Null;
            return Statement{ .Return = expression };
        } else {
            const expression = try self.parseExpression();
            try self.expectLineEnd();
            return Statement{ .Return = expression };
        }
    }

    fn blockStatement(self: *Parser) !Statement {
        self.log("block statement", .{});
        defer self.log("end block statement", .{});

        try self.chopNewlines();
        var statements: std.ArrayList(Statement) = .{};
        while (!self.check(.RBrace) and !self.check(.Eof)) {
            try statements.append(self.arena, try self.parseStatement());
        }
        try self.consume(.RBrace, "Expect '}' after block.");
        try self.expectLineEnd();

        return Statement{ .Block = statements };
    }

    fn varDeclaration(self: *Parser) !Statement {
        self.log("var declaration", .{});
        defer self.log("end var declaration", .{});

        const target = try self.parseExpression();
        if (target.* != .Identifier) {
            return self.errorAtPrevious("Expect identifier after 'let'.");
        }

        if (try self.match(.Assign)) {
            const value = try self.parseExpression();
            try self.expectLineEnd();
            return Statement{
                .VarDeclaration = .{
                    .name = target.Identifier,
                    .expression = value,
                },
            };
        }

        try self.expectLineEnd();
        const nullExpression = try self.arena.create(Expression);
        nullExpression.* = .Null;
        return Statement{
            .VarDeclaration = .{
                .name = target.Identifier,
                .expression = nullExpression,
            },
        };
    }

    fn expressionStatement(self: *Parser) !Statement {
        const expression = try self.parseExpression();
        if (try self.match(.Assign)) {
            const target = switch (expression.*) {
                .Identifier => |name| AssignTarget{ .Identifier = name },
                .Index => |index| AssignTarget{ .Index = index },
                else => return self.errorAtPrevious("Invalid assign target."),
            };
            const value = try self.parseExpression();
            try self.expectLineEnd();
            return Statement{ .Assignment = .{ .target = target, .expression = value } };
        }
        try self.expectLineEnd();
        return Statement{ .Expression = expression };
    }

    fn parseExpression(self: *Parser) !*const Expression {
        return self.parsePrecedence(.Lowest);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !*const Expression {
        try self.advance();

        self.log("expression on {s}", .{self.previous.?.toString()});
        self.debug_indent += 1;

        if (getRule(self.previous.?.type).prefix) |prefixFn| {
            var left = try prefixFn(self);
            const left_owned = try self.arena.create(Expression);

            defer {
                left_owned.* = left;
                self.debug_indent -= 1;
                self.log("parsed {s}", .{@tagName(left_owned.*)});
            }

            while (@intFromEnum(precedence) < getRulePrecedenceValue(self, self.current.type)) {
                try self.advance();
                if (self.current.type == .Eof) break;
                if (getRule(self.previous.?.type).infix) |infixFn| {
                    left = try infixFn(self, left);
                }
            }

            return left_owned;
        } else {
            return self.errorAtPrevious("Expect expression.");
        }
    }

    fn parseCommaSeparated(self: *Parser, T: type, parseFn: *const fn (p: *Parser) anyerror!T, delimiter: scanner.TokenType) !std.ArrayList(T) {
        var items: std.ArrayList(T) = .{};
        var expectComma = false;

        while (!try self.match(delimiter)) {
            try self.chopNewlines();

            if (try self.match(.Eof)) return self.errorAtCurrent("Reached EOF.");
            if (try self.match(delimiter)) break;

            if (expectComma) return self.errorAtPrevious("Expected comma.");

            try items.append(self.arena, try parseFn(self));
            if (!try self.match(.Comma)) expectComma = true;
        }

        return items;
    }

    fn advance(self: *Parser) !void {
        self.previous = self.current;
        self.current = try self.scanner.next();
    }

    fn chopNewlines(self: *Parser) !void {
        while (self.check(.NewLine)) {
            try self.advance();
        }
    }

    fn consume(self: *Parser, expected: scanner.TokenType, message: []const u8) !void {
        if (self.check(expected)) {
            try self.advance();
            return;
        }
        return self.errorAtCurrent(message);
    }

    fn match(self: *Parser, token_type: scanner.TokenType) !bool {
        if (!self.check(token_type)) return false;
        try self.advance();
        return true;
    }

    fn check(self: *Parser, token_type: scanner.TokenType) bool {
        return self.current.type == token_type;
    }

    fn isLineEnd(self: *Parser) bool {
        const token_type = self.current.type;
        return token_type == .NewLine or token_type == .Eof;
    }

    fn expectLineEnd(self: *Parser) !void {
        if (self.isLineEnd()) {
            try self.advance();
            return;
        }
        return self.errorAtCurrent("Expected line end.");
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) anyerror {
        return errorAt(&self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) anyerror {
        return errorAt(&self.previous.?, message);
    }

    fn errorAt(token: *scanner.Token, message: []const u8) anyerror {
        std.debug.print("[line {d}] Error", .{token.line});
        switch (token.type) {
            .Eof => std.debug.print(" at end", .{}),
            else => std.debug.print(" at '{s}'", .{token.toString()}),
        }
        std.debug.print(": {s}\n", .{message});
        return error.ParserError;
    }
};

const Program = std.ArrayList(Statement);

pub const Statement = union(enum) {
    VarDeclaration: VarDeclaration,
    Block: Block,
    Assignment: Assignment,
    For: For,
    While: While,
    Return: *const Expression,
    Assert: *const Expression,
    Print: *const Expression,
    Expression: *const Expression,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .VarDeclaration => |val| try writer.print("VarDeclaration({s} = {f})", .{ val.name, val.expression }),
            .Block => try writer.print("Block", .{}),
            .Assignment => try writer.print("Assignment", .{}),
            .For => try writer.print("For", .{}),
            .While => try writer.print("While", .{}),
            .Return => try writer.print("Return", .{}),
            .Assert => try writer.print("Assert", .{}),
            .Print => try writer.print("Print", .{}),
            .Expression => |val| try writer.print("Expression {f}", .{val}),
        }
    }
};

const Block = std.ArrayList(Statement);

const VarDeclaration = struct {
    name: []const u8,
    expression: *const Expression,
};

const Assignment = struct {
    target: AssignTarget,
    expression: *const Expression,
};

const AssignTarget = union(enum) {
    Identifier: []const u8,
    Index: Index,
};

const For = struct {
    expression: *const Expression,
    capture: []const u8,
    index: ?[]const u8,
    body: Block,
};

const While = struct {
    expression: *const Expression,
    body: Block,
};

pub const Expression = union(enum) {
    Identifier: []const u8,
    String: []const u8,
    Integer: i64,
    Float: f64,
    Boolean: bool,
    Infix: Infix,
    Prefix: Prefix,
    Function: Function,
    Call: Call,
    Range: Range,
    List: std.ArrayList(*const Expression),
    Table: std.ArrayList(TablePair),
    Index: Index,
    Match: Match,
    Null,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .Identifier => |val| try writer.print("Identifier({s})", .{val}),
            .String => |val| try writer.print("String(\"{s}\")", .{val}),
            .Integer => |val| try writer.print("Integer({d})", .{val}),
            .Float => |val| try writer.print("Float({d})", .{val}),
            .Boolean => |val| try writer.print("Boolean({})", .{val}),
            .Infix => |val| try writer.print("Infix({f} {s} {f})", .{ val.left, val.operator.toString(), val.right }),
            .Prefix => |val| try writer.print("Prefix({s} {f})", .{ val.operator.toString(), val.expression }),
            .Function => |_| try writer.print("Fn", .{}),
            .Call => |_| try writer.print("Call", .{}),
            .Range => |_| try writer.print("Range", .{}),
            .List => |val| {
                try writer.print("List(", .{});
                for (val.items) |item| {
                    try writer.print("{f},", .{item});
                }
                try writer.print(")", .{});
            },
            .Table => |_| try writer.print("Table", .{}),
            .Index => |_| try writer.print("Index", .{}),
            .Match => |_| try writer.print("Match", .{}),
            .Null => try writer.print("Null", .{}),
        }
    }
};

const Infix = struct {
    operator: scanner.TokenType,
    left: *const Expression,
    right: *const Expression,
};

const Prefix = struct {
    operator: scanner.TokenType,
    expression: *const Expression,
};

const Function = struct {
    params: std.ArrayList(FunctionParam),
    body: FunctionBody,
};

const FunctionBody = union(enum) {
    Block: Block,
    Expression: *const Expression,
};

const FunctionParam = union(enum) {
    Positional: []const u8,
    Default: FunctionParamDefault,
};

const FunctionParamDefault = struct {
    name: []const u8,
    value: *const Expression,
};

const Call = struct {
    function: *const Expression,
    args: std.ArrayList(FunctionArg),
};

const FunctionArg = union(enum) {
    Positional: *const Expression,
    Named: FunctionArgNamed,
};

const FunctionArgNamed = struct {
    name: []const u8,
    value: *const Expression,
};

const Range = struct {
    start: *const Expression,
    end: *const Expression,
};

const TablePair = struct {
    key: *const Expression,
    value: *const Expression,
};

const Index = struct {
    left: *const Expression,
    index: *const Expression,
};

const Match = struct {
    target: ?*const Expression,
    body: MatchBody,
};

const MatchBody = union(enum) {
    Single: MatchArm,
    Multiple: std.ArrayList(MatchArm),
};

const MatchArm = struct {
    pattern: *const Expression,
    body: Statement,
};

const Precedence = enum(u8) {
    Lowest = 1,
    Range,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equals,
    LessGreater,
    Shift,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
};

fn parseString(parser: *Parser) !Expression {
    const s = parser.previous.?.toString();
    return Expression{ .String = s[1 .. s.len - 1] };
}

fn parseInteger(parser: *Parser) !Expression {
    return Expression{ .Integer = try std.fmt.parseInt(i64, parser.previous.?.toString(), 10) };
}

fn parseFloat(parser: *Parser) !Expression {
    return Expression{ .Float = try std.fmt.parseFloat(f64, parser.previous.?.toString()) };
}

fn parseLiteral(parser: *Parser) !Expression {
    return switch (parser.previous.?.type) {
        .True => Expression{ .Boolean = true },
        .False => Expression{ .Boolean = false },
        .Null => Expression.Null,
        else => unreachable,
    };
}

fn parseIdentifier(parser: *Parser) !Expression {
    return Expression{ .Identifier = parser.previous.?.toString() };
}

fn parseUnary(parser: *Parser) !Expression {
    const operator = parser.previous.?.type;
    const expression = try parser.parseExpression();
    return Expression{ .Prefix = .{ .operator = operator, .expression = expression } };
}

fn parseList(parser: *Parser) !Expression {
    try parser.consume(.LBrace, "Expect '{' after 'List'.");
    const items = try parser.parseCommaSeparated(
        *const Expression,
        struct {
            fn parse(p: *Parser) !*const Expression {
                return try p.parseExpression();
            }
        }.parse,
        .RBrace,
    );
    return Expression{ .List = items };
}

fn parseTable(parser: *Parser) !Expression {
    try parser.consume(.LBrace, "Expect '{' after 'Table'.");
    const items = try parser.parseCommaSeparated(
        TablePair,
        struct {
            fn parse(p: *Parser) !TablePair {
                const key = try p.parseExpression();
                try p.consume(.Assign, "Expect '=' after table key.");
                const value = try p.parseExpression();
                return TablePair{ .key = key, .value = value };
            }
        }.parse,
        .RBrace,
    );
    return Expression{ .Table = items };
}

fn parseFunction(parser: *Parser) !Expression {
    try parser.consume(.LParen, "Expect '(' after 'fn'.");
    const params = try parser.parseCommaSeparated(
        FunctionParam,
        struct {
            fn parse(p: *Parser) !FunctionParam {
                const expression = try p.parseExpression();
                if (try p.match(.Assign)) {
                    if (expression.* != .Identifier) {
                        return p.errorAtPrevious("Invalid left side in default param.");
                    }
                    const right = try p.parseExpression();
                    return FunctionParam{
                        .Default = .{ .name = expression.Identifier, .value = right },
                    };
                } else {
                    if (expression.* != .Identifier) {
                        return p.errorAtPrevious("Invalid function param.");
                    }
                    return FunctionParam{ .Positional = expression.Identifier };
                }
            }
        }.parse,
        .RParen,
    );

    if (try parser.match(.LBrace)) {
        const body = try parser.blockStatement();
        return Expression{
            .Function = .{ .params = params, .body = .{ .Block = body.Block } },
        };
    }

    const body = try parser.parseExpression();
    return Expression{
        .Function = .{
            .params = params,
            .body = .{ .Expression = body },
        },
    };
}

fn parseGrouping(parser: *Parser) !Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RParen, "Expect ')' after expression");
    return expression.*;
}

fn parseMatch(self: *Parser) !Expression {
    var target: ?*const Expression = null;

    if (try self.match(.LParen)) {
        target = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after match target");
    }

    if (!self.check(.LBrace)) {
        const pattern = try self.parseExpression();
        try self.consume(.Arrow, "Expect '->' after match pattern");
        const body = try self.parseStatement();
        return Expression{
            .Match = .{
                .target = target,
                .body = .{
                    .Single = .{ .pattern = pattern, .body = body },
                },
            },
        };
    }

    try self.advance();
    try self.consume(.NewLine, "Expect new line after '{' in match block");

    var arms: std.ArrayList(MatchArm) = .{};

    while (!self.check(.Eof) and !self.check(.RBrace)) {
        try self.chopNewlines();

        const pattern = try self.parseExpression();
        try self.consume(.Arrow, "Expect '->' after match pattern");
        const body = try self.parseStatement();

        try arms.append(self.arena, .{ .pattern = pattern, .body = body });
    }

    try self.consume(.RBrace, "Expect '}' at the end of match block");
    try self.consume(.NewLine, "Expect new line after match block");

    return Expression{
        .Match = .{ .target = target, .body = .{ .Multiple = arms } },
    };
}

fn parseBinary(parser: *Parser, left: Expression) !Expression {
    const operator = parser.previous.?.type;
    const right = try parser.parsePrecedence(getRulePrecedence(parser, operator));

    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;

    return Expression{
        .Infix = .{
            .operator = operator,
            .left = left_owned,
            .right = right,
        },
    };
}

fn parseDotDot(parser: *Parser, left: Expression) !Expression {
    const right = try parser.parseExpression();
    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;
    return Expression{ .Range = .{ .start = left_owned, .end = right } };
}

fn parseCall(parser: *Parser, left: Expression) !Expression {
    const args = try parser.parseCommaSeparated(
        FunctionArg,
        struct {
            fn parse(p: *Parser) !FunctionArg {
                const expression = try p.parseExpression();
                if (try p.match(.Assign)) {
                    if (expression.* != .Identifier) {
                        return p.errorAtPrevious("Invalid left side in named argument.");
                    }
                    const right = try p.parseExpression();
                    return FunctionArg{
                        .Named = FunctionArgNamed{ .name = expression.Identifier, .value = right },
                    };
                } else {
                    return FunctionArg{ .Positional = expression };
                }
            }
        }.parse,
        .RParen,
    );

    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;

    return Expression{
        .Call = .{
            .function = left_owned,
            .args = args,
        },
    };
}

fn parseIndex(parser: *Parser, left: Expression) !Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RBracket, "Expect ']' after index expression.");

    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;

    return Expression{
        .Index = .{
            .left = left_owned,
            .index = expression,
        },
    };
}

const ParseRule = struct {
    prefix: ?*const fn (parser: *Parser) anyerror!Expression,
    infix: ?*const fn (parser: *Parser, left: Expression) anyerror!Expression,
    precedence: ?Precedence,
};

const token_count = @typeInfo(scanner.TokenType).@"enum".fields.len;

const rules: [token_count]ParseRule = initRules();

fn initRules() [token_count]ParseRule {
    var table: [token_count]ParseRule = undefined;

    inline for (@typeInfo(scanner.TokenType).@"enum".fields) |field| {
        const index = field.value;
        const tag = @as(scanner.TokenType, @enumFromInt(index));
        table[index] = switch (tag) {
            .LParen => .{ .prefix = parseGrouping, .infix = parseCall, .precedence = .Call },
            .RParen => .{ .prefix = null, .infix = null, .precedence = null },
            .LBracket => .{ .prefix = null, .infix = parseIndex, .precedence = .Index },
            .RBracket => .{ .prefix = null, .infix = null, .precedence = null },
            .LBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .RBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .Assign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Comma => .{ .prefix = null, .infix = null, .precedence = null },
            .String => .{ .prefix = parseString, .infix = null, .precedence = null },
            .Float => .{ .prefix = parseFloat, .infix = null, .precedence = null },
            .Int => .{ .prefix = parseInteger, .infix = null, .precedence = null },
            .True => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
            .False => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
            .Null => .{ .prefix = parseLiteral, .infix = null, .precedence = null },
            .Identifier => .{ .prefix = parseIdentifier, .infix = null, .precedence = null },
            .Bang => .{ .prefix = parseUnary, .infix = null, .precedence = null },
            .Lt => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .LtOrEq => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .Gt => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .GtOrEq => .{ .prefix = null, .infix = parseBinary, .precedence = .LessGreater },
            .Eq => .{ .prefix = null, .infix = parseBinary, .precedence = .Equals },
            .NotEq => .{ .prefix = null, .infix = parseBinary, .precedence = .Equals },
            .Plus => .{ .prefix = parseUnary, .infix = parseBinary, .precedence = .Sum },
            .Minus => .{ .prefix = parseUnary, .infix = parseBinary, .precedence = .Sum },
            .Slash => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Asterisk => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Percent => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Return => .{ .prefix = null, .infix = null, .precedence = null },
            .Else => .{ .prefix = null, .infix = null, .precedence = null },
            .For => .{ .prefix = null, .infix = null, .precedence = null },
            .Dot => .{ .prefix = null, .infix = null, .precedence = .Index },
            .DotDot => .{ .prefix = null, .infix = parseDotDot, .precedence = .Range },
            .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
            .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
            .Eof => .{ .prefix = null, .infix = null, .precedence = null },
            .And => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalAnd },
            .Or => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalOr },
            .Pipe => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseOr },
            .Ampersand => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseAnd },
            .Caret => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseXor },
            .Tilde => .{ .prefix = parseUnary, .infix = null, .precedence = .Prefix },
            .LeftShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .RightShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .Print => .{ .prefix = null, .infix = null, .precedence = null },
            .Assert => .{ .prefix = null, .infix = null, .precedence = null },
            .Let => .{ .prefix = null, .infix = null, .precedence = null },
            .Match => .{ .prefix = parseMatch, .infix = null, .precedence = null },
            .While => .{ .prefix = null, .infix = null, .precedence = null },
            .Fn => .{ .prefix = parseFunction, .infix = null, .precedence = null },
            .List => .{ .prefix = parseList, .infix = null, .precedence = null },
            .Table => .{ .prefix = parseTable, .infix = null, .precedence = null },
        };
    }

    return table;
}

fn getRule(token_type: scanner.TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

fn getRulePrecedenceValue(parser: *Parser, token_type: scanner.TokenType) usize {
    return @intFromEnum(getRulePrecedence(parser, token_type));
}

fn getRulePrecedence(parser: *Parser, token_type: scanner.TokenType) Precedence {
    if (token_type == .Pipe and parser.ctx.parse_loop_capture) return .Lowest;
    if (getRule(token_type).precedence) |precedence| {
        return precedence;
    }
    return Precedence.Lowest;
}

fn expectStatement(expected: Statement, actual: Statement) anyerror!void {
    try expectTag(expected, actual);

    switch (expected) {
        .VarDeclaration => |stmt| {
            try std.testing.expectEqualStrings(stmt.name, actual.VarDeclaration.name);
            try expectExpression(stmt.expression.*, actual.VarDeclaration.expression.*);
        },
        .Block => |stmt| {
            for (0..stmt.items.len) |i| {
                try expectStatement(stmt.items[i], actual.Block.items[i]);
            }
        },
        .Assignment => |stmt| {
            try expectTag(stmt.target, actual.Assignment.target);
            if (stmt.target == .Identifier) {
                try std.testing.expectEqualStrings(stmt.target.Identifier, actual.Assignment.target.Identifier);
            } else {
                try expectExpression(stmt.target.Index.left.*, actual.Assignment.target.Index.left.*);
                try expectExpression(stmt.target.Index.index.*, actual.Assignment.target.Index.index.*);
            }
            try expectExpression(stmt.expression.*, actual.Assignment.expression.*);
        },
        .For => |stmt| {
            try expectExpression(stmt.expression.*, actual.For.expression.*);
            try std.testing.expectEqualStrings(stmt.capture, actual.For.capture);
            if (stmt.index) |index| {
                try std.testing.expectEqualStrings(index, actual.For.index.?);
            }
            for (0..stmt.body.items.len) |i| {
                try expectStatement(stmt.body.items[i], actual.For.body.items[i]);
            }
        },
        .While => |stmt| {
            try expectExpression(stmt.expression.*, actual.While.expression.*);
            for (0..stmt.body.items.len) |i| {
                try expectStatement(stmt.body.items[i], actual.While.body.items[i]);
            }
        },
        .Return => |stmt| {
            try expectExpression(stmt.*, actual.Return.*);
        },
        .Assert => |stmt| {
            try expectExpression(stmt.*, actual.Assert.*);
        },
        .Print => |stmt| {
            try expectExpression(stmt.*, actual.Print.*);
        },
        .Expression => |stmt| {
            try expectExpression(stmt.*, actual.Expression.*);
        },
    }
}

fn expectTag(expected: anytype, actual: anytype) !void {
    try std.testing.expectEqual(std.meta.activeTag(expected), std.meta.activeTag(actual));
}

fn expectExpression(expected: Expression, actual: Expression) !void {
    try expectTag(expected, actual);

    switch (expected) {
        .Identifier => |ident| try std.testing.expectEqualStrings(ident, actual.Identifier),
        .String => |string| try std.testing.expectEqualStrings(string, actual.String),
        .Prefix => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Prefix.operator);
            try expectExpression(expr.expression.*, actual.Prefix.expression.*);
        },
        .Infix => |expr| {
            try std.testing.expectEqual(expr.operator, actual.Infix.operator);
            try expectExpression(expr.left.*, actual.Infix.left.*);
            try expectExpression(expr.right.*, actual.Infix.right.*);
        },
        .Function => |expr| {
            try std.testing.expectEqual(expr.params.items.len, actual.Function.params.items.len);
            for (0..expr.params.items.len) |i| {
                const first = expr.params.items[i];
                const second = actual.Function.params.items[i];
                try expectTag(first, second);
                if (first == .Positional) {
                    try std.testing.expectEqualStrings(first.Positional, second.Positional);
                } else {
                    try std.testing.expectEqualStrings(first.Default.name, second.Default.name);
                    try expectExpression(first.Default.value.*, second.Default.value.*);
                }
            }
            try expectTag(expr.body, actual.Function.body);
            if (expr.body == .Expression) {
                try expectExpression(expr.body.Expression.*, actual.Function.body.Expression.*);
            } else {
                for (0..expr.body.Block.items.len) |i| {
                    try expectStatement(expr.body.Block.items[i], actual.Function.body.Block.items[i]);
                }
            }
        },
        .Range => |expr| {
            try expectExpression(expr.start.*, actual.Range.start.*);
            try expectExpression(expr.end.*, actual.Range.end.*);
        },
        .Call => |expr| {
            try expectExpression(expr.function.*, actual.Call.function.*);
            try std.testing.expectEqual(expr.args.items.len, actual.Call.args.items.len);
            for (0..expr.args.items.len) |i| {
                const first = expr.args.items[i];
                const second = actual.Call.args.items[i];
                try expectTag(first, second);
                if (first == .Positional) {
                    try expectExpression(first.Positional.*, second.Positional.*);
                } else {
                    try std.testing.expectEqualStrings(first.Named.name, second.Named.name);
                    try expectExpression(first.Named.value.*, second.Named.value.*);
                }
            }
        },
        .List => |expr| {
            try std.testing.expectEqual(expr.items.len, actual.List.items.len);
            for (expr.items, 0..) |item, i| {
                try expectExpression(item.*, actual.List.items[i].*);
            }
        },
        .Table => |expr| {
            try std.testing.expectEqual(expr.items.len, actual.Table.items.len);
            for (expr.items, 0..) |item, i| {
                try expectExpression(item.key.*, actual.Table.items[i].key.*);
                try expectExpression(item.value.*, actual.Table.items[i].value.*);
            }
        },
        .Index => |expr| {
            try expectExpression(expr.left.*, actual.Index.left.*);
            try expectExpression(expr.index.*, actual.Index.index.*);
        },
        .Match => |expr| {
            if (expr.target) |target| {
                try expectExpression(target.*, actual.Match.target.?.*);
            }
            try expectTag(expr.body, actual.Match.body);
            if (expr.body == .Single) {
                try expectExpression(expr.body.Single.pattern.*, actual.Match.body.Single.pattern.*);
                try expectStatement(expr.body.Single.body, actual.Match.body.Single.body);
            } else {
                try std.testing.expectEqual(expr.body.Multiple.items.len, actual.Match.body.Multiple.items.len);
                for (0..expr.body.Multiple.items.len) |i| {
                    const first = expr.body.Multiple.items[i];
                    const second = actual.Match.body.Multiple.items[i];
                    try expectExpression(first.pattern.*, second.pattern.*);
                    try expectStatement(first.body, second.body);
                }
            }
        },
        else => try std.testing.expectEqual(expected, actual),
    }
}

const StatementTestCase = struct {
    description: []const u8,
    input: []const u8,
    expected_statement: Statement,
};

fn runStatementTest(arena: std.mem.Allocator, test_case: StatementTestCase) anyerror!void {
    const ast = try createAst(arena, test_case.input);
    try std.testing.expectEqual(ast.items.len, 1);
    try expectStatement(test_case.expected_statement, ast.items[0]);
}

test "statements" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]StatementTestCase{
        .{
            .description = "var declaration",
            .input =
            \\let foo = 5
            ,
            .expected_statement = Statement{
                .VarDeclaration = .{
                    .name = "foo",
                    .expression = &Expression{ .Integer = 5 },
                },
            },
        },
        .{
            .description = "var declaration empty",
            .input =
            \\let foo
            ,
            .expected_statement = Statement{
                .VarDeclaration = .{
                    .name = "foo",
                    .expression = &Expression{ .Null = {} },
                },
            },
        },
        .{
            .description = "assignment to identifier",
            .input =
            \\foo = 5
            ,
            .expected_statement = Statement{
                .Assignment = .{
                    .target = .{ .Identifier = "foo" },
                    .expression = &Expression{ .Integer = 5 },
                },
            },
        },
        .{
            .description = "assignment to index",
            .input =
            \\foo[0] = 5
            ,
            .expected_statement = Statement{
                .Assignment = .{
                    .target = .{
                        .Index = .{
                            .left = &Expression{ .Identifier = "foo" },
                            .index = &Expression{ .Integer = 0 },
                        },
                    },
                    .expression = &Expression{ .Integer = 5 },
                },
            },
        },
        .{
            .description = "return",
            .input =
            \\return 5
            ,
            .expected_statement = Statement{
                .Return = &Expression{ .Integer = 5 },
            },
        },
        .{
            .description = "return empty",
            .input =
            \\return
            ,
            .expected_statement = Statement{
                .Return = &Expression{ .Null = {} },
            },
        },
        .{
            .description = "print",
            .input =
            \\print(5)
            ,
            .expected_statement = Statement{
                .Print = &Expression{ .Integer = 5 },
            },
        },
        .{
            .description = "assert",
            .input =
            \\assert(5)
            ,
            .expected_statement = Statement{
                .Assert = &Expression{ .Integer = 5 },
            },
        },
        .{
            .description = "expression",
            .input =
            \\5 + 5
            ,
            .expected_statement = Statement{
                .Expression = &Expression{
                    .Infix = .{
                        .left = &Expression{ .Integer = 5 },
                        .right = &Expression{ .Integer = 5 },
                        .operator = .Plus,
                    },
                },
            },
        },
        .{
            .description = "block",
            .input =
            \\{
            \\    let foo = 5
            \\    foo + 2
            \\}
            ,
            .expected_statement = Statement{
                .Block = try test_utils.list(Statement, arena.allocator(), &.{
                    Statement{
                        .VarDeclaration = .{
                            .name = "foo",
                            .expression = &Expression{ .Integer = 5 },
                        },
                    },
                    Statement{
                        .Expression = &Expression{
                            .Infix = .{
                                .left = &Expression{ .Identifier = "foo" },
                                .right = &Expression{ .Integer = 2 },
                                .operator = .Plus,
                            },
                        },
                    },
                }),
            },
        },
        .{
            .description = "block single line",
            .input =
            \\{ print(1) }
            ,
            .expected_statement = Statement{
                .Block = try test_utils.list(Statement, arena.allocator(), &.{
                    Statement{
                        .Print = &Expression{ .Integer = 1 },
                    },
                }),
            },
        },
        .{
            .description = "block empty",
            .input =
            \\{}
            ,
            .expected_statement = Statement{
                .Block = .{},
            },
        },
        .{
            .description = "block empty multiple lines",
            .input =
            \\{
            \\
            \\}
            ,
            .expected_statement = Statement{
                .Block = .{},
            },
        },
        .{
            .description = "while",
            .input =
            \\while(foo < 5) {
            \\    foo = foo + 1
            \\}
            ,
            .expected_statement = Statement{
                .While = .{
                    .expression = &Expression{
                        .Infix = .{
                            .left = &Expression{ .Identifier = "foo" },
                            .right = &Expression{ .Integer = 5 },
                            .operator = .Lt,
                        },
                    },
                    .body = try test_utils.list(Statement, arena.allocator(), &.{
                        Statement{
                            .Assignment = .{
                                .target = .{ .Identifier = "foo" },
                                .expression = &Expression{
                                    .Infix = .{
                                        .left = &Expression{ .Identifier = "foo" },
                                        .right = &Expression{ .Integer = 1 },
                                        .operator = .Plus,
                                    },
                                },
                            },
                        },
                    }),
                },
            },
        },
        .{
            .description = "for",
            .input =
            \\for(0..5) |i| {
            \\    print(i)
            \\}
            ,
            .expected_statement = Statement{
                .For = .{
                    .expression = &Expression{
                        .Range = .{
                            .start = &Expression{ .Integer = 0 },
                            .end = &Expression{ .Integer = 5 },
                        },
                    },
                    .capture = "i",
                    .index = null,
                    .body = try test_utils.list(Statement, arena.allocator(), &.{
                        Statement{
                            .Print = &Expression{ .Identifier = "i" },
                        },
                    }),
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        StatementTestCase,
        "parse statements",
        &test_cases,
        runStatementTest,
    );
}

const ExpressionTestCase = struct {
    description: []const u8,
    input: []const u8,
    expected_expression: ?Expression = null,
    expect_error: bool = false,
};

fn runExpressionTest(arena: std.mem.Allocator, test_case: ExpressionTestCase) anyerror!void {
    const result = createAst(arena, test_case.input);

    if (test_case.expect_error) {
        try std.testing.expectError(error.ParserError, result);
    } else {
        const ast = try result;
        try std.testing.expectEqual(ast.items.len, 1);
        try std.testing.expect(ast.items[0] == .Expression);
        try expectExpression(test_case.expected_expression.?, ast.items[0].Expression.*);
    }
}

test "expressions" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse identifier",
            .input =
            \\foo
            ,
            .expected_expression = Expression{ .Identifier = "foo" },
        },
        .{
            .description = "should parse string literal",
            .input =
            \\"foo"
            ,
            .expected_expression = Expression{ .String = "foo" },
        },
        .{
            .description = "should parse integer literal",
            .input =
            \\666
            ,
            .expected_expression = Expression{ .Integer = 666 },
        },
        .{
            .description = "should parse float literal",
            .input =
            \\3.1415
            ,
            .expected_expression = Expression{ .Float = 3.1415 },
        },
        .{
            .description = "should parse true",
            .input =
            \\true
            ,
            .expected_expression = Expression{ .Boolean = true },
        },
        .{
            .description = "should parse false",
            .input =
            \\false
            ,
            .expected_expression = Expression{ .Boolean = false },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse expressions",
        &test_cases,
        runExpressionTest,
    );
}

test "prefix expressions" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse bang operator with true",
            .input = "!true",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Boolean = true,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse bang operator with false",
            .input = "!false",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Boolean = false,
                    },
                    .operator = .Bang,
                },
            },
        },
        .{
            .description = "should parse tilde operator with false",
            .input = "~false",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Boolean = false,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with integer",
            .input = "+5",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Integer = 5,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with integer",
            .input = "-2",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Integer = 2,
                    },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse tilde operator with integer",
            .input = "~5",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Integer = 5,
                    },
                    .operator = .Tilde,
                },
            },
        },
        .{
            .description = "should parse plus operator with float",
            .input = "+5.41",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Float = 5.41,
                    },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse minus operator with float",
            .input = "-2.1234",
            .expected_expression = Expression{
                .Prefix = Prefix{
                    .expression = &Expression{
                        .Float = 2.1234,
                    },
                    .operator = .Minus,
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse prefix expression",
        &test_cases,
        runExpressionTest,
    );
}

test "infix expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "should parse integer addition",
            .input = "1 + 1",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 1 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse float addition",
            .input = "1.1 + 1.35",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Float = 1.1 },
                    .right = &Expression{ .Float = 1.35 },
                    .operator = .Plus,
                },
            },
        },
        .{
            .description = "should parse integer subtraction",
            .input = "40 - 22",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 40 },
                    .right = &Expression{ .Integer = 22 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse float subtraction",
            .input = "40.54 - 22.33",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Float = 40.54 },
                    .right = &Expression{ .Float = 22.33 },
                    .operator = .Minus,
                },
            },
        },
        .{
            .description = "should parse integer multiplication",
            .input = "5 * 66",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 5 },
                    .right = &Expression{ .Integer = 66 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse integer mod",
            .input = "33 % 2",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 33 },
                    .right = &Expression{ .Integer = 2 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should preserve order of operations without parens",
            .input = "3 * 4 / 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{
                        .Infix = Infix{
                            .left = &Expression{
                                .Integer = 3,
                            },
                            .right = &Expression{
                                .Integer = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in first pos",
            .input = "(3 * 4) / 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{
                        .Infix = Infix{
                            .left = &Expression{
                                .Integer = 3,
                            },
                            .right = &Expression{
                                .Integer = 4,
                            },
                            .operator = .Asterisk,
                        },
                    },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should respect parentheses in second pos",
            .input = "3 * (4 / 3)",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{
                        .Infix = Infix{
                            .left = &Expression{
                                .Integer = 4,
                            },
                            .right = &Expression{
                                .Integer = 3,
                            },
                            .operator = .Slash,
                        },
                    },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float multiplication",
            .input = "5.3 * 66.5",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Float = 5.3 },
                    .right = &Expression{ .Float = 66.5 },
                    .operator = .Asterisk,
                },
            },
        },
        .{
            .description = "should parse float mod",
            .input = "1.1 % 5.3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Float = 1.1 },
                    .right = &Expression{ .Float = 5.3 },
                    .operator = .Percent,
                },
            },
        },
        .{
            .description = "should parse integer division",
            .input = "6 / 2",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 6 },
                    .right = &Expression{ .Integer = 2 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse float division",
            .input = "6.55 / 2.413",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Float = 6.55 },
                    .right = &Expression{ .Float = 2.413 },
                    .operator = .Slash,
                },
            },
        },
        .{
            .description = "should parse integer less than",
            .input = "1 < 5",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .Lt,
                },
            },
        },
        .{
            .description = "should parse integer less than or equal",
            .input = "1 <= 5",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .LtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer greater than",
            .input = "1 > 5",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .Gt,
                },
            },
        },
        .{
            .description = "should parse integer greater than or equal",
            .input = "1 >= 5",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 1 },
                    .right = &Expression{ .Integer = 5 },
                    .operator = .GtOrEq,
                },
            },
        },
        .{
            .description = "should parse integer equals",
            .input = "3 == 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse integer does not equal",
            .input = "3 != 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse integer bitwise or",
            .input = "3 | 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Pipe,
                },
            },
        },
        .{
            .description = "should parse integer bitwise and",
            .input = "3 & 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Ampersand,
                },
            },
        },
        .{
            .description = "should parse integer bitwise xor",
            .input = "3 ^ 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .Caret,
                },
            },
        },
        .{
            .description = "should parse integer shift left",
            .input = "3 << 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .LeftShift,
                },
            },
        },
        .{
            .description = "should parse integer shift right",
            .input = "3 >> 3",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Integer = 3 },
                    .right = &Expression{ .Integer = 3 },
                    .operator = .RightShift,
                },
            },
        },
        .{
            .description = "should parse boolean equals",
            .input = "true == false",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
                    .operator = .Eq,
                },
            },
        },
        .{
            .description = "should parse boolean does not equal",
            .input = "true != false",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
                    .operator = .NotEq,
                },
            },
        },
        .{
            .description = "should parse boolean logical or",
            .input = "true or false",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
                    .operator = .Or,
                },
            },
        },
        .{
            .description = "should parse boolean logical and",
            .input = "true and false",
            .expected_expression = Expression{
                .Infix = Infix{
                    .left = &Expression{ .Boolean = true },
                    .right = &Expression{ .Boolean = false },
                    .operator = .And,
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse infix expression",
        &test_cases,
        runExpressionTest,
    );
}

test "range expression" {
    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "two integers",
            .input =
            \\0..10
            ,
            .expected_expression = Expression{
                .Range = Range{
                    .start = &Expression{
                        .Integer = 0,
                    },
                    .end = &Expression{
                        .Integer = 10,
                    },
                },
            },
        },
        .{
            .description = "two infix expressions",
            .input =
            \\2 + 3..50 - 10
            ,
            .expected_expression = Expression{
                .Range = Range{
                    .start = &Expression{
                        .Infix = Infix{
                            .operator = .Plus,
                            .left = &Expression{
                                .Integer = 2,
                            },
                            .right = &Expression{
                                .Integer = 3,
                            },
                        },
                    },
                    .end = &Expression{
                        .Infix = Infix{
                            .operator = .Minus,
                            .left = &Expression{
                                .Integer = 50,
                            },
                            .right = &Expression{
                                .Integer = 10,
                            },
                        },
                    },
                },
            },
        },
        .{
            .description = "two function calls",
            .input =
            \\start()..end()
            ,
            .expected_expression = Expression{
                .Range = Range{
                    .start = &Expression{
                        .Call = Call{
                            .args = .{},
                            .function = &Expression{
                                .Identifier = "start",
                            },
                        },
                    },
                    .end = &Expression{
                        .Call = Call{
                            .args = .{},
                            .function = &Expression{
                                .Identifier = "end",
                            },
                        },
                    },
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse range expression",
        &test_cases,
        runExpressionTest,
    );
}

test "list literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty list",
            .input =
            \\List{}
            ,
            .expected_expression = Expression{
                .List = .{},
            },
        },
        .{
            .description = "empty list with new line",
            .input =
            \\List{
            \\
            \\}
            ,
            .expected_expression = Expression{
                .List = .{},
            },
        },
        .{
            .description = "list one line",
            .input =
            \\List{1, 2}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list one line trailing comma",
            .input =
            \\List{1, 2,}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list multiple lines",
            .input =
            \\List{ 
            \\    1,
            \\    2
            \\}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list multiple lines trailing comma",
            .input =
            \\List{ 
            \\    1,
            \\    2,
            \\}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list multiple lines whitespace",
            .input =
            \\List{ 
            \\
            \\    1,
            \\
            \\    2
            \\
            \\}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list multiple lines trailing comma whitespace",
            .input =
            \\List{ 
            \\
            \\    1,
            \\
            \\    2,
            \\
            \\}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "list of lists",
            .input =
            \\List{ 
            \\    1,
            \\    2,
            \\}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .Integer = 1 },
                    &Expression{ .Integer = 2 },
                }),
            },
        },
        .{
            .description = "eof",
            .input =
            \\List{ 1, 2,
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma",
            .input =
            \\List{ 1 2 }
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma multiple lines",
            .input =
            \\List{ 
            \\    1 
            \\    2 
            \\}
            ,
            .expect_error = true,
        },
        .{
            .description = "list of lists",
            .input =
            \\List{ 
            \\    List{1, 2},
            \\    List{3, 4},
            \\}
            ,
            .expected_expression = Expression{
                .List = try test_utils.list(*const Expression, arena.allocator(), &.{
                    &Expression{ .List = try test_utils.list(
                        *const Expression,
                        arena.allocator(),
                        &.{
                            &Expression{ .Integer = 1 },
                            &Expression{ .Integer = 2 },
                        },
                    ) },
                    &Expression{ .List = try test_utils.list(
                        *const Expression,
                        arena.allocator(),
                        &.{
                            &Expression{ .Integer = 3 },
                            &Expression{ .Integer = 4 },
                        },
                    ) },
                }),
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse list literal",
        &test_cases,
        runExpressionTest,
    );
}

test "table literal" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty table",
            .input =
            \\Table{}
            ,
            .expected_expression = Expression{
                .Table = .{},
            },
        },
        .{
            .description = "empty table with new line",
            .input =
            \\Table{
            \\
            \\}
            ,
            .expected_expression = Expression{
                .Table = .{},
            },
        },
        .{
            .description = "table one line",
            .input =
            \\Table{"a" = 1, "b" = 2}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(TablePair, arena.allocator(), &.{
                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "table one line trailing comma",
            .input =
            \\Table{"a" = 1, "b" = 2,}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(TablePair, arena.allocator(), &.{
                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "table multiple lines",
            .input =
            \\Table{
            \\    "a" = 1, 
            \\    "b" = 2
            \\}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(TablePair, arena.allocator(), &.{
                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "table multiple lines trailing comma",
            .input =
            \\Table{
            \\    "a" = 1, 
            \\    "b" = 2,
            \\}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(TablePair, arena.allocator(), &.{
                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "table multiple lines whitespace",
            .input =
            \\Table{
            \\
            \\    "a" = 1, 
            \\
            \\    "b" = 2
            \\
            \\}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(TablePair, arena.allocator(), &.{
                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "table multiple lines trailing comma whitespace",
            .input =
            \\Table{
            \\
            \\    "a" = 1, 
            \\
            \\    "b" = 2,
            \\
            \\}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(TablePair, arena.allocator(), &.{
                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                }),
            },
        },
        .{
            .description = "eof",
            .input =
            \\Table{"a" = 1, "b" = 2
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma",
            .input =
            \\Table{"a" = 1 "b" = 2}
            ,
            .expect_error = true,
        },
        .{
            .description = "expect comma multiple lines",
            .input =
            \\Table{
            \\    "a" = 1 
            \\    "b" = 2
            \\}
            ,
            .expect_error = true,
        },
        .{
            .description = "table of tables",
            .input =
            \\Table{
            \\    "a" = Table{"a" = 1, "b" = 2},
            \\    "b" = Table{"a" = 3, "b" = 4},
            \\}
            ,
            .expected_expression = Expression{
                .Table = try test_utils.list(TablePair, arena.allocator(), &.{
                    .{
                        .key = &Expression{ .String = "a" },
                        .value = &Expression{
                            .Table = try test_utils.list(
                                TablePair,
                                arena.allocator(),
                                &.{
                                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 1 } },
                                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 2 } },
                                },
                            ),
                        },
                    },
                    .{
                        .key = &Expression{ .String = "b" },
                        .value = &Expression{
                            .Table = try test_utils.list(
                                TablePair,
                                arena.allocator(),
                                &.{
                                    TablePair{ .key = &Expression{ .String = "a" }, .value = &Expression{ .Integer = 3 } },
                                    TablePair{ .key = &Expression{ .String = "b" }, .value = &Expression{ .Integer = 4 } },
                                },
                            ),
                        },
                    },
                }),
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse table literal",
        &test_cases,
        runExpressionTest,
    );
}

test "function call" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "no args",
            .input =
            \\test()
            ,
            .expected_expression = Expression{
                .Call = Call{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = .{},
                },
            },
        },
        .{
            .description = "with args one line",
            .input =
            \\test(1, 2)
            ,
            .expected_expression = Expression{
                .Call = Call{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
                    }),
                },
            },
        },
        .{
            .description = "with args one line trailing comma",
            .input =
            \\test(1, 2,)
            ,
            .expected_expression = Expression{
                .Call = Call{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
                    }),
                },
            },
        },
        .{
            .description = "with args multiple lines",
            .input =
            \\test(
            \\    1, 
            \\    2
            \\)
            ,
            .expected_expression = Expression{
                .Call = Call{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
                    }),
                },
            },
        },
        .{
            .description = "with args multiple lines trailing comma",
            .input =
            \\test(
            \\    1, 
            \\    2,
            \\)
            ,
            .expected_expression = Expression{
                .Call = Call{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Positional = &Expression{ .Integer = 2 } },
                    }),
                },
            },
        },
        .{
            .description = "with named args",
            .input =
            \\test(
            \\    1, 
            \\    c=2,
            \\    b=3,
            \\)
            ,
            .expected_expression = Expression{
                .Call = Call{
                    .function = &Expression{
                        .Identifier = "test",
                    },
                    .args = try test_utils.list(FunctionArg, arena.allocator(), &.{
                        .{ .Positional = &Expression{ .Integer = 1 } },
                        .{ .Named = .{ .name = "c", .value = &Expression{ .Integer = 2 } } },
                        .{ .Named = .{ .name = "b", .value = &Expression{ .Integer = 3 } } },
                    }),
                },
            },
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

    const test_cases = [_]ExpressionTestCase{
        .{
            .description = "empty",
            .input =
            \\fn() {
            \\
            \\}
            ,
            .expected_expression = Expression{
                .Function = .{ .params = .{}, .body = .{ .Block = .{} } },
            },
        },
        .{
            .description = "with block",
            .input =
            \\fn(a, b) {
            \\    return a + b
            \\}
            ,
            .expected_expression = Expression{
                .Function = .{
                    .params = try test_utils.list(FunctionParam, arena.allocator(), &.{
                        .{ .Positional = "a" },
                        .{ .Positional = "b" },
                    }),
                    .body = .{
                        .Block = try test_utils.list(Statement, arena.allocator(), &.{
                            .{
                                .Return = &Expression{
                                    .Infix = .{
                                        .left = &Expression{ .Identifier = "a" },
                                        .operator = .Plus,
                                        .right = &Expression{ .Identifier = "b" },
                                    },
                                },
                            },
                        }),
                    },
                },
            },
        },
        .{
            .description = "with expression",
            .input =
            \\fn(a, b) a + b
            ,
            .expected_expression = Expression{
                .Function = .{
                    .params = try test_utils.list(FunctionParam, arena.allocator(), &.{
                        .{ .Positional = "a" },
                        .{ .Positional = "b" },
                    }),
                    .body = .{
                        .Expression = &Expression{
                            .Infix = .{
                                .left = &Expression{ .Identifier = "a" },
                                .operator = .Plus,
                                .right = &Expression{ .Identifier = "b" },
                            },
                        },
                    },
                },
            },
        },
        .{
            .description = "with default args",
            .input =
            \\fn(a = 11, b = 12) a + b
            ,
            .expected_expression = Expression{
                .Function = .{
                    .params = try test_utils.list(FunctionParam, arena.allocator(), &.{
                        .{ .Default = .{ .name = "a", .value = &Expression{ .Integer = 11 } } },
                        .{ .Default = .{ .name = "b", .value = &Expression{ .Integer = 12 } } },
                    }),
                    .body = .{
                        .Expression = &Expression{
                            .Infix = .{
                                .left = &Expression{ .Identifier = "a" },
                                .operator = .Plus,
                                .right = &Expression{ .Identifier = "b" },
                            },
                        },
                    },
                },
            },
        },
    };

    try test_utils.runTestsWithArena(
        ExpressionTestCase,
        "parse function literal",
        &test_cases,
        runExpressionTest,
    );
}
