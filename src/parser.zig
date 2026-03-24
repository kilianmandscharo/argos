const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");

fn createAst(arena: std.mem.Allocator, source: []const u8) !Program {
    const parser = Parser.init(arena, source);
    return try parser.parseProgram();
}

pub const Parser = struct {
    current: scanner.Token,
    previous: ?scanner.Token,
    scanner: *scanner.Scanner,
    arena: std.mem.Allocator,
    debug_indent: usize = 0,

    pub fn init(arena: std.mem.Allocator, s: *scanner.Scanner) !Parser {
        const current = s.next();
        if (current.type == .Eof) {
            return error.NoTokens;
        }
        return Parser{
            .scanner = scanner,
            .current = current,
            .previous = null,
            .arena = arena,
        };
    }

    pub fn parseProgram(self: *Parser) !Program {
        var statements: std.ArrayList(Statement) = .{};
        while (self.cur_token.type != .Eof) {
            try self.chopNewlines();
            const statement = try self.parseStatement();
            try statements.append(self.arena, statement);
        }
        return statements;
    }

    fn parseStatement(self: *Parser) !Statement {
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
        } else {
            return try self.expressionStatement();
        }
    }

    fn printStatement(self: *Parser) !Statement {
        self.log("print statement", .{});
        defer self.log("end print statement", .{});

        try self.consume(.LParen, "Expect '(' after print.");
        const expression = try self.parseExpression();
        try self.expectLineEnd();

        return Statement{ .Print = expression };
    }

    fn assertStatement(self: *Parser) !Statement {
        self.log("assert statement", .{});
        defer self.log("end assert statement", .{});

        try self.consume(.LParen, "Expect '(' after print.");
        const expression = try self.parseExpression();
        try self.expectLineEnd();

        return Statement{ .Assert = expression };
    }

    fn whileStatement(self: *Parser) !Statement {
        self.log("while statement", .{});
        defer self.log("end while statement", .{});

        try self.consume(.LParen, "Expect '(' after 'while'");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "Expect ')' after condition");
        const body = try self.blockStatement();
        try self.consume(.NewLine, "Expect new line after while block");

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
        const capture = try self.parseExpression();
        if (capture != .Identifier) {
            return self.errorAtPrevious("Expect identifier in loop capture");
        }
        var index: ?[]const u8 = null;
        if (!self.check(.Pipe)) {
            const second_capture = try self.parseExpression();
            if (second_capture != .Identifier) {
                return self.errorAtPrevious("Expect identifier in loop capture");
            }
            index = second_capture.Identifier;
        }
        try self.consume(.Pipe, "Expect '|' after for loop capture");

        try self.consume(.RBrace, "Expect '{' after loop capture.");
        const body = try self.blockStatement();
        try self.consume(.NewLine, "Expect new line after for block");

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
            return Statement{ .Return = .Null };
        } else {
            const expression = try self.parseExpression();
            try self.expectLineEnd();
            return Statement{ .Return = expression };
        }
    }

    fn blockStatement(self: *Parser) !Statement {
        self.log("block statement", .{});
        defer self.log("end block statement", .{});

        var statements: std.ArrayList(Statement) = .{};
        while (!self.check(.RBrace) and !self.check(.Eof)) {
            try statements.append(self.arena, try self.parseStatement());
            try self.expectLineEnd();
        }
        try self.consume(.RBrace, "Expect '}' after block.");
        try self.consume(.NewLine, "Expect '\n' after block.");

        return Statement{ .Block = statements };
    }

    fn expressionStatement(self: *Parser) !Statement {
        const expression = try self.parseExpression();
        if (self.match(.Assign)) {
            const target = switch (expression) {
                .Identifier => |name| AssignTarget{ .Identifier = name },
                .Index => |index| AssignTarget{ .Index = index },
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

        if (getRule(self.previous.?.type).prefix) |prefixFn| {
            var left = try prefixFn(self);
            const left_owned = try self.arena.create(Expression);

            defer {
                left_owned.* = left;
                self.debug_indent -= 1;
                self.printDebug("parsed {s}\n", .{left_owned.getType()}, .Blue);
            }

            while (@intFromEnum(precedence) < getRulePrecedenceValue(self.current.type)) {
                try self.advance();
                if (self.current.type == .Eof) return;
                if (getRule(self.previous.?.type).infix) |infixFn| {
                    left = try infixFn(self, left);
                }
            }
        } else {
            return self.errorAtPrevious("Expect expression.");
        }

        self.log("end expression", .{});
    }

    fn advance(self: *Parser) void {
        self.previous = self.current;
        self.current = self.scanner.next();
    }

    fn chopNewlines(self: *Parser) !void {
        while (self.check(.NewLine)) {
            try self.advance();
        }
    }

    fn printDebug(self: *Parser, comptime fmt: []const u8, args: anytype, color: logging.LogColor) void {
        logging.log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = .Debug,
            .color = color,
            .indent = self.debug_indent,
            .module = "Parser",
        });
    }

    fn consume(self: *Parser, expected: scanner.TokenType, message: []const u8) !void {
        if (self.check(expected)) {
            try self.parser.advance();
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
        return errorAt(&self.previous, message);
    }

    fn errorAt(token: *scanner.Token, message: []const u8) anyerror {
        std.debug.print("[line {d}] Error", .{token.line});
        switch (token.type) {
            .Eof => std.debug.print(" at end", .{}),
            .Error => {},
            else => std.debug.print(" at '{s}'", .{token.source[token.start .. token.start + token.length]}),
        }
        std.debug.print(": {s}\n", .{message});
        return error.CompileError;
    }
};

const Program = std.ArrayList(Statement);

const Statement = union(enum) {
    VarDeclaration: VarDeclaration,
    FuncDeclaration: FuncDeclaration,
    Block: Block,
    Assignment: Assignment,
    For: For,
    While: While,
    Return: *const Expression,
    Assert: *const Expression,
    Print: *const Expression,
    Expression: *const Expression,
};

const Block = std.ArrayList(Statement);

const VarDeclaration = struct {
    name: []const u8,
    expression: ?*const Expression,
};

const FuncDeclaration = struct {
    name: []const u8,
    expression: *const Function,
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

const Expression = union(enum) {
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
    Default: struct { name: []const u8, value: *const Expression },
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
    Assign,
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
    return Expression{ .String = parser.previous.?.toString() };
}

fn parseInteger(parser: *Parser) !Expression {
    return Expression{ .Integer = try std.fmt.parseInt(i64, parser.previous.?.toString(), 10) };
}

fn parseFloat(parser: *Parser) !Expression {
    return Expression{ .Float = try std.fmt.parseFloat(f64, parser.previous.?.toString()) };
}

fn parseLiteral(parser: *Parser) !Expression {
    return switch (parser.previous.type) {
        .True => try Expression{ .Boolean = true },
        .False => try Expression{ .Boolean = false },
        .Null => try Expression.Null,
        else => unreachable,
    };
}

fn parseIdentifier(parser: *Parser) !Expression {
    return Expression{ .Identifier = parser.previous.?.toString() };
}

fn parseUnary(parser: *Parser) !Expression {
    const operator = parser.previous.?.type;
    try parser.advance();
    const expression = try parser.parseExpression();
    return Expression{ .Prefix = .{ .operator = operator, .expression = expression } };
}

fn parseList(parser: *Parser) !Expression {
    _ = parser;
}

fn parseTable(parser: *Parser) !Expression {
    _ = parser;
}

fn parseFunction(parser: *Parser) !Expression {
    _ = parser;
}

fn parseGrouping(parser: *Parser) !Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RParen, "Expect ')' after expression");
    return expression;
}

fn parseMatch(self: *Parser) !Expression {
    var target: ?*const Expression = null;

    if (self.match(.LParen)) {
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
    try parser.advance();
    const right = try parser.parsePrecedence(getRulePrecedence(operator));

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
    try parser.advance();
    const right = try parser.parseExpression();
    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;
    return Expression{ .Range = .{ .left = left_owned, .right = right } };
}

fn parseCall(parser: *Parser, left: Expression) !Expression {
    var args: std.ArrayList(FunctionArg) = .{};
    while (true) {
        try parser.chopNewlines();
        if (try parser.match(.Eof)) return parser.errorAtCurrent("Reached EOF.");
        if (try parser.match(.RParen)) break;

        const expression = try parser.parseExpression();

        if (try parser.match(.Assign)) {
            if (expression != .Identifier) {
                return parser.errorAtPrevious("Invalid left side in named argument.");
            }
            const right = try parser.parseExpression();
            try args.append(parser.arena, FunctionArg{
                .Named = FunctionArgNamed{ .name = expression.Identifier, .value = right },
            });
        } else {
            try args.append(parser.arena, FunctionArg{ .Positional = expression });
        }

        try parser.advance();
        if (try parser.match(.RParen)) break;
        const token_type = parser.current.type;
        try parser.advance();
        try parser.chopNewlines();
        if (!parser.check(.RParen) and token_type != .Comma) {
            return parser.errorAtPrevious("Expected comma.");
        }
        if (try parser.match(.RParen)) break;
    }

    const left_owned = try parser.arena.create(Expression);
    left_owned.* = left;

    return Expression{ .Call = .{
        .function = left_owned,
        .args = args,
    } };
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
            .Assign => .{ .prefix = null, .infix = null, .precedence = .Assign },
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
            .Plus => .{ .prefix = null, .infix = parseBinary, .precedence = .Sum },
            .Minus => .{ .prefix = parseUnary, .infix = parseBinary, .precedence = .Sum },
            .Slash => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Asterisk => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Percent => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .Return => .{ .prefix = null, .infix = null, .precedence = null },
            .Else => .{ .prefix = null, .infix = null, .precedence = null },
            .For => .{ .prefix = null, .infix = null, .precedence = null },
            .Dot => .{ .prefix = null, .infix = null, .precedence = .Index },
            .DotDot => .{ .prefix = null, .infix = parseDotDot, .precedence = null },
            .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
            .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
            .Eof => .{ .prefix = null, .infix = null, .precedence = null },
            .And => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalAnd },
            .Or => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalOr },
            .Pipe => .{ .prefix = null, .infix = null, .precedence = .BitwiseOr },
            .Ampersand => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseAnd },
            .Caret => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseXor },
            .Tilde => .{ .prefix = null, .infix = parseBinary, .precedence = .Prefix },
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
            .Error => .{ .prefix = null, .infix = null, .precedence = null },
        };
    }

    return table;
}

fn getRule(token_type: scanner.TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

fn getRulePrecedenceValue(token_type: scanner.TokenType) usize {
    return @intFromEnum(getRulePrecedence(token_type));
}

fn getRulePrecedence(token_type: scanner.TokenType) Precedence {
    if (getRule(token_type).precedence) |precedence| {
        return precedence;
    }
    return Precedence.Lowest;
}
