const std = @import("std");
const logging = @import("logging.zig");
const scanner = @import("scanner.zig");
const test_utils = @import("test_utils.zig");
const ast = @import("ast.zig");
const constants = @import("constants.zig");
const vm = @import("vm.zig");

pub fn createAst(arena: std.mem.Allocator, script_context: *vm.ScriptContext) !ast.Program {
    var s = try scanner.Scanner.init(arena, script_context);
    var p = try Parser.init(arena, &s, script_context);
    return try p.parseProgram();
}

pub const Parser = struct {
    current: scanner.Token,
    previous: ?scanner.Token,
    scanner: *scanner.Scanner,
    arena: std.mem.Allocator,
    debug_indent: usize = 0,
    script_context: *vm.ScriptContext,
    ctx: struct {
        parse_loop_capture: bool,
        current_var_name: ?[]const u8,
    },

    pub fn init(arena: std.mem.Allocator, s: *scanner.Scanner, script_context: *vm.ScriptContext) !Parser {
        const current = try s.next();
        if (current.type == .Eof) {
            return error.NoTokens;
        }
        return Parser{
            .script_context = script_context,
            .scanner = s,
            .current = current,
            .previous = null,
            .arena = arena,
            .ctx = .{
                .parse_loop_capture = false,
                .current_var_name = null,
            },
        };
    }

    fn getPrevious(self: *Parser) scanner.Token {
        return self.previous.?;
    }

    fn log(self: *Parser, comptime fmt: []const u8, args: anytype) void {
        logging.log(fmt, args, .{
            .messageLevel = .Debug,
            .currentLevel = .Debug,
            .indent = self.debug_indent,
            .module = "Parser",
        });
    }

    pub fn parseProgram(self: *Parser) !ast.Program {
        var statements: std.ArrayList(ast.Statement) = .empty;
        while (self.current.type != .Eof) {
            try self.chopNewlines();
            if (self.current.type == .Eof) break;
            const statement = try self.parseStatement();
            try statements.append(self.arena, statement);
        }
        return statements;
    }

    fn parseStatement(self: *Parser) anyerror!ast.Statement {
        try self.chopNewlines();
        if (try self.match(.While)) {
            return try self.whileStatement();
        }
        if (try self.match(.For)) {
            return try self.forStatement();
        }
        if (try self.match(.Return)) {
            return try self.returnStatement();
        }
        if (try self.match(.Let)) {
            return try self.varDeclaration();
        }
        return try self.expressionStatement();
    }

    fn whileStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) self.log("while statement", .{});
        defer if (comptime constants.debug_parser) self.log("while statement", .{});

        try self.consume(.LParen, "expect '(' after 'while'");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "expect ')' after condition");

        const body_owned = try self.arena.create(ast.Statement);
        body_owned.* = try self.parseStatement();

        return .{
            .While = .{ .expression = expression, .body = body_owned },
        };
    }

    fn forStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) self.log("for statement", .{});
        defer if (comptime constants.debug_parser) self.log("for statement", .{});

        try self.consume(.LParen, "expect '(' after 'for'");
        const expression = try self.parseExpression();
        try self.consume(.RParen, "expect ')' after range");

        try self.consume(.Pipe, "expect '|' after loop range");
        self.ctx.parse_loop_capture = true;

        const capture = try self.parseExpression();
        if (capture.data != .Identifier) {
            return self.errorAtPrevious("expect identifier in loop capture");
        }

        var index: ?scanner.Token = null;

        if (!self.check(.Pipe)) {
            if (!try self.match(.Comma)) {
                return self.errorAtCurrent("expected comma");
            }

            const second_capture = try self.parseExpression();
            if (second_capture.data != .Identifier) {
                return self.errorAtPrevious("expect identifier in loop capture");
            }
            index = second_capture.token;
        }

        try self.consume(.Pipe, "expect '|' after for loop capture");
        self.ctx.parse_loop_capture = false;

        const body_owned = try self.arena.create(ast.Statement);
        body_owned.* = try self.parseStatement();

        return .{
            .For = .{
                .expression = expression,
                .capture = capture.token,
                .index = index,
                .body = body_owned,
            },
        };
    }

    fn returnStatement(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) self.log("return statement", .{});
        defer if (comptime constants.debug_parser) self.log("return statement", .{});

        if (self.isLineEnd()) {
            const expression = try self.arena.create(ast.Expression);
            expression.data = .{ .Null = {} };
            expression.token = self.getPrevious();
            return .{ .Return = expression };
        } else {
            const expression = try self.parseExpression();
            return .{ .Return = expression };
        }
    }

    fn varDeclaration(self: *Parser) !ast.Statement {
        if (comptime constants.debug_parser) self.log("var declaration", .{});
        defer if (comptime constants.debug_parser) self.log("end var declaration", .{});

        const target = try self.parseExpression();
        if (target.data != .Identifier) {
            return self.errorAtPrevious("expect identifier after 'let'");
        }

        self.ctx.current_var_name = target.data.Identifier;
        defer self.ctx.current_var_name = null;

        if (try self.match(.Assign)) {
            const value = try self.parseExpression();
            return .{
                .VarDeclaration = .{
                    .name = target.token,
                    .expression = value,
                },
            };
        }

        const nullExpression = try self.arena.create(ast.Expression);
        nullExpression.data = .Null;
        return .{
            .VarDeclaration = .{
                .name = target.token,
                .expression = nullExpression,
            },
        };
    }

    fn expressionStatement(self: *Parser) !ast.Statement {
        const expression = try self.parseExpression();
        if (try self.match(.Assign)) return try self.parseAssignment(expression, null);
        if (try self.match(.PlusAssign)) return try self.parseAssignment(expression, .Plus);
        if (try self.match(.MinusAssign)) return try self.parseAssignment(expression, .Minus);
        if (try self.match(.AsteriskAssign)) return try self.parseAssignment(expression, .Asterisk);
        if (try self.match(.SlashAssign)) return try self.parseAssignment(expression, .Slash);
        if (try self.match(.PercentAssign)) return try self.parseAssignment(expression, .Percent);
        if (try self.match(.AmpersandAssign)) return try self.parseAssignment(expression, .Ampersand);
        if (try self.match(.PipeAssign)) return try self.parseAssignment(expression, .Pipe);
        if (try self.match(.CaretAssign)) return try self.parseAssignment(expression, .Caret);
        if (try self.match(.LeftShiftAssign)) return try self.parseAssignment(expression, .LeftShift);
        if (try self.match(.RightShiftAssign)) return try self.parseAssignment(expression, .RightShift);
        return .{ .Expression = expression };
    }

    fn parseAssignment(self: *Parser, expression: *const ast.Expression, op: ?scanner.TokenType) !ast.Statement {
        const target: ast.AssignTarget = switch (expression.data) {
            .Identifier => .{ .Identifier = expression.token },
            .Index => |index| .{ .Index = index },
            .Field => |field| .{ .Field = field },
            else => return self.errorAtPrevious("Invalid assign target."),
        };

        var value: *const ast.Expression = undefined;

        if (op) |operator| {
            const right = try self.parseExpression();

            const owned = try self.arena.create(ast.Expression);
            owned.data = .{
                .Infix = .{
                    .left = expression,
                    .right = right,
                    .operator = operator,
                },
            };
            owned.token = expression.token;

            value = owned;
        } else {
            value = try self.parseExpression();
        }

        return .{ .Assignment = .{ .target = target, .expression = value } };
    }

    fn parseExpression(self: *Parser) !*const ast.Expression {
        return self.parsePrecedence(.Lowest);
    }

    fn parsePrecedence(self: *Parser, precedence: Precedence) !*const ast.Expression {
        try self.advance();

        if (comptime constants.debug_parser) {
            self.log("expression on {s}", .{self.getPrevious().toString()});
            self.debug_indent += 1;
        }

        if (getRule(self.getPrevious().type).prefix) |prefixFn| {
            var left = try prefixFn(self);
            const left_owned = try self.arena.create(ast.Expression);

            defer {
                left_owned.* = left;
                if (comptime constants.debug_parser) {
                    self.debug_indent -= 1;
                    self.log("parsed {s}", .{@tagName(left_owned.data)});
                }
            }

            while (@intFromEnum(precedence) < getRulePrecedenceValue(self, self.current.type)) {
                try self.advance();
                if (self.current.type == .Eof) break;
                const t = self.getPrevious().type;
                if (getRule(t).infix) |infixFn| {
                    if (comptime constants.debug_parser) {
                        self.log("infix on {s}", .{@tagName(t)});
                    }
                    left = try infixFn(self, left);
                }
            }

            return left_owned;
        } else {
            return self.errorAtPrevious("expect expression");
        }
    }

    fn parseCommaSeparated(self: *Parser, T: type, parseFn: *const fn (p: *Parser) anyerror!T, delimiter: scanner.TokenType) !std.ArrayList(T) {
        var items: std.ArrayList(T) = .empty;
        var expectComma = false;

        while (!try self.match(delimiter)) {
            try self.chopNewlines();

            if (try self.match(.Eof)) return self.errorAtCurrent("Reached EOF.");
            if (try self.match(delimiter)) break;

            if (expectComma) return self.errorAtPrevious("expected comma");

            try items.append(self.arena, try parseFn(self));
            if (!try self.match(.Comma)) expectComma = true;
        }

        return items;
    }

    fn parseNewlineSeparated(self: *Parser, T: type, parseFn: *const fn (p: *Parser) anyerror!T, delimiter: scanner.TokenType) !std.ArrayList(T) {
        var items: std.ArrayList(T) = .empty;
        var expectNewline = false;

        while (!try self.match(delimiter)) {
            try self.chopNewlines();

            if (try self.match(.Eof)) return self.errorAtCurrent("Reached EOF.");
            if (try self.match(delimiter)) break;

            if (expectNewline) return self.errorAtPrevious("expected newline");

            try items.append(self.arena, try parseFn(self));
            if (!try self.match(.NewLine)) expectNewline = true;
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

    fn matchLineEnd(self: *Parser) !void {
        if (self.isLineEnd()) {
            try self.advance();
        }
    }

    fn errorAtCurrent(self: *Parser, message: []const u8) anyerror {
        return self.errorAt(self.current, message);
    }

    fn errorAtPrevious(self: *Parser, message: []const u8) anyerror {
        return self.errorAt(self.getPrevious(), message);
    }

    fn errorAt(self: *Parser, token: scanner.Token, message: []const u8) anyerror {
        token.printError(message, self.script_context, "Parser");
        return error.ParserError;
    }
};

const Precedence = enum(u8) {
    Lowest = 1,
    Range,
    LogicalOr,
    LogicalAnd,
    Equals,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    LessGreater,
    Shift,
    Sum,
    Product,
    Prefix,
    Instance,
    Call,
    Index,
};

fn parseString(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const s = token.toString();
    return .init(
        .{ .String = s[1 .. s.len - 1] },
        token,
    );
}

fn parseInteger(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    return .init(
        .{ .Integer = try std.fmt.parseInt(i64, token.toString(), 10) },
        token,
    );
}

fn parseFloat(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    return .init(
        .{ .Float = try std.fmt.parseFloat(f64, token.toString()) },
        token,
    );
}

fn parseLiteral(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const data: ast.ExpressionData = switch (token.type) {
        .True => .{ .Boolean = true },
        .False => .{ .Boolean = false },
        .Null => .{ .Null = {} },
        else => unreachable,
    };
    return .init(data, token);
}

fn parseIdentifier(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    return .init(
        .{ .Identifier = token.toString() },
        token,
    );
}

fn parseUnary(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    const operator = token.type;
    const expression = try parser.parseExpression();
    return .init(
        .{ .Prefix = .{ .operator = operator, .expression = expression } },
        token,
    );
}

fn parseList(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    try parser.consume(.LBrace, "expect '{' after 'List'");
    const items = try parser.parseCommaSeparated(
        *const ast.Expression,
        struct {
            fn parse(p: *Parser) !*const ast.Expression {
                return try p.parseExpression();
            }
        }.parse,
        .RBrace,
    );
    return .init(.{ .List = items }, token);
}

fn parseTable(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    try parser.consume(.LBrace, "expect '{' after 'Table'");
    const items = try parser.parseCommaSeparated(
        ast.TablePair,
        struct {
            fn parse(p: *Parser) !ast.TablePair {
                const key = try p.parseExpression();
                try p.consume(.Assign, "expect '=' after table key");
                const value = try p.parseExpression();
                return .{ .key = key, .value = value };
            }
        }.parse,
        .RBrace,
    );
    return .init(.{ .Table = items }, token);
}

fn parseStruct(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();

    try parser.consume(.LBrace, "expect '{' after 'struct'");

    const declarations = try parser.parseNewlineSeparated(
        ast.VarDeclaration,
        struct {
            fn parse(p: *Parser) !ast.VarDeclaration {
                const statement = try p.parseStatement();
                if (statement != .VarDeclaration) {
                    return p.errorAt(p.getPrevious(), "expect var declaration in struct body");
                }
                return statement.VarDeclaration;
            }
        }.parse,
        .RBrace,
    );

    _ = try parser.match(.NewLine);

    return .init(.{
        .Struct = .{
            .name = parser.ctx.current_var_name,
            .fields = declarations,
        },
    }, token);
}

fn parseFunction(parser: *Parser) !ast.Expression {
    const token = parser.getPrevious();
    try parser.consume(.LParen, "expect '(' after 'fn'");
    const params = try parser.parseCommaSeparated(
        ast.FunctionParam,
        struct {
            fn parse(p: *Parser) !ast.FunctionParam {
                const expression = try p.parseExpression();
                if (try p.match(.Assign)) {
                    if (expression.data != .Identifier) {
                        return p.errorAtPrevious("Invalid left side in default param.");
                    }
                    const right = try p.parseExpression();
                    return .{
                        .Default = .{ .name = expression.token, .value = right },
                    };
                } else {
                    if (expression.data != .Identifier) {
                        return p.errorAtPrevious("Invalid function param.");
                    }
                    return .{ .Positional = expression.token };
                }
            }
        }.parse,
        .RParen,
    );

    const name = parser.ctx.current_var_name;

    const body = try parser.parseExpression();

    return .init(
        .{
            .Function = .{
                .params = params,
                .body = body,
                .name = name,
            },
        },
        token,
    );
}

fn parseGrouping(parser: *Parser) !ast.Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RParen, "expect ')' after expression");
    return expression.*;
}

fn parseMatch(self: *Parser) !ast.Expression {
    const token = self.getPrevious();
    var target: ?*const ast.Expression = null;

    if (try self.match(.LParen)) {
        target = try self.parseExpression();
        try self.consume(.RParen, "expect ')' after match target");
    }

    if (!self.check(.LBrace)) {
        const pattern = try self.parseExpression();
        try self.consume(.Arrow, "expect '->' after match pattern");
        const body = try self.parseExpression();
        return .init(
            .{
                .Match = .{
                    .target = target,
                    .body = .{
                        .Single = .{ .pattern = pattern, .body = body },
                    },
                },
            },
            token,
        );
    }

    try self.advance();
    try self.consume(.NewLine, "expect new line after '{' in match block");

    var arms: std.ArrayList(ast.MatchArm) = .empty;

    while (!self.check(.Eof) and !self.check(.RBrace)) {
        try self.chopNewlines();
        if (self.check(.RBrace)) break;

        const pattern = try self.parseExpression();
        try self.consume(.Arrow, "expect '->' after match pattern");
        const body = try self.parseExpression();

        try arms.append(self.arena, .{ .pattern = pattern, .body = body });
    }

    try self.consume(.RBrace, "expect '}' at the end of match block");

    return .init(
        .{
            .Match = .{ .target = target, .body = .{ .Multiple = arms } },
        },
        token,
    );
}

fn parseBlock(self: *Parser) !ast.Expression {
    const statements = try self.parseNewlineSeparated(
        ast.Statement,
        struct {
            fn parse(p: *Parser) !ast.Statement {
                return try p.parseStatement();
            }
        }.parse,
        .RBrace,
    );

    // TODO: pass token
    return .init(.{ .Block = statements }, scanner.Token.dummy());
}

fn parseBinary(parser: *Parser, left: ast.Expression) !ast.Expression {
    const operator = parser.getPrevious().type;
    const right = try parser.parsePrecedence(getRulePrecedence(parser, operator));

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .init(
        .{
            .Infix = .{
                .operator = operator,
                .left = left_owned,
                .right = right,
            },
        },
        left.token,
    );
}

fn parseDotDot(parser: *Parser, left: ast.Expression) !ast.Expression {
    const right = try parser.parseExpression();
    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;
    return .init(
        .{ .Range = .{ .start = left_owned, .end = right } },
        left.token,
    );
}

fn parseInstance(parser: *Parser, left: ast.Expression) !ast.Expression {
    const fields = try parser.parseCommaSeparated(
        ast.InstanceField,
        struct {
            fn parse(p: *Parser) !ast.InstanceField {
                const key = try p.parseExpression();
                if (key.data != .Identifier) {
                    return p.errorAt(p.getPrevious(), "expect identifier in instance field");
                }
                try p.consume(.Assign, "expect '=' after instance key");
                const value = try p.parseExpression();
                return .{ .key = key.token, .value = value };
            }
        }.parse,
        .RBrace,
    );

    _ = try parser.match(.NewLine);

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .init(.{
        .Instance = .{
            .strukt = left_owned,
            .fields = fields,
        },
    }, left.token);
}

fn parseCall(parser: *Parser, left: ast.Expression) !ast.Expression {
    const args = try parser.parseCommaSeparated(
        ast.FunctionArg,
        struct {
            fn parse(p: *Parser) !ast.FunctionArg {
                const expression = try p.parseExpression();
                if (try p.match(.Assign)) {
                    if (expression.data != .Identifier) {
                        return p.errorAtPrevious("Invalid left side in named argument.");
                    }
                    const right = try p.parseExpression();
                    return .{
                        .Named = .{ .name = expression.token, .value = right },
                    };
                } else {
                    return .{ .Positional = expression };
                }
            }
        }.parse,
        .RParen,
    );

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .init(
        .{
            .Call = .{
                .function = left_owned,
                .args = args,
            },
        },
        left.token,
    );
}

fn parseIndex(parser: *Parser, left: ast.Expression) !ast.Expression {
    const expression = try parser.parseExpression();
    try parser.consume(.RBracket, "expect ']' after index expression");

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .init(
        .{
            .Index = .{
                .left = left_owned,
                .index = expression,
            },
        },
        left.token,
    );
}

fn parseDot(parser: *Parser, left: ast.Expression) !ast.Expression {
    const expression = try parser.parsePrecedence(.Index);

    if (expression.data != .Identifier) {
        return parser.errorAt(expression.token, "expect identifier in field access");
    }

    const left_owned = try parser.arena.create(ast.Expression);
    left_owned.* = left;

    return .init(
        .{
            .Field = .{
                .left = left_owned,
                .field = expression.token,
            },
        },
        left.token,
    );
}

const ParseRule = struct {
    prefix: ?*const fn (parser: *Parser) anyerror!ast.Expression,
    infix: ?*const fn (parser: *Parser, left: ast.Expression) anyerror!ast.Expression,
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
            .LBrace => .{ .prefix = parseBlock, .infix = parseInstance, .precedence = .Instance },
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
            .PlusAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Minus => .{ .prefix = parseUnary, .infix = parseBinary, .precedence = .Sum },
            .MinusAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Slash => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .SlashAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Asterisk => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .AsteriskAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Percent => .{ .prefix = null, .infix = parseBinary, .precedence = .Product },
            .PercentAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Return => .{ .prefix = null, .infix = null, .precedence = null },
            .For => .{ .prefix = null, .infix = null, .precedence = null },
            .Dot => .{ .prefix = null, .infix = parseDot, .precedence = .Index },
            .DotDot => .{ .prefix = null, .infix = parseDotDot, .precedence = .Range },
            .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
            .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
            .Eof => .{ .prefix = null, .infix = null, .precedence = null },
            .And => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalAnd },
            .Or => .{ .prefix = null, .infix = parseBinary, .precedence = .LogicalOr },
            .Pipe => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseOr },
            .PipeAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Ampersand => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseAnd },
            .AmpersandAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Caret => .{ .prefix = null, .infix = parseBinary, .precedence = .BitwiseXor },
            .CaretAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Tilde => .{ .prefix = parseUnary, .infix = null, .precedence = .Prefix },
            .LeftShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .LeftShiftAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .RightShift => .{ .prefix = null, .infix = parseBinary, .precedence = .Shift },
            .RightShiftAssign => .{ .prefix = null, .infix = null, .precedence = .Lowest },
            .Let => .{ .prefix = null, .infix = null, .precedence = null },
            .Match => .{ .prefix = parseMatch, .infix = null, .precedence = null },
            .While => .{ .prefix = null, .infix = null, .precedence = null },
            .Fn => .{ .prefix = parseFunction, .infix = null, .precedence = null },
            .List => .{ .prefix = parseList, .infix = null, .precedence = null },
            .Table => .{ .prefix = parseTable, .infix = null, .precedence = null },
            .Struct => .{ .prefix = parseStruct, .infix = null, .precedence = null },
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
    return .Lowest;
}
