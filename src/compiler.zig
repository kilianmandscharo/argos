const std = @import("std");
const scanner = @import("scanner.zig");
const chunk_module = @import("chunk.zig");

const Value = chunk_module.Value;
const Chunk = chunk_module.Chunk;
const OpCode = chunk_module.OpCode;
const OpByte = chunk_module.OpByte;
const Token = scanner.Token;
const TokenType = scanner.TokenType;
const Scanner = scanner.Scanner;

const Precedence = enum(u8) {
    Lowest = 1,
    Range,
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

fn binary(compiler: *Compiler) !void {
    const operator_type = compiler.parser.previous.type;
    const precedence_value = getRulePrecedenceValue(operator_type);
    try compiler.parsePrecedence(@as(Precedence, @enumFromInt(precedence_value + 1)));

    switch (operator_type) {
        .Plus => try compiler.emitOpCode(.Add),
        .Minus => try compiler.emitOpCode(.Subtract),
        .Asterisk => try compiler.emitOpCode(.Multiply),
        .Slash => try compiler.emitOpCode(.Divide),
        else => unreachable,
    }
}

fn grouping(compiler: *Compiler) !void {
    try compiler.expression();
    compiler.consume(.RParen, "Expect ')' after expression");
}

fn unary(compiler: *Compiler) !void {
    const operator_type = compiler.parser.previous.type;
    try compiler.parsePrecedence(.Prefix);
    switch (operator_type) {
        .Minus => try compiler.emitOpCode(.Negate),
        else => unreachable,
    }
}

fn number(compiler: *Compiler) !void {
    const value = try std.fmt.parseFloat(f64, compiler.parser.previous.toString());
    try compiler.emitConstant(value);
}

const ParseRule = struct {
    prefix: ?*const fn (compiler: *Compiler) anyerror!void,
    infix: ?*const fn (compiler: *Compiler) anyerror!void,
    precedence: ?Precedence,
};

const token_count = @typeInfo(TokenType).@"enum".fields.len;

const rules: [token_count]ParseRule = initRules();

fn initRules() [token_count]ParseRule {
    var table: [token_count]ParseRule = undefined;

    inline for (@typeInfo(TokenType).@"enum".fields) |field| {
        const index = field.value;
        const tag = @as(TokenType, @enumFromInt(index));
        table[index] = switch (tag) {
            .LParen => .{ .prefix = grouping, .infix = null, .precedence = .Call },
            .RParen => .{ .prefix = null, .infix = null, .precedence = null },
            .LBracket => .{ .prefix = null, .infix = null, .precedence = .Index },
            .RBracket => .{ .prefix = null, .infix = null, .precedence = null },
            .LBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .RBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .Assign => .{ .prefix = null, .infix = null, .precedence = .Assign },
            .Comma => .{ .prefix = null, .infix = null, .precedence = null },
            .String => .{ .prefix = null, .infix = null, .precedence = null },
            .Number => .{ .prefix = number, .infix = null, .precedence = null },
            .True => .{ .prefix = null, .infix = null, .precedence = null },
            .False => .{ .prefix = null, .infix = null, .precedence = null },
            .Null => .{ .prefix = null, .infix = null, .precedence = null },
            .Identifier => .{ .prefix = null, .infix = null, .precedence = null },
            .Bang => .{ .prefix = null, .infix = null, .precedence = null },
            .Lt => .{ .prefix = null, .infix = null, .precedence = .LessGreater },
            .LtOrEq => .{ .prefix = null, .infix = null, .precedence = .LessGreater },
            .Gt => .{ .prefix = null, .infix = null, .precedence = .LessGreater },
            .GtOrEq => .{ .prefix = null, .infix = null, .precedence = .LessGreater },
            .Eq => .{ .prefix = null, .infix = null, .precedence = .Equals },
            .NotEq => .{ .prefix = null, .infix = null, .precedence = .Equals },
            .Plus => .{ .prefix = null, .infix = binary, .precedence = .Sum },
            .Minus => .{ .prefix = unary, .infix = binary, .precedence = .Sum },
            .Slash => .{ .prefix = null, .infix = binary, .precedence = .Product },
            .Asterisk => .{ .prefix = null, .infix = binary, .precedence = .Product },
            .Percent => .{ .prefix = null, .infix = null, .precedence = .Product },
            .Return => .{ .prefix = null, .infix = null, .precedence = null },
            .If => .{ .prefix = null, .infix = null, .precedence = null },
            .Else => .{ .prefix = null, .infix = null, .precedence = null },
            .For => .{ .prefix = null, .infix = null, .precedence = null },
            .In => .{ .prefix = null, .infix = null, .precedence = null },
            .Dot => .{ .prefix = null, .infix = null, .precedence = .Index },
            .DotDot => .{ .prefix = null, .infix = null, .precedence = .Range },
            .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
            .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
            .Eof => .{ .prefix = null, .infix = null, .precedence = null },
            .And => .{ .prefix = null, .infix = null, .precedence = .LogicalAnd },
            .Or => .{ .prefix = null, .infix = null, .precedence = .LogicalOr },
            .Pipe => .{ .prefix = null, .infix = null, .precedence = .BitwiseOr },
            .Ampersand => .{ .prefix = null, .infix = null, .precedence = .BitwiseAnd },
            .Caret => .{ .prefix = null, .infix = null, .precedence = .BitwiseXor },
            .Tilde => .{ .prefix = null, .infix = null, .precedence = .Prefix },
            .LeftShift => .{ .prefix = null, .infix = null, .precedence = .Shift },
            .RightShift => .{ .prefix = null, .infix = null, .precedence = .Shift },
            .Error => .{ .prefix = null, .infix = null, .precedence = null },
        };
    }

    return table;
}

fn getRule(token_type: TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

fn getRulePrecedenceValue(token_type: TokenType) usize {
    if (getRule(token_type).precedence) |precedence| {
        return @intFromEnum(precedence);
    }
    return @intFromEnum(Precedence.Lowest);
}

pub const Parser = struct {
    current: Token,
    previous: Token,
    had_error: bool,
    panic_mode: bool,
};

pub const Compiler = struct {
    parser: Parser,
    scanner: Scanner,
    compiling_chunk: *Chunk,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) Compiler {
        return Compiler{
            .parser = Parser{
                .current = undefined,
                .previous = undefined,
                .had_error = false,
                .panic_mode = false,
            },
            .scanner = Scanner.init(source),
            .allocator = allocator,
            .compiling_chunk = undefined,
        };
    }

    pub fn compile(self: *Compiler, chunk: *Chunk) !void {
        self.compiling_chunk = chunk;
        self.advance();
        try self.expression();
        try self.emitOpCode(.Return);
    }

    fn endCompiler(self: *Compiler) void {
        self.emitOpCode(.Return);
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) !void {
        self.advance();

        if (getRule(self.parser.previous.type).prefix) |prefixFn| {
            try prefixFn(self);
        } else {
            self.errorAtPrevious("Expect expression.");
            return;
        }

        while (@intFromEnum(precedence) < getRulePrecedenceValue(self.parser.current.type)) {
            self.advance();
            if (self.parser.current.type == .Eof) return;
            if (getRule(self.parser.previous.type).infix) |infixFn| {
                try infixFn(self);
            }
        }
    }

    fn expression(self: *Compiler) !void {
        try self.parsePrecedence(.Lowest);
    }

    fn emitConstant(self: *Compiler, value: Value) !void {
        try self.currentChunk().writeConstant(self.allocator, value, self.parser.previous.line);
    }

    fn makeConstant(self: *Compiler, value: Value) !usize {
        const constant = try self.currentChunk().addConstant(self.allocator, value);
        if (constant > std.math.maxInt(u8)) {
            self.errorAtPrevious("Too many constants in one chunk");
            return error.TooManyConstants;
        }
        return constant;
    }

    fn currentChunk(self: *Compiler) *Chunk {
        return self.compiling_chunk;
    }

    fn emitBytes(self: *Compiler, first: OpCode, second: u8) void {
        self.emitOpCode(first);
        self.emitByte(second);
    }

    fn emitOpByte(self: *Compiler, op_byte: OpByte) void {
        self.currentChunk().write(self.allocator, op_byte, self.parser.previous.line);
    }

    fn emitByte(self: *Compiler, byte: u8) void {
        self.currentChunk().write(self.allocator, OpByte{ .Byte = byte }, self.parser.previous.line);
    }

    fn emitOpCode(self: *Compiler, op_code: OpCode) !void {
        try self.currentChunk().write(self.allocator, OpByte{ .Op = op_code }, self.parser.previous.line);
    }

    fn advance(self: *Compiler) void {
        self.parser.previous = self.parser.current;
        while (true) {
            self.parser.current = self.scanner.next();
            if (self.parser.current.type != .Error) break;
            self.errorAtCurrent(self.parser.current.toString());
        }
    }

    fn consume(self: *Compiler, expected: TokenType, message: []const u8) void {
        if (self.parser.current.type == expected) {
            self.advance();
            return;
        }
        self.errorAtCurrent(message);
    }

    fn errorAtCurrent(self: *Compiler, message: []const u8) void {
        self.errorAt(&self.parser.current, message);
    }

    fn errorAtPrevious(self: *Compiler, message: []const u8) void {
        self.errorAt(&self.parser.previous, message);
    }

    fn errorAt(self: *Compiler, token: *Token, message: []const u8) void {
        if (self.parser.panic_mode) return;
        self.parser.panic_mode = true;
        std.debug.print("[line {d}] Error", .{token.line});
        switch (token.type) {
            .Eof => std.debug.print(" at end", .{}),
            .Error => {},
            else => std.debug.print(" at '{s}'", .{token.source[token.start .. token.start + token.length]}),
        }
        std.debug.print(": {s}\n", .{message});
    }
};
