const std = @import("std");
const scanner = @import("scanner.zig");
const chunk_module = @import("chunk.zig");
const value_module = @import("value.zig");
const object_module = @import("object.zig");
const vm_module = @import("vm.zig");

const VirtualMachine = vm_module.VirtualMachine;

const Obj = object_module.Obj;
const ObjString = object_module.ObjString;
const allocateStaticString = object_module.allocateStaticString;

const Value = value_module.Value;
const wrapInt = value_module.wrapInt;
const wrapFloat = value_module.wrapFloat;
const wrapBool = value_module.wrapBool;
const wrapString = value_module.wrapObj;

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

pub const Compiler = struct {
    parser: Parser,
    scanner: Scanner,
    compiling_chunk: *Chunk,
    gpa: std.mem.Allocator,
    vm: *VirtualMachine,

    const Parser = struct {
        current: Token,
        previous: Token,
        had_error: bool,
        panic_mode: bool,
    };

    pub fn init(vm: *VirtualMachine, gpa: std.mem.Allocator) Compiler {
        return Compiler{
            .parser = Parser{
                .current = undefined,
                .previous = undefined,
                .had_error = false,
                .panic_mode = false,
            },
            .scanner = undefined,
            .gpa = gpa,
            .compiling_chunk = undefined,
            .vm = vm,
        };
    }

    pub fn compile(self: *Compiler, chunk: *Chunk, source: []const u8) !void {
        self.scanner = Scanner.init(source);
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
        try self.currentChunk().writeConstant(self.gpa, value, self.parser.previous.line);
    }

    fn makeConstant(self: *Compiler, value: Value) !usize {
        const constant = try self.currentChunk().addConstant(self.gpa, value);
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
        self.currentChunk().write(self.gpa, op_byte, self.parser.previous.line);
    }

    fn emitByte(self: *Compiler, byte: u8) void {
        self.currentChunk().write(self.gpa, OpByte{ .Byte = byte }, self.parser.previous.line);
    }

    fn emitOpCode(self: *Compiler, op_code: OpCode) !void {
        try self.currentChunk().write(self.gpa, OpByte{ .Op = op_code }, self.parser.previous.line);
    }

    fn emitOpCodes(self: *Compiler, a: OpCode, b: OpCode) !void {
        try self.currentChunk().write(self.gpa, OpByte{ .Op = a }, self.parser.previous.line);
        try self.currentChunk().write(self.gpa, OpByte{ .Op = b }, self.parser.previous.line);
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

fn binary(compiler: *Compiler) !void {
    const operator_type = compiler.parser.previous.type;
    try compiler.parsePrecedence(getRulePrecedence(operator_type));

    switch (operator_type) {
        .Plus => try compiler.emitOpCode(.Add),
        .Minus => try compiler.emitOpCode(.Subtract),
        .Asterisk => try compiler.emitOpCode(.Multiply),
        .Slash => try compiler.emitOpCode(.Divide),
        .Eq => try compiler.emitOpCode(.Equal),
        .NotEq => try compiler.emitOpCodes(.Equal, .Not),
        .Gt => try compiler.emitOpCode(.Greater),
        .GtOrEq => try compiler.emitOpCodes(.Less, .Not),
        .Lt => try compiler.emitOpCode(.Less),
        .LtOrEq => try compiler.emitOpCodes(.Greater, .Not),
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
        .Bang => try compiler.emitOpCode(.Not),
        else => unreachable,
    }
}

fn float(compiler: *Compiler) !void {
    const value = try std.fmt.parseFloat(f64, compiler.parser.previous.toString());
    try compiler.emitConstant(wrapFloat(value));
}

fn integer(compiler: *Compiler) !void {
    const value = try std.fmt.parseInt(i64, compiler.parser.previous.toString(), 10);
    try compiler.emitConstant(wrapInt(value));
}

fn literal(compiler: *Compiler) !void {
    switch (compiler.parser.previous.type) {
        .False => try compiler.emitOpCode(.False),
        .Null => try compiler.emitOpCode(.Null),
        .True => try compiler.emitOpCode(.True),
        else => unreachable,
    }
}

fn string(compiler: *Compiler) !void {
    const start = compiler.parser.previous.start + 1;
    const end = start + compiler.parser.previous.length - 2;
    const slice = compiler.parser.previous.source[start..end];

    const obj = try allocateStaticString(compiler.vm, slice);

    try compiler.emitConstant(wrapString(obj));
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
            .String => .{ .prefix = string, .infix = null, .precedence = null },
            .Float => .{ .prefix = float, .infix = null, .precedence = null },
            .Int => .{ .prefix = integer, .infix = null, .precedence = null },
            .True => .{ .prefix = literal, .infix = null, .precedence = null },
            .False => .{ .prefix = literal, .infix = null, .precedence = null },
            .Null => .{ .prefix = literal, .infix = null, .precedence = null },
            .Identifier => .{ .prefix = null, .infix = null, .precedence = null },
            .Bang => .{ .prefix = unary, .infix = null, .precedence = null },
            .Lt => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .LtOrEq => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .Gt => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .GtOrEq => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .Eq => .{ .prefix = null, .infix = binary, .precedence = .Equals },
            .NotEq => .{ .prefix = null, .infix = binary, .precedence = .Equals },
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
    return @intFromEnum(getRulePrecedence(token_type));
}

fn getRulePrecedence(token_type: TokenType) Precedence {
    if (getRule(token_type).precedence) |precedence| {
        return precedence;
    }
    return Precedence.Lowest;
}
