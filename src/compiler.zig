const std = @import("std");
const scanner = @import("scanner.zig");

const Token = scanner.Token;
const TokenType = scanner.TokenType;
const Scanner = scanner.Scanner;

pub const OpCode = enum(u8) {
    Return,
    Constant,
    Constant_Long,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
};

pub const OpByte = union(enum) {
    Byte: u8,
    Op: OpCode,
};

pub const Value = f64;

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(Value),
    lines: std.ArrayList(usize),

    pub fn init() Chunk {
        return Chunk{
            .code = .{},
            .constants = .{},
            .lines = .{},
        };
    }

    pub fn write(self: *Chunk, allocator: std.mem.Allocator, op_byte: OpByte, line: usize) !void {
        switch (op_byte) {
            .Byte => |byte| {
                try self.code.append(allocator, byte);
            },
            .Op => |op| {
                try self.code.append(allocator, @intFromEnum(op));
            },
        }
        try self.lines.append(allocator, line);
    }

    pub fn writeConstant(self: *Chunk, allocator: std.mem.Allocator, value: Value, line: usize) !void {
        const constant = try self.addConstant(allocator, value);
        try self.write(allocator, OpByte{ .Op = .Constant }, line);
        try self.write(allocator, OpByte{ .Byte = @intCast(constant) }, line);
    }

    pub fn writeConstantLong(self: *Chunk, allocator: std.mem.Allocator, value: Value, line: usize) !void {
        const constant = try self.addConstant(allocator, value);
        try self.write(allocator, OpByte{ .Op = .Constant_Long }, line);
        try self.write(allocator, OpByte{ .Byte = @intCast((constant >> 16) & 0xFF) }, line);
        try self.write(allocator, OpByte{ .Byte = @intCast((constant >> 8) & 0xFF) }, line);
        try self.write(allocator, OpByte{ .Byte = @intCast((constant & 0xFF)) }, line);
    }

    pub fn addConstant(self: *Chunk, allocator: std.mem.Allocator, value: Value) !usize {
        try self.constants.append(allocator, value);
        return self.constants.items.len - 1;
    }

    pub fn disassemble(self: *Chunk) void {
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
        std.debug.print("\n", .{});
    }

    fn simpleInstruction(name: []const u8, offset: usize) usize {
        std.debug.print("{s}\n", .{name});
        return offset + 1;
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        std.debug.print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{d:4} ", .{self.lines.items[offset]});
        }
        switch (@as(OpCode, @enumFromInt(self.code.items[offset]))) {
            .Return => {
                return Chunk.simpleInstruction("OP_RETURN", offset);
            },
            .Constant => {
                const constant = self.code.items[offset + 1];
                const value = self.constants.items[constant];
                std.debug.print("OP_CONSTANT {d:4} '{d}'\n", .{ constant, value });
                return offset + 2;
            },
            .Constant_Long => {
                const b1 = self.code.items[offset + 1];
                const b2 = self.code.items[offset + 2];
                const b3 = self.code.items[offset + 3];
                const constant: usize = (@as(usize, @intCast(b1)) << 16) | (@as(usize, @intCast(b2)) << 8) | @as(usize, @intCast(b3));
                const value = self.constants.items[constant];
                std.debug.print("OP_CONSTANT_LONG {d:4} '{d}'\n", .{ constant, value });
                return offset + 4;
            },
            .Add => {
                return Chunk.simpleInstruction("OP_ADD", offset);
            },
            .Subtract => {
                return Chunk.simpleInstruction("OP_SUBTRACT", offset);
            },
            .Multiply => {
                return Chunk.simpleInstruction("OP_MULTIPLY", offset);
            },
            .Divide => {
                return Chunk.simpleInstruction("OP_DIVIDE", offset);
            },
            .Negate => {
                return Chunk.simpleInstruction("OP_NEGATE", offset);
            },
        }
    }
};

const DEBUG_TRACE_EXECUTION = true;
const STACK_MAX = 256;

const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

pub const VirtualMachine = struct {
    chunk: *Chunk,
    ip: usize,
    stack: [STACK_MAX]Value,
    stack_top: usize,

    pub fn init() VirtualMachine {
        return VirtualMachine{
            .chunk = undefined,
            .ip = 0,
            .stack = undefined,
            .stack_top = 0,
        };
    }

    fn resetStack(self: *VirtualMachine) void {
        self.stack_top = 0;
    }

    fn push(self: *VirtualMachine, value: Value) void {
        self.stack[self.stack_top] = value;
        self.stack_top += 1;
    }

    fn pop(self: *VirtualMachine) Value {
        self.stack_top -= 1;
        return self.stack[self.stack_top];
    }

    pub fn interpret(self: *VirtualMachine, source: []const u8) !InterpretResult {
        const chunk = try self.compile(source);
        self.chunk = chunk;
        const result = self.run();
        return result;
    }

    pub inline fn readByte(self: *VirtualMachine) u8 {
        const instruction = self.chunk.code.items[self.ip];
        self.ip += 1;
        return instruction;
    }

    pub fn run(self: *VirtualMachine) InterpretResult {
        while (true) {
            if (comptime DEBUG_TRACE_EXECUTION) {
                std.debug.print("          ", .{});
                for (0..self.stack_top) |index| {
                    std.debug.print("[ {d} ]", .{self.stack[index]});
                }
                std.debug.print("\n", .{});
                _ = self.chunk.disassembleInstruction(self.ip);
            }
            const instruction = self.readByte();
            switch (@as(OpCode, @enumFromInt(instruction))) {
                .Constant => {
                    const constant = self.chunk.constants.items[self.readByte()];
                    self.push(constant);
                },
                .Add => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a + b);
                },
                .Subtract => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a - b);
                },
                .Multiply => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a * b);
                },
                .Divide => {
                    const b = self.pop();
                    const a = self.pop();
                    self.push(a / b);
                },
                .Negate => {
                    self.push(-self.pop());
                },
                .Return => {
                    const value = self.pop();
                    std.debug.print("{d}\n", .{value});
                    return .Ok;
                },
                else => return .RuntimeError,
            }
        }
    }
};

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

const ParseRule = struct {};

const token_count = @typeInfo(TokenType).@"enum".fields.len;

const rules: [token_count]ParseRule = initRules();

fn initRules() [token_count]ParseRule {
    var table: [token_count]ParseRule = undefined;

    inline for (@typeInfo(TokenType).@"enum".fields) |field| {
        const index = field.value;
        const tag = @as(TokenType, @enumFromInt(index));
        table[index] = switch (tag) {
            .LParen => .{},
        };
    }

    return table;
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
    compiling_chunk: Chunk,
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

    pub fn compile(self: *Compiler) !Chunk {
        const chunk = Chunk{};
        self.compiling_chunk = chunk;
        return if (self.parser.had_error) error.CompilationError else chunk;
    }

    fn endCompiler(self: *Compiler) void {
        self.emitOpCode(.Return);
    }

    // fn parsePrecedence(self: *Compiler, precedence: Precedence) void {}

    fn expression(self: *Compiler) void {
        self.parsePrecedence(.Lowest);
    }

    fn binary(self: *Compiler) void {
        const operator_type = self.parser.previous.type;
        const rule = self.getRule(operator_type);
        self.parsePrecedence(rule.precedence + 1);

        switch (operator_type) {
            .Plus => self.emitOpCode(.Add),
            .Minus => self.emitOpCode(.Subtract),
            .Asterisk => self.emitOpCode(.Multiply),
            .Slash => self.emitOpCode(.Divide),
            else => unreachable,
        }
    }

    fn grouping(self: *Compiler) void {
        self.expression();
        self.consume(.RParen, "Expect ')' after expression");
    }

    fn unary(self: *Compiler) void {
        const operator_type = self.parser.previous.type;
        self.expression(.Prefix);
        switch (operator_type) {
            .Minus => self.emitOpCode(.Negate),
            else => unreachable,
        }
    }

    fn number(self: *Compiler) !void {
        const value = try std.fmt.parseFloat(f64, self.parser.previous.toString());
        try self.makeConstant(value);
    }

    fn emitConstant(self: *Compiler, value: Value) !void {
        self.emitBytes(.Constant, try self.makeConstant(value));
    }

    fn makeConstant(self: *Compiler, value: Value) !usize {
        const constant = try self.currentChunk().addConstant(self.allocator, value);
        if (constant > std.math.maxInt(u8)) {
            self.errorAtPrevious("Too many constants in one chunk");
            return;
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

    fn emitOpCode(self: *Compiler, op_code: OpCode) void {
        self.currentChunk().write(self.allocator, OpByte{ .Op = op_code }, self.parser.previous.line);
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
