const std = @import("std");

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
