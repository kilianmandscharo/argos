const std = @import("std");
const value_module = @import("value.zig");

const Value = value_module.Value;

pub const OpCode = enum(u8) {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
    Null,
    True,
    False,
    Not,
    Equal,
    Greater,
    Less,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
};

pub const OpByte = union(enum) {
    Byte: u8,
    Op: OpCode,
};

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

    pub fn deinit(self: *Chunk, gpa: std.mem.Allocator) void {
        self.code.deinit(gpa);
        self.constants.deinit(gpa);
        self.lines.deinit(gpa);
    }

    pub fn write(self: *Chunk, gpa: std.mem.Allocator, op_byte: OpByte, line: usize) !void {
        switch (op_byte) {
            .Byte => |byte| {
                try self.code.append(gpa, byte);
            },
            .Op => |op| {
                try self.code.append(gpa, @intFromEnum(op));
            },
        }
        try self.lines.append(gpa, line);
    }

    pub fn writeConstant(self: *Chunk, gpa: std.mem.Allocator, value: Value, line: usize) !void {
        const constant = try self.addConstant(gpa, value);
        const bytes = indexToBytes(constant);
        try self.write(gpa, OpByte{ .Op = .Constant }, line);
        try self.write(gpa, OpByte{ .Byte = bytes[0] }, line);
        try self.write(gpa, OpByte{ .Byte = bytes[1] }, line);
        try self.write(gpa, OpByte{ .Byte = bytes[2] }, line);
    }

    pub fn addConstant(self: *Chunk, gpa: std.mem.Allocator, value: Value) !usize {
        try self.constants.append(gpa, value);
        return self.constants.items.len - 1;
    }

    pub fn disassemble(self: *Chunk) void {
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
        std.debug.print("\n", .{});
    }

    pub fn disassembleInstruction(self: *Chunk, offset: usize) usize {
        std.debug.print("{d:0>4} ", .{offset});
        if (offset > 0 and self.lines.items[offset] == self.lines.items[offset - 1]) {
            std.debug.print("   | ", .{});
        } else {
            std.debug.print("{d:4} ", .{self.lines.items[offset]});
        }
        switch (@as(OpCode, @enumFromInt(self.code.items[offset]))) {
            .Return => return simpleInstruction("OP_RETURN", offset),
            .Add => return simpleInstruction("OP_ADD", offset),
            .Subtract => return simpleInstruction("OP_SUBTRACT", offset),
            .Multiply => return simpleInstruction("OP_MULTIPLY", offset),
            .Divide => return simpleInstruction("OP_DIVIDE", offset),
            .Negate => return simpleInstruction("OP_NEGATE", offset),
            .True => return simpleInstruction("OP_TRUE", offset),
            .False => return simpleInstruction("OP_FALSE", offset),
            .Null => return simpleInstruction("OP_NULL", offset),
            .Not => return simpleInstruction("OP_NOT", offset),
            .Greater => return simpleInstruction("OP_GREATER", offset),
            .Less => return simpleInstruction("OP_LESS", offset),
            .Equal => return simpleInstruction("OP_EQUAL", offset),
            .Print => return simpleInstruction("OP_PRINT", offset),
            .Pop => return simpleInstruction("OP_POP", offset),
            .DefineGlobal => return constantInstruction(self, "OP_DEFINE_GLOBAL", offset),
            .Constant => return constantInstruction(self, "OP_CONSTANT", offset),
            .GetGlobal => return constantInstruction(self, "OP_GET_GLOBAL", offset),
            .SetGlobal => return constantInstruction(self, "OP_SET_GLOBAL", offset),
            .GetLocal => return localInstruction(self, "OP_GET_LOCAL", offset),
            .SetLocal => return localInstruction(self, "OP_SET_LOCAL", offset),
        }
    }
};

pub inline fn indexToBytes(index: usize) [3]u8 {
    return .{
        @intCast((index >> 16) & 0xFF),
        @intCast((index >> 8) & 0xFF),
        @intCast((index & 0xFF)),
    };
}

pub inline fn bytesToIndex(first: u8, second: u8, third: u8) usize {
    return (@as(usize, @intCast(first)) << 16) | (@as(usize, @intCast(second)) << 8) | (@as(usize, @intCast(third)));
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(chunk: *Chunk, name: []const u8, offset: usize) usize {
    const constant = bytesToIndex(
        chunk.code.items[offset + 1],
        chunk.code.items[offset + 2],
        chunk.code.items[offset + 3],
    );
    const value = chunk.constants.items[constant];
    std.debug.print("{s:<18}{d:4} '{f}'\n", .{ name, constant, value });
    return offset + 4;
}

fn localInstruction(chunk: *Chunk, name: []const u8, offset: usize) usize {
    const index = bytesToIndex(
        chunk.code.items[offset + 1],
        chunk.code.items[offset + 2],
        chunk.code.items[offset + 3],
    );
    std.debug.print("{s:<18}{d:4}\n", .{ name, index });
    return offset + 4;
}
