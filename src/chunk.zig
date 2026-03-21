const std = @import("std");
const value = @import("value.zig");
const logging = @import("logging.zig");

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "Chunk",
    });
}

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
    Assert,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    JumpIfNotEq,
    JumpIfGreaterOrEq,
    Jump,
    Loop,
    Call,
    Closure,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
};

pub const OpByte = union(enum) {
    Byte: u8,
    Op: OpCode,
};

pub const Chunk = struct {
    code: std.ArrayList(u8),
    constants: std.ArrayList(value.Value),
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

    pub fn writeConstant(self: *Chunk, gpa: std.mem.Allocator, val: value.Value, line: usize) !void {
        const constant = try self.addConstant(gpa, val);
        const bytes = indexToU24(constant);
        try self.write(gpa, OpByte{ .Op = .Constant }, line);
        try self.write(gpa, OpByte{ .Byte = bytes[0] }, line);
        try self.write(gpa, OpByte{ .Byte = bytes[1] }, line);
        try self.write(gpa, OpByte{ .Byte = bytes[2] }, line);
    }

    pub fn addConstant(self: *Chunk, gpa: std.mem.Allocator, val: value.Value) !usize {
        // TODO: push value on to stack
        try self.constants.append(gpa, val);
        // TODO: pop value from stack
        return self.constants.items.len - 1;
    }

    pub fn disassemble(self: *Chunk, name: []const u8) void {
        logDebug("== {s} ==", .{name});
        var offset: usize = 0;
        while (offset < self.code.items.len) {
            offset = self.disassembleInstruction(offset);
        }
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
            .Assert => return simpleInstruction("OP_ASSERT", offset),
            .Pop => return simpleInstruction("OP_POP", offset),
            .DefineGlobal => return constantInstruction(self, "OP_DEFINE_GLOBAL", offset),
            .Constant => return constantInstruction(self, "OP_CONSTANT", offset),
            .GetGlobal => return constantInstruction(self, "OP_GET_GLOBAL", offset),
            .SetGlobal => return constantInstruction(self, "OP_SET_GLOBAL", offset),
            .GetLocal => return threeByteInstruction(self, "OP_GET_LOCAL", offset),
            .SetLocal => return threeByteInstruction(self, "OP_SET_LOCAL", offset),
            .JumpIfFalse => return jumpByteInstruction(self, "OP_JUMP_IF_FALSE", offset, 1),
            .JumpIfNotEq => return jumpByteInstruction(self, "OP_JUMP_IF_NOT_EQ", offset, 1),
            .JumpIfGreaterOrEq => return jumpByteInstruction(self, "OP_JUMP_IF_GREATER_OR_EQ", offset, 1),
            .Jump => return jumpByteInstruction(self, "OP_JUMP", offset, 1),
            .Loop => return jumpByteInstruction(self, "OP_JUMP", offset, -1),
            .Call => return byteInstruction(self, "OP_CALL", offset),
            .GetUpvalue => return byteInstruction(self, "OP_GET_UPVALUE", offset),
            .SetUpvalue => return byteInstruction(self, "OP_SET_UPVALUE", offset),
            .CloseUpvalue => return simpleInstruction("OP_CLOSE_UPVALUE", offset),
            .Closure => {
                var curr_offset = offset;
                const constant = u24ToIndex(
                    self.code.items[curr_offset + 1],
                    self.code.items[curr_offset + 2],
                    self.code.items[curr_offset + 3],
                );
                curr_offset += 4;
                const val = self.constants.items[constant];
                const function = val.asObj().asFunction();
                std.debug.print("{s:<18}{d:4} {f}\n", .{ "OP_CLOSURE", constant, val });
                for (0..function.upvalue_count) |_| {
                    const is_local = if (self.code.items[curr_offset] == 1) "local" else "upvalue";
                    curr_offset += 1;
                    const index = self.code.items[curr_offset];
                    curr_offset += 1;
                    std.debug.print("{d:0>4}    | {s:<23}{s}{d:4}\n", .{ curr_offset - 2, "", is_local, index });
                }
                return curr_offset;
            },
        }
    }
};

pub inline fn indexToU24(index: usize) [3]u8 {
    return .{
        @intCast((index >> 16) & 0xFF),
        @intCast((index >> 8) & 0xFF),
        @intCast((index & 0xFF)),
    };
}

pub inline fn indexToU16(index: usize) [2]u8 {
    return .{
        @intCast((index >> 8) & 0xFF),
        @intCast((index & 0xFF)),
    };
}

pub inline fn u24ToIndex(first: u8, second: u8, third: u8) usize {
    return (@as(usize, @intCast(first)) << 16) | (@as(usize, @intCast(second)) << 8) | (@as(usize, @intCast(third)));
}

pub inline fn u16ToIndex(first: u8, second: u8) usize {
    return (@as(usize, @intCast(first)) << 8) | (@as(usize, @intCast(second)));
}

fn simpleInstruction(name: []const u8, offset: usize) usize {
    std.debug.print("{s}\n", .{name});
    return offset + 1;
}

fn constantInstruction(chunk: *Chunk, name: []const u8, offset: usize) usize {
    const constant = u24ToIndex(
        chunk.code.items[offset + 1],
        chunk.code.items[offset + 2],
        chunk.code.items[offset + 3],
    );
    const val = chunk.constants.items[constant];
    std.debug.print("{s:<18}{d:4} {f}\n", .{ name, constant, val });
    return offset + 4;
}

fn threeByteInstruction(chunk: *Chunk, name: []const u8, offset: usize) usize {
    const index = u24ToIndex(
        chunk.code.items[offset + 1],
        chunk.code.items[offset + 2],
        chunk.code.items[offset + 3],
    );
    std.debug.print("{s:<18}{d:4}\n", .{ name, index });
    return offset + 4;
}

fn byteInstruction(chunk: *Chunk, name: []const u8, offset: usize) usize {
    std.debug.print("{s:<18}{d:4}\n", .{ name, chunk.code.items[offset + 1] });
    return offset + 2;
}

fn jumpByteInstruction(chunk: *Chunk, name: []const u8, offset: usize, sign: i8) usize {
    const jump = u16ToIndex(
        chunk.code.items[offset + 1],
        chunk.code.items[offset + 2],
    );
    const to = @as(i32, @intCast(offset)) + 3 + @as(i32, sign) * @as(i32, @intCast(jump));
    std.debug.print("{s:<18}{d:4} -> {d}\n", .{ name, offset, to });
    return offset + 3;
}
