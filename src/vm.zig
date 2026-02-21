const std = @import("std");
const chunk_module = @import("chunk.zig");
const compiler_module = @import("compiler.zig");

const OpCode = chunk_module.OpCode;
const Chunk = chunk_module.Chunk;
const Value = chunk_module.Value;
const Compiler = compiler_module.Compiler;

const InterpretResult = enum {
    Ok,
    CompileError,
    RuntimeError,
};

const DEBUG_PRINT_CODE = true;
const DEBUG_TRACE_EXECUTION = true;
const STACK_MAX = 256;

pub const VirtualMachine = struct {
    chunk: Chunk,
    ip: usize,
    stack: [STACK_MAX]Value,
    stack_top: usize,
    compiler: Compiler,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) VirtualMachine {
        return VirtualMachine{
            .chunk = undefined,
            .ip = 0,
            .stack = undefined,
            .stack_top = 0,
            .compiler = Compiler.init(allocator, source),
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

    pub fn interpret(self: *VirtualMachine) !InterpretResult {
        var chunk = Chunk.init();
        try self.compiler.compile(&chunk);
        self.chunk = chunk;
        if (comptime DEBUG_PRINT_CODE) chunk.disassemble();
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
                    // TODO: can be optimized by not popping
                    const a = self.pop();
                    self.push(a + b);
                },
                .Subtract => {
                    const b = self.pop();
                    // TODO: can be optimized by not popping
                    const a = self.pop();
                    self.push(a - b);
                },
                .Multiply => {
                    const b = self.pop();
                    // TODO: can be optimized by not popping
                    const a = self.pop();
                    self.push(a * b);
                },
                .Divide => {
                    const b = self.pop();
                    // TODO: can be optimized by not popping
                    const a = self.pop();
                    self.push(a / b);
                },
                .Negate => {
                    // TODO: can be optimized by not popping
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
