const std = @import("std");
const compiler = @import("compiler.zig");
const Value = compiler.Value;
const OpByte = compiler.OpByte;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    var chunk = compiler.Chunk.init();
    try chunk.writeConstant(allocator, 1.2, 123);
    try chunk.writeConstant(allocator, 1.2, 123);
    try chunk.writeConstant(allocator, 1.2, 123);
    chunk.disassemble();
}
