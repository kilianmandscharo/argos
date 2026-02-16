const std = @import("std");
const compiler = @import("compiler.zig");
const Value = compiler.Value;
const OpByte = compiler.OpByte;
const VirtualMachine = compiler.VirtualMachine;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var vm = VirtualMachine.init();

    const allocator = arena.allocator();

    var chunk = compiler.Chunk.init();

    try chunk.writeConstant(allocator, 1.2, 123);
    try chunk.writeConstant(allocator, 3.4, 123);
    try chunk.write(allocator, OpByte{ .Op = .Add }, 123);

    try chunk.writeConstant(allocator, 5.6, 123);
    try chunk.write(allocator, OpByte{ .Op = .Divide }, 123);

    try chunk.write(allocator, OpByte{ .Op = .Negate }, 123);
    try chunk.write(allocator, OpByte{ .Op = .Return }, 123);

    _ = vm.interpret(&chunk);
}
