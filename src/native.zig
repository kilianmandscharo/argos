const std = @import("std");
const value = @import("value.zig");
const object = @import("object.zig");
const virtual_machine = @import("vm.zig");
const hm = @import("hm.zig");

const Native = struct {
    name: []const u8,
    function: object.NativeFn,
    getDecl: *const fn (type_checker: *hm.TypeChecker) anyerror!hm.Declaration,
};

const c = hm.TypeChecker.c;

pub const natives: []const Native = &.{
    .{
        .name = "clock",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                _ = args;
                const now = std.Io.Timestamp.now(vm.io, .real);
                const time: f64 = @floatFromInt(std.Io.Timestamp.toMilliseconds(now));
                const ms_per_s: f64 = @floatFromInt(std.time.ms_per_s);
                return value.wrapFloat(@divExact(time, ms_per_s));
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                return .initMono(
                    try type_checker.nativeFunction(&.{}, c(.Integer)),
                    &.{},
                );
            }
        }.getDecl,
    },
    .{
        .name = "print",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                _ = vm;
                const val = args[0];
                std.debug.print("{f}\n", .{val});
                return value.valueUnit();
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                const generic_var = try type_checker.variable();
                const vars = try type_checker.compiler_arena.dupe(*hm.Var, &.{generic_var.Var});
                return .initPoly(
                    vars,
                    try type_checker.nativeFunction(&.{generic_var}, c(.Unit)),
                    &.{.Required},
                );
            }
        }.getDecl,
    },
    .{
        .name = "assert",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                const val = args[0];
                if (!val.Bool) return vm.runtimeError("Assertion failed", .{});
                return value.valueUnit();
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                return .initMono(
                    try type_checker.nativeFunction(&.{c(.Boolean)}, c(.Unit)),
                    &.{.Required},
                );
            }
        }.getDecl,
    },
    .{
        .name = "list_len",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                _ = vm;
                const val = args[0];
                return value.wrapInt(@intCast(val.asObj().asList().len()));
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                const generic_var = try type_checker.variable();
                const vars = try type_checker.compiler_arena.dupe(*hm.Var, &.{generic_var.Var});
                return .initPoly(
                    vars,
                    try type_checker.nativeFunction(&.{try type_checker.list(generic_var)}, c(.Integer)),
                    &.{.Required},
                );
            }
        }.getDecl,
    },
    .{
        .name = "list_get",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                const val = args[0];
                const index = args[1];
                var i = index.Int;
                if (i < 0) {
                    return vm.runtimeError("expected list index >= 0, got {d}", .{i});
                }
                var it = val.asObj().asList().iterator();
                while (it.next()) |item| {
                    if (i == 0) return item.value;
                    i -= 1;
                }
                return vm.runtimeError("list index {d} exceeds list length", .{index.Int});
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                const generic_var = try type_checker.variable();
                const vars = try type_checker.compiler_arena.dupe(*hm.Var, &.{generic_var.Var});
                return .initPoly(
                    vars,
                    try type_checker.nativeFunction(&.{ try type_checker.list(generic_var), c(.Integer) }, generic_var),
                    &.{ .Required, .Required },
                );
            }
        }.getDecl,
    },
    .{
        .name = "list_reverse",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                const list = args[0].asObj().asList();

                const new_list = try vm.runtime_allocator.allocateList();
                vm.push(value.wrapObj(&new_list.obj));

                var it = list.iterator();
                while (it.next()) |item| try new_list.prepend(vm.runtime_allocator, item.value);

                return vm.pop();
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                const generic_var = try type_checker.variable();
                const vars = try type_checker.compiler_arena.dupe(*hm.Var, &.{generic_var.Var});
                return .initPoly(
                    vars,
                    try type_checker.nativeFunction(&.{try type_checker.list(generic_var)}, try type_checker.list(generic_var)),
                    &.{.Required},
                );
            }
        }.getDecl,
    },
    .{
        .name = "str_slice",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                const s = args[0].asObj().asString();
                const start = args[1].Int;
                const end = args[2].Int;
                const new_s = try vm.runtime_allocator.copyString(s.chars[@intCast(start)..@intCast(end)]);
                return value.wrapObj(new_s);
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                return .initMono(
                    try type_checker.nativeFunction(&.{ c(.String), c(.Integer), c(.Integer) }, c(.String)),
                    &.{ .Required, .Required, .Required },
                );
            }
        }.getDecl,
    },
    .{
        .name = "str_len",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                _ = vm;
                const s = args[0].asObj().asString();
                return value.wrapInt(@intCast(s.chars.len));
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                return .initMono(
                    try type_checker.nativeFunction(&.{c(.String)}, c(.Integer)),
                    &.{.Required},
                );
            }
        }.getDecl,
    },
    .{
        .name = "str_starts_with",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                _ = vm;
                const s = args[0].asObj().asString();
                const target = args[1].asObj().asString();
                return value.wrapBool(std.mem.startsWith(u8, s.chars, target.chars));
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                return .initMono(
                    try type_checker.nativeFunction(&.{ c(.String), c(.String) }, c(.Boolean)),
                    &.{ .Required, .Required },
                );
            }
        }.getDecl,
    },
    .{
        .name = "str_ends_with",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                _ = vm;
                const s = args[0].asObj().asString();
                const target = args[1].asObj().asString();
                return value.wrapBool(std.mem.endsWith(u8, s.chars, target.chars));
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                return .initMono(
                    try type_checker.nativeFunction(&.{ c(.String), c(.String) }, c(.Boolean)),
                    &.{ .Required, .Required },
                );
            }
        }.getDecl,
    },
    .{
        .name = "str_contains",
        .function = struct {
            fn native(vm: *virtual_machine.VirtualMachine, args: []value.Value) !value.Value {
                _ = vm;
                const s = args[0].asObj().asString();
                const target = args[1].asObj().asString();
                return value.wrapBool(std.mem.containsAtLeast(u8, s.chars, 1, target.chars));
            }
        }.native,
        .getDecl = struct {
            fn getDecl(type_checker: *hm.TypeChecker) anyerror!hm.Declaration {
                return .initMono(
                    try type_checker.nativeFunction(&.{ c(.String), c(.String) }, c(.Boolean)),
                    &.{ .Required, .Required },
                );
            }
        }.getDecl,
    },
};
