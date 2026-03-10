const std = @import("std");
const logging = @import("logging.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const vm_module = @import("vm.zig");

const VirtualMachine = vm_module.VirtualMachine;
const TableGlobals = vm_module.TableGlobals;

const DEBUG_LOG_GC = true;

pub fn collectGarbage(vm: *VirtualMachine) !void {
    if (comptime DEBUG_LOG_GC) {
        logging.log("-- gc begin", .{}, .{});
    }

    markRoots(vm);

    if (comptime DEBUG_LOG_GC) {
        logging.log("-- gc end", .{}, .{});
    }
}

fn markRoots(vm: *VirtualMachine) void {
    for (0..vm.stack_top) |i| {
        markValue(vm.stack[i]);
    }

    for (0..vm.frame_count) |i| {
        markObject(&vm.frames[i].closure.obj);
    }

    var upvalue = vm.open_upvalues;
    while (upvalue) |val| {
        markObject(&val.obj);
        upvalue = val.next;
    }

    markTable(&vm.globals);
}

fn markValue(val: value.Value) void {
    if (val.isObj()) markObject(val.Obj);
}

fn markObject(obj: *object.Obj) void {
    if (comptime DEBUG_LOG_GC) {
        logging.log("0x{x} mark {s}", .{ @intFromPtr(obj), obj.getType() }, .{});
    }

    obj.is_marked = true;
}

fn markTable(table: *TableGlobals) void {
    var it = table.iterator();
    while (it.next()) |entry| {
        markObject(&entry.key_ptr.*.obj);
        markValue(entry.value_ptr.*);
    }
}
