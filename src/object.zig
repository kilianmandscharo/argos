const std = @import("std");
const virtual_machine = @import("vm.zig");
const chunk = @import("chunk.zig");
const value = @import("value.zig");
const memory = @import("memory.zig");
const logging = @import("logging.zig");
const constants = @import("constants.zig");

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "Object",
    });
}

pub const ObjType = enum { String, Function, NativeFn, Closure, Upvalue, List, ListItem, Record };

pub const Obj = struct {
    type: ObjType,
    is_marked: bool,
    next: ?*Obj,

    pub fn getType(self: *@This()) []const u8 {
        return @tagName(self.type);
    }

    pub fn format(
        self: *@This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.type) {
            .String => try self.asString().format(writer),
            .Function => try self.asFunction().format(writer),
            .NativeFn => try self.asNative().format(writer),
            .Closure => try self.asClosure().format(writer),
            .Upvalue => try self.asUpvalue().format(writer),
            .List => try self.asList().format(writer),
            .ListItem => try self.asList().format(writer),
            .Record => try self.asRecord().format(writer),
        }
    }

    pub fn deinit(self: *@This(), gc_alloc: std.mem.Allocator) void {
        if (comptime constants.debug_log_gc) {
            logDebug("0x{x} free type {s}", .{ @intFromPtr(self), self.getType() });
        }

        switch (self.type) {
            .String => {
                const string_obj = self.asString();
                gc_alloc.free(string_obj.chars);
                gc_alloc.destroy(string_obj);
            },
            .Function => {
                const function_obj = self.asFunction();
                function_obj.chunk.deinit(gc_alloc);
                gc_alloc.destroy(function_obj);
            },
            .NativeFn => {
                const native_obj = self.asNative();
                gc_alloc.destroy(native_obj);
            },
            .Closure => {
                const closure_obj = self.asClosure();
                gc_alloc.free(closure_obj.upvalues);
                gc_alloc.destroy(closure_obj);
            },
            .Upvalue => {
                const upvalue_obj = self.asUpvalue();
                gc_alloc.destroy(upvalue_obj);
            },
            .List => {
                const list_obj = self.asList();
                gc_alloc.destroy(list_obj);
            },
            .ListItem => {
                const list_item_obj = self.asListItem();
                gc_alloc.destroy(list_item_obj);
            },
            .Record => {
                const record_obj = self.asRecord();
                gc_alloc.free(record_obj.fields);
                gc_alloc.destroy(record_obj);
            },
        }
    }

    pub inline fn asFunction(self: *@This()) *ObjFunction {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn asString(self: *@This()) *ObjString {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn asNative(self: *@This()) *ObjNative {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn asClosure(self: *@This()) *ObjClosure {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn asUpvalue(self: *@This()) *ObjUpvalue {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn asList(self: *@This()) *ObjList {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn asListItem(self: *@This()) *ObjListItem {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub inline fn asRecord(self: *@This()) *ObjRecord {
        return @alignCast(@fieldParentPtr("obj", self));
    }
};

pub const ObjString = struct {
    pub const KIND = ObjType.String;

    obj: Obj = undefined,
    chars: []const u8,
    hash: u64,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("\"{s}\"", .{self.chars});
    }
};

pub const ObjFunction = struct {
    pub const KIND = ObjType.Function;

    obj: Obj = undefined,
    arity: i32,
    chunk: chunk.Chunk,
    name: ?*ObjString,
    upvalue_count: u8,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.print("fn {s}", .{if (self.name) |name| name.chars else "<anonymous>"});
    }
};

pub const NativeFn = *const fn (vm: *virtual_machine.VirtualMachine, args: []value.Value) anyerror!value.Value;

pub const ObjNative = struct {
    pub const KIND = ObjType.NativeFn;

    obj: Obj = undefined,
    function: NativeFn,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("<native fn>", .{});
    }
};

pub const ObjClosure = struct {
    pub const KIND = ObjType.Closure;

    obj: Obj = undefined,
    function: *ObjFunction,
    upvalues: []*ObjUpvalue,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try self.function.format(writer);
    }
};

pub const ObjUpvalue = struct {
    pub const KIND = ObjType.Upvalue;

    obj: Obj = undefined,
    location: *value.Value,
    closed: ?value.Value,
    next: ?*ObjUpvalue,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("upvalue", .{});
    }
};

pub const ObjList = struct {
    pub const KIND = ObjType.List;

    obj: Obj = undefined,
    data: std.SinglyLinkedList,

    pub inline fn getValueFromNode(listNode: *const std.SinglyLinkedList.Node) value.Value {
        return getItemFromNode(listNode).value;
    }

    pub inline fn getItemFromNode(node: *const std.SinglyLinkedList.Node) *ObjListItem {
        return @alignCast(@constCast(@fieldParentPtr("node", node)));
    }

    pub inline fn clone(self: *ObjList, runtime_allocator: *memory.RuntimeAllocator) !*ObjList {
        const new_list = try runtime_allocator.allocateList();
        new_list.data.first = self.data.first;
        return new_list;
    }

    pub fn cloneAndPrepend(self: *ObjList, runtime_allocator: *memory.RuntimeAllocator, val: value.Value) !*Obj {
        const new_list = try self.clone(runtime_allocator);
        try new_list.prepend(runtime_allocator, val);
        return &new_list.obj;
    }

    pub fn prepend(self: *ObjList, runtime_allocator: *memory.RuntimeAllocator, val: value.Value) !void {
        const item = try runtime_allocator.allocateListItem();
        item.value = val;
        self.data.prepend(&item.node);
    }

    pub fn cloneAndPopFirst(self: *ObjList, runtime_allocator: *memory.RuntimeAllocator) !struct { value: ?value.Value, new_list: *ObjList } {
        if (self.data.first == null) {
            return .{ .value = null, .new_list = self };
        }
        const new_list = try self.clone(runtime_allocator);
        return .{ .value = new_list.popFirst().?, .new_list = new_list };
    }

    pub fn popFirst(self: *ObjList) ?value.Value {
        if (self.data.popFirst()) |node| {
            return getValueFromNode(node);
        }
        return null;
    }

    pub fn len(self: *ObjList) usize {
        return self.data.len();
    }

    pub fn isEmpty(self: *ObjList) bool {
        return self.data.first == null;
    }

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("<List>", .{});
    }

    const ListIterator = struct {
        current: ?*const std.SinglyLinkedList.Node,

        pub fn next(self: *ListIterator) ?*ObjListItem {
            if (self.current) |node| {
                self.current = node.next;
                return getItemFromNode(node);
            }
            return null;
        }
    };

    pub fn iterator(self: *ObjList) ListIterator {
        return .{ .current = self.data.first };
    }
};

pub const ObjListItem = struct {
    pub const KIND = ObjType.ListItem;

    obj: Obj = undefined,
    node: std.SinglyLinkedList.Node,
    value: value.Value,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("<ListItem>", .{});
    }
};

pub const ObjRecord = struct {
    pub const KIND = ObjType.Record;

    obj: Obj = undefined,
    fields: []value.Value,

    pub fn format(self: @This(), writer: *std.Io.Writer) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("<ListItem>", .{});
    }
};
