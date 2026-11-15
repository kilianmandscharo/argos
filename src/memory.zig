const std = @import("std");
const logging = @import("logging.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const virtual_machine = @import("vm.zig");
const compiler = @import("compiler.zig");
const constants = @import("constants.zig");

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "Memory",
    });
}

// TODO: actually enable gc, but only after compilation

pub const RuntimeAllocator = struct {
    backing: std.mem.Allocator,
    bytes_allocated: usize,
    next_gc: usize,
    gray_stack: std.ArrayList(*object.Obj),
    objects: ?*object.Obj,
    vm: *virtual_machine.VirtualMachine,
    should_collect: bool,

    pub fn init(gpa: std.mem.Allocator, vm: *virtual_machine.VirtualMachine) RuntimeAllocator {
        return .{
            .backing = gpa,
            .bytes_allocated = 0,
            .next_gc = 1024 * 1024,
            .gray_stack = .empty,
            .objects = null,
            .vm = vm,
            .should_collect = false,
        };
    }

    pub fn deinit(self: *RuntimeAllocator) void {
        var current = self.objects;
        const gc_alloc = self.allocator();
        while (current) |obj| {
            const next = obj.next;
            obj.deinit(gc_alloc);
            current = next;
        }
        self.gray_stack.deinit(self.backing);
    }

    pub fn allocator(self: *RuntimeAllocator) std.mem.Allocator {
        return .{
            .ptr = self,
            .vtable = &.{ .alloc = alloc, .resize = resize, .remap = remap, .free = free },
        };
    }

    fn account(self: *RuntimeAllocator, old_len: usize, new_len: usize) void {
        if (new_len >= old_len) {
            self.bytes_allocated += new_len - old_len;
            if (self.bytes_allocated > self.next_gc) self.should_collect = true;
        } else {
            self.bytes_allocated -= old_len - new_len;
        }
        if (comptime constants.debug_stress_gc) self.should_collect = true;
    }

    fn alloc(ctx: *anyopaque, len: usize, alignment: std.mem.Alignment, ret_addr: usize) ?[*]u8 {
        const self: *RuntimeAllocator = @ptrCast(@alignCast(ctx));
        const p = self.backing.rawAlloc(len, alignment, ret_addr) orelse return null;
        self.account(0, len);
        return p;
    }

    fn resize(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) bool {
        const self: *RuntimeAllocator = @ptrCast(@alignCast(ctx));
        if (!self.backing.rawResize(memory, alignment, new_len, ret_addr)) return false;
        self.account(memory.len, new_len);
        return true;
    }

    fn remap(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, new_len: usize, ret_addr: usize) ?[*]u8 {
        const self: *RuntimeAllocator = @ptrCast(@alignCast(ctx));
        const p = self.backing.rawRemap(memory, alignment, new_len, ret_addr) orelse return null;
        self.account(memory.len, new_len);
        return p;
    }

    fn free(ctx: *anyopaque, memory: []u8, alignment: std.mem.Alignment, ret_addr: usize) void {
        const self: *RuntimeAllocator = @ptrCast(@alignCast(ctx));
        self.backing.rawFree(memory, alignment, ret_addr);
        self.account(memory.len, 0);
    }

    pub fn allocateObject(self: *RuntimeAllocator, T: type) !*T {
        const o = try self.allocator().create(T);

        o.obj = .{
            .type = T.KIND,
            .is_marked = false,
            .next = self.objects,
        };

        self.objects = &o.obj;

        if (comptime constants.debug_log_gc) {
            logDebug("0x{x} allocate {d} for {s}", .{ @intFromPtr(o), @sizeOf(T), @typeName(T) });
        }

        return o;
    }

    pub fn allocateList(self: *RuntimeAllocator) !*object.ObjList {
        const list = try self.allocateObject(object.ObjList);
        list.data = .{};
        return list;
    }

    pub fn allocateListItem(self: *RuntimeAllocator) !*object.ObjListItem {
        const list_item = try self.allocateObject(object.ObjListItem);
        list_item.value = undefined;
        list_item.node = .{};
        return list_item;
    }

    pub fn allocateUpvalue(self: *RuntimeAllocator, local: *value.Value) !*object.ObjUpvalue {
        const upvalue = try self.allocateObject(object.ObjUpvalue);
        upvalue.location = local;
        upvalue.next = null;
        upvalue.closed = null;
        return upvalue;
    }

    pub fn allocateNative(self: *RuntimeAllocator, function: object.NativeFn) !*object.Obj {
        const native = try self.allocateObject(object.ObjNative);
        native.function = function;
        return &native.obj;
    }

    pub fn allocateFunction(self: *RuntimeAllocator) !*object.ObjFunction {
        const function = try self.allocateObject(object.ObjFunction);
        function.arity = 0;
        function.name = null;
        function.chunk = .init();
        function.upvalue_count = 0;
        return function;
    }

    pub fn allocateClosure(self: *RuntimeAllocator, function: *object.ObjFunction) !*object.ObjClosure {
        const closure = try self.allocateObject(object.ObjClosure);
        closure.function = function;
        closure.upvalues = undefined;
        return closure;
    }

    pub fn allocateString(self: *RuntimeAllocator, chars: []const u8, hash: u64) !*object.Obj {
        const string = try self.allocateObject(object.ObjString);
        string.chars = chars;
        string.hash = hash;
        try self.vm.putString(string);
        return &string.obj;
    }

    pub fn copyString(self: *RuntimeAllocator, chars: []const u8) !*object.Obj {
        const hash = std.hash.Wyhash.hash(0, chars);
        const interned = self.vm.findString(chars, hash);
        const obj = interned orelse try self.allocateString(try self.allocator().dupe(u8, chars), hash);
        return obj;
    }

    pub fn collectGarbage(self: *RuntimeAllocator) !void {
        if (comptime constants.debug_log_gc) {
            logDebug("-- gc begin", .{});
        }

        const before = self.bytes_allocated;

        try self.markRoots();
        try self.traceReferences();
        try self.tableRemoveWhite();
        self.sweep();

        self.next_gc = self.bytes_allocated * constants.gc_heap_grow_factor;

        if (comptime constants.debug_log_gc) {
            logDebug("-- gc end", .{});
            logDebug("collected {d} bytes (from {d} to {d} next at {d})", .{
                before - self.bytes_allocated, before, self.bytes_allocated, self.next_gc,
            });
        }
    }

    fn sweep(self: *RuntimeAllocator) void {
        var previous: ?*object.Obj = null;
        var current = self.objects;
        while (current) |obj| {
            if (obj.is_marked) {
                obj.is_marked = false;
                previous = obj;
                current = obj.next;
            } else {
                const unreached = obj;
                current = obj.next;
                if (previous) |prev| {
                    prev.next = current;
                } else {
                    self.objects = current;
                }
                unreached.deinit(self);
            }
        }
    }

    fn tableRemoveWhite(self: *RuntimeAllocator) void {
        var it = self.vm.strings.keyIterator();
        while (it.next()) |key| {
            if (!key.obj.is_marked) {
                _ = self.vm.strings.remove(key.*);
            }
        }
    }

    fn traceReferences(self: *RuntimeAllocator) !void {
        while (self.gray_stack.pop()) |obj| try self.blackenObj(obj);
    }

    fn blackenObj(self: *RuntimeAllocator, obj: *object.Obj) !void {
        if (comptime constants.debug_log_gc) {
            logDebug("0x{x} blacken {f}", .{ @intFromPtr(obj), obj });
        }

        switch (obj.type) {
            .Upvalue => {
                if (obj.asUpvalue().closed) |val| {
                    try self.markValue(val);
                }
            },
            .Function => {
                const function = obj.asFunction();
                if (function.name) |name| {
                    try self.markObject(&name.obj);
                }
                try self.markValueArray(function.chunk.constants.items);
            },
            .Closure => {
                const closure = obj.asClosure();
                try self.markObject(&closure.function.obj);
                for (closure.upvalues) |upvalue| {
                    try self.markObject(&upvalue.obj);
                }
            },
            .List => {
                var it = obj.asList().iterator();
                while (it.next()) |item| {
                    try self.markObject(&item.obj);
                }
            },
            .ListItem => {
                const item = obj.asListItem();
                try self.markValue(item.value);
            },
            .String, .NativeFn => {},
        }
    }

    fn markValueArray(self: *RuntimeAllocator, array: []value.Value) !void {
        for (array) |val| try self.markValue(val);
    }

    fn markRoots(self: *RuntimeAllocator) !void {
        for (0..self.vm.stack_top) |i| try self.markValue(self.vm.stack[i]);
        for (0..self.vm.frame_count) |i| try self.markObject(&self.vm.frames[i].function.obj);
        var current = self.vm.open_upvalues;
        while (current) |val| {
            try self.markObject(&val.obj);
            current = val.next;
        }
    }

    fn markValue(self: *RuntimeAllocator, val: value.Value) !void {
        if (val.isObj()) try self.markObject(val.Obj);
    }

    pub fn markObject(self: *RuntimeAllocator, obj: *object.Obj) !void {
        if (obj.is_marked) return;
        if (comptime constants.debug_log_gc) {
            logDebug("0x{x} mark {s}", .{ @intFromPtr(obj), obj.getType() });
        }
        obj.is_marked = true;
        try self.gray_stack.append(self.backing, obj);
    }
};
