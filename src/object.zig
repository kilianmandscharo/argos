const std = @import("std");
const virtual_machine = @import("vm.zig");
const chunk = @import("chunk.zig");
const value = @import("value.zig");
const memory = @import("memory.zig");
const logging = @import("logging.zig");

const DEBUG_STRESS_GC = true;
const DEBUG_LOG_GC = true;

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "Object",
    });
}

pub const ObjType = enum {
    String,
    Function,
    NativeFn,
    Closure,
    Upvalue,
};

pub fn allocateObject(vm: *virtual_machine.VirtualMachine, T: type) !*T {
    if (!@hasField(T, "obj")) {
        @compileError("Object type must have field 'obj'");
    }

    vm.bytes_allocated += @sizeOf(T);

    if (comptime DEBUG_STRESS_GC) {
        try memory.collectGarbage(vm);
    }

    if (vm.bytes_allocated > vm.next_gc) {
        try memory.collectGarbage(vm);
    }

    const object = try vm.gpa.create(T);

    object.obj = .{
        .type = T.KIND,
        .is_marked = false,
        .next = vm.objects,
    };

    vm.objects = &object.obj;

    if (comptime DEBUG_LOG_GC) {
        logDebug("0x{x} allocate {d} for {s}", .{ @intFromPtr(object), @sizeOf(T), @typeName(T) });
    }

    return object;
}

pub fn allocateUpvalue(vm: *virtual_machine.VirtualMachine, slot: *value.Value) !*ObjUpvalue {
    const upvalue = try allocateObject(vm, ObjUpvalue);
    upvalue.location = slot;
    upvalue.next = null;
    upvalue.closed = null;
    return upvalue;
}

pub fn allocateNative(vm: *virtual_machine.VirtualMachine, function: NativeFn) !*Obj {
    const native = try allocateObject(vm, ObjNative);
    native.function = function;
    return &native.obj;
}

pub fn allocateFunction(vm: *virtual_machine.VirtualMachine) !*ObjFunction {
    const function = try allocateObject(vm, ObjFunction);
    function.arity = 0;
    function.name = null;
    function.chunk = chunk.Chunk.init();
    function.upvalue_count = 0;
    return function;
}

pub fn allocateClosure(vm: *virtual_machine.VirtualMachine, function: *ObjFunction) !*ObjClosure {
    const closure = try allocateObject(vm, ObjClosure);
    const upvalues = try vm.gpa.alloc(?*ObjUpvalue, function.upvalue_count);
    @memset(upvalues, null);
    closure.function = function;
    closure.upvalues = upvalues;
    return closure;
}

pub fn allocateString(vm: *virtual_machine.VirtualMachine, chars: []const u8, hash: u64) !*Obj {
    return allocateStringInternal(vm, chars, hash, false);
}

pub fn allocateStaticString(vm: *virtual_machine.VirtualMachine, chars: []const u8, hash: u64) !*Obj {
    return allocateStringInternal(vm, chars, hash, true);
}

pub fn copyStaticString(vm: *virtual_machine.VirtualMachine, chars: []const u8) !*Obj {
    const hash = std.hash.Wyhash.hash(0, chars);
    const interned = vm.findString(chars, hash);
    const obj = interned orelse try allocateStaticString(vm, chars, hash);
    return obj;
}

fn allocateStringInternal(vm: *virtual_machine.VirtualMachine, chars: []const u8, hash: u64, static_lifetime: bool) !*Obj {
    const string = try allocateObject(vm, ObjString);
    string.chars = chars;
    string.hash = hash;
    string.static_lifetime = static_lifetime;
    // TODO: push string on to stack
    try vm.strings.put(vm.gpa, string, undefined);
    // TODO: pop string from stack
    return &string.obj;
}

pub const Obj = struct {
    type: ObjType,
    is_marked: bool,
    next: ?*Obj,

    pub fn getType(self: *@This()) []const u8 {
        return switch (self.type) {
            .Function => "Function",
            .NativeFn => "NativeFn",
            .String => "String",
            .Closure => "Closure",
            .Upvalue => "Upvalue",
        };
    }

    pub fn format(
        self: *@This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.type) {
            .String => {
                const string = self.asString();
                try string.format(writer);
            },
            .Function => {
                const function = self.asFunction();
                try function.format(writer);
            },
            .NativeFn => {
                const native = self.asFunction();
                try native.format(writer);
            },
            .Closure => {
                const closure = self.asClosure();
                try closure.format(writer);
            },
            .Upvalue => {
                const upvalue = self.asUpvalue();
                try upvalue.format(writer);
            },
        }
    }

    pub fn deinit(self: *@This(), vm: *virtual_machine.VirtualMachine) void {
        if (comptime DEBUG_LOG_GC) {
            logDebug("0x{x} free type {s}", .{ @intFromPtr(self), self.getType() });
        }

        const gpa = vm.gpa;

        switch (self.type) {
            .String => {
                const string_obj = self.asString();
                if (!string_obj.static_lifetime) {
                    gpa.free(string_obj.chars);
                }
                gpa.destroy(string_obj);
                vm.bytes_allocated -= @sizeOf(ObjString);
            },
            .Function => {
                const function_obj = self.asFunction();
                function_obj.chunk.deinit(gpa);
                gpa.destroy(function_obj);
                vm.bytes_allocated -= @sizeOf(ObjFunction);
            },
            .NativeFn => {
                const native_obj = self.asNative();
                gpa.destroy(native_obj);
                vm.bytes_allocated -= @sizeOf(ObjNative);
            },
            .Closure => {
                const closure_obj = self.asClosure();
                gpa.free(closure_obj.upvalues);
                gpa.destroy(closure_obj);
                vm.bytes_allocated -= @sizeOf(ObjClosure);
            },
            .Upvalue => {
                const upvalue_obj = self.asUpvalue();
                gpa.destroy(upvalue_obj);
                vm.bytes_allocated -= @sizeOf(ObjUpvalue);
            },
        }
    }

    pub fn asFunction(self: *@This()) *ObjFunction {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn asString(self: *@This()) *ObjString {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn asNative(self: *@This()) *ObjNative {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn asClosure(self: *@This()) *ObjClosure {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn asUpvalue(self: *@This()) *ObjUpvalue {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn isString(self: *@This()) bool {
        return self.type == .String;
    }

    pub fn isFunction(self: *@This()) bool {
        return self.type == .Function;
    }

    pub fn isNative(self: *@This()) bool {
        return self.type == .NativeFn;
    }

    pub fn isClosure(self: *@This()) bool {
        return self.type == .Closure;
    }

    pub fn isUpvalue(self: *@This()) bool {
        return self.type == .Upvalue;
    }
};

pub const ObjString = struct {
    pub const KIND = ObjType.String;

    obj: Obj = undefined,
    chars: []const u8,
    // TODO: allowing static strings means we need to keep the source code
    // around for the full runtime, do we want that?
    static_lifetime: bool = false,
    hash: u64,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
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

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("<fn {s}>", .{if (self.name) |name| name.chars else "script"});
    }
};

pub const NativeFn = *const fn (arg_count: usize, args: []value.Value) value.Value;

pub const ObjNative = struct {
    pub const KIND = ObjType.NativeFn;

    obj: Obj = undefined,
    function: NativeFn,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("<native fn>", .{});
    }
};

pub const ObjClosure = struct {
    pub const KIND = ObjType.Closure;

    obj: Obj = undefined,
    function: *ObjFunction,
    upvalues: []?*ObjUpvalue,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try self.function.format(writer);
    }
};

pub const ObjUpvalue = struct {
    pub const KIND = ObjType.Upvalue;

    obj: Obj = undefined,
    location: *value.Value,
    closed: ?value.Value,
    next: ?*ObjUpvalue,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("upvalue", .{});
    }
};
