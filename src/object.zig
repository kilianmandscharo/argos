const std = @import("std");
const vm_module = @import("vm.zig");
const chunk_module = @import("chunk.zig");
const value_module = @import("value.zig");

const VirtualMachine = vm_module.VirtualMachine;
const Chunk = chunk_module.Chunk;
const Value = value_module.Value;

pub const ObjType = enum {
    String,
    Function,
    NativeFn,
    Closure,
    Upvalue,
};

pub fn allocateObject(vm: *VirtualMachine, T: type) !*T {
    if (!@hasField(T, "obj")) {
        @compileError("Object type must have field 'obj'");
    }

    const object = try vm.gpa.create(T);

    object.obj = .{
        .type = T.KIND,
        .next = vm.objects,
    };

    vm.objects = &object.obj;

    return object;
}

pub fn allocateUpvalue(vm: *VirtualMachine, slot: *Value) !*ObjUpvalue {
    const upvalue = try allocateObject(vm, ObjUpvalue);
    upvalue.location = slot;
    return upvalue;
}

pub fn allocateNative(vm: *VirtualMachine, function: NativeFn) !*Obj {
    const native = try allocateObject(vm, ObjNative);
    native.function = function;
    return &native.obj;
}

pub fn allocateFunction(vm: *VirtualMachine) !*ObjFunction {
    const function = try allocateObject(vm, ObjFunction);
    function.arity = 0;
    function.name = null;
    function.chunk = Chunk.init();
    function.upvalue_count = 0;
    return function;
}

pub fn allocateClosure(vm: *VirtualMachine, function: *ObjFunction) !*ObjClosure {
    const closure = try allocateObject(vm, ObjClosure);
    const upvalues = try vm.gpa.alloc(?*ObjUpvalue, function.upvalue_count);
    @memset(upvalues, null);
    closure.function = function;
    closure.upvalues = upvalues;
    return closure;
}

pub fn allocateString(vm: *VirtualMachine, chars: []const u8, hash: u64) !*Obj {
    return allocateStringInternal(vm, chars, hash, false);
}

pub fn allocateStaticString(vm: *VirtualMachine, chars: []const u8, hash: u64) !*Obj {
    return allocateStringInternal(vm, chars, hash, true);
}

pub fn copyStaticString(vm: *VirtualMachine, chars: []const u8) !*Obj {
    const hash = std.hash.Wyhash.hash(0, chars);
    const interned = vm.findString(chars, hash);
    const obj = interned orelse try allocateStaticString(vm, chars, hash);
    return obj;
}

fn allocateStringInternal(vm: *VirtualMachine, chars: []const u8, hash: u64, static_lifetime: bool) !*Obj {
    const object = try allocateObject(vm, ObjString);
    object.chars = chars;
    object.hash = hash;
    object.static_lifetime = static_lifetime;
    try vm.strings.put(vm.gpa, object, undefined);
    return &object.obj;
}

pub const Obj = struct {
    type: ObjType,
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

    pub fn deinit(self: *@This(), gpa: std.mem.Allocator) void {
        switch (self.type) {
            .String => {
                const string_obj = self.asString();
                if (!string_obj.static_lifetime) {
                    gpa.free(string_obj.chars);
                }
                gpa.destroy(string_obj);
            },
            .Function => {
                const function_obj = self.asFunction();
                function_obj.chunk.deinit(gpa);
                gpa.destroy(function_obj);
            },
            .NativeFn => {
                const native_obj = self.asNative();
                gpa.destroy(native_obj);
            },
            .Closure => {
                const closure_obj = self.asClosure();
                gpa.free(closure_obj.upvalues);
                gpa.destroy(closure_obj);
            },
            .Upvalue => {
                const upvalue_obj = self.asUpvalue();
                gpa.destroy(upvalue_obj);
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
    chunk: Chunk,
    name: ?*ObjString,
    upvalue_count: u8,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("<fn {s}>", .{if (self.name) |name| name.chars else "script"});
    }
};

pub const NativeFn = *const fn (arg_count: usize, args: []Value) Value;

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
    location: *Value,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        _ = self;
        try writer.print("upvalue", .{});
    }
};
