const std = @import("std");
const vm_module = @import("vm.zig");

const VirtualMachine = vm_module.VirtualMachine;

pub const ObjType = enum {
    String,
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

    pub fn format(
        self: *@This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self.type) {
            .String => {
                const string = self.asString();
                try string.format(writer);
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
        }
    }

    pub fn asString(self: *@This()) *ObjString {
        return @alignCast(@fieldParentPtr("obj", self));
    }

    pub fn isString(self: *@This()) bool {
        return self.type == .String;
    }
};

pub const ObjString = struct {
    pub const KIND = ObjType.String;

    obj: Obj = undefined,
    chars: []const u8,
    static_lifetime: bool = false,
    hash: u64,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("\"{s}\"", .{self.chars});
    }
};
