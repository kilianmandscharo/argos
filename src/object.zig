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

pub fn allocateString(vm: *VirtualMachine, chars: []const u8) !*Obj {
    const object = try allocateObject(vm, ObjString);
    object.chars = chars;
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
                gpa.free(string_obj.chars);
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

    obj: Obj,
    chars: []const u8,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        try writer.print("\"{s}\"", .{self.chars});
    }
};
