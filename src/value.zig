const std = @import("std");
const object_module = @import("object.zig");

const Obj = object_module.Obj;
const ObjType = object_module.ObjType;

pub const Value = union(enum) {
    Float: f64,
    Int: i64,
    Bool: bool,
    Null,
    Obj: *Obj,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .Float => |val| try writer.print("{d}", .{val}),
            .Int => |val| try writer.print("{d}", .{val}),
            .Bool => |val| try writer.print("{}", .{val}),
            .Null => try writer.print("null", .{}),
            .Obj => |val| try val.format(writer),
        }
    }

    pub fn getType(self: @This()) []const u8 {
        return switch (self) {
            .Float => "Float",
            .Int => "Int",
            .Bool => "Bool",
            .Null => "Null",
            .Obj => "Object",
        };
    }

    pub inline fn isObjType(self: *@This(), expected_type: ObjType) bool {
        return self.* == .Obj and self.Obj.type == expected_type;
    }
};

pub inline fn wrapObj(val: *Obj) Value {
    return Value{ .Obj = val };
}

pub inline fn wrapInt(val: i64) Value {
    return Value{ .Int = val };
}

pub inline fn wrapFloat(val: f64) Value {
    return Value{ .Float = val };
}

pub inline fn wrapBool(val: bool) Value {
    return Value{ .Bool = val };
}

pub inline fn valueNull() Value {
    return .Null;
}
