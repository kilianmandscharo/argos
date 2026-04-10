const std = @import("std");
const scanner = @import("scanner.zig");

pub fn printProgram(program: Program) void {
    for (program.items) |statement| {
        std.debug.print("{f}\n", .{statement});
    }
}

pub const Program = std.ArrayList(Statement);

pub const Statement = union(enum) {
    VarDeclaration: VarDeclaration,
    Block: Block,
    Assignment: Assignment,
    For: For,
    While: While,
    Return: *const Expression,
    Assert: *const Expression,
    Print: *const Expression,
    Expression: *const Expression,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .VarDeclaration => |val| try writer.print("VarDeclaration({s} = {f})", .{ val.name, val.expression }),
            .Block => try writer.print("Block", .{}),
            .Assignment => try writer.print("Assignment", .{}),
            .For => try writer.print("For", .{}),
            .While => try writer.print("While", .{}),
            .Return => try writer.print("Return", .{}),
            .Assert => try writer.print("Assert", .{}),
            .Print => try writer.print("Print", .{}),
            .Expression => |val| try writer.print("Expression {f}", .{val}),
        }
    }
};

const Block = std.ArrayList(Statement);

const VarDeclaration = struct {
    name: []const u8,
    expression: *const Expression,
};

const Assignment = struct {
    target: AssignTarget,
    expression: *const Expression,
};

pub const AssignTarget = union(enum) {
    Identifier: []const u8,
    Index: Index,
};

const For = struct {
    expression: *const Expression,
    capture: []const u8,
    index: ?[]const u8,
    body: Block,
};

const While = struct {
    expression: *const Expression,
    body: Block,
};

pub const Expression = union(enum) {
    Identifier: []const u8,
    String: []const u8,
    Integer: i64,
    Float: f64,
    Boolean: bool,
    Infix: Infix,
    Prefix: Prefix,
    Function: Function,
    Call: Call,
    Range: Range,
    List: std.ArrayList(*const Expression),
    Table: std.ArrayList(TablePair),
    Index: Index,
    Match: Match,
    Null,

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        switch (self) {
            .Identifier => |val| try writer.print("Identifier({s})", .{val}),
            .String => |val| try writer.print("String(\"{s}\")", .{val}),
            .Integer => |val| try writer.print("Integer({d})", .{val}),
            .Float => |val| try writer.print("Float({d})", .{val}),
            .Boolean => |val| try writer.print("Boolean({})", .{val}),
            .Infix => |val| try writer.print("Infix({f} {s} {f})", .{ val.left, val.operator.toString(), val.right }),
            .Prefix => |val| try writer.print("Prefix({s} {f})", .{ val.operator.toString(), val.expression }),
            .Function => |_| try writer.print("Fn", .{}),
            .Call => |_| try writer.print("Call", .{}),
            .Range => |_| try writer.print("Range", .{}),
            .List => |val| {
                try writer.print("List(", .{});
                for (val.items) |item| {
                    try writer.print("{f},", .{item});
                }
                try writer.print(")", .{});
            },
            .Table => |_| try writer.print("Table", .{}),
            .Index => |_| try writer.print("Index", .{}),
            .Match => |_| try writer.print("Match", .{}),
            .Null => try writer.print("Null", .{}),
        }
    }
};

const Infix = struct {
    operator: scanner.TokenType,
    left: *const Expression,
    right: *const Expression,
};

const Prefix = struct {
    operator: scanner.TokenType,
    expression: *const Expression,
};

const Function = struct {
    params: std.ArrayList(FunctionParam),
    body: FunctionBody,
    name: ?[]const u8 = null,
};

const FunctionBody = union(enum) {
    Block: Block,
    Expression: *const Expression,
};

pub const FunctionParam = union(enum) {
    Positional: []const u8,
    Default: FunctionParamDefault,
};

const FunctionParamDefault = struct {
    name: []const u8,
    value: *const Expression,
};

const Call = struct {
    function: *const Expression,
    args: std.ArrayList(FunctionArg),
};

pub const FunctionArg = union(enum) {
    Positional: *const Expression,
    Named: FunctionArgNamed,
};

const FunctionArgNamed = struct {
    name: []const u8,
    value: *const Expression,
};

const Range = struct {
    start: *const Expression,
    end: *const Expression,
};

pub const TablePair = struct {
    key: *const Expression,
    value: *const Expression,
};

pub const Index = struct {
    left: *const Expression,
    index: *const Expression,
};

const Match = struct {
    target: ?*const Expression,
    body: MatchBody,
};

const MatchBody = union(enum) {
    Single: MatchArm,
    Multiple: std.ArrayList(MatchArm),
};

pub const MatchArm = struct {
    pattern: *const Expression,
    body: Statement,
};
