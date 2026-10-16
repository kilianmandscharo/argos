const std = @import("std");
const scanner = @import("scanner.zig");

pub const Expression = struct {
    token: scanner.Token,
    data: ExpressionData,

    pub fn init(data: ExpressionData, token: scanner.Token) @This() {
        return .{
            .data = data,
            .token = token,
        };
    }
};

pub const Pattern = union(enum) {
    // value -> checks against value
    // identifier -> binds new variable
    // head :: tail
    // []
    // [a, b]
    // _
};

pub const ExpressionData = union(enum) {
    Declaration: Declaration,
    Grouping: Grouping,
    Identifier: []const u8,
    String: []const u8,
    Integer: i64,
    Float: f64,
    Boolean: bool,
    Unary: Unary,
    Binary: Binary,
    Function: Function,
    Call: Call,
    List: std.ArrayList(*const Expression),
    Match: Match,
    If: If,
    Unit,
};

pub const Declaration = struct {
    // for now no pattern matching on assignment
    left: scanner.Token,
    right: *const Expression,
};

const Grouping = std.ArrayList(*const Expression);

const Binary = struct {
    operator: scanner.TokenType,
    left: *const Expression,
    right: *const Expression,
};

const Unary = struct {
    operator: scanner.TokenType,
    right: *const Expression,
};

const Function = struct {
    params: std.ArrayList(FunctionParam),
    body: *const Expression,
    name: ?scanner.Token = null,
};

pub const FunctionParam = union(enum) {
    Positional: scanner.Token,
    Default: FunctionParamDefault,
};

const FunctionParamDefault = struct {
    name: scanner.Token,
    value: *const Expression,
};

const Call = struct {
    function: *const Expression,
    args: std.ArrayList(*const Expression),
};

const Match = struct {
    target: *const Expression,
    arms: std.ArrayList(MatchArm),
};

pub const MatchArm = struct {
    pattern: *const Expression,
    expression: *const Expression,
};

pub const If = struct {
    arms: std.ArrayList(IfArm),
    alt: ?*const Expression,
};

pub const IfArm = struct {
    condition: *const Expression,
    then: *const Expression,
};

fn printIndent(writer: *std.Io.Writer, level: usize) !void {
    for (0..level * 2) |_| try writer.writeByte(' ');
}

pub fn printExpression(expr: *const Expression, writer: *std.Io.Writer) anyerror!void {
    return printExpressionInner(expr, writer, 0);
}

fn printExpressionInner(expr: *const Expression, writer: *std.Io.Writer, level: usize) anyerror!void {
    switch (expr.data) {
        .Identifier => |name| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("{s}", .{name});
        },
        .String => |s| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("\"{s}\"", .{s});
        },
        .Integer => |n| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("{d}", .{n});
        },
        .Float => |f| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("{d}", .{f});
        },
        .Boolean => |b| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("{}", .{b});
        },
        .Unit => {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("()", .{});
        },
        .Binary => |e| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("Binary:\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("{s}", .{e.operator.toString()});
            try printExpressionInner(e.left, writer, level + 1);
            try printExpressionInner(e.right, writer, level + 1);
        },
        .Unary => |e| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("Unary:\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("{s}", .{e.operator.toString()});
            try printExpressionInner(e.right, writer, level + 1);
        },
        .Declaration => |e| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("Declare {s}:", .{e.left.data});
            try printExpressionInner(e.right, writer, level + 1);
        },
        .Function => |e| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("Function", .{});
            if (e.name) |token| {
                try writer.print(" {s}:", .{token.data});
            } else {
                try writer.print(" <anonymous>:", .{});
            }
            if (e.params.items.len > 0) {
                try writer.print("\n", .{});
                try printIndent(writer, level + 1);
            }
            for (e.params.items) |param| {
                switch (param) {
                    .Positional => |name| {
                        try writer.print("{s} ", .{name.data});
                    },
                    .Default => @panic("not implemented"),
                }
            }
            try printExpressionInner(e.body, writer, level + 1);
        },
        .Grouping => |e| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("Grouping:", .{});
            for (e.items) |item| {
                try printExpressionInner(item, writer, level + 1);
            }
        },
        .Call => |e| {
            try writer.print("\n", .{});
            try printIndent(writer, level);
            try writer.print("Call:", .{});
            try printExpressionInner(e.function, writer, level + 1);
            for (e.args.items) |arg| {
                try printExpressionInner(arg, writer, level + 1);
            }
        },
        else => @panic("not implemented"),
    }
}
