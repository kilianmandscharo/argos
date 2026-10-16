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

fn operator(op: scanner.TokenType) []const u8 {
    return switch (op) {
        .Bang => "!",
        .Lt => "<",
        .LtOrEq => "<=",
        .Gt => ">",
        .GtOrEq => ">=",
        .Eq => "==",
        .NotEq => "!=",
        .Plus => "+",
        .Minus => "-",
        .Slash => "/",
        .Asterisk => "*",
        .Percent => "%",
        .And => "and",
        .Or => "or",
        .Pipe => "|",
        .Ampersand => "~",
        .Caret => "^",
        .Tilde => "~",
        .LeftShift => "<<",
        .RightShift => ">>",
        else => "<unknown operator>",
    };
}

pub fn printExpression(expr: *const Expression, writer: *std.Io.Writer) anyerror!void {
    return printExpressionInner(expr, writer, 0);
}

fn printExpressionInner(expr: *const Expression, writer: *std.Io.Writer, level: usize) anyerror!void {
    switch (expr.data) {
        .Identifier => |name| {
            try writer.print("{s}", .{name});
        },
        .String => |s| {
            try writer.print("\"{s}\"", .{s});
        },
        .Integer => |n| {
            try writer.print("{d}", .{n});
        },
        .Float => |f| {
            try writer.print("{d}", .{f});
        },
        .Boolean => |b| {
            try writer.print("{}", .{b});
        },
        .Unit => {
            try writer.print("()", .{});
        },
        .Binary => |e| {
            try printIndent(writer, level);
            try writer.print("(", .{});
            try printExpressionInner(e.left, writer, 0);
            try writer.print(" {s} ", .{operator(e.operator)});
            try printExpressionInner(e.right, writer, 0);
            try writer.print(")", .{});
        },
        .Unary => |e| {
            try printIndent(writer, level);
            try writer.print("({s}", .{operator(e.operator)});
            try printExpressionInner(e.right, writer, level + 1);
            try writer.print(")", .{});
        },
        .Declaration => |e| {
            try printIndent(writer, level);
            try writer.print("(Declare {s}\n", .{e.left.data});
            try printExpressionInner(e.right, writer, level + 1);
            try writer.print(")", .{});
        },
        .Function => |e| {
            try printIndent(writer, level);
            try writer.print("(Function", .{});
            if (e.name) |name| {
                try writer.print(" {s}", .{name});
            } else {
                try writer.print(" <anonymous>", .{});
            }
            for (e.params.items) |param| {
                switch (param) {
                    .Positional => |name| {
                        try writer.print(" {s}", .{name.data});
                    },
                    .Default => @panic("not implemented"),
                }
            }
            try writer.print("\n", .{});
            try printExpressionInner(e.body, writer, level + 1);
            try writer.print(")", .{});
        },
        .Grouping => |e| {
            try printIndent(writer, level);
            try writer.print("(", .{});
            for (e.items) |item| {
                try printExpressionInner(item, writer, 0);
            }
            try writer.print(")", .{});
        },
        .Call => |e| {
            try printIndent(writer, level);
            try writer.print("(Call ", .{});
            try printExpressionInner(e.function, writer, 0);
            for (e.args.items) |arg| {
                try printExpression(arg, writer);
            }
        },
        // .List => |items| {
        //     try printIndent(writer, level);
        //     try writer.print("List\n", .{});
        //     for (items.items) |item| {
        //         try printExpression(item.data, writer, level + 1);
        //     }
        // },
        // .Match => |e| {
        //     try printIndent(writer, level);
        //     try writer.print("Match\n", .{});
        //     if (e.target) |target| {
        //         try printIndent(writer, level + 1);
        //         try writer.print("target:\n", .{});
        //         try printExpression(target.data, writer, level + 2);
        //     }
        //     try printIndent(writer, level + 1);
        //     try writer.print("body:\n", .{});
        //     switch (e.body) {
        //         .Single => |arm| {
        //             try printIndent(writer, level + 2);
        //             try writer.print("Single\n", .{});
        //             try printIndent(writer, level + 3);
        //             try writer.print("pattern:\n", .{});
        //             try printExpression(arm.pattern.data, writer, level + 4);
        //             try printIndent(writer, level + 3);
        //             try writer.print("body:\n", .{});
        //             try printExpression(arm.body.data, writer, level + 4);
        //         },
        //         .Multiple => |arms| {
        //             try printIndent(writer, level + 2);
        //             try writer.print("Multiple\n", .{});
        //             for (arms.items) |arm| {
        //                 try printIndent(writer, level + 3);
        //                 try writer.print("Arm\n", .{});
        //                 try printIndent(writer, level + 4);
        //                 try writer.print("pattern:\n", .{});
        //                 try printExpression(arm.pattern.data, writer, level + 5);
        //                 try printIndent(writer, level + 4);
        //                 try writer.print("body:\n", .{});
        //                 try printExpression(arm.body.data, writer, level + 5);
        //             }
        //         },
        //     }
        // },
        else => @panic("not implemented"),
    }
}
