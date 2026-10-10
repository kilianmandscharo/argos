const std = @import("std");
const scanner = @import("scanner.zig");

pub const Program = std.ArrayList(*const Expression);

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
    name: ?[]const u8 = null,
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

pub fn printProgram(program: Program, writer: *std.Io.Writer) !void {
    try writer.print("(Program\n", .{});
    for (program.items) |expr| {
        try printExpression(expr, writer, 0);
    }
    try writer.print(")", .{});
}

fn printIndent(writer: *std.Io.Writer, level: usize) !void {
    for (0..level * 2) |_| try writer.writeByte(' ');
}

fn printExpression(expr: *const Expression, writer: *std.Io.Writer, level: usize) !void {
    switch (expr.data) {
        // .Identifier => |name| {
        //     try printIndent(writer, level);
        //     try writer.print("Identifier({s})\n", .{name});
        // },
        // .String => |s| {
        //     try printIndent(writer, level);
        //     try writer.print("String({s})\n", .{s});
        // },
        // .Integer => |n| {
        //     try printIndent(writer, level);
        //     try writer.print("Integer({d})\n", .{n});
        // },
        // .Float => |f| {
        //     try printIndent(writer, level);
        //     try writer.print("Float({d})\n", .{f});
        // },
        // .Boolean => |b| {
        //     try printIndent(writer, level);
        //     try writer.print("Boolean({})\n", .{b});
        // },
        // .Null => {
        //     try printIndent(writer, level);
        //     try writer.print("Null\n", .{});
        // },
        .Binary => |e| {
            try printIndent(writer, level);
            try writer.print("(", .{});
            try printExpression(e.left, writer, level + 2);
            try writer.print(" {s} ", .{e.operator.toString()});
            try printExpression(e.right, writer, level + 2);
            try writer.print(")", .{});
        },
        // .Prefix => |e| {
        //     try printIndent(writer, level);
        //     try writer.print("Prefix\n", .{});
        //     try printIndent(writer, level + 1);
        //     try writer.print("operator:\n", .{});
        //     try printIndent(writer, level + 2);
        //     try writer.print("{s}\n", .{e.operator.toString()});
        //     try printIndent(writer, level + 1);
        //     try writer.print("expression:\n", .{});
        //     try printExpression(e.right.data, writer, level + 2);
        // },
        // .Function => |e| {
        //     try printIndent(writer, level);
        //     try writer.print("Function\n", .{});
        //     if (e.name) |name| {
        //         try printIndent(writer, level + 1);
        //         try writer.print("name:\n", .{});
        //         try printIndent(writer, level + 2);
        //         try writer.print("{s}\n", .{name});
        //     }
        //     try printIndent(writer, level + 1);
        //     try writer.print("params:\n", .{});
        //     for (e.params.items) |param| {
        //         switch (param) {
        //             .Positional => |name| {
        //                 try printIndent(writer, level + 2);
        //                 try writer.print("Positional({s})\n", .{name.data});
        //             },
        //             .Default => |d| {
        //                 try printIndent(writer, level + 2);
        //                 try writer.print("Default\n", .{});
        //                 try printIndent(writer, level + 3);
        //                 try writer.print("name:\n", .{});
        //                 try printIndent(writer, level + 4);
        //                 try writer.print("{s}\n", .{d.name.data});
        //                 try printIndent(writer, level + 3);
        //                 try writer.print("value:\n", .{});
        //                 try printExpression(d.value.data, writer, level + 4);
        //             },
        //         }
        //     }
        //     try printIndent(writer, level + 1);
        //     try writer.print("body:\n", .{});
        //     try printExpression(e.body.data, writer, level + 2);
        // },
        // .Call => |e| {
        //     try printIndent(writer, level);
        //     try writer.print("Call\n", .{});
        //     try printIndent(writer, level + 1);
        //     try writer.print("function:\n", .{});
        //     try printExpression(e.function.data, writer, level + 2);
        //     try printIndent(writer, level + 1);
        //     try writer.print("args:\n", .{});
        //     for (e.args.items) |arg| {
        //         switch (arg) {
        //             .Positional => |a| {
        //                 try printIndent(writer, level + 2);
        //                 try writer.print("Positional\n", .{});
        //                 try printExpression(a.data, writer, level + 3);
        //             },
        //             .Named => |a| {
        //                 try printIndent(writer, level + 2);
        //                 try writer.print("Named\n", .{});
        //                 try printIndent(writer, level + 3);
        //                 try writer.print("name:\n", .{});
        //                 try printIndent(writer, level + 4);
        //                 try writer.print("{s}\n", .{a.name.data});
        //                 try printIndent(writer, level + 3);
        //                 try writer.print("value:\n", .{});
        //                 try printExpression(a.value.data, writer, level + 4);
        //             },
        //         }
        //     }
        // },
        // .Range => |e| {
        //     try printIndent(writer, level);
        //     try writer.print("Range\n", .{});
        //     try printIndent(writer, level + 1);
        //     try writer.print("start:\n", .{});
        //     try printExpression(e.start.data, writer, level + 2);
        //     try printIndent(writer, level + 1);
        //     try writer.print("end:\n", .{});
        //     try printExpression(e.end.data, writer, level + 2);
        // },
        // .List => |items| {
        //     try printIndent(writer, level);
        //     try writer.print("List\n", .{});
        //     for (items.items) |item| {
        //         try printExpression(item.data, writer, level + 1);
        //     }
        // },
        // .Table => |pairs| {
        //     try printIndent(writer, level);
        //     try writer.print("Table\n", .{});
        //     for (pairs.items) |pair| {
        //         try printIndent(writer, level + 1);
        //         try writer.print("Pair\n", .{});
        //         try printIndent(writer, level + 2);
        //         try writer.print("key:\n", .{});
        //         try printExpression(pair.key.data, writer, level + 3);
        //         try printIndent(writer, level + 2);
        //         try writer.print("value:\n", .{});
        //         try printExpression(pair.value.data, writer, level + 3);
        //     }
        // },
        // .Index => |e| {
        //     try printIndent(writer, level);
        //     try writer.print("Index\n", .{});
        //     try printIndent(writer, level + 1);
        //     try writer.print("left:\n", .{});
        //     try printExpression(e.left.data, writer, level + 2);
        //     try printIndent(writer, level + 1);
        //     try writer.print("index:\n", .{});
        //     try printExpression(e.index.data, writer, level + 2);
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
        // .Block => |stmts| {
        //     try printIndent(writer, level);
        //     try writer.print("Block\n", .{});
        //     for (stmts.items) |s| {
        //         try printStatement(s, writer, level + 1);
        //     }
        // },
        else => {},
    }
}
