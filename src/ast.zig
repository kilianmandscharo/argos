const std = @import("std");
const scanner = @import("scanner.zig");

pub fn printProgram(program: Program, writer: anytype) !void {
    for (program.items) |stmt| {
        try printStatement(stmt, writer, 0);
    }
}

fn printIndent(writer: anytype, level: usize) !void {
    for (0..level * 2) |_| try writer.writeByte(' ');
}

fn printStatement(stmt: Statement, writer: anytype, level: usize) anyerror!void {
    switch (stmt) {
        .VarDeclaration => |s| {
            try printIndent(writer, level);
            try writer.print("VarDeclaration\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("name:\n", .{});
            try printIndent(writer, level + 2);
            try writer.print("{s}\n", .{s.name.data});
            try printIndent(writer, level + 1);
            try writer.print("expression:\n", .{});
            try printExpression(s.expression.data, writer, level + 2);
        },
        .Assignment => |s| {
            try printIndent(writer, level);
            try writer.print("Assignment\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("target:\n", .{});
            switch (s.target) {
                .Identifier => |name| {
                    try printIndent(writer, level + 2);
                    try writer.print("Identifier({s})\n", .{name.data});
                },
                .Index => |idx| {
                    try printIndent(writer, level + 2);
                    try writer.print("Index\n", .{});
                    try printIndent(writer, level + 3);
                    try writer.print("left:\n", .{});
                    try printExpression(idx.left.data, writer, level + 4);
                    try printIndent(writer, level + 3);
                    try writer.print("index:\n", .{});
                    try printExpression(idx.index.data, writer, level + 4);
                },
            }
            try printIndent(writer, level + 1);
            try writer.print("expression:\n", .{});
            try printExpression(s.expression.data, writer, level + 2);
        },
        .For => |s| {
            try printIndent(writer, level);
            try writer.print("For\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("expression:\n", .{});
            try printExpression(s.expression.data, writer, level + 2);
            try printIndent(writer, level + 1);
            try writer.print("capture:\n", .{});
            try printIndent(writer, level + 2);
            try writer.print("{s}\n", .{s.capture.data});
            if (s.index) |idx| {
                try printIndent(writer, level + 1);
                try writer.print("index:\n", .{});
                try printIndent(writer, level + 2);
                try writer.print("{s}\n", .{idx.data});
            }
            try printIndent(writer, level + 1);
            try writer.print("body:\n", .{});
            try printStatement(s.body.*, writer, level + 2);
        },
        .While => |s| {
            try printIndent(writer, level);
            try writer.print("While\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("expression:\n", .{});
            try printExpression(s.expression.data, writer, level + 2);
            try printIndent(writer, level + 1);
            try writer.print("body:\n", .{});
            try printStatement(s.body.*, writer, level + 2);
        },
        .Return => |expr| {
            try printIndent(writer, level);
            try writer.print("Return\n", .{});
            try printExpression(expr.data, writer, level + 1);
        },
        .Expression => |expr| {
            try printIndent(writer, level);
            try writer.print("Expression\n", .{});
            try printExpression(expr.data, writer, level + 1);
        },
    }
}

fn printExpression(expr: ExpressionData, writer: anytype, level: usize) !void {
    switch (expr) {
        .Identifier => |name| {
            try printIndent(writer, level);
            try writer.print("Identifier({s})\n", .{name});
        },
        .String => |s| {
            try printIndent(writer, level);
            try writer.print("String({s})\n", .{s});
        },
        .Integer => |n| {
            try printIndent(writer, level);
            try writer.print("Integer({d})\n", .{n});
        },
        .Float => |f| {
            try printIndent(writer, level);
            try writer.print("Float({d})\n", .{f});
        },
        .Boolean => |b| {
            try printIndent(writer, level);
            try writer.print("Boolean({})\n", .{b});
        },
        .Null => {
            try printIndent(writer, level);
            try writer.print("Null\n", .{});
        },
        .Infix => |e| {
            try printIndent(writer, level);
            try writer.print("Infix\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("operator:\n", .{});
            try printIndent(writer, level + 2);
            try writer.print("{s}\n", .{e.operator.toString()});
            try printIndent(writer, level + 1);
            try writer.print("left:\n", .{});
            try printExpression(e.left.data, writer, level + 2);
            try printIndent(writer, level + 1);
            try writer.print("right:\n", .{});
            try printExpression(e.right.data, writer, level + 2);
        },
        .Prefix => |e| {
            try printIndent(writer, level);
            try writer.print("Prefix\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("operator:\n", .{});
            try printIndent(writer, level + 2);
            try writer.print("{s}\n", .{e.operator.toString()});
            try printIndent(writer, level + 1);
            try writer.print("expression:\n", .{});
            try printExpression(e.expression.data, writer, level + 2);
        },
        .Function => |e| {
            try printIndent(writer, level);
            try writer.print("Function\n", .{});
            if (e.name) |name| {
                try printIndent(writer, level + 1);
                try writer.print("name:\n", .{});
                try printIndent(writer, level + 2);
                try writer.print("{s}\n", .{name});
            }
            try printIndent(writer, level + 1);
            try writer.print("params:\n", .{});
            for (e.params.items) |param| {
                switch (param) {
                    .Positional => |name| {
                        try printIndent(writer, level + 2);
                        try writer.print("Positional({s})\n", .{name.data});
                    },
                    .Default => |d| {
                        try printIndent(writer, level + 2);
                        try writer.print("Default\n", .{});
                        try printIndent(writer, level + 3);
                        try writer.print("name:\n", .{});
                        try printIndent(writer, level + 4);
                        try writer.print("{s}\n", .{d.name.data});
                        try printIndent(writer, level + 3);
                        try writer.print("value:\n", .{});
                        try printExpression(d.value.data, writer, level + 4);
                    },
                }
            }
            try printIndent(writer, level + 1);
            try writer.print("body:\n", .{});
            try printStatement(e.body.*, writer, level + 2);
        },
        .Call => |e| {
            try printIndent(writer, level);
            try writer.print("Call\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("function:\n", .{});
            try printExpression(e.function.data, writer, level + 2);
            try printIndent(writer, level + 1);
            try writer.print("args:\n", .{});
            for (e.args.items) |arg| {
                switch (arg) {
                    .Positional => |a| {
                        try printIndent(writer, level + 2);
                        try writer.print("Positional\n", .{});
                        try printExpression(a.data, writer, level + 3);
                    },
                    .Named => |a| {
                        try printIndent(writer, level + 2);
                        try writer.print("Named\n", .{});
                        try printIndent(writer, level + 3);
                        try writer.print("name:\n", .{});
                        try printIndent(writer, level + 4);
                        try writer.print("{s}\n", .{a.name.data});
                        try printIndent(writer, level + 3);
                        try writer.print("value:\n", .{});
                        try printExpression(a.value.data, writer, level + 4);
                    },
                }
            }
        },
        .Range => |e| {
            try printIndent(writer, level);
            try writer.print("Range\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("start:\n", .{});
            try printExpression(e.start.data, writer, level + 2);
            try printIndent(writer, level + 1);
            try writer.print("end:\n", .{});
            try printExpression(e.end.data, writer, level + 2);
        },
        .List => |items| {
            try printIndent(writer, level);
            try writer.print("List\n", .{});
            for (items.items) |item| {
                try printExpression(item.data, writer, level + 1);
            }
        },
        .Table => |pairs| {
            try printIndent(writer, level);
            try writer.print("Table\n", .{});
            for (pairs.items) |pair| {
                try printIndent(writer, level + 1);
                try writer.print("Pair\n", .{});
                try printIndent(writer, level + 2);
                try writer.print("key:\n", .{});
                try printExpression(pair.key.data, writer, level + 3);
                try printIndent(writer, level + 2);
                try writer.print("value:\n", .{});
                try printExpression(pair.value.data, writer, level + 3);
            }
        },
        .Index => |e| {
            try printIndent(writer, level);
            try writer.print("Index\n", .{});
            try printIndent(writer, level + 1);
            try writer.print("left:\n", .{});
            try printExpression(e.left.data, writer, level + 2);
            try printIndent(writer, level + 1);
            try writer.print("index:\n", .{});
            try printExpression(e.index.data, writer, level + 2);
        },
        .Match => |e| {
            try printIndent(writer, level);
            try writer.print("Match\n", .{});
            if (e.target) |target| {
                try printIndent(writer, level + 1);
                try writer.print("target:\n", .{});
                try printExpression(target.data, writer, level + 2);
            }
            try printIndent(writer, level + 1);
            try writer.print("body:\n", .{});
            switch (e.body) {
                .Single => |arm| {
                    try printIndent(writer, level + 2);
                    try writer.print("Single\n", .{});
                    try printIndent(writer, level + 3);
                    try writer.print("pattern:\n", .{});
                    try printExpression(arm.pattern.data, writer, level + 4);
                    try printIndent(writer, level + 3);
                    try writer.print("body:\n", .{});
                    try printStatement(arm.body, writer, level + 4);
                },
                .Multiple => |arms| {
                    try printIndent(writer, level + 2);
                    try writer.print("Multiple\n", .{});
                    for (arms.items) |arm| {
                        try printIndent(writer, level + 3);
                        try writer.print("Arm\n", .{});
                        try printIndent(writer, level + 4);
                        try writer.print("pattern:\n", .{});
                        try printExpression(arm.pattern.data, writer, level + 5);
                        try printIndent(writer, level + 4);
                        try writer.print("body:\n", .{});
                        try printStatement(arm.body, writer, level + 5);
                    }
                },
            }
        },
        .Block => |stmts| {
            try printIndent(writer, level);
            try writer.print("Block\n", .{});
            for (stmts.items) |s| {
                try printStatement(s, writer, level + 1);
            }
        },
    }
}

pub const Program = std.ArrayList(Statement);

pub const Statement = union(enum) {
    VarDeclaration: VarDeclaration,
    Assignment: Assignment,
    For: For,
    While: While,
    Return: *const Expression,
    Expression: *const Expression,
};

const VarDeclaration = struct {
    name: scanner.Token,
    expression: *const Expression,
};

const Assignment = struct {
    target: AssignTarget,
    expression: *const Expression,
};

pub const AssignTarget = union(enum) {
    Identifier: scanner.Token,
    Index: Index,
};

const For = struct {
    expression: *const Expression,
    capture: scanner.Token,
    index: ?scanner.Token,
    body: *const Statement,
};

const While = struct {
    expression: *const Expression,
    body: *const Statement,
};

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

pub const ExpressionData = union(enum) {
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
    Block: Block,
    Null,
};

const Block = std.ArrayList(Statement);

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
    body: *const Statement,
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
    args: std.ArrayList(FunctionArg),
};

pub const FunctionArg = union(enum) {
    Positional: *const Expression,
    Named: FunctionArgNamed,
};

const FunctionArgNamed = struct {
    name: scanner.Token,
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
