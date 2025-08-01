const std = @import("std");

// TODO: clean up the error handling in the evaluator

const parser_module = @import("parser.zig");
const Statement = parser_module.Statement;
const Expression = parser_module.Expression;
const Operator = parser_module.Operator;
const BlockStatement = parser_module.BlockStatement;

pub const Node = union(enum) {
    Program: std.ArrayList(Statement),
    Expression: Expression,
    Statement: Statement,
};

pub const Object = union(enum) {
    Integer: i64,
    Float: f64,
    String: []const u8,
    Boolean: bool,
    ReturnValue: usize,
    Error: []const u8,
    Function: Function,
    Null,

    fn isError(self: Object) bool {
        return switch (self) {
            .Error => true,
            else => false,
        };
    }

    fn getType(self: Object) []const u8 {
        return switch (self) {
            .Integer => "Integer",
            .Float => "Float",
            .String => "String",
            .Boolean => "Boolean",
            .ReturnValue => "ReturnValue",
            .Error => "Error",
            .Function => "Function",
            .Null => "Null",
        };
    }

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        switch (self) {
            .Integer => |v| try writer.print("{d}", .{v}),
            .Float => |v| try writer.print("{d}", .{v}),
            .String => |v| try writer.print("{s}", .{v}),
            .Boolean => |v| try writer.print("{}", .{v}),
            .ReturnValue => |v| try writer.print("{any}", .{v}),
            .Error => |v| try writer.print("{s}", .{v}),
            .Function => try writer.print("Function", .{}),
            .Null => try writer.print("Null", .{}),
        }
    }
};

const Function = struct {
    params: std.ArrayListUnmanaged([]const u8),
    body: BlockStatement,
    env: *Environment,
};

pub const ObjectStore = struct {
    gpa: std.mem.Allocator,
    objects: std.ArrayListUnmanaged(Object),

    pub fn init(gpa: std.mem.Allocator) ObjectStore {
        return .{
            .gpa = gpa,
            .objects = .{},
        };
    }

    pub fn add(self: *ObjectStore, object: Object) !usize {
        std.debug.print("add to object store: {any}\n", .{object});
        try self.objects.append(self.gpa, object);
        return self.objects.items.len - 1;
    }

    pub fn get(self: *ObjectStore, index: usize) Object {
        return self.objects.items[index];
    }
};

pub const Evaluator = struct {
    gpa: std.mem.Allocator,
    object_store: ObjectStore,

    pub fn init(gpa: std.mem.Allocator) Evaluator {
        const object_store = ObjectStore.init(gpa);
        return Evaluator{
            .gpa = gpa,
            .object_store = object_store,
        };
    }

    pub fn eval(self: *Evaluator, node: Node, env: *Environment) !Object {
        return switch (node) {
            .Program => |statements| {
                return try self.evalProgram(statements, env);
            },
            .Statement => |statement| {
                std.debug.print("eval statement {f}\n", .{statement});
                switch (statement) {
                    .ExpressionStatement => |expression_statement| {
                        return try self.eval(Node{ .Expression = expression_statement.expression.* }, env);
                    },
                    .AssignmentStatement => |assignment| {
                        const val = try self.eval(Node{ .Expression = assignment.expression.* }, env);
                        if (val.isError()) {
                            return val;
                        }
                        try env.set(assignment.identifier.literal, val);
                        return val;
                    },
                    .BlockStatement => |block| {
                        var result: Object = Object.Null;
                        for (block.statements.items) |item| {
                            result = try self.eval(Node{ .Statement = item }, env);
                            switch (result) {
                                .ReturnValue => |idx| return self.object_store.get(idx),
                                .Error => return result,
                                else => {},
                            }
                        }
                        return result;
                    },
                    .ReturnStatement => |return_statement| {
                        const value = try self.eval(Node{ .Expression = return_statement.expression.* }, env);
                        if (value.isError()) {
                            return value;
                        }
                        const object_id = try self.object_store.add(value);
                        const return_val = Object{ .ReturnValue = object_id };
                        return return_val;
                    },
                }
            },
            .Expression => |expression| {
                std.debug.print("eval expression {f}\n", .{expression});
                switch (expression) {
                    .Identifier => |identifier| {
                        const val = env.get(identifier) orelse self.createError("identifier '{s}' not found", .{identifier});
                        return val;
                    },
                    .IntegerLiteral => |integer| return Object{ .Integer = integer },
                    .FloatLiteral => |float| return Object{ .Float = float },
                    .BooleanLiteral => |boolean| return Object{ .Boolean = boolean },
                    .InfixExpression => |infix| {
                        const left = try self.eval(Node{ .Expression = infix.left.* }, env);
                        if (left.isError()) {
                            return left;
                        }
                        const right = try self.eval(Node{ .Expression = infix.right.* }, env);
                        if (right.isError()) {
                            return right;
                        }
                        return self.evalInfixExpression(left, right, infix.operator);
                    },
                    .PrefixExpression => |prefix| {
                        const right = try self.eval(Node{ .Expression = prefix.expression.* }, env);
                        return self.evalPrefixExpression(right, prefix.operator);
                    },
                    .FunctionLiteral => |function| {
                        const object = Object{ .Function = .{ .params = function.params, .body = function.body, .env = env } };
                        try env.set(function.name, object);
                        return object;
                    },
                    .CallExpression => |call_expression| {
                        const func = try self.eval(Node{ .Expression = call_expression.function.* }, env);
                        if (func.isError()) {
                            return func;
                        }

                        var args: std.ArrayListUnmanaged(Object) = .{};
                        for (call_expression.args.items) |arg| {
                            const evaluated = try self.eval(Node{ .Expression = arg.* }, env);
                            if (evaluated.isError()) {
                                return evaluated;
                            }
                            try args.append(self.gpa, evaluated);
                        }

                        switch (func) {
                            .Function => |function| {
                                const extended_env = try Environment.initEnclosed(self.gpa, function.env);
                                for (0..function.params.items.len) |i| {
                                    const name = function.params.items[i];
                                    const arg = args.items[i];
                                    try extended_env.set(name, arg);
                                }

                                const result = self.eval(Node{ .Statement = Statement{ .BlockStatement = function.body } }, extended_env);
                                return result;
                            },
                            else => return self.createError("can't call a non-function", .{}),
                        }
                    },
                    .IfExpression => |if_expression| {
                        const condition = try self.eval(Node{ .Expression = if_expression.condition.* }, env);
                        switch (condition) {
                            .Boolean => |val| {
                                if (val) {
                                    return try self.eval(Node{ .Statement = .{ .BlockStatement = if_expression.body } }, env);
                                }
                                if (if_expression.alternative) |alternative| {
                                    return try self.eval(Node{ .Statement = .{ .BlockStatement = alternative } }, env);
                                }
                                return Object.Null;
                            },
                            else => return try self.createError("can only evaluate boolean if-conditions", .{}),
                        }
                    },
                    .ForExpression => |for_expression| {
                        const lower = try self.eval(Node{ .Expression = for_expression.range.left.* }, env);
                        if (lower.isError()) {
                            return lower;
                        }
                        if (lower != .Integer) {
                            return self.createError("Can only use Integer in range expression", .{});
                        }
                        if (lower.Integer < 0) {
                            return self.createError("Range value can't be negative", .{});
                        }

                        const upper = try self.eval(Node{ .Expression = for_expression.range.right.* }, env);
                        if (upper.isError()) {
                            return upper;
                        }
                        if (upper != .Integer) {
                            return self.createError("Can only use Integer in range expression", .{});
                        }
                        if (upper.Integer < 0) {
                            return self.createError("Range value can't be negative", .{});
                        }

                        for (@intCast(lower.Integer)..@intCast(upper.Integer)) |i| {
                            try env.set(for_expression.variable, Object{ .Integer = @intCast(i) });
                            _ = try self.eval(Node{ .Statement = .{ .BlockStatement = for_expression.body } }, env);
                        }

                        return Object.Null;
                    },
                    else => return Object{ .Error = "Unknown expression" },
                }
            },
        };
    }

    fn evalProgram(self: *Evaluator, statements: std.ArrayList(Statement), env: *Environment) anyerror!Object {
        var result: Object = Object.Null;
        for (statements.items) |statement| {
            result = try self.eval(Node{ .Statement = statement }, env);
            switch (result) {
                .ReturnValue => return result,
                .Error => return result,
                else => {},
            }
        }
        return result;
    }

    fn evalPrefixExpression(self: *Evaluator, right: Object, operator: Operator) !Object {
        switch (operator) {
            .Bang => {
                switch (right) {
                    .Boolean => |v| return Object{ .Boolean = !v },
                    else => return try self.createError("invalid type for bang operator: {s}", .{right.getType()}),
                }
            },
            .Minus => {
                switch (right) {
                    .Integer => |v| return Object{ .Integer = -1 * v },
                    .Float => |v| return Object{ .Float = -1 * v },
                    else => return try self.createError("invalid type for minus operator: {s}", .{right.getType()}),
                }
            },
            else => return try self.createError("invalid operator in prefix position: {any}", .{Operator}),
        }
    }

    fn evalInfixExpression(self: *Evaluator, left: Object, right: Object, operator: Operator) !Object {
        switch (left) {
            .Integer => |left_int| {
                switch (right) {
                    .Integer => |right_int| {
                        return try self.evalIntegerInfixExpression(left_int, right_int, operator);
                    },
                    .Float => |right_float| {
                        return self.evalFloatInfixExpression(@floatFromInt(left_int), right_float, operator);
                    },
                    else => return self.createError("type mismatch: {s} <> {s}", .{ left.getType(), right.getType() }),
                }
            },
            .Float => |left_float| {
                switch (right) {
                    .Integer => |right_int| {
                        return try self.evalFloatInfixExpression(left_float, @floatFromInt(right_int), operator);
                    },
                    .Float => |right_float| {
                        return try self.evalFloatInfixExpression(left_float, right_float, operator);
                    },
                    else => return try self.createError("type mismatch: {s} <> {s}", .{ left.getType(), right.getType() }),
                }
            },
            .Boolean => |left_bool| {
                switch (right) {
                    .Boolean => |right_bool| {
                        return self.evalBooleanInfixExpression(left_bool, right_bool, operator);
                    },
                    else => return self.createError("type mismatch: {s} <> {s}", .{ left.getType(), right.getType() }),
                }
            },
            else => return Object{ .Error = "Unknown left expression type" },
        }
    }

    fn evalBooleanInfixExpression(self: *Evaluator, left: bool, right: bool, operator: Operator) !Object {
        return switch (operator) {
            .Eq => Object{ .Boolean = left == right },
            .NotEq => Object{ .Boolean = left != right },
            else => return try self.createError("invalid operator '{f}' for type Boolean", .{operator}),
        };
    }

    fn evalFloatInfixExpression(self: *Evaluator, left: f64, right: f64, operator: Operator) !Object {
        return switch (operator) {
            .Plus => Object{ .Float = left + right },
            .Minus => Object{ .Float = left - right },
            .Slash => Object{ .Float = left / right },
            .Asterisk => Object{ .Float = left * right },
            .Gt => Object{ .Boolean = left > right },
            .Lt => Object{ .Boolean = left < right },
            .Eq => Object{ .Boolean = left == right },
            .NotEq => Object{ .Boolean = left != right },
            else => return try self.createError("invalid operator '{f}' for type Float", .{operator}),
        };
    }

    fn evalIntegerInfixExpression(self: *Evaluator, left: i64, right: i64, operator: Operator) !Object {
        return switch (operator) {
            .Plus => Object{ .Integer = left + right },
            .Minus => Object{ .Integer = left - right },
            .Slash => blk: {
                const left_float: f64 = @floatFromInt(left);
                const right_float: f64 = @floatFromInt(right);
                break :blk Object{ .Float = left_float / right_float };
            },
            .Asterisk => Object{ .Integer = left * right },
            .Gt => Object{ .Boolean = left > right },
            .Lt => Object{ .Boolean = left < right },
            .Eq => Object{ .Boolean = left == right },
            .NotEq => Object{ .Boolean = left != right },
            else => return try self.createError("invalid operator '{f}' for type Integer", .{operator}),
        };
    }

    fn createError(self: *Evaluator, comptime fmt: []const u8, args: anytype) !Object {
        const error_message = try std.fmt.allocPrint(self.gpa, fmt, args);
        return Object{ .Error = error_message };
    }
};

pub const Environment = struct {
    gpa: std.mem.Allocator,
    store: std.StringHashMapUnmanaged(Object),
    children: std.ArrayListUnmanaged(*Environment),
    outer: ?*Environment,

    pub fn init(gpa: std.mem.Allocator) !*Environment {
        const env = try gpa.create(Environment);
        env.* = Environment{ .store = .{}, .outer = null, .children = .{}, .gpa = gpa };
        return env;
    }

    pub fn deinit(self: *Environment) void {
        for (self.children.items) |child| {
            child.deinit();
        }
        self.gpa.destroy(self);
    }

    pub fn initEnclosed(gpa: std.mem.Allocator, environment: *Environment) !*Environment {
        const env = try gpa.create(Environment);
        env.* = Environment{ .store = .{}, .outer = environment, .children = .{}, .gpa = gpa };
        try environment.children.append(gpa, env);
        return env;
    }

    pub fn get(self: *Environment, key: []const u8) ?Object {
        const val = self.store.get(key);
        if (val != null) {
            return val;
        }
        if (self.outer) |outer| {
            return outer.get(key);
        }
        return null;
    }

    pub fn set(self: *Environment, key: []const u8, val: Object) !void {
        try self.store.put(self.gpa, key, val);
    }
};
