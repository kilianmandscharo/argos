const std = @import("std");

// TODO: clean up the error handling in the evaluator

const parser_module = @import("parser.zig");
const Statement = parser_module.Statement;
const Expression = parser_module.Expression;
const Operator = parser_module.Operator;
const BlockStatement = parser_module.BlockStatement;

const EvaluatorError = error{
    RuntimeError,
    IdentifierNotFound,
};

pub const Node = union(enum) {
    Program: std.ArrayListUnmanaged(Statement),
    Expression: Expression,
    Statement: Statement,
};

pub const Object = union(enum) {
    Integer: i64,
    Float: f64,
    String: []const u8,
    Boolean: bool,
    ReturnValue: usize,
    Function: Function,
    Null,

    fn getType(self: Object) []const u8 {
        return switch (self) {
            .Integer => "Integer",
            .Float => "Float",
            .String => "String",
            .Boolean => "Boolean",
            .ReturnValue => "ReturnValue",
            .Function => "Function",
            .Null => "Null",
        };
    }

    // fn deinit(self: *Object, gpa: std.mem.Allocator) void {
    //     return switch (self.*) {
    //         .Integer => {},
    //         .Float => {},
    //         .String => {},
    //         .Boolean => {},
    //         .ReturnValue => {},
    //         .Function => {},
    //         .Null => {},
    //     };
    // }

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

    pub fn deinit(self: *ObjectStore) void {
        self.objects.deinit(self.gpa);
    }

    pub fn add(self: *ObjectStore, object: Object) !usize {
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

    pub fn deinit(self: *Evaluator) void {
        self.object_store.deinit();
    }

    pub fn eval(self: *Evaluator, node: Node, env: *Environment) !Object {
        return switch (node) {
            .Program => |statements| {
                return try self.evalProgram(statements, env);
            },
            .Statement => |statement| {
                // std.debug.print("eval statement {f}\n", .{statement});
                switch (statement) {
                    .ExpressionStatement => |expression_statement| {
                        return try self.eval(Node{ .Expression = expression_statement.expression.* }, env);
                    },
                    .AssignmentStatement => |assignment| {
                        const val = try self.eval(Node{ .Expression = assignment.expression.* }, env);
                        try env.set(assignment.identifier, val);
                        return val;
                    },
                    .BlockStatement => |block| {
                        var result: Object = Object.Null;
                        for (block.statements.items) |item| {
                            result = try self.eval(Node{ .Statement = item }, env);
                            switch (result) {
                                .ReturnValue => |idx| return self.object_store.get(idx),
                                else => {},
                            }
                        }
                        return result;
                    },
                    .ReturnStatement => |return_statement| {
                        const value = try self.eval(Node{ .Expression = return_statement.expression.* }, env);
                        const object_id = try self.object_store.add(value);
                        const return_val = Object{ .ReturnValue = object_id };
                        return return_val;
                    },
                }
            },
            .Expression => |expression| {
                // std.debug.print("eval expression {f}\n", .{expression});
                switch (expression) {
                    .Identifier => |identifier| {
                        const val = try env.get(identifier);
                        return val;
                    },
                    .IntegerLiteral => |integer| return Object{ .Integer = integer },
                    .FloatLiteral => |float| return Object{ .Float = float },
                    .BooleanLiteral => |boolean| return Object{ .Boolean = boolean },
                    .InfixExpression => |infix| {
                        const left = try self.eval(Node{ .Expression = infix.left.* }, env);
                        const right = try self.eval(Node{ .Expression = infix.right.* }, env);
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

                        var args: std.ArrayListUnmanaged(Object) = .{};
                        defer args.deinit(self.gpa);

                        for (call_expression.args.items) |arg| {
                            const evaluated = try self.eval(Node{ .Expression = arg.* }, env);
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
                            else => {
                                self.printError("can't call a non-function: {f}\n", .{expression});
                                return EvaluatorError.RuntimeError;
                            },
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
                            else => {
                                self.printError("found non-Boolean value in if condition: {s}\n", .{@tagName(condition)});
                                return EvaluatorError.RuntimeError;
                            },
                        }
                    },
                    .ForExpression => |for_expression| {
                        const lower = try self.eval(Node{ .Expression = for_expression.range.left.* }, env);
                        if (lower != .Integer) {
                            self.printError("found non-Integer value in range expression: {s}\n", .{@tagName(lower)});
                            return EvaluatorError.RuntimeError;
                        }
                        if (lower.Integer < 0) {
                            self.printError("range value can't be negative\n", .{});
                            return EvaluatorError.RuntimeError;
                        }

                        const upper = try self.eval(Node{ .Expression = for_expression.range.right.* }, env);
                        if (upper != .Integer) {
                            self.printError("found non-Integer value in range expression: {s}\n", .{@tagName(lower)});
                            return EvaluatorError.RuntimeError;
                        }
                        if (upper.Integer < 0) {
                            self.printError("range value can't be negative\n", .{});
                            return EvaluatorError.RuntimeError;
                        }

                        for (@intCast(lower.Integer)..@intCast(upper.Integer)) |i| {
                            try env.set(for_expression.variable, Object{ .Integer = @intCast(i) });
                            _ = try self.eval(Node{ .Statement = .{ .BlockStatement = for_expression.body } }, env);
                        }

                        return Object.Null;
                    },
                    else => {
                        self.printError("unknown expression: {s}\n", .{@tagName(expression)});
                        return EvaluatorError.RuntimeError;
                    },
                }
            },
        };
    }

    fn printError(self: *Evaluator, comptime format: []const u8, args: anytype) void {
        _ = self;
        std.debug.print(format, args);
    }

    fn evalProgram(self: *Evaluator, statements: std.ArrayListUnmanaged(Statement), env: *Environment) anyerror!Object {
        var result: Object = Object.Null;
        for (statements.items) |statement| {
            result = try self.eval(Node{ .Statement = statement }, env);
            switch (result) {
                .ReturnValue => return result,
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
                    else => {
                        self.printError("invalid type for bang operator: {s}\n", .{right.getType()});
                        return EvaluatorError.RuntimeError;
                    },
                }
            },
            .Minus => {
                switch (right) {
                    .Integer => |v| return Object{ .Integer = -1 * v },
                    .Float => |v| return Object{ .Float = -1 * v },
                    else => {
                        self.printError("invalid type for minus operator: {s}\n", .{right.getType()});
                        return EvaluatorError.RuntimeError;
                    },
                }
            },
            else => {
                self.printError("invalid operator in prefix position: {any}\n", .{Operator});
                return EvaluatorError.RuntimeError;
            },
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
                    else => {
                        self.printError("type mismatch: {s} <> {s}\n", .{ left.getType(), right.getType() });
                        return EvaluatorError.RuntimeError;
                    },
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
                    else => {
                        self.printError("type mismatch: {s} <> {s}\n", .{ left.getType(), right.getType() });
                        return EvaluatorError.RuntimeError;
                    },
                }
            },
            .Boolean => |left_bool| {
                switch (right) {
                    .Boolean => |right_bool| {
                        return self.evalBooleanInfixExpression(left_bool, right_bool, operator);
                    },
                    else => {
                        self.printError("type mismatch: {s} <> {s}\n", .{ left.getType(), right.getType() });
                        return EvaluatorError.RuntimeError;
                    },
                }
            },
            else => {
                self.printError("unknown left expression in infix expression: {s}\n", .{@tagName(left)});
                return EvaluatorError.RuntimeError;
            },
        }
    }

    fn evalBooleanInfixExpression(self: *Evaluator, left: bool, right: bool, operator: Operator) !Object {
        return switch (operator) {
            .Eq => Object{ .Boolean = left == right },
            .NotEq => Object{ .Boolean = left != right },
            else => {
                self.printError("invalid operator '{f}' for type Boolean\n", .{operator});
                return EvaluatorError.RuntimeError;
            },
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
            else => {
                self.printError("invalid operator '{f}' for type Float\n", .{operator});
                return EvaluatorError.RuntimeError;
            },
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
            else => {
                self.printError("invalid operator '{f}' for type Integer\n", .{operator});
                return EvaluatorError.RuntimeError;
            },
        };
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
        self.store.deinit(self.gpa);
        self.children.deinit(self.gpa);
        self.gpa.destroy(self);
    }

    pub fn initEnclosed(gpa: std.mem.Allocator, environment: *Environment) !*Environment {
        const env = try gpa.create(Environment);
        env.* = Environment{ .store = .{}, .outer = environment, .children = .{}, .gpa = gpa };
        try environment.children.append(gpa, env);
        return env;
    }

    pub fn get(self: *Environment, key: []const u8) !Object {
        if (self.store.get(key)) |val| {
            return val;
        }
        if (self.outer) |outer| {
            return outer.get(key);
        }
        std.debug.print("identifier {s} not found", .{key});
        return EvaluatorError.IdentifierNotFound;
    }

    pub fn set(self: *Environment, key: []const u8, val: Object) !void {
        try self.store.put(self.gpa, key, val);
    }
};
