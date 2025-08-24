const std = @import("std");

// TODO: clean up the error handling in the evaluator
// TODO: clean up the heap allocated ReturnValue object

const parser_module = @import("parser.zig");
const Expression = parser_module.Expression;
const Operator = parser_module.Operator;
const BlockExpression = parser_module.BlockExpression;

const EvaluatorError = error{
    RuntimeError,
    IdentifierNotFound,
};

pub const Object = union(enum) {
    Integer: i64,
    Float: f64,
    String: []const u8,
    Boolean: bool,
    ReturnValue: *Object,
    Function: Function,
    Array: *Array,
    Null,

    fn getType(self: Object) []const u8 {
        return switch (self) {
            .Integer => "Integer",
            .Float => "Float",
            .String => "String",
            .Boolean => "Boolean",
            .ReturnValue => "ReturnValue",
            .Function => "Function",
            .Array => "Array",
            .Null => "Null",
        };
    }

    fn deinit(self: *Object, gpa: std.mem.Allocator) !void {
        std.debug.print("deinit object {s}\n", .{self.getType()});
        return switch (self.*) {
            .Integer => {},
            .Float => {},
            .String => {},
            .Boolean => {},
            .ReturnValue => {},
            .Function => {},
            .Array => |array| {
                array.data.deinit(gpa);
                gpa.destroy(array);
            },
            .Null => {},
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
            .Function => try writer.print("Function", .{}),
            .Array => try writer.print("Array", .{}),
            .Null => try writer.print("Null", .{}),
        }
    }
};

const Function = struct {
    params: std.ArrayListUnmanaged([]const u8),
    body: BlockExpression,
    env: *Environment,
};

const Array = struct {
    data: std.ArrayListUnmanaged(Object),
    refs: usize,
};

pub const Evaluator = struct {
    gpa: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator) Evaluator {
        return Evaluator{
            .gpa = gpa,
        };
    }

    pub fn eval(self: *Evaluator, expression: *const Expression, env: *Environment) !Object {
        return switch (expression.*) {
            .Program => |statements| {
                return try self.evalProgram(statements, env);
            },
            .ReturnExpression => |return_expression| {
                const value = try self.eval(return_expression, env);
                const value_owned = try self.gpa.create(Object);
                value_owned.* = value;
                return Object{ .ReturnValue = value_owned };
            },
            .BlockExpression => |block| {
                var result: Object = Object.Null;
                for (block.expressions.items) |item| {
                    result = try self.eval(item, env);
                    switch (result) {
                        .ReturnValue => |obj| return obj.*,
                        else => {},
                    }
                }
                return result;
            },
            .Identifier => |identifier| {
                const val = try env.get(identifier);
                return val;
            },
            .IntegerLiteral => |integer| return Object{ .Integer = integer },
            .FloatLiteral => |float| return Object{ .Float = float },
            .BooleanLiteral => |boolean| return Object{ .Boolean = boolean },
            .InfixExpression => |infix| {
                const left = try self.eval(infix.left, env);
                const right = try self.eval(infix.right, env);
                return self.evalInfixExpression(left, right, infix.operator);
            },
            .PrefixExpression => |prefix| {
                const right = try self.eval(prefix.expression, env);
                return self.evalPrefixExpression(right, prefix.operator);
            },
            .FunctionLiteral => |function| {
                const object = Object{ .Function = .{ .params = function.params, .body = function.body, .env = env } };
                try env.set(function.name, object);
                return object;
            },
            .CallExpression => |call_expression| {
                const func = try self.eval(call_expression.function, env);

                var args: std.ArrayListUnmanaged(Object) = .{};
                defer args.deinit(self.gpa);

                for (call_expression.args.items) |arg| {
                    const evaluated = try self.eval(arg, env);
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

                        const result = self.eval(&Expression{ .BlockExpression = function.body }, extended_env);

                        // TODO: clean up extended_env

                        return result;
                    },
                    else => {
                        self.printError("can't call a non-function: {f}\n", .{expression});
                        return EvaluatorError.RuntimeError;
                    },
                }
            },
            .IfExpression => |if_expression| {
                const condition = try self.eval(if_expression.condition, env);
                switch (condition) {
                    .Boolean => |val| {
                        if (val) {
                            return try self.eval(&Expression{ .BlockExpression = if_expression.body }, env);
                        }
                        if (if_expression.alternative) |alternative| {
                            return try self.eval(&Expression{ .BlockExpression = alternative }, env);
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
                const lower = try self.eval(for_expression.range.left, env);
                if (lower != .Integer) {
                    self.printError("found non-Integer value in range expression: {s}\n", .{@tagName(lower)});
                    return EvaluatorError.RuntimeError;
                }
                if (lower.Integer < 0) {
                    self.printError("range value can't be negative\n", .{});
                    return EvaluatorError.RuntimeError;
                }

                const upper = try self.eval(for_expression.range.right, env);
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
                    _ = try self.eval(&Expression{ .BlockExpression = for_expression.body }, env);
                }

                return Object.Null;
            },
            .AssignmentExpression => |assignment| {
                const val = try self.eval(assignment.expression, env);
                try env.set(assignment.identifier, val);
                return val;
            },
            .ArrayLiteral => |array| {
                var data: std.ArrayListUnmanaged(Object) = .{};
                for (array.items) |item| {
                    const evaluated = try self.eval(item, env);
                    try data.append(self.gpa, evaluated);
                }
                const array_owned = try self.gpa.create(Array);
                array_owned.* = Array{ .data = data, .refs = 1 };
                return Object{ .Array = array_owned };
            },
            .IndexExpression => |index_expression| {
                const evaluated = try self.eval(index_expression.left, env);
                switch (evaluated) {
                    .Array => |array| {
                        const index = try self.eval(index_expression.index_expression, env);
                        switch (index) {
                            .Integer => |integer| {
                                return array.data.items[@intCast(integer)];
                            },
                            else => return self.runtimeError(
                                "invalid type for index in index expression: {s}",
                                .{index.getType()},
                            ),
                        }
                    },
                    else => return self.runtimeError(
                        "invalid left side for index expression: {s}",
                        .{evaluated.getType()},
                    ),
                }
            },
            else => {
                self.printError("unknown expression: {s}\n", .{@tagName(expression.*)});
                return EvaluatorError.RuntimeError;
            },
        };
    }

    fn runtimeError(self: *Evaluator, comptime format: []const u8, args: anytype) EvaluatorError {
        self.printError(format, args);
        return EvaluatorError.RuntimeError;
    }

    fn printError(self: *Evaluator, comptime format: []const u8, args: anytype) void {
        _ = self;
        std.debug.print(format, args);
    }

    fn evalProgram(self: *Evaluator, expressions: std.ArrayListUnmanaged(*const Expression), env: *Environment) anyerror!Object {
        var result: Object = Object.Null;
        for (expressions.items) |expression| {
            result = try self.eval(expression, env);
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
                self.printError("invalid operator in prefix position: {any}\n", .{operator});
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
        std.debug.print("deinit env\n", .{});

        for (self.children.items) |child| {
            child.deinit();
        }

        var object_iterator = self.store.valueIterator();
        while (object_iterator.next()) |object| {
            object.deinit(self.gpa) catch |err| {
                std.debug.print("failed to deinit object: {any}\n", .{err});
            };
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
