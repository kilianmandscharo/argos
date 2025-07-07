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
    Integer: Integer,
    Float: Float,
    String: String,
    Boolean: Boolean,
    ReturnValue: ReturnValue,
    Error: Error,
    Function: Function,
    Null: Null,

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
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .Integer => |v| try writer.print("{d}", .{v.value}),
            .Float => |v| try writer.print("{d}", .{v.value}),
            .String => |v| try writer.print("{s}", .{v.value}),
            .Boolean => |v| try writer.print("{}", .{v.value}),
            .ReturnValue => |v| try writer.print("{any}", .{v.value}),
            .Error => |v| try writer.print("{s}", .{v.value}),
            .Function => try writer.print("Function", .{}),
            .Null => try writer.print("Null", .{}),
        }
    }
};

const Integer = struct {
    value: i64,
};

const Float = struct {
    value: f64,
};

const String = struct {
    value: []const u8,
};

const Boolean = struct {
    value: bool,
};

const ReturnValue = struct {
    value: *const Object,
};

const Error = struct {
    value: []const u8,
};

const Function = struct {
    params: std.ArrayList([]const u8),
    body: BlockStatement,
    env: *Environment,
};

const Null = struct {};

pub const Evaluator = struct {
    gpa: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator) Evaluator {
        return Evaluator{
            .gpa = gpa,
        };
    }

    pub fn eval(self: *Evaluator, node: Node, env: *Environment) Object {
        return switch (node) {
            .Program => |statements| {
                return self.evalProgram(statements, env);
            },
            .Statement => |statement| {
                switch (statement) {
                    .ExpressionStatement => |expression_statement| {
                        return self.eval(Node{ .Expression = expression_statement.expression.* }, env);
                    },
                    .AssignmentStatement => |assignment| {
                        const val = self.eval(Node{ .Expression = assignment.expression.* }, env);
                        if (val.isError()) {
                            return val;
                        }
                        env.set(assignment.identifier.literal, val) catch {
                            return self.createError("failed to assign value", .{});
                        };
                        return val;
                    },
                    .BlockStatement => |block| {
                        var result = Object{ .Null = .{} };
                        for (block.statements.items) |item| {
                            result = self.eval(Node{ .Statement = item }, env);
                            switch (result) {
                                .ReturnValue => return result,
                                .Error => return result,
                                else => {},
                            }
                        }
                        return result;
                    },
                    .ReturnStatement => |return_statement| {
                        const value = self.eval(Node{ .Expression = return_statement.expression.* }, env);
                        if (value.isError()) {
                            return value;
                        }
                        const owned = self.gpa.create(Object) catch {
                            return self.createError("could not return", .{});
                        };
                        owned.* = value;
                        return Object{ .ReturnValue = .{ .value = owned } };
                    },
                }
            },
            .Expression => |expression| {
                switch (expression) {
                    .Identifier => |identifier| {
                        const val = env.get(identifier) orelse self.createError("identifier '{s}' not found", .{identifier});
                        return val;
                    },
                    .IntegerLiteral => |integer| return Object{ .Integer = .{ .value = integer } },
                    .FloatLiteral => |float| return Object{ .Float = .{ .value = float } },
                    .BooleanLiteral => |boolean| return Object{ .Boolean = .{ .value = boolean } },
                    .InfixExpression => |infix| {
                        const left = self.eval(Node{ .Expression = infix.left.* }, env);
                        if (left.isError()) {
                            return left;
                        }
                        const right = self.eval(Node{ .Expression = infix.right.* }, env);
                        if (right.isError()) {
                            return right;
                        }
                        return self.evalInfixExpression(left, right, infix.operator);
                    },
                    .FunctionLiteral => |function| {
                        return Object{ .Function = .{ .params = function.params, .body = function.body, .env = env } };
                    },
                    .CallExpression => |call_expression| {
                        const func = self.eval(Node{ .Expression = call_expression.function.* }, env);
                        if (func.isError()) {
                            return func;
                        }

                        var args = std.ArrayList(Object).init(self.gpa);
                        for (call_expression.args.items) |arg| {
                            const evaluated = self.eval(Node{ .Expression = arg.* }, env);
                            if (evaluated.isError()) {
                                return evaluated;
                            }
                            args.append(evaluated) catch {
                                return self.createError("failed to call function", .{});
                            };
                        }

                        switch (func) {
                            .Function => |function| {
                                const extended_env = Environment.initEnclosed(self.gpa, env) catch {
                                    return self.createError("failed to call function", .{});
                                };
                                for (0..function.params.items.len) |i| {
                                    const name = function.params.items[i];
                                    const arg = args.items[i];
                                    extended_env.set(name, arg) catch {
                                        return self.createError("failed to call function", .{});
                                    };
                                }
                                const result = self.eval(Node{ .Statement = Statement{ .BlockStatement = function.body } }, extended_env);
                                return result;
                            },
                            else => return self.createError("can't call a non-function", .{}),
                        }
                    },
                    else => return Object{ .Error = .{ .value = "Unknown expression" } },
                }
            },
        };
    }

    fn evalProgram(self: *Evaluator, statements: std.ArrayList(Statement), env: *Environment) Object {
        var result = Object{ .Null = .{} };
        for (statements.items) |statement| {
            result = self.eval(Node{ .Statement = statement }, env);
            switch (result) {
                .ReturnValue => |retVal| return retVal.value.*,
                .Error => return result,
                else => {},
            }
        }
        return result;
    }

    fn evalInfixExpression(self: *Evaluator, left: Object, right: Object, operator: Operator) Object {
        switch (left) {
            .Integer => |left_int| {
                switch (right) {
                    .Integer => |right_int| {
                        return self.evalIntegerInfixExpression(left_int.value, right_int.value, operator);
                    },
                    .Float => |right_float| {
                        return self.evalFloatInfixExpression(@floatFromInt(left_int.value), right_float.value, operator);
                    },
                    else => return self.createError("type mismatch: {s} <> {s}", .{ left.getType(), right.getType() }),
                }
            },
            .Float => |left_float| {
                switch (right) {
                    .Integer => |right_int| {
                        return self.evalFloatInfixExpression(left_float.value, @floatFromInt(right_int.value), operator);
                    },
                    .Float => |right_float| {
                        return self.evalFloatInfixExpression(left_float.value, right_float.value, operator);
                    },
                    else => return self.createError("type mismatch: {s} <> {s}", .{ left.getType(), right.getType() }),
                }
            },
            .Boolean => |left_bool| {
                switch (right) {
                    .Boolean => |right_bool| {
                        return self.evalBooleanInfixExpression(left_bool.value, right_bool.value, operator);
                    },
                    else => return self.createError("type mismatch: {s} <> {s}", .{ left.getType(), right.getType() }),
                }
            },
            else => return Object{ .Error = .{ .value = "Unknown left expression type" } },
        }
    }

    fn evalBooleanInfixExpression(self: *Evaluator, left: bool, right: bool, operator: Operator) Object {
        return switch (operator) {
            .Eq => Object{ .Boolean = .{ .value = left == right } },
            .NotEq => Object{ .Boolean = .{ .value = left != right } },
            else => self.createError("invalid operator '{any}' for type Boolean", .{operator}),
        };
    }

    fn evalFloatInfixExpression(self: *Evaluator, left: f64, right: f64, operator: Operator) Object {
        return switch (operator) {
            .Plus => Object{ .Float = .{ .value = left + right } },
            .Minus => Object{ .Float = .{ .value = left - right } },
            .Slash => Object{ .Float = .{ .value = left / right } },
            .Asterisk => Object{ .Float = .{ .value = left * right } },
            .Gt => Object{ .Boolean = .{ .value = left > right } },
            .Lt => Object{ .Boolean = .{ .value = left < right } },
            .Eq => Object{ .Boolean = .{ .value = left == right } },
            .NotEq => Object{ .Boolean = .{ .value = left != right } },
            else => self.createError("invalid operator '{any}' for type Float", .{operator}),
        };
    }

    fn evalIntegerInfixExpression(self: *Evaluator, left: i64, right: i64, operator: Operator) Object {
        return switch (operator) {
            .Plus => Object{ .Integer = .{ .value = left + right } },
            .Minus => Object{ .Integer = .{ .value = left - right } },
            .Slash => {
                const left_float: f64 = @floatFromInt(left);
                const right_float: f64 = @floatFromInt(right);
                return Object{ .Float = .{ .value = left_float / right_float } };
            },
            .Asterisk => Object{ .Integer = .{ .value = left * right } },
            .Gt => Object{ .Boolean = .{ .value = left > right } },
            .Lt => Object{ .Boolean = .{ .value = left < right } },
            .Eq => Object{ .Boolean = .{ .value = left == right } },
            .NotEq => Object{ .Boolean = .{ .value = left != right } },
            else => self.createError("invalid operator '{any}' for type Integer", .{operator}),
        };
    }

    fn createError(self: *Evaluator, comptime fmt: []const u8, args: anytype) Object {
        const error_message = std.fmt.allocPrint(self.gpa, fmt, args) catch |err| {
            std.debug.print("failed to alloc error message: {}", .{err});
            return Object{ .Error = .{ .value = "Unknown error" } };
        };
        return Object{ .Error = .{ .value = error_message } };
    }
};

pub const Environment = struct {
    store: std.StringHashMap(Object),
    outer: ?*Environment,
    children: std.ArrayList(*Environment),
    gpa: std.mem.Allocator,

    pub fn init(gpa: std.mem.Allocator) !*Environment {
        const children = std.ArrayList(*Environment).init(gpa);
        const store = std.StringHashMap(Object).init(gpa);
        const env = try gpa.create(Environment);
        env.* = Environment{ .store = store, .outer = null, .children = children, .gpa = gpa };
        return env;
    }

    pub fn deinit(self: *Environment) void {
        for (self.children.items) |child| {
            child.deinit();
        }
        self.children.deinit();
        self.store.deinit();
        self.gpa.destroy(self);
    }

    pub fn initEnclosed(gpa: std.mem.Allocator, environment: *Environment) !*Environment {
        const children = std.ArrayList(*Environment).init(gpa);
        const store = std.StringHashMap(Object).init(gpa);
        const env = try gpa.create(Environment);
        env.* = Environment{ .store = store, .outer = environment, .children = children, .gpa = gpa };
        try environment.children.append(env);
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
        try self.store.put(key, val);
    }
};
