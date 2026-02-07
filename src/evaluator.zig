const std = @import("std");

const parser_module = @import("parser.zig");
const logging = @import("logging.zig");
const LogColor = logging.LogColor;
const log = logging.log;

const Expression = parser_module.Expression;
const Operator = parser_module.Operator;
const BlockExpression = parser_module.BlockExpression;
const FunctionParam = parser_module.FunctionParam;

const EvaluatorError = error{
    RuntimeError,
    IdentifierNotFound,
};

fn printDebug(comptime fmt: []const u8, args: anytype, debug: bool) void {
    log(fmt, args, .{
        .messageLevel = .Debug,
        .currentLevel = if (debug) .Debug else .Fatal,
    });
}

pub const Object = union(enum) {
    Integer: i64,
    Float: f64,
    String: String,
    Boolean: bool,
    ReturnValue: *Object,
    Function: Function,
    Array: *Array,
    Table: *Table,
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
            .Table => "Table",
            .Null => "Null",
        };
    }

    fn deinit(self: *Object, env: *Environment) void {
        printDebug("deinit object {s}\n", .{self.getType()}, env.debug);
        return switch (self.*) {
            .Integer => {},
            .Float => {},
            .Boolean => {},
            .ReturnValue => {},
            .Function => {},
            .String => |*string| {
                env.gpa.free(string.data);
                printDebug("destroyed string\n", .{}, env.debug);
            },
            .Array => |array| {
                for (array.data.items) |*item| {
                    item.decRef(env);
                }
                array.data.deinit(env.gpa);
                env.gpa.destroy(array);
                printDebug("destroyed array\n", .{}, env.debug);
            },
            .Table => |table| {
                var iterator = table.data.iterator();
                while (iterator.next()) |item| {
                    item.value_ptr.decRef(env);
                }
                table.data.deinit(env.gpa);
                env.gpa.destroy(table);
                printDebug("destroyed table\n", .{}, env.debug);
            },
            .Null => {},
        };
    }

    // TODO: dec all array/table elements on deinitialization
    fn decRef(self: *Object, env: *Environment) void {
        return switch (self.*) {
            .Integer => {},
            .Float => {},
            .Boolean => {},
            .ReturnValue => {},
            .Function => {},
            .Null => {},
            .String => |*string| {
                printDebug("dec string refCount, new count: {d}\n", .{string.ref_count - 1}, env.debug);
                string.ref_count -= 1;
                if (string.ref_count == 0) {
                    self.deinit(env);
                }
            },
            .Array => |array| {
                printDebug("dec array refCount, new count: {d}\n", .{array.ref_count - 1}, env.debug);
                array.ref_count -= 1;
                if (array.ref_count == 0) {
                    self.deinit(env);
                }
            },
            .Table => |table| {
                printDebug("dec table refCount, new count: {d}\n", .{table.ref_count - 1}, env.debug);
                table.ref_count -= 1;
                if (table.ref_count == 0) {
                    self.deinit(env);
                }
            },
        };
    }

    fn incRef(self: *Object, env: *Environment) void {
        return switch (self.*) {
            .Integer => {},
            .Float => {},
            .Boolean => {},
            .ReturnValue => {},
            .Function => {},
            .Null => {},
            .String => |*string| {
                printDebug("inc string refCount, new count: {d}\n", .{string.ref_count + 1}, env.debug);
                string.ref_count += 1;
            },
            .Array => |array| {
                printDebug("inc array refCount, new count: {d}\n", .{array.ref_count + 1}, env.debug);
                array.ref_count += 1;
            },
            .Table => |table| {
                printDebug("inc table refCount, new count: {d}\n", .{table.ref_count + 1}, env.debug);
                table.ref_count += 1;
            },
        };
    }

    pub fn format(
        self: @This(),
        writer: anytype,
    ) !void {
        switch (self) {
            .Integer => |v| try writer.print("{s} {d}", .{ self.getType(), v }),
            .Float => |v| try writer.print("{s} {d}", .{ self.getType(), v }),
            .String => |v| try writer.print("{s} {s}", .{ self.getType(), v.data }),
            .Boolean => |v| try writer.print("{s} {}", .{ self.getType(), v }),
            .ReturnValue => |v| try writer.print("{s} {any}", .{ self.getType(), v }),
            .Function => try writer.print("{s}", .{self.getType()}),
            .Array => try writer.print("{s}", .{self.getType()}),
            .Table => try writer.print("{s}", .{self.getType()}),
            .Null => try writer.print("{s}", .{self.getType()}),
        }
    }
};

const Function = struct {
    params: std.ArrayListUnmanaged(FunctionParam),
    body: *const Expression,
    env: *Environment,
};

pub const Array = struct {
    data: std.ArrayListUnmanaged(Object),
    ref_count: usize,
};

pub const Table = struct {
    data: std.StringHashMapUnmanaged(Object),
    ref_count: usize,
};

pub const String = struct {
    data: []const u8,
    ref_count: usize,
};

pub const Evaluator = struct {
    gpa: std.mem.Allocator,
    debug: bool = false,

    pub fn init(options: struct { gpa: std.mem.Allocator, debug: bool = false }) Evaluator {
        return Evaluator{
            .gpa = options.gpa,
            .debug = options.debug,
        };
    }

    pub fn eval(self: *Evaluator, expression: *const Expression, env: *Environment) !Object {
        return switch (expression.*) {
            .Program => |statements| {
                return try self.evalProgram(statements, env);
            },
            .Statement => |statement| {
                var value = try self.eval(statement, env);
                if (statement.* != .Identifier) {
                    value.decRef(env);
                }
                return value;
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
                        .ReturnValue => {
                            return result;
                        },
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
            .StringLiteral => |string| {
                const string_owned = try self.gpa.dupe(u8, string);
                return Object{ .String = String{ .data = string_owned, .ref_count = 1 } };
            },
            .InfixExpression => |infix| {
                var left = try self.eval(infix.left, env);
                var right = try self.eval(infix.right, env);
                const evaluated = self.evalInfixExpression(&left, &right, infix.operator);
                if (infix.left.* == .StringLiteral) {
                    left.decRef(env);
                }
                if (infix.right.* == .StringLiteral) {
                    right.decRef(env);
                }
                return evaluated;
            },
            .PrefixExpression => |prefix| {
                const right = try self.eval(prefix.expression, env);
                return self.evalPrefixExpression(right, prefix.operator);
            },
            .FunctionLiteral => |function| {
                const object = Object{ .Function = .{ .params = function.params, .body = function.body, .env = env } };
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
                        for (function.params.items, 0..) |param, i| {
                            switch (param) {
                                .Identifier => |name| {
                                    const arg = args.items[i];
                                    try extended_env.set(name, arg);
                                },
                                else => return error.NotImplemented,
                            }
                        }

                        const result = try self.eval(function.body, extended_env);

                        switch (result) {
                            .ReturnValue => |obj| {
                                const retVal = obj.*;
                                self.gpa.destroy(obj);
                                return retVal;
                            },
                            else => {},
                        }

                        // TODO: clean up extended_env at this point

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
                var val = try self.eval(assignment.expression, env);
                switch (assignment.left.*) {
                    .Identifier => |identifier| {
                        try env.set(identifier, val);
                    },
                    .IndexExpression => |index_expression| {
                        const left = try self.eval(index_expression.left, env);
                        switch (left) {
                            .Array => |array| {
                                const index = try self.eval(index_expression.index_expression, env);
                                array.data.items[@intCast(index.Integer)] = val;
                            },
                            .Table => |table| {
                                var index = try self.eval(index_expression.index_expression, env);
                                defer index.decRef(env);
                                try table.data.put(self.gpa, index.String.data, val);
                            },
                            else => return self.runtimeError("invalid left side for index expression: {s}\n", .{left.getType()}),
                        }
                    },
                    else => return self.runtimeError("invalid left side for assignment expression: {s}\n", .{assignment.left.getType()}),
                }
                val.incRef(env);
                return val;
            },
            .ArrayLiteral => |array| {
                var data: std.ArrayListUnmanaged(Object) = .{};
                for (array.items) |item| {
                    const evaluated = try self.eval(item, env);
                    try data.append(self.gpa, evaluated);
                }
                const array_owned = try self.gpa.create(Array);
                array_owned.* = Array{ .data = data, .ref_count = 1 };
                return Object{ .Array = array_owned };
            },
            .TableLiteral => |table| {
                var data: std.StringHashMapUnmanaged(Object) = .{};
                for (table.items) |item| {
                    // we know that every expression is of type Assignment
                    const key = item.AssignmentExpression.left.Identifier;
                    const value = item.AssignmentExpression.expression;
                    const evaluated = try self.eval(value, env);
                    try data.put(self.gpa, key, evaluated);
                }
                const table_owned = try self.gpa.create(Table);
                table_owned.* = Table{ .data = data, .ref_count = 1 };
                return Object{ .Table = table_owned };
            },
            .IndexExpression => |index_expression| {
                var evaluated = try self.eval(index_expression.left, env);

                switch (evaluated) {
                    .Array => |array| {
                        const index = try self.eval(index_expression.index_expression, env);
                        switch (index) {
                            .Integer => |integer| {
                                const val = array.data.items[@intCast(integer)];
                                if (index_expression.left.* == .ArrayLiteral) {
                                    defer evaluated.decRef(env);
                                }
                                return val;
                            },
                            else => return self.runtimeError(
                                "invalid type for index in array index expression: {s}",
                                .{index.getType()},
                            ),
                        }
                    },
                    .Table => |table| {
                        var index = try self.eval(index_expression.index_expression, env);
                        switch (index) {
                            .String => |string| {
                                const val = table.data.get(string.data);
                                if (index_expression.left.* == .TableLiteral) {
                                    defer evaluated.decRef(env);
                                }
                                if (index_expression.index_expression.* == .StringLiteral) {
                                    defer index.decRef(env);
                                }
                                return val orelse .Null;
                            },
                            else => return self.runtimeError(
                                "invalid type for index in table index expression: {s}",
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
            .Null => .Null,
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
                .ReturnValue => |obj| {
                    const retVal = obj.*;
                    self.gpa.destroy(obj);
                    return retVal;
                },
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
            .Tilde => {
                switch (right) {
                    .Integer => |v| return Object{ .Integer = ~v },
                    else => {
                        self.printError("invalid type for tilde operator: {s}\n", .{right.getType()});
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

    fn evalInfixExpression(self: *Evaluator, left: *Object, right: *Object, operator: Operator) !Object {
        switch (left.*) {
            .Integer => |left_int| {
                switch (right.*) {
                    .Integer => |right_int| {
                        return try self.evalIntegerInfixExpression(left_int, right_int, operator);
                    },
                    .Float => |right_float| {
                        return self.evalFloatInfixExpression(@floatFromInt(left_int), right_float, operator);
                    },
                    else => {
                        return self.runtimeError("type mismatch: {s} <> {s}\n", .{ left.getType(), right.getType() });
                    },
                }
            },
            .Float => |left_float| {
                switch (right.*) {
                    .Integer => |right_int| {
                        return try self.evalFloatInfixExpression(left_float, @floatFromInt(right_int), operator);
                    },
                    .Float => |right_float| {
                        return try self.evalFloatInfixExpression(left_float, right_float, operator);
                    },
                    else => {
                        return self.runtimeError("type mismatch: {s} <> {s}\n", .{ left.getType(), right.getType() });
                    },
                }
            },
            .Boolean => |left_bool| {
                switch (right.*) {
                    .Boolean => |right_bool| {
                        return self.evalBooleanInfixExpression(left_bool, right_bool, operator);
                    },
                    else => {
                        return self.runtimeError("type mismatch: {s} <> {s}\n", .{ left.getType(), right.getType() });
                    },
                }
            },
            .String => |left_string| {
                switch (right.*) {
                    .String => |right_string| {
                        return self.evalStringInfixExpression(left_string, right_string, operator);
                    },
                    else => {
                        return self.runtimeError("type mismatch: {s} <> {s}\n", .{ left.getType(), right.getType() });
                    },
                }
            },
            else => {
                return self.runtimeError("unknown left expression in infix expression: {s}\n", .{@tagName(left.*)});
            },
        }
    }

    fn evalStringInfixExpression(self: *Evaluator, left: String, right: String, operator: Operator) !Object {
        return switch (operator) {
            .Plus => {
                const data = try self.gpa.alloc(u8, left.data.len + right.data.len);
                @memcpy(data[0..left.data.len], left.data);
                @memcpy(data[left.data.len..], right.data);
                return Object{ .String = String{ .data = data, .ref_count = 1 } };
            },
            .Eq => {
                return Object{ .Boolean = std.mem.eql(u8, left.data, right.data) };
            },
            .NotEq => {
                return Object{ .Boolean = !std.mem.eql(u8, left.data, right.data) };
            },
            else => {
                self.printError("invalid operator '{f}' for type String\n", .{operator});
                return EvaluatorError.RuntimeError;
            },
        };
    }

    fn evalBooleanInfixExpression(self: *Evaluator, left: bool, right: bool, operator: Operator) !Object {
        return switch (operator) {
            .Eq => Object{ .Boolean = left == right },
            .NotEq => Object{ .Boolean = left != right },
            .Or => Object{ .Boolean = left or right },
            .And => Object{ .Boolean = left and right },
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
            .Percent => Object{ .Float = @rem(left, right) },
            .Gt => Object{ .Boolean = left > right },
            .Lt => Object{ .Boolean = left < right },
            .LtOrEq => Object{ .Boolean = left <= right },
            .GtOrEq => Object{ .Boolean = left >= right },
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
            .Percent => Object{ .Integer = @rem(left, right) },
            .Pipe => Object{ .Integer = left | right },
            .Ampersand => Object{ .Integer = left & right },
            .Caret => Object{ .Integer = left ^ right },
            .LeftShift => Object{ .Integer = left << @intCast(right) },
            .RightShift => Object{ .Integer = left >> @intCast(right) },
            .Gt => Object{ .Boolean = left > right },
            .Lt => Object{ .Boolean = left < right },
            .GtOrEq => Object{ .Boolean = left >= right },
            .LtOrEq => Object{ .Boolean = left <= right },
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
    id: usize = 1,
    gpa: std.mem.Allocator,
    store: std.StringHashMapUnmanaged(Object),
    children: std.AutoArrayHashMapUnmanaged(usize, *Environment),
    child_id_counter: usize,
    outer: ?*Environment,
    debug: bool = false,

    pub fn init(options: struct {
        gpa: std.mem.Allocator,
        id: usize = 1,
        outer: ?*Environment = null,
        debug: bool = false,
    }) !*Environment {
        const env = try options.gpa.create(Environment);
        env.* = Environment{
            .outer = options.outer,
            .gpa = options.gpa,
            .id = options.id,
            .store = .{},
            .children = .{},
            .child_id_counter = 1,
            .debug = options.debug,
        };
        return env;
    }

    pub fn deinit(self: *Environment) void {
        printDebug("deinit env {d}\n", .{self.id}, self.debug);

        // if (self.outer) |outer| {
        //     const result = outer.children.swapRemove(self.id);
        //     _ = result;
        // }

        var child_iterator = self.children.iterator();
        while (child_iterator.next()) |child| {
            child.value_ptr.*.deinit();
        }

        var object_iterator = self.store.valueIterator();
        while (object_iterator.next()) |object| {
            object.deinit(self);
        }

        self.store.deinit(self.gpa);
        self.children.deinit(self.gpa);
        self.gpa.destroy(self);
    }

    pub fn initEnclosed(gpa: std.mem.Allocator, environment: *Environment) !*Environment {
        const child_env = try Environment.init(.{
            .gpa = gpa,
            .outer = environment,
            .id = environment.child_id_counter,
            .debug = environment.debug,
        });

        try environment.children.put(gpa, child_env.id, child_env);
        environment.child_id_counter += 1;

        return child_env;
    }

    pub fn get(self: *Environment, key: []const u8) !Object {
        if (self.store.get(key)) |val| {
            return val;
        }
        if (self.outer) |outer| {
            return outer.get(key);
        }
        printDebug("identifier {s} not found", .{key}, self.debug);
        return EvaluatorError.IdentifierNotFound;
    }

    pub fn set(self: *Environment, key: []const u8, val: Object) !void {
        try self.store.put(self.gpa, key, val);
    }
};
