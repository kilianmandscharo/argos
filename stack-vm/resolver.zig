const std = @import("std");
const ast = @import("ast.zig");
const scanner = @import("scanner.zig");
const vm = @import("vm.zig");

pub const TypeInfo = enum {
    Float,
    Int,
    Bool,
    Null,
    String,
    Function,
    List,
    Table,
    Unknown,
};

pub const Resolver = struct {
    scope: *Scope,
    globals: std.AutoHashMapUnmanaged([]const u8, VariableInfo),
    expression_types: std.AutoHashMapUnmanaged(*const ast.Expression, TypeInfo),
    variable_bindings: std.AutoHashMapUnmanaged(*[]const u8, VariableInfo),
    script_context: *vm.ScriptContext,
    arena: std.mem.Allocator,

    const VariableInfo = struct {
        type_info: TypeInfo,
        is_const: bool,
        is_used: bool,
        is_captured: bool,
        token: scanner.Token,
        depth: u16,
    };

    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    const Scope = struct {
        enclosing: ?*Scope,
        locals: std.ArrayList(VariableInfo),
        upvalues: std.ArrayList(Upvalue),
    };

    fn resolveLocal(scope: *const Scope, name: *const scanner.Token) ?struct { index: u8, local: *VariableInfo } {
        for (scope.locals.items, 0..) |local, i| {
            if (std.mem.eql(u8, name.data, local.token.data)) {
                return .{ .index = @intCast(i), .local = &local };
            }
        }
        return null;
    }

    fn resolveUpvalue(self: *Resolver, scope: *const Scope, name: scanner.Token) ?struct { index: u8, local: *VariableInfo } {
        if (scope.enclosing == null) return null;
        const enclosing = scope.enclosing.?;

        if (resolveLocal(enclosing, name)) |result| {
            enclosing.locals.items[result.index].is_captured = true;
            return .{
                .index = try self.addUpvalue(enclosing, @intCast(result.index), true),
                .local = result.local,
            };
        }

        if (self.resolveUpvalue(enclosing, name)) |result| {
            return .{
                .index = try self.addUpvalue(enclosing, result.index, false),
                .local = result.local,
            };
        }

        return null;
    }

    fn addUpvalue(self: *Resolver, scope: *const Scope, name: *const scanner.Token, index: u8, is_local: bool) !u8 {
        for (scope.upvalues.items, 0..) |upvalue, i| {
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(i);
            }
        }

        if (scope.upvalues.items.len == std.math.maxInt(u8) + 1) {
            return self.errorAt(name, "Too many closure variables in function.");
        }

        try scope.upvalues.append(self.arena, .{
            .is_local = is_local,
            .index = index,
        });

        return scope.upvalues.items.len;
    }

    fn errorAt(self: *Resolver, token: scanner.Token, message: []const u8) anyerror {
        token.printError(message, self.script_context, "Resolver");
        return error.ResolveError;
    }

    fn resolveStatement(self: *Resolver, stmt: ast.Statement) !void {
        switch (stmt) {
            .VarDeclaration => |val| {
                const is_global = self.scope.enclosing == null;
                const type_info = self.resolveExpression(val.expression);

                if (is_global) {
                    if (self.globals.get(val.name.data)) |_| {
                        return self.errorAt(val.name, "Already a global variable with this name.");
                    }
                    try self.globals.put(self.arena, val.name.data, .{
                        .type_info = type_info,
                        .is_const = false,
                        .is_used = false,
                        .is_captured = false,
                        .token = val.name,
                    });
                } else {
                    if (resolveLocal(self.scope, &val.name)) |_| {
                        return self.errorAt(val.name, "Already a variable with this name in this scope.");
                    }
                    if (self.scope.locals.items.len == std.math.maxInt(u8) + 1) {
                        return self.errorAt(val.name, "Too many local variables in scope.");
                    }
                    try self.scope.locals.append(self.arena, .{
                        .type_info = type_info,
                        .is_const = false,
                        .is_used = false,
                        .is_captured = false,
                        .token = val.name,
                    });
                }
            },
            .Assignment => {},
            .For => {},
            .While => {},
            .Return => {},
            .Expression => {},
        }
    }

    fn resolveExpression(self: *Resolver, expr: *const ast.Expression) !TypeInfo {
        if (self.expression_types.get(expr)) |t| return t;

        const t = switch (expr.data) {
            .Identifier => blk: {
                if (resolveLocal(self.scope, &expr.token)) |result| {
                    break :blk result.local.type_info;
                }
                if (self.resolveUpvalue(self.scope, &expr.token)) |result| {
                    break :blk result.local.type_info;
                }
                if (self.globals.get(expr.token.data)) |var_info| {
                    break :blk var_info.type_info;
                }
                return self.errorAt(&expr.token, "identifier not found");
            },
            .String => .String,
            .Integer => .Int,
            .Float => .Float,
            .Boolean => .Bool,
            .Infix => .Unknown,
            .Prefix => .Unknown,
            .Function => .Unknown,
            .Call => .Unknown,
            .Range => .Unknown,
            .List => .List,
            .Table => .Table,
            .Index => .Unknown,
            .Match => .Unknown,
            .Null => .Null,
            .Block => .Unknown,
        };

        try self.expression_types.put(self.arena, expr, t);

        return t;
    }
};
