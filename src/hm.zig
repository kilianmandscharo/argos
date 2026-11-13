const std = @import("std");
const vm = @import("vm.zig");
const ast = @import("ast.zig");
const constants = @import("constants.zig");
const logging = @import("logging.zig");

pub const Type = union(enum) {
    Var: *Var,
    Const: Const,
    Func: *Func,
    List: *const Type,

    pub fn prune(self: *const Type) *const Type {
        switch (self.*) {
            .Var => |v| {
                const link = v.link orelse return self;
                const t = link.prune();
                v.link = t;
                return t;
            },
            else => return self,
        }
    }

    pub fn hasInstance(self: *const Type, class: Class) bool {
        return switch (class) {
            .Plus => switch (self.*) {
                .Const => |c| c == .Integer or c == .Float or c == .String,
                else => false,
            },
            .Num => switch (self.*) {
                .Const => |c| c == .Integer or c == .Float,
                else => false,
            },
            .Show => switch (self.*) {
                .Const => true,
                else => false,
            },
            .Eq => switch (self.*) {
                .Const => true,
                else => false,
            },
            .Ord => switch (self.*) {
                .Const => |c| c == .Integer or c == .Float,
                else => false,
            },
        };
    }

    pub fn format(
        self: @This(),
        writer: *std.Io.Writer,
    ) std.Io.Writer.Error!void {
        const resolved = self.prune();
        switch (resolved.*) {
            .Const => |c| try writer.print("{s}", .{@tagName(c)}),
            .Var => |v| {
                try writer.print("t{d}", .{v.id});
                if (v.classes.count() > 0) {
                    try writer.print(" {{", .{});
                    var first = true;
                    var it = v.classes.iterator();
                    while (it.next()) |class| {
                        if (!first) try writer.print(", ", .{});
                        first = false;
                        try writer.print("{s}", .{@tagName(class)});
                    }
                    try writer.print("}}", .{});
                }
            },
            .List => |l| {
                try writer.print("[", .{});
                try l.format(writer);
                try writer.print("]", .{});
            },
            .Func => |f| {
                try writer.print("(", .{});
                for (f.from, 0..) |param, i| {
                    try param.format(writer);
                    if (i != f.from.len - 1) {
                        try writer.print(", ", .{});
                    }
                }
                try writer.print(") -> ", .{});
                try f.to.format(writer);
            },
        }
    }
};

pub const Func = struct {
    from: []const *const Type,
    to: *const Type,
    is_generic: bool = false,
    resolved: std.ArrayList(*const Type) = .empty,
};

const Const = enum {
    Unit,
    String,
    Integer,
    Float,
    Boolean,
};

pub const Class = enum { Plus, Num, Show, Eq, Ord };

pub const Var = struct {
    id: u32,
    link: ?*const Type = null,
    classes: std.EnumSet(Class),
};

fn countRequired(params: []const Param) usize {
    var count: usize = 0;
    for (params) |param| {
        if (param == .Required) count += 1;
    }
    return count;
}

const Param = ast.FunctionParamType;
const Params = []const Param;

const Declaration = struct {
    scheme: Scheme,
    params: ?Params = null,

    pub fn fromScheme(scheme: Scheme) Declaration {
        return .{ .scheme = scheme };
    }

    pub fn initMono(t: *const Type, params: Params) Declaration {
        return .{ .scheme = .{ .Mono = t }, .params = params };
    }

    pub fn initMonoEmpty(t: *const Type) Declaration {
        return .{ .scheme = .{ .Mono = t }, .params = null };
    }

    pub fn getGenericFuncType(self: Declaration) ?*const Type {
        if (self.scheme == .Poly and self.scheme.Poly.t.* == .Func) {
            return self.scheme.Poly.t;
        }
        return null;
    }
};

const Scheme = union(enum) {
    Mono: *const Type,
    Poly: struct {
        vars: std.ArrayList(*Var),
        t: *const Type,
    },
};

const Env = struct {
    store: std.StringHashMapUnmanaged(Declaration),
    outer: ?*Env,

    pub fn init(arena: std.mem.Allocator, outer: ?*Env) !*Env {
        const env = try arena.create(Env);
        env.* = .{
            .outer = outer,
            .store = .{},
        };
        return env;
    }

    pub fn get(self: *Env, key: []const u8) ?Declaration {
        if (self.store.get(key)) |val| {
            return val;
        }
        if (self.outer) |outer| {
            return outer.get(key);
        }
        return null;
    }

    pub fn set(self: *Env, arena: std.mem.Allocator, key: []const u8, val: Declaration) !void {
        try self.store.put(arena, key, val);
    }
};

pub const TypeChecker = struct {
    arena: std.mem.Allocator,
    scratch_arena: std.heap.ArenaAllocator,
    current_var_id: u32 = 0,
    type_map: TypeMap,
    script_context: *vm.ScriptContext,

    const VarSet = std.AutoArrayHashMapUnmanaged(*Var, void);
    const TypeMap = std.AutoArrayHashMapUnmanaged(*const ast.Expression, *const Type);

    pub fn init(arena: std.mem.Allocator, script_context: *vm.ScriptContext) @This() {
        return .{
            .arena = arena,
            .scratch_arena = std.heap.ArenaAllocator.init(std.heap.page_allocator),
            .type_map = .empty,
            .script_context = script_context,
        };
    }

    pub fn deinit(self: *TypeChecker) void {
        self.scratch_arena.deinit();
    }

    pub inline fn scratch(self: *TypeChecker) std.mem.Allocator {
        return self.scratch_arena.allocator();
    }

    pub fn c(k: Const) *const Type {
        return switch (k) {
            .Unit => &.{ .Const = .Unit },
            .String => &.{ .Const = .String },
            .Integer => &.{ .Const = .Integer },
            .Float => &.{ .Const = .Float },
            .Boolean => &.{ .Const = .Boolean },
        };
    }

    fn newVar(self: *TypeChecker) !*Var {
        const new_var = try self.arena.create(Var);
        new_var.* = .{
            .id = self.current_var_id,
            .classes = .empty,
        };
        self.current_var_id += 1;
        return new_var;
    }

    pub fn checkTypes(self: *TypeChecker, expr: *const ast.Expression) !struct { root: *const Type, type_map: TypeMap } {
        const env = try Env.init(self.scratch(), null);

        try env.set(self.arena, "@assert", .initMono(
            try self.function(&.{c(.Boolean)}, c(.Unit)),
            &.{.Required},
        ));

        try env.set(self.arena, "@print", .initMono(
            try self.function(&.{try self.variable()}, c(.Unit)),
            &.{.Required},
        ));

        try env.set(self.arena, "@len", .initMono(
            try self.function(&.{try self.genericList()}, c(.Integer)),
            &.{.Required},
        ));

        try env.set(self.arena, "@nth", .initMono(
            try self.function(&.{ try self.genericList(), c(.Integer) }, c(.Integer)),
            &.{.Required},
        ));

        const root = try self.infer(env, expr);

        var it = self.type_map.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.* = entry.value_ptr.*.prune();
        }

        return .{ .root = root, .type_map = self.type_map };
    }

    pub fn variable(self: *TypeChecker) !*const Type {
        const v = try self.newVar();
        return try self.createType(.{ .Var = v });
    }

    pub fn list(self: *TypeChecker, t: *const Type) !*const Type {
        return try self.createType(.{ .List = t });
    }

    pub fn genericList(self: *TypeChecker) !*const Type {
        return try self.createType(.{ .List = try self.variable() });
    }

    pub fn function(self: *TypeChecker, from: []const *const Type, to: *const Type) !*const Type {
        const func = try self.arena.create(Func);
        func.* = .{ .from = from, .to = to };
        return try self.createType(.{ .Func = func });
    }

    pub fn createType(self: *TypeChecker, t: Type) !*const Type {
        const allocated = try self.arena.create(Type);
        allocated.* = t;
        return allocated;
    }

    fn copy(self: *TypeChecker, freshVars: std.AutoHashMapUnmanaged(*Var, *const Type), t: *const Type) !*const Type {
        const resolved = t.prune();
        return switch (resolved.*) {
            .Var => |v| {
                return freshVars.get(v) orelse resolved;
            },
            .List => |l| {
                return try self.list(try self.copy(freshVars, l));
            },
            .Func => |f| {
                var from: std.ArrayList(*const Type) = .empty;
                for (f.from) |param| {
                    try from.append(self.arena, try self.copy(freshVars, param));
                }
                const to = try self.copy(freshVars, f.to);
                return try self.function(from.items, to);
            },
            else => resolved,
        };
    }

    fn instantiate(self: *TypeChecker, decl: Declaration) !*const Type {
        return switch (decl.scheme) {
            .Mono => |t| {
                return try self.copy(.empty, t);
            },
            .Poly => |s| {
                var fresh_vars: std.AutoHashMapUnmanaged(*Var, *const Type) = .empty;
                for (s.vars.items) |v| {
                    const fresh_var = try self.variable();
                    fresh_var.Var.classes = v.classes;
                    try fresh_vars.put(self.scratch(), v, fresh_var);
                }
                return try self.copy(fresh_vars, s.t.prune());
            },
        };
    }

    fn freeVars(self: *TypeChecker, out: *VarSet, t: *const Type) !void {
        switch (t.prune().*) {
            .Var => |v| {
                try out.put(self.scratch(), v, {});
            },
            .Func => |f| {
                for (f.from) |param| {
                    try self.freeVars(out, param);
                }
                try self.freeVars(out, f.to);
            },
            .List => |l| try self.freeVars(out, l),
            .Const => {},
        }
    }

    fn freeVarsFromEnv(self: *TypeChecker, env: *Env, out: *VarSet) !void {
        var current: ?*const Env = env;
        while (current) |e| : (current = e.outer) {
            var it = e.store.valueIterator();
            while (it.next()) |decl| {
                switch (decl.scheme) {
                    .Mono => |m| try self.freeVars(out, m),
                    .Poly => |poly| {
                        var tmp: VarSet = .empty;
                        try self.freeVars(&tmp, poly.t);
                        for (poly.vars.items) |v| _ = tmp.swapRemove(v);
                        for (tmp.keys()) |v| try out.put(self.scratch(), v, {});
                    },
                }
            }
        }
    }

    fn generalize(self: *TypeChecker, env: *Env, t: *const Type) !Scheme {
        var varsFromEnv: VarSet = .empty;
        try self.freeVarsFromEnv(env, &varsFromEnv);

        var freeVarsFromType: VarSet = .empty;
        try self.freeVars(&freeVarsFromType, t);

        var vars: std.ArrayList(*Var) = .empty;

        for (freeVarsFromType.keys()) |v| {
            if (!varsFromEnv.contains(v)) {
                try vars.append(self.scratch(), v);
            }
        }

        return if (vars.items.len > 0) .{ .Poly = .{ .vars = vars, .t = t } } else .{ .Mono = t };
    }

    fn occurs(v: *Var, t: *const Type) bool {
        switch (t.prune().*) {
            .Var => |other| return other == v,
            .Func => |f| {
                for (f.from) |param| {
                    if (occurs(v, param)) return true;
                }
                return occurs(v, f.to);
            },
            .List => |l| return occurs(v, l),
            else => return false,
        }
    }

    fn constrain(t: *const Type, class: Class) !void {
        const resolved = t.prune();
        switch (resolved.*) {
            .Var => |v| v.classes.insert(class),
            else => if (!resolved.hasInstance(class)) return error.MissingInstance,
        }
    }

    fn unify(self: *TypeChecker, type_a: *const Type, type_b: *const Type) !void {
        const a = type_a.prune();
        const b = type_b.prune();

        if (comptime constants.debug_type_checker) {
            logging.log("Unify {f} with {f}", .{ a, b }, .{ .module = "TypeChecker" });
        }

        if (a.* == .Var and b.* == .Var and a.Var == b.Var) return;

        if (a.* == .Var) {
            if (occurs(a.Var, b)) return error.InfiniteType;
            switch (b.*) {
                .Var => |other| other.classes.setUnion(a.Var.classes),
                else => {
                    var it = a.Var.classes.iterator();
                    while (it.next()) |class| if (!b.hasInstance(class)) return error.MissingInstance;
                },
            }
            a.Var.link = b;
        } else if (b.* == .Var) {
            try self.unify(b, a);
        } else if (a.* == .Const and b.* == .Const and @backingInt(a.Const) == @backingInt(b.Const)) {
            return;
        } else if (a.* == .Func and b.* == .Func) {
            for (0..a.Func.from.len) |i| {
                try self.unify(a.Func.from[i], b.Func.from[i]);
            }
            try self.unify(a.Func.to, b.Func.to);
        } else if (a.* == .List and b.* == .List) {
            try self.unify(a.List, b.List);
        } else {
            return error.CantUnify;
        }
    }

    fn infer(self: *TypeChecker, env: *Env, expr: *const ast.Expression) anyerror!*const Type {
        if (comptime constants.debug_type_checker) {
            logging.log("infer {s}", .{@tagName(expr.data)}, .{ .module = "TypeChecker" });
        }
        const t = try self.inferType(env, expr);
        try self.type_map.put(self.arena, expr, t);
        return t;
    }

    fn mustResolveIdentifier(self: *TypeChecker, env: *Env, expr: *const ast.Expression, name: []const u8) !Declaration {
        if (env.get(name)) |decl| {
            return decl;
        }
        expr.token.printError("unbound var", self.script_context, "TypeChecker");
        return error.UnboundVar;
    }

    fn inferFunction(self: *TypeChecker, env: *Env, f: *const ast.Function) !struct { t: *const Type, params: Params } {
        var params: std.ArrayList(*const Type) = .empty;
        var decl_params: std.ArrayList(Param) = .empty;
        var found_default = false;

        for (f.params.items) |param| {
            switch (param) {
                .Required => |name| {
                    if (found_default) return error.DefaultBeforeRequiredParam;
                    const fresh_var = try self.variable();
                    const scheme: Scheme = .{
                        .Mono = fresh_var,
                    };
                    try env.set(self.scratch(), name.data, .fromScheme(scheme));
                    try params.append(self.arena, fresh_var);
                    try decl_params.append(self.scratch(), .Required);
                },
                .Default => |default| {
                    const t = switch (default.value.data) {
                        .String, .Integer, .Float, .Boolean, .Unit, .List => try self.infer(env, default.value),
                        else => return error.NotSupportedInDefaultParam,
                    };
                    const fresh_var = try self.variable();
                    try self.unify(fresh_var, t);
                    const scheme: Scheme = .{
                        .Mono = fresh_var,
                    };
                    try env.set(self.scratch(), default.name.data, .fromScheme(scheme));
                    try params.append(self.arena, fresh_var);
                    found_default = true;
                    try decl_params.append(self.scratch(), .Default);
                },
            }
        }

        return .{ .t = try self.function(params.items, try self.infer(env, f.body)), .params = decl_params.items };
    }

    fn inferCallWithParams(self: *TypeChecker, env: *Env, func_t: *Func, args: []const *const ast.Expression, param_types: Params) !*const Type {
        if (args.len < countRequired(param_types) or args.len > param_types.len) return error.ArityMismatch;
        for (args, 0..) |arg, i| {
            const arg_type = try self.infer(env, arg);
            try self.unify(arg_type, func_t.from[i]);
        }
        const r = try self.variable();
        try self.unify(func_t.to, r);
        return r;
    }

    fn inferCall(self: *TypeChecker, env: *Env, expr: *const ast.Expression, call: *const ast.Call) !*const Type {
        const args = call.args.items;

        switch (call.function.data) {
            .Identifier => |name| {
                const decl = try self.mustResolveIdentifier(env, expr, name);
                const generic_func_type = decl.getGenericFuncType();

                // either a function or in a recursive call an unknown variable
                const func_t = (try self.infer(env, call.function)).prune();
                var return_t: *const Type = undefined;

                if (decl.params) |params| {
                    return_t = try self.inferCallWithParams(env, func_t.Func, args, params);
                } else {
                    var params: std.ArrayList(*const Type) = .empty;
                    for (args) |arg| try params.append(self.arena, try self.infer(env, arg));
                    return_t = try self.variable();
                    try self.unify(func_t, try self.function(params.items, return_t));
                }

                if (generic_func_type) |original_f| {
                    try original_f.Func.resolved.append(self.arena, func_t);
                    func_t.Func.is_generic = true;
                }

                return return_t.prune();
            },
            .Function => |f| {
                const func_t = (try self.infer(env, call.function)).Func;
                const param_types = try f.getParamTypes(self.scratch());
                return try self.inferCallWithParams(env, func_t, args, param_types);
            },
            else => return error.ExpressionNotCallable,
        }
    }

    fn inferType(self: *TypeChecker, env: *Env, expr: *const ast.Expression) !*const Type {
        return switch (expr.data) {
            .Call => |call| try self.inferCall(env, expr, &call),
            .Declaration => |declaration| {
                if (declaration.right.data == .Function) {
                    const inner_env = try Env.init(self.scratch(), env);
                    try inner_env.set(self.scratch(), declaration.left.data, .initMonoEmpty(try self.variable())); // for recursive calls
                    const result = try self.inferFunction(inner_env, &declaration.right.data.Function);
                    try self.type_map.put(self.arena, expr, result.t);
                    const scheme = try self.generalize(env, result.t);
                    try env.set(self.scratch(), declaration.left.data, .{ .scheme = scheme, .params = result.params });
                } else {
                    const t = try self.infer(env, declaration.right);
                    const scheme = try self.generalize(env, t);
                    try env.set(self.scratch(), declaration.left.data, .fromScheme(scheme));
                }
                return c(.Unit);
            },
            .Unary => |unary| {
                const t = try self.infer(env, unary.right);
                switch (unary.operator) {
                    .Minus => {
                        try constrain(t, .Num);
                        return t;
                    },
                    .Bang => {
                        try self.unify(t, c(.Boolean));
                        return c(.Boolean);
                    },
                    .Tilde => {
                        try self.unify(t, c(.Integer));
                        return c(.Integer);
                    },
                    else => return error.InvalidUnaryOperator,
                }
            },
            .Binary => |binary| {
                const lt = try self.infer(env, binary.left);
                const rt = try self.infer(env, binary.right);
                switch (binary.operator) {
                    .And, .Or => {
                        try self.unify(lt, c(.Boolean));
                        try self.unify(rt, c(.Boolean));
                        return c(.Boolean);
                    },
                    .Plus => {
                        try self.unify(lt, rt);
                        try constrain(lt, .Plus);
                        return lt;
                    },
                    .Minus, .Asterisk, .Slash, .Percent => {
                        try self.unify(lt, rt);
                        try constrain(lt, .Num);
                        return lt;
                    },
                    .Eq, .NotEq => {
                        try self.unify(lt, rt);
                        try constrain(lt, .Eq);
                        return c(.Boolean);
                    },
                    .Gt, .GtOrEq, .Lt, .LtOrEq => {
                        try self.unify(lt, rt);
                        try constrain(lt, .Ord);
                        return c(.Boolean);
                    },
                    .Ampersand, .Pipe, .Caret, .LeftShift, .RightShift => {
                        try self.unify(lt, c(.Integer));
                        try self.unify(rt, c(.Integer));
                        return c(.Integer);
                    },
                    .Cons => {
                        const list_t = try self.list(lt);
                        try self.unify(rt, list_t);
                        return list_t;
                    },
                    else => return error.InvalidBinaryOperator,
                }
            },
            .Grouping => |grouping| {
                const inner_env = try Env.init(self.scratch(), env);
                for (grouping.items[0 .. grouping.items.len - 1]) |e| {
                    _ = try self.infer(inner_env, e);
                }
                return try self.infer(inner_env, grouping.items[grouping.items.len - 1]);
            },
            .List => |l| {
                if (l.items.len == 0) return try self.genericList();
                const t = try self.infer(env, l.items[0]);
                for (l.items[1..l.items.len]) |e| {
                    try self.unify(t, try self.infer(env, e));
                }
                return try self.list(t);
            },
            .Match => |match| {
                const target_t = try self.infer(env, match.target);
                const result = try self.variable();
                for (match.arms.items) |arm| {
                    switch (arm.pattern.data) {
                        .Identifier => |name| {
                            if (std.mem.eql(u8, name, "_")) {
                                try self.unify(try self.infer(env, arm.expression), result);
                                continue;
                            }
                            return error.NotSupported;
                        },
                        .Binary => |binary| {
                            switch (binary.operator) {
                                .Cons => {
                                    try self.unify(target_t, try self.genericList());
                                    const inner_env = try Env.init(self.scratch(), env);
                                    if (!std.mem.eql(u8, binary.left.data.Identifier, "_")) {
                                        const fresh_var = try self.variable();
                                        try self.unify(fresh_var, target_t.prune().List);
                                        try inner_env.set(self.scratch(), binary.left.data.Identifier, .initMonoEmpty(fresh_var));
                                    }
                                    if (!std.mem.eql(u8, binary.right.data.Identifier, "_")) {
                                        const fresh_var = try self.variable();
                                        try self.unify(fresh_var, target_t.prune());
                                        try inner_env.set(self.scratch(), binary.right.data.Identifier, .initMonoEmpty(fresh_var));
                                    }
                                    const t = try self.infer(inner_env, arm.expression);
                                    try self.unify(t, result);
                                },
                                else => return error.NotSupported,
                            }
                        },
                        else => {
                            try self.unify(target_t, try self.infer(env, arm.pattern));
                            try self.unify(try self.infer(env, arm.expression), result);
                        },
                    }
                }
                return result;
            },
            .If => |ifExpr| {
                const result = try self.variable();
                for (ifExpr.arms.items) |arm| {
                    try self.unify(try self.infer(env, arm.condition), c(.Boolean));
                    try self.unify(try self.infer(env, arm.then), result);
                }
                if (ifExpr.alt) |alt| {
                    try self.unify(try self.infer(env, alt), result);
                } else {
                    try self.unify(result, c(.Unit));
                }
                return result;
            },
            .Identifier => |name| {
                return try self.instantiate(try self.mustResolveIdentifier(env, expr, name));
            },
            .Function => |f| {
                const inner_env = try Env.init(self.scratch(), env);
                const result = try self.inferFunction(inner_env, &f);
                try self.type_map.put(self.arena, expr, result.t);
                return result.t;
            },
            .String => c(.String),
            .Integer => c(.Integer),
            .Float => c(.Float),
            .Boolean => c(.Boolean),
            .Unit => c(.Unit),
        };
    }
};
