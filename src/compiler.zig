const std = @import("std");
const scanner = @import("scanner.zig");
const chunk = @import("chunk.zig");
const value = @import("value.zig");
const object = @import("object.zig");
const virtual_machine = @import("vm.zig");
const logging = @import("logging.zig");
const memory = @import("memory.zig");
const constants = @import("constants.zig");

fn logDebug(comptime fmt: []const u8, args: anytype) void {
    logging.log(fmt, args, .{
        .module = "Compiler",
    });
}

const Precedence = enum(u8) {
    Lowest = 1,
    Assign,
    LogicalOr,
    LogicalAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseAnd,
    Equals,
    LessGreater,
    Shift,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
};

const FunctionType = enum {
    Function,
    Script,
};

pub const Parser = struct {
    current: scanner.Token,
    previous: scanner.Token,
    scanner: *scanner.Scanner,

    pub fn init(scanner_instance: *scanner.Scanner) @This() {
        return Parser{
            .current = undefined,
            .previous = undefined,
            .scanner = scanner_instance,
        };
    }

    pub fn advance(self: *Parser) !void {
        self.previous = self.current;
        self.current = self.scanner.next();
    }
};

pub const Compiler = struct {
    parser: *Parser,
    gpa: std.mem.Allocator,
    vm: *virtual_machine.VirtualMachine,
    locals: [UINT8_COUNT]Local,
    local_count: u32,
    scope_depth: u32,
    function: ?*object.ObjFunction,
    type: FunctionType,
    current_var: ?scanner.Token,
    enclosing: ?*Compiler,
    upvalues: [UINT8_COUNT]Upvalue,
    indent: usize,

    const Local = struct {
        name: scanner.Token,
        depth: ?u32,
        is_captured: bool,
    };

    const Upvalue = struct {
        index: u8,
        is_local: bool,
    };

    const UINT8_COUNT = std.math.maxInt(u8) + 1;

    pub fn init(
        self: *Compiler,
        vm: *virtual_machine.VirtualMachine,
        gpa: std.mem.Allocator,
        parser: *Parser,
        func_type: FunctionType,
        enclosing: ?*Compiler,
        current_var: ?scanner.Token,
        indent: usize,
    ) !void {
        self.parser = parser;
        self.gpa = gpa;
        self.vm = vm;
        self.locals = undefined;
        self.scope_depth = 0;
        self.local_count = 0;
        self.type = func_type;
        self.current_var = null;
        self.enclosing = enclosing;
        self.upvalues = undefined;
        self.indent = indent;
        self.function = null;

        // Set current_compiler BEFORE any allocations that could trigger GC
        vm.current_compiler = self;

        self.function = try object.allocateFunction(vm);

        const local = &self.locals[self.local_count];
        self.local_count += 1;
        local.depth = 0;
        local.is_captured = false;
        local.name.source = "";
        local.name.start = 0;
        local.name.length = 0;

        if (func_type != .Script) {
            if (current_var) |curr| {
                const start = curr.start;
                const end = start + curr.length;
                const slice = curr.source[start..end];
                const obj = try object.copyStaticString(vm, slice);
                self.function.?.name = obj.asString();
            }
        }
    }

    fn getFunctionName(self: *Compiler) []const u8 {
        return if (self.function.?.name) |name| name.chars else "script";
    }

    fn log(self: *Compiler, comptime format: []const u8, args: anytype) void {
        if (!constants.debug_print_steps) return;
        const name = self.getFunctionName();
        logging.log(
            format,
            args,
            .{ .indent = self.indent, .module = name },
        );
    }

    pub fn compile(self: *Compiler) !*object.ObjFunction {
        try self.parser.advance();
        // TODO: we stop compilation on the first error, which makes sense for
        // degugging, but how to best handle?
        while (!try self.match(.Eof)) {
            self.declaration() catch |err| {
                self.currentChunk().disassemble("<error>");
                return err;
            };
        }
        return try self.endCompiler();
    }

    fn endCompiler(self: *Compiler) !*object.ObjFunction {
        try self.emitReturn();
        self.vm.current_compiler = self.enclosing;
        const function = self.function;
        if (comptime constants.debug_disassemble) {
            const name = self.getFunctionName();
            logDebug("Finished compiling {s}", .{name});
            self.currentChunk().disassemble(name);
        }
        return function.?;
    }

    fn emitReturn(self: *Compiler) !void {
        try self.emitOpCode(.Null);
        try self.emitOpCode(.Return);
    }

    fn parsePrecedence(self: *Compiler, precedence: Precedence) !void {
        try self.parser.advance();

        self.log("expression on {s}", .{self.parser.previous.toString()});

        if (getRule(self.parser.previous.type).prefix) |prefixFn| {
            const can_assign = @intFromEnum(precedence) < @intFromEnum(Precedence.Assign);
            try prefixFn(self, can_assign);

            while (@intFromEnum(precedence) < getRulePrecedenceValue(self.parser.current.type)) {
                try self.parser.advance();
                if (self.parser.current.type == .Eof) return;
                if (getRule(self.parser.previous.type).infix) |infixFn| {
                    try infixFn(self);
                }
            }

            if (!can_assign and try self.match(.Assign)) {
                return self.errorAtPrevious("Invalid assignment target.");
            }
        } else {
            return self.errorAtPrevious("Expect expression.");
        }

        self.log("end expression", .{});
    }

    fn isLineEnd(self: *Compiler) bool {
        const token_type = self.parser.current.type;
        return token_type == .NewLine or token_type == .Eof;
    }

    fn expectLineEnd(self: *Compiler) !void {
        if (self.isLineEnd()) {
            try self.parser.advance();
            return;
        }
        return self.errorAtCurrent("Expected line end.");
    }

    fn declaration(self: *Compiler) !void {
        try self.chopNewlines();
        if (try self.match(.Let)) {
            try self.varDeclaration();
        } else {
            try self.statement();
        }
    }

    fn varDeclaration(self: *Compiler) !void {
        self.log("var declaration", .{});
        defer self.log("end var declaration", .{});

        const global = try self.parseVariable("Expect variable name.");
        if (try self.match(.Assign)) {
            if (self.check(.Fn)) {
                self.markInitialized();
            }
            try self.expression();
        } else {
            try self.emitOpCode(.Null);
        }
        _ = try self.match(.NewLine);
        try self.defineVariable(global);
    }

    fn defineVariable(self: *Compiler, global: usize) !void {
        self.current_var = null;
        if (self.scope_depth > 0) {
            self.markInitialized();
            return;
        }
        try self.emitOpCode(.DefineGlobal);
        try self.emitU24(global);
    }

    fn markInitialized(self: *Compiler) void {
        if (self.scope_depth == 0) return;
        self.locals[self.local_count - 1].depth = self.scope_depth;
    }

    fn parseVariable(self: *Compiler, message: []const u8) !usize {
        try self.consume(.Identifier, message);
        self.current_var = self.parser.previous;

        try self.declareVariable();
        if (self.scope_depth > 0) return 0;

        return try self.identifierConstant(&self.parser.previous);
    }

    fn declareVariable(self: *Compiler) !void {
        if (self.scope_depth == 0) return;
        var name = self.parser.previous;

        if (self.local_count > 0) {
            var i = self.local_count;
            while (i > 0) : (i -= 1) {
                var local = self.locals[i - 1];
                if (local.depth != null and local.depth.? < self.scope_depth) {
                    break;
                }
                if (Compiler.identifiersEqual(&name, &local.name)) {
                    return self.errorAtPrevious("Already a variable with this name in this scope.");
                }
            }
        }

        try self.addLocal(name);
    }

    fn addLocal(self: *Compiler, name: scanner.Token) !void {
        if (self.local_count == std.math.maxInt(u8) + 1) {
            return self.errorAtPrevious("Too many local variables in block.");
        }
        const local = &self.locals[self.local_count];
        self.local_count += 1;
        local.name = name;
        local.depth = null;
        local.is_captured = false;
    }

    fn identifierConstant(self: *Compiler, name: *scanner.Token) !usize {
        const start = name.start;
        const end = name.start + name.length;
        const slice = name.source[start..end];
        const obj = try object.copyStaticString(self.vm, slice);
        return try self.makeConstant(value.wrapObj(obj));
    }

    fn statement(self: *Compiler) !void {
        if (try self.match(.Print)) {
            try self.printStatement();
        } else if (try self.match(.Assert)) {
            try self.assertStatement();
        } else if (try self.match(.Match)) {
            try self.matchStatement();
        } else if (try self.match(.While)) {
            try self.whileStatement();
        } else if (try self.match(.For)) {
            try self.forStatement();
        } else if (try self.match(.Return)) {
            try self.returnStatement();
        } else if (try self.match(.LBrace)) {
            self.beginScope();
            try self.blockStatement();
            try self.endScope();
        } else {
            try self.expressionStatement();
        }
    }

    fn returnStatement(self: *Compiler) !void {
        self.log("return statement", .{});
        defer self.log("end return statement", .{});

        if (self.type == .Script) {
            return self.errorAtPrevious("Can't return from top-level code.");
        }
        if (self.isLineEnd()) {
            try self.parser.advance();
            try self.emitReturn();
        } else {
            try self.expression();
            try self.expectLineEnd();
            try self.emitOpCode(.Return);
        }
    }

    fn whileStatement(self: *Compiler) anyerror!void {
        self.log("while statement", .{});
        defer self.log("end while statement", .{});

        const loop_start = self.currentChunk().code.items.len;
        try self.consume(.LParen, "Expect '(' after 'while'");
        try self.expression();
        try self.consume(.RParen, "Expect ')' after condition");

        const exit_jump = try self.emitJump(.JumpIfFalse);
        try self.emitOpCode(.Pop);
        try self.statement();
        try self.emitLoop(loop_start);

        try self.patchJump(exit_jump);
        try self.emitOpCode(.Pop);
    }

    fn forStatement(self: *Compiler) anyerror!void {
        self.log("for statement", .{});
        defer self.log("end for statement", .{});

        self.beginScope();

        try self.consume(.LParen, "Expect '(' after 'for'");
        try self.expression();
        try self.consume(.DotDot, "Expect '..' after expression");
        try self.expression();
        try self.consume(.RParen, "Expect ')' after range");

        try self.consume(.Pipe, "Expect '|' after loop range");
        try self.consume(.Identifier, "Expect identifier in loop capture");
        try self.declareVariable();
        self.markInitialized();
        try self.consume(.Pipe, "Expect '|' after variable capture");

        const increment_var_index = self.local_count - 1;

        // a dummy local for the right side of the range
        try self.addLocal(.{
            .source = "",
            .type = .Identifier,
            .start = 0,
            .length = 0,
            .line = 0,
        });
        self.markInitialized();

        const loop_start = self.currentChunk().code.items.len;

        const exit_jump = try self.emitJump(.JumpIfGreaterOrEq);
        try self.statement();

        try self.emitOpCode(.GetLocal);
        try self.emitU24(increment_var_index);
        try self.emitConstant(value.wrapInt(1));
        try self.emitOpCode(.Add);
        try self.emitOpCode(.SetLocal);
        try self.emitU24(increment_var_index);
        try self.emitOpCode(.Pop);

        try self.emitLoop(loop_start);

        try self.patchJump(exit_jump);

        try self.endScope();
    }

    fn emitLoop(self: *Compiler, loop_start: usize) !void {
        try self.emitOpCode(.Loop);

        const offset = self.currentChunk().code.items.len - loop_start + 2;
        if (offset > std.math.maxInt(u16)) {
            return self.errorAtPrevious("Loop body too large.");
        }

        try self.emitU16(offset);
    }

    fn matchStatement(self: *Compiler) anyerror!void {
        self.log("match statement", .{});
        defer self.log("end match statement", .{});

        var has_target = false;

        if (self.check(.LParen)) {
            has_target = true;
            try self.parser.advance();
            try self.expression();
            try self.consume(.RParen, "Expect ')' after match target");
        }

        const instruction: chunk.OpCode = if (has_target) .JumpIfNotEq else .JumpIfFalse;

        if (!self.check(.LBrace)) {
            try self.expression();
            try self.consume(.Arrow, "Expect '->' after match expression");

            const then_jump = try self.emitJump(instruction);
            try self.statement();
            try self.patchJump(then_jump);
            try self.emitOpCode(.Pop);

            if (has_target) try self.emitOpCode(.Pop);

            return;
        }

        try self.parser.advance();
        try self.consume(.NewLine, "Expect new line after '{' in match block");

        var else_jumps: std.ArrayList(usize) = .{};
        errdefer else_jumps.deinit(self.gpa);

        while (!self.check(.Eof) and !self.check(.RBrace)) {
            try self.chopNewlines();

            if (self.check(.Else)) break;

            try self.expression();
            try self.consume(.Arrow, "Expect '->' after match expression");

            const then_jump = try self.emitJump(instruction);
            try self.emitOpCode(.Pop);
            try self.statement();
            try else_jumps.append(self.gpa, try self.emitJump(.Jump));
            try self.patchJump(then_jump);
            try self.emitOpCode(.Pop);
        }

        // TODO: what if the else branch is not the last?
        if (self.check(.Else)) {
            try self.parser.advance();
            try self.consume(.Arrow, "Expect '->' after else in match expression");
            try self.statement();
        }

        for (else_jumps.items) |jump| {
            try self.patchJump(jump);
        }

        if (has_target) try self.emitOpCode(.Pop);

        try self.consume(.RBrace, "Expect '}' at the end of match block");
        try self.consume(.NewLine, "Expect new line after match block");

        else_jumps.deinit(self.gpa);
    }

    fn emitJump(self: *Compiler, instruction: chunk.OpCode) !usize {
        try self.emitOpCode(instruction);
        try self.emitByte(0xff);
        try self.emitByte(0xff);
        return self.currentChunk().code.items.len - 2;
    }

    fn patchJump(self: *Compiler, offset: usize) !void {
        const cur_chunk = self.currentChunk();
        const jump = cur_chunk.code.items.len - offset - 2;
        if (jump > std.math.maxInt(u16)) {
            return self.errorAtPrevious("Too much code to jump over.");
        }
        const bytes = chunk.indexToU16(jump);
        cur_chunk.code.items[offset] = bytes[0];
        cur_chunk.code.items[offset + 1] = bytes[1];
    }

    fn addUpvalue(self: *Compiler, index: u8, is_local: bool) !u8 {
        const upvalue_count = self.function.?.upvalue_count;

        for (0..upvalue_count) |i| {
            const upvalue = &self.upvalues[i];
            if (upvalue.index == index and upvalue.is_local == is_local) {
                return @intCast(i);
            }
        }

        if (upvalue_count == UINT8_COUNT) {
            return self.errorAtPrevious("Too many closure variables in function.");
        }

        self.upvalues[upvalue_count].is_local = is_local;
        self.upvalues[upvalue_count].index = index;
        const retVal = self.function.?.upvalue_count;
        self.function.?.upvalue_count += 1;
        return retVal;
    }

    fn resolveUpvalue(self: *Compiler, name: *scanner.Token) !?u8 {
        if (self.enclosing == null) return null;

        const enclosing = self.enclosing.?;
        const local = try enclosing.resolveLocal(name);
        if (local) |index| {
            enclosing.locals[index].is_captured = true;
            return try self.addUpvalue(@intCast(index), true);
        }

        const upvalue = try self.enclosing.?.resolveUpvalue(name);
        if (upvalue) |index| return try self.addUpvalue(index, false);

        return null;
    }

    fn chopNewlines(self: *Compiler) !void {
        while (self.check(.NewLine)) {
            try self.parser.advance();
        }
    }

    fn blockStatement(self: *Compiler) anyerror!void {
        self.log("block statement", .{});
        defer self.log("end block statement", .{});

        while (!self.check(.RBrace) and !self.check(.Eof)) {
            try self.declaration();
        }
        try self.consume(.RBrace, "Expect '}' after block.");
        try self.consume(.NewLine, "Expect '\n' after block.");
    }

    fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }

    fn endScope(self: *Compiler) !void {
        self.scope_depth -= 1;
        while (self.local_count > 0 and
            self.locals[self.local_count - 1].depth != null and
            self.locals[self.local_count - 1].depth.? > self.scope_depth)
        {
            if (self.locals[self.local_count - 1].is_captured) {
                try self.emitOpCode(.CloseUpvalue);
            } else {
                try self.emitOpCode(.Pop);
            }
            self.local_count -= 1;
        }
    }

    fn resolveLocal(self: *Compiler, name: *scanner.Token) !?usize {
        if (self.local_count == 0) return null;
        var i = self.local_count;
        while (i > 0) : (i -= 1) {
            const local = &self.locals[i - 1];
            if (Compiler.identifiersEqual(name, &local.name)) {
                if (local.depth == null) {
                    return self.errorAtPrevious("Can't read local variable in its own initializer.");
                }
                return i - 1;
            }
        }
        return null;
    }

    fn identifiersEqual(a: *scanner.Token, b: *scanner.Token) bool {
        const a_string = a.toString();
        const b_string = b.toString();
        if (a_string.len != b_string.len) return false;
        return std.mem.eql(u8, a_string, b_string);
    }

    fn printStatement(self: *Compiler) !void {
        self.log("print statement", .{});
        defer self.log("end print statement", .{});

        try self.expression();
        try self.expectLineEnd();
        try self.emitOpCode(.Print);
    }

    fn assertStatement(self: *Compiler) !void {
        self.log("assert statement", .{});
        defer self.log("end assert statement", .{});

        try self.expression();
        try self.expectLineEnd();
        try self.emitOpCode(.Assert);
    }

    fn expressionStatement(self: *Compiler) !void {
        self.log("expression statement", .{});
        defer self.log("end expression statement", .{});

        try self.expression();
        try self.expectLineEnd();
        try self.emitOpCode(.Pop);
    }

    fn expression(self: *Compiler) !void {
        try self.parsePrecedence(.Lowest);
    }

    fn emitConstant(self: *Compiler, val: value.Value) !void {
        try self.currentChunk().writeConstant(self.gpa, val, self.parser.previous.line);
    }

    fn makeConstant(self: *Compiler, val: value.Value) !usize {
        return try self.currentChunk().addConstant(self.gpa, val);
    }

    fn currentChunk(self: *Compiler) *chunk.Chunk {
        return &self.function.?.chunk;
    }

    fn emitBytes(self: *Compiler, first: chunk.OpCode, second: u8) !void {
        try self.emitOpCode(first);
        try self.emitByte(second);
    }

    fn emitOpByte(self: *Compiler, op_byte: chunk.OpByte) !void {
        try self.currentChunk().write(self.gpa, op_byte, self.parser.previous.line);
    }

    fn emitByte(self: *Compiler, byte: u8) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Byte = byte }, self.parser.previous.line);
    }

    fn emitU24(self: *Compiler, index: usize) !void {
        const bytes = chunk.indexToU24(index);
        try self.emitByte(bytes[0]);
        try self.emitByte(bytes[1]);
        try self.emitByte(bytes[2]);
    }

    fn emitU16(self: *Compiler, index: usize) !void {
        const bytes = chunk.indexToU16(index);
        try self.emitByte(bytes[0]);
        try self.emitByte(bytes[1]);
    }

    fn emitOpCode(self: *Compiler, op_code: chunk.OpCode) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = op_code }, self.parser.previous.line);
    }

    fn emitOpCodes(self: *Compiler, a: chunk.OpCode, b: chunk.OpCode) !void {
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = a }, self.parser.previous.line);
        try self.currentChunk().write(self.gpa, chunk.OpByte{ .Op = b }, self.parser.previous.line);
    }

    fn consume(self: *Compiler, expected: scanner.TokenType, message: []const u8) !void {
        if (self.check(expected)) {
            try self.parser.advance();
            return;
        }
        return self.errorAtCurrent(message);
    }

    fn match(self: *Compiler, token_type: scanner.TokenType) !bool {
        if (!self.check(token_type)) return false;
        try self.parser.advance();
        return true;
    }

    fn check(self: *Compiler, token_type: scanner.TokenType) bool {
        return self.parser.current.type == token_type;
    }

    fn errorAtCurrent(self: *Compiler, message: []const u8) anyerror {
        return Compiler.errorAt(&self.parser.current, message);
    }

    fn errorAtPrevious(self: *Compiler, message: []const u8) anyerror {
        return Compiler.errorAt(&self.parser.previous, message);
    }

    fn errorAt(token: *scanner.Token, message: []const u8) anyerror {
        std.debug.print("[line {d}] Error", .{token.line});
        switch (token.type) {
            .Eof => std.debug.print(" at end", .{}),
            .Error => {},
            else => std.debug.print(" at '{s}'", .{token.source[token.start .. token.start + token.length]}),
        }
        std.debug.print(": {s}\n", .{message});
        return error.CompileError;
    }
};

fn binary(compiler: *Compiler) !void {
    const operator_type = compiler.parser.previous.type;
    try compiler.parsePrecedence(getRulePrecedence(operator_type));

    switch (operator_type) {
        .Plus => try compiler.emitOpCode(.Add),
        .Minus => try compiler.emitOpCode(.Subtract),
        .Asterisk => try compiler.emitOpCode(.Multiply),
        .Slash => try compiler.emitOpCode(.Divide),
        .Eq => try compiler.emitOpCode(.Equal),
        .NotEq => try compiler.emitOpCodes(.Equal, .Not),
        .Gt => try compiler.emitOpCode(.Greater),
        .GtOrEq => try compiler.emitOpCodes(.Less, .Not),
        .Lt => try compiler.emitOpCode(.Less),
        .LtOrEq => try compiler.emitOpCodes(.Greater, .Not),
        else => unreachable,
    }
}

fn grouping(compiler: *Compiler, can_assign: bool) !void {
    _ = can_assign;
    try compiler.expression();
    try compiler.consume(.RParen, "Expect ')' after expression");
}

fn unary(compiler: *Compiler, can_assign: bool) !void {
    _ = can_assign;
    const operator_type = compiler.parser.previous.type;
    try compiler.parsePrecedence(.Prefix);
    switch (operator_type) {
        .Minus => try compiler.emitOpCode(.Negate),
        .Bang => try compiler.emitOpCode(.Not),
        else => unreachable,
    }
}

fn float(compiler: *Compiler, can_assign: bool) !void {
    _ = can_assign;
    const val = try std.fmt.parseFloat(f64, compiler.parser.previous.toString());
    try compiler.emitConstant(value.wrapFloat(val));
}

fn integer(compiler: *Compiler, can_assign: bool) !void {
    _ = can_assign;
    const val = try std.fmt.parseInt(i64, compiler.parser.previous.toString(), 10);
    try compiler.emitConstant(value.wrapInt(val));
}

fn literal(compiler: *Compiler, can_assign: bool) !void {
    _ = can_assign;
    switch (compiler.parser.previous.type) {
        .False => try compiler.emitOpCode(.False),
        .Null => try compiler.emitOpCode(.Null),
        .True => try compiler.emitOpCode(.True),
        else => unreachable,
    }
}

fn string(compiler: *Compiler, can_assign: bool) !void {
    _ = can_assign;
    const start = compiler.parser.previous.start + 1;
    const end = start + compiler.parser.previous.length - 2;
    const slice = compiler.parser.previous.source[start..end];
    try compiler.emitConstant(value.wrapObj(try object.copyStaticString(compiler.vm, slice)));
}

fn variable(compiler: *Compiler, can_assign: bool) !void {
    var name = compiler.parser.previous;
    try namedVariable(compiler, &name, can_assign);
}

fn namedVariable(compiler: *Compiler, name: *scanner.Token, can_assign: bool) !void {
    var getOp: chunk.OpCode = undefined;
    var setOp: chunk.OpCode = undefined;

    var arg: usize = undefined;
    var emit_u24 = true;

    if (try compiler.resolveLocal(name)) |val| {
        getOp = .GetLocal;
        setOp = .SetLocal;
        arg = val;
    } else if (try compiler.resolveUpvalue(name)) |val| {
        getOp = .GetUpvalue;
        setOp = .SetUpvalue;
        arg = val;
        emit_u24 = false;
    } else {
        arg = try compiler.identifierConstant(name);
        getOp = .GetGlobal;
        setOp = .SetGlobal;
    }

    if (can_assign and try compiler.match(.Assign)) {
        try compiler.expression();
        try compiler.emitOpCode(setOp);
        if (emit_u24) {
            try compiler.emitU24(arg);
        } else {
            try compiler.emitByte(@intCast(arg));
        }
    } else {
        try compiler.emitOpCode(getOp);
        if (emit_u24) {
            try compiler.emitU24(arg);
        } else {
            try compiler.emitByte(@intCast(arg));
        }
    }
}

fn logicalAnd(compiler: *Compiler) !void {
    const end_jump = try compiler.emitJump(.JumpIfFalse);
    try compiler.emitOpCode(.Pop);
    try compiler.parsePrecedence(.LogicalAnd);
    try compiler.patchJump(end_jump);
}

fn logicalOr(compiler: *Compiler) !void {
    const else_jump = try compiler.emitJump(.JumpIfFalse);
    const end_jump = try compiler.emitJump(.Jump);

    try compiler.patchJump(else_jump);
    try compiler.emitOpCode(.Pop);

    try compiler.parsePrecedence(.LogicalOr);
    try compiler.patchJump(end_jump);
}

fn func(compiler: *Compiler, can_assign: bool) !void {
    _ = can_assign;

    compiler.indent += 1;

    var new_compiler: Compiler = undefined;
    try Compiler.init(
        &new_compiler,
        compiler.vm,
        compiler.gpa,
        compiler.parser,
        .Function,
        compiler,
        compiler.current_var,
        compiler.indent,
    );

    new_compiler.vm.current_compiler = &new_compiler;
    new_compiler.beginScope();

    try new_compiler.consume(.LParen, "Expect '(' after function name.");

    if (!new_compiler.check(.RParen)) {
        while (true) {
            new_compiler.function.?.arity += 1;
            if (new_compiler.function.?.arity > 255) {
                return compiler.errorAtCurrent("Can't have more than 255 parameters.");
            }
            const constant = try new_compiler.parseVariable("Expect parameter name.");
            try new_compiler.defineVariable(constant);
            if (!try new_compiler.match(.Comma)) break;
        }
    }

    try new_compiler.consume(.RParen, "Expect ')' after function parameters.");
    try new_compiler.statement();

    const function = try new_compiler.endCompiler();

    const constant = try compiler.makeConstant(value.wrapObj(&function.obj));
    try compiler.emitOpCode(.Closure);
    try compiler.emitU24(constant);

    for (0..function.upvalue_count) |i| {
        try compiler.emitByte(@intFromBool(new_compiler.upvalues[i].is_local));
        try compiler.emitByte(new_compiler.upvalues[i].index);
    }

    compiler.indent -= 1;
}

fn call(compiler: *Compiler) !void {
    const arg_count = try argumentList(compiler);
    try compiler.emitBytes(.Call, arg_count);
}

fn argumentList(compiler: *Compiler) !u8 {
    var count: u8 = 0;
    if (!compiler.check(.RParen)) {
        while (true) {
            if (count == 255) {
                return compiler.errorAtPrevious("Can't have more than 255 arguments");
            }
            try compiler.expression();
            count += 1;
            if (!try compiler.match(.Comma)) break;
        }
    }
    try compiler.consume(.RParen, "Expect ')' after arguments.");
    return count;
}

const ParseRule = struct {
    prefix: ?*const fn (compiler: *Compiler, can_assign: bool) anyerror!void,
    infix: ?*const fn (compiler: *Compiler) anyerror!void,
    precedence: ?Precedence,
};

const token_count = @typeInfo(scanner.TokenType).@"enum".fields.len;

const rules: [token_count]ParseRule = initRules();

fn initRules() [token_count]ParseRule {
    var table: [token_count]ParseRule = undefined;

    inline for (@typeInfo(scanner.TokenType).@"enum".fields) |field| {
        const index = field.value;
        const tag = @as(scanner.TokenType, @enumFromInt(index));
        table[index] = switch (tag) {
            .LParen => .{ .prefix = grouping, .infix = call, .precedence = .Call },
            .RParen => .{ .prefix = null, .infix = null, .precedence = null },
            .LBracket => .{ .prefix = null, .infix = null, .precedence = .Index },
            .RBracket => .{ .prefix = null, .infix = null, .precedence = null },
            .LBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .RBrace => .{ .prefix = null, .infix = null, .precedence = null },
            .Assign => .{ .prefix = null, .infix = null, .precedence = .Assign },
            .Comma => .{ .prefix = null, .infix = null, .precedence = null },
            .String => .{ .prefix = string, .infix = null, .precedence = null },
            .Float => .{ .prefix = float, .infix = null, .precedence = null },
            .Int => .{ .prefix = integer, .infix = null, .precedence = null },
            .True => .{ .prefix = literal, .infix = null, .precedence = null },
            .False => .{ .prefix = literal, .infix = null, .precedence = null },
            .Null => .{ .prefix = literal, .infix = null, .precedence = null },
            .Identifier => .{ .prefix = variable, .infix = null, .precedence = null },
            .Bang => .{ .prefix = unary, .infix = null, .precedence = null },
            .Lt => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .LtOrEq => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .Gt => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .GtOrEq => .{ .prefix = null, .infix = binary, .precedence = .LessGreater },
            .Eq => .{ .prefix = null, .infix = binary, .precedence = .Equals },
            .NotEq => .{ .prefix = null, .infix = binary, .precedence = .Equals },
            .Plus => .{ .prefix = null, .infix = binary, .precedence = .Sum },
            .Minus => .{ .prefix = unary, .infix = binary, .precedence = .Sum },
            .Slash => .{ .prefix = null, .infix = binary, .precedence = .Product },
            .Asterisk => .{ .prefix = null, .infix = binary, .precedence = .Product },
            .Percent => .{ .prefix = null, .infix = null, .precedence = .Product },
            .Return => .{ .prefix = null, .infix = null, .precedence = null },
            .If => .{ .prefix = null, .infix = null, .precedence = null },
            .Else => .{ .prefix = null, .infix = null, .precedence = null },
            .For => .{ .prefix = null, .infix = null, .precedence = null },
            .In => .{ .prefix = null, .infix = null, .precedence = null },
            .Dot => .{ .prefix = null, .infix = null, .precedence = .Index },
            .DotDot => .{ .prefix = null, .infix = null, .precedence = null },
            .Arrow => .{ .prefix = null, .infix = null, .precedence = null },
            .NewLine => .{ .prefix = null, .infix = null, .precedence = null },
            .Eof => .{ .prefix = null, .infix = null, .precedence = null },
            .And => .{ .prefix = null, .infix = logicalAnd, .precedence = .LogicalAnd },
            .Or => .{ .prefix = null, .infix = logicalOr, .precedence = .LogicalOr },
            .Pipe => .{ .prefix = null, .infix = null, .precedence = .BitwiseOr },
            .Ampersand => .{ .prefix = null, .infix = null, .precedence = .BitwiseAnd },
            .Caret => .{ .prefix = null, .infix = null, .precedence = .BitwiseXor },
            .Tilde => .{ .prefix = null, .infix = null, .precedence = .Prefix },
            .LeftShift => .{ .prefix = null, .infix = null, .precedence = .Shift },
            .RightShift => .{ .prefix = null, .infix = null, .precedence = .Shift },
            .Print => .{ .prefix = null, .infix = null, .precedence = null },
            .Assert => .{ .prefix = null, .infix = null, .precedence = null },
            .Let => .{ .prefix = null, .infix = null, .precedence = null },
            .Match => .{ .prefix = null, .infix = null, .precedence = null },
            .While => .{ .prefix = null, .infix = null, .precedence = null },
            .Fn => .{ .prefix = func, .infix = null, .precedence = null },
            .Error => .{ .prefix = null, .infix = null, .precedence = null },
        };
    }

    return table;
}

fn getRule(token_type: scanner.TokenType) *const ParseRule {
    return &rules[@intFromEnum(token_type)];
}

fn getRulePrecedenceValue(token_type: scanner.TokenType) usize {
    return @intFromEnum(getRulePrecedence(token_type));
}

fn getRulePrecedence(token_type: scanner.TokenType) Precedence {
    if (getRule(token_type).precedence) |precedence| {
        return precedence;
    }
    return Precedence.Lowest;
}
