const std = @import("std");

pub const debug_parser = true;
pub const debug_trace_execution = false;
pub const frames_max = 64;
pub const stack_max = std.math.maxInt(u8) * frames_max;

pub const debug_stress_gc = false;
pub const debug_log_gc = false;
pub const gc_heap_grow_factor = 2;

pub const debug_disassemble = false;
pub const debug_print_steps = false;
