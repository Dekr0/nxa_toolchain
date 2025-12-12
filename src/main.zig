pub fn main() void
{
    const tr = tracy.trace(@src());
    defer tr.end();

    var runtime: Io.Threaded = .init_single_threaded;
    const io = runtime.io();

    var dga: Debug_Allocator(.{}) = .init;
    defer _ = dga.deinit();

    const gpa = dga.allocator();

    const data_path = process.getEnvVarOwned(gpa, "DATA") catch |err|
    {
        log.err("get env `DATA`: {}", .{err});
        return;
    };
    defer gpa.free(data_path);

    const dsaa_data = core.decmp_dsaa_alloc(io, gpa, data_path) catch |err|
    {
        log.err("decompress DSAA: {}", .{err});
        return;
    };
    defer gpa.free(dsaa_data);
}

const std = @import("std");
const builtin = @import("builtin");

const core = @import("core.zig");
const tracy = @import("tracy.zig");

const Io = std.Io;

const mem = std.mem;

const heap = std.heap;
const Debug_Allocator = heap.DebugAllocator;

const log = std.log;
const process = std.process;

const Debug = builtin.mode;
