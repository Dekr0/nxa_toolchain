export fn extract_dsaa(c_archive_name: [*c]u8) c_int
{
    var runtime: Io.Threaded = .init_single_threaded;
    defer runtime.deinit();
    const io = runtime.io();

    var scratch_space = pga.alloc(u8, 1024 * 4098) catch
    {
        return -1;
    };
    defer pga.free(scratch_space);

    var scratch_fba = heap.FixedBufferAllocator.init(scratch_space[0..]);
    defer scratch_fba.reset();
    var scratch_arena = heap.ArenaAllocator.init(scratch_fba.allocator());
    defer scratch_arena.deinit();
    const sa = scratch_arena.allocator();

    const archive_name: []const u8 = mem.span(c_archive_name);

    const data_path = process.getEnvVarOwned(sa, "DATA") catch
    {
        return -1;
    };
    defer sa.free(data_path);

    core.build_archive_no_cache(io, pga, archive_name, data_path) catch {
        return -1;
    };

    return 0;
}

const std = @import("std");
pub const core = @import("core.zig");

const Io = std.Io;
const heap = std.heap;
const mem = std.mem;
const process = std.process;

const pga = std.heap.page_allocator;
