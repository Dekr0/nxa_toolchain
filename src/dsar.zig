const r_u8     = bit.r_u8;
const r_i8     = bit.r_i8;
const r_u16    = bit.r_u16;
const r_i16    = bit.r_i16;
const r_u32    = bit.r_u32;
const r_i32    = bit.r_i32;
const r_u64    = bit.r_u64;
const r_i64    = bit.r_i64;
const r_struct = bit.r_struct;

pub const Cmp_Type = enum (u8)
{
    Un_Cmp = 0x00,
    Cmp   = 0x03
};

const Chunk_Type = enum (u8)
{
    Unk      = 0x01,
    Start    = 0x02,
    Continue = 0x04
};

const Error = error
{
    Invalid_Magic,
    Invalid_Padding,
    LZ4_Decompression_Error,
};

pub const Header = packed struct
{
    num_entries: u32,
    userdata_01: u32, // ???
    length     : u32
};

/// A DSAR entry describe the information about the data of an asset
pub const Entry = packed struct
{
    uncmp_offset: u64,
    cmp_offset  : u64,
    uncmp_size  : u32,
    cmp_size    : u32,
    cmp_type    : Cmp_Type,
    chunk_type  : Chunk_Type,
    userdata_04 : u16,
    pad         : u32
};

pub const DSAR = struct
{
    /// allocator that allocate DSAR struct
    gpa: mem.Allocator,

    /// allocator that allocate DSAR entries and other DSAR data
    backing: []u8,
    fba    :   Fixed_Buffer_Allocator,

    header    :   Header,
    /// total size of all uncompressed data
    uncmp_size:   u64,
    entries   : []Entry,

    const Self = @This();

    /// The provided reader will move forward. The intended process is to call 
    /// `init` right after `alloc` since header section is already consume.
    pub fn alloc(gpa: mem.Allocator, r: *Io.Reader) anyerror!*DSAR
    {
        const header = read_header(r) catch |err|
        {
            log.err("read DSAR header: {}", .{ err });
            return err;
        };

        var d = gpa.create(DSAR) catch |err|
        {
            log.err("allocate memory for DSAR structure: {}", .{ err });
            return err;
        };

        d.gpa = gpa;

        d.backing = gpa.alloc(
            u8, header.num_entries * @sizeOf(Entry) + @sizeOf([]Entry)
        ) catch |err|
        {
            log.err("allocator memory for DSAR structural data: {}", .{ err });
            return err;
        };

        d.header = header;
        d.fba = Fixed_Buffer_Allocator.init(d.backing[0..]);

        return d;
    }

    /// expect header section is consumed prior to this call
    pub fn init(d: *Self, r: *Io.Reader) anyerror!void
    {
        const allocator = d.fba.allocator();
        const res = read_entries_alloc(allocator, r, d.header) catch |err|
        {
            log.err("read dsar entries: {}", .{ err });
            return err;
        };
        d.uncmp_size = res.size;
        d.entries = res.entries;
    }

    pub fn free(d: *Self) void
    {
        d.fba.allocator().free(d.entries);
        d.fba.reset();
        d.gpa.free(d.backing);
        d.gpa.free(d);
    }
};

fn read_header(r: *Io.Reader) anyerror!Header
{
    var h: Header = undefined;

    const magic = r_u64(r) catch |err|
    {
        log.err("read NXA magic number: {}", .{ err });
        return err;
    };
    if (magic != 281489241625412)
    {
        log.err("invalid magic number: {d}", .{ magic });
        return Error.Invalid_Magic;
    }

    h.num_entries = r_u32(r) catch |err|
    {
        log.err("read number of entries: {}", .{ err });
        return err;
    };
    h.userdata_01 = r_u32(r) catch |err|
    {
        log.err("read userdata_01: {}", .{ err });
        return err;
    };
    h.length = r_u32(r) catch |err|
    {
        log.err("read length: {}", .{ err });
        return err;
    };

    const pad_4 = r_u32(r) catch |err|
    {
        log.err("read four padding bytes: {}", .{ err });
        return err;
    };
    if (pad_4 != 0)
    {
        return Error.Invalid_Padding;
    }

    const pad_word = r_u64(r) catch |err|
    {
        log.err("read PADDING word: {}", .{ err });
        return err;
    };
    if (pad_word != 3046489749524332880)
    {
        return Error.Invalid_Padding;
    }

    return h;
}

/// @param  entries: pre-allocated DSAR entries
/// @return u64    : total size of all uncompressed data
fn read_entries(r: *Io.Reader, entries: []Entry) anyerror!u64
{
    var size: u64 = 0;

    for (entries, 0..entries.len) |*e, i|
    {
        e = r_struct(Entry, r) catch |err|
        {
            log.err("read {d}-th entry struct: {}", .{ i, err });
            return err;
        };

        log.info(
            "{d}-th data: compressed size {d}; uncompressed size {d}. " ++ 
            "Decompressing...",
            .{ i, e.cmp_size, e.uncmp_size }
        );

        size += e.uncmp_size;
    }

    return size;
}

/// @param  h      : DSAR header
/// @return entries: allocated DSAR entries
/// @return size   : total size of all uncompressed data
fn read_entries_alloc(
    gpa:  mem.Allocator,
    r  : *Io.Reader,
    h  :  Header,
) anyerror!struct{ entries: []Entry, size: u64 }
{
    var size: u64 = 0;

    var entries = gpa.alloc(Entry, h.num_entries) catch |err|
    {
        log.err("allocate {d} bytes of memory for NXA entries: {}", .{
            @sizeOf(Entry) * h.num_entries, err
        });
        return err;
    };

    for (entries, 0..entries.len) |*e, i|
    {
        e.* = r_struct(Entry, r) catch |err|
        {
            log.err("read {d}-th entry struct: {}", .{ i, err });
            return err;
        };

        // log.info(
        //     "{d}-th data: compressed size {d}; uncompressed size {d}. " ++ 
        //     "Decompressing...",
        //     .{ i, e.cmp_size, e.uncmp_size }
        // );

        size += e.uncmp_size;
    }

    return .{ .entries = entries, .size = size };
}

/// @param entries: DSAR entries
/// @param w      : output writer to write uncompressed data
fn decmp_dsar_internal(
    scr_a  :   mem.Allocator,
    entries: []Entry,
    fr     :  *Io.File.Reader,
    w      :  *Io.Writer,
) anyerror!void
{
    var ta: Track_Allocator = undefined;
    if (builtin.mode == .Debug)
    {
        ta = .{
            .backing = scr_a,
            .tag = @src().fn_name,
            .backing_type = "Scratch Space Allocator",
        };
    }
    defer if (builtin.mode == .Debug) { ta.stat(); };

    var arena = Arena_Allocator.init(scr_a);
    defer arena.deinit();
    const arena_a = arena.allocator();

    for (entries) |e|
    {
        defer _ = arena.reset(.free_all);
        var cmp_data = try arena_a.alloc(u8, e.cmp_size);
        @memset(cmp_data[0..], 0);

        fr.seekTo(e.cmp_offset) catch |err|
        {
            log.err("seek to data portion: {}\n", .{ err });
            return err;
        };
        fr.interface.readSliceAll(cmp_data[0..]) catch |err|
        {
            log.err("read all compressed data: {}", .{ err });
            return err;
        };

        switch (e.cmp_type)
        {
            .Cmp =>
            {
                var uncmp_data = try arena_a.alloc(u8, e.uncmp_size);
                @memset(uncmp_data[0..], 0);

                const res = lz4.LZ4_decompress_safe(
                    @alignCast(cmp_data[0..]),
                    @alignCast(uncmp_data[0..]), 
                    @intCast(e.cmp_size),
                    @intCast(e.uncmp_size)
                );
                if (res < 0)
                {
                    log.err("LZ4 safe decompression: {d}", .{ res });
                    return Error.LZ4_Decompression_Error;
                }
                if (res != e.uncmp_size)
                {
                    log.err(
                        "LZ4 safe decompression: decompressed file " ++
                        "size ({d}) is smaller than expected ({d})", 
                        .{ res, e.uncmp_size }
                    );
                    return Error.LZ4_Decompression_Error;
                }

                w.writeAll(uncmp_data) catch |err|
                {
                    log.err("write all uncompressed data: {}", .{ err });
                    return err;
                };
            },
            .Un_Cmp =>
            {
                w.writeAll(cmp_data) catch |err|
                {
                    log.err("write all uncompressed data: {}", .{ err });
                    return err;
                };
            }
        }
    }
}

/// @param out    : buffer to store uncompressed data
/// @param entries: DSAR entries
fn decmp_dsar_alloc_internal(
    scr_a  :   mem.Allocator,
    out    : []u8,
    entries: []Entry,
    fr     :  *Io.File.Reader
) anyerror!void
{
    @memset(out[0..], 0);

    var w = Io.Writer.fixed(out[0..]);

    decmp_dsar_internal(scr_a, entries, fr, &w) catch |err|
    {
        log.err("decompress nxa: {}", .{ err });
        return err;
    };
}

/// @param path: file path to a `.nxa` file
pub fn decmp_dsar_alloc(
    io   :   Io,
    gpa  :   mem.Allocator,
    path : []const u8
) anyerror![]u8
{
    const f = fs.openFileAbsolute(path, .{ .mode = .read_only }) catch |err| {
        log.err("open {s}: {}", .{ path, err });
        return err;
    };
    defer f.close();

    const s = f.stat() catch |err|
    {
        log.err("stat of {s}: {}", .{ path, err });
        return err;
    };

    var scr_space = gpa.alloc(u8, 1024 + s.size) catch |err|
    {
        log.err("allocate scratch space to decompress DSAR data in {s}: {}", .{ path, err});
        return err;
    };
    defer gpa.free(scr_space);

    var fba = Fixed_Buffer_Allocator.init(scr_space[0..]);

    const allocator = fba.allocator();

    var ta: Track_Allocator = undefined;
    if (Debug)
    {
        ta = .{
            .backing = allocator,
            .tag = @src().fn_name,
            .backing_type = "Fixed_Buffer_Allocator"
        };
    }
    defer if (Debug) { ta.stat(); };

    var fr_buffer = try allocator.alloc(u8, buf_size);

    var fr = f.reader(io, fr_buffer[0..]);
    const r = &fr.interface;

    const d = try DSAR.alloc(allocator, r);

    d.init(r) catch |err|
    {
        log.err("initialize data for DSAR structure of {s}: {}", .{ path, err });
        return err;
    };

    var out = gpa.alloc(u8, d.uncmp_size) catch |err|
    {
        log.err("allocate memory for uncompressed dsar data: {}", .{ err });
        return err;
    };

    decmp_dsar_alloc_internal(allocator, out[0..], d.entries, &fr) catch |err|
    {
        log.err(
             "decompress full DSAR data to destination buffer from {s}: {}",
            .{ path, err }
        );
        return err;
    };

    return out;
}

/// @param comptime T: input type of DSAR data (*Io.File.Reader or []const u8)
/// @param offset    : resource offset
pub fn get_asset_from_nxa(
    scr_a     :   mem.Allocator,
    comptime T:   type,
    r         :  *Io.Reader,
    w         :  *Io.Writer,
    entries   : []Entry,
    offset    :   u64,
) anyerror!u64
{
    var written: u64 = 0;
    var found = false;
    const len = entries.len - 1;
    for (entries[0..len], 0..len) |e, i|
    {
        // ???
        if (e.uncmp_offset != offset and !found) 
        {
            continue;
        }
        found = true;

        const ctype_u8: u8 = @intFromEnum(e.chunk_type);
        const ctype_start_u8: u8 = @intFromEnum(Chunk_Type.Start);
        const mask = (ctype_u8 & ctype_start_u8) > 0;
        if (mask and written > 0)
        {
            return written;
        }

        switch (T)
        {
            *Io.File.Reader =>
            {
                const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
                fr.seekTo(e.cmp_offset) catch |err|
                {
                    log.err(
                        "seek to compressed offset of {d}-th asset: {}",
                        .{ i, err }
                    );
                    return err;
                };
            },
            else => unreachable
        }

        switch (e.cmp_type)
        {
            .Un_Cmp =>
            {
                debug.assert(e.cmp_size == e.uncmp_size);
                r.streamExact(w, e.cmp_size) catch |err|
                {
                    log.err(
                        "stream uncompressed data of {d}-th asset" ++ 
                        " directly to output: {}",
                        .{ i, err }
                    );
                    return err;
                };
                written += e.cmp_size;
            },
            .Cmp =>
            {
                var cmp_data = scr_a.alloc(u8, e.cmp_size) catch |err|
                {
                    log.err(
                        "allocate memory for compressed data of {d}-th "
                        ++ "asset: {}",
                        .{ i, err }
                    );
                    return err;
                };
                defer scr_a.free(cmp_data);
                @memset(cmp_data[0..], 0);

                r.readSliceAll(cmp_data) catch |err|
                {
                    log.err(
                        "read compressed data of {d}-th asset: {}", .{ i, err }
                    );
                    return err;
                };

                var uncmp_data = scr_a.alloc(u8, e.uncmp_size) catch |err|
                {
                    log.err(
                        "allocate memory for uncompressed data of {d}-th "
                        ++ "asset: {}",
                        .{ i, err }
                    );
                    return err;
                };
                defer scr_a.free(uncmp_data);
                @memset(uncmp_data[0..], 0);

                const res = lz4.LZ4_decompress_safe(
                    @alignCast(cmp_data[0..]),
                    @alignCast(uncmp_data[0..]), 
                    @intCast(e.cmp_size),
                    @intCast(e.uncmp_size)
                );
                if (res < 0)
                {
                    log.err("LZ4 safe decompression: {d}", .{ res });
                    return Error.LZ4_Decompression_Error;
                }
                if (res != e.uncmp_size)
                {
                    log.err(
                        "LZ4 safe decompression: decompressed file " ++
                        "size ({d}) is smaller than expected ({d})", 
                        .{ res, e.uncmp_size }
                    );
                    return Error.LZ4_Decompression_Error;
                }

                w.writeAll(uncmp_data) catch |err|
                {
                    log.err(
                        "write all uncompressed data of {d}-th asset: {}",
                        .{ i, err }
                    );
                    return err;
                };
                written += uncmp_data.len;
            }
        }
    }

    return written;
}

/// Extract a section of assets (data) from a `.nxa` file
///
/// @param offset   : offset to first byte of the first asset in a section of 
/// @param nxa_path : `.nxa` file path
/// @param data_path: data folder path
/// assets (data)
pub fn get_assets_from_nxa(
    scr_a     :   mem.Allocator,
    comptime T:   type,
    r         :  *Io.Reader,
    offset    :   u64,
    out       : []u8
) anyerror!void
{
    const d = DSAR.alloc(scr_a, r) catch |err|
    {
        log.err("allocate memory for DSAR structure: {}", .{ err });
        return err;
    };

    d.init(r) catch |err| {
        log.err("initialize DSAR structure: {}", .{ err });
        return err;
    };

    var w = Io.Writer.fixed(out);

    while (w.end < out.len)
    {
        const prev = w.end;
        const written = get_asset_from_nxa(scr_a, T, r, &w, d.entries, w.end + offset) catch |err|
        {
            log.err("get part of a section of assets: {}", .{ err });
            return err;
        };
        debug.assert(w.end - prev == written);
    }
    debug.assert(w.end == out.len);
}


const std = @import("std");
const builtin = @import("builtin");
const bit = @import("bit.zig");
const track_allocator = @import("track_allocator.zig");
const lz4 = @cImport(
    @cInclude("lz4.h")
);

const debug = std.debug;

const Io = std.Io;
const cwd = Io.Dir.cwd();

const fs = std.fs;

const heap = std.heap;
const Arena_Allocator = heap.ArenaAllocator;
const Fixed_Buffer_Allocator = heap.FixedBufferAllocator;

const mem = std.mem;

const log = std.log;
const process = std.process;
const testing = std.testing;

const Track_Allocator = track_allocator.Track_Allocator;

const buf_size = 1024;

const Debug = builtin.mode == .Debug;
