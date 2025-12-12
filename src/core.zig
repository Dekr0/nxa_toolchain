pub fn cache_dsaa(_: Io, data: []const u8) anyerror!void
{
    var cache = fs.cwd().createFile("dsaa", .{}) catch |err|
    {
        log.err("create cache file to store uncompress dsaa data: {}", .{ err });
        return err;
    };

    defer cache.close();

    cache.writeAll(data) catch |err|
    {
        log.err("write uncompressed dsaa data to cache file: {}", .{ err });
        return err;
    };
}

/// Get uncompressed data of DSAA
pub fn decmp_dsaa_alloc(
    io       :   Io,
    gpa      :   mem.Allocator,
    data_path: []const u8
) anyerror![]const u8
{
    var ta: Track_Allocator = undefined;
    if (builtin.mode == .Debug)
    {
        ta = .{ .backing = gpa, .tag = @src().fn_name, .backing_type = "mem.Allocator" };
    }
    defer if (builtin.mode == .Debug) { ta.stat(); };

    const dsaa_path = fs.path.join(gpa, &.{ data_path, "bundles.nxa" }) catch |err|
    {
        log.err("join path {s} with \"bundles.nxa\": {}", .{ data_path, err });
        return err;
    };
    defer gpa.free(dsaa_path);

    const data = dsar.decmp_dsar_alloc(io, gpa, dsaa_path) catch |err|
    {
        log.err("Decompress DSAA data from bundles.nxa into memory: {}", .{err});
        return err;
    };

    return data;
}

/// @param name: archive (hash) name
/// @param data: data folder
pub fn build_archive(
    io   :   Io,
    gpa  :   mem.Allocator,
    name : []const u8,
    data : []const u8
) anyerror!void
{
    _ = cwd.statPath(io, "dsaa", .{}) catch |stat_err| switch (stat_err)
    {
        Io.Dir.StatPathError.FileNotFound =>
        {
            return try build_archive_no_cache(io, gpa, name, data);
        }
    };

    // var arena = heap.ArenaAllocator.init(scratch_allocator);
    // defer arena.reset(.free_all);
    // const allocator = arena.allocator();
}

/// @param name: archive name
pub fn build_archive_no_cache(
    io       :   Io,
    gpa      :   mem.Allocator,
    name     : []const u8,
    data_path: []const u8
) anyerror!void
{
    const raw = decmp_dsaa_alloc(io, gpa, data_path) catch |err| {
        log.err("decompress DSAA data from bundles.nxa: {}", .{err});
        return err;
    };

    const d = DSAA.alloc(gpa, raw.len) catch |err|
    {
        log.err("allocate memory for DSAA structure: {}", .{err});
        gpa.free(raw);
        return err;
    };
    defer d.free();

    d.init([]const u8, raw) catch |err|
    {
        log.err("initialize DSAA structure: {}", .{err});
        gpa.free(raw);
        return err;
    };

    gpa.free(raw);

    if (d.archive_info(name)) |info|
    {
        var str_space = gpa.alloc(u8, 512) catch |err|
        {
            log.err("allocate memory to formating nxa file path: {}", .{ err });
            return err;
        };
        defer gpa.free(str_space);

        var str_fba = Fixed_Buffer_Allocator.init(str_space[0..]);
        const str_builder = str_fba.allocator();

        var max_scr_space: usize = 0;
        for (info.assets) |a|
        {
            defer str_fba.reset();

            const nxa_name = try mem.concat(str_builder, u8, &.{
                "bundles.", &fmt.digits2(a.nxa_bundle_idx), ".nxa" 
            });
            const nxa_path = try fs.path.join(str_builder, &.{ data_path, nxa_name });

            const f = fs.openFileAbsolute(nxa_path, .{ .mode = .read_only }) catch |err|
            {
                log.err("open {s}: {}", .{ nxa_path, err });
                return err;
            };
            defer f.close();

            const s = f.stat() catch |err|
            {
                log.err("stat {s}: {}", .{ nxa_path, err });
                return err;
            };

            max_scr_space = @max(s.size, max_scr_space);
        }

        var arch_scr_space = gpa.alloc(u8, 1024 + @divFloor(max_scr_space, 4)) catch |err|
        {
            log.err(
                "allocate scratch space for building archive {s}: {}",
                .{ name, err }
            );
            return err;
        };
        defer gpa.free(arch_scr_space);

        var arch_fba = Fixed_Buffer_Allocator.init(arch_scr_space[0..]);
        const arch_scr_a = arch_fba.allocator();

        var archive_data = gpa.alloc(u8, info.archive.size) catch |err|
        {
            log.err("allocate backing data of archive {s}: {}", .{ name, err });
            return err;
        };
        defer gpa.free(archive_data);

        const len = info.assets.len - 1;
        for (info.assets[0..len], 0..len) |a, i|
        {
            defer str_fba.reset();
            defer arch_fba.reset();

            const asset_size = info.assets[i + 1].offset - a.offset;

            const nxa_name = try mem.concat(str_builder, u8, &.{
                "bundles.", &fmt.digits2(a.nxa_bundle_idx), ".nxa" 
            });
            const nxa_path = try fs.path.join(str_builder, &.{ data_path, nxa_name });

            const f = fs.openFileAbsolute(nxa_path, .{ .mode = .read_only }) catch |err|
            {
                log.err("open {s}: {}", .{ nxa_path, err });
                return err;
            };
            defer f.close();

            var fr_buffer = try arch_scr_a.alloc(u8, 1024);
            @memset(fr_buffer, 0);

            var fr = f.reader(io, fr_buffer[0..]);
            const r = &fr.interface;

            dsar.get_assets_from_nxa(
                 arch_scr_a,
                *Io.File.Reader,
                 r,
                 a.uncmp_nxa_offset,
                 archive_data[a.offset..a.offset + asset_size]
            ) catch |err|
            {
                log.err("get assets from {s}: {}", .{ nxa_path, err });
                return err;
            };
        }

        {
            defer str_fba.reset();
            defer arch_fba.reset();

            const a = info.assets[len];

            const asset_size = info.archive.size - a.offset;

            const nxa_name = try mem.concat(str_builder, u8, &.{
                "bundles.", &fmt.digits2(a.nxa_bundle_idx), ".nxa" 
            });
            const nxa_path = try fs.path.join(str_builder, &.{ data_path, nxa_name });

            const f = fs.openFileAbsolute(nxa_path, .{ .mode = .read_only }) catch |err|
            {
                log.err("open {s}: {}", .{ nxa_path, err });
                return err;
            };
            defer f.close();

            var fr_buffer = try arch_scr_a.alloc(u8, 1024);
            @memset(fr_buffer, 0);

            var fr = f.reader(io, fr_buffer[0..]);
            const r = &fr.interface;

            dsar.get_assets_from_nxa(
                 arch_scr_a,
                *Io.File.Reader,
                 r,
                 a.uncmp_nxa_offset,
                 archive_data[a.offset..a.offset + asset_size]
            ) catch |err|
            {
                log.err("get assets from {s}: {}", .{ nxa_path, err });
                return err;
            };
        }

        fs.cwd().writeFile(.{ .sub_path = name, .data = archive_data, .flags = .{} }) catch |err|
        {
            log.err("write archive data to {s}: {}", .{ name, err });
            return err;
        };
    }
    else
    {
        log.warn("no archive with name: {s}", .{name});
    }
}

const std = @import("std");
const builtin = @import("builtin");
const lz4 = @cImport(
    @cInclude("lz4.h")
);

const bit = @import("bit.zig");
const dsaa = @import("dsaa.zig");
const dsar = @import("dsar.zig");
const track_allocator = @import("track_allocator.zig");

const Io = std.Io;
const cwd = std.Io.Dir.cwd();

const debug = std.debug;
const fmt = std.fmt;
const fs = std.fs;
const mem = std.mem;

const heap = std.heap;
const Debug_Allocator = heap.DebugAllocator;
const Fixed_Buffer_Allocator = heap.FixedBufferAllocator;
const Arena_Allocator = heap.ArenaAllocator;

const log = std.log;
const process = std.process;

const testing = std.testing;

const Track_Allocator = track_allocator.Track_Allocator;
const DSAA = dsaa.DSAA;
const DSAR = dsar.DSAR;
