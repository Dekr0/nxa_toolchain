const r_u8     = bit.r_u8;
const r_i8     = bit.r_i8;
const r_u16    = bit.r_u16;
const r_i16    = bit.r_i16;
const r_u32    = bit.r_u32;
const r_i32    = bit.r_i32;
const r_u64    = bit.r_u64;
const r_i64    = bit.r_i64;
const r_struct = bit.r_struct;

pub const Header = packed struct
{
    userdata_01 : u32,
    userdata_02 : u32,
    num_nxas    : u32,
    num_archives: u32
};

pub const Archive_Entry = struct
{
    name_offset       : u32,
    num_assets        : u32,
    assets_base_offset: u32,
    // padding        : u32
    size              : u64
};

pub const Asset_Entry = struct
{
    nxa_bundle_idx  : u8,
    uncmp_nxa_offset: u32,
    /// hypothetical asset offset in the original archive file structure
    offset          : u64, 

};

pub const DSAA = struct
{
    /// allocator that allocate DSAA structure and hash map
    gpa: mem.Allocator,

    /// for archive entries, archive names, and asset entries
    backing: []u8,
    fba    :   Fixed_Buffer_Allocator,

    header: Header,

    archive_entries:   []Archive_Entry,
    archive_names  : [][]u8,
    asset_entries  : [][]Asset_Entry,
    archives_table :     std.StringArrayHashMap(usize),

    /// @param size: size of uncompressed DSAA data, used for estimated memory 
    /// of DSAA struct
    pub fn alloc(gpa: mem.Allocator, size: usize) anyerror!*DSAA
    {
        var d = gpa.create(DSAA) catch |err|
        {
            log.err("allocate memory for DSAA structure: {}", .{ err });
            return err;
        };
    
        d.gpa = gpa;
    
        // TODO: narrow down the size so that it doesn't over allocated too much
        d.backing = gpa.alloc(u8, size * 2) catch |err|
        {
            log.err("allocate memory for DSAA structural data: {}", .{ err });
            return err;
        };
        d.fba = Fixed_Buffer_Allocator.init(d.backing[0..]);
    
        return d;
    }

    /// @param comptime T: input type of DSAA data (Io.File.Reader or []const u8)
    /// @param input     : input DSAA data
    pub fn init(d: *DSAA, comptime T: type, input: T) anyerror!void {
        switch (T)
        {
            *Io.File.Reader =>
            {
                const fr: *Io.File.Reader = input;
                const r = &fr.interface;

                const data_allocator = d.fba.allocator();

                d.header = read_header(r) catch |err|
                {
                    log.err("read dsaa header: {}", .{ err });
                    return err;
                };

                d.archive_entries = read_archive_entry_alloc(
                    data_allocator, T, r, d.header,
                ) catch |err|
                {
                    log.err("read archive entries: {}", .{ err });
                    return err;
                };

                const res = alloc_archives_table(
                    d.gpa,
                    data_allocator,
                    T, r,
                    d.archive_entries
                ) catch |err|
                {
                    log.err("allocate archives table: {}", .{ err });
                    return err;
                };

                d.archive_names = res.names;
                d.archives_table = res.table;

                d.asset_entries = read_assets_entries_alloc(
                    data_allocator, T, r, d.archive_entries,
                ) catch |err|
                {
                    log.err("read assets entries for all archive entries: {}", .{ err });
                    return err;
                };
            },
            []const u8 =>
            {
                const data: []const u8 = input;

                var r = Io.Reader.fixed(data);

                const data_allocator = d.fba.allocator();

                d.header = read_header(&r) catch |err|
                {
                    log.err("read dsaa header: {}", .{ err });
                    return err;
                };

                d.archive_entries = read_archive_entry_alloc(
                    data_allocator, T, &r, d.header
                ) catch |err|
                {
                    log.err("read archive entries: {}", .{ err });
                    return err;
                };

                const res = alloc_archives_table(
                    d.gpa,
                    data_allocator,
                    T, &r,
                    d.archive_entries
                ) catch |err|
                {
                    log.err("allocate archives table: {}", .{ err });
                    return err;
                };

                d.archive_names = res.names;
                d.archives_table = res.table;

                d.asset_entries = read_assets_entries_alloc(
                    data_allocator,
                    T, &r,
                    d.archive_entries,
                ) catch |err|
                {
                    log.err("read assets entries for all archive entries: {}", .{ err });
                    return err;
                };
            },
            else => unreachable
        }
    }

    /// @param name: archive name
    pub fn archive_info(d: *DSAA, name: []const u8) ?struct{ 
        archive:   Archive_Entry,
        assets : []Asset_Entry,
    }
    {
        if (d.archives_table.get(name)) |i|
        {
            return .{
                .archive = d.archive_entries[i],
                .assets = d.asset_entries[i]
            };
        }
        else
        {
            return null;
        }
    }

    pub fn free(d: *DSAA) void
    {
        d.fba.reset();
        d.gpa.free(d.backing);
        d.archives_table.deinit();
        d.gpa.destroy(d);
    }
};

pub const Error = error
{
    Invalid_Magic,
    Invalid_Padding
};

fn read_header(r: *Io.Reader) anyerror!Header
{
    const magic = r_u32(r) catch |err|
    {
        log.err("read dsaa magic: {}", .{ err });
        return err;
    };

    if (magic != 1094800196)
    {
        log.err("invalid magic number: {d}", .{ magic });
        return Error.Invalid_Magic;
    }

    const h = r_struct(Header, r) catch |err|
    {
        log.err("read dsaa header: {}", .{ err });
        return err;
    };

    // most likely because of alignment pentaly
    const pad_04 = r_u32(r) catch |err|
    {
        log.err("read 4 bytes of padding after dsaa header: {}", .{ err });
        return err;
    };

    if (pad_04 != 858993459)
    {
        return Error.Invalid_Padding;
    }

    return h;
}

/// @param comptime T: input type of DSAA data (Io.File.Reader or []const u8)
/// @param entries   : pre-allocated archive entries
fn read_archive_entry(
    comptime T:   type,
    r         :  *Io.File.Reader,
    entries   : []Archive_Entry
) anyerror!void
{
    switch (T)
    {
        *Io.File.Reader =>
        {
            const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
            fr.seekTo(24) catch |err|
            {
                log.err("jump to first byte of archive entries: {}", .{ err });
                return err;
            };
        },
        []const u8 => r.seek = 24,
        else => unreachable
    }

    for (entries, 0..entries.len) |*e, i|
    {
        e.*.size = r_u64(r) catch |err|
        {
            log.err("read archive size of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        e.*.name_offset = r_u32(r) catch |err|
        {
            log.err("read names offset of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        e.*.num_assets = r_u32(r) catch |err|
        {
            log.err("read # of assets of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        e.*.assets_base_offset = r_u32(r) catch |err|
        {
            log.err("read assets base offset of {d}-th archive entry: {}", .{ i, err });
            return err;
        };

        // most likely because of alignment pentaly
        const pad_04 = r_u32(r) catch |err|
        {
            log.err("read 4 bytes padding of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        if (pad_04 != 0)
        {
            log.err("4 bytes padding is not equal to zero for {d}-th archive entry", .{});
            return Error.InvalidPadding;
        }
    }
}

/// @param comptime T: input type of DSAA data (Io.File.Reader or []const u8)
/// @param h         : DSAA header
fn read_archive_entry_alloc(
    gpa       :  mem.Allocator,
    comptime T:  type,
    r         : *Io.Reader,
    h         :  Header
) anyerror![]Archive_Entry
{
    switch (T)
    {
        *Io.File.Reader =>
        {
            const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
            fr.seekTo(24) catch |err|
            {
                log.err("jump to first byte of archive entries: {}", .{ err });
                return err;
            };
        },
        []const u8 => r.seek = 24,
        else => unreachable
    }

    var entries = gpa.alloc(Archive_Entry, h.num_archives) catch |err|
    {
        log.err("allocator memory for archive entries: {}", .{ err });
        return err;
    };

    for (entries[0..], 0..h.num_archives) |*e, i|
    {
        e.*.size = r_u64(r) catch |err|
        {
            log.err("read archive size of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        e.*.name_offset = r_u32(r) catch |err|
        {
            log.err("read archive name offset of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        e.*.num_assets = r_u32(r) catch |err|
        {
            log.err("read # of assets of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        e.*.assets_base_offset = r_u32(r) catch |err|
        {
            log.err("read assets base offset of {d}-th archive entry: {}", .{ i, err });
            return err;
        };

        // most likely because of alignment pentaly
        const pad_04 = r_u32(r) catch |err|
        {
            log.err("read 4 bytes padding of {d}-th archive entry: {}", .{ i, err });
            return err;
        };
        if (pad_04 != 0)
        {
            log.err("4 bytes padding is not equal to zero for {d}-th archive entry", .{ i });
            return Error.Invalid_Padding;
        }
    }

    return entries;
}

/// @param comptime T: input type of DSAA data (Io.File.Reader or []const u8)
/// @param entries   : archive entries
fn alloc_archives_table(
    table_allocator:  mem.Allocator,
    data_allocator :  mem.Allocator,
    comptime T     :  type,
    r              : *Io.Reader,
    entries        :[]Archive_Entry
) anyerror!struct{
    names: [][]u8,
    table:     std.StringArrayHashMap(usize)
}
{
    var names = data_allocator.alloc([]u8, entries.len) catch |err|
    {
        log.err("allocate memory for archive names: {}", .{ err });
        return err;
    };

    var table = std.StringArrayHashMap(usize).init(table_allocator);

    for (entries, 0..entries.len) |entry, i|
    {
        switch(T)
        {
            *Io.File.Reader =>
            {
                const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
                fr.seekTo(entry.name_offset) catch |err|
                {
                    log.err("seek to {d}-th archive name: {}", .{ i, err });
                    return err;
                };
            },
            []const u8 => r.seek = entry.name_offset,
            else => unreachable
        }

        const src = r.takeSentinel(0) catch |err|
        {
            log.err("read {d}-th archive name: {}", .{ i, err });
            return err;
        };

        names[i] = data_allocator.alloc(u8, src.len) catch |err|
        {
            log.err("allocate memory for {d}-th archive name: {}", .{ i, err });
            return err;
        };

        @memcpy(names[i], src[0..src.len]);

        const res = table.getOrPut(names[i]) catch |err|
        {
            log.err("get or put {d}-th archive name: {}", .{ i, err });
            return err;
        };

        if (res.found_existing)
        {
            debug.panic("{d}-th has an duplicated archive name: {s}", .{ i, src });
        }
        else
        {
            res.value_ptr.* = i;
        }
    }

    return .{ .names = names, .table = table };
}

/// @param comptime T: input type of DSAA data (Io.File.Reader or []const u8)
/// @param entries   : archive entries
fn read_names_alloc(
    gpa       :  mem.Allocator,
    comptime T:  type,
    r         : *Io.Reader,
    entries   :[]Archive_Entry
) anyerror![][]u8
{
    var names = gpa.alloc([]u8, entries.len) catch |err|
    {
        log.err("allocate memory for archive names: {}", .{ err });
        return err;
    };

    for (names[0..], 0..names.len) |*name, i|
    {
        switch (T)
        {
            *Io.File.Reader =>
            {
                const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
                fr.seekTo(entries[i].name_offset) catch |err|
                {
                    log.err("seek to {d}-th archive name: {}", .{ i, err });
                    return err;
                };
            },
            []const u8 => r.seek = entries[i].name_offset,
            else => unreachable
        }

        const src = r.takeSentinel(0) catch |err|
        {
            log.err("read {d}-th archive name: {}", .{ i, err });
            return err;
        };

        // skip null terminate character
        name.* = gpa.alloc(u8, src.len) catch |err|
        {
            log.err("allocate memory for {d}-th archive name: {}", .{ i, err });
            return err;
        };

        @memcpy(name.*, src[0..src.len]);
    }

    return names;
}

/// Provide an array of archive entries, return the array index of an archive 
/// entry whose archive name equals to proivded target.
///
/// @param entries: archive entries
/// @param target : target archive name
fn archive_entry(
    comptime T:  type,
    r         : *Io.Reader,
    entries   :[]Archive_Entry,
    target    :[]const u8
) anyerror!usize
{
    for (entries, 0..entries.len) |e, i|
    {
        switch (T)
        {
            *Io.File.Reader =>
            {
                const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
                fr.seekTo(e.name_offset) catch |err|
                {
                    log.err("seek to archive name: {}", .{ err });
                    return err;
                };
            },
            []const u8 => r.seek = e.name_offset,
            else => unreachable
        }

        const src = r.takeSentinel(0) catch |err|
        {
            log.err("read archive name: {}", .{ err });
            return err;
        };

        if (mem.eql(u8, target, src)) { return i; }
    }
    return -1;
}

/// read all asset entries of a given archive entry
///
/// @param assets : pre-allocated asset entries
/// @param archive: archive entry
fn read_assets_entry(
    comptime T:   type,
    r         :  *Io.Reader,
    assets    : []Asset_Entry,
    archive   :   Archive_Entry
) anyerror!void
{
    switch (T)
    {
        Io.File.Reader =>
        {
            const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
            fr.seekTo(archive.assets_base_offset) catch |err|
            {
                log.err(
                    "seek to assets base offset {d}: {}",
                    .{ archive.assets_base_offset, err }
                );
                return err;
            };
        },
        []const u8 => r.seek = archive.assets_base_offset,
        else => unreachable
    }

    for (assets[0..], 0..assets.len) |*a, i|
    {
        a.offset = r_u64(r) catch |err|
        {
            log.err("read offset of {d}-th asset entry: {}", .{ i, err });
            return err;
        };

        a.uncmp_nxa_offset = r_u32(r) catch |err|
        {
            log.err("read uncompressed nxa offset of {d}-th asset entry: {}", .{ i, err });
            return err;
        };

        const pad_03: u24 = i.takeInt(u24, .little);
        if (pad_03 != 0)
        {
            log.err("non zero padding of {d}-th asset entry", .{ i });
            return Error.InvalidPadding;
        }

        a.nxa_bundle_idx = r_u8(r) catch |err|
        {
            log.err("read nxa bundle index of {d}-th asset entry: {}", .{ i, err });
            return err;
        };
    }
}

/// read all asset entries of a given archive entry
///
/// @param comptime T: input type of DSAA data
/// @param archive   : archive entry
///
/// @return []AssetEntry: asset entries
fn read_assets_entry_alloc(
    gpa       : mem.Allocator,
    comptime T: type,
    r         :*Io.Reader,
    entry     : Archive_Entry
) anyerror![]Asset_Entry
{
    var entries = gpa.alloc(Asset_Entry, entry.num_assets) catch |err|
    {
        log.err("allocate memory for asset entries: {}", .{ err });
        return err;
    };

    switch (T)
    {
        *Io.File.Reader =>
        {
            const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
            fr.seekTo(entry.assets_base_offset) catch |err|
            {
                log.err(
                "seek to assets base offset {d}: {}", 
                .{ entry.assets_base_offset, err }
            );
                return err;
            };
        },
        []const u8 => r.seek = entry.assets_base_offset,
        else => unreachable
    }

    for (entries[0..], 0..entries.len) |*a, i|
    {
        a.offset = r_u64(r) catch |err|
        {
            log.err("read offset of {d}-th asset entry: {}", .{ i, err });
            return err;
        };

        a.uncmp_nxa_offset = r_u32(r) catch |err|
        {
            log.err("read uncompressed nxa offset of {d}-th asset entry: {}", .{ i, err });
            return err;
        };

        const pad_03: u24 = r.takeInt(u24, .little) catch |err|
        {
            log.err("read alignment padding of {d}-th asset entry: {}", .{ i, err });
            return err;
        };
        if (pad_03 != 0)
        {
            log.err("non zero padding of {d}-th asset entry", .{ i });
            return Error.Invalid_Padding;
        }

        a.nxa_bundle_idx = r_u8(r) catch |err|
        {
            log.err("read nxa bundle index of {d}-th asset entry: {}", .{ i, err });
            return err;
        };
    }

    return entries;
}

/// read asset entries of all archive entries
///
/// @param comptime T: input type of DSAA data
/// 
/// @return [][]Asset_Entry
fn read_assets_entries_alloc(
    gpa       :  mem.Allocator,
    comptime T:  type,
    r         : *Io.Reader,
    entries   :[]Archive_Entry,
) anyerror![][]Asset_Entry
{
    var asset_entries = gpa.alloc([]Asset_Entry, entries.len) catch |err|
    {
        log.err("allocate memory for assets entries: {}", .{ err });
        return err;
    };

    for (entries, 0..entries.len) |e, i|
    {
        asset_entries[i] = read_assets_entry_alloc(gpa, T, r, e) catch |err|
        {
            log.err("read assets entries for {d}-th archive entry: {}", .{ i, err });
            return err;
        };
    }

    return asset_entries;
}

/// @param comptime T: input type of DSAA data
/// @param h         : DSAA header
fn read_nxa_names(
    gpa       : mem.Allocator,
    comptime T: type,
    r         :*Io.Reader,
    h         : Header
) anyerror![][]u8
{
    switch (T)
    {
        *Io.File.Reader =>
        {
            const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
            fr.seekTo(24 + h.num_archives * 24) catch |err|
            {
                log.err("seek to first byte of nxa bundle name offset hint: {}", .{ err });
                return err;
            };
        },
        []const u8 => r.seek = 24 + h.num_archives * 24,
        else => unreachable
    }

    var names = gpa.alloc([]u8, h.num_nxas) catch |err|
    {
        log.err("allocate memory for nxa bundles' name: {}", .{ err });
        return err;
    };

    var scr_space = gpa.alloc(u8, @sizeOf(u32) * h.num_nxas) catch |err|
    {
        log.err("allocate scratch space for reading nxa names: {}", .{ err });
        return err;
    };
    defer gpa.free(scr_space);

    var fba = Fixed_Buffer_Allocator.init(scr_space[0..]);
    defer fba.reset();

    const scr_a = fba.allocator();

    var offsets = scr_a.alloc(u32, h.num_nxas) catch |err|
    {
        log.err("allocat memory for nxa name offsets: {}", .{ err });
        return err;
    };
    defer scr_a.free(offsets);

    for (offsets, 0..offsets.len) |*o, i|
    {
        o.* = r_u32(r) catch |err|
        {
            log.err("read nxa name of {d}-th nxa bundle: {}", .{ i, err });
            return err;
        };
    }

    for (names, 0..names.len) |*n, i|
    {
        switch (T)
        {
            *Io.File.Reader =>
            {
                const fr: *Io.File.Reader = @alignCast(@fieldParentPtr("interface", r));
                fr.seekTo(offsets[i]) catch |err|
                {
                    log.err("seek to name of {d}-th nxa bundle: {}", .{ i, err });
                    return err;
                };
            },
            []const u8 => r.seek = offsets[i],
            else => unreachable
        }

        const src = r.takeSentinel(0) catch |err|
        {
            log.err("read name of {d}-th nxa bundle: {}", .{ i, err });
            return err;
        };

        n.* = gpa.alloc(u8, src.len) catch |err|
        {
            log.err("allocate memory for name of {d}-th nxa bundle: {}", .{ i, err });
            return err;
        };

        @memcpy(n.*, src[0..]);
    }

    return names;
}

pub fn Test_Context(
    comptime cfg :   Debug_Allocator_Cfg,
    comptime path: []const u8,
    comptime size:   usize,
) type 
{
    return struct {
        runtime   :   Io.Threaded,
        io        :   Io,
        dga       :   Debug_Allocator(cfg),
        arena     :   Arena_Allocator,
        gpa       :   mem.Allocator,
        file      :   Io.File,
        reader_buf: []u8,
        reader    :   Io.File.Reader,

        const Self = @This();

        pub fn init_single_threaded(self: *Self) anyerror!void
        {
            self.runtime = .init_single_threaded;
            self.io = self.runtime.io();
            self.dga = .init;
            self.arena = Arena_Allocator.init(self.dga.allocator());
            self.gpa = self.arena.allocator();
            self.file = try cwd.openFile(
                self.io, path, .{ .mode = .read_only }
            );
            self.reader_buf = try self.gpa.alloc(u8, size);
            self.reader = self.file.reader(self.io, self.reader_buf);
        }

        pub fn deinit(self: *Self) void
        {
            self.file.close(self.io);
            self.gpa.free(self.reader_buf);
            self.arena.deinit();
            _ = self.dga.deinit();
            self.runtime.deinit();
        }

        pub fn dsaa_size(self: *Self) anyerror!u64
        {
            return (try self.file.stat(self.io)).size;
        }
    };
}

test "read_dsaa_header"
{
    debug.print("running test read_dsaa_header\n", .{});

    var ctx: Test_Context(.{}, "dsaa", 1024) = undefined;
    try ctx.init_single_threaded();
    defer ctx.deinit();

    const fr = &ctx.reader;
    const ri = &fr.interface;

    const h = try read_header(ri);

    try testing.expect(h.num_nxas == 30);

    debug.print("{any}", .{ h });

    debug.print("\n", .{});
}

test "read_archive_entries"
{
    debug.print("running test read_archive_entries\n", .{});

    var ctx: Test_Context(.{}, "dsaa", 1024) = undefined;
    try ctx.init_single_threaded();
    defer ctx.deinit();

    const ta = ctx.gpa;
    const fr = &ctx.reader;
    const ri = &fr.interface;

    const h = try read_header(ri);

    try testing.expect(h.num_nxas == 30);

    const entries = try read_archive_entry_alloc(ta, *Io.File.Reader, ri, h);

    for (entries) |e| { debug.print("{any}\n", .{ e }); }

    debug.print("\n", .{});
}

test "read_archive_names"
{
    std.debug.print("running test read_archive_names\n", .{});

    var ctx: Test_Context(.{}, "dsaa", 1024) = undefined;
    try ctx.init_single_threaded();
    defer ctx.deinit();

    const ta = ctx.gpa;
    const fr = &ctx.reader;
    const ri = &fr.interface;

    const h = try read_header(ri);

    try testing.expect(h.num_nxas == 30);

    const entries = try read_archive_entry_alloc(ta, *Io.File.Reader, ri, h);

    const names = try read_names_alloc(ta, *Io.File.Reader, ri, entries);

    for (names) |n| { debug.print("{s}\n", .{ n }); }

    std.debug.print("\n", .{});
}

test "read_asset_entries"
{
    var ctx: Test_Context(.{}, "dsaa", 1024) = undefined;
    try ctx.init_single_threaded();
    defer ctx.deinit();

    const ta = ctx.gpa;
    const fr = &ctx.reader;
    const ri = &fr.interface;

    const h = try read_header(ri);

    try testing.expect(h.num_nxas == 30);

    const archives = try read_archive_entry_alloc(ta, *Io.File.Reader, ri, h);

    var assets = try ta.alloc([]Asset_Entry, archives.len);
    for (archives, 0..assets.len) |archive, i|
    {
        assets[i] = try read_assets_entry_alloc(ta, *Io.File.Reader, ri, archive);
        for (assets[i]) |asset|
        {
            try testing.expect(
                asset.nxa_bundle_idx >= 0 and 
                asset.nxa_bundle_idx <= 30
            );
        }
    }
}

test "read_nxa_bundle_names"
{
    var ctx: Test_Context(.{}, "dsaa", 1024) = undefined;
    try ctx.init_single_threaded();
    defer ctx.deinit();

    const ta = ctx.gpa;
    const fr = &ctx.reader;
    const r = &fr.interface;

    const h = try read_header(r);

    try testing.expect(h.num_nxas == 30);

    const names = try read_nxa_names(ta, *Io.File.Reader, r, h);
    for (names) |n|
    {
        try testing.expect(mem.endsWith(u8, n[0..], ".nxa"));
    }
}

test "dsaa_pipelines"
{
    testing.log_level = .debug;
    var ctx: Test_Context(.{}, "dsaa", 1024) = undefined;
    try ctx.init_single_threaded();
    defer ctx.deinit();

    const ta = ctx.gpa;
    
    var d = try DSAA.alloc(ta, try ctx.dsaa_size());
    try d.init(*Io.File.Reader, &ctx.reader);
}

test "archive_info_lookup"
{
    var ctx: Test_Context(.{}, "dsaa", 1024) = undefined;
    try ctx.init_single_threaded();
    defer ctx.deinit();

    const ta = ctx.gpa;
    
    var d = try DSAA.alloc(ta, try ctx.dsaa_size());
    try d.init(*Io.File.Reader, &ctx.reader);

    const needles = [_][]const u8
    {
        "e75f556a740e00c9",
        "6ad7cc21015a5f85",
        "ebc7888cf3886e91"
    };

    for (needles) |n|
    {
        try testing.expect(d.archive_info(n) != null);
    }
}

const std = @import("std");

const bit = @import("bit.zig");

const Io = std.Io;
const cwd = std.Io.Dir.cwd();

const debug = std.debug;
const fs = std.fs;

const heap = std.heap;
const Arena_Allocator = heap.ArenaAllocator;
const Debug_Allocator = heap.DebugAllocator;
const Debug_Allocator_Cfg = heap.DebugAllocatorConfig;
const Fixed_Buffer_Allocator = heap.FixedBufferAllocator;

const log = std.log;
const mem = std.mem;

const testing = std.testing;
