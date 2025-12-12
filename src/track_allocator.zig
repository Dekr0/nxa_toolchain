pub const Track_Allocator = struct
{
    backing       : mem.Allocator,
    backing_type  : []const u8 = "mem.Allocator", // avoid changing this after init
    tag           : []const u8,
    allocated     : usize = 0,
    peak_req      : usize = 0,
    min_req       : usize = 0,
    min_allocated : usize = 0,
    peak_allocated: usize = 0,

    const Self = @This();

    pub fn allocator(self: *Self) mem.Allocator
    {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc  = alloc,
                .free   = free,
                .remap  = remap,
                .resize = resize
            },
        };
    }

    pub fn alloc(
        ctx      : *anyopaque, // mem.Allocator
        size     : usize,
        alignment: mem.Alignment,
        ret_addr : usize
    ) ?[*]u8
    {
        const self: *Self = @ptrCast(@alignCast(ctx));

        if (size > 0)
        {
            self.min_req = if (self.min_req == 0) size else @min(self.min_req, size);
        }

        self.peak_req = @max(self.peak_req, size);

        const data = self.backing.rawAlloc(size, alignment, ret_addr);
        if (data) |_|
        {
            self.allocated += size;
            self.log_post_alloc();
        }
        
        if (self.allocated > 0)
        {
            self.min_allocated = if (self.min_allocated == 0)
                self.allocated else 
                @min(self.min_allocated, self.allocated);
        }
        self.peak_allocated = @max(self.peak_allocated, self.allocated);
        return data;
    }

    pub fn free(
        ctx      :  *anyopaque, // mem.Allocator
        memory   : []u8,
        alignment:   mem.Alignment,
        ret_addr :   usize
    ) void
    {
        const self: *Self = @ptrCast(@alignCast(ctx));
        const len = memory.len;
        self.backing.rawFree(memory, alignment, ret_addr);
        self.allocated -= len;
        if (self.allocated > 0)
        {
            self.min_allocated = if (self.min_allocated == 0) 
                self.allocated else
                @min(self.min_allocated, self.allocated);
        }
        self.peak_allocated = @max(self.peak_allocated, self.allocated);
        self.log_free(len);
    }

    pub fn remap(
        ctx      :  *anyopaque,
        memory   : []u8,
        alignment:   mem.Alignment,
        new_size :   usize,
        ret_addr :   usize
    ) ?[*]u8
    {
        const self: *Self = @ptrCast(@alignCast(ctx));

        return self.backing.rawRemap(memory, alignment, new_size, ret_addr);
    }

    pub fn resize(
        ctx      :  *anyopaque,
        memory   : []u8,
        alignment:   mem.Alignment,
        new_size :   usize,
        ret_addr :   usize
    ) bool
    {
        const self: *Self = @ptrCast(@alignCast(ctx));
        const pre_len = memory.len;
        const ok = self.backing.rawResize(
            memory, alignment, new_size, ret_addr
        );
        const old = self.allocated;
        if (ok)
        {
            const post_len = memory.len;
            self.allocated += new_size - pre_len;
            self.log_valid_resize(old, new_size, alignment, pre_len, post_len);
        }
        else
        {
            const post_len = memory.len;
            self.log_invalid_resize(old,new_size, alignment, pre_len, post_len);
        }
        if (self.allocated > 0)
        {
            self.min_allocated = if (self.min_allocated == 0)
                self.allocated else 
                @min(self.min_allocated, self.allocated);
        }
        self.peak_allocated = @max(self.peak_allocated, self.allocated);
        return ok;
    }

    inline fn log_post_alloc(self: *Self) void
    {
        if (mem.containsAtLeast(u8, self.backing_type, 1, @typeName(Self)))
        {
            return;
        }
        log.debug(post_alloc_msg, .{ self.tag, self.backing_type, self.allocated });
    }

    inline fn log_free(self: *Self, mem_len:  usize) void
    {
        if (mem.containsAtLeast(u8, self.backing_type, 1, @typeName(Self)))
        {
            return;
        }
        log.debug(free_msg, .{ self.tag, self.backing_type, self.allocated, mem_len });
    }

    inline fn log_valid_resize(
        self         : *Self,
        old_allocated:  usize,
        req_size     :  usize,
        alignment    :  mem.Alignment,
        pre_len      :  usize,
        post_len     :  usize
    ) void
    {
        if (mem.containsAtLeast(u8, self.backing_type, 1, @typeName(Self)))
        {
            return;
        }
        log.debug(valid_resize_msg, .{
            self.tag,
            self.backing_type,
            old_allocated,
            self.allocated,
            req_size,
            alignment,
            pre_len,
            post_len,
        });
    }

    inline fn log_invalid_resize(
        self         : *Self,
        old_allocated:  usize,
        req_size     :  usize,
        alignment    :  mem.Alignment,
        pre_len      :  usize,
        post_len     :  usize
    ) void
    {
        if (mem.containsAtLeast(u8, self.backing_type, 1, @typeName(Self)))
        {
            return;
        }
        log.debug(invalid_resize_msg, .{
            self.tag,
            self.backing_type,
            old_allocated,
            self.allocated,
            req_size,
            alignment,
            pre_len,
            post_len
        });
    }

    pub inline fn stat(self: *Self) void
    {
        if (mem.containsAtLeast(u8, self.backing_type, 1, @typeName(Self)))
        {
            return;
        }
        log.debug(stat_msg, .{
            self.tag,
            self.backing_type,
            self.allocated,
            self.peak_req,
            self.peak_allocated,
            self.min_req,
            self.min_allocated
        });
    }
};

pub fn foo(scratch_allocator: mem.Allocator) !void
{
    var tracking_allocator: Track_Allocator = .{
        .backing = scratch_allocator,
        .tag = "foo",
        .backing_type = "mem.Allocator",
    };

    var arena = std.heap.ArenaAllocator.init(tracking_allocator.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    for (0..4) |_|
    {
        defer _ = arena.reset(.free_all);
        var alloc_a = try allocator.alloc(u16, 8);
        _ = alloc_a[0..];
        var alloc_b = try allocator.alloc(u32, 4);
        _ = alloc_b[0..];
        try bar(allocator);
    }

    tracking_allocator.stat();
}

pub fn bar(scratch_allocator: mem.Allocator) !void
{
    var tracking_allocator: Track_Allocator = .{
        .backing = scratch_allocator,
        .tag = "bar",
        .backing_type = "mem.Allocator",
    };

    var arena = std.heap.ArenaAllocator.init(tracking_allocator.allocator());
    defer arena.deinit();
    const allocator = arena.allocator();

    for (0..8) |_|
    {
        defer _ = arena.reset(.free_all);
        var alloc_a = try allocator.alloc(u16, 4);
        _ = alloc_a[0..];
        var alloc_b = try allocator.alloc(u32, 2);
        _ = alloc_b[0..];
    }

    tracking_allocator.stat();
}

test "track_allocator_scoping"
{
    std.debug.print("running track_allocator_scoping\n", .{});

    testing.log_level = .debug;

    var dga = std.heap.DebugAllocator(.{}).init;
    defer _ = dga.deinit();
    var gpa = dga.allocator();

    var scratch_space = try gpa.alloc(u8, 512);
    defer gpa.free(scratch_space);
    var fba = std.heap.FixedBufferAllocator.init(scratch_space[0..]);
    defer fba.reset();
    const fbai = fba.allocator();
    var arena = std.heap.ArenaAllocator.init(fbai);
    defer arena.deinit();
    const arenai = arena.allocator();

    try foo(arenai);
    
    std.debug.print("\n", .{});
}

const std = @import("std");

const debug = std.debug;
const mem   = std.mem;
const log   = std.log;

const testing = std.testing;

const post_alloc_msg = 
\\
\\--- Posted alloc
\\ - tag              : {s}
\\ - backing allocator: {s}
\\ - allocated        : {d}B
\\---
;
const free_msg =
\\
\\--- Posted free
\\ - tag              : {s}
\\ - backing allocator: {s}
\\ - allocated        : {d}B
\\ - free             : {d}B
\\---
;
const valid_resize_msg =
\\
\\--- Valid resize request
\\ - tag                  : {s}
\\ - backing allocator    : {s}
\\ - old allocated        : {d}B
\\ - allocated            : {d}B
\\ - new size             : {d}B
\\ - alignment            : {d}B
\\ - pre resize data size : {d}B
\\ - post resize data size: {d}B
\\---
;
const invalid_resize_msg =
\\
\\--- Invalid resize request
\\ - tag                  : {s}
\\ - backing allocator    : {s}
\\ - old allocated        : {d}B
\\ - allocated            : {d}B
\\ - new size             : {d}B
\\ - alignment            : {d}B
\\ - pre resize data size : {d}B
\\ - post resize data size: {d}B
\\---
;
const stat_msg =
\\
\\--- Track allocator stat
\\ - tag              : {s}
\\ - backing allocator: {s}
\\ - allocated        : {d}B
\\ - peak alloc size  : {d}B
\\ - peak allocated   : {d}B
\\ - min alloc size   : {d}B
\\ - min allocated    : {d}B
\\---
;
