const Tracy_Cfg = struct
{
    repo           : ?[]const u8,
    callstack      :    bool,
    allocation     :    bool,
    callstack_depth:    u32
};

pub fn build(b: *Build) !void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const lz4_dep = b.dependency("lz4", .{
        .target = target,
        .optimize = optimize
    });

    const tracy_cfg = build_tracy_option(b);

    const core_mod = build_core_lib(b, target, optimize, lz4_dep);

    build_exe(b, core_mod, tracy_cfg, target, optimize);

    try build_test(b, target, optimize, lz4_dep);
}

pub fn build_tracy_option(b: *Build) Tracy_Cfg
{
    const tracy = b.option(
        []const u8,
        "tracy",
        "Enable Tracy integration. Supply path to Tracy source"
    );
    const tracy_callstack = b.option(
        bool,
        "tracy-callstack",
        "Include callstack information with Tracy data. " ++ 
        "Does nothing if -Dtracy is not provided"
    ) orelse (tracy != null);
    const tracy_allocation = b.option(
        bool,
        "tracy-allocation",
        "Include allocation information with Tracy data." ++
        " Does nothing if -Dtracy is not provided"
    ) orelse (tracy != null);
    const tracy_callstack_depth: u32 = b.option(
        u32,
        "tracy-callstack-depth",
        "Declare callstack depth for Tracy data. " ++ 
        "Does nothing if -Dtracy_callstack is not provided"
    ) orelse 10;

    return .{
        .repo = tracy,
        .callstack = tracy_callstack,
        .allocation = tracy_allocation,
        .callstack_depth = tracy_callstack_depth
    };
}

pub fn build_core_lib(
    b      : *Build,
    t      :  ResolvedTarget,
    o      :  OptimizeMode,
    lz4_dep: *Dependency
) *Module
{
    const core_mod = b.addModule("nxa", .{
        .root_source_file = b.path("src/root.zig"),
        .target    = t,
        .optimize  = o,
        .link_libc = true,
    });

    core_mod.linkLibrary(lz4_dep.artifact("lz4"));

    const lib = b.addLibrary(.{
        .name = "nxa",
        .linkage = .dynamic,
        .root_module = core_mod
    });

    b.installArtifact(lib);

    return core_mod;
}

pub fn build_exe(
    b       : *Build,
    core_mod: *Module,
    tracy   :  Tracy_Cfg,
    t       :  ResolvedTarget,
    o       :  OptimizeMode,
) void
{
    const exe = b.addExecutable(.{
        .name = "nxa_cli",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = t,
            .optimize = o,
            .imports = &.{
                .{ .name = "nxa", .module = core_mod }
            }
        })
    });

    b.installArtifact(exe);

    const exe_options = b.addOptions();
    exe.root_module.addOptions("build_options", exe_options);

    exe_options.addOption(bool, "enable_tracy", tracy.repo != null);
    exe_options.addOption(bool, "enable_tracy_callstack", tracy.callstack);
    exe_options.addOption(bool, "enable_tracy_allocation", tracy.allocation);
    exe_options.addOption(u32, "tracy_callstack_depth", tracy.callstack_depth);

    if (tracy.repo) |repo|
    {
        const client_cpp = b.pathJoin(
            &[_][]const u8{ repo, "public", "TracyClient.cpp" },
        );
 
        const tracy_c_flags: []const []const u8 = &.{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };
 
        exe.root_module.addIncludePath(.{ .cwd_relative = repo });
        exe.root_module.addCSourceFile(
            .{ .file = .{ .cwd_relative = client_cpp }, .flags = tracy_c_flags }
        );
        exe.root_module.linkSystemLibrary("c++", .{ .use_pkg_config = .no });
        exe.root_module.link_libc = true;
    }

    const run_exe = b.addRunArtifact(exe);
    if (b.args) |args|
    {
        run_exe.addArgs(args);
    }
    const run_step = b.step("run", "run nxa cli");
    run_step.dependOn(&run_exe.step);
}

pub fn build_test(
    b       : *Build,
    t       :  ResolvedTarget,
    o       :  OptimizeMode,
    lz4_dep : *Dependency
) !void
{
    var backing: [1024]u8 = undefined;

    var fba = heap.FixedBufferAllocator.init(backing[0..]);
    const fbai = fba.allocator();

    const ignores: []const []const u8 = &.{
        "async.zig",
        "bit.zig",
        "io.zig",
        "main.zig"
    };
    
    const cwd = fs.cwd();

    var dir = try cwd.openDir("src", .{ .iterate = true });
    var iter = dir.iterate();

    // scan the source folder, and create test step dynamically?
    const nullable_opts = b.option(
        []const []const u8, "test_filter", "test filter"
    );
    var filters: []const []const u8 = &.{ "" };
    if (nullable_opts) |opts|
    {
        filters = opts;
    }

    outer: while (try iter.next()) |entry|
    {
        defer fba.reset();

        if (entry.kind != .file)
        {
            continue;
        }
        for (ignores) |ignore|
        {
            if (mem.eql(u8, ignore, entry.name))
            {
                continue :outer;
            }
        }

        const path = try fs.path.join(fbai, &.{ "src", entry.name });
        const name_no_ext = mem.trimEnd(u8, entry.name, ".zig");
        const test_cmd = try mem.concat(fbai, u8, &.{ "test_", name_no_ext },);
        const test_desc = try mem.concat(fbai, u8, &.{ "run tests in ", entry.name });

        const test_compile = b.addTest(.{
            .name = test_cmd,
            .filters = filters[0..],
            .root_module = b.createModule(.{
                .root_source_file = b.path(path),
                .target = t,
                .optimize = o,
                .link_libc = true
            })
        });
        test_compile.linkLibrary(lz4_dep.artifact("lz4"));
        b.installArtifact(test_compile);
        const test_arti = b.addRunArtifact(test_compile);
        const test_step = b.step(test_cmd, test_desc);
        test_step.dependOn(&test_arti.step);
    }
}

const std = @import("std");

const Io = std.Io;

const fs = std.fs;
const heap = std.heap;
const mem = std.mem;

const Build = std.Build;
const Module = Build.Module;
const ResolvedTarget = Build.ResolvedTarget;
const Dependency = Build.Dependency;

const OptimizeMode = std.builtin.OptimizeMode;
