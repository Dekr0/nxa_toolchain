pub inline fn r_u8(r: *Io.Reader) Io.Reader.Error!u8
{
    return try r.takeInt(u8, .little);
}

pub inline fn r_i8(r: *Io.Reader) Io.Reader.Error!i8
{
    return try r.takeInt(i8, .little);
}

pub inline fn r_u16(r: *Io.Reader) Io.Reader.Error!u16
{
    return try r.takeInt(u16, .little);
}

pub inline fn r_i16(r: *Io.Reader) Io.Reader.Error!i16
{
    return try r.takeInt(i16, .little);
}

pub inline fn r_u32(r: *Io.Reader) Io.Reader.Error!u32
{
    return try r.takeInt(u32, .little);
}

pub inline fn r_i32(r: *Io.Reader) Io.Reader.Error!i32
{
    return try r.takeInt(i32, .little);
}

pub inline fn r_u64(r: *Io.Reader) Io.Reader.Error!u64
{
    return try r.takeInt(u64, .little);
}

pub inline fn r_i64(r: *Io.Reader) Io.Reader.Error!i64
{
    return try r.takeInt(i64, .little);
}

pub inline fn r_struct(comptime T: type, r: *Io.Reader) Io.Reader.Error!T
{
    return try r.takeStruct(T, .little);
}

const std = @import("std");

const Io = std.Io;

