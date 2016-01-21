# This file is a part of Julia. License is MIT: http://julialang.org/license

# Generic IO stubs

lock(::IO) = nothing
unlock(::IO) = nothing
reseteof(x::IO) = nothing

const SZ_UNBUFFERED_IO = 65536
buffer_writes(x::IO, bufsize=SZ_UNBUFFERED_IO) = nothing

function isopen end
function close end
function flush end
function wait_connected end
function wait_readnb end
function wait_readbyte end
function wait_close end
function nb_available end
function readavailable end
function isreadable end
function iswritable end
function copy end
function eof end

# all subtypes should implement this
read(s::IO, ::Type{UInt8}) = error(typeof(s)," does not support byte I/O")
write(s::IO, x::UInt8) = error(typeof(s)," does not support byte I/O")

# Generic wrappers around other IO objects
abstract AbstractPipe <: IO
function pipe_reader end
function pipe_writer end

write(io::AbstractPipe, byte::UInt8) = write(pipe_writer(io), byte)
write(io::AbstractPipe, bytes::Vector{UInt8}) = write(pipe_writer(io), bytes)
write{T<:AbstractPipe}(io::T, args...) = write(pipe_writer(io), args...)
write{S<:AbstractPipe}(io::S, a::Array) = write(pipe_writer(io), a)
buffer_or_write(io::AbstractPipe, p::Ptr, n::Integer) = buffer_or_write(pipe_writer(io), p, n)
buffer_writes(io::AbstractPipe, args...) = buffer_writes(pipe_writer(io), args...)
flush(io::AbstractPipe) = flush(pipe_writer(io))

read(io::AbstractPipe, byte::Type{UInt8}) = read(pipe_reader(io), byte)
read!(io::AbstractPipe, bytes::Vector{UInt8}) = read!(pipe_reader(io), bytes)
read{T<:AbstractPipe}(io::T, args...) = read(pipe_reader(io), args...)
read!{T<:AbstractPipe}(io::T, args...) = read!(pipe_reader(io), args...)
readuntil{T<:AbstractPipe}(io::T, args...) = readuntil(pipe_reader(io), args...)
read(io::AbstractPipe) = read(pipe_reader(io))
readavailable(io::AbstractPipe) = readavailable(pipe_reader(io))

isreadable(io::AbstractPipe) = isreadable(pipe_reader(io))
iswritable(io::AbstractPipe) = iswritable(pipe_writer(io))
isopen(io::AbstractPipe) = isopen(pipe_writer(io)) || isopen(pipe_reader(io))
close(io::AbstractPipe) = (close(pipe_writer(io)); close(pipe_reader(io)))
wait_readnb(io::AbstractPipe, nb::Int) = wait_readnb(pipe_reader(io), nb)
wait_readbyte(io::AbstractPipe, byte::UInt8) = wait_readbyte(pipe_reader(io), byte)
wait_close(io::AbstractPipe) = (wait_close(pipe_writer(io)); wait_close(pipe_reader(io)))
nb_available(io::AbstractPipe) = nb_available(pipe_reader(io))
eof(io::AbstractPipe) = eof(pipe_reader(io))
reseteof(io::AbstractPipe) = reseteof(pipe_reader(io))


# Exception-safe wrappers (io = open(); try f(io) finally close(io))

write(filename::AbstractString, args...) = open(io->write(io, args...), filename, "w")

read(filename::AbstractString, args...) = open(io->read(io, args...), filename)
read!(filename::AbstractString, a) = open(io->read!(io, a), filename)
readstring(filename::AbstractString) = open(readstring, filename)
readuntil(filename::AbstractString, args...) = open(io->readuntil(io, args...), filename)
readline(filename::AbstractString) = open(readline, filename)
readlines(filename::AbstractString) = open(readlines, filename)


## byte-order mark, ntoh & hton ##

const ENDIAN_BOM = reinterpret(UInt32,UInt8[1:4;])[1]

if ENDIAN_BOM == 0x01020304
    ntoh(x) = x
    hton(x) = x
    ltoh(x) = bswap(x)
    htol(x) = bswap(x)
elseif ENDIAN_BOM == 0x04030201
    ntoh(x) = bswap(x)
    hton(x) = bswap(x)
    ltoh(x) = x
    htol(x) = x
else
    error("seriously? what is this machine?")
end

isreadonly(s) = isreadable(s) && !iswritable(s)

## binary I/O ##

write(io::IO, x) = throw(MethodError(write, (io, x)))
function write(io::IO, xs...)
    local written::Int = 0
    for x in xs
        written += write(io, x)
    end
    written
end

if ENDIAN_BOM == 0x01020304
    function write(s::IO, x::Union{Int8,Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128})
        sz = sizeof(x)
        local written::Int = 0
        for n = sz:-1:1
            written += write(s, (x>>>((n-1)<<3))%UInt8)
        end
        return written
    end
else
    function write(s::IO, x::Union{Int8,Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128})
        sz = sizeof(x)
        local written::Int = 0
        for n = 1:sz
            written += write(s, (x>>>((n-1)<<3))%UInt8)
        end
        return written
    end
end

write(s::IO, x::Bool)    = write(s, UInt8(x))
write(s::IO, x::Float16) = write(s, reinterpret(Int16,x))
write(s::IO, x::Float32) = write(s, reinterpret(Int32,x))
write(s::IO, x::Float64) = write(s, reinterpret(Int64,x))

write(to::IO, p::Ptr) = write(to, convert(UInt, p))

function write(s::IO, a::AbstractArray)
    nb = 0
    for i in eachindex(a)
        nb += write(s, a[i])
    end
    return nb
end

function write(s::IO, ch::Char)
    c = reinterpret(UInt32, ch)
    if c < 0x80
        return write(s, c%UInt8)
    elseif c < 0x800
        return (write(s, (( c >> 6          ) | 0xC0)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    elseif c < 0x10000
        return (write(s, (( c >> 12         ) | 0xE0)%UInt8)) +
               (write(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    elseif c < 0x110000
        return (write(s, (( c >> 18         ) | 0xF0)%UInt8)) +
               (write(s, (((c >> 12) & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (((c >> 6)  & 0x3F ) | 0x80)%UInt8)) +
               (write(s, (( c        & 0x3F ) | 0x80)%UInt8))
    else
        return write(s, '\ufffd')
    end
end

function write(s::IO, p::Ptr, n::Integer)
    local written::Int = 0
    for i=1:n
        written += write(s, unsafe_load(p, i))
    end
    return written
end

function write(io::IO, s::Symbol)
    pname = unsafe_convert(Ptr{UInt8}, s)
    return write(io, pname, Int(ccall(:strlen, Csize_t, (Cstring,), pname)))
end

function write(to::IO, from::IO)
    while !eof(from)
        write(to, readavailable(from))
    end
end


read(s::IO, ::Type{Int8}) = reinterpret(Int8, read(s,UInt8))

function read{T <: Union{Int16,UInt16,Int32,UInt32,Int64,UInt64,Int128,UInt128}}(s::IO, ::Type{T})
    x = zero(T)
    for n = 1:sizeof(x)
        x |= (convert(T,read(s,UInt8))<<((n-1)<<3))
    end
    return x
end

read(s::IO, ::Type{Bool})    = (read(s,UInt8)!=0)
read(s::IO, ::Type{Float16}) = box(Float16,unbox(Int16,read(s,Int16)))
read(s::IO, ::Type{Float32}) = box(Float32,unbox(Int32,read(s,Int32)))
read(s::IO, ::Type{Float64}) = box(Float64,unbox(Int64,read(s,Int64)))

read{T}(s::IO, ::Type{Ptr{T}}) = convert(Ptr{T}, read(s,UInt))

read{T}(s::IO, t::Type{T}, d1::Int, dims::Int...) = read(s, t, tuple(d1,dims...))
read{T}(s::IO, t::Type{T}, d1::Integer, dims::Integer...) =
    read(s, t, convert(Tuple{Vararg{Int}},tuple(d1,dims...)))

read{T}(s::IO, ::Type{T}, dims::Dims) = read!(s, Array(T, dims))

function read!(s::IO, a::Vector{UInt8})
    for i in 1:length(a)
        a[i] = read(s, UInt8)
    end
    return a
end

function read!{T}(s::IO, a::Array{T})
    if isbits(T)
        nb::Int = length(a) * sizeof(T)
        read!(s, reinterpret(UInt8, a, (nb,)))
    else
        for i in eachindex(a)
            a[i] = read(s, T)
        end
    end
    return a
end

function read(s::IO, ::Type{Char})
    ch = read(s, UInt8)
    if ch < 0x80
        return Char(ch)
    end

    # mimic utf8.next function
    trailing = Base.utf8_trailing[ch+1]
    c::UInt32 = 0
    for j = 1:trailing
        c += ch
        c <<= 6
        ch = read(s, UInt8)
    end
    c += ch
    c -= Base.utf8_offset[trailing+1]
    Char(c)
end

function readuntil(s::IO, delim::Char)
    if delim < Char(0x80)
        data = readuntil(s, delim%UInt8)
        enc = byte_string_classify(data)
        return (enc==1) ? ASCIIString(data) : UTF8String(data)
    end
    out = IOBuffer()
    while !eof(s)
        c = read(s, Char)
        write(out, c)
        if c == delim
            break
        end
    end
    takebuf_string(out)
end

function readuntil{T}(s::IO, delim::T)
    out = T[]
    while !eof(s)
        c = read(s, T)
        push!(out, c)
        if c == delim
            break
        end
    end
    out
end

# based on code by Glen Hertz
function readuntil(s::IO, t::AbstractString)
    l = length(t)
    if l == 0
        return ""
    end
    if l > 40
        warn("readuntil(IO,AbstractString) will perform poorly with a long string")
    end
    out = IOBuffer()
    m = Array(Char, l)  # last part of stream to match
    t = collect(t)
    i = 0
    while !eof(s)
        i += 1
        c = read(s, Char)
        write(out, c)
        if i <= l
            m[i] = c
        else
            # shift to last part of s
            for j = 2:l
                m[j-1] = m[j]
            end
            m[l] = c
        end
        if i >= l && m == t
            break
        end
    end
    return takebuf_string(out)
end

readline() = readline(STDIN)
readline(s::IO) = readuntil(s, '\n')
readchomp(x) = chomp!(readstring(x))

# read up to nb bytes into nb, returning # bytes read
function readbytes!(s::IO, b::AbstractArray{UInt8}, nb=length(b))
    olb = lb = length(b)
    nr = 0
    while nr < nb && !eof(s)
        a = read(s, UInt8)
        nr += 1
        if nr > lb
            lb = nr * 2
            resize!(b, lb)
        end
        b[nr] = a
    end
    if lb > olb
        resize!(b, nr) # shrink to just contain input data if was resized
    end
    return nr
end

# read up to nb bytes from s, returning a Vector{UInt8} of bytes read.
function read(s::IO, nb=typemax(Int))
    # Let readbytes! grow the array progressively by default
    # instead of taking of risk of over-allocating
    b = Array(UInt8, nb == typemax(Int) ? 1024 : nb)
    nr = readbytes!(s, b, nb)
    resize!(b, nr)
end

function readstring(s::IO)
    b = read(s)
    return isvalid(ASCIIString, b) ? ASCIIString(b) : UTF8String(b)
end

## high-level iterator interfaces ##

type EachLine
    stream::IO
    ondone::Function
    EachLine(stream) = EachLine(stream, ()->nothing)
    EachLine(stream, ondone) = new(stream, ondone)
end
eachline(stream::IO) = EachLine(stream)
eachline(filename::AbstractString) = EachLine(open(filename), close)

start(itr::EachLine) = nothing
function done(itr::EachLine, nada)
    if !eof(itr.stream)
        return false
    end
    itr.ondone()
    true
end
next(itr::EachLine, nada) = (readline(itr.stream), nothing)
eltype(::Type{EachLine}) = ByteString

readlines(s=STDIN) = collect(eachline(s))