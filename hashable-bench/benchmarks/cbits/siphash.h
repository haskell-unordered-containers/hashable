#ifndef _hashable_siphash_h
#define _hashable_siphash_h

#include <stdint.h>
#include <stdlib.h>

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;

#define SIPHASH_ROUNDS 2
#define SIPHASH_FINALROUNDS 4

u64 hashable_siphash(int, int, u64, u64, const u8 *, size_t);
u64 hashable_siphash24(u64, u64, const u8 *, size_t);

#if defined(__i386)

/* To use SSE instructions, we have to adjust the stack from its
   default of 4-byte alignment to use 16-byte alignment. */

# define ALIGNED_STACK __attribute__((force_align_arg_pointer))

u64 hashable_siphash24_sse2(u64, u64, const u8 *, size_t) ALIGNED_STACK;
u64 hashable_siphash24_sse41(u64, u64, const u8 *, size_t) ALIGNED_STACK;
#endif

#if defined(_WIN32)
# define __LITTLE_ENDIAN 1234
# define __BIG_ENDIAN 4321
# define __BYTE_ORDER __LITTLE_ENDIAN

#elif (defined(__FreeBSD__) && __FreeBSD_version >= 470000) || defined(__OpenBSD__) || defined(__NetBSD__)
# include <sys/endian.h>
# define __BIG_ENDIAN BIG_ENDIAN
# define __LITTLE_ENDIAN LITTLE_ENDIAN
# define __BYTE_ORDER BYTE_ORDER

#elif (defined(BSD) && (BSD >= 199103)) || defined(__APPLE__)
# include <machine/endian.h>
# define __BIG_ENDIAN BIG_ENDIAN
# define __LITTLE_ENDIAN LITTLE_ENDIAN
# define __BYTE_ORDER BYTE_ORDER

#elif defined(__linux__)
# include <endian.h>
#endif

static inline u64 peek_u64le(const u64 *p)
{
    u64 x = *p;

#if __BYTE_ORDER == __BIG_ENDIAN
    x = ((x & 0xff00000000000000ull) >> 56) |
	((x & 0x00ff000000000000ull) >> 40) |
	((x & 0x0000ff0000000000ull) >> 24) |
	((x & 0x000000ff00000000ull) >> 8) |
	((x & 0x00000000ff000000ull) << 8) |
	((x & 0x0000000000ff0000ull) << 24) |
	((x & 0x000000000000ff00ull) << 40) |
	((x & 0x00000000000000ffull) << 56);
#endif

    return x;
}

#endif /* _hashable_siphash_h */
