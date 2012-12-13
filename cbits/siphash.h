#ifndef _hashable_siphash_h
#define _hashable_siphash_h

#include <stdint.h>

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint16_t u16;
typedef uint8_t u8;

#define SIPHASH_ROUNDS 2
#define SIPHASH_FINALROUNDS 4

u64 hashable_siphash(int, int, u64, u64, const u8 *, size_t);
u64 hashable_siphash24(u64, u64, const u8 *, size_t);

#if defined(__i386)
u64 hashable_siphash24_sse2(u64, u64, const u8 *, size_t);
u64 hashable_siphash24_sse41(u64, u64, const u8 *, size_t);
#endif

#endif /* _hashable_siphash_h */
