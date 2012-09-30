#include <stddef.h>
#include <stdint.h>

typedef uint64_t u64;
typedef uint8_t u8;

#define ROTL(x,b) (u64)(((x) << (b)) | ((x) >> (64 - (b))))

#define SIPROUND \
  do { \
    v0 += v1; v1=ROTL(v1,13); v1 ^= v0; v0=ROTL(v0,32); \
    v2 += v3; v3=ROTL(v3,16); v3 ^= v2; \
    v0 += v3; v3=ROTL(v3,21); v3 ^= v0; \
    v2 += v1; v1=ROTL(v1,17); v1 ^= v2; v2=ROTL(v2,32); \
  } while(0)

u64 siphash(int c, int d, u64 k0, u64 k1, const u8 *str, size_t len)
{
    u64 v0 = 0x736f6d6570736575ull ^ k0;
    u64 v1 = 0x646f72616e646f6dull ^ k1;
    u64 v2 = 0x6c7967656e657261ull ^ k0;
    u64 v3 = 0x7465646279746573ull ^ k1;
    u64 b = ((u64) len) << 56;
    const u8 *end, *p;
    int i;

    for (p = str, end = str + (len & ~7); p < end; p += 8) {
	u64 m = *(u64 *) p;
	v3 ^= m;
	if (c == 2) {
	    SIPROUND;
	    SIPROUND;
	} else {
	    for (i = 0; i < c; i++)
		SIPROUND;
	}
	v0 ^= m;
    }

    switch (len & 7) {
    case 7: b |= ((u64)p[6]) << 48;
    case 6: b |= ((u64)p[5]) << 40;
    case 5: b |= ((u64)p[4]) << 32;
    case 4: b |= ((u64)p[3]) << 24;
    case 3: b |= ((u64)p[2]) << 16;
    case 2: b |= ((u64)p[1]) <<  8;
    case 1: b |= ((u64)p[0]);
    }

    v3 ^= b;
    if (c == 2) {
	SIPROUND;
	SIPROUND;
    } else {
	for (i = 0; i < c; i++)
	    SIPROUND;
    }
    v0 ^= b;

    v2 ^= 0xff;
    if (d == 4) {
    SIPROUND;
    SIPROUND;
    SIPROUND;
    SIPROUND;
    } else {
	for (i = 0; i < d; i++)
	    SIPROUND;
    }
    b = v0 ^ v1 ^ v2  ^ v3;
    return b;
}
