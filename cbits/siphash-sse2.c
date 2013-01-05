/*
 * The original code was developed by Samuel Neves, and has been
 * only lightly modified.
 *
 * Used with permission.
 */
#pragma GCC target("sse2")

#include <emmintrin.h>
#include "siphash.h"

#define _mm_roti_epi64(x, c) ((16 == (c)) ? _mm_shufflelo_epi16((x), _MM_SHUFFLE(2,1,0,3)) : _mm_xor_si128(_mm_slli_epi64((x), (c)), _mm_srli_epi64((x), 64-(c))))

u64 hashable_siphash24_sse2(u64 ik0, u64 ik1, const u8 *m, size_t n)
{
	__m128i v0, v1, v2, v3;
	__m128i k0, k1;
	__m128i mi, mask, len;
	size_t i, k;
	union { u64 gpr; __m128i xmm; } hash;

	k0 = _mm_loadl_epi64((__m128i*)(&ik0));
	k1 = _mm_loadl_epi64((__m128i*)(&ik1));

	v0 = _mm_xor_si128(k0, _mm_set_epi32(0, 0, 0x736f6d65, 0x70736575));
	v1 = _mm_xor_si128(k1, _mm_set_epi32(0, 0, 0x646f7261, 0x6e646f6d));
	v2 = _mm_xor_si128(k0, _mm_set_epi32(0, 0, 0x6c796765, 0x6e657261));
	v3 = _mm_xor_si128(k1, _mm_set_epi32(0, 0, 0x74656462, 0x79746573));

#define HALF_ROUND(a,b,c,d,s,t) \
	do \
	{ \
		a = _mm_add_epi64(a, b);  c = _mm_add_epi64(c, d); \
		b = _mm_roti_epi64(b, s); d = _mm_roti_epi64(d, t); \
		b = _mm_xor_si128(b, a);  d = _mm_xor_si128(d, c); \
	} while(0)

#define COMPRESS(v0,v1,v2,v3) \
	do \
	{ \
		HALF_ROUND(v0,v1,v2,v3,13,16); \
		v0 = _mm_shufflelo_epi16(v0, _MM_SHUFFLE(1,0,3,2)); \
		HALF_ROUND(v2,v1,v0,v3,17,21); \
		v2 = _mm_shufflelo_epi16(v2, _MM_SHUFFLE(1,0,3,2)); \
	} while(0)

	for(i = 0; i < (n-n%8); i += 8)
	{
		mi = _mm_loadl_epi64((__m128i*)(m + i));
		v3 = _mm_xor_si128(v3, mi);
		for(k = 0; k < SIPHASH_ROUNDS; ++k) COMPRESS(v0,v1,v2,v3);
		v0 = _mm_xor_si128(v0, mi);
	}

	mi = _mm_loadl_epi64((__m128i*)(m + i));
	len = _mm_set_epi32(0, 0, (n&0xff) << 24, 0);
	mask = _mm_srli_epi64(_mm_set_epi32(0, 0, 0xffffffff, 0xffffffff), 8*(8-n%8));
	mi = _mm_xor_si128(_mm_and_si128(mi, mask), len);

	v3 = _mm_xor_si128(v3, mi);
	for(k = 0; k < SIPHASH_ROUNDS; ++k) COMPRESS(v0,v1,v2,v3);
	v0 = _mm_xor_si128(v0, mi);

	v2 = _mm_xor_si128(v2, _mm_set_epi32(0, 0, 0, 0xff));
	for(k = 0; k < SIPHASH_FINALROUNDS; ++k) COMPRESS(v0,v1,v2,v3);

	v0 = _mm_xor_si128(_mm_xor_si128(v0, v1), _mm_xor_si128(v2, v3));
	hash.xmm = v0;

#undef COMPRESS
#undef HALF_ROUND
	//return _mm_extract_epi32(v0, 0) | (((u64)_mm_extract_epi32(v0, 1)) << 32);
	return hash.gpr;
}
