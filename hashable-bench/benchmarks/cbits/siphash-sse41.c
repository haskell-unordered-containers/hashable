/*
 * The original code was developed by Samuel Neves, and has been
 * only lightly modified.
 *
 * Used with permission.
 */
#pragma GCC target("sse4.1")

#include <smmintrin.h>
#include "siphash.h"

// Specialized for siphash, do not reuse
#define rotate16(x) _mm_shufflehi_epi16((x), _MM_SHUFFLE(2,1,0,3))

#define _mm_roti_epi64(x, c) (((c) == 16) ? rotate16((x)) : _mm_xor_si128(_mm_slli_epi64((x), (c)), _mm_srli_epi64((x), 64-(c))))
//#define _mm_roti_epi64(x, c)  _mm_xor_si128(_mm_slli_epi64((x), (c)), _mm_srli_epi64((x), 64-(c)))


u64 hashable_siphash24_sse41(u64 _k0, u64 _k1, const unsigned char *m, size_t n)
{
	__m128i v0, v1, v02, v13;
	__m128i k0;
	__m128i mi, mask, len, h;
	const __m128i zero = _mm_setzero_si128();
	size_t i, k;
	union { u64 gpr; __m128i xmm; } hash;
	unsigned char key[16];

	((u64 *)key)[0] = _k0;
	((u64 *)key)[1] = _k1;

	k0 = _mm_loadu_si128((__m128i*)(key + 0));

	v0 = _mm_xor_si128(k0, _mm_set_epi32(0x646f7261, 0x6e646f6d, 0x736f6d65, 0x70736575));
	v1 = _mm_xor_si128(k0, _mm_set_epi32(0x74656462, 0x79746573, 0x6c796765, 0x6e657261));

	v02 = _mm_unpacklo_epi64(v0, v1);
	v13 = _mm_unpackhi_epi64(v0, v1);

#define HALF_ROUND(a,b,s,t) \
do \
{ \
	__m128i b1,b2; \
	a = _mm_add_epi64(a, b);  \
	b1 = _mm_roti_epi64(b, s); b2 = _mm_roti_epi64(b, t); b = _mm_blend_epi16(b1, b2, 0xF0); \
	b = _mm_xor_si128(b, a);  \
} while(0)

#define COMPRESS(v02,v13) \
	do \
	{ \
		HALF_ROUND(v02,v13,13,16); \
		v02 = _mm_shuffle_epi32(v02, _MM_SHUFFLE(0,1,3,2)); \
		HALF_ROUND(v02,v13,17,21); \
		v02 = _mm_shuffle_epi32(v02, _MM_SHUFFLE(0,1,3,2)); \
	} while(0)

	for(i = 0; i < (n-n%8); i += 8)
	{
		mi = _mm_loadl_epi64((__m128i*)(m + i));
		v13 = _mm_xor_si128(v13, _mm_unpacklo_epi64(zero, mi));
		for(k = 0; k < SIPHASH_ROUNDS; ++k) COMPRESS(v02,v13);
		v02 = _mm_xor_si128(v02, mi);
	}

	mi = _mm_loadl_epi64((__m128i*)(m + i));
	len = _mm_set_epi32(0, 0, (n&0xff) << 24, 0);
	mask = _mm_srli_epi64(_mm_set_epi32(0, 0, 0xffffffff, 0xffffffff), 8*(8-n%8));
	mi = _mm_xor_si128(_mm_and_si128(mi, mask), len);

	v13 = _mm_xor_si128(v13, _mm_unpacklo_epi64(zero, mi));
	for(k = 0; k < SIPHASH_ROUNDS; ++k) COMPRESS(v02,v13);
	v02 = _mm_xor_si128(v02, mi);

	v02 = _mm_xor_si128(v02, _mm_set_epi32(0, 0xff, 0, 0));
	for(k = 0; k < SIPHASH_FINALROUNDS; ++k) COMPRESS(v02,v13);

	v0 = _mm_xor_si128(v02, v13);
	v0 = _mm_xor_si128(v0, _mm_castps_si128(_mm_movehl_ps(_mm_castsi128_ps(zero), _mm_castsi128_ps(v0))));
	hash.xmm = v0;

#undef COMPRESS
#undef HALF_ROUND
	//return _mm_extract_epi32(v0, 0) | (((u64)_mm_extract_epi32(v0, 1)) << 32);
	return hash.gpr;
}
