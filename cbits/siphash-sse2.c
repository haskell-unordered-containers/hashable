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
	const u8 *p;

	/* We used to use the _mm_seti_epi32 intrinsic to initialize
	   SSE2 registers. This compiles to a movdqa instruction,
	   which requires 16-byte alignment. On 32-bit Windows, it
	   looks like ghc's runtime linker doesn't align ".rdata"
	   sections as requested, so we got segfaults for our trouble.

	   Now we use an intrinsic that cares less about alignment
	   (_mm_loadu_si128, aka movdqu) instead, and all seems
	   happy. */

	static const u32 const iv[6][4] = {
		{ 0x70736575, 0x736f6d65, 0, 0 },
		{ 0x6e646f6d, 0x646f7261, 0, 0 },
		{ 0x6e657261, 0x6c796765, 0, 0 },
		{ 0x79746573, 0x74656462, 0, 0 },
		{ -1, -1, 0, 0 },
		{ 255, 0, 0, 0 },
	};

	k0 = _mm_loadl_epi64((__m128i*)(&ik0));
	k1 = _mm_loadl_epi64((__m128i*)(&ik1));

	v0 = _mm_xor_si128(k0, _mm_loadu_si128((__m128i*) &iv[0]));
	v1 = _mm_xor_si128(k1, _mm_loadu_si128((__m128i*) &iv[1]));
	v2 = _mm_xor_si128(k0, _mm_loadu_si128((__m128i*) &iv[2]));
	v3 = _mm_xor_si128(k1, _mm_loadu_si128((__m128i*) &iv[3]));

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
		if (SIPHASH_ROUNDS == 2) {
			COMPRESS(v0,v1,v2,v3); COMPRESS(v0,v1,v2,v3);
		} else {
			for (k = 0; k < SIPHASH_ROUNDS; ++k)
				COMPRESS(v0,v1,v2,v3);
		}
		v0 = _mm_xor_si128(v0, mi);
	}

	p = m + n;

	/* We must be careful to not trigger a segfault by reading an
	   unmapped page. So where is the end of our input? */

	if (((uintptr_t) p & 4095) == 0)
		/* Exactly at a page boundary: do not read past the end. */
		mi = _mm_setzero_si128();
	else if (((uintptr_t) p & 4095) <= 4088)
		/* Inside a page: safe to read past the end, as we'll
		   mask out any bits we shouldn't have looked at below. */
		mi = _mm_loadl_epi64((__m128i*)(m + i));
	else
		/* Within 8 bytes of the end of a page: ensure that
		   our final read re-reads some bytes so that we do
		   not cross the page boundary, then shift our result
		   right so that the re-read bytes vanish. */
		mi = _mm_srli_epi64(_mm_loadl_epi64((__m128i*)(((uintptr_t) m + i) & ~7)),
				    8 * (((uintptr_t) m + i) % 8));

	len = _mm_set_epi32(0, 0, (n&0xff) << 24, 0);
	mask = _mm_srli_epi64(_mm_loadu_si128((__m128i*) &iv[4]), 8*(8-n%8));
	mi = _mm_xor_si128(_mm_and_si128(mi, mask), len);

	v3 = _mm_xor_si128(v3, mi);
	if (SIPHASH_ROUNDS == 2) {
		COMPRESS(v0,v1,v2,v3); COMPRESS(v0,v1,v2,v3);
	} else {
		for (k = 0; k < SIPHASH_ROUNDS; ++k)
			COMPRESS(v0,v1,v2,v3);
	}
	v0 = _mm_xor_si128(v0, mi);

	v2 = _mm_xor_si128(v2, _mm_loadu_si128((__m128i*) &iv[5]));
	if (SIPHASH_FINALROUNDS == 4) {
		COMPRESS(v0,v1,v2,v3); COMPRESS(v0,v1,v2,v3);
		COMPRESS(v0,v1,v2,v3); COMPRESS(v0,v1,v2,v3);
	} else {
		for (k = 0; k < SIPHASH_FINALROUNDS; ++k)
			COMPRESS(v0,v1,v2,v3);
	}

	v0 = _mm_xor_si128(_mm_xor_si128(v0, v1), _mm_xor_si128(v2, v3));
	hash.xmm = v0;

#undef COMPRESS
#undef HALF_ROUND
	//return _mm_extract_epi32(v0, 0) | (((u64)_mm_extract_epi32(v0, 1)) << 32);
	return hash.gpr;
}
