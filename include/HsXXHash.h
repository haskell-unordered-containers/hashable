#ifndef HS_XXHASH_H
#define HS_XXHASH_H

#include <stdint.h>

#define XXH_INLINE_ALL
#include "xxhash.h"

#define hs_XXH3_sizeof_state_s sizeof(struct XXH3_state_s)

static inline uint64_t hs_XXH3_64bits_withSeed_offset(const uint8_t *ptr, size_t off, size_t len, uint64_t seed) {
    return XXH3_64bits_withSeed(ptr + off, len, seed);
}

static inline uint64_t hs_XXH3_64bits_withSeed_u64(uint64_t val, uint64_t seed) {
    return XXH3_64bits_withSeed(&val, sizeof(val), seed);
}

static inline uint64_t hs_XXH3_64bits_withSeed_u32(uint32_t val, uint64_t seed) {
    return XXH3_64bits_withSeed(&val, sizeof(val), seed);
}

static inline void hs_XXH3_64bits_update_offset(XXH3_state_t *statePtr, const uint8_t *ptr, size_t off, size_t len) {
    XXH3_64bits_update(statePtr, ptr + off, len);
}

static inline void hs_XXH3_64bits_update_u64(XXH3_state_t *statePtr, uint64_t val) {
    XXH3_64bits_update(statePtr, &val, sizeof(val));
}

static inline void hs_XXH3_64bits_update_u32(XXH3_state_t *statePtr, uint32_t val) {
    XXH3_64bits_update(statePtr, &val, sizeof(val));
}

#endif /* HS_XXHASH_H */
