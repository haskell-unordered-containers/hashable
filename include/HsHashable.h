#ifndef HS_HASHABLE_H
#define HS_HASHABLE_H

#include "MachDeps.h"
#include <stdint.h>

#if WORD_SIZE_IN_BITS == 64
#define FNV_PRIME 1099511628211
#define FNV_SIGNED int64_t
#define FNV_UNSIGNED uint64_t
#else
#define FNV_PRIME 16777619
#define FNV_SIGNED int32_t
#define FNV_UNSIGNED uint32_t
#endif

uint64_t hs_hashable_init();

FNV_UNSIGNED hashable_fnv_hash(const unsigned char* str, FNV_SIGNED len, FNV_UNSIGNED salt);
FNV_UNSIGNED hashable_fnv_hash_offset(const unsigned char* str, FNV_SIGNED offset, FNV_SIGNED len, FNV_UNSIGNED salt);

#endif
