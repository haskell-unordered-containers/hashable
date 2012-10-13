/*
 * These hash functions were developed by Thomas Wang.
 *
 * http://www.concentric.net/~ttwang/tech/inthash.htm
 */

#include <stdint.h>

uint32_t hashable_wang_32(uint32_t a)
{
    a = (a ^ 61) ^ (a >> 16);
    a = a + (a << 3);
    a = a ^ (a >> 4);
    a = a * 0x27d4eb2d;
    a = a ^ (a >> 15);
    return a;
}

uint64_t hashable_wang_64(uint64_t key)
{
    key = (~key) + (key << 21); // key = (key << 21) - key - 1;
    key = key ^ ((key >> 24) | (key << 40));
    key = (key + (key << 3)) + (key << 8); // key * 265
    key = key ^ ((key >> 14) | (key << 50));
    key = (key + (key << 2)) + (key << 4); // key * 21
    key = key ^ ((key >> 28) | (key << 36));
    key = key + (key << 31);
    return key;
}
