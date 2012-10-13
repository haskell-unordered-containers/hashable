#include <stdint.h>

/*
 * 32- and 64-bit hashes by Thomas Wang.
 */

uint32_t hash_wang_32(uint32_t a)
{
    a = (a ^ 61) ^ (a >> 16);
    a = a + (a << 3);
    a = a ^ (a >> 4);
    a = a * 0x27d4eb2d;
    a = a ^ (a >> 15);
    return a;
}

uint64_t hash_wang_64(uint64_t key)
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

/*
 * 32-bit hashes by Bob Jenkins.
 */

uint32_t hash_jenkins_32a(uint32_t a)
{
    a = (a+0x7ed55d16) + (a<<12);
    a = (a^0xc761c23c) ^ (a>>19);
    a = (a+0x165667b1) + (a<<5);
    a = (a+0xd3a2646c) ^ (a<<9);
    a = (a+0xfd7046c5) + (a<<3);
    a = (a^0xb55a4f09) ^ (a>>16);
    return a;
}

uint32_t hash_jenkins_32b(uint32_t a)
{
    a -= (a<<6);
    a ^= (a>>17);
    a -= (a<<9);
    a ^= (a<<4);
    a -= (a<<3);
    a ^= (a<<10);
    a ^= (a>>15);
    return a;
}
