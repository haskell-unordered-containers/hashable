/* Bernstein's hash */
long djb_hash(const unsigned char* str, long len, long hash) {

  while (len--) {
    hash = (hash * 33) ^ *str++;
  }

  return hash;
}

/* Used for ByteArray#s. We can't treat them like pointers in
   native Haskell, but we can in unsafe FFI calls.
 */
long djb_hash_offset(const unsigned char* str, long offset, long len, long hash) {
  return djb_hash(str + offset, len, hash);
}
