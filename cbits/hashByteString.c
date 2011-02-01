/* Bernstein's hash */
long djb_hash(const unsigned char* str, long len, long hash) {

  while (len--) {
    hash = (hash * 33) ^ *str++;
  }

  return hash;
}
