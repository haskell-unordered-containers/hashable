/* Bernstein's hash */
int djb_hash(const unsigned char* str, int len, int hash) {

  while (len--) {
    hash = (hash * 33) ^ *str++;
  }

  return hash;
}
