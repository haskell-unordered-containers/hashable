/* Bernstein's hash */
int hashByteString(const char* str, int len) {
  int hash = 0;

  while (len--) {
    hash = (hash * 33) ^ *str++;
  }

  return hash;
}
