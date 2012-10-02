#include "MachDeps.h"

int hashable_getRandomBytes(int nbytes, unsigned char *dest);

#if defined(mingw32_HOST_OS) || defined(__MINGW32__) 

#include "wincrypt.h"

int hashable_getRandomBytes(int nbytes, unsigned char *dest)
{
  HCRYPTPROV hCryptProv;
  int ret;

  if (CryptAcquireContext(&hCryptProv, NULL, NULL, PROV_RSA_FULL,
			  CRYPT_VERIFYCONTEXT))
    return -1;
  
  if (!CryptGenRandom(hCryptProv, (DWOORD) nbytes, (BYTE *) dest))
    ret = -1;

  CryptReleaseContext(hCryptProv, 0);

 bail:
  return ret;
}

#else

#include <fcntl.h>
#include <sys/types.h>
#include <unistd.h>

int hashable_getRandomBytes(int nbytes, unsigned char *dest)
{
  ssize_t off, nread;
  int fd;
  
  fd = open("/dev/urandom", O_RDONLY);
  if (fd == -1)
    return -1;

  for (off = 0; nbytes > 0; nbytes -= nread) {
    nread = read(fd, dest + off, nbytes);
    off += nread;
    if (nread == -1) {
      off = -1;
      break;
    }
  }

 bail:
  close(fd);

  return off;
}

#endif
