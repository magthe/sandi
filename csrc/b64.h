#ifndef _B64_H_
#define _B64_H_

#include <stddef.h>
#include <stdint.h>

void b64_encode(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b64_finalize(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

#endif
