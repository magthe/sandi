#ifndef _B64_H_
#define _B64_H_

#include <stddef.h>
#include <stdint.h>

void b64_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b64_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b64_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b64_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

#endif
