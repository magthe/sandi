#ifndef _B32_H_
#define _B32_H_

#include <stddef.h>
#include <stdint.h>

void b32_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b32_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b32_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b32_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

#endif
