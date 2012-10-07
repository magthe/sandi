#ifndef _XX_H_
#define _XX_H_

#include <stddef.h>
#include <stdint.h>

void xx_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int xx_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int xx_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int xx_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

#endif
