#ifndef _B64URL_H_
#define _B64URL_H_

#include <stddef.h>
#include <stdint.h>

void b64u_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b64u_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b64u_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b64u_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

#endif
