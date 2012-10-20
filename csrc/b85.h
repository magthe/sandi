// Copyright: (c) Magnus Therning, 2012
// License: BSD3, found in the LICENSE file

#ifndef _B85_H_
#define _B85_H_

#include <stddef.h>
#include <stdint.h>

void b85_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b85_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b85_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int b85_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

#endif
