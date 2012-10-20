// Copyright: (c) Magnus Therning, 2012
// License: BSD3, found in the LICENSE file

#ifndef _UU_H_
#define _UU_H_

#include <stddef.h>
#include <stdint.h>

void uu_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int uu_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int uu_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int uu_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

#endif
