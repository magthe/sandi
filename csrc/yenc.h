// Copyright: (c) Magnus Therning, 2012
// License: BSD3, found in the LICENSE file

#ifndef _YENC_H_
#define _YENC_H_

#include <stddef.h>
#include <stdint.h>

void y_enc(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int y_dec(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);

#endif
