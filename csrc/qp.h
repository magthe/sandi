// Copyright: (c) Magnus Therning, 2012
// License: BSD3, found in the LICENSE file

#ifndef _QP_H_
#define _QP_H_

#include <stddef.h>
#include <stdint.h>

void qp_enc(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);
int qp_dec(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen);

#endif
