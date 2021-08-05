// Copyright: (c) Magnus Therning, 2012, 2013
// License: BSD3, found in the LICENSE file

#ifndef _CODEC_H_
#define _CODEC_H_

#include <stddef.h>
#include <stdint.h>

void b16_enc(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b16_dec(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);

void b32_enc_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b32_enc_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b32_dec_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b32_dec_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

void b32h_enc_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b32h_enc_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b32h_dec_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b32h_dec_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

void b64_enc_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b64_enc_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b64_dec_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b64_dec_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

void b64u_enc_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b64u_enc_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b64u_dec_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b64u_dec_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

void b85_enc_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b85_enc_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int b85_dec_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int b85_dec_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

void qp_enc(uint8_t split, uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int qp_dec(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);

void uu_enc_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int uu_enc_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int uu_dec_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int uu_dec_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

void xx_enc_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int xx_enc_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);
int xx_dec_part(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int xx_dec_final(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen);

void y_enc(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);
int y_dec(uint8_t *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t **rem, size_t *remlen);

#endif
