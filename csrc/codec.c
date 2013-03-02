// Copyright: (c) Magnus Therning, 2012
// License: BSD3, found in the LICENSE file

#include <assert.h>
#include <string.h>

#include "codec.h"

// {{{1 base16
static char const b16_encmap[] = "0123456789ABCDEF";

void b16_enc(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t od = *dstlen, i;

    assert(src || srclen == 0);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i < srclen && *dstlen + 1 < od; i++, *dstlen += 2) {
        uint8_t o0 = src[i] >> 4, o1 = src[i] & 0x0f;
        dst[*dstlen] = b16_encmap[o0];
        dst[*dstlen + 1] = b16_encmap[o1];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

static uint8_t const b16_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

int b16_dec(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t od = *dstlen, i;
    int res = 0;

    assert(src || srclen == 0);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i < srclen && *dstlen < od; i += 2, (*dstlen)++) {
        if(i + 1 >= srclen) { res = 0; break; }
        uint8_t o0 = b16_decmap[src[i]], o1 = b16_decmap[src[i + 1]];
        if((o0 | o1) & 0xf0) { res = 1; break; }
        else dst[*dstlen] = o0 << 4 | o1;
    }

    *rem = src + i;
    *remlen = srclen - i;

    return(res);
}

// {{{1 base32

static char const b32_encmap[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567";

void b32_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t od = *dstlen, i;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i + 5 <= srclen && *dstlen + 8 <= od; i += 5, *dstlen += 8) {
        int32_t o0, o1, o2, o3, o4, o5, o6, o7;
        o0 = src[i] >> 3;
        o1 = ((src[i] << 2) | (src[i+1] >> 6)) & 0x1f;
        o2 = (src[i+1] >> 1) & 0x1f;
        o3 = ((src[i+1] << 4) | (src[i+2] >> 4)) & 0x1f;
        o4 = ((src[i+2] << 1) | (src[i+3] >> 7)) & 0x1f;
        o5 = (src[i+3] >>2) & 0x1f;
        o6 = ((src[i+3] << 3) | (src[i+4] >> 5)) & 0x1f;
        o7 = src[i+4] & 0x1f;
        *dst++ = b32_encmap[o0];
        *dst++ = b32_encmap[o1];
        *dst++ = b32_encmap[o2];
        *dst++ = b32_encmap[o3];
        *dst++ = b32_encmap[o4];
        *dst++ = b32_encmap[o5];
        *dst++ = b32_encmap[o6];
        *dst++ = b32_encmap[o7];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

int b32_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        int32_t o0, o1, o2, o3, o4, o5, o6;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 3;
        o1 = (src[0] << 2) & 0x1f;
        *dst++ = b32_encmap[o0];
        *dst++ = b32_encmap[o1];
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    case 2:
        o0 = src[0] >> 3;
        o1 = ((src[0] << 2) | (src[1] >> 6)) & 0x1f;
        o2 = (src[1] >> 1) & 0x1f;
        o3 = src[1] << 4 & 0x1f;
        *dst++ = b32_encmap[o0];
        *dst++ = b32_encmap[o1];
        *dst++ = b32_encmap[o2];
        *dst++ = b32_encmap[o3];
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    case 3:
        o0 = src[0] >> 3;
        o1 = ((src[0] << 2) | (src[1] >> 6)) & 0x1f;
        o2 = (src[1] >> 1) & 0x1f;
        o3 = ((src[1] << 4) | (src[2] >> 4)) & 0x1f;
        o4 = (src[2] << 1) & 0x1f;
        *dst++ = b32_encmap[o0];
        *dst++ = b32_encmap[o1];
        *dst++ = b32_encmap[o2];
        *dst++ = b32_encmap[o3];
        *dst++ = b32_encmap[o4];
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    case 4:
        o0 = src[0] >> 3;
        o1 = ((src[0] << 2) | (src[1] >> 6)) & 0x1f;
        o2 = (src[1] >> 1) & 0x1f;
        o3 = ((src[1] << 4) | (src[2] >> 4)) & 0x1f;
        o4 = ((src[2] << 1) | (src[3] >> 7)) & 0x1f;
        o5 = (src[3] >>2) & 0x1f;
        o6 = (src[3] << 3) & 0x1f;
        *dst++ = b32_encmap[o0];
        *dst++ = b32_encmap[o1];
        *dst++ = b32_encmap[o2];
        *dst++ = b32_encmap[o3];
        *dst++ = b32_encmap[o4];
        *dst++ = b32_encmap[o5];
        *dst++ = b32_encmap[o6];
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

// decode map, 0x80 = not allowed, 0x40 = end char
static uint8_t const b32_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x80, 0x80, 0x80, 0x80, 0x80, 0x40, 0x80, 0x80,
    0x80, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
    0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80 };

int b32_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t od = *dstlen, i;
    int res = 0;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i + 8 <= srclen && *dstlen + 5 <= od; i += 8, *dstlen += 5) {
        uint8_t o0, o1, o2, o3, o4, o5, o6, o7;

        o0 = b32_decmap[src[i]];
        o1 = b32_decmap[src[i+1]];
        o2 = b32_decmap[src[i+2]];
        o3 = b32_decmap[src[i+3]];
        o4 = b32_decmap[src[i+4]];
        o5 = b32_decmap[src[i+5]];
        o6 = b32_decmap[src[i+6]];
        o7 = b32_decmap[src[i+7]];
        if(!(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6 | o7))) { // no illegal chars, and no '='
            *dst++ = (o0 << 3) | (o1 >> 2);
            *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
            *dst++ = (o3 << 4) | (o4 >> 1);
            *dst++ = (o4 << 7) | (o5 << 2) | (o6 >> 3);
            *dst++ = (o6 << 5) | o7;
        } else if((!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3 & o4 & o5 & o6 & o7)) // two legal chars, six '='
            || (!(0xc0 & (o0 | o1 | o2 | o3)) && (0x40 & o4 & o5 & o6 & o7)) // four legal chars, four '='
            || (!(0xc0 & (o0 | o1 | o2 | o3 | o4)) && (0x40 & o5 & o6 & o7)) // five legal chars, three '='
            || (!(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6)) && (0x40 & o7))) { // seven legal chars, one '='
            res = 0;
            break;
        } else {
            res = 1;
            break;
        }
    }

    *rem = src + i;
    *remlen = srclen - i;
    return(res);
}

int b32_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    uint8_t o0, o1, o2, o3, o4, o5, o6, o7;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    if(0 == srclen) {
        *dstlen = 0;
        return(0);
    }
    o0 = b32_decmap[src[0]];
    o1 = b32_decmap[src[1]];
    o2 = b32_decmap[src[2]];
    o3 = b32_decmap[src[3]];
    o4 = b32_decmap[src[4]];
    o5 = b32_decmap[src[5]];
    o6 = b32_decmap[src[6]];
    o7 = b32_decmap[src[7]];
    if(!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3 & o4 & o5 & o6 & o7)) { // two legal chars, six '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6);
        *dstlen = 1;
    } else if(!(0xc0 & (o0 | o1 | o2 | o3)) && (0x40 & o4 & o5 & o6 & o7)) { // four legal chars, four '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
        *dst++ = (o3 << 4);
        *dstlen = 2;
    }
    else if(!(0xc0 & (o0 | o1 | o2 | o3 | o4)) && (0x40 & o5 & o6 & o7)) { // five legal chars, three '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
        *dst++ = (o3 << 4) | (o4 >> 1);
        *dst++ = (o4 << 7) | (o5 << 2) | (o6 >> 3);
        *dstlen = 3;
    } else if(!(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6)) && (0x40 & o7)) { // seven legal chars, one '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
        *dst++ = (o3 << 4) | (o4 >> 1);
        *dst++ = (o4 << 7) | (o5 << 2) | (o6 >> 3);
        *dst++ = (o6 << 5);
        *dstlen = 4;
    } else
        return(1);

    return(0);
}

// {{{1 base32hex

static char const b32h_encmap[] = "0123456789ABCDEFGHIJKLMNOPQRSTUV";

void b32h_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t od = *dstlen, i;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i + 5 <= srclen && *dstlen + 8 <= od; i += 5, *dstlen += 8) {
        int32_t o0, o1, o2, o3, o4, o5, o6, o7;
        o0 = src[i] >> 3;
        o1 = ((src[i] << 2) | (src[i+1] >> 6)) & 0x1f;
        o2 = (src[i+1] >> 1) & 0x1f;
        o3 = ((src[i+1] << 4) | (src[i+2] >> 4)) & 0x1f;
        o4 = ((src[i+2] << 1) | (src[i+3] >> 7)) & 0x1f;
        o5 = (src[i+3] >>2) & 0x1f;
        o6 = ((src[i+3] << 3) | (src[i+4] >> 5)) & 0x1f;
        o7 = src[i+4] & 0x1f;
        *dst++ = b32h_encmap[o0];
        *dst++ = b32h_encmap[o1];
        *dst++ = b32h_encmap[o2];
        *dst++ = b32h_encmap[o3];
        *dst++ = b32h_encmap[o4];
        *dst++ = b32h_encmap[o5];
        *dst++ = b32h_encmap[o6];
        *dst++ = b32h_encmap[o7];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

int b32h_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        int32_t o0, o1, o2, o3, o4, o5, o6;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 3;
        o1 = (src[0] << 2) & 0x1f;
        *dst++ = b32h_encmap[o0];
        *dst++ = b32h_encmap[o1];
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    case 2:
        o0 = src[0] >> 3;
        o1 = ((src[0] << 2) | (src[1] >> 6)) & 0x1f;
        o2 = (src[1] >> 1) & 0x1f;
        o3 = src[1] << 4 & 0x1f;
        *dst++ = b32h_encmap[o0];
        *dst++ = b32h_encmap[o1];
        *dst++ = b32h_encmap[o2];
        *dst++ = b32h_encmap[o3];
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    case 3:
        o0 = src[0] >> 3;
        o1 = ((src[0] << 2) | (src[1] >> 6)) & 0x1f;
        o2 = (src[1] >> 1) & 0x1f;
        o3 = ((src[1] << 4) | (src[2] >> 4)) & 0x1f;
        o4 = (src[2] << 1) & 0x1f;
        *dst++ = b32h_encmap[o0];
        *dst++ = b32h_encmap[o1];
        *dst++ = b32h_encmap[o2];
        *dst++ = b32h_encmap[o3];
        *dst++ = b32h_encmap[o4];
        *dst++ = '=';
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    case 4:
        o0 = src[0] >> 3;
        o1 = ((src[0] << 2) | (src[1] >> 6)) & 0x1f;
        o2 = (src[1] >> 1) & 0x1f;
        o3 = ((src[1] << 4) | (src[2] >> 4)) & 0x1f;
        o4 = ((src[2] << 1) | (src[3] >> 7)) & 0x1f;
        o5 = (src[3] >>2) & 0x1f;
        o6 = (src[3] << 3) & 0x1f;
        *dst++ = b32h_encmap[o0];
        *dst++ = b32h_encmap[o1];
        *dst++ = b32h_encmap[o2];
        *dst++ = b32h_encmap[o3];
        *dst++ = b32h_encmap[o4];
        *dst++ = b32h_encmap[o5];
        *dst++ = b32h_encmap[o6];
        *dst++ = '=';
        *dstlen = 8;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

// decode map, 0x80 = not allowed, 0x40 = end char
static uint8_t const b32h_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x80, 0x80, 0x80, 0x40, 0x80, 0x80,
    0x80, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18,
    0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80 };

int b32h_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t od = *dstlen, i;
    int res = 0;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i + 8 <= srclen && *dstlen + 5 <= od; i += 8, *dstlen += 5) {
        uint8_t o0, o1, o2, o3, o4, o5, o6, o7;

        o0 = b32h_decmap[src[i]];
        o1 = b32h_decmap[src[i+1]];
        o2 = b32h_decmap[src[i+2]];
        o3 = b32h_decmap[src[i+3]];
        o4 = b32h_decmap[src[i+4]];
        o5 = b32h_decmap[src[i+5]];
        o6 = b32h_decmap[src[i+6]];
        o7 = b32h_decmap[src[i+7]];
        if(!(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6 | o7))) { // no illegal chars, and no '='
            *dst++ = (o0 << 3) | (o1 >> 2);
            *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
            *dst++ = (o3 << 4) | (o4 >> 1);
            *dst++ = (o4 << 7) | (o5 << 2) | (o6 >> 3);
            *dst++ = (o6 << 5) | o7;
        } else if((!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3 & o4 & o5 & o6 & o7)) // two legal chars, six '='
            || (!(0xc0 & (o0 | o1 | o2 | o3)) && (0x40 & o4 & o5 & o6 & o7)) // four legal chars, four '='
            || (!(0xc0 & (o0 | o1 | o2 | o3 | o4)) && (0x40 & o5 & o6 & o7)) // five legal chars, three '='
            || (!(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6)) && (0x40 & o7))) { // seven legal chars, one '='
            res = 0;
            break;
        } else {
            res = 1;
            break;
        }
    }

    *rem = src + i;
    *remlen = srclen - i;
    return(res);
}

int b32h_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    uint8_t o0, o1, o2, o3, o4, o5, o6, o7;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    if(0 == srclen) {
        *dstlen = 0;
        return(0);
    }
    o0 = b32h_decmap[src[0]];
    o1 = b32h_decmap[src[1]];
    o2 = b32h_decmap[src[2]];
    o3 = b32h_decmap[src[3]];
    o4 = b32h_decmap[src[4]];
    o5 = b32h_decmap[src[5]];
    o6 = b32h_decmap[src[6]];
    o7 = b32h_decmap[src[7]];
    if(!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3 & o4 & o5 & o6 & o7)) { // two legal chars, six '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6);
        *dstlen = 1;
    } else if(!(0xc0 & (o0 | o1 | o2 | o3)) && (0x40 & o4 & o5 & o6 & o7)) { // four legal chars, four '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
        *dst++ = (o3 << 4);
        *dstlen = 2;
    }
    else if(!(0xc0 & (o0 | o1 | o2 | o3 | o4)) && (0x40 & o5 & o6 & o7)) { // five legal chars, three '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
        *dst++ = (o3 << 4) | (o4 >> 1);
        *dst++ = (o4 << 7) | (o5 << 2) | (o6 >> 3);
        *dstlen = 3;
    } else if(!(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6)) && (0x40 & o7)) { // seven legal chars, one '='
        *dst++ = (o0 << 3) | (o1 >> 2);
        *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
        *dst++ = (o3 << 4) | (o4 >> 1);
        *dst++ = (o4 << 7) | (o5 << 2) | (o6 >> 3);
        *dst++ = (o6 << 5);
        *dstlen = 4;
    } else
        return(1);

    return(0);
}

// {{{1 base64

static char const b64_encmap[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void b64_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i + 3 <= srclen && *dstlen + 4 <= od; i += 3, *dstlen += 4) {
        int32_t o0, o1, o2, o3;
        o0 = src[i] >> 2;
        o1 = ((src[i] << 4) | (src[i+1] >> 4)) & 0x3f;
        o2 = ((src[i+1] << 2) | (src[i+2] >> 6)) & 0x3f;
        o3 = src[i+2] & 0x3f;
        *dst++ = b64_encmap[o0];
        *dst++ = b64_encmap[o1];
        *dst++ = b64_encmap[o2];
        *dst++ = b64_encmap[o3];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

int b64_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        int32_t o0, o1, o2;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 2;
        o1 = (src[0] << 4) & 0x3f;
        *dst++ = b64_encmap[o0];
        *dst++ = b64_encmap[o1];
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 4;
        return(0);
        break;
    case 2:
        o0 = src[0] >> 2;
        o1 = ((src[0] << 4) | (src[1] >> 4)) & 0x3f;
        o2 = (src[1] << 2) & 0x3f;
        *dst++ = b64_encmap[o0];
        *dst++ = b64_encmap[o1];
        *dst++ = b64_encmap[o2];
        *dst++ = '=';
        *dstlen = 4;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

// decode map, 0x80 = not allowed, 0x40 = end char
static uint8_t const b64_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x3e, 0x80, 0x80, 0x80, 0x3f,
    0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x80, 0x80, 0x80, 0x40, 0x80, 0x80,
    0x80, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
    0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
    0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80 };


int b64_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;
    int res = 0;

    for(i = 0, *dstlen = 0; i + 4 <= srclen && *dstlen + 3 <= od; i += 4, *dstlen += 3) {
        uint8_t o0, o1, o2, o3;

        o0 = b64_decmap[src[i]];
        o1 = b64_decmap[src[i+1]];
        o2 = b64_decmap[src[i+2]];
        o3 = b64_decmap[src[i+3]];
        if(!(0xc0 & (o0 | o1 | o2 | o3))) { // no illegal chars, and no '='
            *dst++ = (o0 << 2) | (o1 >> 4);
            *dst++ = (o1 << 4) | (o2 >> 2);
            *dst++ = (o2 << 6) | o3;
        } else if((!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3)) // two legal chars, two '='
            || (!(0xc0 & (o0 | o1 | o2)) && (0x40 & o3))) { // three legal chars, one '='
            res = 0;
            break;
        } else {
            res = 1;
            break;
        }
    }

    *rem = src + i;
    *remlen = srclen - i;
    return(res);
}

int b64_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    uint8_t o0, o1, o2, o3;

    if(0 == srclen) {
        *dstlen = 0;
        return(0);
    }
    o0 = b64_decmap[src[0]];
    o1 = b64_decmap[src[1]];
    o2 = b64_decmap[src[2]];
    o3 = b64_decmap[src[3]];
    if(!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3)) { // two legal chars, two '='
        *dst++ = (o0 << 2) | (o1 >> 4);
        *dstlen = 1;
    } else if(!(0xc0 & (o0 | o1 | o2)) && (0x40 & o3)) { // three legal chars, one '='
            *dst++ = (o0 << 2) | (o1 >> 4);
            *dst++ = (o1 << 4) | (o2 >> 2);
            *dstlen = 2;
    } else
        return(1);

    return(0);
}

// {{{1 base64url

static char const b64u_encmap[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_";

void b64u_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i + 3 <= srclen && *dstlen + 4 <= od; i += 3, *dstlen += 4) {
        int32_t o0, o1, o2, o3;
        o0 = src[i] >> 2;
        o1 = ((src[i] << 4) | (src[i+1] >> 4)) & 0x3f;
        o2 = ((src[i+1] << 2) | (src[i+2] >> 6)) & 0x3f;
        o3 = src[i+2] & 0x3f;
        *dst++ = b64u_encmap[o0];
        *dst++ = b64u_encmap[o1];
        *dst++ = b64u_encmap[o2];
        *dst++ = b64u_encmap[o3];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

int b64u_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        int32_t o0, o1, o2;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 2;
        o1 = (src[0] << 4) & 0x3f;
        *dst++ = b64u_encmap[o0];
        *dst++ = b64u_encmap[o1];
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 4;
        return(0);
        break;
    case 2:
        o0 = src[0] >> 2;
        o1 = ((src[0] << 4) | (src[1] >> 4)) & 0x3f;
        o2 = (src[1] << 2) & 0x3f;
        *dst++ = b64u_encmap[o0];
        *dst++ = b64u_encmap[o1];
        *dst++ = b64u_encmap[o2];
        *dst++ = '=';
        *dstlen = 4;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

// decode map, 0x80 = not allowed, 0x40 = end char
static uint8_t const b64u_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x3e, 0x80, 0x80,
    0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x80, 0x80, 0x80, 0x40, 0x80, 0x80,
    0x80, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
    0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x80, 0x80, 0x80, 0x80, 0x3f,
    0x80, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
    0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80 };


int b64u_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;
    int res = 0;

    for(i = 0, *dstlen = 0; i + 4 <= srclen && *dstlen + 3 <= od; i += 4, *dstlen += 3) {
        uint8_t o0, o1, o2, o3;

        o0 = b64u_decmap[src[i]];
        o1 = b64u_decmap[src[i+1]];
        o2 = b64u_decmap[src[i+2]];
        o3 = b64u_decmap[src[i+3]];
        if(!(0xc0 & (o0 | o1 | o2 | o3))) { // no illegal chars, and no '='
            *dst++ = (o0 << 2) | (o1 >> 4);
            *dst++ = (o1 << 4) | (o2 >> 2);
            *dst++ = (o2 << 6) | o3;
        } else if((!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3)) // two legal chars, two '='
            || (!(0xc0 & (o0 | o1 | o2)) && (0x40 & o3))) { // three legal chars, one '='
            res = 0;
            break;
        } else {
            res = 1;
            break;
        }
    }

    *rem = src + i;
    *remlen = srclen - i;
    return(res);
}

int b64u_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    uint8_t o0, o1, o2, o3;

    if(0 == srclen) {
        *dstlen = 0;
        return(0);
    }
    o0 = b64u_decmap[src[0]];
    o1 = b64u_decmap[src[1]];
    o2 = b64u_decmap[src[2]];
    o3 = b64u_decmap[src[3]];
    if(!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3)) { // two legal chars, two '='
        *dst++ = (o0 << 2) | (o1 >> 4);
        *dstlen = 1;
    } else if(!(0xc0 & (o0 | o1 | o2)) && (0x40 & o3)) { // three legal chars, one '='
            *dst++ = (o0 << 2) | (o1 >> 4);
            *dst++ = (o1 << 4) | (o2 >> 2);
            *dstlen = 2;
    } else
        return(1);

    return(0);
}

// {{{1 base85

uint8_t b85_zeroes[] = { 0, 0, 0, 0 };
uint8_t b85_spaces[] = { 0x20, 0x20, 0x20, 0x20 };

void b85_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src ||  0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i + 4 <= srclen && *dstlen < od; i += 4) {
        if(memcmp(src + i, b85_zeroes, 4) == 0) {
            dst[*dstlen] = 'z';
            *dstlen += 1;
        } else if(memcmp(src + i, b85_spaces, 4) == 0) {
            dst[*dstlen] = 'y';
            *dstlen += 1;
        } else {
            if(od < *dstlen + 5) goto exit;
            uint32_t v = (src[i] << 24) | (src[i+1] << 16) | (src[i+2] << 8) | src[i+3];
            dst[*dstlen + 4] = v % 85 + 33; v /= 85;
            dst[*dstlen + 3] = v % 85 + 33; v /= 85;
            dst[*dstlen + 2] = v % 85 + 33; v /= 85;
            dst[*dstlen + 1] = v % 85 + 33; v /= 85;
            dst[*dstlen] = v % 85 + 33;
            *dstlen += 5;
        }
    }
exit:
    *rem = src + i;
    *remlen = srclen - i;
}

int b85_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src ||  0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        uint32_t v;

    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        v = (src[0] << 24) | 1;
        v /= 85; v /= 85; v /= 85;
        dst[1] = v % 85 + 33; v /= 85;
        dst[0] = v % 85 + 33;
        *dstlen = 2;
        return(0);
        break;
    case 2:
        v = (src[0] << 24) | (src[1] << 16) | 1;
        v /= 85; v /= 85;
        dst[2] = v % 85 + 33; v /= 85;
        dst[1] = v % 85 + 33; v /= 85;
        dst[0] = v % 85 + 33;
        *dstlen = 3;
        return(0);
        break;
    case 3:
        v = (src[0] << 24) | (src[1] << 16) |(src[2] << 8) | 1;
        v /= 85;
        dst[3] = v % 85 + 33; v /= 85;
        dst[2] = v % 85 + 33; v /= 85;
        dst[1] = v % 85 + 33; v /= 85;
        dst[0] = v % 85 + 33;
        *dstlen = 4;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

static uint8_t const b85_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e,
    0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e,
    0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e,
    0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e,
    0x3f, 0x40, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46, 0x47, 0x48, 0x49, 0x4a, 0x4b, 0x4c, 0x4d, 0x4e,
    0x4f, 0x50, 0x51, 0x52, 0x53, 0x54, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

int b85_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src ||  0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;
    int res = 0;

    for(i = 0, *dstlen = 0; i < srclen && *dstlen + 4 <= od; *dstlen += 4) {
        switch(src[i]) {
            uint32_t o0, o1, o2, o3, o4, v;
        case 'z':
            dst[*dstlen + 3] = dst[*dstlen + 2] = dst[*dstlen + 1] = dst[*dstlen] = 0;
            i++;
            break;
        case 'y':
            dst[*dstlen + 3] = dst[*dstlen + 2] = dst[*dstlen + 1] = dst[*dstlen] = 0x20;
            i++;
            break;
        default:
            if(srclen < i + 5) { res = 0; goto exit; }
            o0 = b85_decmap[src[i]];
            o1 = b85_decmap[src[i + 1]];
            o2 = b85_decmap[src[i + 2]];
            o3 = b85_decmap[src[i + 3]];
            o4 = b85_decmap[src[i + 4]];
            if(0x80 & (o0 | o1 | o2 | o3 | o4)) { res = 1; goto exit; }
            v = o0 * 52200625; // 85 ** 4
            v += o1 * 614125; // 85 ** 3
            v += o2 * 7225; // 85 ** 2
            v += o3 * 85; // 85 ** 1
            v += o4; // 85 ** 0
            dst[*dstlen + 3] = v & 0xff; v = v >> 8;
            dst[*dstlen + 2] = v & 0xff; v = v >> 8;
            dst[*dstlen + 1] = v & 0xff; v = v >> 8;
            dst[*dstlen + 0] = v & 0xff;
            i += 5;
            break;
        }
    }
exit:
    *rem = src + i;
    *remlen = srclen - i;
    return(res);
}

int b85_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src ||  0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        uint32_t o0, o1, o2, o3, o4, v;

    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 2:
        o0 = b85_decmap[src[0]];
        o1 = b85_decmap[src[1]];
        o2 = b85_decmap[(uint8_t)'u'];
        o3 = b85_decmap[(uint8_t)'u'];
        o4 = b85_decmap[(uint8_t)'u'];
        if(0x80 & ( o0 | o1)) { return(1); }
        v = o0 * 52200625; // 85 ** 4
        v += o1 * 614125; // 85 ** 3
        v += o2 * 7225; // 85 ** 2
        v += o3 * 85; // 85 ** 1
        v += o4; // 85 ** 0
        v = v >> 24;
        dst[0] = v & 0xff;
        *dstlen = 1;
        return(0);
        break;
    case 3:
        o0 = b85_decmap[src[0]];
        o1 = b85_decmap[src[1]];
        o2 = b85_decmap[src[2]];
        o3 = b85_decmap[(uint8_t)'u'];
        o4 = b85_decmap[(uint8_t)'u'];
        if(0x80 & ( o0 | o1 | o2)) { return(1); }
        v = o0 * 52200625; // 85 ** 4
        v += o1 * 614125; // 85 ** 3
        v += o2 * 7225; // 85 ** 2
        v += o3 * 85; // 85 ** 1
        v += o4; // 85 ** 0
        v = v >> 16;
        dst[1] = v & 0xff; v = v >> 8;
        dst[0] = v & 0xff;
        *dstlen = 2;
        return(0);
        break;
    case 4:
        o0 = b85_decmap[src[0]];
        o1 = b85_decmap[src[1]];
        o2 = b85_decmap[src[2]];
        o3 = b85_decmap[src[3]];
        o4 = b85_decmap[(uint8_t)'u'];
        if(0x80 & ( o0 | o1 | o2 | o3)) { return(1); }
        v = o0 * 52200625; // 85 ** 4
        v += o1 * 614125; // 85 ** 3
        v += o2 * 7225; // 85 ** 2
        v += o3 * 85; // 85 ** 1
        v += o4; // 85 ** 0
        v = v >> 8;
        dst[2] = v & 0xff; v = v >> 8;
        dst[1] = v & 0xff; v = v >> 8;
        dst[0] = v & 0xff;
        *dstlen = 3;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

// {{{1 quoted-printable

static char const qp_encmap[] = "0123456789ABCDEF";

void qp_enc(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || srclen == 0);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i < srclen && *dstlen < od; i++, (*dstlen)++) {
        if((33 <= src[i] && src[i] <= 60) ||
            (62 <= src[i] && src[i] <= 126)) {
            dst[*dstlen] = src[i];
        } else {
            uint8_t o0 = src[i] >> 4, o1 = src[i] & 0x0f;
            if(*dstlen + 3 >= od) goto exit;
            dst[*dstlen] = '=';
            dst[*dstlen + 1] = qp_encmap[o0];
            dst[*dstlen + 2] = qp_encmap[o1];
            *dstlen += 2;
        }
    }

exit:
    *rem = src + i;
    *remlen = srclen -i;
}

static uint8_t const qp_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

int qp_dec(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || srclen == 0);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;
    int res = 0;

    for(i = 0, *dstlen = 0; i < srclen && *dstlen < od; i++, (*dstlen)++) {
        if((33 <= src[i] && src[i] <= 60) ||
            (62 <= src[i] && src[i] <= 126)) {
            dst[*dstlen] = src[i];
        } else if('=' == src[i]) {
            if(i + 2 >= srclen) { res = 1; goto exit; }
            uint8_t o0 = qp_decmap[src[i + 1]], o1 = qp_decmap[src[i + 2]];
            if((o0 | o1) & 0xf0) { res = 1; break; }
            dst[*dstlen] = o0 << 4 | o1;
            i += 2;
        } else { res = 1; goto exit; }
    }

exit:
    *rem = src + i;
    *remlen = srclen -i;

    return(res);
}

// {{{1 uu

static char const uu_encmap[] = "`!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_";

void uu_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i + 3 <= srclen && *dstlen + 4 <= od; i += 3, *dstlen += 4) {
        int32_t o0, o1, o2, o3;
        o0 = src[i] >> 2;
        o1 = ((src[i] << 4) | (src[i+1] >> 4)) & 0x3f;
        o2 = ((src[i+1] << 2) | (src[i+2] >> 6)) & 0x3f;
        o3 = src[i+2] & 0x3f;
        *dst++ = uu_encmap[o0];
        *dst++ = uu_encmap[o1];
        *dst++ = uu_encmap[o2];
        *dst++ = uu_encmap[o3];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

int uu_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        int32_t o0, o1, o2;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 2;
        o1 = (src[0] << 4) & 0x3f;
        *dst++ = uu_encmap[o0];
        *dst++ = uu_encmap[o1];
        *dstlen = 2;
        return(0);
        break;
    case 2:
        o0 = src[0] >> 2;
        o1 = ((src[0] << 4) | (src[1] >> 4)) & 0x3f;
        o2 = (src[1] << 2) & 0x3f;
        *dst++ = uu_encmap[o0];
        *dst++ = uu_encmap[o1];
        *dst++ = uu_encmap[o2];
        *dstlen = 3;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

// decode map, 0x80 = not allowed, 0x40 = end char
static uint8_t const uu_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x40, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f, 
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 
    0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 
    0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 
    0x00, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80 };

int uu_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;
    int res = 0;

    for(i = 0, *dstlen = 0; i + 4 <= srclen && *dstlen + 3 <= od; i += 4, *dstlen += 3) {
        uint8_t o0, o1, o2, o3;

        o0 = uu_decmap[src[i]];
        o1 = uu_decmap[src[i+1]];
        o2 = uu_decmap[src[i+2]];
        o3 = uu_decmap[src[i+3]];
        if(!(0xc0 & (o0 | o1 | o2 | o3))) { // no illegal chars, and no ' '
            *dst++ = (o0 << 2) | (o1 >> 4);
            *dst++ = (o1 << 4) | (o2 >> 2);
            *dst++ = (o2 << 6) | o3;
        } else {
            res = 1;
            break;
        }
    }

    *rem = src + i;
    *remlen = srclen - i;
    return(res);
}

int uu_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    uint8_t o0, o1, o2;

    switch(srclen) {
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 2:
        o0 = uu_decmap[src[0]];
        o1 = uu_decmap[src[1]];
        if(0xc0 & (o0 | o1)) goto error;
        dst[0] = (o0 << 2) | (o1 >> 4);
        *dstlen = 1;
        return(0);
        break;
    case 3:
        o0 = uu_decmap[src[0]];
        o1 = uu_decmap[src[1]];
        o2 = uu_decmap[src[2]];
        if(0xc0 & (o0 | o1 | o2)) goto error;
        dst[0] = (o0 << 2) | (o1 >> 4);
        dst[1] = (o1 << 4) | (o2 >> 2);
        *dstlen = 2;
        return(0);
        break;
    }

error:
    *dstlen = 0;
    return(1);
}

// {{{1 xx

static char const xx_encmap[] = "+-0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

void xx_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i + 3 <= srclen && *dstlen + 4 <= od; i += 3, *dstlen += 4) {
        int32_t o0, o1, o2, o3;
        o0 = src[i] >> 2;
        o1 = ((src[i] << 4) | (src[i+1] >> 4)) & 0x3f;
        o2 = ((src[i+1] << 2) | (src[i+2] >> 6)) & 0x3f;
        o3 = src[i+2] & 0x3f;
        *dst++ = xx_encmap[o0];
        *dst++ = xx_encmap[o1];
        *dst++ = xx_encmap[o2];
        *dst++ = xx_encmap[o3];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

int xx_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        int32_t o0, o1, o2;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 2;
        o1 = (src[0] << 4) & 0x3f;
        *dst++ = xx_encmap[o0];
        *dst++ = xx_encmap[o1];
        *dstlen = 2;
        return(0);
        break;
    case 2:
        o0 = src[0] >> 2;
        o1 = ((src[0] << 4) | (src[1] >> 4)) & 0x3f;
        o2 = (src[1] << 2) & 0x3f;
        *dst++ = xx_encmap[o0];
        *dst++ = xx_encmap[o1];
        *dst++ = xx_encmap[o2];
        *dstlen = 3;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}

// decode map, 0x80 = not allowed, 0x40 = end char
static uint8_t const xx_decmap[] = {
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x40, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x00, 0x80, 0x01, 0x80, 0x80, 
    0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x0c, 0x0d, 0x0e, 0x0f, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17, 0x18, 0x19, 0x1a, 
    0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22, 0x23, 0x24, 0x25, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 
    0x35, 0x36, 0x37, 0x38, 0x39, 0x3a, 0x3b, 0x3c, 0x3d, 0x3e, 0x3f, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 
    0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80,
};

int xx_dec_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;
    int res = 0;

    for(i = 0, *dstlen = 0; i + 4 <= srclen && *dstlen + 3 <= od; i += 4, *dstlen += 3) {
        uint8_t o0, o1, o2, o3;

        o0 = xx_decmap[src[i]];
        o1 = xx_decmap[src[i+1]];
        o2 = xx_decmap[src[i+2]];
        o3 = xx_decmap[src[i+3]];
        if(!(0xc0 & (o0 | o1 | o2 | o3))) { // no illegal chars, and no ' '
            *dst++ = (o0 << 2) | (o1 >> 4);
            *dst++ = (o1 << 4) | (o2 >> 2);
            *dst++ = (o2 << 6) | o3;
        } else {
            res = 1;
            break;
        }
    }

    *rem = src + i;
    *remlen = srclen - i;
    return(res);
}

int xx_dec_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    uint8_t o0, o1, o2;

    switch(srclen) {
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 2:
        o0 = xx_decmap[src[0]];
        o1 = xx_decmap[src[1]];
        if(0xc0 & (o0 | o1)) goto error;
        dst[0] = (o0 << 2) | (o1 >> 4);
        *dstlen = 1;
        return(0);
        break;
    case 3:
        o0 = xx_decmap[src[0]];
        o1 = xx_decmap[src[1]];
        o2 = xx_decmap[src[2]];
        if(0xc0 & (o0 | o1 | o2)) goto error;
        dst[0] = (o0 << 2) | (o1 >> 4);
        dst[1] = (o1 << 4) | (o2 >> 2);
        *dstlen = 2;
        return(0);
        break;
    }

error:
    *dstlen = 0;
    return(1);
}

// {{{1 yenc

void y_enc(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i < srclen && *dstlen < od; i++, (*dstlen)++) {
        switch(src[i]) {
        case 19:
        case 214:
        case 224:
        case 227:
            if(*dstlen >= od - 1) goto exit; // is there room for 2 chars in dst?
            dst[(*dstlen)++] = 61;
            dst[*dstlen] = src[i] + 106;
            break;
        default:
            dst[*dstlen] = src[i] + 42;
            break;
        }
    }

exit:
    *rem = src + i;
    *remlen = srclen - i;
}

int y_dec(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    size_t od = *dstlen, i;

    for(i = 0, *dstlen = 0; i < srclen && *dstlen < od; i++, (*dstlen)++) {
        if(61 == src[i]) {
            if(srclen <= i + 1) goto exit;
            dst[*dstlen] = src[++i] - 106;
        } else
            dst[*dstlen] = src[i] - 42;
    }

exit:
    *rem = src + i;
    *remlen = srclen - i;

    return(0);
}
