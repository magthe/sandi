// Copyright: (c) Magnus Therning, 2012
// License: BSD3, found in the LICENSE file

#include <assert.h>

#include "b32hex.h"

static char const encmap[] = "0123456789ABCDEFGHIJKLMNOPQRSTUV";

void b32h_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t i;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i + 5 <= srclen; i += 5, *dstlen += 8) {
        int32_t o0, o1, o2, o3, o4, o5, o6, o7;
        o0 = src[i] >> 3;
        o1 = ((src[i] << 2) | (src[i+1] >> 6)) & 0x1f;
        o2 = (src[i+1] >> 1) & 0x1f;
        o3 = ((src[i+1] << 4) | (src[i+2] >> 4)) & 0x1f;
        o4 = ((src[i+2] << 1) | (src[i+3] >> 7)) & 0x1f;
        o5 = (src[i+3] >>2) & 0x1f;
        o6 = ((src[i+3] << 3) | (src[i+4] >> 5)) & 0x1f;
        o7 = src[i+4] & 0x1f;
        *dst++ = encmap[o0];
        *dst++ = encmap[o1];
        *dst++ = encmap[o2];
        *dst++ = encmap[o3];
        *dst++ = encmap[o4];
        *dst++ = encmap[o5];
        *dst++ = encmap[o6];
        *dst++ = encmap[o7];
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
        int32_t o0, o1, o2, o3, o4, o5, o6, o7;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 3;
        o1 = (src[0] << 2) & 0x1f;
        *dst++ = encmap[o0];
        *dst++ = encmap[o1];
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
        *dst++ = encmap[o0];
        *dst++ = encmap[o1];
        *dst++ = encmap[o2];
        *dst++ = encmap[o3];
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
        *dst++ = encmap[o0];
        *dst++ = encmap[o1];
        *dst++ = encmap[o2];
        *dst++ = encmap[o3];
        *dst++ = encmap[o4];
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
        *dst++ = encmap[o0];
        *dst++ = encmap[o1];
        *dst++ = encmap[o2];
        *dst++ = encmap[o3];
        *dst++ = encmap[o4];
        *dst++ = encmap[o5];
        *dst++ = encmap[o6];
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
static uint8_t const decmap[] = {
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
    size_t i;
    int res = 0;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i + 8 <= srclen; i += 8, *dstlen += 5) {
        uint8_t o0, o1, o2, o3, o4, o5, o6, o7, all;

        o0 = decmap[src[i]];
        o1 = decmap[src[i+1]];
        o2 = decmap[src[i+2]];
        o3 = decmap[src[i+3]];
        o4 = decmap[src[i+4]];
        o5 = decmap[src[i+5]];
        o6 = decmap[src[i+6]];
        o7 = decmap[src[i+7]];
        if(!(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6 | o7))) { // no illegal chars, and no '='
            *dst++ = (o0 << 3) | (o1 >> 2);
            *dst++ = (o1 << 6) | (o2 << 1) | (o3 >> 4);
            *dst++ = (o3 << 4) | (o4 >> 1);
            *dst++ = (o4 << 7) | (o5 << 2) | (o6 >> 3);
            *dst++ = (o6 << 5) | o7;
        } else if(!(0xc0 & (o0 | o1)) && (0x40 & o2 & o3 & o4 & o5 & o6 & o7) // two legal chars, six '='
            || !(0xc0 & (o0 | o1 | o2 | o3)) && (0x40 & o4 & o5 & o6 & o7) // four legal chars, four '='
            || !(0xc0 & (o0 | o1 | o2 | o3 | o4)) && (0x40 & o5 & o6 & o7) // five legal chars, three '='
            || !(0xc0 & (o0 | o1 | o2 | o3 | o4 | o5 | o6)) && (0x40 & o7)) { // seven legal chars, one '='
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
    uint8_t o0, o1, o2, o3, o4, o5, o6, o7, all;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);

    if(0 == srclen) {
        *dstlen = 0;
        return(0);
    }
    o0 = decmap[src[0]];
    o1 = decmap[src[1]];
    o2 = decmap[src[2]];
    o3 = decmap[src[3]];
    o4 = decmap[src[4]];
    o5 = decmap[src[5]];
    o6 = decmap[src[6]];
    o7 = decmap[src[7]];
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
