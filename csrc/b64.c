#include <stdio.h>
#include <assert.h>

#include "b64.h"

static char const alphabet[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

void b64_enc_part(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t i;

    assert(src);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

    for(i = 0, *dstlen = 0; i + 3 <= srclen; i += 3, *dstlen += 4) {
        int32_t o0, o1, o2, o3;
        o0 = src[i] >> 2;
        o1 = ((src[i] << 4) | (src[i+1] >> 4)) & 0x3f;
        o2 = ((src[i+1] << 2) | (src[i+2] >> 6)) & 0x3f;
        o3 = src[i+2] & 0x3f;
        *dst++ = alphabet[o0];
        *dst++ = alphabet[o1];
        *dst++ = alphabet[o2];
        *dst++ = alphabet[o3];
    }

    *rem = src + i;
    *remlen = srclen - i;
}

int b64_enc_final(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen)
{
    assert(src);
    assert(dst);
    assert(dstlen);

    switch(srclen) {
        int32_t o0, o1, o2, o3;
    case 0:
        *dstlen = 0;
        return(0);
        break;
    case 1:
        o0 = src[0] >> 2;
        o1 = (src[0] << 4) & 0x3f;
        *dst++ = alphabet[o0];
        *dst++ = alphabet[o1];
        *dst++ = '=';
        *dst++ = '=';
        *dstlen = 4;
        return(0);
        break;
    case 2:
        o0 = src[0] >> 2;
        o1 = ((src[0] << 4) | (src[1] >> 4)) & 0x3f;
        o2 = (src[1] << 2) & 0x3f;
        *dst++ = alphabet[o0];
        *dst++ = alphabet[o1];
        *dst++ = alphabet[o2];
        *dst++ = '=';
        *dstlen = 4;
        return(0);
        break;
    default:
        return(1);
        break;
    }
}
