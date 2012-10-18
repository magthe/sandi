#include <assert.h>

#include "yenc.h"

void y_enc(uint8_t const *src, size_t srclen,
    uint8_t *dst, size_t *dstlen,
    uint8_t const **rem, size_t *remlen)
{
    size_t od = *dstlen, i;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

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
    size_t od = *dstlen, i;

    assert(src || 0 == srclen);
    assert(dst);
    assert(dstlen);
    assert(rem);
    assert(remlen);

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
