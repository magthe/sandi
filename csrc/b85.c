// Copyright: (c) Magnus Therning, 2012
// License: BSD3, found in the LICENSE file

#include <assert.h>
#include <string.h>

#include "b85.h"

uint8_t zeroes[] = { 0, 0, 0, 0 };
uint8_t spaces[] = { 0x20, 0x20, 0x20, 0x20 };

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
        if(memcmp(src + i, zeroes, 4) == 0) {
            dst[*dstlen] = 'z';
            *dstlen += 1;
        } else if(memcmp(src + i, spaces, 4) == 0) {
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

static uint8_t const decmap[] = {
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
            o0 = decmap[src[i]];
            o1 = decmap[src[i + 1]];
            o2 = decmap[src[i + 2]];
            o3 = decmap[src[i + 3]];
            o4 = decmap[src[i + 4]];
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
        o0 = decmap[src[0]];
        o1 = decmap[src[1]];
        o2 = decmap['u'];
        o3 = decmap['u'];
        o4 = decmap['u'];
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
        o0 = decmap[src[0]];
        o1 = decmap[src[1]];
        o2 = decmap[src[2]];
        o3 = decmap['u'];
        o4 = decmap['u'];
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
        o0 = decmap[src[0]];
        o1 = decmap[src[1]];
        o2 = decmap[src[2]];
        o3 = decmap[src[3]];
        o4 = decmap['u'];
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
