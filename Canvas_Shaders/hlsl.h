#ifndef HLSL_H
#define	HLSL_H

#define in
#define out
#define inout

struct float2 {
    float x, y, r, g;
    float2 xx, xy, yx, yy;
    float3 xxy;
    float4 xxyy;
};

struct float3 {
    float x, y, z, r, g, b;
    float2 xx, xy, xz, yx, yy, yz, zz;
    float3 xxy, yyz;
};

struct float4 {
    float x, y, z, w, r, g, b, a;
    float2 xx, xy, xz, xw, yx, yy, yz, yw, zw, zz;
    float3 xyz, rgb, yyz;
};

#endif	/* HLSL_H */

