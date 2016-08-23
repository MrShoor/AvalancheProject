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

#define SamplerState int

template<gentype> struct Texture2D{
    float4 Sample(SamplerState, gentype texcoord);
};

void discard;
float sin(float rad);
float cos(float rad);
float sqrt(float v);
template<gentype> float sign(gentype v);
template<gentype> float length(gentype v);
template<gentype> float dot(gentype v);
template<gentype> gentype sign(gentype v);
template<gentype> gentype cross(gentype v);
template<gentype> gentype normalize(gentype v);
template<gentype> gentype min(gentype v);
template<gentype> gentype max(gentype v);
template<gentype> gentype abs(gentype v);
template<gentype> gentype mul(gentype val1, gentype val2);
template<gentype> gentype lerp(gentype min, gentype max, float k);
template<gentype> gentype clamp(gentype v, gentype minval, gentype maxval);

#endif	/* HLSL_H */

