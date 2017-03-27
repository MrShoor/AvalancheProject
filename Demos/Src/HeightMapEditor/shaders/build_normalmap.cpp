#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float2 vsCoord  : vsCoord;
};

struct VS_Output {
    float4 Pos      : SV_Position;
    float2 TexCoord : TexCoord;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = float4(In.vsCoord*2.0 - 1.0, 0.0, 1.0);
    Out.Pos.xy *= FBOFlip*float2(1.0,-1.0);
    
    Out.TexCoord = In.vsCoord;
    return Out;
}

struct PS_Output {
    float4 Color: SV_Target0;
};

Texture2D HeightMap; SamplerState HeightMapSampler;
float HeightScale;

static const int2 Offsets[8] = {
    { 0,  1},
    { 1,  1},
    { 1,  0},
    { 1, -1},
    { 0, -1},
    {-1, -1},
    {-1,  0},
    {-1,  1}    
};

float GetHeight(float2 TexCoord) {
    return HeightMap.Sample(HeightMapSampler, TexCoord).r * HeightScale;
}

float GetHeight(float2 TexCoord, int2 Offset) {
    return HeightMap.Sample(HeightMapSampler, TexCoord, Offset).r * HeightScale;
}

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float Z0 = GetHeight(In.TexCoord);
    float3 pts[9];
    
    uint i;
    [unroll(8)]
    for (i = 0; i < 8; i++) {
        pts[i].xy = Offsets[i];
        pts[i].z = GetHeight(In.TexCoord, Offsets[i]);
    }
    pts[8] = pts[0];
    
    float3 n = 0.0;
    for (i = 0; i < 8; i++)
        n += cross(pts[i+1], pts[i]);
    n = normalize(n);
    
    Out.Color.rg = n.xy*0.5 + 0.5;
    Out.Color.ba = 0.0;
    return Out;
}