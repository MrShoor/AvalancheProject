#include "hlsl.h"
#include "matrices.h"
#include "..\phong.h"

struct VS_Input {
    float2 vsCoord : vsCoord;
};

struct VS_Output {
    float4 Pos    : SV_Position;
    float2 TexCrd : TexCrd;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = float4(In.vsCoord, 0.5, 1.0);
    Out.TexCrd = In.vsCoord*float2(0.5,-0.5)*FBOFlip + 0.5;
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target;
};

Texture2D Accum; SamplerState AccumSampler;
Texture2D Product; SamplerState ProductSampler;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float4 accum = Accum.Sample(AccumSampler, In.TexCrd);
    Out.Color.rgb = accum.rgb / max(0.00001, accum.a);
    Out.Color.a = 1.0 - Product.Sample(ProductSampler, In.TexCrd).r;
    return Out;
}