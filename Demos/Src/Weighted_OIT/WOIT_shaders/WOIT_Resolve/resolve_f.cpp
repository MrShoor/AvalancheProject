#include "hlsl.h"

struct PS_Input {
    float4 Pos : SV_Position;
    float2 TexCrd : TexCrd;
};

struct PS_Output {
    float4 Color : COLOR;
};

Texture2D Accum; SamplerState AccumSampler;
Texture2D Product; SamplerState ProductSampler;

PS_Output PS(PS_Input In) {
    PS_Output Out;
    float4 accum = Accum.Sample(AccumSampler, In.TexCrd);
    Out.Color.rgb = accum.rgb / max(0.00001, accum.a);
    Out.Color.a = 1.0 - Product.Sample(ProductSampler, In.TexCrd).r;
    //Out.Color =  * Product.Sample(ProductSampler, In.TexCrd).r;// * 0.01;
    return Out;
}