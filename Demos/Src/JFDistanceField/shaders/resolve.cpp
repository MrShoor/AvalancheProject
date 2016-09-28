#include "hlsl.h"
#include "SSQuadOutput.h"

struct PS_Output {
    float4 Color : SV_Target0;
};

Texture2D SrcTex; SamplerState SrcTexSampler;
Texture2D SrcDistanceField; SamplerState SrcDistanceFieldSampler;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float2 offset = SrcDistanceField.Sample(SrcDistanceFieldSampler, In.TexCoord).rg;
    Out.Color.a = 1.0;
    Out.Color.rgb = SrcTex.Sample(SrcTexSampler, In.TexCoord + offset).rgb;
    if (dot(offset, offset) > 0.0) 
        Out.Color.rgb *= 0.5;
    return Out;
}