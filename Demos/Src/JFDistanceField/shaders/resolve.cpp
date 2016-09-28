#include "hlsl.h"
#include "SSQuadOutput.h"

struct PS_Output {
    float4 Color : SV_Target0;
};

float Time;
Texture2D SrcTex; SamplerState SrcTexSampler;
Texture2D SrcDistanceField; SamplerState SrcDistanceFieldSampler;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float2 offset = SrcDistanceField.Sample(SrcDistanceFieldSampler, In.TexCoord).rg;
    Out.Color.a = 1.0;
    Out.Color.rgb = SrcTex.Sample(SrcTexSampler, In.TexCoord + offset).rgb;
    float2 TexSize;
    SrcTex.GetDimensions(TexSize.x, TexSize.y);
    offset *= TexSize;
    float offsetLen = length(offset);
    if (offsetLen > 0.0) {
        offsetLen *= 25.0;
        offsetLen = pow(offsetLen, 0.5);
        float t = Time + cos(Time*4.0)*0.5;
        Out.Color.rgb *= (cos(t*4.0-offsetLen)+1.0)*0.5;
        //Out.Color.rgb = frac(offsetLen/16);
    }
    return Out;
}