#include "hlsl.h"
#include "SSQuadOutput.h"

struct PS_Output {
    float4 Color : SV_Target0;
};

Texture2D SrcTex; SamplerState SrcTexSampler;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float4 c = SrcTex.Sample(SrcTexSampler, In.TexCoord);
    if (c.a < 1.0){
        Out.Color = 1.#INF;
    } else {
        Out.Color = 0.0;
    }
    return Out;
}