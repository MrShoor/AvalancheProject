#include "hlsl.h"
#include "SSQuadOutput.h"

struct PS_Output {
    float4 Color : SV_Target0;
};

float2 JumpStep;
float Aspect;
Texture2D SrcDistanceField; SamplerState SrcDistanceFieldSampler;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float2 minVec = 1.#INF;
    float minDistSqr = 1.#INF;
            
    for (int j = -1; j < 2; j++) {
        for (int i = -1; i < 2; i++) {
            float2 JumpOffset = JumpStep * float2(i, j);
            float2 DistVec = JumpOffset + SrcDistanceField.Sample(SrcDistanceFieldSampler, In.TexCoord + JumpOffset).rg;
            float DistLenSqr = dot(DistVec*float2(Aspect, 1.0), DistVec*float2(Aspect, 1.0));
            if (DistLenSqr<minDistSqr) {
                minVec = DistVec;
                minDistSqr = DistLenSqr;
            }
        }
    }
    
    Out.Color = float4(minVec, 0, 0);
    return Out;
}