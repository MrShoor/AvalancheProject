#include "hlsl.h"

struct PS_Input {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
    float2 TexCrd : TexCrd;
};

struct PS_Output {
    float4 Color : SV_Target;
};

Texture2D Diffuse; SamplerState DiffuseSampler;

PS_Output PS(PS_Input In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);
    float4 diff = Diffuse.Sample(DiffuseSampler, In.TexCrd);
    Out.Color = max(0.0, -dot(normalize(In.ViewPos), n)) * diff;
    return Out;
}