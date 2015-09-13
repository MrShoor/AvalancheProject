#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float3 vsCoord : vsCoord;
    float3 vsNormal: vsNormal;
    float2 vsTexCrd: vsTexCrd;
};

struct VS_Output {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
    float2 TexCrd : TexCrd;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = mul(VP_Matrix, float4(In.vsCoord, 1.0));
    Out.Normal = mul(V_Matrix, float4(In.vsNormal, 0.0)).xyz;
    Out.ViewPos = mul(V_Matrix, float4(In.vsCoord, 1.0)).xyz;
    Out.TexCrd = In.vsTexCrd;
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target;
};

Texture2D Diffuse; SamplerState DiffuseSampler;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);
    float4 diff = Diffuse.Sample(DiffuseSampler, In.TexCrd);
    Out.Color = max(0.0, -dot(normalize(In.ViewPos), n)) * diff;
    return Out;
}