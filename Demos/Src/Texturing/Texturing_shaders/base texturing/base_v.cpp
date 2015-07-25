#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float3 vsCoord : vsCoord;
    float3 vsNormal: vsNormal;
    float2 vsTexCrd: vsTexCrd;
};

struct VS_Output {
    float4 Pos   : SV_Position;
    float3 Normal: Normal;
    float2 TexCrd: TexCrd;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = mul(VP_Matrix, float4(In.vsCoord, 1.0));    
    Out.Normal = mul(V_Matrix, float4(In.vsNormal, 0.0)).xyz;
    Out.TexCrd = In.vsTexCrd;
    return Out;
}