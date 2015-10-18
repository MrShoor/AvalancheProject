#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float3 vsCoord   : vsCoord;
    float3 vsNormal  : vsNormal;
    float3 aiPosition: aiPosition;
    float4 aiColor   : aiColor;
};

struct VS_Output {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
    float4 Color  : Color;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    float4 crd = float4(In.vsCoord + In.aiPosition, 1.0);
    Out.Pos = mul(crd, VP_Matrix);
    Out.Normal = mul(In.vsNormal, (float3x3)V_Matrix);
    Out.ViewPos = mul(crd, V_Matrix).xyz;
    Out.Color = In.aiColor;
    return Out;
}

struct PS_Output {
    float4 Color : COLOR;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);
    Out.Color = max(0.0, -dot(normalize(In.ViewPos), n)) * In.Color;
    return Out;
}