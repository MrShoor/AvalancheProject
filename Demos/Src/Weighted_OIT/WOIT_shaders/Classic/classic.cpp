#include "hlsl.h"
#include "matrices.h"
#include "..\phong.h"

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
    Out.Pos = mul(VP_Matrix, crd);
    Out.Normal = mul((float3x3)V_Matrix, In.vsNormal);
    Out.ViewPos = mul(V_Matrix, crd).xyz;
    Out.Color = In.aiColor;
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);
    Out.Color.xyz = Phong(0.0, 0.0, In.ViewPos, n, In.Color.xyz);
    Out.Color.a = In.Color.a;
    return Out;
}