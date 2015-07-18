#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float3 vsCoord : vsCoord;
    float3 vsNormal: vsNormal;
};

struct VS_Output {
    float4 Pos   : POSITION;
    float3 Normal: Normal;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = mul(VP_Matrix, float4(In.vsCoord, 1.0));    
    Out.Normal = mul(V_Matrix, float4(In.vsNormal, 0.0)).xyz;
    return Out;
}