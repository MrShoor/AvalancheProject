#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float3 vsCoord : vsCoord;
    float4 vsColor1: vsColor1;
};

struct VS_Output {
    float4 Pos   : POSITION;
    float4 Color1 : Color1;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = mul(VP_Matrix, float4(In.vsCoord, 1.0));
    Out.Color1 = In.vsColor1;
    return Out;
}