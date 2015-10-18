#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float3 vsCoord : vsCoord;
};

struct VS_Output {
    float4 Pos   : POSITION;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = mul(float4(In.vsCoord, 1.0), MVP_Matrix);
    return Out;
}