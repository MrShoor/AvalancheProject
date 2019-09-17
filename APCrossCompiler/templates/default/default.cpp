#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    float3 S_(vsCoord);
};

struct VS_Output {
    float4 S_Position(Pos);
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = mul(float4(In.vsCoord, 1.0), VP_Matrix);
    return Out;
}

///////////////////////////////////////////////////////

struct PS_Output {
    float4 S_Target0(Color);
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    Out.Color = 1.0;
    return Out;
}