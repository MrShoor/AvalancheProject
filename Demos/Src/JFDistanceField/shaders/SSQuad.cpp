#include "hlsl.h"
#include "SSQuadOutput.h"

struct VS_Input {
    float2 vsCoord : vsCoord;
};

VS_Output VS (VS_Input In) {
    VS_Output Out;
    Out.Pos = float4(In.vsCoord, 0.0, 1.0);
    Out.TexCoord = (In.vsCoord*float2(1.0, -1.0) + 1.0) * 0.5;
    return Out;
}
