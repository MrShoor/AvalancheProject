#include "hlsl.h"
#include "matrices.h"

float4 FrameTexRect;

struct VS_Input {
    float2 vsCoord : vsCoord;
};

struct VS_Output {
    float4 Pos    : SV_Position;
    float2 TexCrd : TexCrd;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = float4(In.vsCoord, 0.5, 1.0);
    Out.TexCrd = lerp(FrameTexRect.xy, FrameTexRect.zw, In.vsCoord*float2(0.5,-0.5)*FBOFlip + 0.5);
    return Out;
}