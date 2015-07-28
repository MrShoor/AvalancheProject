#include "hlsl.h"

struct PS_Input {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
    float4 Color  : Color;
};

struct PS_Output {
    float4 Color : COLOR;
};

PS_Output PS(PS_Input In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);
    Out.Color = max(0.0, -dot(normalize(In.ViewPos), n)) * In.Color;
    return Out;
}