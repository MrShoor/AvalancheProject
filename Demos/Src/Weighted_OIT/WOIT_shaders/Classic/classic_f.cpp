#include "hlsl.h"
#include "..\phong.h"

struct PS_Input {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
    float4 Color  : Color;
};

struct PS_Output {
    float4 Color : SV_Target;
};

PS_Output PS(PS_Input In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);
    Out.Color.xyz = Phong(0.0, 0.0, In.ViewPos, n, In.Color.xyz);
    Out.Color.a = In.Color.a;
    return Out;
}