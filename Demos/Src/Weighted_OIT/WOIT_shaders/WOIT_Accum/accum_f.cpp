#include "hlsl.h"
#include "..\phong.h"

float2 NearFarPlane;

struct PS_Input {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
    float4 Color  : Color;
};

struct PS_Output {
    float4 Color0 : SV_Target0;
    float4 Color1 : SV_Target1;
};

float WeightFunc(float z, float a)
{
    return a*max(0.01, 9000.0 * pow(abs(1.0 - z), 30.0));
    //return a*max(0.01, 3000.0 * abs(1.0 - z));
    //return clamp(pow(min(1.0, a * 10.0) + 0.01, 3.0) * 1e8 * pow(1.0 - z * 0.9, 3.0), 1e-2, 3e3);
}

PS_Output PS(PS_Input In) {
    PS_Output Out;
    
    float z = (In.ViewPos.z - NearFarPlane.x)/(NearFarPlane.y - NearFarPlane.x);
    
    float3 n = normalize(In.Normal);
    Out.Color0.a = WeightFunc(z, In.Color.a) * In.Color.a;
    Out.Color0.xyz = Phong(0.0, 0.0, In.ViewPos, n, In.Color.xyz) * Out.Color0.a;
    Out.Color1 = 1.0 - In.Color.a;
    return Out;
}