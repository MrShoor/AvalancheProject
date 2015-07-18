#include "hlsl.h"

struct PS_Input {
    float4 Pos   : POSITION;
    float3 Normal: Normal;
};

struct PS_Output {
    float4 Color : COLOR;
};

PS_Output PS(PS_Input In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);    
    Out.Color = max(0.3, -n.z);
    return Out;
}