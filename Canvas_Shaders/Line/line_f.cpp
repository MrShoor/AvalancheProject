#include "hlsl.h"

struct PS_Input {
    float4 Pos   : POSITION;
    float4 Color1 : Color1;
//    float2 quadCoord: quadCoord;
};

struct PS_Output {
    float4 Color : COLOR;
};

PS_Output PS(PS_Input In) {
    PS_Output Out;
    Out.Color = In.Color1;    
    //float l = clamp(abs(length(In.quadCoord) - 0.6), 0.0, 1.0);
    //Out.Color.a = pow(abs(1.1-l), 10.0);
    return Out;
}