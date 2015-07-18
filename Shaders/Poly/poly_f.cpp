#include "hlsl.h"

struct PS_Input {
    float4 Pos   : POSITION;
    float4 Color1 : Color1;
};

struct PS_Output {
    float4 Color : COLOR;
};

PS_Output PS(PS_Input In) {
    PS_Output Out;
    Out.Color = In.Color1;
    return Out;
}