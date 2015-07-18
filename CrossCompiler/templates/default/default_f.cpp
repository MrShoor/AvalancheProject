#include "hlsl.h"

struct PS_Input {
    float4 Pos   : POSITION;
};

struct PS_Output {
    float4 Color : COLOR;
};

PS_Output PS(PS_Input In) {
    PS_Output Out;
    Out.Color = 1.0;
    return Out;
}