#include "hlsl.h"
#include "matrices.h"
#include "brush_utils.h"

float2 ViewPortSize;

struct VS_Input {
    float2 vsCoord : vsCoord;
};

struct VS_Output {
    float4 Pos      : SV_Position;
    float2 wCoord   : wCoord;
    float2 TexCoord : TexCoord;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.wCoord = In.vsCoord * BrushRadius + BrushPos;
    
    Out.TexCoord = Out.wCoord / ViewPortSize;    
    Out.Pos.xy = Out.TexCoord * 2.0 - 1.0;
    Out.Pos.xy *= float2(1,-1)*FBOFlip;
    Out.Pos.zw = float2(0,1);
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float d = GetBrushValue(In.wCoord);
    
    Out.Color.xyz = 1.0;
    Out.Color.a = d*BrushForce*0.001;
    return Out;
}