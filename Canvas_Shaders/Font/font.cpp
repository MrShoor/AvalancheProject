#include "hlsl.h"
#include "UICommon.h"

float4x4 Transform;

struct VS_Input {
    float2 Pos       : Pos;
    float  Align     : Align;
    float2 Size      : Size;
    float  SDFOffset : SDFOffset;
    float4 Color     : Color;
    uint   GlyphID   : GlyphID;
    
    uint   VertexID  : SV_VertexID;
};

struct VS_Output {
    float4 Pos      : SV_Position;
    float2 TexCoord : TexCoord;
};

static const float2 QuadVertices[4] = { {-0.5,-0.5}, {-0.5, 0.5}, {0.5, -0.5}, {0.5, 0.5} };

VS_Output VS(VS_Input In) {
    VS_Output Out;
    
    float4 crd = float4(QuadVertices[In.VertexID], 0.0, 1.0);
    crd.xy *= In.Size;
    //todo apply align
    crd.xy += In.Pos;
    
    crd = mul(crd, Transform);
    Out.Pos = mul(crd, UIMatrix);
    Out.TexCoord = 1.0;
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    Out.Color = 1.0;
    
    return Out;
}