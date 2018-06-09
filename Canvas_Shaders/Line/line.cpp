#include "hlsl.h"
#include "UICommon.h"
#include "hinting.h"

struct VS_Input {
    float2 quadCoord : quadCoord;
    float4 Coords : Coords;
    float4 Normals : Normals;
    float2 Width: Width;
    float4 HintingAlign: HintingAlign;
    float4 Color : Color;    
};

struct VS_Output {
    float4 Pos   : SV_Position;
    float4 Color : Color;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    
    bool PostHinting = In.HintingAlign.z;
    
    if (!PostHinting) {
        In.Coords.xy = HintVertex(In.Coords.xy, In.HintingAlign.xy);
        In.Coords.zw = HintVertex(In.Coords.zw, In.HintingAlign.xy);
    };
    
    float2 Crd = lerp(In.Coords.xy, In.Coords.zw, In.quadCoord.x); 
    float2 Norm = lerp(In.Normals.xy, In.Normals.zw, In.quadCoord.x);
    float w = max(In.Width.x, In.Width.y*PixelToUnit)*0.5;
    Crd += Norm*w*(In.quadCoord.y+In.HintingAlign.w);
    
    Crd.xy = mul(float3(Crd.xy,1), CanvasTransform()).xy;
    
    Out.Pos = mul(float4(Crd, 0.0, 1.0), UIMatrix);
    Out.Pos.z = 0.5;

    if (PostHinting) {
        Out.Pos = PostHintVertex(Out.Pos, In.HintingAlign.xy);
    }
    
    Out.Color = In.Color;
//    Out.Pos = In.Coords;
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    Out.Color = In.Color;
    //float l = clamp(abs(length(In.quadCoord) - 0.6), 0.0, 1.0);
    //Out.Color.a = pow(abs(1.1-l), 10.0);
    return Out;
}