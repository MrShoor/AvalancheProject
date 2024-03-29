#include "hlsl.h"
#include "UICommon.h"
#include "hinting.h"

struct VS_Input {
    float2 S_(quadCoord);
    float4 S_(Coords);
    float4 S_(Normals);
    float2 S_(Width);
    float4 S_(HintingAlign);
    float4 S_(Color);
};

struct VS_Output {
    float4 S_Position(Pos);
    float4 S_(Color);
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
    Crd += Norm*w*(In.quadCoord.y+0.001*In.HintingAlign.z+In.HintingAlign.w);
    
    Crd.xy = mul(float3(Crd.xy,1), CanvasTransform()).xy;
    
    Out.Pos = mul(float4(Crd, 0.0, 1.0), UIMatrix);
    Out.Pos.z = ZValue * Out.Pos.w;

    if (PostHinting) {
        Out.Pos = PostHintVertex(Out.Pos, In.HintingAlign.xy);
    }
    
    Out.Color = In.Color;
//    Out.Pos = In.Coords;
    return Out;
}

struct PS_Output {
    float4 S_Target0(Color);
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    Out.Color = In.Color;
    //float l = clamp(abs(length(In.quadCoord) - 0.6), 0.0, 1.0);
    //Out.Color.a = pow(abs(1.1-l), 10.0);
    return Out;
}