#include "hlsl.h"
#include "UICommon.h"

//float Rad;
//float2 FiModifier;

struct VS_Input {
    float2 quadCoord : quadCoord;
    float4 Coords : Coords;
    float4 Normals : Normals;
    float2 Width: Width;
    float4 HintingAlign: HintingAlign;
};

struct VS_Output {
    float4 Pos   : POSITION;
    float4 Color1 : Color1;
};

float3 HintVertex(in float3 crd, in float2 HintInfo) {
    float4 hintcrd = mul(float4(crd, 1.0), UIMatrix);
    hintcrd.xyz /= hintcrd.w;
    hintcrd.xy += float2(1.0,1.0);
    hintcrd.xy *= 0.5*ViewPortSize;
    if (HintInfo.x)
        hintcrd.x = round(hintcrd.x);
    if (HintInfo.y)
        hintcrd.y = round(hintcrd.y);
    hintcrd.xy /= 0.5*ViewPortSize;
    hintcrd.xy -= float2(1.0, 1.0);
    hintcrd.xyz *= hintcrd.w;
    hintcrd = mul(hintcrd, UIMatrixInverse);
    return hintcrd.xyz;
}

float2 HintVertex(in float2 crd, in float2 HintInfo) {
    float3 crd3 = float3(crd, 0.0);
    crd3 = HintVertex(crd3, HintInfo);
    return crd3.xy;
}

float4 PostHintVertex(in float4 crd, in float2 HintInfo) {
    float2 hintcrd;
    hintcrd = crd.xy;
    hintcrd /= crd.w;
    hintcrd += float2(1.0,1.0);
    hintcrd *= 0.5*ViewPortSize;
    if (HintInfo.x)
        hintcrd.x = round(hintcrd.x);
    if (HintInfo.y)
        hintcrd.y = round(hintcrd.y);
    hintcrd /= 0.5*ViewPortSize;
    hintcrd -= float2(1.0, 1.0);
    hintcrd *= crd.w;
    return float4(hintcrd, crd.z, crd.w);
}

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
    
    Norm = normalize(mul(float4(Norm, 0.0, 0.0), UIMatrix).xy);
    Out.Pos = mul(float4(Crd, 0.0, 1.0), UIMatrix);
    Out.Pos.z = 0.5;

    if (PostHinting) {
        Out.Pos = PostHintVertex(Out.Pos, In.HintingAlign.xy);
    }
    
    Out.Color1 = 1.0;
//    Out.Pos = In.Coords;
    return Out;
}