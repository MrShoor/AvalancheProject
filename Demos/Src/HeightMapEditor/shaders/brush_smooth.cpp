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
    float2 TexCoord_Center : TexCoord_Center;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.wCoord = In.vsCoord * BrushRadius + BrushPos;
    Out.TexCoord_Center = BrushPos / ViewPortSize;
    Out.TexCoord = Out.wCoord / ViewPortSize;    
    Out.Pos.xy = Out.TexCoord * 2.0 - 1.0;
    Out.Pos.xy *= float2(1,-1)*FBOFlip;
    Out.Pos.zw = float2(0,1);
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target0;
};

Texture2D HeightMap; SamplerState HeightMapSampler;

float GetTargetValue(VS_Output In) {
    float summ = 0;
    float summK = 0;
    [unroll]
    for (int y = -7; y < 8; y++)
        [unroll]
        for (int x = -7; x < 8; x++) {
            summ += HeightMap.Sample(HeightMapSampler, In.TexCoord, int2(x, y)).r;
            summK += 1.0;
        }
    return summ / summK;
}

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float d = GetBrushValue(In.wCoord);
    
    float TargetLevel = GetTargetValue(In);
    
    d = d*BrushForce*0.05;
    Out.Color.x = lerp(HeightMap.SampleLevel(HeightMapSampler, In.TexCoord, 0).r, TargetLevel, d);

    Out.Color.yzw = 1.0;
    return Out;
}