#include "hlsl.h"
#include "matrices.h"

float2 PixelSize;

float Rad;
float2 FiModifier;

float4x4 WndMatrix;

struct VS_Input {
    float2 quadCoord : quadCoord;
    float4 Coords : Coords;
    float4 Normals : Normals;
    float2 Width: Width;
    float3 HintingAlign: HintingAlign;
};

struct VS_Output {
    float4 Pos   : POSITION;
    float4 Color1 : Color1;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    float2 Crd = lerp(In.Coords.xy, In.Coords.zw, In.quadCoord.x);
    float2 Norm = lerp(In.Normals.xy, In.Normals.zw, In.quadCoord.x);
    Crd += Norm*In.quadCoord.y*In.Width.x;
    
    Norm = normalize(mul(WndMatrix, float4(Norm, 0.0, 0.0)).xy);
    Out.Pos = mul(WndMatrix, float4(Crd, 0.0, 1.0));
    
    //Out.Pos.xy /= Out.Pos.w;
    //float2 offset = Norm*In.quadCoord.y*In.Width.x;
    //Out.Pos.xy += offset;
    //Out.Pos.xy *= Out.Pos.w;
    Out.Color1 = 1.0;
    return Out;
}