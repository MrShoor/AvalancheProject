#include "hlsl.h"
#pragma pack_matrix( row_major )

//float Rad;
//float2 FiModifier;

float4x4 UIMatrix;
float PixelToUnit;

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
    float w = max(In.Width.x, In.Width.y*PixelToUnit)*0.5;
    Crd += Norm*w*(In.quadCoord.y+In.HintingAlign.z);
    
    Norm = normalize(mul(float4(Norm, 0.0, 0.0), UIMatrix).xy);
    Out.Pos = mul(float4(Crd, 0.0, 1.0), UIMatrix);
    Out.Pos.z = 0.5;
    
    //Out.Pos.xy /= Out.Pos.w;
    //float2 offset = Norm*In.quadCoord.y*In.Width.x;
    //Out.Pos.xy += offset;
    //Out.Pos.xy *= Out.Pos.w;
    Out.Color1 = 1.0;
    return Out;
}