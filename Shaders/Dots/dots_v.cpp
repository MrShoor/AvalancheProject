#include "hlsl.h"
#include "matrices.h"

float2 PixelSize;

float Rad;
float2 FiModifier;

struct VS_Input {
    float2 quadCoord : quadCoord;
    float3 vsCoord : vsCoord;
    float4 vsColor1: vsColor1;
};

struct VS_Output {
    float4 Pos   : POSITION;
    float4 Color1 : Color1;
    float2 quadCoord: quadCoord;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.Pos = mul(VP_Matrix, float4(In.vsCoord.xy, 0.0, 1.0));
    Out.Pos.xy /= Out.Pos.w;
    float fispeed = FiModifier.x * In.vsCoord.z;
    float fi = In.vsCoord.z + FiModifier.y * fispeed;
    //float fi = FiModifier.y;
    float2 offset = Rad * float2(cos(fi), sin(fi));
    Out.Pos.xy += PixelSize * (In.quadCoord + offset);
    Out.Pos.xy *= Out.Pos.w;
    Out.Color1 = In.vsColor1;
    Out.quadCoord = In.quadCoord;
    return Out;
}