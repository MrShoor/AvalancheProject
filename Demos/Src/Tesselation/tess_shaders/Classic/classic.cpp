#include "hlsl.h"
#include "matrices.h"
#include "..\phong.h"

struct VS_Input {
    float3 vsCoord   : vsCoord;
    float3 vsNormal  : vsNormal;
};

struct VS_Output {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
};

VS_Output VS(VS_Input In) {
    VS_Output Out;
    float4 crd = float4(In.vsCoord, 1.0);
    Out.Pos = mul(crd, VP_Matrix);
    Out.Normal = mul(In.vsNormal, (float3x3)V_Matrix);
    Out.ViewPos = mul(crd, V_Matrix).xyz;
    return Out;
}
//-------------------------------------------------
//Tesselation control shader
#define MAX_POINTS 32
struct TC_OutConstants {
    float Edges[3]        : SV_TessFactor;
    float Inside[1]       : SV_InsideTessFactor;
};

TC_OutConstants TC_ConstantFunc(InputPatch<VS_Output, MAX_POINTS> In) {
    TC_OutConstants Out;
    Out.Edges[0] = 5.0;
    Out.Edges[1] = 5.0;
    Out.Edges[2] = 5.0;
    Out.Inside[0] = 5.0;
    return Out;
}

typedef VS_Output TC_Output;

struct TC_PathParams {
    uint i : SV_OutputControlPointID;
    uint PatchID : SV_PrimitiveID;    
};

[domain("tri")]
[partitioning("integer")]
[outputtopology("triangle_cw")]
[outputcontrolpoints(3)]
[patchconstantfunc("TC_ConstantFunc")]
TC_Output TC(InputPatch<VS_Output, MAX_POINTS> ip, TC_PathParams params) {
    VS_Output Out;
    Out = ip[params.i];
    return Out;
}

//--------------------------------------------------------
//Tesselation evaluation shader
struct DS_Output {
    float4 Pos    : SV_Position;
    float3 Normal : Normal;
    float3 ViewPos: ViewPos;
};

[domain("tri")]
DS_Output DS(TC_OutConstants input, float3 uvwCoord : SV_DomainLocation, OutputPatch<TC_Output, MAX_POINTS> patch)
{
    DS_Output Out;

    float4 Pos     = uvwCoord.x * patch[0].Pos     + uvwCoord.y * patch[1].Pos     + uvwCoord.z * patch[2].Pos;
    float3 ViewPos = uvwCoord.x * patch[0].ViewPos + uvwCoord.y * patch[1].ViewPos + uvwCoord.z * patch[2].ViewPos;
    float3 Normal  = uvwCoord.x * patch[0].Normal  + uvwCoord.y * patch[1].Normal  + uvwCoord.z * patch[2].Normal;
    Out.Pos = Pos;
    Out.ViewPos = ViewPos;
    Out.Normal = Normal;

    return Out;
}

//--------------------------------------------------------
//Pixel shader

struct PS_Output {
    float4 Color : SV_Target;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float3 n = normalize(In.Normal);
    float4 Color = float4(1,1,1,1);
    Out.Color.xyz = Phong(0.0, 0.0, In.ViewPos, n, Color.xyz);
    Out.Color.a = Color.a;
    return Out;
}