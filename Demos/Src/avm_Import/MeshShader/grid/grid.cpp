#include "hlsl.h"
#include "matrices.h"

struct VS_Input {
    uint VID: SV_VertexID;
};

float4 SSAQuad[4] = {
    {-1, -1, 0, 1},
    {-1,  1, 0, 1},
    { 1, -1, 0, 1},
    { 1,  1, 0, 1}
};

struct VS_Output {
    float4 Pos : SV_Position;
    float3 WorldCrd : WorldCrd;
};

VS_Output VS(VS_Input In) {
   VS_Output Out;
   Out.Pos = SSAQuad[In.VID];
   float4 tmp = mul(Out.Pos, VP_InverseMatrix);
   Out.WorldCrd = tmp.xyz / tmp.w;
   return Out;
}

//****************PS***************************

void GridIntersection(float3 RayOrigin, float3 RayDir, out float3 WorldCrd) {
  if (RayDir.y == 0.0) discard;
  float k = -RayOrigin.y / RayDir.y;
  if (k < 0.0) discard;
  WorldCrd = RayOrigin + RayDir*k;
  return;
}

static const float GridDensityPow = 1.0;
static const float GridStep = 0.1;

#define USE_AA
float GridTex(float2 uv, float2 GridStep) {
    uv = uv / GridStep;
    float2 dx = abs(ddx(uv));
    float2 dy = abs(ddy(uv));
    uv = frac(uv);
    float2 size = 1.0 / float2(max(dx.x, dy.x), max(dx.y, dy.y));
    float2 PixelCoord = uv * size;
    PixelCoord = min(size - PixelCoord, PixelCoord);
    float k = max(0.0, 1.0 - min(PixelCoord.x, PixelCoord.y));
    #ifdef USE_AA
    k = pow(k, 1.0/2.2); //sRGB
    #else
    if (k < 0.5) k = 0.0; else k = 1.0;
    #endif

    float2 mip = log2(size);
    float t = clamp((min(mip.x, mip.y) * 0.25), 0.0, 1.0);
    t = pow(t, GridDensityPow);
    return k * t;
}

struct PS_Output {
    float4 Color : SV_Target0;
    float  Depth : SV_Depth;
};

static const float3 GridColors[4] = {
    {0.5, 0.5, 1},
    {0.5, 1, 0.5},
    {1, 0.5, 0.5},
    {1, 1, 1}
};

PS_Output PS(VS_Output In)
{
    PS_Output Out;
    float3 WorldCrd;
    float4 vViewPosition = mul(float4(0,0,0,1), V_InverseMatrix);
    vViewPosition.xyz /= vViewPosition.w;
   
    GridIntersection(vViewPosition.xyz, In.WorldCrd-vViewPosition.xyz, WorldCrd);
    float4 proj = mul(float4(WorldCrd, 1.0), VP_Matrix);
    Out.Depth = proj.z / proj.w;
    Out.Depth = saturate(Out.Depth);

    Out.Color.a = 0.5;
    Out.Color.xyz = 0.0;
    Out.Color.xyz = lerp(Out.Color.xyz, GridColors[0], GridTex(WorldCrd.xz, GridStep*1.0));
    Out.Color.xyz = lerp(Out.Color.xyz, GridColors[1], GridTex(WorldCrd.xz, GridStep*10.0));
    Out.Color.xyz = lerp(Out.Color.xyz, GridColors[2], GridTex(WorldCrd.xz, GridStep*100.0));
    
    float mainAxis = GridTex(WorldCrd.xz, GridStep*1000.0);
    if ((abs(WorldCrd.x)<GridStep*2.0)||(abs(WorldCrd.z)<GridStep*2.0)){
        Out.Color.xyz = lerp(Out.Color.xyz, GridColors[3], mainAxis);
        Out.Color.a = lerp(Out.Color.a, 1.0, mainAxis);
    }
    
//    Out.Color.xyz = 0.25;
//    Out.Color.a = GridTex(WorldCrd.xz, GridStep);
//    Out.Color.a = max(Out.Color.a, GridTex(WorldCrd.xz, GridStep*10.0));
//    Out.Color.a = max(Out.Color.a, GridTex(WorldCrd.xz, GridStep*100.0));
    return Out;
}