#include "hlsl.h"
#include "matrices.h"
#include "..\lighting.h"

struct VS_Input {
    float3 vsCoord   : vsCoord;
    float3 vsNormal  : vsNormal;
    float2 vsTex     : vsTex;
    float  vsMatIndex: vsMatIndex;
    float4 vsWIndex  : vsWIndex;
    float4 vsWeight  : vsWeight;
};

struct VS_Output {
    float4 Pos    : SV_Position;
    float3 vCoord : vCoord;
    float3 vNorm  : vNorm;
    float2 vTex   : vTex;
};

Texture2D BoneTransform; SamplerState BoneTransformSampler;
float BonePixelHeight;
float4x4 GetBoneTransform(in float BoneIndex) {
    float2 texSize;
    float ycrd = BonePixelHeight*(BoneIndex+0.5);
    float4x4 m = 0;
    m[0] = BoneTransform.SampleLevel(BoneTransformSampler, float2(0.125, ycrd), 0);
    m[1] = BoneTransform.SampleLevel(BoneTransformSampler, float2(0.375, ycrd), 0);
    m[2] = BoneTransform.SampleLevel(BoneTransformSampler, float2(0.625, ycrd), 0);
    m[3] = BoneTransform.SampleLevel(BoneTransformSampler, float2(0.875, ycrd), 0);
    return m;
}

float4x4 GetBoneTransform(in float4 Indices, in float4 Weights) {
    float4x4 m = {
        1,0,0,0,
        0,1,0,0,
        0,0,1,0,
        0,0,0,1
    };
    if (BonePixelHeight==0.0) return(m);    
    if (Indices.x>=0.0) m  = GetBoneTransform(Indices.x)*Weights.x;
    if (Indices.y>=0.0) m += GetBoneTransform(Indices.y)*Weights.y;
    if (Indices.z>=0.0) m += GetBoneTransform(Indices.z)*Weights.z;
    if (Indices.w>=0.0) m += GetBoneTransform(Indices.w)*Weights.w;
    return m;
}

VS_Output VS(VS_Input In) {
    VS_Output Out;
    float3 crd = mul(GetBoneTransform(In.vsWIndex, In.vsWeight), float4(In.vsCoord, 1.0)).xyz;
    //float3 crd = In.vsCoord;
    Out.vCoord = mul(V_Matrix, float4(crd, 1.0)).xyz;
    Out.vNorm = mul((float3x3)V_Matrix, normalize(In.vsNormal));
    Out.vTex = In.vsTex;
    Out.Pos = mul(P_Matrix, float4(Out.vCoord, 1.0));
    return Out;
}

Texture2D DiffuseMap; SamplerState DiffuseMapSampler;

struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float4 diff = DiffuseMap.Sample(DiffuseMapSampler, In.vTex);
    float4 spec = {0,0,0,0};
    float4 amb = 0.3;
    float3 lightColor = {1,1,1};
    float3 n = normalize(In.vNorm);
    float3 viewDir = normalize(In.vCoord);
    
    float4 c = PhongColor(-n, viewDir, viewDir, lightColor, diff, spec, amb, 0.0);
    Out.Color = c;
    return Out;
}