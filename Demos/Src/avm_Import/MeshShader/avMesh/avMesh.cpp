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
    float4 aiBoneMatDifNormOffset: aiBoneMatDifNormOffset;
};

struct VS_Output {
    float4 Pos                 : SV_Position;
    float3 vCoord              : vCoord;
    float3 vNorm               : vNorm;
    float2 vTex                : vTex;
    float4 Diffuse             : Diffuse;
    float4 Specular            : Specular;
    float4 DiffK_SpecPow_MapInd: DiffK_SpecPow_MapInd;
};

Texture2DArray BoneTransform; SamplerState BoneTransformSampler;
Texture2D Materials; SamplerState MaterialsSampler;

float BonePixelHeight;
float4x4 GetBoneTransform(in float ArraySlice, in float BoneCoord) {
    float4x4 m;
    m[0] = BoneTransform.SampleLevel(BoneTransformSampler, float3(0.125, BoneCoord, ArraySlice), 0);
    m[1] = BoneTransform.SampleLevel(BoneTransformSampler, float3(0.375, BoneCoord, ArraySlice), 0);
    m[2] = BoneTransform.SampleLevel(BoneTransformSampler, float3(0.625, BoneCoord, ArraySlice), 0);
    m[3] = BoneTransform.SampleLevel(BoneTransformSampler, float3(0.875, BoneCoord, ArraySlice), 0);
    return m;
}

float4x4 GetBoneTransform(in float ArraySlice, in float4 Indices, in float4 Weights) {
    float3 TexSize;
    BoneTransform.GetDimensions(TexSize.x, TexSize.y, TexSize.z);
    float4 IndicesNorm = (Indices+0.5) / TexSize.y;
    float4x4 m = {
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    };
    if (Indices.x>=0.0) m  = GetBoneTransform(ArraySlice, IndicesNorm.x)*Weights.x;
    if (Indices.y>=0.0) m += GetBoneTransform(ArraySlice, IndicesNorm.y)*Weights.y;
    if (Indices.z>=0.0) m += GetBoneTransform(ArraySlice, IndicesNorm.z)*Weights.z;
    if (Indices.w>=0.0) m += GetBoneTransform(ArraySlice, IndicesNorm.w)*Weights.w;
    return m;
}

void GetMaterial(in float MatIndex, out float4 Diff, out float4 Spec, out float2 DiffK_SpecPow)
{
    float2 texSize;
    Materials.GetDimensions(texSize.x, texSize.y);
    float2 pixSize = 1.0/texSize;
    Diff          = Materials.SampleLevel(MaterialsSampler, float2(0.5, MatIndex+0.5)*pixSize, 0);
    Spec          = Materials.SampleLevel(MaterialsSampler, float2(1.5, MatIndex+0.5)*pixSize, 0);
    DiffK_SpecPow = Materials.SampleLevel(MaterialsSampler, float2(2.5, MatIndex+0.5)*pixSize, 0).xy;
}

VS_Output VS(VS_Input In) {
    VS_Output Out;
    float4x4 mBone = GetBoneTransform(In.aiBoneMatDifNormOffset.x, In.vsWIndex, In.vsWeight);
    float3 crd = mul(mBone, float4(In.vsCoord, 1.0)).xyz;
    float3 norm = mul( (float3x3) mBone, In.vsNormal);
    //float3 crd = In.vsCoord;
    Out.vCoord = mul(V_Matrix, float4(crd, 1.0)).xyz;
    Out.vNorm = mul((float3x3)V_Matrix, normalize(norm));
    Out.vTex = In.vsTex;
    Out.Pos = mul(P_Matrix, float4(Out.vCoord, 1.0));    
    GetMaterial(In.aiBoneMatDifNormOffset.y + In.vsMatIndex, 
                Out.Diffuse, 
                Out.Specular, 
                Out.DiffK_SpecPow_MapInd.xy);
    Out.DiffK_SpecPow_MapInd.zw = In.aiBoneMatDifNormOffset.zw + In.vsMatIndex;
    return Out;
}

///////////////////////////////////////////////////////////////////////////////

Texture2DArray Maps; SamplerState MapsSampler;

struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    float4 diff = lerp(In.Diffuse, Maps.Sample(MapsSampler, float3(In.vTex, In.DiffK_SpecPow_MapInd.z)), In.DiffK_SpecPow_MapInd.x);
    float4 spec = {0,0,0,0};
    float4 amb = 0.3;
    float3 lightColor = {1,1,1};
    float3 n = normalize(In.vNorm);
    float3 viewDir = normalize(In.vCoord);
    
    float4 c = PhongColor(-n, viewDir, viewDir, lightColor, diff, spec, amb, 0.0);
    Out.Color = c;
    return Out;
}