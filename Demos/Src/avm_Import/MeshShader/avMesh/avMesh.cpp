#include "hlsl.h"
#include "matrices.h"
#include "lighting.h"
#include "avModelMaterials.h"

struct VS_Input {
    float3 vsCoord   : vsCoord;
    float3 vsNormal  : vsNormal;
    float2 vsTex     : vsTex;
    float  vsMatIndex: vsMatIndex;
    float4 vsWIndex  : vsWIndex;
    float4 vsWeight  : vsWeight;
    float2 aiBoneMatOffset: aiBoneMatOffset;
};

struct VS_Output {
    float4 Pos       : SV_Position;
    float3 vCoord    : vCoord;
    float3 vNorm     : vNorm;
    float2 vTex      : vTex;
    float  MatIndex  : MatIndex;
};

Texture2D BoneTransform; SamplerState BoneTransformSampler;

float4x4 GetBoneTransform(in float BoneCoord) {
    float2 TexSize;
    BoneTransform.GetDimensions(TexSize.x, TexSize.y);
    float2 PixSize = 1.0 / TexSize;
    
    float2 TexCoord;
    TexCoord.x = frac(BoneCoord / TexSize.x);
    TexCoord.y = trunc(BoneCoord / TexSize.x) / TexSize.y;
    TexCoord += 0.5 * PixSize;
    
    float4x4 m;
    m[0] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x,                 TexCoord.y), 0);
    m[1] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x +     PixSize.x, TexCoord.y), 0);
    m[2] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x + 2.0*PixSize.x, TexCoord.y), 0);
    m[3] = BoneTransform.SampleLevel(BoneTransformSampler, float2(TexCoord.x + 3.0*PixSize.x, TexCoord.y), 0);
    return m;
}

float4x4 GetBoneTransform(in float4 Indices, in float4 Weights) {
    float4x4 m = {
        1, 0, 0, 0,
        0, 1, 0, 0,
        0, 0, 1, 0,
        0, 0, 0, 1
    };
    float4 ind = Indices*4.0;
    if (Indices.x>=0.0) m  = GetBoneTransform(ind.x)*Weights.x;
    if (Indices.y>=0.0) m += GetBoneTransform(ind.y)*Weights.y;
    if (Indices.z>=0.0) m += GetBoneTransform(ind.z)*Weights.z;
    if (Indices.w>=0.0) m += GetBoneTransform(ind.w)*Weights.w;
    return m;
}

VS_Output VS(VS_Input In) {
    VS_Output Out;
    float4x4 mBone = GetBoneTransform(In.vsWIndex+In.aiBoneMatOffset.x, In.vsWeight);
    float3 crd = mul(float4(In.vsCoord, 1.0), mBone).xyz;
    float3 norm = mul( In.vsNormal, (float3x3) mBone );
    Out.vCoord = mul(float4(crd, 1.0), V_Matrix).xyz;
    Out.vNorm = mul(normalize(norm), (float3x3)V_Matrix);
    Out.vTex = In.vsTex;
    Out.Pos = mul(float4(Out.vCoord, 1.0), P_Matrix);
    Out.MatIndex = In.aiBoneMatOffset.y + In.vsMatIndex;
    return Out;
}

///////////////////////////////////////////////////////////////////////////////

float3x3 CalcTBN(float3 vPos, float3 vNorm, float2 vTex) {
    float3 dPos1 = ddx(vPos);
    float3 dPos2 = ddy(vPos);
    float2 dTex1 = ddx(vTex);
    float2 dTex2 = ddy(vTex);
 
    float3 v2 = cross(dPos2, vNorm);
    float3 v1 = cross(vNorm, dPos1);
    float3 T = v2 * dTex1.x + v1 * dTex2.x;
    float3 B = v2 * dTex1.y + v1 * dTex2.y;
 
    float invdet = 1.0/sqrt(max( dot(T,T), dot(B,B) ));
    
    return float3x3( T * invdet, B * invdet, vNorm );
}

float3 UnpackNormal(float4 PackedNormal) {
    float3 Out = (PackedNormal.xyz-0.5)*2.0;
    Out.z = -Out.z;
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    In.vNorm = normalize(In.vNorm);
    
    ModelMaterialDesc m = LoadMaterialDesc(round(In.MatIndex));
    float3x3 tbn = CalcTBN(In.vCoord, In.vNorm, In.vTex);    
    float3 norm = UnpackNormal(m.Geometry_Normal(In.vTex, float4(0.5,0.5,1,0)));
    float4 diff = m.Diffuse_Color(In.vTex, m.Diff);
    float roughness = m.Geometry_Hardness(In.vTex, 0.5).x;
    float metallic = m.Specular_Intensity(In.vTex, 0.0).x;
    
    norm = mul(norm, tbn);
    
    float4 spec = {1,1,1,1};
    float4 amb = 0.3;
    float3 lightColor = {1,1,1};
    float3 n = normalize(norm);
    float3 viewDir = normalize(In.vCoord);
    
    float4 c = PhongColor(n, viewDir, viewDir, lightColor, diff, spec, amb, 20.0);
    //c.xyz = diff;
    //c.xyz = m.mapSpecular_Hardness_mapGeometry_Normal.w;
    Out.Color = c;
    return Out;
}