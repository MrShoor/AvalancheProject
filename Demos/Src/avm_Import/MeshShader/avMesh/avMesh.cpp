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
    Out.MatIndex = In.aiBoneMatOffset.y + In.vsMatIndex + 0.5;
    return Out;
}

///////////////////////////////////////////////////////////////////////////////

float3 UnpackNormal(float4 PackedNormal) {
    float3 Out = (PackedNormal.xyz-0.5)*2.0;
    return Out;
}

struct PS_Output {
    float4 Color : SV_Target0;
};

static const float LightInt = 3;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    In.vNorm = normalize(In.vNorm);
    
    ModelMaterialDesc m = LoadMaterialDesc((int)In.MatIndex);
    float3 norm = In.vNorm;
    
    if (m.mapSpecular_Hardness_mapGeometry_Normal.w > 0.001) {
        float3x3 tbn = CalcTBN(In.vCoord, In.vNorm, In.vTex);
        float3 norm = UnpackNormal(m.Geometry_Normal(In.vTex, float4(0.5,0.5,1,0)));
        norm = mul(norm, tbn);
    }    
    
    float4 diff = m.Diffuse_Color(In.vTex, m.Diff);
    //diff = pow(abs(diff), 2.2);
    float roughness = m.Geometry_Hardness(In.vTex, 0.5).x;
    float metallic = m.Specular_Intensity(In.vTex, 0.0).x;
    
    metallic = 1.0 - pow(abs(1.0-metallic), 32);
    
    float4 spec = {1,1,1,1};
    float4 amb = 0.3;
    float3 lightColor = {1,1,1};
    float3 n = normalize(norm);
    float3 viewDir = normalize(-In.vCoord);
    
    float3 F0;
    F0 = diff.xyz * metallic;
    diff.xyz *= (1.0 - metallic);
    
    //float3 c = PhongColor(n, viewDir, viewDir, lightColor, diff, spec, amb, 20.0).rgb;
    //float3 LightPos = float3(10, 0, 0);
    //float3 LightDir = normalize(LightPos - In.vCoord);
    //float3 H = normalize(LightDir + viewDir);
    
    //float3 c = CookTorrance_GGX(n, LightDir, viewDir, H, F0, diff.xyz, roughness)*5;
    float3 c = CookTorrance_GGX_sampled(n, viewDir, F0, diff.xyz, roughness)*LightInt;

    Out.Color = float4(tonemapReinhard(c), diff.a);
    //Out.Color = -In.vNorm.z;
    return Out;
}