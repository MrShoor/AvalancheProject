/* 
 * File:   avModelMaterials.h
 * Author: alexander.busarov
 *
 * Created on April 25, 2017, 6:53 PM
 */

#ifndef AVMODELMATERIALS_H
#define	AVMODELMATERIALS_H

Texture2D Materials; SamplerState MaterialsSampler;
Texture2DArray Maps; SamplerState MapsSampler;

struct ModelMaterialDesc {
    float4 Diff;
    float4 Spec;
    float4 Hardness_IOR_EmitFactor;
    
    float4 mapDiffuse_Intensity_Color;
    float4 mapDiffuse_Alpha_Translucency;
    float4 mapShading_Ambient_Emit;
    float4 mapShading_Mirror_RayMirror;
    float4 mapSpecular_Intensity_Color;
    float4 mapSpecular_Hardness_mapGeometry_Normal;
    float4 mapGeometry_Warp_Displace;
    
    float4 Diffuse_Color(float2 TexCoord, float4 BaseValue) {
        if (mapDiffuse_Intensity_Color.w > 0.001) {
            return lerp(BaseValue, Maps.Sample(MapsSampler, float3(TexCoord, mapDiffuse_Intensity_Color.z)), mapDiffuse_Intensity_Color.w);
        } else {
            return BaseValue;
        }
    }
    float4 Geometry_Normal(float2 TexCoord, float4 BaseValue) {
        if (mapSpecular_Hardness_mapGeometry_Normal.w > 0.001) {
            return lerp(BaseValue, Maps.Sample(MapsSampler, float3(TexCoord, mapSpecular_Hardness_mapGeometry_Normal.z)), mapSpecular_Hardness_mapGeometry_Normal.w);
        } else {
            return BaseValue;
        }
    }
    float4 Geometry_Hardness(float2 TexCoord, float4 BaseValue) {
        if (mapSpecular_Hardness_mapGeometry_Normal.y > 0.001) {
            return lerp(BaseValue, Maps.Sample(MapsSampler, float3(TexCoord, mapSpecular_Hardness_mapGeometry_Normal.x)), mapSpecular_Hardness_mapGeometry_Normal.y);
        } else {
            return BaseValue;
        }
    }
    float4 Specular_Intensity(float2 TexCoord, float4 BaseValue) {
        if (mapSpecular_Intensity_Color.y > 0.001) {
            return lerp(BaseValue, Maps.Sample(MapsSampler, float3(TexCoord, mapSpecular_Intensity_Color.x)), mapSpecular_Intensity_Color.y);
        } else {
            return BaseValue;
        }
    }
};

ModelMaterialDesc LoadMaterialDesc(int MatIndex) {
    ModelMaterialDesc Out;
    Out.Diff                                    = Materials.Load(int3(0, MatIndex, 0));
    Out.Spec                                    = Materials.Load(int3(1, MatIndex, 0));
    Out.Hardness_IOR_EmitFactor                 = Materials.Load(int3(2, MatIndex, 0));
    Out.mapDiffuse_Intensity_Color              = Materials.Load(int3(3, MatIndex, 0));
    Out.mapDiffuse_Alpha_Translucency           = Materials.Load(int3(4, MatIndex, 0));
    Out.mapShading_Ambient_Emit                 = Materials.Load(int3(5, MatIndex, 0));
    Out.mapShading_Mirror_RayMirror             = Materials.Load(int3(6, MatIndex, 0));
    Out.mapSpecular_Intensity_Color             = Materials.Load(int3(7, MatIndex, 0));
    Out.mapSpecular_Hardness_mapGeometry_Normal = Materials.Load(int3(8, MatIndex, 0));
    Out.mapGeometry_Warp_Displace               = Materials.Load(int3(9, MatIndex, 0));
    return Out;
}

#endif	/* AVMODELMATERIALS_H */

