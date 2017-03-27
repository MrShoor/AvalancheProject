/* 
 * File:   HeightMap.h
 * Author: alexander.busarov
 *
 * Created on March 11, 2017, 8:19 AM
 */

#ifndef HEIGHTMAP_H
#define	HEIGHTMAP_H

Texture2D HeightMap; SamplerState HeightMapSampler;
Texture2D HeightNormalMap; SamplerState HeightNormalMapSampler;

float2 GetTexCoord(float2 coord2D) {
    float2 texSize;
    HeightMap.GetDimensions(texSize.x, texSize.y);
    //return coord2D / texSize;
    float2 uv = coord2D / texSize;
    uv = uv*texSize + 0.5;
    float2 iuv = floor(uv);
    float2 fuv = frac(uv);
    uv = iuv + fuv*fuv*(3.0-2.0*fuv); // fuv*fuv*fuv*(fuv*(fuv*6.0-15.0)+10.0);;
    uv = (uv - 0.5)/texSize;
    return uv;
}

float3 GetMapCoord(float2 coord2D) {
    float3 c = float3(coord2D, -HeightMap.Sample(HeightMapSampler, GetTexCoord(coord2D)).r*255);
    return c.xyz;
}

float3 GetMapCoord(float2 coord2D, float LOD) {
    float3 c = float3(coord2D, -HeightMap.SampleLevel(HeightMapSampler, GetTexCoord(coord2D), LOD).r*255);
    return c.xyz;
}

float3 GetMapNormal(float2 coord2D, float LOD) {
    float2 rg = HeightNormalMap.SampleLevel(HeightNormalMapSampler, GetTexCoord(coord2D), LOD).xy;
    rg *= 2.0;
    rg -= 1.0;
    float z = sqrt(abs(1.0 - dot(rg, rg)));
    return float3(rg, -z);
}

float3 GetMapNormal(float2 coord2D) {
    float2 rg = HeightNormalMap.Sample(HeightNormalMapSampler, GetTexCoord(coord2D)).xy;
    rg *= 2.0;
    rg -= 1.0;
    float z = sqrt(abs(1.0 - dot(rg, rg)));
    return float3(rg, -z);
}

void GetMapCoordWithNormal(float2 coord2D, out float3 wCoord, out float3 wNormal) {
    wCoord = GetMapCoord(coord2D);
    wNormal = GetMapNormal(coord2D);
}

void GetMapCoordWithNormal(float2 coord2D, float LOD, out float3 wCoord, out float3 wNormal) {
    wCoord = GetMapCoord(coord2D, LOD);
    wNormal = GetMapNormal(coord2D, LOD);
}

#endif	/* HEIGHTMAP_H */

