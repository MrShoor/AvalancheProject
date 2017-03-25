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
    return coord2D / texSize;
}

float3 GetMapCoord(float2 coord2D) {
    float3 c = float3(coord2D, -HeightMap.SampleLevel(HeightMapSampler, GetTexCoord(coord2D), 0).r*255);
    return c.xyz;
}

float3 GetMapCoord(float2 coord2D, float LOD) {
    float3 c = float3(coord2D, -HeightMap.SampleLevel(HeightMapSampler, GetTexCoord(coord2D), LOD).r*255);
    return c.xyz;
}

float3 GetMapNormal(float2 coord2D) {
    float2 rg = HeightNormalMap.SampleLevel(HeightNormalMapSampler, GetTexCoord(coord2D), 0).xy;
    rg *= 2.0;
    rg -= 1.0;
    float z = sqrt(abs(1.0 - dot(rg, rg)));
    return float3(rg, z);
}

void GetMapCoordWithNormal(float2 coord2D, out float3 wCoord, out float3 wNormal) {
    wCoord = GetMapCoord(coord2D);
    
    float3 neibs[4];
    neibs[0] = GetMapCoord(coord2D+float2( 1, 0)) - wCoord;
    neibs[1] = GetMapCoord(coord2D+float2( 0, 1)) - wCoord;
    neibs[2] = GetMapCoord(coord2D+float2(-1, 0)) - wCoord;
    neibs[3] = GetMapCoord(coord2D+float2( 0,-1)) - wCoord;
    wNormal = 0.0000001;
    for (uint i = 0; i < 4; i++) {
        wNormal += cross(neibs[i], neibs[(i+1)%4]);
    }

    wNormal = normalize(wNormal);
}

void GetMapCoordWithNormal(float2 coord2D, float LOD, out float3 wCoord, out float3 wNormal) {
    wCoord = GetMapCoord(coord2D);
    wNormal = GetMapNormal(coord2D);
    /*
    wCoord = GetMapCoord(coord2D);
    
    float3 neibs[4];
    neibs[0] = GetMapCoord(coord2D+float2( 1, 0)) - wCoord;
    neibs[1] = GetMapCoord(coord2D+float2( 0, 1)) - wCoord;
    neibs[2] = GetMapCoord(coord2D+float2(-1, 0)) - wCoord;
    neibs[3] = GetMapCoord(coord2D+float2( 0,-1)) - wCoord;
    wNormal = 0.0000001;
    for (uint i = 0; i < 4; i++) {
        wNormal += cross(neibs[i], neibs[(i+1)%4]);
    }

    wNormal = normalize(wNormal);
     */ 
}

#endif	/* HEIGHTMAP_H */

