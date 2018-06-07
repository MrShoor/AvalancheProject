/* 
 * File:   hinting.h
 * Author: alexander.busarov
 *
 * Created on June 6, 2018, 10:24 PM
 */

#ifndef HINTING_H
#define	HINTING_H

#include "UICommon.h"

float3 HintVertex(in float3 crd, in float2 HintInfo) {
    float4 hintcrd = mul(float4(crd, 1.0), UIMatrix);
    hintcrd.xyz /= hintcrd.w;
    hintcrd.xy += float2(1.0,1.0);
    hintcrd.xy *= 0.5*ViewPortSize;
    if (HintInfo.x)
        hintcrd.x = round(hintcrd.x);
    if (HintInfo.y)
        hintcrd.y = round(hintcrd.y);
    hintcrd.xy /= 0.5*ViewPortSize;
    hintcrd.xy -= float2(1.0, 1.0);
    hintcrd.xyz *= hintcrd.w;
    hintcrd = mul(hintcrd, UIMatrixInverse);
    return hintcrd.xyz;
}

float2 HintVertex(in float2 crd, in float2 HintInfo) {
    float3 crd3 = float3(crd, 0.0);
    crd3 = HintVertex(crd3, HintInfo);
    return crd3.xy;
}

float4 PostHintVertex(in float4 crd, in float2 HintInfo) {
    float2 hintcrd;
    hintcrd = crd.xy;
    hintcrd /= crd.w;
    hintcrd += float2(1.0,1.0);
    hintcrd *= 0.5*ViewPortSize;
    if (HintInfo.x)
        hintcrd.x = round(hintcrd.x);
    if (HintInfo.y)
        hintcrd.y = round(hintcrd.y);
    hintcrd /= 0.5*ViewPortSize;
    hintcrd -= float2(1.0, 1.0);
    hintcrd *= crd.w;
    return float4(hintcrd, crd.z, crd.w);
}

#endif	/* HINTING_H */

