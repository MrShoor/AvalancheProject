/* 
 * File:   UICommon.h
 * Author: alexander.busarov
 *
 * Created on June 3, 2018, 8:28 PM
 */

#ifndef UICOMMON_H
#define	UICOMMON_H

#pragma pack_matrix( row_major )
float4x4 UIMatrix;
float4x4 UIMatrixInverse;
float2 ViewPortSize;
float PixelToUnit;

float3 RotationOffset;

float3x3 Mat(float Rotate, float2 Offset) {
    float3x3 Out;
    float cs = cos(Rotate);
    float sn = sin(Rotate);
    Out[0] = float3(cs, -sn, 0);
    Out[1] = float3(sn,  cs, 0);
    Out[2] = float3(Offset, 1);
    return Out;
}

#endif	/* UICOMMON_H */
