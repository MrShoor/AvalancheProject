/* 
 * File:   brush_utils.h
 * Author: alexander.busarov
 *
 * Created on March 26, 2017, 11:04 PM
 */

#ifndef BRUSH_UTILS_H
#define	BRUSH_UTILS_H

float2 BrushPos;
float  BrushRadius;

static const float PI = 3.1415926535897932384626433832795;

float BrushForce;
float BrushSharp;

float GetBrushValue(float2 wPos) {
    float d = length(wPos - BrushPos);
    d = saturate((1.0-d/BrushRadius)*BrushSharp);
    
    d = 1.0 - (cos(d*PI)+1.0)*0.5;
    d = saturate(d);
    return d;
}

#endif	/* BRUSH_UTILS_H */

