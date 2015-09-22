/* 
 * File:   lighting.h
 * Author: MrShoor
 *
 * Created on 19 sen 2015, 23:35
 */

#ifndef LIGHTING_H
#define	LIGHTING_H

float4 PhongColor(float3 Normal, float3 ViewDir, float3 LightDir, float3 LightColor, float4 Diffuse, float4 Specular, float4 Ambient, float SpecPower)
{
   float3 Reflect = reflect(LightDir, Normal);
   float3 DiffuseK = LightColor * (max(0, dot(Normal, LightDir)));
   float3 DiffuseColor = Diffuse.rgb * DiffuseK + Ambient.rgb;
   float3 SpecularColor = Specular.rgb * (pow(max(0.0, -dot(LightDir, Reflect)), SpecPower));
   return float4(DiffuseColor + SpecularColor, Diffuse.a);
}

#endif	/* LIGHTING_H */

