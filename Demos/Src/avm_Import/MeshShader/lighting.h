/* 
 * File:   lighting.h
 * Author: MrShoor
 *
 * Created on 19 sen 2015, 23:35
 */

#ifndef LIGHTING_H
#define	LIGHTING_H

static const float PI = 3.1415926535897932384626433832795;

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

float4 PhongColor(float3 Normal, float3 ViewDir, float3 LightDir, float3 LightColor, float4 Diffuse, float4 Specular, float4 Ambient, float SpecPower)
{
   float3 Reflect = reflect(LightDir, Normal);
   float3 DiffuseK = LightColor * (max(0, dot(Normal, LightDir)));
   float3 DiffuseColor = Diffuse.rgb * DiffuseK + Ambient.rgb;
   float3 SpecularColor = Specular.rgb * (pow(max(0.0, -dot(LightDir, Reflect)), SpecPower));
   return float4(DiffuseColor + SpecularColor, Diffuse.a);
}

float GGX_PartialGeometry(float cosThetaN, float alpha)
{
    float cosTheta_sqr = saturate(cosThetaN*cosThetaN);
    float tan2 = ( 1 - cosTheta_sqr ) / cosTheta_sqr;
    float GP = 2 / ( 1 + sqrt( 1 + alpha * alpha * tan2 ) );
    return GP;
}

float GGX_Distribution(float cosThetaNH, float alpha)
{
    float alpha2 = alpha * alpha;
    float NH_sqr = saturate(cosThetaNH * cosThetaNH);
    float den = NH_sqr * alpha2 + (1.0 - NH_sqr);
    return alpha2 / ( PI * den * den );
}

float3 FresnelSchlick(float3 F0, float cosTheta) {
    return F0 + (1.0 - F0) * pow(1.0 - saturate(cosTheta), 5.0);
}

float3 CookTorrance_GGX(float3 n, float3 l, float3 v, float3 h, float3 F0, float3 albedo, float roughness) {
    //precompute dots
    float NL = dot(n, l);
    if (NL <= 0.0) return 0.0;
    float NV = dot(n, v);
    if (NV <= 0.0) return 0.0;
    float NH = dot(n, h);
    float HV = dot(h, v);
    
    //precompute roughness square
    float roug_sqr = roughness*roughness;
    
    //calc coefficients
    float G = GGX_PartialGeometry(NV, roug_sqr) * GGX_PartialGeometry(NL, roug_sqr);
    float D = GGX_Distribution(NH, roug_sqr);
    float3 F = FresnelSchlick(F0, HV);
    
    //mix
    float3 specK = G*D*F*0.25/(NV);    
    float3 diffK = saturate(1.0-F)/PI;
    return max(0.0, albedo*diffK*NL + specK);
}

#define MaxSamplesCount 1024
float uSamplesCount;
float4 uHammersleyPts[MaxSamplesCount];
TextureCube uRadiance; SamplerState uRadianceSampler;
TextureCube uIrradiance; SamplerState uIrradianceSampler;

float ComputeLOD_AParam(){
    float w, h;
    uRadiance.GetDimensions(w, h);
    return 0.5*log2(w*h/uSamplesCount);
}

float ComputeLOD(float AParam, float pdf, float3 l) {
    float du = 2.0*1.2*(abs(l.z)+1.0);
    return max(0.0, AParam-0.5*log2(pdf*du*du)+1.0);
}

float3x3 GetSampleTransform(float3 Normal) {
  float3x3 w;
  float3 up = abs(Normal.y) < 0.999 ? float3(0,1,0) : float3(1,0,0);
  w[0] = normalize ( cross( up, Normal ) );
  w[1] = cross( Normal, w[0] );
  w[2] = Normal;
  return w;
}

float3 GGX_Sample(float2 E, float alpha) {
    float Phi = 2.0*PI*E.x;
    float cosThetha = saturate(sqrt( (1.0 - E.y) / (1.0 + alpha*alpha * E.y - E.y) ));
    float sinThetha = sqrt( 1.0 - cosThetha*cosThetha);
    return float3(sinThetha*cos(Phi), sinThetha*sin(Phi), cosThetha);
}

float3 CookTorrance_GGX_sample(float3 n, float3 l, float3 v, float3 h, float3 f0, float roughness, out float3 FK, out float pdf) {
    pdf = 0.0;
    FK = 0.0;
//    n = normalize(n);
//    v = normalize(v);
//    l = normalize(l);
//    float3 h = normalize(v+l);
    //precompute dots
    float NL = dot(n, l);
    if (NL <= 0.0) return 0.0;
    float NV = dot(n, v);
    if (NV <= 0.0) return 0.0;
    float NH = dot(n, h);
    float HV = dot(h, v);
    
    //precompute roughness square
    float roug_sqr = roughness*roughness;
    
    //calc coefficients
    float G = GGX_PartialGeometry(NV, roug_sqr) * GGX_PartialGeometry(NL, roug_sqr);
    float3 F = FresnelSchlick(f0, HV);
    FK = F;
    
    float D = GGX_Distribution(NH, roug_sqr);
    pdf = D*NH/(4.0*HV);

    float3 specK = G*F*HV/(NV*NH);
    return max(float3(0,0,0), specK);
}

float3 CookTorrance_GGX_sampled(float3 n, float3 v, float3 F0, float3 albedo, float roughness) {
    float3 Out = 0.0;
    float3x3 HTransform = GetSampleTransform(n);
    
    float LOD_Aparam = ComputeLOD_AParam();
    
    float3 specColor = 0.0;
    float3 FK_summ = 0.0;
    for (uint i=0; i<(uint)uSamplesCount; i++){
        float3 H = GGX_Sample(uHammersleyPts[i].xy, roughness*roughness);
        H = mul(H, HTransform);
        float3 LightDir = reflect(-v, H);

        float3 specK;
        float pdf;
        float3 FK;
        specK = CookTorrance_GGX_sample(n, LightDir, v, H, F0, roughness, FK, pdf);
        FK_summ += FK;
        float LOD = ComputeLOD(LOD_Aparam, pdf, LightDir);
        float3 LightColor = uRadiance.SampleLevel(uRadianceSampler, mul(LightDir.xyz, (float3x3)V_InverseMatrix), LOD).rgb;
        specColor += specK * LightColor;
    }
    specColor /= uSamplesCount;
    FK_summ /= uSamplesCount;
    float3 LightColor = uIrradiance.Sample(uIrradianceSampler, mul(n, (float3x3)V_InverseMatrix)).rgb;
    return albedo*saturate(1.0-FK_summ)*LightColor + specColor;
}

float3 tonemapReinhard_simple(float3 x){
    return x / (1.0 + x);
}

float3 tonemapReinhard(float3 x){
    float exposure = 0.125;
    float lum = dot(x, float3(0.2126f, 0.7152f, 0.0722f));
    float L = exposure*lum;//(scale / averageLum) * lum;
    //float Ld = (L * (1.0 + L / lumwhite2)) / (1.0 + L);
    float Ld = (L * (1.0 + L)) / (1.0 + L);
    return (x / lum) * Ld;
}

#endif	/* LIGHTING_H */

