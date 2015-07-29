#ifndef PHONG_H
#define	PHONG_H
float3 Phong(float3 lightPos, float3 eyePos, float3 pixelPos, float3 normal, float3 diffuse)
{
    float3 lightDirInv = normalize(lightPos - pixelPos);
    float DiffK = max(0.0, dot(lightDirInv, normal));
    return (diffuse*DiffK);
}
#endif	/* PHONG_H */