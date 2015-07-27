#version 330
struct vec1 {
	float x;
};
struct uvec1 {
	uint x;
};
struct ivec1 {
	int x;
};
uniform sampler2D Diffuse;
 in  vec4 Normal0;
vec4 Input1;
 in  vec4 ViewPos0;
vec4 Input2;
 in  vec4 TexCrd0;
vec4 Input3;
layout(location = 0) out  vec4 SV_Target0;
#define Output0 SV_Target0
vec4 Temp[2];
ivec4 Temp_int[2];
uvec4 Temp_uint[2];
void main()
{
    Input1 = Normal0;
    Input2 = ViewPos0;
    Input3 = TexCrd0;
    Temp[0].x = dot(Input1.xyz, Input1.xyz);
    Temp[0].x = inversesqrt(Temp[0].x);
    Temp[0].xyz = Temp[0].xxx * Input1.xyz;
    Temp[0].w = dot(Input2.xyz, Input2.xyz);
    Temp[0].w = inversesqrt(Temp[0].w);
    Temp[1].xyz = Temp[0].www * Input2.xyz;
    Temp[0].x = dot(Temp[1].xyz, Temp[0].xyz);
    Temp[0].x = max((-Temp[0].x), intBitsToFloat(0x0));
    Temp[1] = texture(Diffuse, Input3.xy);
    Output0 = Temp[0].xxxx * Temp[1];
    return;
}
