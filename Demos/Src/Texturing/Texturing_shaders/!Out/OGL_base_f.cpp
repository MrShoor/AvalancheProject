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
 in  vec4 Normal0;
vec4 Input1;
layout(location = 0) out  vec4 SV_Target0;
#define Output0 SV_Target0
vec4 Temp[1];
ivec4 Temp_int[1];
uvec4 Temp_uint[1];
void main()
{
    Input1 = Normal0;
    Temp[0].x = dot(Input1.xyz, Input1.xyz);
    Temp[0].x = inversesqrt(Temp[0].x);
    Temp[0].x = Temp[0].x * Input1.z;
    Output0 = max((-Temp[0].xxxx), vec4(intBitsToFloat(0x3E99999A), intBitsToFloat(0x3E99999A), intBitsToFloat(0x3E99999A), intBitsToFloat(0x3E99999A)));
    return;
}
