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
 in  vec4 Color1;
vec4 Input1;
 in  vec4 quadCoord0;
vec4 Input2;
layout(location = 0) out  vec4 SV_Target0;
#define Output0 SV_Target0
vec4 Temp[1];
ivec4 Temp_int[1];
uvec4 Temp_uint[1];
void main()
{
    Input1 = Color1;
    Input2 = quadCoord0;
    Temp[0].x = dot(Input2.xy, Input2.xy);
    Temp[0].x = sqrt(Temp[0].x);
    Temp[0].x = Temp[0].x + intBitsToFloat(int(0xBF19999A));
    Temp[0].x = min(abs(Temp[0].x), intBitsToFloat(0x3F800000));
    Temp[0].x = (-Temp[0].x) + intBitsToFloat(0x3F8CCCCD);
    Temp[0].x = log2(Temp[0].x);
    Temp[0].x = Temp[0].x * intBitsToFloat(int(0x41200000));
    Output0.w = exp2(Temp[0].x);
    Output0.xyz = Input1.xyz;
    return;
}
