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
uniform 	mat4 VP_Matrix;
uniform 	mat4 V_Matrix;
uniform 	mat4 P_Matrix;
uniform 	mat4 VP_InverseMatrix;
uniform 	mat4 V_InverseMatrix;
uniform 	mat4 P_InverseMatrix;
uniform 	vec2 PixelSize;
uniform 	float Rad;
uniform 	vec2 FiModifier;
uniform 	mat4 WndMatrix;
 in  vec4 in_quadCoord0;
vec4 Input0;
 in  vec4 in_Coords0;
vec4 Input1;
 in  vec4 in_Normals0;
vec4 Input2;
 in  vec4 in_Width0;
vec4 Input3;
#undef Output0
#define Output0 phase0_Output0
vec4 phase0_Output0;
 out  vec4 Color1;
#define Output1 Color1
vec4 Temp[2];
ivec4 Temp_int[2];
uvec4 Temp_uint[2];
void main()
{
    Input0 = in_quadCoord0;
    Input1 = in_Coords0;
    Input2 = in_Normals0;
    Input3 = in_Width0;
    Temp[0].xy = (-Input2.xy) + Input2.zw;
    Temp[0].xy = Input0.xx * Temp[0].xy + Input2.xy;
    Temp[0].xy = Temp[0].xy * Input0.yy;
    Temp[0].zw = (-Input1.xy) + Input1.zw;
    Temp[0].zw = Input0.xx * Temp[0].zw + Input1.xy;
    Temp[0].xy = Temp[0].xy * Input3.xx + Temp[0].zw;
    Temp[1] = Temp[0].yyyy * WndMatrix[1];
    Temp[0] = WndMatrix[0] * Temp[0].xxxx + Temp[1];
    Output0 = Temp[0] + WndMatrix[3];
    Output1 = vec4(intBitsToFloat(0x3F800000), intBitsToFloat(0x3F800000), intBitsToFloat(0x3F800000), intBitsToFloat(0x3F800000));
    gl_Position = vec4(phase0_Output0);
    return;
}
