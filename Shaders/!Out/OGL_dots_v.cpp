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
 in  vec4 in_quadCoord0;
vec4 Input0;
 in  vec4 in_vsCoord0;
vec4 Input1;
 in  vec4 in_vsColor1;
vec4 Input2;
#undef Output0
#define Output0 phase0_Output0
vec4 phase0_Output0;
 out  vec4 Color1;
#define Output1 Color1
 out  vec4 quadCoord0;
#define Output2 quadCoord0
vec4 Temp[2];
ivec4 Temp_int[2];
uvec4 Temp_uint[2];
void main()
{
    Input0 = in_quadCoord0;
    Input1 = in_vsCoord0;
    Input2 = in_vsColor1;
    Temp[0].x = Input1.z * FiModifier.xyxx.x;
    Temp[0].x = FiModifier.xyxx.y * Temp[0].x + Input1.z;
    Temp[1].x = cos(Temp[0].x);
    Temp[0].x = sin(Temp[0].x);
    Temp[1].y = Temp[0].x;
    Temp[0].xy = Rad * Temp[1].xy + Input0.xy;
    Temp[1] = Input1.yyyy * VP_Matrix[1];
    Temp[1] = VP_Matrix[0] * Input1.xxxx + Temp[1];
    Temp[1] = Temp[1] + VP_Matrix[3];
    Temp[0].zw = Temp[1].xy / Temp[1].ww;
    Temp[0].xy = PixelSize.xyxx.xy * Temp[0].xy + Temp[0].zw;
    Output0.xy = Temp[1].ww * Temp[0].xy;
    Output0.zw = Temp[1].zw;
    Output1 = Input2;
    Output2.xy = Input0.xy;
    gl_Position = vec4(phase0_Output0);
    return;
}
