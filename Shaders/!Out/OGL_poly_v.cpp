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
 in  vec4 in_vsCoord0;
vec4 Input0;
 in  vec4 in_vsColor1;
vec4 Input1;
#undef Output0
#define Output0 phase0_Output0
vec4 phase0_Output0;
 out  vec4 Color1;
#define Output1 Color1
vec4 Temp[1];
ivec4 Temp_int[1];
uvec4 Temp_uint[1];
void main()
{
    Input0 = in_vsCoord0;
    Input1 = in_vsColor1;
    Temp[0] = Input0.yyyy * VP_Matrix[1];
    Temp[0] = VP_Matrix[0] * Input0.xxxx + Temp[0];
    Temp[0] = VP_Matrix[2] * Input0.zzzz + Temp[0];
    Output0 = Temp[0] + VP_Matrix[3];
    Output1 = Input1;
    gl_Position = vec4(phase0_Output0);
    return;
}
