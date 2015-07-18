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
uniform 	mat4 M_Matrix;
uniform 	mat4 MVP_Matrix;
uniform 	mat4 MV_Matrix;
uniform 	mat4 P_Matrix;
uniform 	mat4 M_InverseMatrix;
uniform 	mat4 MVP_InverseMatrix;
uniform 	mat4 MV_InverseMatrix;
uniform 	mat4 P_InverseMatrix;
uniform 	mat4 VP_Matrix;
uniform 	mat4 VP_InverseMatrix;
uniform 	mat4 V_Matrix;
uniform 	mat4 V_InverseMatrix;
 in  vec4 in_vsCoord0;
vec4 Input0;
 in  vec4 in_vsNormal0;
vec4 Input1;
#undef Output0
#define Output0 phase0_Output0
vec4 phase0_Output0;
 out  vec4 Normal0;
#define Output1 Normal0
vec4 Temp[1];
ivec4 Temp_int[1];
uvec4 Temp_uint[1];
void main()
{
    Input0 = in_vsCoord0;
    Input1 = in_vsNormal0;
    Temp[0] = Input0.yyyy * VP_Matrix[1];
    Temp[0] = VP_Matrix[0] * Input0.xxxx + Temp[0];
    Temp[0] = VP_Matrix[2] * Input0.zzzz + Temp[0];
    Output0 = Temp[0] + VP_Matrix[3];
    Temp[0].xyz = Input1.yyy * V_Matrix[1].xyz;
    Temp[0].xyz = V_Matrix[0].xyz * Input1.xxx + Temp[0].xyz;
    Output1.xyz = V_Matrix[2].xyz * Input1.zzz + Temp[0].xyz;
    gl_Position = vec4(phase0_Output0);
    return;
}
