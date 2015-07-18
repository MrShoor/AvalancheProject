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
layout(location = 0) out  vec4 SV_Target0;
#define Output0 SV_Target0
void main()
{
    Input1 = Color1;
    Output0 = Input1;
    return;
}
