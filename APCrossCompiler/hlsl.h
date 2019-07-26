#ifndef HLSL_H
#define	HLSL_H

#define in
#define out
#define inout
#define globallycoherent
#define linear
#define centroid
#define nointerpolation
#define noperspective
#define sample
#define register(a)

#define SV_VertexID 1
#define SV_InstanceID 1
#define SV_PrimitiveID 1
#define SV_Position 1
#define SV_Target0 1
#define SV_Target1 1
#define SV_Target2 1
#define SV_Target3 1
#define SV_Depth 1
#define SV_RenderTargetArrayIndex 1
#define SV_ViewportArrayIndex 1
#define SV_DepthGreaterEqual 1
#define SV_DepthLessEqual 1
#define SV_ClipDistance0 1
#define SV_ClipDistance1 1
#define SV_ClipDistance2 1
#define SV_ClipDistance3 1
#define SV_CullDistance0 1
#define SV_CullDistance1 1
#define SV_CullDistance2 1
#define SV_CullDistance3 1
#define SV_DispatchThreadID 1
#define SV_DomainLocation 1
#define SV_GroupID 1
#define SV_GroupIndex 1
#define SV_GroupThreadID 1
#define SV_GSInstanceID 1
#define SV_Coverage 1
#define SV_InnerCoverage 1
#define SV_InsideTessFactor 1
#define SV_IsFrontFace 1
#define SV_OutputControlPointID 1
#define SV_SampleIndex 1
#define SV_StencilRef 1
#define SV_TessFactor 1

#define SamplerState int
typedef unsigned int uint;

struct int2;
struct int3;
struct int4;
struct uint2;
struct uint3;
struct uint4;

struct int2 {
    int x, y, r, g;
    int2 xx, xy, yx, yy;
    int3 xxx, xxy, xyx, xyy, yxx, yxy, yyx, yyy;
    int4 xxxx, xxxy, xxyx, xxyy, xyxx, xyxy, xyyx, xyyy, yxxx, yxxy, yxyx, yxyy, 
         yyxx, yyxy, yyyx, yyyy;
    int& operator[](uint idx);
};

struct uint2 {
    uint x, y, r, g;
    uint2 xx, xy, yx, yy;
    uint3 xxx, xxy, xyx, xyy, yxx, yxy, yyx, yyy;
    uint4 xxxx, xxxy, xxyx, xxyy, xyxx, xyxy, xyyx, xyyy, yxxx, yxxy, yxyx, yxyy, 
           yyxx, yyxy, yyyx, yyyy;
    uint& operator[](uint idx);
};

struct float2 {
    float x, y, r, g;
    float2 xx, xy, yx, yy;
    float3 xxx, xxy, xyx, xyy, yxx, yxy, yyx, yyy;
    float4 xxxx, xxxy, xxyx, xxyy, xyxx, xyxy, xyyx, xyyy, yxxx, yxxy, yxyx, yxyy, 
           yyxx, yyxy, yyyx, yyyy;
    float& operator[](uint idx);
};

struct int3 {
    int x, y, z, r, g, b;
    int2 xx, xy, xz, yx, yy, yz, zx, zy, zz;
    int3 xxx, xxy, xxz, xyx, xyy, xyz, xzx, xzy, xzz, yxx, yxy, yxz, yyx, yyy, 
           yyz, yzx, yzy, yzz, zxx, zxy, zxz, zyx, zyy, zyz, zzx, zzy, zzz;
    int4 xxxx, xxxy, xxxz, xxyx, xxyy, xxyz, xxzx, xxzy, xxzz, xyxx, xyxy, xyxz, 
           xyyx, xyyy, xyyz, xyzx, xyzy, xyzz, xzxx, xzxy, xzxz, xzyx, xzyy, xzyz, 
           xzzx, xzzy, xzzz, yxxx, yxxy, yxxz, yxyx, yxyy, yxyz, yxzx, yxzy, yxzz, 
           yyxx, yyxy, yyxz, yyyx, yyyy, yyyz, yyzx, yyzy, yyzz, yzxx, yzxy, yzxz, 
           yzyx, yzyy, yzyz, yzzx, yzzy, yzzz, zxxx, zxxy, zxxz, zxyx, zxyy, zxyz, 
           zxzx, zxzy, zxzz, zyxx, zyxy, zyxz, zyyx, zyyy, zyyz, zyzx, zyzy, zyzz, 
           zzxx, zzxy, zzxz, zzyx, zzyy, zzyz, zzzx, zzzy, zzzz;
    int& operator[](uint idx);
};

struct uint3 {
    uint x, y, z, r, g, b;
    uint2 xx, xy, xz, yx, yy, yz, zx, zy, zz;
    uint3 xxx, xxy, xxz, xyx, xyy, xyz, xzx, xzy, xzz, yxx, yxy, yxz, yyx, yyy, 
           yyz, yzx, yzy, yzz, zxx, zxy, zxz, zyx, zyy, zyz, zzx, zzy, zzz;
    uint4 xxxx, xxxy, xxxz, xxyx, xxyy, xxyz, xxzx, xxzy, xxzz, xyxx, xyxy, xyxz, 
           xyyx, xyyy, xyyz, xyzx, xyzy, xyzz, xzxx, xzxy, xzxz, xzyx, xzyy, xzyz, 
           xzzx, xzzy, xzzz, yxxx, yxxy, yxxz, yxyx, yxyy, yxyz, yxzx, yxzy, yxzz, 
           yyxx, yyxy, yyxz, yyyx, yyyy, yyyz, yyzx, yyzy, yyzz, yzxx, yzxy, yzxz, 
           yzyx, yzyy, yzyz, yzzx, yzzy, yzzz, zxxx, zxxy, zxxz, zxyx, zxyy, zxyz, 
           zxzx, zxzy, zxzz, zyxx, zyxy, zyxz, zyyx, zyyy, zyyz, zyzx, zyzy, zyzz, 
           zzxx, zzxy, zzxz, zzyx, zzyy, zzyz, zzzx, zzzy, zzzz;
    uint& operator[](uint idx);
};

struct float3 {
    float x, y, z, r, g, b;
    float2 xx, xy, xz, yx, yy, yz, zx, zy, zz;
    float3 xxx, xxy, xxz, xyx, xyy, xyz, xzx, xzy, xzz, yxx, yxy, yxz, yyx, yyy, 
           yyz, yzx, yzy, yzz, zxx, zxy, zxz, zyx, zyy, zyz, zzx, zzy, zzz;
    float4 xxxx, xxxy, xxxz, xxyx, xxyy, xxyz, xxzx, xxzy, xxzz, xyxx, xyxy, xyxz, 
           xyyx, xyyy, xyyz, xyzx, xyzy, xyzz, xzxx, xzxy, xzxz, xzyx, xzyy, xzyz, 
           xzzx, xzzy, xzzz, yxxx, yxxy, yxxz, yxyx, yxyy, yxyz, yxzx, yxzy, yxzz, 
           yyxx, yyxy, yyxz, yyyx, yyyy, yyyz, yyzx, yyzy, yyzz, yzxx, yzxy, yzxz, 
           yzyx, yzyy, yzyz, yzzx, yzzy, yzzz, zxxx, zxxy, zxxz, zxyx, zxyy, zxyz, 
           zxzx, zxzy, zxzz, zyxx, zyxy, zyxz, zyyx, zyyy, zyyz, zyzx, zyzy, zyzz, 
           zzxx, zzxy, zzxz, zzyx, zzyy, zzyz, zzzx, zzzy, zzzz;
    float& operator[](uint idx);
};

struct int4 {
    int    x, y, z, w, r, g, b, a;
    int2   xx, xy, xz, xw, yx, yy, yz, yw, zx, zy, zz, zw, wx, wy, wz, ww;
    int3   xxx, xxy, xxz, xxw, xyx, xyy, xyz, xyw, xzx, xzy, xzz, xzw, xwx, xwy, 
           xwz, xww, yxx, yxy, yxz, yxw, yyx, yyy, yyz, yyw, yzx, yzy, yzz, yzw, 
           ywx, ywy, ywz, yww, zxx, zxy, zxz, zxw, zyx, zyy, zyz, zyw, zzx, zzy, 
           zzz, zzw, zwx, zwy, zwz, zww, wxx, wxy, wxz, wxw, wyx, wyy, wyz, wyw, 
           wzx, wzy, wzz, wzw, wwx, wwy, wwz, www;
    int4   xxxx, xxxy, xxxz, xxxw, xxyx, xxyy, xxyz, xxyw, xxzx, xxzy, xxzz, xxzw, 
           xxwx, xxwy, xxwz, xxww, xyxx, xyxy, xyxz, xyxw, xyyx, xyyy, xyyz, xyyw, 
           xyzx, xyzy, xyzz, xyzw, xywx, xywy, xywz, xyww, xzxx, xzxy, xzxz, xzxw, 
           xzyx, xzyy, xzyz, xzyw, xzzx, xzzy, xzzz, xzzw, xzwx, xzwy, xzwz, xzww, 
           xwxx, xwxy, xwxz, xwxw, xwyx, xwyy, xwyz, xwyw, xwzx, xwzy, xwzz, xwzw, 
           xwwx, xwwy, xwwz, xwww, yxxx, yxxy, yxxz, yxxw, yxyx, yxyy, yxyz, yxyw, 
           yxzx, yxzy, yxzz, yxzw, yxwx, yxwy, yxwz, yxww, yyxx, yyxy, yyxz, yyxw, 
           yyyx, yyyy, yyyz, yyyw, yyzx, yyzy, yyzz, yyzw, yywx, yywy, yywz, yyww, 
           yzxx, yzxy, yzxz, yzxw, yzyx, yzyy, yzyz, yzyw, yzzx, yzzy, yzzz, yzzw, 
           yzwx, yzwy, yzwz, yzww, ywxx, ywxy, ywxz, ywxw, ywyx, ywyy, ywyz, ywyw, 
           ywzx, ywzy, ywzz, ywzw, ywwx, ywwy, ywwz, ywww, zxxx, zxxy, zxxz, zxxw, 
           zxyx, zxyy, zxyz, zxyw, zxzx, zxzy, zxzz, zxzw, zxwx, zxwy, zxwz, zxww, 
           zyxx, zyxy, zyxz, zyxw, zyyx, zyyy, zyyz, zyyw, zyzx, zyzy, zyzz, zyzw, 
           zywx, zywy, zywz, zyww, zzxx, zzxy, zzxz, zzxw, zzyx, zzyy, zzyz, zzyw, 
           zzzx, zzzy, zzzz, zzzw, zzwx, zzwy, zzwz, zzww, zwxx, zwxy, zwxz, zwxw, 
           zwyx, zwyy, zwyz, zwyw, zwzx, zwzy, zwzz, zwzw, zwwx, zwwy, zwwz, zwww, 
           wxxx, wxxy, wxxz, wxxw, wxyx, wxyy, wxyz, wxyw, wxzx, wxzy, wxzz, wxzw, 
           wxwx, wxwy, wxwz, wxww, wyxx, wyxy, wyxz, wyxw, wyyx, wyyy, wyyz, wyyw, 
           wyzx, wyzy, wyzz, wyzw, wywx, wywy, wywz, wyww, wzxx, wzxy, wzxz, wzxw, 
           wzyx, wzyy, wzyz, wzyw, wzzx, wzzy, wzzz, wzzw, wzwx, wzwy, wzwz, wzww, 
           wwxx, wwxy, wwxz, wwxw, wwyx, wwyy, wwyz, wwyw, wwzx, wwzy, wwzz, wwzw, 
           wwwx, wwwy, wwwz, wwww;
    int& operator[](uint idx);
};

struct uint4 {
    uint    x, y, z, w, r, g, b, a;
    uint2   xx, xy, xz, xw, yx, yy, yz, yw, zx, zy, zz, zw, wx, wy, wz, ww;
    uint3   xxx, xxy, xxz, xxw, xyx, xyy, xyz, xyw, xzx, xzy, xzz, xzw, xwx, xwy, 
           xwz, xww, yxx, yxy, yxz, yxw, yyx, yyy, yyz, yyw, yzx, yzy, yzz, yzw, 
           ywx, ywy, ywz, yww, zxx, zxy, zxz, zxw, zyx, zyy, zyz, zyw, zzx, zzy, 
           zzz, zzw, zwx, zwy, zwz, zww, wxx, wxy, wxz, wxw, wyx, wyy, wyz, wyw, 
           wzx, wzy, wzz, wzw, wwx, wwy, wwz, www;
    uint4   xxxx, xxxy, xxxz, xxxw, xxyx, xxyy, xxyz, xxyw, xxzx, xxzy, xxzz, xxzw, 
           xxwx, xxwy, xxwz, xxww, xyxx, xyxy, xyxz, xyxw, xyyx, xyyy, xyyz, xyyw, 
           xyzx, xyzy, xyzz, xyzw, xywx, xywy, xywz, xyww, xzxx, xzxy, xzxz, xzxw, 
           xzyx, xzyy, xzyz, xzyw, xzzx, xzzy, xzzz, xzzw, xzwx, xzwy, xzwz, xzww, 
           xwxx, xwxy, xwxz, xwxw, xwyx, xwyy, xwyz, xwyw, xwzx, xwzy, xwzz, xwzw, 
           xwwx, xwwy, xwwz, xwww, yxxx, yxxy, yxxz, yxxw, yxyx, yxyy, yxyz, yxyw, 
           yxzx, yxzy, yxzz, yxzw, yxwx, yxwy, yxwz, yxww, yyxx, yyxy, yyxz, yyxw, 
           yyyx, yyyy, yyyz, yyyw, yyzx, yyzy, yyzz, yyzw, yywx, yywy, yywz, yyww, 
           yzxx, yzxy, yzxz, yzxw, yzyx, yzyy, yzyz, yzyw, yzzx, yzzy, yzzz, yzzw, 
           yzwx, yzwy, yzwz, yzww, ywxx, ywxy, ywxz, ywxw, ywyx, ywyy, ywyz, ywyw, 
           ywzx, ywzy, ywzz, ywzw, ywwx, ywwy, ywwz, ywww, zxxx, zxxy, zxxz, zxxw, 
           zxyx, zxyy, zxyz, zxyw, zxzx, zxzy, zxzz, zxzw, zxwx, zxwy, zxwz, zxww, 
           zyxx, zyxy, zyxz, zyxw, zyyx, zyyy, zyyz, zyyw, zyzx, zyzy, zyzz, zyzw, 
           zywx, zywy, zywz, zyww, zzxx, zzxy, zzxz, zzxw, zzyx, zzyy, zzyz, zzyw, 
           zzzx, zzzy, zzzz, zzzw, zzwx, zzwy, zzwz, zzww, zwxx, zwxy, zwxz, zwxw, 
           zwyx, zwyy, zwyz, zwyw, zwzx, zwzy, zwzz, zwzw, zwwx, zwwy, zwwz, zwww, 
           wxxx, wxxy, wxxz, wxxw, wxyx, wxyy, wxyz, wxyw, wxzx, wxzy, wxzz, wxzw, 
           wxwx, wxwy, wxwz, wxww, wyxx, wyxy, wyxz, wyxw, wyyx, wyyy, wyyz, wyyw, 
           wyzx, wyzy, wyzz, wyzw, wywx, wywy, wywz, wyww, wzxx, wzxy, wzxz, wzxw, 
           wzyx, wzyy, wzyz, wzyw, wzzx, wzzy, wzzz, wzzw, wzwx, wzwy, wzwz, wzww, 
           wwxx, wwxy, wwxz, wwxw, wwyx, wwyy, wwyz, wwyw, wwzx, wwzy, wwzz, wwzw, 
           wwwx, wwwy, wwwz, wwww;
    uint& operator[](uint idx);
};

struct float4 {
    float x, y, z, w, r, g, b, a;
    float2 xx, xy, xz, xw, yx, yy, yz, yw, zx, zy, zz, zw, wx, wy, wz, ww;
    float3 xxx, xxy, xxz, xxw, xyx, xyy, xyz, xyw, xzx, xzy, xzz, xzw, xwx, xwy, 
           xwz, xww, yxx, yxy, yxz, yxw, yyx, yyy, yyz, yyw, yzx, yzy, yzz, yzw, 
           ywx, ywy, ywz, yww, zxx, zxy, zxz, zxw, zyx, zyy, zyz, zyw, zzx, zzy, 
           zzz, zzw, zwx, zwy, zwz, zww, wxx, wxy, wxz, wxw, wyx, wyy, wyz, wyw, 
           wzx, wzy, wzz, wzw, wwx, wwy, wwz, www;
    float4 xxxx, xxxy, xxxz, xxxw, xxyx, xxyy, xxyz, xxyw, xxzx, xxzy, xxzz, xxzw, 
           xxwx, xxwy, xxwz, xxww, xyxx, xyxy, xyxz, xyxw, xyyx, xyyy, xyyz, xyyw, 
           xyzx, xyzy, xyzz, xyzw, xywx, xywy, xywz, xyww, xzxx, xzxy, xzxz, xzxw, 
           xzyx, xzyy, xzyz, xzyw, xzzx, xzzy, xzzz, xzzw, xzwx, xzwy, xzwz, xzww, 
           xwxx, xwxy, xwxz, xwxw, xwyx, xwyy, xwyz, xwyw, xwzx, xwzy, xwzz, xwzw, 
           xwwx, xwwy, xwwz, xwww, yxxx, yxxy, yxxz, yxxw, yxyx, yxyy, yxyz, yxyw, 
           yxzx, yxzy, yxzz, yxzw, yxwx, yxwy, yxwz, yxww, yyxx, yyxy, yyxz, yyxw, 
           yyyx, yyyy, yyyz, yyyw, yyzx, yyzy, yyzz, yyzw, yywx, yywy, yywz, yyww, 
           yzxx, yzxy, yzxz, yzxw, yzyx, yzyy, yzyz, yzyw, yzzx, yzzy, yzzz, yzzw, 
           yzwx, yzwy, yzwz, yzww, ywxx, ywxy, ywxz, ywxw, ywyx, ywyy, ywyz, ywyw, 
           ywzx, ywzy, ywzz, ywzw, ywwx, ywwy, ywwz, ywww, zxxx, zxxy, zxxz, zxxw, 
           zxyx, zxyy, zxyz, zxyw, zxzx, zxzy, zxzz, zxzw, zxwx, zxwy, zxwz, zxww, 
           zyxx, zyxy, zyxz, zyxw, zyyx, zyyy, zyyz, zyyw, zyzx, zyzy, zyzz, zyzw, 
           zywx, zywy, zywz, zyww, zzxx, zzxy, zzxz, zzxw, zzyx, zzyy, zzyz, zzyw, 
           zzzx, zzzy, zzzz, zzzw, zzwx, zzwy, zzwz, zzww, zwxx, zwxy, zwxz, zwxw, 
           zwyx, zwyy, zwyz, zwyw, zwzx, zwzy, zwzz, zwzw, zwwx, zwwy, zwwz, zwww, 
           wxxx, wxxy, wxxz, wxxw, wxyx, wxyy, wxyz, wxyw, wxzx, wxzy, wxzz, wxzw, 
           wxwx, wxwy, wxwz, wxww, wyxx, wyxy, wyxz, wyxw, wyyx, wyyy, wyyz, wyyw, 
           wyzx, wyzy, wyzz, wyzw, wywx, wywy, wywz, wyww, wzxx, wzxy, wzxz, wzxw, 
           wzyx, wzyy, wzyz, wzyw, wzzx, wzzy, wzzz, wzzw, wzwx, wzwy, wzwz, wzww, 
           wwxx, wwxy, wwxz, wwxw, wwyx, wwyy, wwyz, wwyw, wwzx, wwzy, wwzz, wwzw, 
           wwwx, wwwy, wwwz, wwww;
    float& operator[](uint idx);
};

struct float2x2 {
    float2& operator[](uint idx);
};

struct float3x3 {
    float3& operator[](uint idx);
};

struct float4x4 {
    float4& operator[](uint idx);
};

struct Mips2D {
    float4 operator[][](in uint mip, in uint2 pos);
};
        
struct Texture2D{
    float4 Load(int3 pixelcoord);
    float4 Load(int3 pixelcoord, int2 offset);
    float4 Sample(SamplerState, float2 texcoord, int2 offset = {0,0}, float lodClamp = {1024});
    float4 Sample(SamplerState, float2 texcoord, int2 offset, float lodClamp, out uint status);
    float4 SampleLevel(SamplerState, float2 texcoord, int mipLevel, int2 offset = {0,0});
    float4 SampleLevel(SamplerState, float2 texcoord, int mipLevel, int2 offset, out uint status);
    float4 SampleGrad(SamplerState, float2 texcoord, float2 DDX, float2 DDY, int2 offset = {0,0}, float lodClamp = {1024});
    float4 SampleGrad(SamplerState, float2 texcoord, float2 DDX, float2 DDY, int2 offset, float lodClamp, out uint status);
    float4 SampleBias(SamplerState, float2 texcoord, float bias, int2 offset = {0,0}, float lodClamp = {1024});
    float4 SampleBias(SamplerState, float2 texcoord, float bias, int2 offset, float lodClamp, out uint status);
    float4 SampleCmp(SamplerState, float2 texcoord, float CompareValue, int2 offset = {0,0}, float lodClamp = {1024});
    float4 SampleCmp(SamplerState, float2 texcoord, float CompareValue, int2 offset, float lodClamp, out uint status);
    float4 SampleCmpLevelZero(SamplerState, float2 texcoord, float CompareValue, int2 offset = {0,0});
    float4 SampleCmpLevelZero(SamplerState, float2 texcoord, float CompareValue, int2 offset, out uint status);
    float4 Gather(SamplerState, float2 texcoord, int2 offset);
    float4 Gather(SamplerState, float2 texcoord, int2 offset, out uint status);
    float4 GatherRed(SamplerState, float2 texcoord, int2 offset);
    float4 GatherRed(SamplerState, float2 texcoord, int2 offset, out uint status);
    float4 GatherRed(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherRed(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherGreen(SamplerState, float2 texcoord, int2 offset);
    float4 GatherGreen(SamplerState, float2 texcoord, int2 offset, out uint status);
    float4 GatherGreen(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherGreen(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherBlue(SamplerState, float2 texcoord, int2 offset);
    float4 GatherBlue(SamplerState, float2 texcoord, int2 offset, out uint status);
    float4 GatherBlue(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherBlue(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherAlpha(SamplerState, float2 texcoord, int2 offset);
    float4 GatherAlpha(SamplerState, float2 texcoord, int2 offset, out uint status);
    float4 GatherAlpha(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherAlpha(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmp(SamplerState, float2 texcoord, float CompareValue, int2 offset);
    float4 GatherCmp(SamplerState, float2 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpRed(SamplerState, float2 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpRed(SamplerState, float2 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpRed(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpRed(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmpGreen(SamplerState, float2 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpGreen(SamplerState, float2 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpGreen(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpGreen(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmpBlue(SamplerState, float2 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpBlue(SamplerState, float2 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpBlue(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpBlue(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmpAlpha(SamplerState, float2 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpAlpha(SamplerState, float2 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpAlpha(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpAlpha(SamplerState, float2 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    void GetDimensions(int mipLevel, out float Width, out float Height, out float NumberOfLevels);
    void GetDimensions(out float Width, out float Height);
    void GetDimensions(int mipLevel, out uint Width, out uint Height, out uint NumberOfLevels);
    void GetDimensions(out uint Width, out uint Height);
    float4 operator[](in uint2 pos);
    Mips2D& mips();
};

struct Mips2DArray {
    float4 operator[][](in uint mip, in uint3 pos);
};

struct Texture2DArray{
    float4 Load(int4 pixelcoord);
    float4 Load(int4 pixelcoord, int2 offset);
    float4 Sample(SamplerState, float3 texcoord, int2 offset = {0,0}, float lodClamp = {1024});
    float4 Sample(SamplerState, float3 texcoord, int2 offset, float lodClamp, out uint status);
    float4 SampleLevel(SamplerState, float3 texcoord, int mipLevel, int2 offset = {0,0});
    float4 SampleLevel(SamplerState, float3 texcoord, int mipLevel, int2 offset, out uint status);
    float4 SampleGrad(SamplerState, float3 texcoord, float2 DDX, float2 DDY, int2 offset = {0,0}, float lodClamp = {1024});
    float4 SampleGrad(SamplerState, float3 texcoord, float2 DDX, float2 DDY, int2 offset, float lodClamp, out uint status);
    float4 SampleBias(SamplerState, float3 texcoord, float bias, int2 offset = {0,0}, float lodClamp = {1024});
    float4 SampleBias(SamplerState, float3 texcoord, float bias, int2 offset, float lodClamp, out uint status);
    float4 SampleCmp(SamplerState, float3 texcoord, float CompareValue, int2 offset = {0,0}, float lodClamp = {1024});
    float4 SampleCmp(SamplerState, float3 texcoord, float CompareValue, int2 offset, float lodClamp, out uint status);
    float4 SampleCmpLevelZero(SamplerState, float3 texcoord, float CompareValue, int2 offset = {0,0});
    float4 SampleCmpLevelZero(SamplerState, float3 texcoord, float CompareValue, int2 offset, out uint status);
    float4 Gather(SamplerState, float3 texcoord, int2 offset);
    float4 Gather(SamplerState, float3 texcoord, int2 offset, out uint status);
    float4 GatherRed(SamplerState, float3 texcoord, int2 offset);
    float4 GatherRed(SamplerState, float3 texcoord, int2 offset, out uint status);
    float4 GatherRed(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherRed(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherGreen(SamplerState, float3 texcoord, int2 offset);
    float4 GatherGreen(SamplerState, float3 texcoord, int2 offset, out uint status);
    float4 GatherGreen(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherGreen(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherBlue(SamplerState, float3 texcoord, int2 offset);
    float4 GatherBlue(SamplerState, float3 texcoord, int2 offset, out uint status);
    float4 GatherBlue(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherBlue(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherAlpha(SamplerState, float3 texcoord, int2 offset);
    float4 GatherAlpha(SamplerState, float3 texcoord, int2 offset, out uint status);
    float4 GatherAlpha(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherAlpha(SamplerState, float3 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmp(SamplerState, float3 texcoord, float CompareValue, int2 offset);
    float4 GatherCmp(SamplerState, float3 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpRed(SamplerState, float3 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpRed(SamplerState, float3 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpRed(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpRed(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmpGreen(SamplerState, float3 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpGreen(SamplerState, float3 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpGreen(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpGreen(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmpBlue(SamplerState, float3 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpBlue(SamplerState, float3 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpBlue(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpBlue(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    float4 GatherCmpAlpha(SamplerState, float3 texcoord, float CompareValue, int2 offset);
    float4 GatherCmpAlpha(SamplerState, float3 texcoord, float CompareValue, int2 offset, out uint status);
    float4 GatherCmpAlpha(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
    float4 GatherCmpAlpha(SamplerState, float3 texcoord, float CompareValue, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
    void GetDimensions(int mipLevel, out float Width, out float Height, out float Elements, out float NumberOfLevels);
    void GetDimensions(out float Width, out float Height, out float Elements);
    void GetDimensions(int mipLevel, out uint Width, out uint Height, out uint Elements, out uint NumberOfLevels);
    void GetDimensions(out uint Width, out uint Height, out uint Elements);
    float4 operator[](in uint3 pos);
    Mips2DArray& mips();
};

template<TAnyStruct> struct AppendStructuredBuffer{
    void Append(in TAnyStruct value);
    void GetDimensions(out uint numStructs, out uint stride);
};

struct RWResource {    
};

template<TAnyStruct> struct RWStructuredBuffer: public RWResource{
    uint IncrementCounter();
    uint DecrementCounter();
    void GetDimensions(out uint numStructs, out uint stride);
    TAnyStruct Load(int Location);
    TAnyStruct Load(int Location, out uint Status);
    TAnyStruct operator[](in uint pos);
};

template<TAnyStruct> struct StructuredBuffer{
    void GetDimensions(out uint numStructs, out uint stride);
    TAnyStruct Load(int Location);
    TAnyStruct Load(int Location, out uint Status);
    TAnyStruct operator[](in uint pos);
};

void discard;

template<gentype> gentype abs(gentype v);
template<gentype> gentype acos(gentype v);
template<gentype> gentype all(gentype v);
                  void    AllMemoryBarrier();
                  void    AllMemoryBarrierWithGroupSync();
template<gentype> gentype any(gentype v);
                  double  asdouble(uint lowbits, uint highbits);
template<gentype> gentype asfloat(gentype v);
template<gentype> gentype asin(gentype v);
template<gentype> gentype asint(gentype v);
template<gentype> gentype asuint(gentype v);
template<gentype> gentype atan(gentype v);
template<gentype> gentype atan2(gentype y, gentype x);
template<gentype> gentype ceil(gentype v);
                  bool    CheckAccessFullyMapped(uint status);
template<gentype> gentype clamp(gentype v, gentype minval, gentype maxval);
template<gentype> void    clip(gentype v);
template<gentype> gentype cos(gentype v);
template<gentype> gentype cosh(gentype v);
                  uint    countbits(uint v);
template<gentype> gentype cross(gentype v);
template<gentype> gentype ddx(gentype v);
template<gentype> gentype ddx_coarse(gentype v);
template<gentype> gentype ddx_fine(gentype v);
template<gentype> gentype ddy(gentype v);
template<gentype> gentype ddy_coarse(gentype v);
template<gentype> gentype ddy_fine(gentype v);
template<gentype> gentype degrees(gentype v);
template<gentype> float   determinant(gentype v);
                  void    DeviceMemoryBarrier();
                  void    DeviceMemoryBarrierWithGroupSync();
template<gentype> gentype distance(gentype x, gentype y);
template<gentype> float   dot(gentype v);
template<gentype> gentype exp(gentype v);
template<gentype> gentype exp2(gentype v);
template<gentype> gentype faceforward(gentype n, gentype i, gentype ng);
                  int     firstbithigh(int v);
                  int     firstbitlow(int v);
template<gentype> gentype floor(gentype v);
template<gentype> double  fma(double a, double b, double c);
template<gentype> gentype fmod(gentype x, gentype y);
template<gentype> gentype frac(gentype x);
template<gentype> gentype frexp(gentype x, gentype exp);
template<gentype> gentype fwidth(gentype v);
                  uint    GetRenderTargetSampleCount();
                  float2  GetRenderTargetSamplePosition(int Index);
                  void    GroupMemoryBarrier();
                  void    GroupMemoryBarrierWithGroupSync();
                  void    InterlockedAdd(uint dest, uint value, out uint original_value);
                  void    InterlockedAnd(uint dest, uint value, out uint original_value);
                  void    InterlockedCompareExchange(uint dest, uint compare_value, uint value, out uint original_value);
                  void    InterlockedCompareStore(uint dest, uint compare_value, uint value);
                  void    InterlockedExchange(uint dest, uint value, out uint original_value);
                  void    InterlockedMax(uint dest, uint value, out uint original_value);
                  void    InterlockedMin(uint dest, uint value, out uint original_value);
                  void    InterlockedOr(uint dest, uint value, out uint original_value);
                  void    InterlockedXor(uint dest, uint value, out uint original_value);
template<gentype> gentype isfinite(gentype x);
template<gentype> gentype isinf(gentype x);
template<gentype> gentype isnan(gentype x);
template<gentype> gentype ldexp(gentype x, gentype exp);
template<gentype> float   length(gentype x);
template<gentype> gentype lerp(gentype min, gentype max, gentype k);
template<gentype> gentype log(gentype x);
template<gentype> gentype log10(gentype x);
template<gentype> gentype log2(gentype x);
template<gentype> gentype mad(gentype m, gentype a, gentype b);
template<gentype> gentype max(gentype v);
template<gentype> gentype min(gentype v);
template<gentype> gentype modf(gentype v, out gentype ip);
template<gentype> gentype mul(gentype val1, gentype val2);
template<gentype> float   noise(gentype v);
template<gentype> gentype normalize(gentype v);
template<gentype> gentype pow(gentype v);
template<gentype> gentype radians(gentype v);
template<gentype> gentype rcp(gentype v);
template<gentype> gentype reflect(gentype i, gentype n);
template<gentype> gentype refract(gentype i, gentype n, float refidx);
                  uint    reversebits(uint i);
template<gentype> gentype round(gentype v);
template<gentype> gentype rsqrt(gentype v);
template<gentype> gentype saturate(gentype v);
template<gentype> gentype sign(gentype v);
template<gentype> gentype sin(gentype v);
template<gentype> void    sincos(gentype v, out gentype s, out gentype c);
template<gentype> gentype sinh(gentype v);
template<gentype> gentype smoothstep(gentype min, gentype max, gentype k);
template<gentype> gentype sqrt(gentype v);
template<gentype> gentype step(gentype y, gentype x);
template<gentype> gentype tan(gentype v);
template<gentype> gentype tanh(gentype v);
template<gentype> gentype transpose(gentype v);
template<gentype> gentype trunc(gentype v);

#endif	/* HLSL_H */

