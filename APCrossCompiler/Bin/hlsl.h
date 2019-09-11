#ifndef HLSL_H
#define	HLSL_H

#ifdef CppEditor
        #define S_(a) a
        #define S_VertexID(a) a
        #define S_InstanceID(a) a
        #define S_PrimitiveID(a) a
        #define S_Position(a) a
        #define S_Target0(a) a
        #define S_Target1(a) a
        #define S_Target2(a) a
        #define S_Target3(a) a
        #define S_Depth(a) a
        #define S_RenderTargetArrayIndex(a) a
        #define S_ViewportArrayIndex(a) a
        #define S_DepthGreaterEqual(a) a
        #define S_DepthLessEqual(a) a
        #define S_ClipDistance0(a) a
        #define S_ClipDistance1(a) a
        #define S_ClipDistance2(a) a
        #define S_ClipDistance3(a) a
        #define S_CullDistance0(a) a
        #define S_CullDistance1(a) a
        #define S_CullDistance2(a) a
        #define S_CullDistance3(a) a
        #define S_DispatchThreadID(a) a
        #define S_DomainLocation(a) a
        #define S_GroupID(a) a
        #define S_GroupIndex(a) a
        #define S_GroupThreadID(a) a
        #define S_GSInstanceID(a) a
        #define S_Coverage(a) a
        #define S_InnerCoverage(a) a
        #define S_InsideTessFactor(a) a
        #define S_IsFrontFace(a) a
        #define S_OutputControlPointID(a) a
        #define S_SampleIndex(a) a
        #define S_StencilRef(a) a
        #define S_TessFactor(a) a
        
        #define in
        #define out
        #define inout
        #define globallycoherent
        #define linear
        #define centroid
        #define nointerpolation
        #define noperspective
        #define register_(a)

        #define point
        #define line
        #define triangle
        #define lineadj
        #define triangleadj
        #define maxvertexcount(a) [noexcept]

        #define numthreads(a,b,c) [noexcept]

        #define unroll [noexcept]
        #define loop [noexcept]

        #define SamplerState int
        typedef unsigned int uint;

        struct int2;
        struct int3;
        struct int4;
        struct uint2;
        struct uint3;
        struct uint4;
        struct float2;
        struct float3;
        struct float4;

        struct int2 {
            int x, y, r, g;
            int2 &xx, &xy, &yx, &yy;
            int3 &xxx, &xxy, &xyx, &xyy, &yxx, &yxy, &yyx, &yyy;
            int4 &xxxx, &xxxy, &xxyx, &xxyy, &xyxx, &xyxy, &xyyx, &xyyy, &yxxx, &yxxy, &yxyx, &yxyy, 
                &yyxx, &yyxy, &yyyx, &yyyy;
            int& operator[](uint idx);

            int2(int x);
            int2(int x, int y);
        };

        struct uint2 {
            uint x, y, r, g;
            uint2 &xx, &xy, &yx, &yy;
            uint3 &xxx, &xxy, &xyx, &xyy, &yxx, &yxy, &yyx, &yyy;
            uint4 &xxxx, &xxxy, &xxyx, &xxyy, &xyxx, &xyxy, &xyyx, &xyyy, &yxxx, &yxxy, &yxyx, &yxyy, 
                &yyxx, &yyxy, &yyyx, &yyyy;
            uint& operator[](uint idx);

            uint2(uint x);
            uint2(uint x, uint y);
        };

        struct float2 {
            float x, y, r, g;
            float2 &xx, &xy, &yx, &yy;
            float3 &xxx, &xxy, &xyx, &xyy, &yxx, &yxy, &yyx, &yyy;
            float4 &xxxx, &xxxy, &xxyx, &xxyy, &xyxx, &xyxy, &xyyx, &xyyy, &yxxx, &yxxy, &yxyx, &yxyy, 
                &yyxx, &yyxy, &yyyx, &yyyy;
            float& operator[](uint idx);
            float2();
            float2(float x);
            float2(double x);
            float2(float x, float y);
            float2& operator = (float x);
            float2& operator = (const float2& xyzw);
            float2 operator - (const float2& v) const;
            float2 operator - (const int2& v) const;
            float2 operator - (const uint2& v) const;
            float2 operator - (float s) const;
            float2 operator + (const float2& v) const;
            float2 operator + (const int2& v) const;
            float2 operator + (const uint2& v) const;
            float2 operator + (float s) const;
            float2 operator / (const float2& v) const;
            float2 operator / (const int2& v) const;
            float2 operator / (const uint2& v) const;
            float2 operator / (float s) const;
            float2 operator * (const float2& v) const;
            float2 operator * (const int2& v) const;
            float2 operator * (const uint2& v) const;
            float2 operator * (float s) const;
            float2 operator -= (const float2& v) const;
            float2 operator -= (const int2& v) const;
            float2 operator -= (const uint2& v) const;
            float2 operator -= (float s) const;
            float2 operator += (const float2& v) const;
            float2 operator += (const int2& v) const;
            float2 operator += (const uint2& v) const;
            float2 operator += (float s) const;
            float2 operator /= (const float2& v) const;
            float2 operator /= (const int2& v) const;
            float2 operator /= (const uint2& v) const;
            float2 operator /= (float s) const;
            float2 operator *= (const float2& v) const;
            float2 operator *= (const int2& v) const;
            float2 operator *= (const uint2& v) const;
            float2 operator *= (float s) const;
            float2 operator - ();
            operator int2();
            operator uint2();
            operator float();
        };
        float2 operator * (float s, const float2& v);
        float2 operator + (float s, const float2& v);
        float2 operator / (float s, const float2& v);
        float2 operator - (float s, const float2& v);

        struct int3 {
            int x, y, z, r, g, b;
            int2 &xx, &xy, &xz, &yx, &yy, &yz, &zx, &zy, &zz;
            int3 &xxx, &xxy, &xxz, &xyx, &xyy, &xyz, &xzx, &xzy, &xzz, &yxx, &yxy, &yxz, &yyx, &yyy, 
                &yyz, &yzx, &yzy, &yzz, &zxx, &zxy, &zxz, &zyx, &zyy, &zyz, &zzx, &zzy, &zzz;
            int4 &xxxx, &xxxy, &xxxz, &xxyx, &xxyy, &xxyz, &xxzx, &xxzy, &xxzz, &xyxx, &xyxy, &xyxz, 
                &xyyx, &xyyy, &xyyz, &xyzx, &xyzy, &xyzz, &xzxx, &xzxy, &xzxz, &xzyx, &xzyy, &xzyz,
                &xzzx, &xzzy, &xzzz, &yxxx, &yxxy, &yxxz, &yxyx, &yxyy, &yxyz, &yxzx, &yxzy, &yxzz, 
                &yyxx, &yyxy, &yyxz, &yyyx, &yyyy, &yyyz, &yyzx, &yyzy, &yyzz, &yzxx, &yzxy, &yzxz, 
                &yzyx, &yzyy, &yzyz, &yzzx, &yzzy, &yzzz, &zxxx, &zxxy, &zxxz, &zxyx, &zxyy, &zxyz, 
                &zxzx, &zxzy, &zxzz, &zyxx, &zyxy, &zyxz, &zyyx, &zyyy, &zyyz, &zyzx, &zyzy, &zyzz, 
                &zzxx, &zzxy, &zzxz, &zzyx, &zzyy, &zzyz, &zzzx, &zzzy, &zzzz;
            int& operator[](uint idx);
            int3();
            int3(float x);
            int3(float x, float y, float z);
            int3(const float2& xy, float z);
            int3(float x, const float2& yz);
            int3(const int3& xyz);
            int3& operator = (float x);
            int3& operator = (const int3& xyz);
            int3 operator - (int3 v);
            int3 operator - (float s);
            int3 operator + (int3 v);
            int3 operator + (float s);
            int3 operator / (int3 v);
            int3 operator / (float s);
            int3 operator * (int3 v);
            int3 operator * (float s);
            int3 operator -= (int3 v);
            int3 operator -= (float s);
            int3 operator += (int3 v);
            int3 operator += (float s);
            int3 operator /= (int3 v);
            int3 operator /= (float s);
            int3 operator *= (int3 v);
            int3 operator *= (float s);
            int3 operator - ();
        };

        struct uint3 {
            uint x, y, z, r, g, b;
            uint2 &xx, &xy, &xz, &yx, &yy, &yz, &zx, &zy, &zz;
            uint3 &xxx, &xxy, &xxz, &xyx, &xyy, &xyz, &xzx, &xzy, &xzz, &yxx, &yxy, &yxz, &yyx, &yyy, 
                &yyz, &yzx, &yzy, &yzz, &zxx, &zxy, &zxz, &zyx, &zyy, &zyz, &zzx, &zzy, &zzz;
            uint4 &xxxx, &xxxy, &xxxz, &xxyx, &xxyy, &xxyz, &xxzx, &xxzy, &xxzz, &xyxx, &xyxy, &xyxz, 
                &xyyx, &xyyy, &xyyz, &xyzx, &xyzy, &xyzz, &xzxx, &xzxy, &xzxz, &xzyx, &xzyy, &xzyz,
                &xzzx, &xzzy, &xzzz, &yxxx, &yxxy, &yxxz, &yxyx, &yxyy, &yxyz, &yxzx, &yxzy, &yxzz, 
                &yyxx, &yyxy, &yyxz, &yyyx, &yyyy, &yyyz, &yyzx, &yyzy, &yyzz, &yzxx, &yzxy, &yzxz, 
                &yzyx, &yzyy, &yzyz, &yzzx, &yzzy, &yzzz, &zxxx, &zxxy, &zxxz, &zxyx, &zxyy, &zxyz, 
                &zxzx, &zxzy, &zxzz, &zyxx, &zyxy, &zyxz, &zyyx, &zyyy, &zyyz, &zyzx, &zyzy, &zyzz, 
                &zzxx, &zzxy, &zzxz, &zzyx, &zzyy, &zzyz, &zzzx, &zzzy, &zzzz;
            uint& operator[](uint idx);
            uint3();
            uint3(float x);
            uint3(float x, float y, float z);
            uint3(const float2& xy, float z);
            uint3(float x, const float2& yz);
            uint3(const uint3& xyz);
            uint3(const int3& xyz);
            uint3& operator = (float x);
            uint3& operator = (const uint3& xyz);
            uint3& operator = (const int3& xyz);
            uint3 operator - (uint3 v);
            uint3 operator - (float s);
            uint3 operator + (uint3 v);
            uint3 operator + (float s);
            uint3 operator / (uint3 v);
            uint3 operator / (float s);
            uint3 operator * (uint3 v);
            uint3 operator * (float s);
            uint3 operator -= (uint3 v);
            uint3 operator -= (float s);
            uint3 operator += (uint3 v);
            uint3 operator += (float s);
            uint3 operator /= (uint3 v);
            uint3 operator /= (float s);
            uint3 operator *= (uint3 v);
            uint3 operator *= (float s);
            uint3 operator - ();
        };

        struct float3 {
            float x, y, z, r, g, b;
            float2 &xx, &xy, &xz, &yx, &yy, &yz, &zx, &zy, &zz;
            float3 &xxx, &xxy, &xxz, &xyx, &xyy, &xyz, &xzx, &xzy, &xzz, &yxx, &yxy, &yxz, &yyx, &yyy, 
                &yyz, &yzx, &yzy, &yzz, &zxx, &zxy, &zxz, &zyx, &zyy, &zyz, &zzx, &zzy, &zzz;
            float4 &xxxx, &xxxy, &xxxz, &xxyx, &xxyy, &xxyz, &xxzx, &xxzy, &xxzz, &xyxx, &xyxy, &xyxz, 
                &xyyx, &xyyy, &xyyz, &xyzx, &xyzy, &xyzz, &xzxx, &xzxy, &xzxz, &xzyx, &xzyy, &xzyz,
                &xzzx, &xzzy, &xzzz, &yxxx, &yxxy, &yxxz, &yxyx, &yxyy, &yxyz, &yxzx, &yxzy, &yxzz, 
                &yyxx, &yyxy, &yyxz, &yyyx, &yyyy, &yyyz, &yyzx, &yyzy, &yyzz, &yzxx, &yzxy, &yzxz, 
                &yzyx, &yzyy, &yzyz, &yzzx, &yzzy, &yzzz, &zxxx, &zxxy, &zxxz, &zxyx, &zxyy, &zxyz, 
                &zxzx, &zxzy, &zxzz, &zyxx, &zyxy, &zyxz, &zyyx, &zyyy, &zyyz, &zyzx, &zyzy, &zyzz, 
                &zzxx, &zzxy, &zzxz, &zzyx, &zzyy, &zzyz, &zzzx, &zzzy, &zzzz;
            float& operator[](uint idx);
            float3();
            float3(float x);
            float3(double x);
            float3(float x, float y, float z);
            float3(const float2& xy, float z);
            float3(float x, const float2& yz);
            float3(const float3& xyz);
            float3& operator = (float x);
            float3& operator = (const float3& xyz);
            float3 operator - (float3 v);
            float3 operator - (int3 v);
            float3 operator - (uint3 v);
            float3 operator - (float s);
            float3 operator + (float3 v);
            float3 operator + (int3 v);
            float3 operator + (uint3 v);
            float3 operator + (float s);
            float3 operator / (float3 v);
            float3 operator / (int3 v);
            float3 operator / (uint3 v);
            float3 operator / (float s);
            float3 operator * (float3 v);
            float3 operator * (int3 v);
            float3 operator * (uint3 v);
            float3 operator * (float s);
            float3 operator -= (float3 v);
            float3 operator -= (int3 v);
            float3 operator -= (uint3 v);
            float3 operator -= (float s);
            float3 operator += (float3 v);
            float3 operator += (int3 v);
            float3 operator += (uint3 v);
            float3 operator += (float s);
            float3 operator /= (float3 v);
            float3 operator /= (int3 v);
            float3 operator /= (uint3 v);
            float3 operator /= (float s);
            float3 operator *= (float3 v);
            float3 operator *= (int3 v);
            float3 operator *= (uint3 v);
            float3 operator *= (float s);
            float3 operator - ();
            operator int2();
            operator uint2();
        };
        float3 operator * (float s, float3 v);
        float3 operator * (int s, float3 v);
        float3 operator + (float s, float3 v);
        float3 operator + (int s, float3 v);
        float3 operator / (float s, float3 v);
        float3 operator / (int s, float3 v);
        float3 operator - (float s, float3 v);
        float3 operator - (int s, float3 v);

        struct int4 {
            int    x, y, z, w, r, g, b, a;
            int2   &xx, &xy, &xz, &xw, &yx, &yy, &yz, &yw, &zx, &zy, &zz, &zw, &wx, &wy, &wz, &ww;
            int3   &xxx, &xxy, &xxz, &xxw, &xyx, &xyy, &xyz, &xyw, &xzx, &xzy, &xzz, &xzw, &xwx, &xwy, 
                &xwz, &xww, &yxx, &yxy, &yxz, &yxw, &yyx, &yyy, &yyz, &yyw, &yzx, &yzy, &yzz, &yzw, 
                &ywx, &ywy, &ywz, &yww, &zxx, &zxy, &zxz, &zxw, &zyx, &zyy, &zyz, &zyw, &zzx, &zzy, 
                &zzz, &zzw, &zwx, &zwy, &zwz, &zww, &wxx, &wxy, &wxz, &wxw, &wyx, &wyy, &wyz, &wyw, 
                &wzx, &wzy, &wzz, &wzw, &wwx, &wwy, &wwz, &www;
            int4   &xxxx, &xxxy, &xxxz, &xxxw, &xxyx, &xxyy, &xxyz, &xxyw, &xxzx, &xxzy, &xxzz, &xxzw,
                &xxwx, &xxwy, &xxwz, &xxww, &xyxx, &xyxy, &xyxz, &xyxw, &xyyx, &xyyy, &xyyz, &xyyw,
                &xyzx, &xyzy, &xyzz, &xyzw, &xywx, &xywy, &xywz, &xyww, &xzxx, &xzxy, &xzxz, &xzxw,
                &xzyx, &xzyy, &xzyz, &xzyw, &xzzx, &xzzy, &xzzz, &xzzw, &xzwx, &xzwy, &xzwz, &xzww,
                &xwxx, &xwxy, &xwxz, &xwxw, &xwyx, &xwyy, &xwyz, &xwyw, &xwzx, &xwzy, &xwzz, &xwzw,
                &xwwx, &xwwy, &xwwz, &xwww, &yxxx, &yxxy, &yxxz, &yxxw, &yxyx, &yxyy, &yxyz, &yxyw,
                &yxzx, &yxzy, &yxzz, &yxzw, &yxwx, &yxwy, &yxwz, &yxww, &yyxx, &yyxy, &yyxz, &yyxw,
                &yyyx, &yyyy, &yyyz, &yyyw, &yyzx, &yyzy, &yyzz, &yyzw, &yywx, &yywy, &yywz, &yyww,
                &yzxx, &yzxy, &yzxz, &yzxw, &yzyx, &yzyy, &yzyz, &yzyw, &yzzx, &yzzy, &yzzz, &yzzw,
                &yzwx, &yzwy, &yzwz, &yzww, &ywxx, &ywxy, &ywxz, &ywxw, &ywyx, &ywyy, &ywyz, &ywyw,
                &ywzx, &ywzy, &ywzz, &ywzw, &ywwx, &ywwy, &ywwz, &ywww, &zxxx, &zxxy, &zxxz, &zxxw,
                &zxyx, &zxyy, &zxyz, &zxyw, &zxzx, &zxzy, &zxzz, &zxzw, &zxwx, &zxwy, &zxwz, &zxww,
                &zyxx, &zyxy, &zyxz, &zyxw, &zyyx, &zyyy, &zyyz, &zyyw, &zyzx, &zyzy, &zyzz, &zyzw,
                &zywx, &zywy, &zywz, &zyww, &zzxx, &zzxy, &zzxz, &zzxw, &zzyx, &zzyy, &zzyz, &zzyw,
                &zzzx, &zzzy, &zzzz, &zzzw, &zzwx, &zzwy, &zzwz, &zzww, &zwxx, &zwxy, &zwxz, &zwxw,
                &zwyx, &zwyy, &zwyz, &zwyw, &zwzx, &zwzy, &zwzz, &zwzw, &zwwx, &zwwy, &zwwz, &zwww,
                &wxxx, &wxxy, &wxxz, &wxxw, &wxyx, &wxyy, &wxyz, &wxyw, &wxzx, &wxzy, &wxzz, &wxzw,
                &wxwx, &wxwy, &wxwz, &wxww, &wyxx, &wyxy, &wyxz, &wyxw, &wyyx, &wyyy, &wyyz, &wyyw,
                &wyzx, &wyzy, &wyzz, &wyzw, &wywx, &wywy, &wywz, &wyww, &wzxx, &wzxy, &wzxz, &wzxw,
                &wzyx, &wzyy, &wzyz, &wzyw, &wzzx, &wzzy, &wzzz, &wzzw, &wzwx, &wzwy, &wzwz, &wzww,
                &wwxx, &wwxy, &wwxz, &wwxw, &wwyx, &wwyy, &wwyz, &wwyw, &wwzx, &wwzy, &wwzz, &wwzw,
                &wwwx, &wwwy, &wwwz, &wwww;
            int& operator[](uint idx);
        };

        struct uint4 {
            uint    x, y, z, w, r, g, b, a;
            uint2  &xx, &xy, &xz, &xw, &yx, &yy, &yz, &yw, &zx, &zy, &zz, &zw, &wx, &wy, &wz, &ww;
            uint3  &xxx, &xxy, &xxz, &xxw, &xyx, &xyy, &xyz, &xyw, &xzx, &xzy, &xzz, &xzw, &xwx, &xwy, 
                &xwz, &xww, &yxx, &yxy, &yxz, &yxw, &yyx, &yyy, &yyz, &yyw, &yzx, &yzy, &yzz, &yzw, 
                &ywx, &ywy, &ywz, &yww, &zxx, &zxy, &zxz, &zxw, &zyx, &zyy, &zyz, &zyw, &zzx, &zzy, 
                &zzz, &zzw, &zwx, &zwy, &zwz, &zww, &wxx, &wxy, &wxz, &wxw, &wyx, &wyy, &wyz, &wyw, 
                &wzx, &wzy, &wzz, &wzw, &wwx, &wwy, &wwz, &www;
            uint4  &xxxx, &xxxy, &xxxz, &xxxw, &xxyx, &xxyy, &xxyz, &xxyw, &xxzx, &xxzy, &xxzz, &xxzw,
                &xxwx, &xxwy, &xxwz, &xxww, &xyxx, &xyxy, &xyxz, &xyxw, &xyyx, &xyyy, &xyyz, &xyyw,
                &xyzx, &xyzy, &xyzz, &xyzw, &xywx, &xywy, &xywz, &xyww, &xzxx, &xzxy, &xzxz, &xzxw,
                &xzyx, &xzyy, &xzyz, &xzyw, &xzzx, &xzzy, &xzzz, &xzzw, &xzwx, &xzwy, &xzwz, &xzww,
                &xwxx, &xwxy, &xwxz, &xwxw, &xwyx, &xwyy, &xwyz, &xwyw, &xwzx, &xwzy, &xwzz, &xwzw,
                &xwwx, &xwwy, &xwwz, &xwww, &yxxx, &yxxy, &yxxz, &yxxw, &yxyx, &yxyy, &yxyz, &yxyw,
                &yxzx, &yxzy, &yxzz, &yxzw, &yxwx, &yxwy, &yxwz, &yxww, &yyxx, &yyxy, &yyxz, &yyxw,
                &yyyx, &yyyy, &yyyz, &yyyw, &yyzx, &yyzy, &yyzz, &yyzw, &yywx, &yywy, &yywz, &yyww,
                &yzxx, &yzxy, &yzxz, &yzxw, &yzyx, &yzyy, &yzyz, &yzyw, &yzzx, &yzzy, &yzzz, &yzzw,
                &yzwx, &yzwy, &yzwz, &yzww, &ywxx, &ywxy, &ywxz, &ywxw, &ywyx, &ywyy, &ywyz, &ywyw,
                &ywzx, &ywzy, &ywzz, &ywzw, &ywwx, &ywwy, &ywwz, &ywww, &zxxx, &zxxy, &zxxz, &zxxw,
                &zxyx, &zxyy, &zxyz, &zxyw, &zxzx, &zxzy, &zxzz, &zxzw, &zxwx, &zxwy, &zxwz, &zxww,
                &zyxx, &zyxy, &zyxz, &zyxw, &zyyx, &zyyy, &zyyz, &zyyw, &zyzx, &zyzy, &zyzz, &zyzw,
                &zywx, &zywy, &zywz, &zyww, &zzxx, &zzxy, &zzxz, &zzxw, &zzyx, &zzyy, &zzyz, &zzyw,
                &zzzx, &zzzy, &zzzz, &zzzw, &zzwx, &zzwy, &zzwz, &zzww, &zwxx, &zwxy, &zwxz, &zwxw,
                &zwyx, &zwyy, &zwyz, &zwyw, &zwzx, &zwzy, &zwzz, &zwzw, &zwwx, &zwwy, &zwwz, &zwww,
                &wxxx, &wxxy, &wxxz, &wxxw, &wxyx, &wxyy, &wxyz, &wxyw, &wxzx, &wxzy, &wxzz, &wxzw,
                &wxwx, &wxwy, &wxwz, &wxww, &wyxx, &wyxy, &wyxz, &wyxw, &wyyx, &wyyy, &wyyz, &wyyw,
                &wyzx, &wyzy, &wyzz, &wyzw, &wywx, &wywy, &wywz, &wyww, &wzxx, &wzxy, &wzxz, &wzxw,
                &wzyx, &wzyy, &wzyz, &wzyw, &wzzx, &wzzy, &wzzz, &wzzw, &wzwx, &wzwy, &wzwz, &wzww,
                &wwxx, &wwxy, &wwxz, &wwxw, &wwyx, &wwyy, &wwyz, &wwyw, &wwzx, &wwzy, &wwzz, &wwzw,
                &wwwx, &wwwy, &wwwz, &wwww;
            uint& operator[](uint idx);
        };

        struct float4 {
            float x, y, z, w, r, g, b, a;
            float2 &xx, &xy, &xz, &xw, &yx, &yy, &yz, &yw, &zx, &zy, &zz, &zw, &wx, &wy, &wz, &ww;
            float3 &xxx, &xxy, &xxz, &xxw, &xyx, &xyy, &xyz, &xyw, &xzx, &xzy, &xzz, &xzw, &xwx, &xwy, 
                &xwz, &xww, &yxx, &yxy, &yxz, &yxw, &yyx, &yyy, &yyz, &yyw, &yzx, &yzy, &yzz, &yzw, 
                &ywx, &ywy, &ywz, &yww, &zxx, &zxy, &zxz, &zxw, &zyx, &zyy, &zyz, &zyw, &zzx, &zzy, 
                &zzz, &zzw, &zwx, &zwy, &zwz, &zww, &wxx, &wxy, &wxz, &wxw, &wyx, &wyy, &wyz, &wyw, 
                &wzx, &wzy, &wzz, &wzw, &wwx, &wwy, &wwz, &www;
            float4 &xxxx, &xxxy, &xxxz, &xxxw, &xxyx, &xxyy, &xxyz, &xxyw, &xxzx, &xxzy, &xxzz, &xxzw,
                &xxwx, &xxwy, &xxwz, &xxww, &xyxx, &xyxy, &xyxz, &xyxw, &xyyx, &xyyy, &xyyz, &xyyw,
                &xyzx, &xyzy, &xyzz, &xyzw, &xywx, &xywy, &xywz, &xyww, &xzxx, &xzxy, &xzxz, &xzxw,
                &xzyx, &xzyy, &xzyz, &xzyw, &xzzx, &xzzy, &xzzz, &xzzw, &xzwx, &xzwy, &xzwz, &xzww,
                &xwxx, &xwxy, &xwxz, &xwxw, &xwyx, &xwyy, &xwyz, &xwyw, &xwzx, &xwzy, &xwzz, &xwzw,
                &xwwx, &xwwy, &xwwz, &xwww, &yxxx, &yxxy, &yxxz, &yxxw, &yxyx, &yxyy, &yxyz, &yxyw,
                &yxzx, &yxzy, &yxzz, &yxzw, &yxwx, &yxwy, &yxwz, &yxww, &yyxx, &yyxy, &yyxz, &yyxw,
                &yyyx, &yyyy, &yyyz, &yyyw, &yyzx, &yyzy, &yyzz, &yyzw, &yywx, &yywy, &yywz, &yyww,
                &yzxx, &yzxy, &yzxz, &yzxw, &yzyx, &yzyy, &yzyz, &yzyw, &yzzx, &yzzy, &yzzz, &yzzw,
                &yzwx, &yzwy, &yzwz, &yzww, &ywxx, &ywxy, &ywxz, &ywxw, &ywyx, &ywyy, &ywyz, &ywyw,
                &ywzx, &ywzy, &ywzz, &ywzw, &ywwx, &ywwy, &ywwz, &ywww, &zxxx, &zxxy, &zxxz, &zxxw,
                &zxyx, &zxyy, &zxyz, &zxyw, &zxzx, &zxzy, &zxzz, &zxzw, &zxwx, &zxwy, &zxwz, &zxww,
                &zyxx, &zyxy, &zyxz, &zyxw, &zyyx, &zyyy, &zyyz, &zyyw, &zyzx, &zyzy, &zyzz, &zyzw,
                &zywx, &zywy, &zywz, &zyww, &zzxx, &zzxy, &zzxz, &zzxw, &zzyx, &zzyy, &zzyz, &zzyw,
                &zzzx, &zzzy, &zzzz, &zzzw, &zzwx, &zzwy, &zzwz, &zzww, &zwxx, &zwxy, &zwxz, &zwxw,
                &zwyx, &zwyy, &zwyz, &zwyw, &zwzx, &zwzy, &zwzz, &zwzw, &zwwx, &zwwy, &zwwz, &zwww,
                &wxxx, &wxxy, &wxxz, &wxxw, &wxyx, &wxyy, &wxyz, &wxyw, &wxzx, &wxzy, &wxzz, &wxzw,
                &wxwx, &wxwy, &wxwz, &wxww, &wyxx, &wyxy, &wyxz, &wyxw, &wyyx, &wyyy, &wyyz, &wyyw,
                &wyzx, &wyzy, &wyzz, &wyzw, &wywx, &wywy, &wywz, &wyww, &wzxx, &wzxy, &wzxz, &wzxw,
                &wzyx, &wzyy, &wzyz, &wzyw, &wzzx, &wzzy, &wzzz, &wzzw, &wzwx, &wzwy, &wzwz, &wzww,
                &wwxx, &wwxy, &wwxz, &wwxw, &wwyx, &wwyy, &wwyz, &wwyw, &wwzx, &wwzy, &wwzz, &wwzw,
                &wwwx, &wwwy, &wwwz, &wwww;
            float& operator[](uint idx);
            float4();
            float4(float x);
            float4(double x);
            float4(float x, float y, float z, float w);
            float4(const float2& xy, float z, float w);
            float4(float x, const float2& yz, float w);
            float4(float x, float y, const float2& zw);
            float4(const float3& xyz, float w);
            float4(float x, const float3& yzw);
            float4(const float4& xyzw);
            float4& operator = (float x);
            float4& operator = (const float4& xyzw);
            float4 operator - (float4 v);
            float4 operator - (int4 v);
            float4 operator - (uint4 v);
            float4 operator - (float s);
            float4 operator + (float4 v);
            float4 operator + (int4 v);
            float4 operator + (uint4 v);
            float4 operator + (float s);
            float4 operator / (float4 v);
            float4 operator / (int4 v);
            float4 operator / (uint4 v);
            float4 operator / (float s);
            float4 operator * (float4 v);
            float4 operator * (int4 v);
            float4 operator * (uint4 v);
            float4 operator * (float s);
            float4 operator -= (float4 v);
            float4 operator -= (int4 v);
            float4 operator -= (uint4 v);
            float4 operator -= (float s);
            float4 operator += (float4 v);
            float4 operator += (int4 v);
            float4 operator += (uint4 v);
            float4 operator += (float s);
            float4 operator /= (float4 v);
            float4 operator /= (int4 v);
            float4 operator /= (uint4 v);
            float4 operator /= (float s);
            float4 operator *= (float4 v);
            float4 operator *= (int4 v);
            float4 operator *= (uint4 v);
            float4 operator *= (float s);
            float4 operator - ();
            operator int4();
            operator uint4();
        };
        float4 operator * (float s, float4 v);
        float4 operator * (int s, float4 v);
        float4 operator + (float s, float4 v);
        float4 operator + (int s, float4 v);
        float4 operator / (float s, float4 v);
        float4 operator / (int s, float4 v);
        float4 operator - (float s, float4 v);
        float4 operator - (int s, float4 v);

        struct float2x2 {
            float2& operator[](uint idx);
        };

        struct float3x3 {
            float3& operator[](uint idx);
            float3x3();
            float3x3(float m00, float m01, float m02,
                     float m10, float m11, float m12,
                     float m20, float m21, float m22);
            float3x3(float3 row1, float3 row2, float3 row3);
            float3x3 operator - (float3x3 v);
            float3x3 operator - (float s);
            float3x3 operator + (float3x3 v);
            float3x3 operator + (float s);
            float3x3 operator / (float3x3 v);
            float3x3 operator / (float s);
            float3x3 operator * (float3x3 v);
            float3x3 operator * (float s);
            float3x3 operator -= (float3x3 v);
            float3x3 operator -= (float s);
            float3x3 operator += (float3x3 v);
            float3x3 operator += (float s);
            float3x3 operator /= (float3x3 v);
            float3x3 operator /= (float s);
            float3x3 operator *= (float3x3 v);
            float3x3 operator *= (float s);
            float3x3 operator - ();
        };

        struct float4x4 {
            float4& operator[](uint idx);
            float4x4();
            float4x4(float m00, float m01, float m02, float m03,
                     float m10, float m11, float m12, float m13,
                     float m20, float m21, float m22, float m23,
                     float m30, float m31, float m32, float m33);
            float4x4 operator - (float4x4 v);
            float4x4 operator - (float s);
            float4x4 operator + (float4x4 v);
            float4x4 operator + (float s);
            float4x4 operator / (float4x4 v);
            float4x4 operator / (float s);
            float4x4 operator * (float4x4 v);
            float4x4 operator * (float s);
            float4x4 operator -= (float4x4 v);
            float4x4 operator -= (float s);
            float4x4 operator += (float4x4 v);
            float4x4 operator += (float s);
            float4x4 operator /= (float4x4 v);
            float4x4 operator /= (float s);
            float4x4 operator *= (float4x4 v);
            float4x4 operator *= (float s);
            float4x4 operator - ();
            operator float3x3();
        };
        float4x4 operator * (float s, float4x4 v);
        float4x4 operator + (float s, float4x4 v);
        float4x4 operator / (float s, float4x4 v);
        float4x4 operator - (float s, float4x4 v);

        struct Mips2D {
            struct Mip2D {
                float4 operator[](in uint2 pos);
            };
            Mip2D operator[](in uint mip);
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
            float4 GatherRed(SamplerState, float2 texcoord, int2 offset = {0,0});
            float4 GatherRed(SamplerState, float2 texcoord, int2 offset, out uint status);
            float4 GatherRed(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
            float4 GatherRed(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
            float4 GatherGreen(SamplerState, float2 texcoord, int2 offset = {0,0});
            float4 GatherGreen(SamplerState, float2 texcoord, int2 offset, out uint status);
            float4 GatherGreen(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
            float4 GatherGreen(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
            float4 GatherBlue(SamplerState, float2 texcoord, int2 offset = {0,0});
            float4 GatherBlue(SamplerState, float2 texcoord, int2 offset, out uint status);
            float4 GatherBlue(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4);
            float4 GatherBlue(SamplerState, float2 texcoord, int2 offset1, int2 offset2, int2 offset3, int2 offset4, out uint status);
            float4 GatherAlpha(SamplerState, float2 texcoord, int2 offset = {0,0});
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
            float CalculateLevelOfDetail(SamplerState sampler, float2 uv);
            float4 operator[](in uint2 pos);
            Mips2D& mips();
        };

        struct TextureCube{
            float4 Sample(SamplerState, float3 texcoord, float lodClamp = {1024});
            float4 Sample(SamplerState, float3 texcoord, float lodClamp, out uint status);
            float4 SampleLevel(SamplerState, float3 texcoord, int mipLevel);
            float4 SampleLevel(SamplerState, float3 texcoord, int mipLevel, out uint status);
            float4 SampleGrad(SamplerState, float3 texcoord, float3 DDX, float3 DDY, float lodClamp = {1024});
            float4 SampleGrad(SamplerState, float3 texcoord, float3 DDX, float3 DDY, float lodClamp, out uint status);
            float4 SampleBias(SamplerState, float3 texcoord, float bias, float lodClamp = {1024});
            float4 SampleBias(SamplerState, float3 texcoord, float bias, float lodClamp, out uint status);
            float4 SampleCmp(SamplerState, float3 texcoord, float CompareValue, float lodClamp = {1024});
            float4 SampleCmp(SamplerState, float3 texcoord, float CompareValue, float lodClamp, out uint status);
            float4 SampleCmpLevelZero(SamplerState, float3 texcoord, float CompareValue);
            float4 SampleCmpLevelZero(SamplerState, float3 texcoord, float CompareValue, out uint status);
            float4 Gather(SamplerState, float3 texcoord);
            float4 Gather(SamplerState, float3 texcoord, out uint status);
            float4 GatherRed(SamplerState, float3 texcoord);
            float4 GatherRed(SamplerState, float3 texcoord, out uint status);
            float4 GatherGreen(SamplerState, float3 texcoord);
            float4 GatherGreen(SamplerState, float3 texcoord, out uint status);
            float4 GatherBlue(SamplerState, float3 texcoord);
            float4 GatherBlue(SamplerState, float3 texcoord, out uint status);
            float4 GatherAlpha(SamplerState, float3 texcoord);
            float4 GatherAlpha(SamplerState, float3 texcoord, out uint status);
            float4 GatherCmp(SamplerState, float3 texcoord, float CompareValue);
            float4 GatherCmp(SamplerState, float3 texcoord, float CompareValue, out uint status);
            float4 GatherCmpRed(SamplerState, float3 texcoord, float CompareValue);
            float4 GatherCmpRed(SamplerState, float3 texcoord, float CompareValue, out uint status);
            float4 GatherCmpGreen(SamplerState, float3 texcoord, float CompareValue);
            float4 GatherCmpGreen(SamplerState, float3 texcoord, float CompareValue, out uint status);
            float4 GatherCmpBlue(SamplerState, float3 texcoord, float CompareValue);
            float4 GatherCmpBlue(SamplerState, float3 texcoord, float CompareValue, out uint status);
            float4 GatherCmpAlpha(SamplerState, float3 texcoord, float CompareValue);
            float4 GatherCmpAlpha(SamplerState, float3 texcoord, float CompareValue, out uint status);
            float CalculateLevelOfDetail(SamplerState sampler, float3 uv);
        };

        template<typename ordinaltype>
        struct RWTexture2D {
            void GetDimensions(out int width, out int height);
            ordinaltype Load(int3 pixelcoord);
            ordinaltype Load(int3 pixelcoord, out int status);
            ordinaltype operator[](in uint2 pos);
            ordinaltype operator[](in int2 pos);
            ordinaltype operator[](in float2 pos);
        };

        struct Mips2DArray {
            struct Mip2DArray {
                float4 operator[](in uint3 pos);
            };
            Mip2DArray operator[](in uint mip);
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
            void GetDimensions(int mipLevel, out int Width, out int Height, out int Elements, out int NumberOfLevels);
            void GetDimensions(out int Width, out int Height, out int Elements);
            float CalculateLevelOfDetail(SamplerState sampler, float2 uv);
            float4 operator[](in uint3 pos);
            Mips2DArray& mips();
        };

        struct TextureCubeArray{
            float4 Sample(SamplerState, float4 texcoord, float lodClamp = {1024});
            float4 Sample(SamplerState, float4 texcoord, float lodClamp, out uint status);
            float4 SampleLevel(SamplerState, float4 texcoord, int mipLevel);
            float4 SampleLevel(SamplerState, float4 texcoord, int mipLevel, out uint status);
            float4 SampleGrad(SamplerState, float4 texcoord, float3 DDX, float3 DDY, float lodClamp = {1024});
            float4 SampleGrad(SamplerState, float4 texcoord, float3 DDX, float3 DDY, float lodClamp, out uint status);
            float4 SampleBias(SamplerState, float4 texcoord, float bias, float lodClamp = {1024});
            float4 SampleBias(SamplerState, float4 texcoord, float bias, float lodClamp, out uint status);
            float4 SampleCmp(SamplerState, float4 texcoord, float CompareValue, float lodClamp = {1024});
            float4 SampleCmp(SamplerState, float4 texcoord, float CompareValue, float lodClamp, out uint status);
            float4 SampleCmpLevelZero(SamplerState, float4 texcoord, float CompareValue);
            float4 SampleCmpLevelZero(SamplerState, float4 texcoord, float CompareValue, out uint status);
            float4 Gather(SamplerState, float4 texcoord);
            float4 Gather(SamplerState, float4 texcoord, out uint status);
            float4 GatherRed(SamplerState, float4 texcoord);
            float4 GatherRed(SamplerState, float4 texcoord, out uint status);
            float4 GatherGreen(SamplerState, float4 texcoord);
            float4 GatherGreen(SamplerState, float4 texcoord, out uint status);
            float4 GatherBlue(SamplerState, float4 texcoord);
            float4 GatherBlue(SamplerState, float4 texcoord, out uint status);
            float4 GatherAlpha(SamplerState, float4 texcoord);
            float4 GatherAlpha(SamplerState, float4 texcoord, out uint status);
            float4 GatherCmp(SamplerState, float4 texcoord, float CompareValue);
            float4 GatherCmp(SamplerState, float4 texcoord, float CompareValue, out uint status);
            float4 GatherCmpRed(SamplerState, float4 texcoord, float CompareValue);
            float4 GatherCmpRed(SamplerState, float4 texcoord, float CompareValue, out uint status);
            float4 GatherCmpGreen(SamplerState, float4 texcoord, float CompareValue);
            float4 GatherCmpGreen(SamplerState, float4 texcoord, float CompareValue, out uint status);
            float4 GatherCmpBlue(SamplerState, float4 texcoord, float CompareValue);
            float4 GatherCmpBlue(SamplerState, float4 texcoord, float CompareValue, out uint status);
            float4 GatherCmpAlpha(SamplerState, float4 texcoord, float CompareValue);
            float4 GatherCmpAlpha(SamplerState, float4 texcoord, float CompareValue, out uint status);
            float CalculateLevelOfDetail(SamplerState sampler, float4 uv);
        };

        struct Mips3D {
            struct Mip3D {
                float4 operator[](in uint3 pos);
            };
            Mip3D operator[](in uint mip);
        };

        template<typename gentype = float4>
        struct Texture3D {
            gentype Sample(SamplerState, float3 texcoord, int3 offset = {0,0,0}, float lodClamp = {1024});
            gentype Sample(SamplerState, float3 texcoord, int3 offset, float lodClamp, out uint status);
            gentype SampleBias(SamplerState, float3 texcoord, float bias, int3 offset = {0,0,0}, float lodClamp = {1024});
            gentype SampleBias(SamplerState, float3 texcoord, float bias, int3 offset, float lodClamp, out uint status);
            gentype SampleGrad(SamplerState, float3 texcoord, float3 ddx, float3 ddy, int3 offset = {0,0,0}, float lodClamp = {1024});
            gentype SampleGrad(SamplerState, float3 texcoord, float3 ddx, float3 ddy, int3 offset, float lodClamp, out uint status);
            gentype SampleLevel(SamplerState, float3 texcoord, float lod, int3 offset = {0,0,0}, float lodClamp = {1024});
            gentype SampleLevel(SamplerState, float3 texcoord, float lod, int3 offset, float lodClamp, out uint status);
            gentype Load(int4 texcoord, int3 offset);
            gentype Load(int4 texcoord, int3 offset, out uint status);
            gentype operator[](uint3 texcoord);
            void GetDimensions(int mipLevel, out float Width, out float Height, out float Depth, out float NumberOfLevels);
            void GetDimensions(out float Width, out float Height, out float Depth);
            void GetDimensions(int mipLevel, out int Width, out int Height, out int Depth, out int NumberOfLevels);
            void GetDimensions(out int Width, out int Height, out int Depth);
            Mips3D& mips();
        };

        template<typename TAnyStruct> struct AppendStructuredBuffer{
            void Append(in TAnyStruct value);
            void GetDimensions(out uint numStructs, out uint stride);
        };

        struct RWResource {    
        };

        template<typename TAnyStruct> struct RWStructuredBuffer: public RWResource{
            uint IncrementCounter();
            uint DecrementCounter();
            void GetDimensions(out uint numStructs, out uint stride);
            TAnyStruct Load(int Location);
            TAnyStruct Load(int Location, out uint Status);
            TAnyStruct operator[](in uint pos);
        };

        template<typename TAnyStruct> struct StructuredBuffer{
            void GetDimensions(out uint numStructs, out uint stride);
            TAnyStruct Load(int Location);
            TAnyStruct Load(int Location, out uint Status);
            TAnyStruct operator[](in uint pos);
        };

        template<typename TAnyStruct> struct TriangleStream{
            void Append(TAnyStruct v);
            void RestartStrip();
        };

        template<typename TAnyStruct> struct PointStream{
            void Append(TAnyStruct v);
            void RestartStrip();
        };

        template<typename TAnyStruct> struct LineStream{
            void Append(TAnyStruct v);
            void RestartStrip();
        };

        void discard;

        template<typename gentype> gentype abs(gentype v);
        template<typename gentype> gentype acos(gentype v);
        template<typename gentype> gentype all(gentype v);
                        void    AllMemoryBarrier();
                        void    AllMemoryBarrierWithGroupSync();
        template<typename gentype> gentype any(gentype v);
                        double  asdouble(uint lowbits, uint highbits);
        template<typename gentype> gentype asfloat(gentype v);
        template<typename gentype> gentype asin(gentype v);
        template<typename gentype> gentype asint(gentype v);
        template<typename gentype> gentype asuint(gentype v);
        template<typename gentype> gentype atan(gentype v);
        template<typename gentype> gentype atan2(gentype y, gentype x);
        template<typename gentype> gentype ceil(gentype v);
                        bool    CheckAccessFullyMapped(uint status);
        template<typename gentype> gentype clamp(gentype v, gentype minval, gentype maxval);
        template<typename gentype> void    clip(gentype v);
        template<typename gentype> gentype cos(gentype v);
        template<typename gentype> gentype cosh(gentype v);
                        uint    countbits(uint v);
        template<typename gentype> gentype cross(gentype v1, gentype v2);
        template<typename gentype> gentype ddx(gentype v);
        template<typename gentype> gentype ddx_coarse(gentype v);
        template<typename gentype> gentype ddx_fine(gentype v);
        template<typename gentype> gentype ddy(gentype v);
        template<typename gentype> gentype ddy_coarse(gentype v);
        template<typename gentype> gentype ddy_fine(gentype v);
        template<typename gentype> gentype degrees(gentype v);
        template<typename gentype> float   determinant(gentype v);
                        void    DeviceMemoryBarrier();
                        void    DeviceMemoryBarrierWithGroupSync();
        template<typename gentype> gentype distance(gentype x, gentype y);
        template<typename gentype> float   dot(gentype v1, gentype v2);
        template<typename gentype> gentype exp(gentype v);
        template<typename gentype> gentype exp2(gentype v);
        template<typename gentype> gentype faceforward(gentype n, gentype i, gentype ng);
                        int     firstbithigh(int v);
                        int     firstbitlow(int v);
        template<typename gentype> gentype floor(gentype v);
        template<typename gentype> double  fma(double a, double b, double c);
        template<typename gentype> gentype fmod(gentype x, gentype y);
        template<typename gentype> gentype frac(gentype x);
        template<typename gentype> gentype frexp(gentype x, gentype exp);
        template<typename gentype> gentype fwidth(gentype v);
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
        template<typename gentype> gentype isfinite(gentype x);
        template<typename gentype> gentype isinf(gentype x);
        template<typename gentype> gentype isnan(gentype x);
        template<typename gentype> gentype ldexp(gentype x, gentype exp);
        template<typename gentype> float   length(gentype x);
        template<typename gentype, typename gentype2> gentype lerp(gentype min, gentype max, gentype2 k);
        template<typename gentype> gentype log(gentype x);
        template<typename gentype> gentype log10(gentype x);
        template<typename gentype> gentype log2(gentype x);
        template<typename gentype> gentype mad(gentype m, gentype a, gentype b);
        template<class gentype, typename gentype2> gentype max(gentype v1, gentype2 v2);
        template<class gentype, typename gentype2> gentype min(gentype v1, gentype2 v2);
        template<typename gentype> gentype modf(gentype v, out gentype ip);
                                   float2 mul(float2 val1, float2x2 val2);
                                   float2 mul(float2x2 val1, float2 val2);
                                   float3 mul(float3 val1, float3x3 val2);
                                   float3 mul(float3x3 val1, float3 val2);
                                   float4 mul(float4 val1, float4x4 val2);
                                   float4 mul(float4x4 val1, float4 val2);
        template<typename gentype> float   noise(gentype v);
        template<typename gentype> gentype normalize(gentype v);
        template<class gentype, typename p> gentype pow(gentype base, p power);
        template<typename gentype> gentype radians(gentype v);
        template<typename gentype> gentype rcp(gentype v);
        template<typename gentype> gentype reflect(gentype i, gentype n);
        template<typename gentype> gentype refract(gentype i, gentype n, float refidx);
                        uint    reversebits(uint i);
        template<typename gentype> gentype round(gentype v);
        template<typename gentype> gentype rsqrt(gentype v);
        template<typename gentype> gentype saturate(gentype v);
        template<typename gentype> gentype sign(gentype v);
        template<typename gentype> gentype sin(gentype v);
        template<typename gentype> void    sincos(gentype v, out gentype s, out gentype c);
        template<typename gentype> gentype sinh(gentype v);
        template<typename gentype> gentype smoothstep(gentype min, gentype max, gentype k);
        template<typename gentype> gentype sqrt(gentype v);
        template<typename gentype> gentype step(gentype y, gentype x);
        template<typename gentype> gentype tan(gentype v);
        template<typename gentype> gentype tanh(gentype v);
        template<typename gentype> gentype transpose(gentype v);
                                   int  trunc(float v);
                                   int2 trunc(float2 v);
                                   int3 trunc(float3 v);
                                   int4 trunc(float4 v);
    #else // !CppEditor
        #define S_(a) a : a
        #define S_VertexID(a) a : SV_VertexID
        #define S_InstanceID(a) a : SV_InstanceID
        #define S_PrimitiveID(a) a : SV_PrimitiveID
        #define S_Position(a) a : SV_Position
        #define S_Target0(a) a : SV_Target0
        #define S_Target1(a) a : SV_Target1
        #define S_Target2(a) a : SV_Target2
        #define S_Target3(a) a : SV_Target3
        #define S_Depth(a) a : SV_Depth
        #define S_RenderTargetArrayIndex(a) a : SV_RenderTargetArrayIndex
        #define S_ViewportArrayIndex(a) a : SV_ViewportArrayIndex
        #define S_DepthGreaterEqual(a) a : SV_DepthGreaterEqual
        #define S_DepthLessEqual(a) a : SV_DepthLessEqual
        #define S_ClipDistance0(a) a : SV_ClipDistance0
        #define S_ClipDistance1(a) a : SV_ClipDistance1
        #define S_ClipDistance2(a) a : SV_ClipDistance2
        #define S_ClipDistance3(a) a : SV_ClipDistance3
        #define S_CullDistance0(a) a : SV_CullDistance0
        #define S_CullDistance1(a) a : SV_CullDistance1
        #define S_CullDistance2(a) a : SV_CullDistance2
        #define S_CullDistance3(a) a : SV_CullDistance3
        #define S_DispatchThreadID(a) a : SV_DispatchThreadID
        #define S_DomainLocation(a) a : SV_DomainLocation
        #define S_GroupID(a) a : SV_GroupID
        #define S_GroupIndex(a) a : SV_GroupIndex
        #define S_GroupThreadID(a) a : SV_GroupThreadID
        #define S_GSInstanceID(a) a : SV_GSInstanceID
        #define S_Coverage(a) a : SV_Coverage
        #define S_InnerCoverage(a) a : SV_InnerCoverage
        #define S_InsideTessFactor(a) a : SV_InsideTessFactor
        #define S_IsFrontFace(a) a : SV_IsFrontFace
        #define S_OutputControlPointID(a) a : SV_OutputControlPointID
        #define S_SampleIndex(a) a : SV_SampleIndex
        #define S_StencilRef(a) a : SV_StencilRef
        #define S_TessFactor(a) a : SV_TessFactor

        #define register_(a) : register(a)
    #endif

#endif	/* HLSL_H */

