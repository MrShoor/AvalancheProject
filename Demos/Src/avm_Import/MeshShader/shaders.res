        ��  ��                  d  0   ��
 D X _ A V M E S H         	        VERT<  CODE   �  DXBCԷ,^��ʢ�W.^\   �     4   l  @  �  �  RDEF0     �          �� �  �  |                             �            ����          �                              BoneTransformSampler BoneTransform $Globals �      �                   @             ,  @   @             7  �   @             A  �   @            J     @             Z  @  @             l  �  @             }  �  @             �     @             �  @  @             �  �  @            �  �  @             �            �      �          �      M_Matrix ���            MVP_Matrix MV_Matrix P_Matrix M_InverseMatrix MVP_InverseMatrix MV_InverseMatrix P_InverseMatrix VP_Matrix VP_InverseMatrix V_Matrix V_InverseMatrix FBOFlip ���            BonePixelHeight              Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGN�         �                    �                   �                   �                    �                   �                   vsCoord vsNormal vsTex vsMatIndex vsWIndex vsWeight OSGN�         h                    t                   {                   �                   SV_Position vCoord vNorm vTex ��SHDR�	  @  j  Y  F�      1   Z   `     X  p     UU  _  r     _  r    _  2    _  �    _  �    g  �         e  r     e  r     e  2     h     9        *�      0   @       
        
�      F    @                   
               
    @     ?8       
      *�      0   6  �     @         >  �>   ?H  �         F~      `     @      H  �     &
    F~      `     @      H  �     6    F~      `     @      8  �     F        8  �     F        8  �     F          6  �     @    �?            6  �     @        �?        6  �     @            �?                          @     ?8       
      *�      0   6  �     @         >  �>   ?H  �         F~      `     @      H  �     &
    F~      `     @      H  �     6    F~      `     @      2  	�     F    V    F    2  	�     F    V    F    2  	�     F    V    F       *               *    @     ?8       
      *�      0   6  �     @         >  �>   ?H  �         F~      `     @      H  �     &
    F~      `     @      H  �     6    F~      `     @      2  	�     F    �    F    2  	�     F    �    F    2  	�     F    �    F       :               :    @     ?8        
      *�      0   6  �      @         >  �>   ?H  �          F~      `     @      H  �     &
     F~      `     @      H  �      6     F~      `     @      2  	�     F    �    F    2  	�     F    �    F    2  	�     F     �    F        6  �     @    �?            6  �     @        �?        6  �     @            �?      6  r      F     6  �      @    �?       F    F       "     F    F             F    F     8  �      V    �      )   2  
�      �      (        V     2  
r      F�      *         �        r      F     F�      +     �      F    F    D  �      :      8  r     �     F    8  r     V    F�      )   2  
�     F�      (        F    2  
r     F�      *   �
    F    8  �     V     F�         2  
�     F�               F    2  
�     F�         �
     F       �      F    F�         6  r     F     6  2     F    >  STATt   N          	                                                           2                                       UBLK,     $Globals        P_Matrix       �       @                                                                      V_Matrix       �      @                                                                      BonePixelHeight                        BoneTransform                  FRAG  CODE�  �  DXBC$�9G~����-5��   �     4   �   x  �  P  RDEF�                  �� �  y   \                             n            ����          DiffuseMapSampler DiffuseMap Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ��ISGN�         h                    t                   {                   �                   SV_Position vCoord vNorm vTex ��OSGN,                               SV_Target ��SHDR�  @   g   Z   `     X  p     UU  b r    b r    b 2    e  �      h             F    F    D        
      8  r            F      �      F    F    D  �      :      8  r     �     F            F�A       F    4        
      @      E  	�     F    F~      `     2  r      F          @  ���>���>���>    6  �      :     >  STATt                                                                                                                UBLK8      $Globals       
   DiffuseMap                  �  4   ��
 O G L _ A V M E S H           	        {
 "Vertex": "#version 330\r\nstruct vec1 {\r\n\tfloat x;\r\n};\r\nstruct uvec1 {\r\n\tuint x;\r\n};\r\nstruct ivec1 {\r\n\tint x;\r\n};\r\nuniform \tmat4 M_Matrix;\r\nuniform \tmat4 MVP_Matrix;\r\nuniform \tmat4 MV_Matrix;\r\nuniform \tmat4 P_Matrix;\r\nuniform \tmat4 M_InverseMatrix;\r\nuniform \tmat4 MVP_InverseMatrix;\r\nuniform \tmat4 MV_InverseMatrix;\r\nuniform \tmat4 P_InverseMatrix;\r\nuniform \tmat4 VP_Matrix;\r\nuniform \tmat4 VP_InverseMatrix;\r\nuniform \tmat4 V_Matrix;\r\nuniform \tmat4 V_InverseMatrix;\r\nuniform \tvec2 FBOFlip;\r\nuniform \tfloat BonePixelHeight;\r\nuniform sampler2D BoneTransform;\r\n in  vec4 in_vsCoord0;\r\nvec4 Input0;\r\n in  vec4 in_vsNormal0;\r\nvec4 Input1;\r\n in  vec4 in_vsTex0;\r\nvec4 Input2;\r\n in  vec4 in_vsWIndex0;\r\nvec4 Input4;\r\n in  vec4 in_vsWeight0;\r\nvec4 Input5;\r\n#undef Output0\r\n#define Output0 phase0_Output0\r\nvec4 phase0_Output0;\r\n out  vec4 vCoord0;\r\n#define Output1 vCoord0\r\n out  vec4 vNorm0;\r\n#define Output2 vNorm0\r\n out  vec4 vTex0;\r\n#define Output3 vTex0\r\nvec4 Temp[7];\r\nivec4 Temp_int[7];\r\nuvec4 Temp_uint[7];\r\nvoid main()\r\n{\r\n    Input0 = in_vsCoord0;\r\n    Input1 = in_vsNormal0;\r\n    Input2 = in_vsTex0;\r\n    Input4 = in_vsWIndex0;\r\n    Input5 = in_vsWeight0;\r\n    Temp[0].x = uintBitsToFloat((BonePixelHeight!=intBitsToFloat(0x0)) ? 0xFFFFFFFFu : 0u);\r\n    if((floatBitsToUint(Temp[0]).x)!=0u){\r\n        Temp[0] = uintBitsToFloat(uvec4(greaterThanEqual(Input4, vec4(intBitsToFloat(0x0), intBitsToFloat(0x0), intBitsToFloat(0x0), intBitsToFloat(0x0)))) * 0xFFFFFFFFu);\r\n        if((floatBitsToUint(Temp[0]).x)!=0u){\r\n            Temp[0].x = Input4.x + intBitsToFloat(0x3F000000);\r\n            Temp[1].x = Temp[0].x * BonePixelHeight;\r\n            Temp[1].yzw = vec3(intBitsToFloat(0x3E000000), intBitsToFloat(0x3EC00000), intBitsToFloat(0x3F200000));\r\n            Temp[2] = textureLod(BoneTransform, Temp[1].yx, intBitsToFloat(0x0));\r\n            Temp[3] = textureLod(BoneTransform, Temp[1].zx, intBitsToFloat(0x0));\r\n            Temp[1] = textureLod(BoneTransform, Temp[1].wx, intBitsToFloat(0x0));\r\n            Temp[2] = Temp[2] * Input5.xxxx;\r\n            Temp[3] = Temp[3] * Input5.xxxx;\r\n            Temp[1] = Temp[1] * Input5.xxxx;\r\n        } else {\r\n            Temp[2] = vec4(intBitsToFloat(0x3F800000), intBitsToFloat(0x0), intBitsToFloat(0x0), intBitsToFloat(0x0));\r\n            Temp[3] = vec4(intBitsToFloat(0x0), intBitsToFloat(0x3F800000), intBitsToFloat(0x0), intBitsToFloat(0x0));\r\n            Temp[1] = vec4(intBitsToFloat(0x0), intBitsToFloat(0x0), intBitsToFloat(0x3F800000), intBitsToFloat(0x0));\r\n        \/\/ENDIF\r\n        }\r\n        if((floatBitsToUint(Temp[0]).y)!=0u){\r\n            Temp[0].x = Input4.y + intBitsToFloat(0x3F000000);\r\n            Temp[4].x = Temp[0].x * BonePixelHeight;\r\n            Temp[4].yzw = vec3(intBitsToFloat(0x3E000000), intBitsToFloat(0x3EC00000), intBitsToFloat(0x3F200000));\r\n            Temp[5] = textureLod(BoneTransform, Temp[4].yx, intBitsToFloat(0x0));\r\n            Temp[6] = textureLod(BoneTransform, Temp[4].zx, intBitsToFloat(0x0));\r\n            Temp[4] = textureLod(BoneTransform, Temp[4].wx, intBitsToFloat(0x0));\r\n            Temp[2] = Temp[5] * Input5.yyyy + Temp[2];\r\n            Temp[3] = Temp[6] * Input5.yyyy + Temp[3];\r\n            Temp[1] = Temp[4] * Input5.yyyy + Temp[1];\r\n        \/\/ENDIF\r\n        }\r\n        if((floatBitsToUint(Temp[0]).z)!=0u){\r\n            Temp[0].x = Input4.z + intBitsToFloat(0x3F000000);\r\n            Temp[4].x = Temp[0].x * BonePixelHeight;\r\n            Temp[4].yzw = vec3(intBitsToFloat(0x3E000000), intBitsToFloat(0x3EC00000), intBitsToFloat(0x3F200000));\r\n            Temp[5] = textureLod(BoneTransform, Temp[4].yx, intBitsToFloat(0x0));\r\n            Temp[6] = textureLod(BoneTransform, Temp[4].zx, intBitsToFloat(0x0));\r\n            Temp[4] = textureLod(BoneTransform, Temp[4].wx, intBitsToFloat(0x0));\r\n            Temp[2] = Temp[5] * Input5.zzzz + Temp[2];\r\n            Temp[3] = Temp[6] * Input5.zzzz + Temp[3];\r\n            Temp[1] = Temp[4] * Input5.zzzz + Temp[1];\r\n        \/\/ENDIF\r\n        }\r\n        if((floatBitsToUint(Temp[0]).w)!=0u){\r\n            Temp[0].x = Input4.w + intBitsToFloat(0x3F000000);\r\n            Temp[0].x = Temp[0].x * BonePixelHeight;\r\n            Temp[0].yzw = vec3(intBitsToFloat(0x3E000000), intBitsToFloat(0x3EC00000), intBitsToFloat(0x3F200000));\r\n            Temp[4] = textureLod(BoneTransform, Temp[0].yx, intBitsToFloat(0x0));\r\n            Temp[5] = textureLod(BoneTransform, Temp[0].zx, intBitsToFloat(0x0));\r\n            Temp[0] = textureLod(BoneTransform, Temp[0].wx, intBitsToFloat(0x0));\r\n            Temp[2] = Temp[4] * Input5.wwww + Temp[2];\r\n            Temp[3] = Temp[5] * Input5.wwww + Temp[3];\r\n            Temp[1] = Temp[0] * Input5.wwww + Temp[1];\r\n        \/\/ENDIF\r\n        }\r\n    } else {\r\n        Temp[2] = vec4(intBitsToFloat(0x3F800000), intBitsToFloat(0x0), intBitsToFloat(0x0), intBitsToFloat(0x0));\r\n        Temp[3] = vec4(intBitsToFloat(0x0), intBitsToFloat(0x3F800000), intBitsToFloat(0x0), intBitsToFloat(0x0));\r\n        Temp[1] = vec4(intBitsToFloat(0x0), intBitsToFloat(0x0), intBitsToFloat(0x3F800000), intBitsToFloat(0x0));\r\n    \/\/ENDIF\r\n    }\r\n    Temp[0].xyz = Input0.xyz;\r\n    Temp[0].w = intBitsToFloat(0x3F800000);\r\n    Temp[2].x = dot(Temp[2], Temp[0]);\r\n    Temp[2].y = dot(Temp[3], Temp[0]);\r\n    Temp[0].x = dot(Temp[1], Temp[0]);\r\n    Temp[0].yzw = Temp[2].yyy * V_Matrix[1].xyz;\r\n    Temp[0].yzw = V_Matrix[0].xyz * Temp[2].xxx + Temp[0].yzw;\r\n    Temp[0].xyz = V_Matrix[2].xyz * Temp[0].xxx + Temp[0].yzw;\r\n    Temp[0].xyz = Temp[0].xyz + V_Matrix[3].xyz;\r\n    Temp[0].w = dot(Input1.xyz, Input1.xyz);\r\n    Temp[0].w = inversesqrt(Temp[0].w);\r\n    Temp[1].xyz = Temp[0].www * Input1.xyz;\r\n    Temp[2].xyz = Temp[1].yyy * V_Matrix[1].xyz;\r\n    Temp[1].xyw = V_Matrix[0].xyz * Temp[1].xxx + Temp[2].xyz;\r\n    Output2.xyz = V_Matrix[2].xyz * Temp[1].zzz + Temp[1].xyw;\r\n    Temp[1] = Temp[0].yyyy * P_Matrix[1];\r\n    Temp[1] = P_Matrix[0] * Temp[0].xxxx + Temp[1];\r\n    Temp[1] = P_Matrix[2] * Temp[0].zzzz + Temp[1];\r\n    Output0 = Temp[1] + P_Matrix[3];\r\n    Output1.xyz = Temp[0].xyz;\r\n    Output3.xy = Input2.xy;\r\n    gl_Position = vec4(phase0_Output0);\r\n    return;\r\n}\r\n",
 "Fragment": "#version 330\r\nstruct vec1 {\r\n\tfloat x;\r\n};\r\nstruct uvec1 {\r\n\tuint x;\r\n};\r\nstruct ivec1 {\r\n\tint x;\r\n};\r\nuniform sampler2D DiffuseMap;\r\n in  vec4 vCoord0;\r\nvec4 Input1;\r\n in  vec4 vNorm0;\r\nvec4 Input2;\r\n in  vec4 vTex0;\r\nvec4 Input3;\r\nlayout(location = 0) out  vec4 SV_Target0;\r\n#define Output0 SV_Target0\r\nvec4 Temp[2];\r\nivec4 Temp_int[2];\r\nuvec4 Temp_uint[2];\r\nvoid main()\r\n{\r\n    Input1 = vCoord0;\r\n    Input2 = vNorm0;\r\n    Input3 = vTex0;\r\n    Temp[0].x = dot(Input2.xyz, Input2.xyz);\r\n    Temp[0].x = inversesqrt(Temp[0].x);\r\n    Temp[0].xyz = Temp[0].xxx * Input2.xyz;\r\n    Temp[0].w = dot(Input1.xyz, Input1.xyz);\r\n    Temp[0].w = inversesqrt(Temp[0].w);\r\n    Temp[1].xyz = Temp[0].www * Input1.xyz;\r\n    Temp[0].x = dot((-Temp[0].xyz), Temp[1].xyz);\r\n    Temp[0].x = max(Temp[0].x, intBitsToFloat(0x0));\r\n    Temp[1] = texture(DiffuseMap, Input3.xy);\r\n    Output0.xyz = Temp[1].xyz * Temp[0].xxx + vec3(intBitsToFloat(0x3E99999A), intBitsToFloat(0x3E99999A), intBitsToFloat(0x3E99999A));\r\n    Output0.w = Temp[1].w;\r\n    return;\r\n}\r\n",
 "Name": "avMesh"
}