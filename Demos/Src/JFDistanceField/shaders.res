        ��  ��                  �	  4   ��
 D X _ R E S O L V E         0 	        VERTb  CODE    DXBCf� g�ɕ�"���%        4      0  �  �  RDEF�      H          �� �  �   <                              $Globals ���<      `              x             �       FBOFlip             Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGN(                              vsCoord OSGNP         8                    D                   SV_Position TexCoord ���SHDR  @  A   Y  F�         _  2     g  �         e  2     h     6  2      F     6  �      @                �?8  2      F     F�          2  2      F      @    �?  ��        @    �?  �?        8  
2     F      @     ?   ?        >  STATt                                                                                                                 UBLK>      $Globals         FBOFlip                           FRAG@  CODE�  �  DXBC�ߜ��tm����K)6�   �     4   �    H  0  RDEF�              �� �  L  �                             �                            �            ����          �            ����         �                              SrcTexSampler SrcDistanceFieldSampler SrcTex SrcDistanceField $Globals ��                   4            <      Time ���             Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGNP         8                    D                   SV_Position TexCoord ���OSGN,                               SV_Target ��SHDR�  @   �   Y  F�         Z   `     Z   `    X  p     UU  X  p    UU  b 2    e  �      h     8        
�          @    �@M   �        
      2  
      
      @     ?
�          =  �     @      F~     E  	�     F    F~     `    8  b                 2     F     F    E  	�     F     F~      `       "      �     �     K  "            8  B            @    �A1  "      @            D  B      *        
B      @    �?  �?  �?  �?*      2  
      
      @    �@* �A       M   �        
               
      @    �?8        
      @     ?8  �            	    7  	r      V     �     F    6  �      @    �?>  STATt                                                                                                               UBLK�      $Globals         Time                          SrcTex                     SrcDistanceField                  �  4   ��
 O G L _ R E S O L V E       0 	        {
 "Vertex": "#version 430\r\nuniform vec2 FBOFlip;\r\n\r\nin vec2 vsCoord;\r\nout vec2 TexCoord;\r\n\r\nvoid main() {\r\n    gl_Position = vec4(vsCoord, 0.0, 1.0);\r\n    TexCoord = (vsCoord*FBOFlip*vec2(1.0,-1.0) + 1.0) * 0.5;\r\n}\r\n",
 "Fragment": "#version 430\r\n\r\nin vec2 TexCoord;\r\n\r\nuniform float Time;\r\nuniform sampler2D SrcTex;\r\nuniform sampler2D SrcDistanceField;\r\n\r\nlayout(location = 0) out vec4 OutColor;\r\n\r\nvoid main() {\r\n    vec2 offset = texture(SrcDistanceField, TexCoord).rg;\r\n    OutColor.a = 1.0;\r\n    OutColor.rgb = texture(SrcTex, TexCoord + offset).rgb;\r\n    vec2 TexSize = textureSize(SrcTex, 0).xy;\r\n    offset *= TexSize;\r\n    float offsetLen = length(offset);\r\n    if (offsetLen > 0.0) {\r\n        offsetLen *= 25.0;\r\n        offsetLen = pow(offsetLen, 0.5);\r\n        float t = Time + cos(Time*4.0)*0.5;\r\n        OutColor.rgb *= (cos(t*4.0-offsetLen)+1.0)*0.5;\r\n    }\r\n}\r\n",
 "Name": "resolve"
}   Z  4   ��
 D X _ P R E P A R E         0 	        VERTb  CODE    DXBCf� g�ɕ�"���%        4      0  �  �  RDEF�      H          �� �  �   <                              $Globals ���<      `              x             �       FBOFlip             Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGN(                              vsCoord OSGNP         8                    D                   SV_Position TexCoord ���SHDR  @  A   Y  F�         _  2     g  �         e  2     h     6  2      F     6  �      @                �?8  2      F     F�          2  2      F      @    �?  ��        @    �?  �?        8  
2     F      @     ?   ?        >  STATt                                                                                                                 UBLK>      $Globals         FBOFlip                           FRAG�  CODE�  �  DXBC�9��M�)+;@]���   �     4   �   8  l  $  RDEF�                  �� �  q   \                             j            ����          SrcTexSampler SrcTex Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ��ISGNP         8                    D                   SV_Position TexCoord ���OSGN,                               SV_Target ��SHDR�   @   ,   Z   `     X  p     UU  b 2    e  �      h     E  	�      F    F~      `     1        :      @    �?  
�            @    �  �  �  �>  STATt                                                                                                                UBLK4      $Globals          SrcTex                    =  4   ��
 O G L _ P R E P A R E       0 	        {
 "Vertex": "#version 430\r\nuniform vec2 FBOFlip;\r\n\r\nin vec2 vsCoord;\r\nout vec2 TexCoord;\r\n\r\nvoid main() {\r\n    gl_Position = vec4(vsCoord, 0.0, 1.0);\r\n    TexCoord = (vsCoord*FBOFlip*vec2(1.0,-1.0) + 1.0) * 0.5;\r\n}\r\n",
 "Fragment": "#version 430\r\nconst float inf = 10000000.0;\r\n\r\nin vec2 TexCoord;\r\n\r\nuniform sampler2D SrcTex;\r\n\r\nlayout(location = 0) out vec4 OutColor;\r\n\r\nvoid main() {\r\n    if (texture(SrcTex, TexCoord).a < 1.0)\r\n		OutColor = vec4(inf);\r\n    else\r\n		OutColor = vec4(0.0);\r\n}\r\n",
 "Name": "prepare"
}   j	  0   ��
 D X _ J F P A S S       0 	        VERTb  CODE    DXBCf� g�ɕ�"���%        4      0  �  �  RDEF�      H          �� �  �   <                              $Globals ���<      `              x             �       FBOFlip             Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGN(                              vsCoord OSGNP         8                    D                   SV_Position TexCoord ���SHDR  @  A   Y  F�         _  2     g  �         e  2     h     6  2      F     6  �      @                �?8  2      F     F�          2  2      F      @    �?  ��        @    �?  �?        8  
2     F      @     ?   ?        >  STATt                                                                                                                 UBLK>      $Globals         FBOFlip                           FRAG�  CODE\  X  DXBC��'KN�퓉~�z�   X     4   �  �  (  �  RDEF`     �          �� �  ,  |                             �            ����          �                              SrcDistanceFieldSampler SrcDistanceField $Globals ���      �              �                                    JumpStep ���            Aspect �             Microsoft (R) HLSL Shader Compiler 9.29.952.3111 ���ISGNP         8                    D                   SV_Position TexCoord ���OSGN,                               SV_Target ��SHDR�  @   �   Y  F�         Z   `     X  p     UU  b 2    e  �      h     6  �      @    �  �  �����0  !       :      @      
     +  "     :      6  r     F     6  B     @  ����0  !  �     *     @      :     +       *     2  
2     F�          F     F    E  	�     F     F~      `     2  
2     F�          F     F     8       
     *�          6  "            B     F     F     1       *     *     7  	r          F    F      B     *     @       6  r      F      �      :      @       6  2      F      6  �      @                  >  STATt                                                                                                            UBLK�      $Globals         JumpStep                             Aspect                         SrcDistanceField                    f  4   ��
 O G L _ J F P A S S         0 	        {
 "Vertex": "#version 430\r\nuniform vec2 FBOFlip;\r\n\r\nin vec2 vsCoord;\r\nout vec2 TexCoord;\r\n\r\nvoid main() {\r\n    gl_Position = vec4(vsCoord, 0.0, 1.0);\r\n    TexCoord = (vsCoord*FBOFlip*vec2(1.0,-1.0) + 1.0) * 0.5;\r\n}\r\n",
 "Fragment": "#version 430\r\n\r\nconst float inf = 10000000.0;\r\n\r\nin vec2 TexCoord;\r\n\r\nuniform vec2 JumpStep;\r\nuniform float Aspect;\r\nuniform sampler2D SrcDistanceField;\r\n\r\n\layout(location = 0) out vec4 OutColor;\r\n\r\n\r\nvoid main() {\r\n    vec2 minVec = vec2(inf);\r\n   float minDistSqr = inf;\r\n\r\n    for (int j = -1; j < 2; j++) {\r\n        for (int i = -1; i < 2; i++) {\r\n            vec2 JumpOffset = JumpStep * vec2(i, j);\r\n            vec2 DistVec = JumpOffset +texture(SrcDistanceField, TexCoord + JumpOffset).rg;\r\n            float DistLenSqr = dot(DistVec*vec2(Aspect, 1.0), DistVec*vec2(Aspect, 1.0));\r\n            if (DistLenSqr<minDistSqr) {\r\n                minVec = DistVec;\r\n                minDistSqr = DistLenSqr;\r\n            }\r\n        }\r\n    }\r\n\r\n    OutColor = vec4(minVec, 0, 0);\r\n}\r\n",
 "Name": "JFPass"
}  