unit hlsl2glsl;

interface

const
    hlsl2glslDll = 'hlslang.dll';

// Copyright (c) The HLSL2GLSLFork Project Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE.txt file.

type


/// Types of languages the HLSL2GLSL translator can consume.
TEShLanguage = (EShLangVertex,
                EShLangFragment,
                EShLangCount);

/// Binding table.  This can be used for locating attributes, uniforms, globals, etc., as needed.
TShBinding = record
    AName: PAnsiChar;
    Binding: Integer;
end;
PShBinding = ^TShBinding;

TShBindingTable = record
    numBindings: Integer;
    bindings: PShBinding;
end;

/// GLSL shader variable types
/// NOTE: these are ordered to exactly match the internal enums
TEShType = (EShTypeVoid,
            EShTypeBool,
            EShTypeBVec2,
            EShTypeBVec3,
            EShTypeBVec4,
            EShTypeInt,
            EShTypeIVec2,
            EShTypeIVec3,
            EShTypeIVec4,
            EShTypeFloat,
            EShTypeVec2,
            EShTypeVec3,
            EShTypeVec4,
            EShTypeMat2,
            EShTypeMat2x3,
            EShTypeMat2x4,
            EShTypeMat3x2,
            EShTypeMat3,
            EShTypeMat3x4,
            EShTypeMat4x2,
            EShTypeMat4x3,
            EShTypeMat4x4,
            EShTypeSampler,
            EShTypeSampler1D,
            EShTypeSampler1DShadow,
            EShTypeSampler2D,
            EShTypeSampler2DShadow,
            EShTypeSampler3D,
            EShTypeSamplerCube,
            EShTypeSamplerRect,
            EShTypeSamplerRectShadow,
            EShTypeStruct);


/// HLSL attribute semantics
/// NOTE: these are ordered to exactly match the internal tables
TEAttribSemantic = (EAttrSemNone,
                    EAttrSemPosition,
                    EAttrSemPosition1,
                    EAttrSemPosition2,
                    EAttrSemPosition3,
                    EAttrSemVPos,
                    EAttrSemVFace,
                    EAttrSemNormal,
                    EAttrSemNormal1,
                    EAttrSemNormal2,
                    EAttrSemNormal3,
                    EAttrSemColor0,
                    EAttrSemColor1,
                    EAttrSemColor2,
                    EAttrSemColor3,
                    EAttrSemTex0,
                    EAttrSemTex1,
                    EAttrSemTex2,
                    EAttrSemTex3,
                    EAttrSemTex4,
                    EAttrSemTex5,
                    EAttrSemTex6,
                    EAttrSemTex7,
                    EAttrSemTex8,
                    EAttrSemTex9,
                    EAttrSemTangent,
                    EAttrSemTangent1,
                    EAttrSemTangent2,
                    EAttrSemTangent3,
                    EAttrSemBinormal,
                    EAttrSemBinormal1,
                    EAttrSemBinormal2,
                    EAttrSemBinormal3,
                    EAttrSemBlendWeight,
                    EAttrSemBlendWeight1,
                    EAttrSemBlendWeight2,
                    EAttrSemBlendWeight3,
                    EAttrSemBlendIndices,
                    EAttrSemBlendIndices1,
                    EAttrSemBlendIndices2,
                    EAttrSemBlendIndices3,
                    EAttrSemPSize,
                    EAttrSemPSize1,
                    EAttrSemPSize2,
                    EAttrSemPSize3,
                    EAttrSemDepth,
                    EAttrSemUnknown,
                    EAttrSemVertexID,
                    EAttrSemInstanceID,
                    EAttrSemPrimitiveID,
                    EAttrSemCoverage,
                    EAttrSemCount);
PEAttribSemantic = ^TEAttribSemantic;


/// Uniform info struct
TShUniformInfo = record
    AName    : PAnsiChar;
    semantic : PAnsiChar;
    atype    : TEShType;
    arraySize: Integer;
    init     : PSingle;
end;
PShUniformInfo = ^TShUniformInfo;

/// Target language version
TETargetVersion = (// NOTE: keep ordering roughly in increasing capability set
                   ETargetGLSL_ES_100,
                   ETargetGLSL_110,
                   ETargetGLSL_120,
                   ETargetGLSL_140,
                   ETargetGLSL_ES_300,
                   // ETargetGLSL_330,
                   ETargetVersionCount);


const
	ETranslateOpNone = 0;
	ETranslateOpIntermediate = 1;
	/// Some drivers (e.g. OS X 10.6.x) have bugs with GLSL 1.20 array
	/// initializer syntax. If you need to support this configuration,
	/// use this flag to generate compatible syntax. You'll need
	/// to prepend HLSL2GLSL_ENABLE_ARRAY_120_WORKAROUND to the shader.
	///
	/// Example of emitted code for a simple array declaration:
	/// (HLSL Source)
	///		float2 samples[] = {
	///			float2(-1, 0.1),
	///			float2(0, 0.5),
	///			float2(1, 0.1)
	///		};
	/// (GLSL Emitted result)
	///		#if defined(HLSL2GLSL_ENABLE_ARRAY_120_WORKAROUND)
	///			vec2 samples[];
	///			samples[0] = vec2(-1.0, 0.1);
	///			samples[1] = vec2(0.0, 0.5);
	///			samples[2] = vec2(1.0, 0.1);
	///		#else
	///			const vec2 samples[] = vec2[](vec2(-1.0, 0.1), vec2(0.0, 0.5), vec2(1.0, 0.1));
	///		#endif
    ETranslateOpEmitGLSL120ArrayInitWorkaround = 1 shl 1;
	// Instead of using built-in "gl_MultiTexCoord0" for "appdata_t.texcoord : TEXCOORD0"
	//  we will output an attribute "xlat_attrib_TEXCOORD0". Targeting GLSL ES forces this
	//  as there are no built-in attributes in that variant.
    ETranslateOpAvoidBuiltinAttribNames = 1 shl 2;
	// Always use "gl_MultiTexCoord0" for "TEXCOORD0" and so on,
	// even in GLSL ES. It is expected that client code will add #defines to handle them
	// later on.
    ETranslateOpForceBuiltinAttribNames = 1 shl 3;
	// When not using built-in attribute names (due to ETranslateOpAvoidBuiltinAttribNames or GLSL ES),
	//  instead of outputting e.g. "xlat_attrib_TEXCOORD0" for "appdata_t.texcoord : TEXCOORD0"
	//  we will output "appdata_t_texcoord"
    ETranslateOpPropogateOriginalAttribNames = 1 shl 4;

/// Generic opaque handle.  This type is used for handles to the parser/translator.
/// If handle creation fails, 0 will be returned.
type TShHandle = Pointer;

/// Initialize the HLSL2GLSL translator.  This function must be called once prior to calling any other
/// HLSL2GLSL translator functions
/// \return
///   1 on success, 0 on failure
function Hlsl2Glsl_Initialize(): Integer; cdecl; external hlsl2glslDll;

/// Shutdown the HLSL2GLSL translator.  This function should be called to de-initialize the HLSL2GLSL
/// translator and should only be called once on shutdown.
procedure Hlsl2Glsl_Shutdown(); cdecl; external hlsl2glslDll;

/// Construct a compiler for the given language (one per shader)
function Hlsl2Glsl_ConstructCompiler( const language: TEShLanguage ): TShHandle; cdecl; external hlsl2glslDll;


procedure Hlsl2Glsl_DestructCompiler( handle: TShHandle ); cdecl; external hlsl2glslDll;



/// Parse HLSL shader to prepare it for final translation.
function Hlsl2Glsl_Parse(
	const handle: TShHandle;
	const shaderString: PAnsiChar;
	targetVersion: TETargetVersion;
	options: Cardinal): Integer; cdecl; external hlsl2glslDll;



/// After parsing a HLSL shader, do the final translation to GLSL.
function Hlsl2Glsl_Translate(
	const handle: TShHandle;
	const entry: PAnsiChar;
	targetVersion: TETargetVersion;
	options: Cardinal): Integer; cdecl; external hlsl2glslDll;


/// After translating HLSL shader(s), retrieve the translated GLSL source.
function Hlsl2Glsl_GetShader( const handle: TShHandle ): PAnsiChar; cdecl; external hlsl2glslDll;


function Hlsl2Glsl_GetInfoLog( const handle: TShHandle ): PAnsiChar; cdecl; external hlsl2glslDll;


/// After translating, retrieve the number of uniforms
function Hlsl2Glsl_GetUniformCount( const handle: TShHandle ): Integer; cdecl; external hlsl2glslDll;


/// After translating, retrieve the uniform info table
function Hlsl2Glsl_GetUniformInfo( const handle: TShHandle ): PShUniformInfo; cdecl; external hlsl2glslDll;


/// Instead of mapping HLSL attributes to GLSL fixed-function attributes, this function can be used to
/// override the  attribute mapping.  This tells the code generator to use user-defined attributes for
/// the semantics that are specified.
///
/// \param handle
///      Handle to the compiler.  This should be called BEFORE calling Hlsl2Glsl_Translate
/// \param pSemanticEnums
///      Array of semantic enums to set
/// \param pSemanticNames
///      Array of user attribute names to use
/// \param nNumSemantics
///      Number of semantics to set in the arrays
/// \return
///      1 on success, 0 on failure
type
  TPAnsiCharArray = packed array[0..(MaxLongint div SizeOf(PAnsiChar))-1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;

function Hlsl2Glsl_SetUserAttributeNames ( handle: TShHandle;
                                           pSemanticEnums: PEAttribSemantic;
                                           pSemanticNames: PPAnsiCharArray;
                                           nNumSemantics: Integer ): Integer; cdecl; external hlsl2glslDll;


/// Instead of using OpenGL fixed-function varyings (such as gl_TexCoord[x]), use user named varyings
/// instead.
///
/// \param handle
///      Handle to the compiler.  This should be called BEFORE calling Hlsl2Glsl_Translate
/// \param bUseUserVarying
///      If true, all user varyings will be used.  If false, the translator will attempt to use
///      GL fixed-function varyings
/// \return
///      1 on success, 0 on failure
function Hlsl2Glsl_UseUserVaryings ( handle: TShHandle;
                                     bUseUserVaryings: Boolean ): Integer; cdecl; external hlsl2glslDll;


function Hlsl2Glsl_VersionUsesPrecision (version: TETargetVersion): Boolean; cdecl; external hlsl2glslDll;

implementation

end.
