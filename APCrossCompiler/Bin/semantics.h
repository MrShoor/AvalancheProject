#ifdef CppEditor
    #define S_Sem(a) a
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
#else
    #define S_Sem(a) a : a
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
#endif