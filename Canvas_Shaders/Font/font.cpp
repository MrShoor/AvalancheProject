#include "hlsl.h"
#include "UICommon.h"

float4x4 Transform;

struct VS_Input {
    float2 Pos       : Pos;
    float  Align     : Align;
    float2 Size      : Size;
    float  SDFOffset : SDFOffset;
    float4 Color     : Color;
    uint   GlyphID   : GlyphID;
    
    uint   VertexID  : SV_VertexID;
};

struct VS_Output {
    float4 Pos      : SV_Position;
    float4 Color    : Color;
    float3 TexCoord : TexCoord;
};

static const float2 QuadVertices[4] = { {-0.5,-0.5}, {-0.5, 0.5}, {0.5, -0.5}, {0.5, 0.5} };

struct AtlasRegion {
    float4 rect;
    float slice;
};
StructuredBuffer<AtlasRegion> AtlasRegions;
float2 InvAtlasSize;

VS_Output VS(VS_Input In) {
    VS_Output Out;
    
    float4 crd = float4(QuadVertices[In.VertexID], 0.0, 1.0);
    crd.xy *= In.Size;
    //todo apply align
    crd.xy += In.Pos;
    
    crd = mul(crd, Transform);
    Out.Pos = mul(crd, UIMatrix);
    
    AtlasRegion region = AtlasRegions[In.GlyphID];
    region.rect *= InvAtlasSize.xyxy;
    Out.TexCoord.xy = lerp(region.rect.xy, region.rect.zw, QuadVertices[In.VertexID] + 0.5);
    Out.TexCoord.z = region.slice;
    
    Out.Color = In.Color;
    
    return Out;
}

Texture2DArray Atlas; SamplerState AtlasSampler;

struct PS_Output {
    float4 Color : SV_Target0;
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    
    float4 c = In.Color;
    c.a *= Atlas.Sample(AtlasSampler, In.TexCoord).r;
    c.a = 1.0;
    c.rgb = abs(Atlas.Sample(AtlasSampler, In.TexCoord).r) * 0.25;
    Out.Color = c;
    
    return Out;
}