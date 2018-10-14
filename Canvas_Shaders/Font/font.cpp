#include "hlsl.h"
#include "UICommon.h"

float3 XBoundsYPos;

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
float2 AtlasSize;

VS_Output VS(VS_Input In) {
    VS_Output Out;
    
    float4 crd = float4(QuadVertices[In.VertexID], 0.0, 1.0);
    crd.xy *= In.Size;
    crd.x += lerp(XBoundsYPos.x, XBoundsYPos.y, In.Align);
    crd.y += XBoundsYPos.z;
    crd.xy += In.Pos;
    
    crd.xy = mul(float3(crd.xy,1), CanvasTransform()).xy;
    Out.Pos = mul(crd, UIMatrix);
    Out.Pos.z = ZValue * Out.Pos.w;
    
    AtlasRegion region = AtlasRegions[In.GlyphID];
    region.rect /= AtlasSize.xyxy;
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
    
    float tex_per_pixel = length(ddy(In.TexCoord));// + ddy(In.TexCoord));
    tex_per_pixel *= AtlasSize.x;
    
    float4 c = In.Color;
    float Y = dot(c.xyz, float3(0.212656, 0.715158, 0.072186));
    float r = Atlas.Sample(AtlasSampler, In.TexCoord).r;
    c.a *= saturate(-r/tex_per_pixel + 0.5 + lerp(0.3, 0.0, Y));
    c.rgb *= c.a;
    
    Out.Color = c;
    
    return Out;
}