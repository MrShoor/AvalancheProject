#include "hlsl.h"
#include "UICommon.h"

float4 BoundsXY;
float2 YPos_ClipWithBounds;

struct VS_Input {
    float2 S_(Pos);
    float  S_(Align);
    float2 S_(Size);
    float  S_(SDFOffset);
    float4 S_(Color);
    uint   S_(GlyphID);
    
    uint   S_VertexID(VertexID);
};

struct VS_Output {
    float4 S_Position(Pos);
    float2 S_(BoundsPos);
    float4 S_(Color);
    float3 S_(TexCoord);
    float  S_(SDFOffset);
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
    crd.x += lerp(BoundsXY.x, BoundsXY.y, In.Align);
    crd.y += YPos_ClipWithBounds.x;
    crd.xy += In.Pos;
    Out.BoundsPos = crd.xy;
    
    crd.xy = mul(float3(crd.xy,1), CanvasTransform()).xy;
    Out.Pos = mul(crd, UIMatrix);
    Out.Pos.z = ZValue * Out.Pos.w;
    
    AtlasRegion region = AtlasRegions[In.GlyphID];
    region.rect /= AtlasSize.xyxy;
    Out.TexCoord.xy = lerp(region.rect.xy, region.rect.zw, QuadVertices[In.VertexID] + 0.5);
    Out.TexCoord.z = region.slice;
    
    Out.Color = In.Color;
    Out.SDFOffset = In.SDFOffset;
    
    return Out;
}

Texture2DArray Atlas; SamplerState AtlasSampler;

struct PS_Output {
    float4 S_Target0(Color);
};

PS_Output PS(VS_Output In) {
    PS_Output Out;
    
    float tex_per_pixel = length(ddy(In.TexCoord));// + ddy(In.TexCoord));
    tex_per_pixel *= AtlasSize.x;
    
    if (YPos_ClipWithBounds.y) {
        if ( (In.BoundsPos.x < BoundsXY.x) || (In.BoundsPos.x > BoundsXY.y) ||
             (In.BoundsPos.y < BoundsXY.z) || (In.BoundsPos.y > BoundsXY.w) )
            discard;
    }
    
    float4 c = In.Color;
    float Y = dot(c.xyz, float3(0.212656, 0.715158, 0.072186));
    float r = Atlas.Sample(AtlasSampler, In.TexCoord).r + In.SDFOffset;
    c.a *= saturate(-r/tex_per_pixel + 0.5 + lerp(0.3, 0.0, Y));
    c.xyz *= c.a;
    
    Out.Color = c;
    
    return Out;
}