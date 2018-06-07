#include "hlsl.h"
#include "UICommon.h"
#include "hinting.h"

struct VS_Input {
    float2 Coords   : Coords;
    float2 Hinting  : Hinting;
    float4 Color    : Color;
    float2 TexCoord : TexCoord;
    int    SpriteID : SpriteID;
};

struct VS_Output {
    float4 Pos      : SV_Position;
    float3 TexCoord : TexCoord;
    float4 Color    : Color;
};

struct AtlasRegion {
    float4 rect;
    float slice;
};
StructuredBuffer<AtlasRegion> AtlasRegions;
float2 AtlasSize;

VS_Output VS(VS_Input In) {
    VS_Output Out;
    
    float2 crd = mul(float3(In.Coords,1), Mat(RotationOffset.x, RotationOffset.yz)).xy;
    
    Out.Pos = mul(float4(crd, 0.0, 1.0), UIMatrix);
    Out.Pos.z = 0.5;

    Out.Pos = PostHintVertex(Out.Pos, In.Hinting.xy);
    
    Out.Color = In.Color;
    
    if (In.SpriteID < 0) {
        Out.TexCoord = -1;
    } else {
        AtlasRegion region = AtlasRegions[In.SpriteID];
        region.rect /= AtlasSize.xyxy;
        Out.TexCoord.xy = lerp(region.rect.xy, region.rect.zw, In.TexCoord);
        Out.TexCoord.z = region.slice;
    }

    return Out;
}

struct PS_Output {
    float4 Color : SV_Target0;
};

Texture2DArray Atlas; SamplerState AtlasSampler;

PS_Output PS(VS_Output In) {
    PS_Output Out;
    
    float3 texCrd = (In.TexCoord.z < 0) ? 0.0 : In.TexCoord;
    float4 texColor = (In.TexCoord.z < 0) ? 1.0 : Atlas.Sample(AtlasSampler, texCrd);
    Out.Color = In.Color * texColor;
    
    return Out;
}