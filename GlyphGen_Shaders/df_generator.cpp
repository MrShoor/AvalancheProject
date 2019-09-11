#include "hlsl.h"

struct VS_Input {
    uint S_VertexID(vid);
};

struct VS_Output {
    float4 S_Position(pos);
};

static const float2 TriCoord[3] = { {-1,-1}, {-1,3}, {3,-1} };

VS_Output VS(VS_Input In) {
    VS_Output Out;
    Out.pos.xy = TriCoord[In.vid];
    Out.pos.zw = float2(0,1);
    return Out;
}

///////////////////////////////////////////////////////////////

struct PS_Output {
    float4 S_Target0(color);
};

float GlyphSize;
StructuredBuffer<float4> Glyph : register(t32);

float Cross2D(float2 v1, float2 v2) {
    return v1.x*v2.y - v1.y*v2.x;
}

float DistanceToLine(float4 seg, float2 pt) {
    float2 segdir = seg.zw - seg.xy;
    float2 ptdir = pt - seg.xy;
    float s = Cross2D(segdir, ptdir);
    if (dot(segdir, ptdir) < 0) return length(ptdir);
    ptdir = pt - seg.zw;
    if (dot(segdir, ptdir) > 0) return length(ptdir);
    return sqrt( s*s/dot(segdir,segdir) );
}

const static float2 eps = {0, 0.001};

uint InTriangle(float4 seg, float2 pt) {
    float d = (seg.x-eps.x)*(seg.w-eps.y)-(seg.z-eps.x)*(seg.y-eps.y);
    if (d == 0) return false;
    float l1 = (seg.w-eps.y)*(pt.x-eps.x)-(seg.z-eps.x)*(pt.y-eps.y);
    float l2 = (seg.x-eps.x)*(pt.y-eps.y)-(seg.y-eps.y)*(pt.x-eps.x);
    l1/=d;
    l2/=d;
    if (l1<=0) return false;
    if (l2<=0) return false;
    if (l1+l2>=1) return false;
    return true;
    //float2 segdir = seg.zw - seg.xy;
    //return !( ( Cross2D(seg.xy, pt) * Cross2D(pt, seg.zw) <= 0 ) || ( Cross2D(segdir, -seg.xy) * Cross2D(segdir, pt-seg.xy) <= 0 ) );
}

PS_Output PS(VS_Output In) {
    PS_Output Out;
    Out.color = 0;

    float mind = 100000.0;
    uint inTri = 0;
    [loop]
    for (int i = 0; i < GlyphSize; i++) {
        mind = min(mind, DistanceToLine(Glyph[i], In.pos.xy));
        inTri += InTriangle(Glyph[i], In.pos.xy);
    }
    Out.color.r = inTri%2 ? -mind : mind;

    return Out;
}