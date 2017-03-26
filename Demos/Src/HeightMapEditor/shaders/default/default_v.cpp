#include "hlsl.h"
#include "matrices.h"
#include "HeightMap.h"

//VS
struct VS_Input {
    float2 vsCoord  : vsCoord;

    float4 aiPosSize     : aiPosSize;
    float4 aiBorderDelta : aiBorderDelta;
    float2 aiQuadDelta   : aiQuadDelta;    
};


struct VS_Data {
    float3 vsCoord  : vsCoord;
};

float4 fArea;
float CellSize;

VS_Data VS(VS_Input In) {
    VS_Data Out;
    
//    int4 Area = (int4) fArea;
//    uint ID = In.InstanceID;
//        
//    Out.vsCoord.x = ID % Area.z + Area.x;
//    Out.vsCoord.y = ID / Area.z + Area.y;
//    Out.vsCoord.xy *= CellSize;
//    Out.vsCoord.xy += In.vsCoord*CellSize;
//    Out.vsCoord = GetMapCoord(Out.vsCoord.xy, 0);
    Out.vsCoord.xy = In.vsCoord * In.aiPosSize.zw + In.aiPosSize.xy;
    Out.vsCoord = GetMapCoord(Out.vsCoord.xy, 0);

    return Out;
}
//end of VS

//HS
struct HS_ConstOut {
    float edges[4]  : SV_TessFactor;
    float inside[2] : SV_InsideTessFactor;
};

float2 ViewPortSize;

float CalcSegmentTessLevel(float2 pt1, float2 pt2) {
    float Out;    
    Out = length(pt1-pt2) / 32.0;//48.0;
    Out = clamp(Out, 1.0, CellSize*2);
    //return CellSize*3;
    return Out;
}

float CalcDistanceTessLevel(float z) {
    float Out;    
    //Out = 1000.0/z;//48.0;
    Out = 100.0*z;
    Out = clamp(Out, 1.0, CellSize*2);
    //return CellSize*3;
    return Out;
}

HS_ConstOut ScreenSpaceConstantFunction(InputPatch<VS_Data, 4> Patch, uint patchId : SV_PrimitiveID) {    
    HS_ConstOut Out;

    float4 pts[4];
    for (int i = 0; i < 4; i++) {
        pts[i] = mul(float4(Patch[i].vsCoord, 1.0), VP_Matrix);
        pts[i].xy /= pts[i].w;
        pts[i].xy *= ViewPortSize;
    }
        
    // Set the tessellation factors for the three edges of the triangle.
    Out.edges[0] = CalcSegmentTessLevel(pts[0].xy, pts[1].xy);
    Out.edges[1] = CalcSegmentTessLevel(pts[1].xy, pts[3].xy);
    Out.edges[2] = CalcSegmentTessLevel(pts[3].xy, pts[2].xy);
    Out.edges[3] = CalcSegmentTessLevel(pts[2].xy, pts[0].xy);
    
    // Set the tessellation factor for tessallating inside the triangle.
    Out.inside[0] = max(Out.edges[1], Out.edges[3]);
    Out.inside[1] = max(Out.edges[0], Out.edges[2]);

    return Out;
}

HS_ConstOut EdgeDistanceConstantFunction(InputPatch<VS_Data, 4> Patch, uint patchId : SV_PrimitiveID) {    
    HS_ConstOut Out;
    float tessellationAmount = 2.0;   
    
    float4 vCrd[4];
    float k[4];
    for (int i = 0; i < 4; i++) {
        vCrd[i] = mul(float4(Patch[i].vsCoord,1.0), V_Matrix);
    }
    k[0] = length(vCrd[0].xyz - vCrd[1].xyz)/max(1.0, (vCrd[0].z + vCrd[1].z));
    k[1] = length(vCrd[1].xyz - vCrd[3].xyz)/max(1.0, (vCrd[1].z + vCrd[3].z));
    k[2] = length(vCrd[3].xyz - vCrd[2].xyz)/max(1.0, (vCrd[3].z + vCrd[2].z));
    k[3] = length(vCrd[2].xyz - vCrd[0].xyz)/max(1.0, (vCrd[2].z + vCrd[0].z));
        
    // Set the tessellation factors for the three edges of the triangle.
    Out.edges[0] = CalcDistanceTessLevel(k[0]);
    Out.edges[1] = CalcDistanceTessLevel(k[1]);
    Out.edges[2] = CalcDistanceTessLevel(k[2]);
    Out.edges[3] = CalcDistanceTessLevel(k[3]);

    // Set the tessellation factor for tessallating inside the triangle.
    Out.inside[0] = max(Out.edges[1], Out.edges[3]);
    Out.inside[1] = max(Out.edges[0], Out.edges[2]);

    return Out;
}

[domain("quad")]
//[partitioning("fractional_odd")]
[partitioning("integer")]
[outputtopology("triangle_cw")]
[outputcontrolpoints(4)]
[patchconstantfunc("EdgeDistanceConstantFunction")]
VS_Data HS(InputPatch<VS_Data, 4> patch, uint pointId : SV_OutputControlPointID, uint patchId : SV_PrimitiveID) {
    VS_Data Out;
    Out = patch[pointId];
    return Out;
}
//end of HS

//DS
struct DS_Output {
    float3 wCoord: wCoord;
    float3 vCoord: vCoord;
    float2 vHMTex: vHMTex;
    float3 vNorm : vNorm;
    float4 Pos: SV_Position;
};

[domain("quad")]
DS_Output DS(HS_ConstOut ConstIn, float2 uvCoord: SV_DomainLocation, OutputPatch<VS_Data, 4> patch) {
    VS_Data In;
    float3 v1 = lerp(patch[0].vsCoord, patch[2].vsCoord, uvCoord.x);
    float3 v2 = lerp(patch[1].vsCoord, patch[3].vsCoord, uvCoord.x);
    In.vsCoord = lerp(v2, v1, uvCoord.y);
    DS_Output Out;
    float2 texSize;
    HeightMap.GetDimensions(texSize.x, texSize.y);
    
    float2 coord2D = In.vsCoord.xy;
    
    Out.vHMTex = GetTexCoord(coord2D);
    GetMapCoordWithNormal(coord2D, 0.0, Out.wCoord, Out.vNorm);
    //Out.wCoord = float3(coord2D, 0.0);
    //Out.vNorm = float3(0,0,1.0);
    Out.vNorm = mul(Out.vNorm, (float3x3) V_Matrix);
    
    Out.vCoord = mul(float4(Out.wCoord, 1.0), V_Matrix).xyz;
    Out.Pos = mul(float4(Out.vCoord, 1.0), P_Matrix);
    return Out;
}
//end of DS

//PS
struct PS_Output {
    float4 Color : SV_Target0;
};

Texture2DArray MapTiles; SamplerState MapTilesSampler;
Texture2D MapTileInfo; SamplerState MapTileInfoSampler;

float3 GetPixelColor(float2 texCoord) {
    float2 texSize;
    MapTileInfo.GetDimensions(texSize.x, texSize.y);
    texCoord *= texSize;
    float4 lowhi = float4(floor(texCoord), ceil(texCoord));
    float2 k = texCoord - lowhi.xy;
    lowhi /= texSize.xyxy;
    
    texCoord *= 0.5;
    
    float4 col[4];
    col[0] = MapTiles.Sample(MapTilesSampler, float3(texCoord, MapTileInfo.Sample(MapTileInfoSampler, lowhi.xy).r*255) );
    col[1] = MapTiles.Sample(MapTilesSampler, float3(texCoord, MapTileInfo.Sample(MapTileInfoSampler, lowhi.xw).r*255) );
    col[2] = MapTiles.Sample(MapTilesSampler, float3(texCoord, MapTileInfo.Sample(MapTileInfoSampler, lowhi.zy).r*255) );
    col[3] = MapTiles.Sample(MapTilesSampler, float3(texCoord, MapTileInfo.Sample(MapTileInfoSampler, lowhi.zw).r*255) );
    
    col[0] = lerp(col[0], col[2], k.x);
    col[1] = lerp(col[1], col[3], k.x);
    return lerp(col[0], col[1], k.y).xyz;
}

PS_Output PS(DS_Output In) {
    PS_Output Out;
    In.vNorm = normalize(In.vNorm);
    In.vNorm = GetMapNormal(In.wCoord.xy);
    In.vNorm = mul(In.vNorm, (float3x3) V_Matrix);
    
    float diffK = dot(normalize(mul(float3(1,1,1), (float3x3)V_Matrix)), -In.vNorm);
//    Out.Color.rgb = GetPixelColor(In.vHMTex)*diffK;
    Out.Color.rgb = diffK;
    Out.Color.a = 1.0;
    return Out;
}
//end of PS