#include "hlsl.h"

struct VS_Input {
    uint VertexID: SV_VertexID;
};

struct VS_Output {
    float4 aiPosSize     : aiPosSize;
    float4 aiBorderDelta : aiBorderDelta;
    float2 aiQuadDelta   : aiQuadDelta;
};

float2 fArea;
float CellSize;
float HeightScale;
Texture2D HeightMap; SamplerState HeightMapSampler;

float GetEdgeMaxSlope(float2 pt1, float2 pt2) {
    float maxdelta = 0;
    float2 texSize;
    HeightMap.GetDimensions(texSize.x, texSize.y);
    float2 ptex1 = pt1/texSize;
    float2 ptex2 = pt2/texSize;
    
    float currZ, prevZ;
    prevZ = HeightMap.SampleLevel(HeightMapSampler, ptex1, 0).r*HeightScale;
    for (uint i = 1; i < (uint)abs(CellSize); i++) {
        float k = saturate(i / (CellSize - 1));
        float2 t = lerp(ptex1, ptex2, k);        
        currZ = HeightMap.SampleLevel(HeightMapSampler, t, 0).r*HeightScale;
        maxdelta = max(maxdelta, abs(currZ - prevZ));
        prevZ = currZ;
    }
    return maxdelta;
}


float2 GetCellMaxSlope(float2 pt1, float2 pt2) {
    float2 Out = 0.0;
    uint i;
    for (i = 0; i < (uint)abs(CellSize); i++) {
        float k = saturate(i / (CellSize - 1));
        float x = lerp(pt1.x, pt2.x, k);
        Out.x = max(Out.x, GetEdgeMaxSlope(float2(x, pt1.y), float2(x, pt2.y)));       
    }
    
    for (i = 0; i < (uint)abs(CellSize); i++) {
        float k = saturate(i / (CellSize - 1));
        float y = lerp(pt1.y, pt2.y, k);
        Out.y = max(Out.y, GetEdgeMaxSlope(float2(pt1.x, y), float2(pt2.x, y)));       
    }
    
    return Out;
}

VS_Output VS(VS_Input In) {
    VS_Output Out;
    int2 Area = (int2) fArea;
    uint ID = In.VertexID;    
    Out.aiPosSize.x = ID % Area.x;
    Out.aiPosSize.y = ID / Area.x;
    Out.aiPosSize.xy *= CellSize;
    Out.aiPosSize.zw = CellSize;
    
    //0 0 - 0 1 : 0 1
    //0 1 - 1 1 : 1 3
    //1 1 - 1 0 : 3 2
    //1 0 - 0 0 : 3 0
    
    Out.aiBorderDelta.x = GetEdgeMaxSlope(Out.aiPosSize.xy + Out.aiPosSize.zw * float2(0,0),
                                          Out.aiPosSize.xy + Out.aiPosSize.zw * float2(0,1));
    Out.aiBorderDelta.y = GetEdgeMaxSlope(Out.aiPosSize.xy + Out.aiPosSize.zw * float2(0,1),
                                          Out.aiPosSize.xy + Out.aiPosSize.zw * float2(1,1));
    Out.aiBorderDelta.z = GetEdgeMaxSlope(Out.aiPosSize.xy + Out.aiPosSize.zw * float2(1,1),
                                          Out.aiPosSize.xy + Out.aiPosSize.zw * float2(1,0));
    Out.aiBorderDelta.w = GetEdgeMaxSlope(Out.aiPosSize.xy + Out.aiPosSize.zw * float2(1,0),
                                          Out.aiPosSize.xy + Out.aiPosSize.zw * float2(0,0));
    //Out.aiBorderDelta = 2.0;
    Out.aiQuadDelta = GetCellMaxSlope(Out.aiPosSize.xy, Out.aiPosSize.xy + Out.aiPosSize.zw);
    return Out;
}