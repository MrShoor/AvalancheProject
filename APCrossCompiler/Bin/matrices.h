#ifndef MATRICES_H
#define	MATRICES_H
#pragma pack_matrix( row_major )
float4x4 P_Matrix;
float4x4 P_InverseMatrix;
float4x4 VP_Matrix;
float4x4 VP_InverseMatrix;
float4x4 V_Matrix;
float4x4 V_InverseMatrix;
float2   FBOFlip;
#endif	/* MATRICES_H */