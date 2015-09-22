import bpy
import os
import numpy as np
import bmesh

OX = 0
OY = 2
OZ = 1
OW = 3
MaxWeightsCount = 4;

#outfilename = 'E:\\Projects\\AvalancheProject\\avm_export\\test.txt'
outfilename = 'E:\\Projects\\AvalancheProject\\Demos\\Src\\avm_Import\\test.txt'
#outfilename = 'C:\\MyProj\\AvalancheProject\\Demos\\Src\\avm_Import\\test.txt'
        
def Export(WFloat, WInt, WStr, WBool):
    poseBoneIndices = {}
    
    def GetPoseBoneIndex(arm, boneName):
        boneDic = poseBoneIndices.get(arm, None)
        if boneDic is None:
            return -1
        return boneDic.get(boneName, -1)
    
    def AddPoseBoneIndex(arm, boneName, index):
        boneDic = poseBoneIndices.get(arm, None)
        if boneDic is None:
            poseBoneIndices[arm] = {}
        poseBoneIndices[arm][boneName] = index
                    
    def WriteMatrix(m):
        WFloat(m[OX][OX])
        WFloat(m[OX][OY])
        WFloat(m[OX][OZ])
        WFloat(m[OX][OW])
        
        WFloat(m[OY][OX])
        WFloat(m[OY][OY])
        WFloat(m[OY][OZ])
        WFloat(m[OY][OW])
        
        WFloat(m[OZ][OX])
        WFloat(m[OZ][OY])
        WFloat(m[OZ][OZ])
        WFloat(m[OZ][OW])
        
        WFloat(m[OW][OX])
        WFloat(m[OW][OY])
        WFloat(m[OW][OZ])
        WFloat(m[OW][OW])
        
    def WriteVec(v):
        WFloat(v[OX])
        WFloat(v[OY])
        WFloat(v[OZ])
                
    def WriteMesh(obj):
        obj.update_from_editmode()
        WStr(obj.name)
        if obj.parent is None:
            WStr('')
        else:
            WStr(obj.parent.name)
        transform = obj.matrix_world
        #write vertices data
        m = obj.data
        WInt(len(m.vertices))
        for v in m.vertices:
            WriteVec(transform*v.co.to_4d())
            WriteVec(transform.to_3x3()*v.normal)
            def RemapG(group):
                return GetPoseBoneIndex(obj.parent, obj.vertex_groups[group.group].name);
            
            gr = [(RemapG(g), g.weight) for g in v.groups if RemapG(g) >= 0]
            gr.sort(key=lambda w: w[1], reverse=True)
            gr = gr[0:MaxWeightsCount]
            WInt(len(gr))
            summW = 0;
            for g in gr:
                summW += g[1]
            for g in gr:
                WInt( g[0] )
                WFloat(g[1]/summW)
        #write faces data
        bm = bmesh.new()
        try:
            bm.from_mesh(m)
            bmesh.ops.triangulate(bm, faces=bm.faces)
            WInt(len(bm.faces))
            for f in bm.faces:
                WInt(f.material_index)
                WBool(f.smooth)
                WFloat(f.normal[OX])
                WFloat(f.normal[OY])
                WFloat(f.normal[OZ])
                #for l, v in zip(f.loops, f.verts):
                for v in f.verts:
                    WInt(v.index)
        finally:
            bm.free()
            del bm
    
    def WriteBone(bone):
        WStr(bone.name)
        if bone.parent is None:
            WStr('')
        else:
            WStr(bone.parent.name)
        WInt(GetPoseBoneIndex(bone.id_data, bone.name))
        WriteMatrix(bone.matrix_channel*bone.id_data.matrix_world.inverted())
        #WriteMatrix(bone.matrix)
        #WriteMatrix( bone.id_data.children[0].convert_space(bone, bone.matrix_channel, 'WORLD', 'WORLD') )
    
    def WriteArmature(obj):
        obj.update_from_editmode()
        WStr(obj.name)
        
        #indexing bones
        i = 0
        for b in obj.pose.bones:
            AddPoseBoneIndex(obj, b.name, i)
            i += 1

        WInt(len(obj.pose.bones))
        for b in obj.pose.bones:
            WriteBone(b)
        
    def WriteObject(obj):
        switch = {
            'MESH': WriteMesh,
        }
        delegate = switch.get(obj.type, None)
        if not delegate is None:
            delegate(obj)

    WInt(len([a for a in bpy.data.objects if a.type=='ARMATURE']));
    for a in bpy.data.objects:
        if a.type=='ARMATURE':
            WriteArmature(a)

    WInt(len([m for m in bpy.data.objects if m.type=='MESH']));
    for m in bpy.data.objects:
        if m.type=='MESH':
            WriteMesh(m)
            
    #bpy.data.actions

def ExportToFile(fname):
    if os.path.isfile(fname):
        os.remove(outfilename)
    outfile = open(outfilename, 'wb')
    try:
        def WFloat(value):
            outfile.write(np.float32(value))
        def WInt(value):
            outfile.write(np.int32(value))
        def WStr(value):
            b = value.encode('utf-8');
            WInt(len(b))
            outfile.write(b)
        def WBool(value):
            if value:
                outfile.write(np.ubyte(1))
            else:
                outfile.write(np.ubyte(0))
            
        Export(WFloat, WInt, WStr, WBool)
    finally:
        outfile.close()
        print('Done!')
        
def ExportToConsole():    
    def WFloat(value):
        print('f32: '+str(value))
    def WInt(value):
        print('i32: '+str(value))
    def WStr(value):
        print('s  : '+value)
    def WBool(value):
        if value:            
            print('b  : True')
        else:
            print('b  : False')
    print('---------')
    Export(WFloat, WInt, WStr, WBool)
            
ExportToFile(outfilename)
#ExportToConsole()