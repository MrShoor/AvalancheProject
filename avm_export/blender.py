import bpy
import os
import numpy as np
import bmesh

#outfilename = 'E:\\Projects\\AvalancheProject\\avm_export\\test.txt'
outfilename = 'E:\\Projects\\AvalancheProject\\Demos\\Src\\avm_Import\\test.txt'
        
def Export(WFloat, WInt, WStr, WBool):
    def WriteObjInfo(obj):
        WStr('Obj.Name: '+obj.name)
        if not obj.parent is None:
            WStr('Obj.Parent: '+obj.parent.name)
                
    def WriteMesh(obj):
        WStr(obj.name)
        obj.update_from_editmode()
        #write vertices groups
        WInt(len(obj.vertex_groups))
        for vg in obj.vertex_groups:
            WStr(vg.name)
            WInt(vg.index)
        #write vertices data
        m = obj.data
        WInt(len(m.vertices))
        for v in m.vertices:
            WFloat(v.co[0])
            WFloat(v.co[2])
            WFloat(v.co[1])
            WFloat(v.normal[0])
            WFloat(v.normal[2])
            WFloat(v.normal[1])
            WInt(len(v.groups))
            for g in v.groups:
                WInt(g.group)
                WFloat(g.weight)
        #write faces data
        bm = bmesh.new()
        try:
            bm.from_mesh(m)
            bmesh.ops.triangulate(bm, faces=bm.faces)
            WInt(len(bm.faces))
            for f in bm.faces:
                WInt(f.material_index)
                WBool(f.smooth)
                WFloat(f.normal[0])
                WFloat(f.normal[2])
                WFloat(f.normal[1])
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
        WFloat(bone.matrix_local[0][0])
        WFloat(bone.matrix_local[0][2])
        WFloat(bone.matrix_local[0][1])
        WFloat(bone.matrix_local[0][3])
        
        WFloat(bone.matrix_local[2][0])
        WFloat(bone.matrix_local[2][2])
        WFloat(bone.matrix_local[2][1])
        WFloat(bone.matrix_local[2][3])
        
        WFloat(bone.matrix_local[1][0])
        WFloat(bone.matrix_local[1][2])
        WFloat(bone.matrix_local[1][1])
        WFloat(bone.matrix_local[1][3])
        
        WFloat(bone.matrix_local[3][0])
        WFloat(bone.matrix_local[3][2])
        WFloat(bone.matrix_local[3][1])
        WFloat(bone.matrix_local[3][3])        
    
    def WriteArmature(obj):
        WStr(obj.name)
        obj.update_from_editmode()
        
        WInt(len(obj.children))
        for c in obj.children:
            WStr(c.name)
            
        WInt(len(obj.data.bones))
        for b in obj.data.bones:
            WriteBone(b)
        
    def WriteObject(obj):
        switch = {
            'MESH': WriteMesh,
        }
        delegate = switch.get(obj.type, None)
        if not delegate is None:
            delegate(obj)
        
    WInt(len([m for m in bpy.data.objects if m.type=='MESH']));
    for m in bpy.data.objects:
        if m.type=='MESH':
            WriteMesh(m)
        
    WInt(len([a for a in bpy.data.objects if a.type=='ARMATURE']));
    for a in bpy.data.objects:
        if a.type=='ARMATURE':
            WriteArmature(a)
            
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
ExportToConsole()