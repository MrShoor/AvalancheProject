import bpy
import os
import numpy as np
import bmesh

outfilename = 'E:\\Projects\\AvalancheProject\\avm_export\\test.txt'
        
def Export(WFloat, WInt, WStr, WBool):
    def WriteObjInfo(obj):
        WStr('Obj.Name: '+obj.name)
        if not obj.parent is None:
            WStr('Obj.Parent: '+obj.parent.name)
                
    def WriteMesh(obj):
        WStr('Mesh.Name: '+obj.name)
        obj.update_from_editmode()
        #write vertices data
        m = obj.data
        WInt(len(m.vertices))
        for v in m.vertices:
            WFloat(v.co[0])
            WFloat(v.co[1])
            WFloat(v.co[2])
            WFloat(v.normal[0])
            WFloat(v.normal[1])
            WFloat(v.normal[2])
            WInt(len(v.groups))
            for g in v.groups:
                WInt(g.group)
                WFloat(g.weight)
        #write faces data
        bm = bmesh.new()
        try:
            bm.from_mesh(m)
            bmesh.ops.triangulate(bm, faces=bm.faces)
            for f in bm.faces:
                WInt(f.material_index)
                WBool(f.smooth)
                WFloat(f.normal[0])
                WFloat(f.normal[1])
                WFloat(f.normal[2])
                #for l, v in zip(f.loops, f.verts):
                for v in f.verts:
                    WInt(v.index)
        finally:
            bm.free()
            del bm
        
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