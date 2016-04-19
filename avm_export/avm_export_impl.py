import bpy
import os
import numpy as np
import bmesh
import shutil as sh

OX = 0
OY = 2
OZ = 1
OW = 3
MaxWeightsCount = 4;

#outfilename = 'E:\\Projects\\AvalancheProject\\avm_export\\test.txt'
outfilename = 'E:\\Projects\\AvalancheProject\\Demos\\Media\\WhipperNude\\WhipperNude.avm'
#outfilename = 'E:\\Projects\\AvalancheProject\\Demos\\Media\\NewI\\mesh.avm'
#outfilename = 'C:\\MyProj\\AvalancheProject\\Demos\\Src\\avm_Import\\test.txt'
        
def Export(WFloat, WInt, WStr, WBool):
    poseBoneIndices = {}
    imgToCopy = {}
    
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
        
    def AddImageToCopy(path, name):
        if path in imgToCopy:
            return imgToCopy[path]

        ind = 0
        tmpname = name
        while tmpname in imgToCopy.values():
            ind += 1
            tmpname = name + '_' + str(ind)
        imgToCopy[path] = tmpname
        
        return tmpname
                    
    def WriteMatrix(m):
        WFloat(m[OX][OX])
        WFloat(m[OY][OX])
        WFloat(m[OZ][OX])
        WFloat(m[OW][OX])
        
        WFloat(m[OX][OY])
        WFloat(m[OY][OY])
        WFloat(m[OZ][OY])
        WFloat(m[OW][OY])
        
        WFloat(m[OX][OZ])
        WFloat(m[OY][OZ])
        WFloat(m[OZ][OZ])
        WFloat(m[OW][OZ])
        
        WFloat(m[OX][OW])
        WFloat(m[OY][OW])
        WFloat(m[OZ][OW])
        WFloat(m[OW][OW])
        
    def WriteVec(v):
        WFloat(v[OX])
        WFloat(v[OY])
        WFloat(v[OZ])
        
    def WriteTexVec(v):
        WFloat(v[0])
        WFloat(1.0-v[1])
        
    def WriteColor(c):
        WFloat(c[0])
        WFloat(c[1])
        WFloat(c[2])
        WFloat(c[3])

    def WriteMesh(mesh):
        WStr(mesh.name)
        
        #write materials
        materials = mesh.materials;
        if len(materials) == 0:
            materials = [None] #allocate minum one material
        WInt(len(materials))
        for mat in materials:
            diffuseColor = [1,1,1,1]
            specularColor = [1,1,1,1]
            specularPower = 50
            diffuseMap = ''
            diffuseMapFactor = 0
            normalMap = ''
            if (not mat is None):
                diffuseColor = [c*mat.diffuse_intensity for c in mat.diffuse_color]
                diffuseColor.append(mat.alpha)
                specularColor = [c*mat.specular_intensity for c in mat.specular_color]
                specularColor.append(mat.specular_alpha)
                specularPower = mat.specular_hardness
                for ts in mat.texture_slots:
                    if (not ts is None) and (not ts.texture is None) and (ts.texture.type == 'IMAGE') and (not ts.texture.image is None):
                        fpath = ts.texture.image.filepath
                        if fpath.find(r'\\') == 0:
                            fpath = '//'+fpath[2:]
                        absTexPath = bpy.path.abspath(fpath)
                        if os.path.isfile(absTexPath):                            
                            if ts.use_map_color_diffuse:
                                diffuseMapFactor = ts.diffuse_color_factor
                                diffuseMap = AddImageToCopy(absTexPath, ts.texture.image.name)
                            elif ts.use_map_normal:
                                normalMap = AddImageToCopy(absTexPath, ts.texture.image.name)
                        else:
                            print('Warning! Texture "' + absTexPath + '" not found')
            WriteColor(diffuseColor)
            WFloat(diffuseMapFactor)
            WriteColor(specularColor)
            WFloat(specularPower)
            WStr(diffuseMap)
            WStr(normalMap)

        #write vertices data
        WInt(len(mesh.vertices))
        for v in mesh.vertices:
            WriteVec(v.co.to_4d())
            WriteVec(v.normal)

            gr = [(g.group, g.weight) for g in v.groups if g.group >= 0]
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
            
            uv_layer = bm.loops.layers.uv.active
            #uv_tex = bm.faces.layers.tex.active
            WInt(len(bm.faces))
            for f in bm.faces:
                #print(f[uv_tex].image.name)
                WInt(f.material_index)
                WBool(f.smooth)
                WriteVec(f.normal)
                for l, v in zip(f.loops, f.verts):
                    WInt(v.index)
                    if uv_layer is None:
                        WriteTexVec([0,0])
                    else:
                        WriteTexVec(l[uv_layer].uv)
        finally:
            bm.free()
            del bm

    def WriteMeshInstance(obj):
        obj.update_from_editmode()
        WStr(obj.name)
        if obj.parent is None:
            WStr('')
        else:
            WStr(obj.parent.name)
        WriteMatrix(obj.matrix_local)
        WStr(obj.data.name)
        WInt(len(obj.vertex_groups))
        for vg in obj.vertex_groups:
            WStr(vg.name)
    
    def GetPoseBoneAbsTransform(bone):
        #return bone.id_data.matrix_world*bone.matrix_channel*bone.id_data.matrix_world.inverted()
        return bone.matrix_channel
    
    def GetPoseBoneTransform(bone):
        m = GetPoseBoneAbsTransform(bone)
        if not (bone.parent is None):
            m2 = GetPoseBoneAbsTransform(bone.parent)
            m = m2.inverted()*m
        return m
    
    def WriteBone(bone):
        WStr(bone.name)
        if bone.parent is None:
            WStr('')
        else:
            WStr(bone.parent.name)
        WInt(GetPoseBoneIndex(bone.id_data, bone.name))
        WriteMatrix(GetPoseBoneTransform(bone))
        #WriteMatrix( bone.id_data.children[0].convert_space(bone, bone.matrix_channel, 'WORLD', 'WORLD') )        
    
    def WriteArmature(obj):
        obj.update_from_editmode()
        WStr(obj.name)
        
        #indexing bones
        i = 0
        for b in obj.pose.bones:
            AddPoseBoneIndex(obj, b.name, i)
            i += 1
            
        #saving bone
        WInt(len(obj.pose.bones))
        for b in obj.pose.bones:
            WriteBone(b)
            
        #saving animations
        def GetAffectedBones(action):
            return [g.name for g in action.groups if GetPoseBoneIndex(obj, g.name)>=0]
        
        actionsCount = 0
        for act in bpy.data.actions:
            if len(GetAffectedBones(act)) > 0:
                actionsCount += 1
        WInt(actionsCount)
        for act in bpy.data.actions:
            aBones = GetAffectedBones(act)
            if len(aBones) == 0:
                continue
            WStr(act.name)
            WInt(len(aBones))
            for bName in aBones:
                WInt(GetPoseBoneIndex(obj, bName))
            
            oldAction = obj.animation_data.action
            oldFrame = bpy.context.scene.frame_current
            try:
                obj.animation_data.action = act
                frameStart = int(act.frame_range[0])
                frameEnd = int(act.frame_range[1])
                WInt(frameStart)
                WInt(frameEnd)
                for frame in range(frameStart, frameEnd):
                    bpy.context.scene.frame_set(frame)
                    for bName in aBones:
                        WriteMatrix(GetPoseBoneTransform(obj.pose.bones[bName]))
            finally:
                bpy.context.scene.frame_set(oldFrame)
                obj.animation_data.action = oldAction

    armatures = [a for a in bpy.data.objects if (a.type=='ARMATURE') and (len(a.users_scene)>0)]
    WInt(len(armatures))
    for a in armatures:
        WriteArmature(a)

    meshes = bpy.data.meshes
    WInt(len(meshes))
    for m in meshes:
        WriteMesh(m);
        
    inst = [m for m in bpy.data.objects if (m.type=='MESH') and (len(m.users_scene)>0)]    
    WInt(len(inst))
    for obj in inst:
        WriteMeshInstance(obj)
           
    return imgToCopy

def ExportToFile(fname):
    if os.path.isfile(fname):
        os.remove(fname)
    outfile = open(fname, 'wb')
    ExportDone = False
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
            
        images = Export(WFloat, WInt, WStr, WBool)
        outdir = os.path.dirname(fname)
        for path, outname in images.items():
            if path != os.path.join(outdir, outname):
                sh.copyfile(path, os.path.join(outdir, outname))
        ExportDone = True
    finally:
        outfile.close()
        if not ExportDone:
            print('Failed!')
            os.remove(fname)
        else:
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
            
#ExportToFile(outfilename)
#ExportToConsole()