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
#outfilename = 'E:\\Projects\\AvalancheProject\\Demos\\Media\\WhipperNude\\WhipperNude.avm'
#outfilename = 'E:\\Projects\\AvalancheProject\\Demos\\Media\\NewI\\mesh.avm'
outfilename = 'D:\\Projects\\AvalancheProject\\Demos\\Media\\Char\char.avm'
#outfilename = 'C:\\MyProj\\AvalancheProject\\Demos\\Src\\avm_Import\\test.txt'
        
def Export(WFloat, WInt, WStr, WBool):
    poseBoneIndices = {}
    imgToCopy = {}
    meshToVertexGroup = {}
    
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
            m_diffuseColor = [1,1,1,1]
            m_specularColor = [1,1,1,1]
            m_specularHardness = 50
            m_specularIOR = 0
            m_emitFactor = 0;
            
            diffuseMap_Intensity = ''
            diffuseMap_IntensityFactor = 0            
            diffuseMap_Color = ''
            diffuseMap_ColorFactor = 0
            diffuseMap_Alpha = ''
            diffuseMap_AlphaFactor = 0
            diffuseMap_Translucency = ''
            diffuseMap_TranslucencyFactor = 0
            shadingMap_Ambient = ''
            shadingMap_AmbientFactor = 0
            shadingMap_Emit = ''
            shadingMap_EmitFactor = 0
            shadingMap_Mirror = ''
            shadingMap_MirrorFactor = 0
            shadingMap_RayMirror = ''
            shadingMap_RayMirrorFactor = 0        
            specularMap_Intensity = ''
            specularMap_IntensityFactor = 0
            specularMap_Color = ''
            specularMap_ColorFactor = 0
            specularMap_Hardness = ''
            specularMap_HardnessFactor = 0     
            geometryMap_Normal = ''
            geometryMap_NormalFactor = 0
            geometryMap_Warp = ''
            geometryMap_WarpFactor = 0
            geometryMap_Displace = ''
            geometryMap_DisplaceFactor = 0
            
            if (not mat is None):
                m_diffuseColor = [c*mat.diffuse_intensity for c in mat.diffuse_color]
                m_diffuseColor.append(mat.alpha)
                m_specularColor = [c*mat.specular_intensity for c in mat.specular_color]
                m_specularColor.append(mat.specular_alpha)
                m_specularHardness = mat.specular_hardness
                m_specularIOR = mat.specular_ior
                m_emitFactor = mat.emit
                
                for ts in mat.texture_slots:
                    if (not ts is None) and (ts.use) and (not ts.texture is None) and (ts.texture.type == 'IMAGE') and (not ts.texture.image is None):
                        fpath = ts.texture.image.filepath
                        if fpath.find(r'\\') == 0:
                            fpath = '//'+fpath[2:]
                        absTexPath = bpy.path.abspath(fpath)
                        if os.path.isfile(absTexPath):                            
                            if ts.use_map_diffuse:
                                diffuseMap_Intensity = AddImageToCopy(absTexPath, ts.texture.image.name)
                                diffuseMap_IntensityFactor = ts.diffuse_factor
                            elif ts.use_map_color_diffuse:
                                diffuseMap_Color = AddImageToCopy(absTexPath, ts.texture.image.name)
                                diffuseMap_ColorFactor = ts.diffuse_color_factor
                            elif ts.use_map_alpha:
                                diffuseMap_Alpha = AddImageToCopy(absTexPath, ts.texture.image.name)
                                diffuseMap_AlphaFactor = ts.alpha_factor
                            elif ts.use_map_translucency:
                                diffuseMap_Translucency = AddImageToCopy(absTexPath, ts.texture.image.name)
                                diffuseMap_TranslucencyFactor = ts.alpha_factor
                            elif ts.use_map_ambient:
                                shadingMap_Ambient = AddImageToCopy(absTexPath, ts.texture.image.name)
                                shadingMap_AmbientFactor = ts.ambient_factor
                            elif ts.use_map_emit:
                                shadingMap_Emit = AddImageToCopy(absTexPath, ts.texture.image.name)
                                shadingMap_EmitFactor = ts.emit_factor
                            elif ts.use_map_mirror:
                                shadingMap_MirrorFactor = AddImageToCopy(absTexPath, ts.texture.image.name)
                                shadingMap_MirrorFactor = ts.mirror_factor
                            elif ts.use_map_raymir:
                                shadingMap_RayMirrorFactor = AddImageToCopy(absTexPath, ts.texture.image.name)
                                shadingMap_RayMirrorFactor = ts.raymir_factor
                            elif ts.use_map_reflect:
                                specularMap_Intensity = AddImageToCopy(absTexPath, ts.texture.image.name)
                                specularMap_IntensityFactor = ts.specular_factor
                            elif ts.use_map_color_spec:
                                specularMap_Color = AddImageToCopy(absTexPath, ts.texture.image.name)
                                specularMap_ColorFactor = ts.specular_color_factor
                            elif ts.use_map_hardness:
                                specularMap_Hardness = AddImageToCopy(absTexPath, ts.texture.image.name)
                                specularMap_HardnessFactor = ts.hardness_factor
                            elif ts.use_map_normal:
                                geometryMap_Normal = AddImageToCopy(absTexPath, ts.texture.image.name)
                                geometryMap_NormalFactor = ts.normal_factor                                
                            elif ts.use_map_warp:
                                geometryMap_Warp = AddImageToCopy(absTexPath, ts.texture.image.name)
                                geometryMap_WarpFactor = ts.warp_factor
                            elif ts.use_map_displacement:
                                geometryMap_Displace = AddImageToCopy(absTexPath, ts.texture.image.name)
                                geometryMap_DisplaceFactor = ts.displacement_factor
                        else:
                            print('Warning! Texture "' + absTexPath + '" not found')                     
                            
            WriteColor(m_diffuseColor)
            WriteColor(m_specularColor)
            WFloat(m_specularHardness)
            WFloat(m_specularIOR)
            WFloat(m_emitFactor)
            
            WStr(diffuseMap_Intensity)
            WFloat(diffuseMap_IntensityFactor)
            WStr(diffuseMap_Color)
            WFloat(diffuseMap_ColorFactor)
            WStr(diffuseMap_Alpha)
            WFloat(diffuseMap_AlphaFactor)
            WStr(diffuseMap_Translucency)
            WFloat(diffuseMap_TranslucencyFactor)
            WStr(shadingMap_Ambient)
            WFloat(shadingMap_AmbientFactor)
            WStr(shadingMap_Emit)
            WFloat(shadingMap_EmitFactor)
            WStr(shadingMap_Mirror)
            WFloat(shadingMap_MirrorFactor)
            WStr(shadingMap_RayMirror)
            WFloat(shadingMap_RayMirrorFactor)
            WStr(specularMap_Intensity)
            WFloat(specularMap_IntensityFactor)
            WStr(specularMap_Color)
            WFloat(specularMap_ColorFactor)
            WStr(specularMap_Hardness)
            WFloat(specularMap_HardnessFactor)
            WStr(geometryMap_Normal)
            WFloat(geometryMap_NormalFactor)
            WStr(geometryMap_Warp)
            WFloat(geometryMap_WarpFactor)
            WStr(geometryMap_Displace)
            WFloat(geometryMap_DisplaceFactor)
            
        #write vertex groups
        vg = meshToVertexGroup.get(mesh, None)
        if (not vg is None):
            WInt(len(vg))
            for g in vg:
                WStr(g.name)
        else:
            WInt(0);
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
        WriteVec(bone.head)
        WriteVec(bone.tail)
        #WriteMatrix( bone.id_data.children[0].convert_space(bone, bone.matrix_channel, 'WORLD', 'WORLD') )        
    
    def WriteArmature(obj):
        obj.update_from_editmode()
        WStr(obj.name)
        WriteMatrix(obj.matrix_local)
        
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
            def GetChannelName(channel):
                return channel.data_path.split('["')[1].split('"]')[0]
            affectedNames = {}
            for g in action.groups:
                for c in g.channels:
                    CName = GetChannelName(c)
                    if (GetPoseBoneIndex(obj, CName)>=0):
                        affectedNames[CName] = True
            return affectedNames.keys()
        
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
    meshes = bpy.data.meshes
    inst = [m for m in bpy.data.objects if (m.type=='MESH') and (len(m.users_scene)>0)]    
    #init vertex groups for meshes
    for obj in inst:
        meshToVertexGroup[obj.data] = obj.vertex_groups
    
    WInt(len(armatures))
    for a in armatures:
        WriteArmature(a)

    WInt(len(meshes))
    for m in meshes:
        WriteMesh(m);

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