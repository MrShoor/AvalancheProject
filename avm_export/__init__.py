#!BPY

bl_info = {
    "name": "AvalancheExport",
    "description": "Export 3d model for loading at AvalancheProject engine",
    "author": "Alexander Busarov",
    "version": (1, 0),
    "blender": (2, 65, 0),
    "location": "File > Import-Export",
    "warning": "", # used for warning icon and text in addons panel
    "wiki_url": "http://wiki.blender.org/index.php/Extensions:2.5/Py/"
                "Scripts/My_Script",
    "category": "Import-Export"}

import bpy
from bpy.props import StringProperty

import avm_export

class Export_avModel(bpy.types.Operator):
    """Export selection to avModel"""
    bl_idname = "export_scene.avm"
    bl_label = "Export avModel"
    
    filepath = StringProperty(subtype='FILE_PATH')
    
    def Export(self):
        File = open(self.filepath, 'w')
        
        for scene in bpy.data.scenes:
            for obj in scene.objects:
                File.write(obj.name)
        File.close()
    
    def execute(self, context):
        self.filepath = bpy.path.ensure_ext(self.filepath, ".avm")
        avm_export.ExportToFile(self.filepath)
        #from . import export_avm_impl
        #Exporter = export_avm_impl.avModelExporter(self, context)
        #self.Export()
        return {'FINISHED'}

    def invoke(self, context, event):
        if not self.filepath:
            self.filepath = bpy.path.ensure_ext(bpy.data.filepath, ".avm")
        context.window_manager.fileselect_add(self)
        return {'RUNNING_MODAL'}

def menu_func(self, context):
    self.layout.operator(Export_avModel.bl_idname, text="avModel (.avm)")

def register():
    bpy.utils.register_module(__name__)

    bpy.types.INFO_MT_file_export.append(menu_func)

def unregister():
    bpy.utils.unregister_module(__name__)

    bpy.types.INFO_MT_file_export.remove(menu_func)

if __name__ == "__main__":
    register()