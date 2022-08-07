#!BPY

bl_info = {
    "name": "AvalancheExport",
    "description": "Export 3d model for loading at AvalancheProject engine",
    "author": "Alexander Busarov",
    "version": (1, 0),
    "blender": (2, 81, 6),
    "location": "File > Import-Export",
    "warning": "", # used for warning icon and text in addons panel
    "doc_url": "",
    "category": "Import-Export"}

import bpy
from bpy.props import (
        StringProperty,
        BoolProperty,
        )
from bpy_extras.io_utils import (
        ExportHelper,
        path_reference_mode,
        )        

from . import avm_export_impl_28

class Export_avModel(bpy.types.Operator, ExportHelper):
    """Export selection to avModel"""
    bl_idname = "export_scene.avm"
    bl_label = "Export avModel"
    bl_options = {'PRESET'}
    
    filename_ext = ".avm"
    
    filepath = StringProperty(subtype='FILE_PATH')
    
    do_pbr_pack = BoolProperty(
            name="Pack Metallic, AO, Roghness",
            description="Pack Specular.Intensity, Shading.Ambient, Specular.Hardness in single texture to RGB channels accordingly",
            default=True,
            )
            
    path_mode: path_reference_mode
    
    check_extension = True
    
    def execute(self, context):
        self.filepath = bpy.path.ensure_ext(self.filepath, ".avm")
        avm_export_impl_28.ExportToFile(self.filepath, self.do_pbr_pack)
        #from . import export_avm_impl
        #Exporter = export_avm_impl.avModelExporter(self, context)
        #self.Export()
        return {'FINISHED'}
        
    def draw(self, context):
        pass

def menu_func(self, context):
    self.layout.operator(Export_avModel.bl_idname, text="avModel (.avm)")

def register():
    bpy.utils.register_class(Export_avModel)

    bpy.types.TOPBAR_MT_file_export.append(menu_func)

def unregister():
    bpy.utils.unregister_class(Export_avModel)

    bpy.types.TOPBAR_MT_file_export.remove(menu_func)

if __name__ == "__main__":
    register()