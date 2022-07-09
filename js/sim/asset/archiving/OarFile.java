/**
 * A library to interact with Virtual Worlds such as OpenSim
 * Copyright (C) 2012  Jitendra Chauhan, Email: jitendra.chauhan@gmail.com
 *
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 */
package com.ngt.jopenmetaverse.shared.sim.asset.archiving;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.StringWriter;
import java.util.Arrays;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions.PermissionMask;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntryFace;
import com.ngt.jopenmetaverse.shared.sim.asset.*;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetPrim.PrimObject;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetPrim.PrimObject.ShapeBlock;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.sim.events.asm.archive.AssetLoadedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.archive.SceneObjectLoadedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.archive.SettingsLoadedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.archive.TerrainLoadedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.inventory.Inventory;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.HoleType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PrimFlags;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.FileUtils;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.PlatformUtils;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class OarFile
    {
//        public delegate void AssetLoadedCallback(Asset asset, long bytesRead, long totalBytes);
//        public delegate void TerrainLoadedCallback(float[,] terrain, long bytesRead, long totalBytes);
//        public delegate void SceneObjectLoadedCallback(AssetPrim linkset, long bytesRead, long totalBytes);
//        public delegate void SettingsLoadedCallback(String regionName, RegionSettings settings);

//	private MethodDelegate<Void,AssetLoadedCallbackArgs> assetLoadedCallback;
//	private MethodDelegate<Void,TerrainLoadedCallbackArgs> terrainLoadedCallback;
//	private MethodDelegate<Void,SceneObjectLoadedCallbackArgs> sceneObjectLoadedCallback;
//	private MethodDelegate<Void,SettingsLoadedCallbackArgs> settingsLoadedCallback;
	
        //region Archive Loading

        public static void UnpackageArchive(String filename, MethodDelegate<Void,AssetLoadedCallbackArgs> assetCallback, MethodDelegate<Void,TerrainLoadedCallbackArgs> terrainCallback,
            MethodDelegate<Void,SceneObjectLoadedCallbackArgs> objectCallback, MethodDelegate<Void,SettingsLoadedCallbackArgs> settingsCallback)
        {
        	int successfulAssetRestores = 0;
        	int failedAssetRestores = 0;

        	try
        	{
        		File file = new File(filename);
        		long totalBytes = file.length();
        		InputStream fileStream = new FileInputStream(new File(filename));
        		GZIPInputStream loadStream = new GZIPInputStream(fileStream);

        		TarArchiveReader archive = new TarArchiveReader(loadStream);

        		TarEntry tarEntry;
        		TarArchiveReader.TarEntryType entryType;
        		byte[] data;
        		String filePath;

        		while ((tarEntry = archive.ReadEntry()).data != null)
        		{
        			filePath = tarEntry.getFilePath();
        			data = tarEntry.getData();
        			entryType = tarEntry.entryType;
        			if (filePath.startsWith(ArchiveConstants.OBJECTS_PATH))
        			{
        				// Deserialize the XML bytes
        				if (objectCallback != null)
        					LoadObjects(data, objectCallback, totalBytes - fileStream.available() , totalBytes);
        			}
        			else if (filePath.startsWith(ArchiveConstants.ASSETS_PATH))
        			{
        				if (assetCallback != null)
        				{
        					if (LoadAsset(filePath, data, assetCallback, totalBytes - fileStream.available(), totalBytes))
        						successfulAssetRestores++;
        					else
        						failedAssetRestores++;
        				}
        			}
        			else if (filePath.startsWith(ArchiveConstants.TERRAINS_PATH))
        			{
        				if (terrainCallback != null)
        					LoadTerrain(filePath, data, terrainCallback, totalBytes - fileStream.available(), totalBytes);
        			}
        			else if (filePath.startsWith(ArchiveConstants.SETTINGS_PATH))
        			{
        				if (settingsCallback != null)
        					LoadRegionSettings(filePath, data, settingsCallback);
        			}
        		}
        		archive.Close();
        	}
        	catch (Exception e)
        	{
        		JLogger.error("[OarFile] Error loading OAR file: " + e.getMessage() + Utils.getExceptionStackTraceAsString(e));
        		return;
        	}

        	if (failedAssetRestores > 0)
        		JLogger.warn(String.format("[OarFile]: Failed to load %s assets", failedAssetRestores));
        }

        private static boolean LoadAsset(String assetPath, byte[] data, 
        		MethodDelegate<Void,AssetLoadedCallbackArgs> assetCallback
        		, long bytesRead, long totalBytes)
        {
            // Right now we're nastily obtaining the UUID from the filename
            String filename = assetPath.replaceAll(assetPath.substring(0, ArchiveConstants.ASSETS_PATH.length()), "");
            int i = filename.lastIndexOf(ArchiveConstants.ASSET_EXTENSION_SEPARATOR);

            if (i == -1)
            {
                JLogger.warn(String.format(
                    "[OarFile]: Could not find extension information in asset path {0} since it's missing the separator {1}.  Skipping",
                    assetPath, ArchiveConstants.ASSET_EXTENSION_SEPARATOR));
                return false;
            }

            String extension = filename.substring(i);
            UUID uuid;
            uuid = UUID.Parse(filename.substring(0, filename.length() - extension.length()));

            if (ArchiveConstants.EXTENSION_TO_ASSET_TYPE.containsKey(extension))
            {
                AssetType assetType = ArchiveConstants.EXTENSION_TO_ASSET_TYPE.get(extension);
                Asset asset = null;

                switch (assetType)
                {
                    case Animation:
                        asset = new AssetAnimation(uuid, data);
                        break;
                    case Bodypart:
                        asset = new AssetBodypart(uuid, data);
                        break;
                    case Clothing:
                        asset = new AssetClothing(uuid, data);
                        break;
                    case Gesture:
                        asset = new AssetGesture(uuid, data);
                        break;
                    case Landmark:
                        asset = new AssetLandmark(uuid, data);
                        break;
                    case LSLBytecode:
                        asset = new AssetScriptBinary(uuid, data);
                        break;
                    case LSLText:
                        asset = new AssetScriptText(uuid, data);
                        break;
                    case Notecard:
                        asset = new AssetNotecard(uuid, data);
                        break;
                    case Object:
                        asset = new AssetPrim(uuid, data);
                        break;
                    case Sound:
                        asset = new AssetSound(uuid, data);
                        break;
                    case Texture:
                        asset = new AssetTexture(uuid, data);
                        break;
                    default:
                        JLogger.error("[OarFile] Unhandled asset type " + assetType);
                        break;
                }

                if (asset != null)
                {
                    assetCallback.execute(new AssetLoadedCallbackArgs(asset, bytesRead, totalBytes));
                    return true;
                }
            }

            JLogger.warn("[OarFile] Failed to load asset");
            return false;
        }

        private static boolean LoadRegionSettings(String filePath, byte[] data, 
        		MethodDelegate<Void,SettingsLoadedCallbackArgs> settingsCallback)
        {
            RegionSettings settings = null;
            boolean loaded = false;

            try
            {
               ByteArrayInputStream stream = new ByteArrayInputStream(data);
                    settings = RegionSettings.FromStream(stream);
                loaded = true;
                PlatformUtils.closeStream(stream);
            }
            catch (Exception ex)
            {
                JLogger.warn("[OarFile] Failed to parse region settings file " + filePath + ": " + Utils.getExceptionStackTraceAsString(ex));
            }

            // Parse the region name out of the filename
            String regionName = FileUtils.getFileNameWithoutExtension(filePath);

            if (loaded)
                settingsCallback.execute(new SettingsLoadedCallbackArgs(regionName, settings));

            return loaded;
        }

        private static boolean LoadTerrain(String filePath, byte[] data, 
        		MethodDelegate<Void,TerrainLoadedCallbackArgs> terrainCallback, 
        		long bytesRead, long totalBytes)
        {
            float[][] terrain = new float[256][256];
            boolean loaded = false;

            String extention = FileUtils.getExtension(filePath);
            {
                if(extention.equals(".r32") ||
                		extention.equals(".f32"))
                {
                    // RAW32
                    if (data.length == 256 * 256 * 4)
                    {
                        int pos = 0;
                        for (int y = 0; y < 256; y++)
                        {
                            for (int x = 0; x < 256; x++)
                            {
                                terrain[y][x] = Utils.clamp(Utils.bytesToFloat(data, pos), 0.0f, 255.0f);
                                pos += 4;
                            }
                        }

                        loaded = true;
                    }
                    else
                    {
                        JLogger.warn("[OarFile] RAW32 terrain file " + filePath + " has the wrong number of bytes: " + data.length);
                    }
                }
                
                else if(extention.equals(".ter") ||
                    // Terragen
                extention.equals(".raw") ||
                    // LLRAW
                extention.equals(".jpg") ||
                extention.equals(".jpeg") ||
                    // JPG
                extention.equals(".bmp") ||
                    // BMP
                extention.equals(".png") ||
                    // PNG
                extention.equals(".gif") ||
                    // GIF
                extention.equals(".tif") ||
                extention.equals(".tiff"))
                    // TIFF)
                { //FIXME nned to implement
                    JLogger.warn("[OarFile] Unrecognized terrain format in " + filePath);
                }
                else
                    JLogger.warn("[OarFile] Unrecognized terrain format in " + filePath);
            }

            if (loaded)
                terrainCallback.execute(new TerrainLoadedCallbackArgs(terrain, bytesRead, totalBytes));

            return loaded;
        }

        public static void LoadObjects(byte[] objectData, 
        		MethodDelegate<Void,SceneObjectLoadedCallbackArgs> objectCallback, 
        		long bytesRead, long totalBytes) throws Exception
        {
        	ByteArrayInputStream stream = new ByteArrayInputStream(objectData); 
			DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
			Document doc = dBuilder.parse(stream);

			//Get the root name
			Element root = doc.getDocumentElement(); 
        	
//            XmlDocument doc = new XmlDocument();
//
//            using (XmlTextReader reader = new XmlTextReader(new MemoryStream(objectData)))
//            {
//                reader.WhitespaceHandling = WhitespaceHandling.None;
//                doc.Load(reader);
//            }

            if (root.getNodeName().equals("scene"))
            {
            	NodeList childNodes = root.getChildNodes();
            	
                for (int i =0; i < childNodes.getLength(); i++)
                {
                	Node node = childNodes.item(i);
                    AssetPrim linkset = new AssetPrim(nodeToString(node));
                    if (linkset != null)
                        objectCallback.execute(new SceneObjectLoadedCallbackArgs(linkset, bytesRead, totalBytes));
                }
            }
            else
            {
//                AssetPrim linkset = new AssetPrim(rootNode.OuterXml);
                AssetPrim linkset = new AssetPrim(nodeToString(root));
                if (linkset != null)
                	objectCallback.execute(new SceneObjectLoadedCallbackArgs(linkset, bytesRead, totalBytes));
            }
        }

        //endregion Archive Loading

        //region Archive Saving

        public static void PackageArchive(String directoryName, String filename) throws Exception
        {
            final String ARCHIVE_XML = "<?xml version=\"1.0\" encoding=\"utf-16\"?>\n<archive major_version=\"0\" minor_version=\"1\" />";

            File inputfile = new File(filename);
    		long totalBytes = inputfile.length();
    		FileOutputStream fileStream = new FileOutputStream(inputfile);
    		GZIPOutputStream loadStream = new GZIPOutputStream(fileStream);
            
            TarArchiveWriter archive = new TarArchiveWriter(loadStream);

            // Create the archive.xml file
            archive.WriteFile("archive.xml", ARCHIVE_XML);

            // Add the assets
            File[] files = FileUtils.getFileList(directoryName + "/" + ArchiveConstants.ASSETS_PATH, false);
            for (File file : files)
                archive.WriteFile(ArchiveConstants.ASSETS_PATH + file.getName(), FileUtils.readBytes(file));

            // Add the objects
            files = FileUtils.getFileList(directoryName + "/" + ArchiveConstants.OBJECTS_PATH, false);
            for (File file : files)
                archive.WriteFile(ArchiveConstants.OBJECTS_PATH + file.getName(), FileUtils.readBytes(file));

            // Add the terrain(s)
            files = FileUtils.getFileList(directoryName + "/" + ArchiveConstants.TERRAINS_PATH, false);
            for (File file : files)
                archive.WriteFile(ArchiveConstants.TERRAINS_PATH + file.getName(), FileUtils.readBytes(file));

            archive.Close();
        }

        public static void SavePrims(List<AssetPrim> prims, String primsPath, 
        		String assetsPath, String textureCacheFolder)
        {
            Map<UUID, UUID> textureList = new HashMap<UUID, UUID>();

            // Delete all of the old linkset files
            try { FileUtils.deleteDirectory(new File(primsPath), true); }
            catch (Exception e ) {JLogger.info(Utils.getExceptionStackTraceAsString(e)); }

            // Create a new folder for the linkset files
            File file = new File(primsPath);
            try { file.mkdirs(); }
            catch (Exception ex)
            {
                JLogger.error("Failed saving prims: " + Utils.getExceptionStackTraceAsString(ex));
                return;
            }

            for (AssetPrim assetPrim : prims)
            {
                SavePrim(assetPrim, FileUtils.combineFilePath(primsPath, "Primitive_" + assetPrim.Parent.ID + ".xml"));

                CollectTextures(assetPrim.Parent, textureList);
                if (assetPrim.Children != null)
                {
                    for (PrimObject child : assetPrim.Children)
                        CollectTextures(child, textureList);
                }
            }
            SaveTextures( Arrays.asList(textureList.keySet().toArray(new UUID[0])), assetsPath, textureCacheFolder);
        }

        static void CollectTextures(PrimObject prim, Map<UUID, UUID> textureList)
        {
            if (prim.Textures != null)
            {
                // Add all of the textures on this prim to the save list
                //TODO need to verify following
                if (prim.Textures.DefaultTexture != null)
                    textureList.put(prim.Textures.DefaultTexture.getTextureID(),  prim.Textures.DefaultTexture.getTextureID());

                if (prim.Textures.FaceTextures != null)
                {
                    for (int i = 0; i < prim.Textures.FaceTextures.length; i++)
                    {
                        TextureEntryFace face = prim.Textures.FaceTextures[i];
                        //TODO need to verify following
                        if (face != null)
                            textureList.put(face.getTextureID(), textureList.get(face.getTextureID()));
                    }
                }
            }
        }

        public static void SaveTextures(List<UUID> textures, String assetsPath, String textureCacheFolder)
        {
            int count = 0;

            // Delete the assets folder
            try { FileUtils.deleteDirectory(new File(assetsPath), true);}
            catch (Exception e) {JLogger.info(Utils.getExceptionStackTraceAsString(e)); }

            // Create a new assets folder
            File file = new File(assetsPath);
            try { file.mkdirs(); }
            catch (Exception ex)
            {
                JLogger.error("Failed saving assets: " + Utils.getExceptionStackTraceAsString(ex));
                return;
            }

            // Create a map of all of the textures in the cache
            File[] files = FileUtils.getFileList(textureCacheFolder, "*.texture", true);
            Map<UUID, String> idToFiles = new HashMap<UUID, String>(files.length);
            for (int i = 0; i < files.length; i++)
            {
                String filename = files[i].getName();
                UUID id;

                if ((id = UUID.Parse(FileUtils.getFileNameWithoutExtension(filename)))!=null)
                    idToFiles.put(id, filename);
            }

            for (int i = 0; i < textures.size(); i++)
            {
                UUID texture = textures.get(i);

                if (idToFiles.containsKey(texture))
                {
                    try
                    {
                        FileUtils.copyFile(idToFiles.get(texture), FileUtils.combineFilePath(assetsPath, texture.toString() + "_texture.jp2"), false);
                        ++count;
                    }
                    catch (Exception ex)
                    {
                        JLogger.error("Failed to save texture " + texture.toString() + ": " + Utils.getExceptionStackTraceAsString(ex));
                    }
                }
                else
                {
                    JLogger.warn("Skipping missing texture " + texture.toString());
                }
            }

            JLogger.info("Copied " + count + " textures to the asset archive folder");
        }

        static void SavePrim(AssetPrim prim, String filename)
        {
            try
            {
                FileOutputStream stream = new FileOutputStream(filename);                
                SOGToXml2(stream, prim);
                FileUtils.closeStream(stream);
               
            }
            catch (Exception ex)
            {
                JLogger.error("Failed saving linkset: " + Utils.getExceptionStackTraceAsString(ex));
            }
        }

        public static void SOGToXml2(OutputStream writer, AssetPrim prim) throws Exception
        {
        	DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
    		DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
     
    		// root elements
    		Document doc = docBuilder.newDocument();
    		Element rootElement = doc.createElement("SceneObjectGroup");
    		doc.appendChild(rootElement);
            
            SOPToXml(doc, rootElement, prim.Parent, null);

    		Element otherPartsElement = doc.createElement("OtherParts");
    		doc.appendChild(otherPartsElement);
    		
    		  for (PrimObject child : prim.Children)
    			  SOPToXml(doc, otherPartsElement, child, prim.Parent);
            
    		//TODO need to implement    		
//            writer.WriteStartElement(String.Empty, "SceneObjectGroup", String.Empty);
//            SOPToXml(writer, prim.Parent, null);
//            writer.WriteStartElement(String.Empty, "OtherParts", String.Empty);
//
//            foreach (PrimObject child in prim.Children)
//                SOPToXml(writer, child, prim.Parent);
//
//            writer.WriteEndElement();
//            writer.WriteEndElement();
    		
    		
    		TransformerFactory transformerFactory = TransformerFactory.newInstance();
    		Transformer transformer = transformerFactory.newTransformer();
    		
    		DOMSource source = new DOMSource(doc);
    		StreamResult result = new StreamResult(writer);
    		transformer.transform(source, result);
        	
        }

        static void SOPToXml(Document doc, Element writer, PrimObject prim, PrimObject parent) throws IOException
        {
//            writer.WriteStartElement("SceneObjectPart");
//            writer.WriteAttributeString("xmlns:xsi", "http://www.w3.org/2001/XMLSchema-instance");
//            writer.WriteAttributeString("xmlns:xsd", "http://www.w3.org/2001/XMLSchema");
    		Element sceneObjectPartElement = doc.createElement("SceneObjectPart");

            Attr attr = doc.createAttribute("xmlns:xsi");
    		attr.setValue("http://www.w3.org/2001/XMLSchema-instance");
    		sceneObjectPartElement.setAttributeNode(attr);
    		
    		attr = doc.createAttribute("xmlns:xsd");
    		attr.setValue("http://www.w3.org/2001/XMLSchema");
    		sceneObjectPartElement.setAttributeNode(attr);
    		
    		writer.appendChild(sceneObjectPartElement);

            WriteUUID(doc, writer, "CreatorID", prim.CreatorID);
            WriteUUID(doc, writer, "FolderID", prim.FolderID);
            WriteElementString(doc, writer, "InventorySerial", (prim.Inventory != null) ? Float.toString(prim.Inventory.Serial) : "0");
            
            // FIXME: Task inventory
//            writer.WriteStartElement("TaskInventory"); writer.WriteEndElement();
            WriteInventory(doc, writer, "TaskInventory", null);
            
            EnumSet<PrimFlags> flags = PrimFlags.get(PrimFlags.None.getIndex());
            if (prim.UsePhysics) flags = PrimFlags.get(PrimFlags.or(flags, PrimFlags.Physics));
            if (prim.Phantom) flags = PrimFlags.get(PrimFlags.or(flags, PrimFlags.Phantom));
            if (prim.DieAtEdge) flags = PrimFlags.get(PrimFlags.or(flags, PrimFlags.DieAtEdge));
            if (prim.ReturnAtEdge) flags = PrimFlags.get(PrimFlags.or(flags,  PrimFlags.ReturnAtEdge));
            if (prim.Temporary) flags = PrimFlags.get(PrimFlags.or(flags, PrimFlags.Temporary));
            if (prim.Sandbox) flags = PrimFlags.get(PrimFlags.or(flags, PrimFlags.Sandbox));
            WriteElementString(doc, writer, "ObjectFlags", Long.toString(PrimFlags.getIndex(flags)));

            WriteUUID(doc, writer, "UUID", prim.ID);
            WriteElementString(doc, writer, "LocalId", Long.toString(prim.LocalID));
            WriteElementString(doc, writer, "Name", prim.Name);
            WriteElementString(doc, writer, "Material", Integer.toString((int)prim.Material));
            WriteElementString(doc, writer, "RegionHandle", prim.RegionHandle.toString());
            WriteElementString(doc, writer, "ScriptAccessPin", Integer.toString(prim.RemoteScriptAccessPIN));

            Vector3 groupPosition;
            if (parent == null)
                groupPosition = prim.Position;
            else
                groupPosition = parent.Position;

            WriteVector(doc, writer, "GroupPosition", groupPosition);
            if (prim.ParentID == 0)
                WriteVector(doc, writer, "OffsetPosition", Vector3.Zero);
            else
                WriteVector(doc, writer, "OffsetPosition", prim.Position);
            WriteQuaternion(doc, writer, "RotationOffset", prim.Rotation);
            WriteVector(doc, writer, "Velocity", prim.Velocity);
            WriteVector(doc, writer, "RotationalVelocity", Vector3.Zero);
            WriteVector(doc, writer, "AngularVelocity", prim.AngularVelocity);
            WriteVector(doc, writer, "Acceleration", prim.Acceleration);
            WriteElementString(doc, writer, "Description", prim.Description);

//            writer.WriteStartElement("Color");
//                writer.WriteElementString(doc, colorElement, "R", prim.TextColor.R.ToString(Utils.EnUsCulture));
//                writer.WriteElementString("G", prim.TextColor.G.ToString(Utils.EnUsCulture));
//                writer.WriteElementString("B", prim.TextColor.B.ToString(Utils.EnUsCulture));
//                writer.WriteElementString("A", prim.TextColor.G.ToString(Utils.EnUsCulture));
//            writer.WriteEndElement();
            WriteColor4(doc, writer, "Color", prim.TextColor);
            
            WriteElementString(doc, writer, "Text", prim.Text);
            WriteElementString(doc, writer, "SitName", prim.SitName);
            WriteElementString(doc, writer, "TouchName", prim.TouchName);

            WriteElementString(doc, writer, "LinkNum", Integer.toString(prim.LinkNumber));
            WriteElementString(doc, writer, "ClickAction", Integer.toString(prim.ClickAction));
            
            WriteShap(doc, writer, "Shape", prim, prim.Shape);
            
//            writer.WriteStartElement("Shape");
//            WriteElementString(doc, writer, "PathBegin", Primitive.PackBeginCut(prim.Shape.PathBegin).ToString());
//            WriteElementString(doc, writer, "PathCurve", prim.Shape.PathCurve.ToString());
//            WriteElementString(doc, writer, "PathEnd", Primitive.PackEndCut(prim.Shape.PathEnd).ToString());
//            WriteElementString(doc, writer, "PathRadiusOffset", Primitive.PackPathTwist(prim.Shape.PathRadiusOffset).ToString());
//            WriteElementString(doc, writer, "PathRevolutions", Primitive.PackPathRevolutions(prim.Shape.PathRevolutions).ToString());
//            WriteElementString(doc, writer, "PathScaleX", Primitive.PackPathScale(prim.Shape.PathScaleX).ToString());
//            WriteElementString(doc, writer, "PathScaleY", Primitive.PackPathScale(prim.Shape.PathScaleY).ToString());
//            WriteElementString(doc, writer, "PathShearX", ((byte)Primitive.PackPathShear(prim.Shape.PathShearX)).ToString());
//            WriteElementString(doc, writer, "PathShearY", ((byte)Primitive.PackPathShear(prim.Shape.PathShearY)).ToString());
//            WriteElementString(doc, writer, "PathSkew", Primitive.PackPathTwist(prim.Shape.PathSkew).ToString());
//            WriteElementString(doc, writer, "PathTaperX", Primitive.PackPathTaper(prim.Shape.PathTaperX).ToString());
//            WriteElementString(doc, writer, "PathTaperY", Primitive.PackPathTaper(prim.Shape.PathTaperY).ToString());
//            WriteElementString(doc, writer, "PathTwist", Primitive.PackPathTwist(prim.Shape.PathTwist).ToString());
//            WriteElementString(doc, writer, "PathTwistBegin", Primitive.PackPathTwist(prim.Shape.PathTwistBegin).ToString());
//            WriteElementString(doc, writer, "PCode", prim.PCode.ToString());
//            WriteElementString(doc, writer, "ProfileBegin", Primitive.PackBeginCut(prim.Shape.ProfileBegin).ToString());
//            WriteElementString(doc, writer, "ProfileEnd", Primitive.PackEndCut(prim.Shape.ProfileEnd).ToString());
//            WriteElementString(doc, writer, "ProfileHollow", Primitive.PackProfileHollow(prim.Shape.ProfileHollow).ToString());
//            WriteVector(doc, writer, "Scale", prim.Scale);
//            WriteElementString(doc, writer, "State", prim.State.ToString());
//
//            AssetPrim.ProfileShape shape = (AssetPrim.ProfileShape)(prim.Shape.ProfileCurve & 0x0F);
//            HoleType hole = (HoleType)(prim.Shape.ProfileCurve & 0xF0);
//            WriteElementString(doc, writer, "ProfileShape", shape.ToString());
//            WriteElementString(doc, writer, "HollowShape", hole.ToString());
//            WriteElementString(doc, writer, "ProfileCurve", prim.Shape.ProfileCurve.ToString());
//
//            writer.WriteStartElement("TextureEntry");
//
//            byte[] te;
//            if (prim.Textures != null)
//                te = prim.Textures.GetBytes();
//            else
//                te = Utils.EmptyBytes;
//
//            writer.WriteBase64(te, 0, te.Length);
//            writer.WriteEndElement();
//
//            // FIXME: ExtraParams
//            writer.WriteStartElement("ExtraParams"); writer.WriteEndElement();
//
//            writer.WriteEndElement();

            WriteVector(doc, writer, "Scale", prim.Scale);
            WriteElementString(doc, writer, "UpdateFlag", "0");
            WriteVector(doc, writer, "SitTargetOrientation", Vector3.UnitZ); // TODO: Is this really a vector and not a quaternion?
            WriteVector(doc, writer, "SitTargetPosition", prim.SitOffset);
            WriteVector(doc, writer, "SitTargetPositionLL", prim.SitOffset);
            WriteQuaternion(doc, writer, "SitTargetOrientationLL", prim.SitRotation);
            WriteElementString(doc, writer, "ParentID", Long.toString(prim.ParentID));
            WriteElementString(doc, writer, "CreationDate", Long.toString((prim.CreationDate.getTime())));
            WriteElementString(doc, writer, "Category", "0");
            WriteElementString(doc, writer, "SalePrice", Integer.toString(prim.SalePrice));
            WriteElementString(doc, writer, "ObjectSaleType", Integer.toString((prim.SaleType)));
            WriteElementString(doc, writer, "OwnershipCost", "0");
            WriteUUID(doc, writer, "GroupID", prim.GroupID);
            WriteUUID(doc, writer, "OwnerID", prim.OwnerID);
            WriteUUID(doc, writer, "LastOwnerID", prim.LastOwnerID);
            WriteElementString(doc, writer, "BaseMask", Long.toString(PermissionMask.All.getIndex()));
            WriteElementString(doc, writer, "OwnerMask", Long.toString(PermissionMask.All.getIndex()));
            WriteElementString(doc, writer, "GroupMask", Long.toString(PermissionMask.All.getIndex()));
            WriteElementString(doc, writer, "EveryoneMask", Long.toString(PermissionMask.All.getIndex()));
            WriteElementString(doc, writer, "NextOwnerMask", Long.toString(PermissionMask.All.getIndex()));
            WriteElementString(doc, writer, "Flags", "None");
            WriteUUID(doc, writer, "SitTargetAvatar", UUID.Zero);
        }

        
        static void WriteElementString(Document doc, Element writer, String name, String value)
        {
        	Element ele = doc.createElement(name);
        	ele.appendChild(doc.createTextNode(value));
        	writer.appendChild(ele);
        }
        
        static void WriteUUID(Document doc, Element writer, String name, UUID id)
        {
        	Element ele = doc.createElement(name);
        	ele.appendChild(doc.createElement("UUID").appendChild(doc.createTextNode(id.toString())));
        	writer.appendChild(ele);
//            writer.WriteStartElement(name);
//            writer.WriteElementString("UUID", id.ToString());
//            writer.WriteEndElement();
        }

        static void WriteColor4(Document doc, Element writer, String name, Color4 color)
        {
        	Element ele = doc.createElement(name);
        	ele.appendChild(doc.createElement("R").appendChild(doc.createTextNode(Float.toString(color.getR()))));
        	ele.appendChild(doc.createElement("B").appendChild(doc.createTextNode(Float.toString(color.getB()))));
        	ele.appendChild(doc.createElement("G").appendChild(doc.createTextNode(Float.toString(color.getG()))));
        	ele.appendChild(doc.createElement("A").appendChild(doc.createTextNode(Float.toString(color.getA()))));
        	writer.appendChild(ele);

//            writer.WriteStartElement(name);
//            writer.WriteElementString("X", vec.X.ToString(Utils.EnUsCulture));
//            writer.WriteElementString("Y", vec.Y.ToString(Utils.EnUsCulture));
//            writer.WriteElementString("Z", vec.Z.ToString(Utils.EnUsCulture));
//            writer.WriteEndElement();
        }
        
        static void WriteVector(Document doc, Element writer, String name, Vector3 vec)
        {
        	Element ele = doc.createElement(name);
        	ele.appendChild(doc.createElement("X").appendChild(doc.createTextNode(Float.toString(vec.X))));
        	ele.appendChild(doc.createElement("Y").appendChild(doc.createTextNode(Float.toString(vec.Y))));
        	ele.appendChild(doc.createElement("Z").appendChild(doc.createTextNode(Float.toString(vec.Z))));
        	writer.appendChild(ele);

//            writer.WriteStartElement(name);
//            writer.WriteElementString("X", vec.X.ToString(Utils.EnUsCulture));
//            writer.WriteElementString("Y", vec.Y.ToString(Utils.EnUsCulture));
//            writer.WriteElementString("Z", vec.Z.ToString(Utils.EnUsCulture));
//            writer.WriteEndElement();
        }

        static void WriteQuaternion(Document doc, Element writer, String name, Quaternion quat)
        {
        	Element ele = doc.createElement(name);
        	ele.appendChild(doc.createElement("X").appendChild(doc.createTextNode(Float.toString(quat.X))));
        	ele.appendChild(doc.createElement("Y").appendChild(doc.createTextNode(Float.toString(quat.Y))));
        	ele.appendChild(doc.createElement("Z").appendChild(doc.createTextNode(Float.toString(quat.Z))));
        	ele.appendChild(doc.createElement("W").appendChild(doc.createTextNode(Float.toString(quat.W))));
        	writer.appendChild(ele);

//            writer.WriteStartElement(name);
//            writer.WriteElementString("X", quat.X.ToString(Utils.EnUsCulture));
//            writer.WriteElementString("Y", quat.Y.ToString(Utils.EnUsCulture));
//            writer.WriteElementString("Z", quat.Z.ToString(Utils.EnUsCulture));
//            writer.WriteElementString("W", quat.W.ToString(Utils.EnUsCulture));
//            writer.WriteEndElement();
        }

        static void WriteInventory(Document doc, Element writer, String name, Inventory vec)
        {
        	Element ele = doc.createElement(name);
        	//FIXME need to implement
        	writer.appendChild(ele);
        }
        
        static void WriteShap(Document doc, Element writer, String name, PrimObject prim, ShapeBlock shape) throws IOException
        {
        	Element ele = doc.createElement(name);
        	
            WriteElementString(doc, ele, "PathBegin", Integer.toString(Primitive.PackBeginCut(shape.PathBegin)));
            WriteElementString(doc, ele, "PathCurve", Integer.toString(shape.PathCurve));
            WriteElementString(doc, ele, "PathEnd", Integer.toString(Primitive.PackEndCut(shape.PathEnd)));
            WriteElementString(doc, ele, "PathRadiusOffset", Byte.toString(Primitive.PackPathTwist(shape.PathRadiusOffset)));
            WriteElementString(doc, ele, "PathRevolutions", Byte.toString(Primitive.PackPathRevolutions(shape.PathRevolutions)));
            WriteElementString(doc, ele, "PathScaleX", Byte.toString(Primitive.PackPathScale(shape.PathScaleX)));
            WriteElementString(doc, ele, "PathScaleY", Byte.toString(Primitive.PackPathScale(shape.PathScaleY)));
            WriteElementString(doc, ele, "PathShearX", Byte.toString(((byte)Primitive.PackPathShear(shape.PathShearX))));
            WriteElementString(doc, ele, "PathShearY", Byte.toString(((byte)Primitive.PackPathShear(shape.PathShearY))));
            WriteElementString(doc, ele, "PathSkew", Byte.toString(Primitive.PackPathTwist(shape.PathSkew)));
            WriteElementString(doc, ele, "PathTaperX", Byte.toString(Primitive.PackPathTaper(shape.PathTaperX)));
            WriteElementString(doc, ele, "PathTaperY", Byte.toString(Primitive.PackPathTaper(shape.PathTaperY)));
            WriteElementString(doc, ele, "PathTwist", Byte.toString(Primitive.PackPathTwist(shape.PathTwist)));
            WriteElementString(doc, ele, "PathTwistBegin", Byte.toString(Primitive.PackPathTwist(shape.PathTwistBegin)));
            WriteElementString(doc, ele, "PCode", Integer.toString(prim.PCode));
            WriteElementString(doc, ele, "ProfileBegin", Integer.toString(Primitive.PackBeginCut(shape.ProfileBegin)));
            WriteElementString(doc, ele, "ProfileEnd", Integer.toString(Primitive.PackEndCut(shape.ProfileEnd)));
            WriteElementString(doc, ele, "ProfileHollow", Integer.toString(Primitive.PackProfileHollow(shape.ProfileHollow)));
            WriteVector(doc, writer, "Scale", prim.Scale);
            WriteElementString(doc, ele, "State", Integer.toString(prim.State));

            AssetPrim.ProfileShape shape2 = AssetPrim.ProfileShape.get((byte)(shape.ProfileCurve & 0x0F));
            HoleType hole = HoleType.get((byte)(shape.ProfileCurve & 0xF0));
            WriteElementString(doc, ele, "ProfileShape", shape2.toString());
            WriteElementString(doc, ele, "HollowShape", hole.toString());
            WriteElementString(doc, ele, "ProfileCurve", Integer.toString(shape.ProfileCurve));

            WriteTextureEntry(doc, ele, "TextureEntry", prim);
            
//            writer.WriteStartElement("TextureEntry");
//
//            byte[] te;
//            if (prim.Textures != null)
//                te = prim.Textures.GetBytes();
//            else
//                te = Utils.EmptyBytes;
//
//            writer.WriteBase64(te, 0, te.length);
//            writer.WriteEndElement();

            // FIXME: ExtraParams
//            writer.WriteStartElement("ExtraParams"); writer.WriteEndElement();
            WriteExtraParams(doc, ele, "ExtraParams");
            
//            writer.WriteEndElement();
        	
        	writer.appendChild(ele);
        }
        
        static void WriteTextureEntry(Document doc, Element writer, String name, PrimObject prim) throws IOException
        {
        	Element ele = doc.createElement(name);
        	byte[] te;
            if (prim.Textures != null)
                te = prim.Textures.GetBytes();
            else
                te = Utils.EmptyBytes;

//            writer.WriteBase64(doc, ele, te, 0, te.length);
        	ele.appendChild(doc.createTextNode(Utils.encodeBase64String(te)));
        	writer.appendChild(ele);
        }
        

        static void WriteExtraParams(Document doc, Element writer, String name)
        {
        	Element ele = doc.createElement(name);
            // FIXME: ExtraParams
        	writer.appendChild(ele);
        }
        
        //endregion Archive Saving
        
    	private static String nodeToString(Node node) throws TransformerFactoryConfigurationError, TransformerException {
    		StringWriter sw = new StringWriter();
    		
    		 Transformer t = TransformerFactory.newInstance().newTransformer();
    		 t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
    		 t.setOutputProperty(OutputKeys.INDENT, "yes");
    		 t.transform(new DOMSource(node), new StreamResult(sw));
    		
    		return sw.toString();
    		}
        
    }