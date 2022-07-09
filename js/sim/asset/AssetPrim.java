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
package com.ngt.jopenmetaverse.shared.sim.asset;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive;
import com.ngt.jopenmetaverse.shared.exception.asset.AssetPrimitiveParsingException;
import com.ngt.jopenmetaverse.shared.protocol.primitives.FlexibleData;
import com.ngt.jopenmetaverse.shared.protocol.primitives.LightData;
import com.ngt.jopenmetaverse.shared.protocol.primitives.ObjectProperties;
import com.ngt.jopenmetaverse.shared.protocol.primitives.ParticleSystem;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.protocol.primitives.SculptData;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntry;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetPrim.PrimObject.ShapeBlock;
import com.ngt.jopenmetaverse.shared.sim.asset.archiving.OarFile;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.Enums;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.Enums.InventoryType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.HoleType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PrimFlags;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.SculptType;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector2;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// A linkset asset, containing a parent primitive and zero or more children
/// </summary>
public class AssetPrim extends Asset
{
    /// <summary>
    /// Only used internally for XML serialization/deserialization
    /// </summary>
    public enum ProfileShape
    {
        Circle((byte)0),
        Square((byte)1),
        IsometricTriangle((byte)2),
        EquilateralTriangle((byte)3),
        RightTriangle((byte)4),
        HalfCircle((byte)5);
		private byte index;
		ProfileShape(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,ProfileShape> lookup  = new HashMap<Byte,ProfileShape>();

		static {
			for(ProfileShape s : EnumSet.allOf(ProfileShape.class))
				lookup.put(s.getIndex(), s);
		}

		public static ProfileShape get(Byte index)
		{
			return lookup.get(index);
		}
    }

    public PrimObject Parent;
    public List<PrimObject> Children;

    /// <summary>Override the base classes AssetType</summary>
    @Override
    public AssetType getAssetType() { return AssetType.Object; } 

    /// <summary>Initializes a new instance of an AssetPrim object</summary>
    public AssetPrim() { }

    /// <summary>
    /// Initializes a new instance of an AssetPrim object
    /// </summary>
    /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
    /// <param name="assetData">A byte array containing the raw asset data</param>
    public AssetPrim(UUID assetID, byte[] assetData) { super(assetID, assetData);}

    public AssetPrim(String xmlData) throws AssetPrimitiveParsingException
    {
        DecodeXml(xmlData);
    }

    public AssetPrim(PrimObject parent, List<PrimObject> children)
    {
        Parent = parent;
        if (children != null)
            Children = children;
        else
            Children = new ArrayList<PrimObject>(0);
    }

    /// <summary>
    /// 
    /// </summary>
    @Override
    public void Encode() throws AssetPrimitiveParsingException
    {
        AssetData = Utils.stringToBytes(EncodeXml());
    }

    /// <summary>
    /// 
    /// </summary>
    /// <returns></returns>
    @Override
    public boolean Decode() throws AssetPrimitiveParsingException
    {
        if (AssetData != null && AssetData.length > 0)
        {
                String xmlData;
				try {
					xmlData = Utils.bytesToString(AssetData);
	                DecodeXml(xmlData);
				} catch (UnsupportedEncodingException e) {
					throw new AssetPrimitiveParsingException(e);
				}
                return true;
        }
        return false;
    }

    public String EncodeXml() throws AssetPrimitiveParsingException
    {
    	ByteArrayOutputStream baos = new ByteArrayOutputStream();

    	try{
            OarFile.SOGToXml2(baos, this);
            baos.flush();
    	}
    	catch(Exception e)
    	{
    		throw new AssetPrimitiveParsingException(e);
    	}
            return baos.toString();
    }
    
    public boolean DecodeXml(String xmlData) throws AssetPrimitiveParsingException
    {
    	byte[] xmlDataBytes = Utils.stringToBytes(xmlData);
    	ByteArrayInputStream inputStream = new ByteArrayInputStream(xmlDataBytes);
    	Element root = null;
    	try{
    	DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
    	DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
    	Document doc = dBuilder.parse(inputStream);
    	root = doc.getDocumentElement();

    	}
    	catch(Exception e)
    	{
    		throw new AssetPrimitiveParsingException(e);
    	}
    	//Get the root name

    	return readSceneObjectGroup(root);
    }
    
    public static PrimObject LoadPrim(Node root) throws AssetPrimitiveParsingException
    {
		if (root.getNodeType() != Node.ELEMENT_NODE|| !root.getNodeName().equals("SceneObjectPart"))
			throw new AssetPrimitiveParsingException("Expected <SceneObjectPart>");
    	
        PrimObject obj = new PrimObject();
        obj.Shape = new PrimObject.ShapeBlock();
        obj.Inventory = new PrimObject.InventoryBlock();
        obj.AllowedDrop = true;
        obj.PassTouches = false;
        
		 Vector3 offsetPosition = new Vector3();
		 Vector3 groupPosition = new Vector3();
        
        NodeList nodeList = root.getChildNodes();
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		Node node = nodeList.item(i);
    		String nodeName = node.getNodeName();
    		if(node.getNodeType() == Node.ELEMENT_NODE)
    		{
    			 if(nodeName.equals("AllowedDrop"))
    			 {
 			        obj.AllowedDrop = Boolean.parseBoolean(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equals("CreatorID"))
    			 {
    				 obj.CreatorID = ReadUUID(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("FolderID"))
    			 {
    			        obj.FolderID = ReadUUID(root);
    			 }
    			 else if(nodeName.equalsIgnoreCase("InventorySerial"))
    			 {
    				 String s = node.getFirstChild().getNodeValue().trim();
    				 obj.Inventory.Serial = Integer.parseInt(s);
    			 }    	
    			 else if(nodeName.equalsIgnoreCase("TaskInventory"))
    			 {
    				 obj.Inventory.Items = ReadTaskInventoryItems(node);
    			 }     			 
    			 else if(nodeName.equalsIgnoreCase("ObjectFlags"))
    			 {
    				 EnumSet<PrimFlags> flags = PrimFlags.get((long)Long.parseLong(node.getFirstChild().getNodeValue().trim()));
    				 obj.UsePhysics = PrimFlags.and(flags , PrimFlags.Physics) != 0;
    				 obj.Phantom = PrimFlags.and(flags , PrimFlags.Phantom) != 0;
    				 obj.DieAtEdge = PrimFlags.and(flags , PrimFlags.DieAtEdge) != 0;
    				 obj.ReturnAtEdge = PrimFlags.and(flags , PrimFlags.ReturnAtEdge) != 0;
    				 obj.Temporary = PrimFlags.and(flags , PrimFlags.Temporary) != 0;
    				 obj.Sandbox = PrimFlags.and(flags , PrimFlags.Sandbox) != 0;
    			 }     	
    			 else if(nodeName.equalsIgnoreCase("UUID"))
    			 {
    			        obj.ID = ReadUUID(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("LocalId"))
    			 {
    			        obj.LocalID = Long.parseLong(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("Name"))
    			 {
    			        obj.Name = node.getFirstChild().getNodeValue().trim();
    			 }
    			 else if(nodeName.equalsIgnoreCase("Material"))
    			 {
    			        obj.Material = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("PassTouches"))
    			 {
    			        obj.PassTouches = Boolean.parseBoolean(node.getFirstChild().getNodeValue().trim());
    			 } 
    			 else if(nodeName.equalsIgnoreCase("RegionHandle"))
    			 {
    			        obj.RegionHandle = new BigInteger(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("ScriptAccessPin"))
    			 {
    			        obj.RemoteScriptAccessPIN = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("PlaySoundSlavePrims"))
    			 {
    				 //Discard
    			 }    			
    			 else if(nodeName.equalsIgnoreCase("LoopSoundSlavePrims"))
    			 {
    				 //Discard
    			 }   
    			 else if(nodeName.equalsIgnoreCase("GroupPosition"))
    			 {
    				 groupPosition = ReadVector(node);
    			 }     			 
    			 else if(nodeName.equalsIgnoreCase("OffsetPosition"))
    			 {
    				 offsetPosition = ReadVector(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("RotationOffset"))
    			 {
    				 obj.Rotation = ReadQuaternion(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("Velocity"))
    			 {
    				 obj.Velocity  = ReadVector(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("RotationalVelocity"))
    			 {
    				 Vector3 rotationalVelocity = ReadVector(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("AngularVelocity"))
    			 {
    				 Vector3 AngularVelocity = ReadVector(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("Acceleration"))
    			 {
    				 Vector3 Acceleration = ReadVector(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("Description"))
    			 {
    			        obj.Description = node.getFirstChild().getNodeValue().trim();
    			 }
    			 else if(nodeName.equalsIgnoreCase("Color"))
    			 {
    				 obj.TextColor = ReadColor(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("Text"))
    			 {
    			        obj.Text = node.getFirstChild().getNodeValue().trim();
    			 }
    			 else if(nodeName.equalsIgnoreCase("SitName"))
    			 {
    			        obj.SitName = node.getFirstChild().getNodeValue().trim();
    			 }
    			 else if(nodeName.equalsIgnoreCase("TouchName"))
    			 {
    			        obj.TouchName = node.getFirstChild().getNodeValue().trim();
    			 }
    			 else if(nodeName.equalsIgnoreCase("LinkNum"))
    			 {
    			        obj.LinkNumber = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("ClickAction"))
    			 {
    			        obj.ClickAction = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("Shape"))
    			 {
    				 obj.Shape = ReadShap(node, obj);
    			 }
    			 else if(nodeName.equalsIgnoreCase("Scale"))
    			 {
    				 obj.Scale  = ReadVector(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("SitTargetPositionLL"))
    			 {
    				 obj.SitOffset  = ReadVector(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("SitTargetOrientationLL"))
    			 {
    				 obj.SitRotation  = ReadQuaternion(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("ParentID"))
    			 {
    			        obj.ParentID = Long.parseLong(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("CreationDate"))
    			 {
    			        obj.CreationDate = Utils.unixTimeToDate(Long.parseLong(node.getFirstChild().getNodeValue().trim()));
    			 }
    			 else if(nodeName.equalsIgnoreCase("Category"))
    			 {
    				 int category = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("SalePrice"))
    			 {
    				 obj.SalePrice = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("ObjectSaleType"))
    			 {
    				 obj.SaleType = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("OwnershipCost"))
    			 {
    				 int ownershipCost = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("GroupID"))
    			 {
    				 obj.GroupID = ReadUUID(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("OwnerID"))
    			 {
    				 obj.OwnerID = ReadUUID(node);
    			 }  
    			 else if(nodeName.equalsIgnoreCase("LastOwnerID"))
    			 {
    				 obj.LastOwnerID = ReadUUID(node);
    			 }  
    			 else if(nodeName.equalsIgnoreCase("BaseMask"))
    			 {
    			        obj.PermsBase = Long.parseLong(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("OwnerMask"))
    			 {
    			        obj.PermsOwner = Long.parseLong(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("GroupMask"))
    			 {
    			        obj.PermsGroup = Long.parseLong(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("EveryoneMask"))
    			 {
    			        obj.PermsEveryone = Long.parseLong(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("NextOwnerMask"))
    			 {
    			        obj.PermsNextOwner = Long.parseLong(node.getFirstChild().getNodeValue().trim());
    			 }
    			 else if(nodeName.equalsIgnoreCase("CollisionSound"))
    			 {
    				 obj.CollisionSound = ReadUUID(node);
    			 }
    			 else if(nodeName.equalsIgnoreCase("CollisionSoundVolume"))
    			 {
    				 obj.CollisionSoundVolume = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			 }  
     			else
     			{
     				JLogger.warn("Not handled parsing of node " + root.getNodeName() + ":" + nodeName);
     			}
    		}
    	}
        
        if (obj.ParentID == 0)
            obj.Position = groupPosition;
        else
            obj.Position = offsetPosition;

        return obj;
    }

    private boolean readSceneObjectGroup(Node root) throws AssetPrimitiveParsingException
    {
    	NodeList nodeList = root.getChildNodes();		
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		if(nodeList.item(i).getNodeType() == Node.ELEMENT_NODE 
    				&& root.getNodeName().equals("SceneObjectGroup"))
    		{
    			return readSceneObjectPartParent(nodeList.item(i));
    		}
    	}
    	return false;
    }

    private boolean readSceneObjectPartParent(Node root) throws AssetPrimitiveParsingException
    {
    	NodeList nodeList = root.getChildNodes();		
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		if(nodeList.item(i).getNodeType() == Node.ELEMENT_NODE 
    				&& root.getNodeName().equals("SceneObjectPart"))
    		{
    			Parent = LoadPrim(nodeList.item(i));
    			break;
    		}
    	}

    	if (Parent != null)
    	{
    		if (this.getAssetID().equals(UUID.Zero))
    			this.setAssetID(Parent.ID);

    		return readSceneObjectPartChildren(root);
    	}
    	else
    	{
    		JLogger.error("Failed to load root linkset prim");
    		return false;
    	}

    }


    private boolean readSceneObjectPartChildren(Node root) throws AssetPrimitiveParsingException
    {
    	NodeList nodeList = root.getChildNodes();
    	List<PrimObject> children = new ArrayList<PrimObject>();
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		if(nodeList.item(i).getNodeType() == Node.ELEMENT_NODE 
    				&& root.getNodeName().equals("SceneObjectPart"))
    		{
    			PrimObject child = LoadPrim(nodeList.item(i));
    			if (child != null)
    				children.add(child);
    		}
    	}

    	Children = children;
    	return true;
    }
    
    /*
     * Return 
     * 	parsed ShapeBlock
     *  update PrimObject for certain fields
     */
    private static ShapeBlock ReadShap(Node root, PrimObject prim) throws AssetPrimitiveParsingException 
    {
    	ShapeBlock shape = new ShapeBlock();
    	ProfileShape profileShape = ProfileShape.Circle;
    	HoleType holeType = HoleType.Circle; 
    	UUID sculptTexture = UUID.Zero;
    	SculptType sculptType = SculptType.None; 
        boolean hasFlexi = false;
        boolean hasLight = false;
    	
        PrimObject.FlexibleBlock flexible = new PrimObject.FlexibleBlock();
        PrimObject.LightBlock light = new PrimObject.LightBlock();
    	
    	NodeList inodeList = root.getChildNodes();
    	for(int j = 0; j < inodeList.getLength(); j++)
    	{
    		Node node = inodeList.item(j);
    		if(node.getNodeType() == Node.ELEMENT_NODE)
    		{
    			String nodeName = node.getNodeName();
    			if(nodeName.equalsIgnoreCase("ProfileCurve"))
    			{
    				shape.ProfileCurve = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("TextureEntry"))
    			{
    				byte[] teData = Utils.decodeBase64String(node.getFirstChild().getNodeValue().trim());
    				try {
						prim.Textures =  new TextureEntry(teData, 0, teData.length);
					} catch (Exception e) {
						throw new AssetPrimitiveParsingException(e);
					}
    			}
    			else if(nodeName.equalsIgnoreCase("PathBegin"))
    			{
    				shape.PathBegin = Primitive.UnpackBeginCut(Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathCurve"))
    			{
    				shape.PathCurve = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("PathEnd"))
    			{
    				shape.PathEnd = Primitive.UnpackEndCut(Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathRadiusOffset"))
    			{
    				shape.PathRadiusOffset = Primitive.UnpackPathTwist((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}   
    			else if(nodeName.equalsIgnoreCase("PathRevolutions"))
    			{
    				shape.PathRevolutions = Primitive.UnpackPathRevolutions((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathScaleX"))
    			{
    				shape.PathScaleX = Primitive.UnpackPathScale((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathScaleY"))
    			{
    				shape.PathScaleY = Primitive.UnpackPathScale((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathShearX"))
    			{
    				shape.PathShearX = Primitive.UnpackPathShear((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathShearY"))
    			{
    				shape.PathShearY = Primitive.UnpackPathShear((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathSkew"))
    			{
    				shape.PathSkew = Primitive.UnpackPathTwist((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathTaperX"))
    			{
    				shape.PathTaperX = Primitive.UnpackPathTaper((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathTaperY"))
    			{
    				shape.PathTaperY = Primitive.UnpackPathShear((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathTwist"))
    			{
    				shape.PathTwist = Primitive.UnpackPathTwist((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("PathTwistBegin"))
    			{
    				shape.PathTwistBegin = Primitive.UnpackPathTwist((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			} 
    			else if(nodeName.equalsIgnoreCase("PCode"))
    			{
    				prim.PCode = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("ProfileBegin"))
    			{
    				shape.ProfileBegin = Primitive.UnpackBeginCut(Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("ProfileEnd"))
    			{
    				shape.ProfileEnd = Primitive.UnpackEndCut(Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("ProfileHollow"))
    			{
    				shape.ProfileHollow = Primitive.UnpackProfileHollow(Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("Scale"))
    			{
    				prim.Scale = ReadVector(node);
    			}
    			else if(nodeName.equalsIgnoreCase("State"))
    			{
    				prim.State = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("ProfileShape"))
    			{
    				profileShape = ProfileShape.valueOf(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("HollowShape"))
    			{
    				holeType = HoleType.valueOf(node.getFirstChild().getNodeValue().trim());
    			}
      			else if(nodeName.equalsIgnoreCase("SculptTexture"))
    			{
      				sculptTexture = ReadUUID(node);
    			}
    			else if(nodeName.equalsIgnoreCase("SculptType"))
    			{
    				sculptType = SculptType.get((byte)Integer.parseInt(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiSoftness"))
    			{
    				flexible.Softness = Integer.parseInt(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiTension"))
    			{
    				flexible.Tension = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiDrag"))
    			{
    				flexible.Drag = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiGravity"))
    			{
    				flexible.Gravity = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiWind"))
    			{
    				flexible.Wind = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiForceX"))
    			{
    				flexible.Force.X = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiForceY"))
    			{
    				flexible.Force.Y = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiForceZ"))
    			{
    				flexible.Force.Z = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("LightColorR"))
    			{
    				light.Color.setR(Float.parseFloat(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("LightColorG"))
    			{
    				light.Color.setG(Float.parseFloat(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("LightColorB"))
    			{
    				light.Color.setB(Float.parseFloat(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("LightColorA"))
    			{
    				light.Color.setA(Float.parseFloat(node.getFirstChild().getNodeValue().trim()));
    			}
    			else if(nodeName.equalsIgnoreCase("LightRadius"))
    			{
    				light.Radius = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("LightCutoff"))
    			{
    				light.Cutoff = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("LightFalloff"))
    			{
    				light.Falloff = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("LightIntensity"))
    			{
    				light.Intensity = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("FlexiEntry"))
    			{
    				hasFlexi = Boolean.parseBoolean(node.getFirstChild().getNodeValue().trim());
    			}
    			else if(nodeName.equalsIgnoreCase("LightEntry"))
    			{
    				hasLight = Boolean.parseBoolean(node.getFirstChild().getNodeValue().trim());
    			}
    			else
    			{
    				JLogger.warn("Not handled parsing of node " + root.getNodeName() + ":" + nodeName);
    			}

    		}
    	}
    	
        shape.ProfileCurve = Utils.ubyteToInt(profileShape.getIndex()) | Utils.ubyteToInt(holeType.getIndex());
        if (!sculptTexture.equals(UUID.Zero))
        {
            prim.Sculpt = new PrimObject.SculptBlock();
            prim.Sculpt.Texture = sculptTexture;
            prim.Sculpt.Type = Utils.ubyteToInt(sculptType.getIndex());
        }
    	
        if (hasFlexi)
            prim.Flexible = flexible;
        if (hasLight)
            prim.Light = light;
        
    	return shape;
	}

	static private PrimObject.InventoryBlock.ItemBlock ReadTaskInventoryItem(Node root) throws AssetPrimitiveParsingException
    {
    	PrimObject.InventoryBlock.ItemBlock item = new PrimObject.InventoryBlock.ItemBlock();
      	NodeList inodeList = root.getChildNodes();
    	for(int j = 0; j < inodeList.getLength(); j++)
    	{
    		Node inode = inodeList.item(j);
    		if(inode.getNodeType() == Node.ELEMENT_NODE)
    		{
    			String inodeName = inode.getNodeName();
    			if(inodeName.equalsIgnoreCase("AssetID"))
    			{
    				item.AssetID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("BasePermissions"))
    			{
    				item.PermsBase = Integer.parseInt(inode.getFirstChild().getNodeValue().trim());
    			}
    			else if(inodeName.equalsIgnoreCase("CreationDate"))
    			{
    				item.CreationDate = Utils.unixTimeToDate(Long.parseLong(inode.getFirstChild().getNodeValue().trim()));
    			} 
    			else if(inodeName.equalsIgnoreCase("CreatorID"))
    			{
    				item.CreatorID = ReadUUID(inode);
    			} 
    			else if(inodeName.equalsIgnoreCase("CreationDate"))
    			{
    				item.CreatorID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("Description"))
    			{
    				item.Description = inode.getFirstChild().getNodeValue().trim();
    			}
    			else if(inodeName.equalsIgnoreCase("EveryonePermissions"))
    			{
    				item.PermsEveryone = Long.parseLong(inode.getFirstChild().getNodeValue().trim());
    			}
    			else if(inodeName.equalsIgnoreCase("Flags"))
    			{
    				item.Flags = Integer.parseInt(inode.getFirstChild().getNodeValue().trim());
    			}
    			else if(inodeName.equalsIgnoreCase("GroupPermissions"))
    			{
    				item.PermsGroup = Long.parseLong(inode.getFirstChild().getNodeValue().trim());
    			}
    			else if(inodeName.equalsIgnoreCase("InvType"))
    			{
    				item.InvType = InventoryType.get((byte)Integer.parseInt(inode.getFirstChild().getNodeValue().trim()));
    			}
    			else if(inodeName.equalsIgnoreCase("ItemID"))
    			{
    				item.ID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("OldItemID"))
    			{
    				//FIXME is it required
    				//    	    			item.oldItemID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("LastOwnerID"))
    			{
    				item.LastOwnerID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("Name"))
    			{
    				item.Name = inode.getFirstChild().getNodeValue().trim();
    			}
    			else if(inodeName.equalsIgnoreCase("NextPermissions"))
    			{
    				item.PermsNextOwner = Long.parseLong(inode.getFirstChild().getNodeValue().trim());
    			}
    			else if(inodeName.equalsIgnoreCase("OwnerID"))
    			{
    				item.OwnerID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("CurrentPermissions"))
    			{
    				item.PermsOwner = Long.parseLong(inode.getFirstChild().getNodeValue().trim());
    			}
    			else if(inodeName.equalsIgnoreCase("ParentID"))
    			{
    				//TODO handle
    				//    	    			item.parentID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("ParentPartID"))
    			{
    				//TODO handle
    				//    	    			item.parentPartID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("PermsGranter"))
    			{
    				item.PermsGranterID = ReadUUID(inode);
    			}
    			else if(inodeName.equalsIgnoreCase("PermsMask"))
    			{
    				item.PermsBase = Long.parseLong(inode.getFirstChild().getNodeValue().trim());
    			}
    			else if(inodeName.equalsIgnoreCase("Type"))
    			{
    				item.Type = AssetType.get((byte)Integer.parseInt(inode.getFirstChild().getNodeValue().trim()));
    			}
    			else
    				throw new AssetPrimitiveParsingException("Not implemented node: " + inodeName);
    		}
    	}
    	return item;
    }
    
    
    static PrimObject.InventoryBlock.ItemBlock[] ReadTaskInventoryItems(Node root) throws AssetPrimitiveParsingException
    {
    	List<PrimObject.InventoryBlock.ItemBlock> invItems = new ArrayList<PrimObject.InventoryBlock.ItemBlock>();
    	
      	NodeList nodeList = root.getChildNodes();		
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		Node node = nodeList.item(i);
    		if(node.getNodeType() == Node.ELEMENT_NODE 
    				&& node.getNodeName().equalsIgnoreCase("TaskInventoryItem"))
    		{
    			if(node.getNodeName().equalsIgnoreCase("TaskInventoryItem"))
    				invItems.add(ReadTaskInventoryItem(node));
    			else
    				new AssetPrimitiveParsingException("Not implemented Node: " + node.getNodeName());
    		}
    	}
       return invItems.toArray(new PrimObject.InventoryBlock.ItemBlock[0]);
    }
    
    
    static UUID ReadUUID(Node root)
    {
        UUID id;
        String idStr = "";

      	NodeList nodeList = root.getChildNodes();		
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		Node node = nodeList.item(i);
    		if(node.getNodeType() == Node.ELEMENT_NODE)
    		{
    			String nodeName = node.getNodeName();
    			if(nodeName.equalsIgnoreCase("Guid"))
    			{
    	            idStr = node.getFirstChild().getNodeValue().trim();
    	            break;
    			}
    			else if(nodeName.equalsIgnoreCase("UUID"))
    			{
    	            idStr = node.getFirstChild().getNodeValue().trim();
    	            break;
    			}
    		}
    	}

        id  = UUID.Parse(idStr);
        return id;
    }

    
    static Color4 ReadColor(Node root)
    {
        Color4 color = new Color4();

    	NodeList nodeList = root.getChildNodes();		
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		Node node = nodeList.item(i);
    		if(node.getNodeType() == Node.ELEMENT_NODE)
    		{
    			String nodeName = node.getNodeName();
    			if(nodeName.equalsIgnoreCase("R"))
    				color.setR(Float.parseFloat(node.getFirstChild().getNodeValue().trim()));
    			else if(nodeName.equalsIgnoreCase("G"))
    				color.setG(Float.parseFloat(node.getFirstChild().getNodeValue().trim()));
    			else if(nodeName.equalsIgnoreCase("B"))
    				color.setB(Float.parseFloat(node.getFirstChild().getNodeValue().trim()));
    			else if(nodeName.equalsIgnoreCase("A"))
    				color.setA(Float.parseFloat(node.getFirstChild().getNodeValue().trim())); 
    		}
    	}
    	
        return color;
    }
    
    static Vector3 ReadVector(Node root)
    {
        Vector3 vec = new Vector3();

    	NodeList nodeList = root.getChildNodes();		
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		Node node = nodeList.item(i);
    		if(node.getNodeType() == Node.ELEMENT_NODE)
    		{
    			String nodeName = node.getNodeName();
    			if(nodeName.equalsIgnoreCase("X"))
    				vec.X = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			else if(nodeName.equalsIgnoreCase("Y"))
    				vec.Y = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			else if(nodeName.equalsIgnoreCase("Z"))
    				vec.Z = Float.parseFloat(node.getFirstChild().getNodeValue().trim());  
    		}
    	}
    	
        return vec;
    }

    static Quaternion ReadQuaternion(Node root)
    {
        Quaternion quat = new Quaternion();

        
      	NodeList nodeList = root.getChildNodes();		
    	for(int i = 0; i < nodeList.getLength(); i++)
    	{
    		Node node = nodeList.item(i);
    		if(node.getNodeType() == Node.ELEMENT_NODE)
    		{
    			String nodeName = node.getNodeName();
    			if(nodeName.equalsIgnoreCase("X"))
    				quat.X = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			else if(nodeName.equalsIgnoreCase("Y"))
    				quat.Y = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
    			else if(nodeName.equalsIgnoreCase("Z"))
    				quat.Z = Float.parseFloat(node.getFirstChild().getNodeValue().trim());
       			else if(nodeName.equalsIgnoreCase("W"))
    				quat.W = Float.parseFloat(node.getFirstChild().getNodeValue().trim()); 
    		}
    	}
        return quat;
    }


  /// <summary>
  /// The deserialized form of a single primitive in a linkset asset
  /// </summary>
  public static class PrimObject
  {
      public static class FlexibleBlock
      {
          public int Softness;
          public float Gravity;
          public float Drag;
          public float Wind;
          public float Tension;
          public Vector3 Force;

          public OSDMap Serialize()
          {
              OSDMap map = new OSDMap();
              map.put("softness", OSD.FromInteger(Softness));
              map.put("gravity", OSD.FromReal(Gravity));
              map.put("drag", OSD.FromReal(Drag));
              map.put("wind", OSD.FromReal(Wind));
              map.put("tension", OSD.FromReal(Tension));
              map.put("force", OSD.FromVector3(Force));
              return map;
          }

          public void Deserialize(OSDMap map)
          {
              Softness = map.get("softness").asInteger();
              Gravity = (float)map.get("gravity").asReal();
              Drag = (float)map.get("drag").asReal();
              Wind = (float)map.get("wind").asReal();
              Tension = (float)map.get("tension").asReal();
              Force = map.get("force").asVector3();
          }
      }
    
    public static class LightBlock
    {
        public Color4 Color;
        public float Intensity;
        public float Radius;
        public float Falloff;
        public float Cutoff;

        public OSDMap Serialize()
        {
            OSDMap map = new OSDMap();
            map.put("color", OSD.FromColor4(Color));
            map.put("intensity", OSD.FromReal(Intensity));
            map.put("radius", OSD.FromReal(Radius));
            map.put("falloff", OSD.FromReal(Falloff));
            map.put("cutoff", OSD.FromReal(Cutoff));
            return map;
        }

        public void Deserialize(OSDMap map)
        {
            Color = map.get("color").asColor4();
            Intensity = (float)map.get("intensity").asReal();
            Radius = (float)map.get("radius").asReal();
            Falloff = (float)map.get("falloff").asReal();
            Cutoff = (float)map.get("cutoff").asReal();
        }
    }

    public static class SculptBlock
    {
        public UUID Texture;
        public int Type;

        public OSDMap Serialize()
        {
            OSDMap map = new OSDMap();
            map.put("texture", OSD.FromUUID(Texture));
            map.put("type", OSD.FromInteger(Type));
            return map;
        }

        public void Deserialize(OSDMap map)
        {
            Texture = map.get("texture").asUUID();
            Type = map.get("type").asInteger();
        }
    }

    public static class ParticlesBlock
    {
        public int Flags;
        public int Pattern;
        public float MaxAge;
        public float StartAge;
        public float InnerAngle;
        public float OuterAngle;
        public float BurstRate;
        public float BurstRadius;
        public float BurstSpeedMin;
        public float BurstSpeedMax;
        public int BurstParticleCount;
        public Vector3 AngularVelocity;
        public Vector3 Acceleration;
        public UUID TextureID;
        public UUID TargetID;
        public int DataFlags;
        public float ParticleMaxAge;
        public Color4 ParticleStartColor;
        public Color4 ParticleEndColor;
        public Vector2 ParticleStartScale;
        public Vector2 ParticleEndScale;

        public OSDMap Serialize()
        {
            OSDMap map = new OSDMap();
            map.put("flags", OSD.FromInteger(Flags));
            map.put("pattern", OSD.FromInteger(Pattern));
            map.put("max_age", OSD.FromReal(MaxAge));
            map.put("start_age", OSD.FromReal(StartAge));
            map.put("inner_angle", OSD.FromReal(InnerAngle));
            map.put("outer_angle", OSD.FromReal(OuterAngle));
            map.put("burst_rate", OSD.FromReal(BurstRate));
            map.put("burst_radius", OSD.FromReal(BurstRadius));
            map.put("burst_speed_min", OSD.FromReal(BurstSpeedMin));
            map.put("burst_speed_max", OSD.FromReal(BurstSpeedMax));
            map.put("burst_particle_count", OSD.FromInteger(BurstParticleCount));
            map.put("angular_velocity", OSD.FromVector3(AngularVelocity));
            map.put("acceleration", OSD.FromVector3(Acceleration));
            map.put("texture_id", OSD.FromUUID(TextureID));
            map.put("target_id", OSD.FromUUID(TargetID));
            map.put("data_flags", OSD.FromInteger(DataFlags));
            map.put("particle_max_age", OSD.FromReal(ParticleMaxAge));
            map.put("particle_start_color", OSD.FromColor4(ParticleStartColor));
            map.put("particle_end_color", OSD.FromColor4(ParticleEndColor));
            map.put("particle_start_scale", OSD.FromVector2(ParticleStartScale));
            map.put("particle_end_scale", OSD.FromVector2(ParticleEndScale));
            return map;
        }

        public void Deserialize(OSDMap map)
        {
            Flags = map.get("flags").asInteger();
            Pattern = map.get("pattern").asInteger();
            MaxAge = (float)map.get("max_age").asReal();
            StartAge = (float)map.get("start_age").asReal();
            InnerAngle = (float)map.get("inner_angle").asReal();
            OuterAngle = (float)map.get("outer_angle").asReal();
            BurstRate = (float)map.get("burst_rate").asReal();
            BurstRadius = (float)map.get("burst_radius").asReal();
            BurstSpeedMin = (float)map.get("burst_speed_min").asReal();
            BurstSpeedMax = (float)map.get("burst_speed_max").asReal();
            BurstParticleCount = map.get("burst_particle_count").asInteger();
            AngularVelocity = map.get("angular_velocity").asVector3();
            Acceleration = map.get("acceleration").asVector3();
            TextureID = map.get("texture_id").asUUID();
            DataFlags = map.get("data_flags").asInteger();
            ParticleMaxAge = (float)map.get("particle_max_age").asReal();
            ParticleStartColor = map.get("particle_start_color").asColor4();
            ParticleEndColor = map.get("particle_end_color").asColor4();
            ParticleStartScale = map.get("particle_start_scale").asVector2();
            ParticleEndScale = map.get("particle_end_scale").asVector2();
        }
    }

    public static class ShapeBlock
    {
        public int PathCurve;
        public float PathBegin;
        public float PathEnd;
        public float PathScaleX;
        public float PathScaleY;
        public float PathShearX;
        public float PathShearY;
        public float PathTwist;
        public float PathTwistBegin;
        public float PathRadiusOffset;
        public float PathTaperX;
        public float PathTaperY;
        public float PathRevolutions;
        public float PathSkew;
        public int ProfileCurve;
        public float ProfileBegin;
        public float ProfileEnd;
        public float ProfileHollow;

        public OSDMap Serialize()
        {
            OSDMap map = new OSDMap();
            map.put("path_curve", OSD.FromInteger(PathCurve));
            map.put("path_begin", OSD.FromReal(PathBegin));
            map.put("path_end", OSD.FromReal(PathEnd));
            map.put("path_scale_x", OSD.FromReal(PathScaleX));
            map.put("path_scale_y", OSD.FromReal(PathScaleY));
            map.put("path_shear_x", OSD.FromReal(PathShearX));
            map.put("path_shear_y", OSD.FromReal(PathShearY));
            map.put("path_twist", OSD.FromReal(PathTwist));
            map.put("path_twist_begin", OSD.FromReal(PathTwistBegin));
            map.put("path_radius_offset", OSD.FromReal(PathRadiusOffset));
            map.put("path_taper_x", OSD.FromReal(PathTaperX));
            map.put("path_taper_y", OSD.FromReal(PathTaperY));
            map.put("path_revolutions", OSD.FromReal(PathRevolutions));
            map.put("path_skew", OSD.FromReal(PathSkew));
            map.put("profile_curve", OSD.FromInteger(ProfileCurve));
            map.put("profile_begin", OSD.FromReal(ProfileBegin));
            map.put("profile_end", OSD.FromReal(ProfileEnd));
            map.put("profile_hollow", OSD.FromReal(ProfileHollow));
            return map;
        }

        public void Deserialize(OSDMap map)
        {
            PathCurve = map.get("path_curve").asInteger();
            PathBegin = (float)map.get("path_begin").asReal();
            PathEnd = (float)map.get("path_end").asReal();
            PathScaleX = (float)map.get("path_scale_x").asReal();
            PathScaleY = (float)map.get("path_scale_y").asReal();
            PathShearX = (float)map.get("path_shear_x").asReal();
            PathShearY = (float)map.get("path_shear_y").asReal();
            PathTwist = (float)map.get("path_twist").asReal();
            PathTwistBegin = (float)map.get("path_twist_begin").asReal();
            PathRadiusOffset = (float)map.get("path_radius_offset").asReal();
            PathTaperX = (float)map.get("path_taper_x").asReal();
            PathTaperY = (float)map.get("path_taper_y").asReal();
            PathRevolutions = (float)map.get("path_revolutions").asReal();
            PathSkew = (float)map.get("path_skew").asReal();
            ProfileCurve = map.get("profile_curve").asInteger();
            ProfileBegin = (float)map.get("profile_begin").asReal();
            ProfileEnd = (float)map.get("profile_end").asReal();
            ProfileHollow = (float)map.get("profile_hollow").asReal();
        }
    }

    public static class InventoryBlock
    {
        public static class ItemBlock
        {
            public UUID ID;
            public String Name;
            public UUID OwnerID;
            public UUID CreatorID;
            public UUID GroupID;
            public UUID LastOwnerID;
            public UUID PermsGranterID;
            public UUID AssetID;
            public AssetType Type;
            public InventoryType InvType;
            public String Description;
            //uint
            public long PermsBase;
            public long PermsOwner;
            public long PermsGroup;
            public long PermsEveryone;
            public long PermsNextOwner;
            
            public int Flags;
            public Date CreationDate;

            public OSDMap Serialize()
            {
                OSDMap map = new OSDMap();
                map.put("id", OSD.FromUUID(ID));
                map.put("name", OSD.FromString(Name));
                map.put("owner_id", OSD.FromUUID(OwnerID));
                map.put("creator_id", OSD.FromUUID(CreatorID));
                map.put("group_id", OSD.FromUUID(GroupID));
                map.put("last_owner_id", OSD.FromUUID(LastOwnerID));
                map.put("perms_granter_id", OSD.FromUUID(PermsGranterID));
                map.put("asset_id", OSD.FromUUID(AssetID));
                map.put("asset_type", OSD.FromInteger((int)Type.getIndex()));
                map.put("inv_type", OSD.FromInteger((int)InvType.getIndex()));
                map.put("description", OSD.FromString(Description));
                map.put("perms_base", OSD.FromUInteger(PermsBase));
                map.put("perms_owner", OSD.FromUInteger(PermsOwner));
                map.put("perms_group", OSD.FromUInteger(PermsGroup));
                map.put("perms_everyone", OSD.FromUInteger(PermsEveryone));
                map.put("perms_next_owner", OSD.FromUInteger(PermsNextOwner));
                map.put("flags", OSD.FromInteger(Flags));
                map.put("creation_date", OSD.FromDate(CreationDate));
                return map;
            }

            public void Deserialize(OSDMap map)
            {
                ID = map.get("id").asUUID();
                Name = map.get("name").asString();
                OwnerID = map.get("owner_id").asUUID();
                CreatorID = map.get("creator_id").asUUID();
                GroupID = map.get("group_id").asUUID();
                LastOwnerID = map.get("last_owner_id").asUUID();
                PermsGranterID = map.get("perms_granter_id").asUUID();
                AssetID = map.get("asset_id").asUUID();
                Type = AssetType.get((byte)map.get("asset_type").asInteger());
                InvType = InventoryType.get((byte)map.get("inv_type").asInteger());
                Description = map.get("description").asString();
                PermsBase = map.get("perms_base").asUInteger();
                PermsOwner = map.get("perms_owner").asUInteger();
                PermsGroup = map.get("perms_group").asUInteger();
                PermsEveryone = map.get("perms_everyone").asUInteger();
                PermsNextOwner = map.get("perms_next_owner").asUInteger();
                Flags = map.get("flags").asInteger();
                CreationDate = map.get("creation_date").asDate();
            }
        }

        public int Serial;
        public ItemBlock[] Items;

        public OSDMap Serialize()
        {
            OSDMap map = new OSDMap();
            map.put("serial", OSD.FromInteger(Serial));

            if (Items != null)
            {
                OSDArray array = new OSDArray(Items.length);
                for (int i = 0; i < Items.length; i++)
                    array.add(Items[i].Serialize());
                map.put("items", array);
            }

            return map;
        }

        public void Deserialize(OSDMap map)
        {
            Serial = map.get("serial").asInteger();

            if (map.containsKey("items"))
            {
                OSDArray array = (OSDArray)map.get("items");
                Items = new ItemBlock[array.count()];

                for (int i = 0; i < array.count(); i++)
                {
                    ItemBlock item = new ItemBlock();
                    item.Deserialize((OSDMap)array.get(i));
                    Items[i] = item;
                }
            }
            else
            {
                Items = new ItemBlock[0];
            }
        }
    }

    public UUID ID;
    public boolean AllowedDrop;
    public Vector3 AttachmentPosition;
    public Quaternion AttachmentRotation;
    public Quaternion BeforeAttachmentRotation;
    public String Name;
    public String Description;
    //uint
    public long PermsBase;
    public long PermsOwner;
    public long PermsGroup;
    public long PermsEveryone;
    public long PermsNextOwner;
    
    public UUID CreatorID;
    public UUID OwnerID;
    public UUID LastOwnerID;
    public UUID GroupID;
    public UUID FolderID;
    
    //ulong
    public BigInteger RegionHandle;
    public int ClickAction;
    public int LastAttachmentPoint;
    public int LinkNumber;
    
    //uint
    public long LocalID;
    public long ParentID;
    
    public Vector3 Position;
    public Quaternion Rotation;
    public Vector3 Velocity;
    public Vector3 AngularVelocity;
    public Vector3 Acceleration;
    public Vector3 Scale;
    public Vector3 SitOffset;
    public Quaternion SitRotation;
    public Vector3 CameraEyeOffset;
    public Vector3 CameraAtOffset;
    public int State;
    public int PCode;
    public int Material;
    public boolean PassTouches;
    public UUID SoundID;
    public float SoundGain;
    public float SoundRadius;
    public int SoundFlags;
    public Color4 TextColor;
    public String Text;
    public String SitName;
    public String TouchName;
    public boolean Selected;
    public UUID SelectorID;
    public boolean UsePhysics;
    public boolean Phantom;
    public int RemoteScriptAccessPIN;
    public boolean VolumeDetect;
    public boolean DieAtEdge;
    public boolean ReturnAtEdge;
    public boolean Temporary;
    public boolean Sandbox;
    public Date CreationDate;
    public Date RezDate;
    public int SalePrice;
    public int SaleType;
    public byte[] ScriptState;
    public UUID CollisionSound;
    public float CollisionSoundVolume;
    public PrimObject.FlexibleBlock Flexible;
    public LightBlock Light;
    public SculptBlock Sculpt;
    public ParticlesBlock Particles;
    public ShapeBlock Shape;
    public TextureEntry Textures;
    public InventoryBlock Inventory;

    public OSDMap Serialize()
    {
        OSDMap map = new OSDMap();
        map.put("id", OSD.FromUUID(ID));
        map.put("attachment_position", OSD.FromVector3(AttachmentPosition));
        map.put("attachment_rotation", OSD.FromQuaternion(AttachmentRotation));
        map.put("before_attachment_rotation", OSD.FromQuaternion(BeforeAttachmentRotation));
        map.put("name", OSD.FromString(Name));
        map.put("description", OSD.FromString(Description));
        map.put("perms_base", OSD.FromUInteger(PermsBase));
        map.put("perms_owner", OSD.FromUInteger(PermsOwner));
        map.put("perms_group", OSD.FromUInteger(PermsGroup));
        map.put("perms_everyone", OSD.FromUInteger(PermsEveryone));
        map.put("perms_next_owner", OSD.FromUInteger(PermsNextOwner));
        map.put("creator_identity", OSD.FromUUID(CreatorID));
        map.put("owner_identity", OSD.FromUUID(OwnerID));
        map.put("last_owner_identity", OSD.FromUUID(LastOwnerID));
        map.put("group_identity", OSD.FromUUID(GroupID));
        map.put("folder_id", OSD.FromUUID(FolderID));
        map.put("region_handle", OSD.FromULong(RegionHandle));
        map.put("click_action", OSD.FromInteger(ClickAction));
        map.put("last_attachment_point", OSD.FromInteger(LastAttachmentPoint));
        map.put("link_number", OSD.FromInteger(LinkNumber));
        map.put("local_id", OSD.FromUInteger(LocalID));
        map.put("parent_id", OSD.FromUInteger(ParentID));
        map.put("position", OSD.FromVector3(Position));
        map.put("rotation", OSD.FromQuaternion(Rotation));
        map.put("velocity", OSD.FromVector3(Velocity));
        map.put("angular_velocity", OSD.FromVector3(AngularVelocity));
        map.put("acceleration", OSD.FromVector3(Acceleration));
        map.put("scale", OSD.FromVector3(Scale));
        map.put("sit_offset", OSD.FromVector3(SitOffset));
        map.put("sit_rotation", OSD.FromQuaternion(SitRotation));
        map.put("camera_eye_offset", OSD.FromVector3(CameraEyeOffset));
        map.put("camera_at_offset", OSD.FromVector3(CameraAtOffset));
        map.put("state", OSD.FromInteger(State));
        map.put("prim_code", OSD.FromInteger(PCode));
        map.put("material", OSD.FromInteger(Material));
        map.put("pass_touches", OSD.FromBoolean(PassTouches));
        map.put("sound_id", OSD.FromUUID(SoundID));
        map.put("sound_gain", OSD.FromReal(SoundGain));
        map.put("sound_radius", OSD.FromReal(SoundRadius));
        map.put("sound_flags", OSD.FromInteger(SoundFlags));
        map.put("text_color", OSD.FromColor4(TextColor));
        map.put("text", OSD.FromString(Text));
        map.put("sit_name", OSD.FromString(SitName));
        map.put("touch_name", OSD.FromString(TouchName));
        map.put("selected", OSD.FromBoolean(Selected));
        map.put("selector_id", OSD.FromUUID(SelectorID));
        map.put("use_physics", OSD.FromBoolean(UsePhysics));
        map.put("phantom", OSD.FromBoolean(Phantom));
        map.put("remote_script_access_pin", OSD.FromInteger(RemoteScriptAccessPIN));
        map.put("volume_detect", OSD.FromBoolean(VolumeDetect));
        map.put("die_at_edge", OSD.FromBoolean(DieAtEdge));
        map.put("return_at_edge", OSD.FromBoolean(ReturnAtEdge));
        map.put("temporary", OSD.FromBoolean(Temporary));
        map.put("sandbox", OSD.FromBoolean(Sandbox));
        map.put("creation_date", OSD.FromDate(CreationDate));
        map.put("rez_date", OSD.FromDate(RezDate));
        map.put("sale_price", OSD.FromInteger(SalePrice));
        map.put("sale_type", OSD.FromInteger(SaleType));

        if (Flexible != null)
            map.put("flexible", Flexible.Serialize());
        if (Light != null)
            map.put("light", Light.Serialize());
        if (Sculpt != null)
            map.put("sculpt", Sculpt.Serialize());
        if (Particles != null)
            map.put("particles", Particles.Serialize());
        if (Shape != null)
            map.put("shape", Shape.Serialize());
        if (Textures != null)
            map.put("textures", Textures.GetOSD());
        if (Inventory != null)
            map.put("inventory", Inventory.Serialize());

        return map;
    }

    public void Deserialize(OSDMap map)
    {
        ID = map.get("id").asUUID();
        AttachmentPosition = map.get("attachment_position").asVector3();
        AttachmentRotation = map.get("attachment_rotation").asQuaternion();
        BeforeAttachmentRotation = map.get("before_attachment_rotation").asQuaternion();
        Name = map.get("name").asString();
        Description = map.get("description").asString();
        PermsBase = map.get("perms_base").asUInteger();
        PermsOwner = map.get("perms_owner").asUInteger();
        PermsGroup = map.get("perms_group").asUInteger();
        PermsEveryone = map.get("perms_everyone").asUInteger();
        PermsNextOwner = map.get("perms_next_owner").asUInteger();
        CreatorID = map.get("creator_identity").asUUID();
        OwnerID = map.get("owner_identity").asUUID();
        LastOwnerID = map.get("last_owner_identity").asUUID();
        GroupID = map.get("group_identity").asUUID();
        FolderID = map.get("folder_id").asUUID();
        RegionHandle = map.get("region_handle").asULong();
        ClickAction = map.get("click_action").asInteger();
        LastAttachmentPoint = map.get("last_attachment_point").asInteger();
        LinkNumber = map.get("link_number").asInteger();
        LocalID = map.get("local_id").asUInteger();
        ParentID = map.get("parent_id").asUInteger();
        Position = map.get("position").asVector3();
        Rotation = map.get("rotation").asQuaternion();
        Velocity = map.get("velocity").asVector3();
        AngularVelocity = map.get("angular_velocity").asVector3();
        Acceleration = map.get("acceleration").asVector3();
        Scale = map.get("scale").asVector3();
        SitOffset = map.get("sit_offset").asVector3();
        SitRotation = map.get("sit_rotation").asQuaternion();
        CameraEyeOffset = map.get("camera_eye_offset").asVector3();
        CameraAtOffset = map.get("camera_at_offset").asVector3();
        State = map.get("state").asInteger();
        PCode = map.get("prim_code").asInteger();
        Material = map.get("material").asInteger();
        PassTouches = map.get("pass_touches").asBoolean();
        SoundID = map.get("sound_id").asUUID();
        SoundGain = (float)map.get("sound_gain").asReal();
        SoundRadius = (float)map.get("sound_radius").asReal();
        SoundFlags = map.get("sound_flags").asInteger();
        TextColor = map.get("text_color").asColor4();
        Text = map.get("text").asString();
        SitName = map.get("sit_name").asString();
        TouchName = map.get("touch_name").asString();
        Selected = map.get("selected").asBoolean();
        SelectorID = map.get("selector_id").asUUID();
        UsePhysics = map.get("use_physics").asBoolean();
        Phantom = map.get("phantom").asBoolean();
        RemoteScriptAccessPIN = map.get("remote_script_access_pin").asInteger();
        VolumeDetect = map.get("volume_detect").asBoolean();
        DieAtEdge = map.get("die_at_edge").asBoolean();
        ReturnAtEdge = map.get("return_at_edge").asBoolean();
        Temporary = map.get("temporary").asBoolean();
        Sandbox = map.get("sandbox").asBoolean();
        CreationDate = map.get("creation_date").asDate();
        RezDate = map.get("rez_date").asDate();
        SalePrice = map.get("sale_price").asInteger();
        SaleType = map.get("sale_type").asInteger();
    }

    public Primitive ToPrimitive()
    {
        Primitive prim = new Primitive();
        prim.Properties = new ObjectProperties();
        
        prim.Acceleration = this.Acceleration;
        prim.AngularVelocity = this.AngularVelocity;
        prim.ClickAction = EnumsPrimitive.ClickAction.get((byte)this.ClickAction);
        prim.Properties.CreationDate = this.CreationDate;
        prim.Properties.CreatorID = this.CreatorID;
        prim.Properties.Description = this.Description;
        if (this.DieAtEdge) prim.Flags = prim.Flags | PrimFlags.DieAtEdge.getIndex();
        prim.Properties.FolderID = this.FolderID;
        prim.Properties.GroupID = this.GroupID;
        prim.ID = this.ID;
        prim.Properties.LastOwnerID = this.LastOwnerID;
        prim.LocalID = this.LocalID;
        prim.PrimData.Material = EnumsPrimitive.Material.get((byte)this.Material);
        prim.Properties.Name = this.Name;
        prim.OwnerID = this.OwnerID;
        prim.ParentID = this.ParentID;
        prim.PrimData.PCode = EnumsPrimitive.PCode.get((byte)this.PCode);
        prim.Properties.Permissions = new Permissions(this.PermsBase, this.PermsEveryone, this.PermsGroup, this.PermsNextOwner, this.PermsOwner);
        if (this.Phantom) prim.Flags = prim.Flags | PrimFlags.Phantom.getIndex();
        prim.Position = this.Position;
        if (this.ReturnAtEdge) prim.Flags = prim.Flags | PrimFlags.ReturnAtEdge.getIndex();
        prim.Rotation = this.Rotation;
        prim.Properties.SalePrice = this.SalePrice;
        prim.Properties.SaleType = Enums.SaleType.get((byte)this.SaleType);
        if (this.Sandbox) prim.Flags = prim.Flags | PrimFlags.Sandbox.getIndex();
        prim.Scale = this.Scale;
        prim.SoundFlags = (byte)this.SoundFlags;
        prim.SoundGain = this.SoundGain;
        prim.Sound = this.SoundID;
        prim.SoundRadius = this.SoundRadius;
        prim.PrimData.State = (byte)this.State;
        if (this.Temporary) prim.Flags = prim.Flags | PrimFlags.Temporary.getIndex();
        prim.Text = this.Text;
        prim.TextColor = this.TextColor;
        prim.Textures = this.Textures;
        if (this.UsePhysics) prim.Flags = prim.Flags | PrimFlags.Physics.getIndex();
        prim.Velocity = this.Velocity;

        prim.PrimData.PathBegin = this.Shape.PathBegin;
        prim.PrimData.PathCurve = EnumsPrimitive.PathCurve.get((byte)this.Shape.PathCurve);
        prim.PrimData.PathEnd = this.Shape.PathEnd;
        prim.PrimData.PathRadiusOffset = this.Shape.PathRadiusOffset;
        prim.PrimData.PathRevolutions = this.Shape.PathRevolutions;
        prim.PrimData.PathScaleX = this.Shape.PathScaleX;
        prim.PrimData.PathScaleY = this.Shape.PathScaleY;
        prim.PrimData.PathShearX = this.Shape.PathShearX;
        prim.PrimData.PathShearY = this.Shape.PathShearY;
        prim.PrimData.PathSkew = this.Shape.PathSkew;
        prim.PrimData.PathTaperX = this.Shape.PathTaperX;
        prim.PrimData.PathTaperY = this.Shape.PathTaperY;
        prim.PrimData.PathTwist = this.Shape.PathTwist;
        prim.PrimData.PathTwistBegin = this.Shape.PathTwistBegin;
        prim.PrimData.ProfileBegin = this.Shape.ProfileBegin;
        prim.PrimData.profileCurve = (byte)this.Shape.ProfileCurve;
        prim.PrimData.ProfileEnd = this.Shape.ProfileEnd;
        prim.PrimData.ProfileHollow = this.Shape.ProfileHollow;

        if (this.Flexible != null)
        {
            prim.Flexible = new FlexibleData();
            prim.Flexible.Drag = this.Flexible.Drag;
            prim.Flexible.Force = this.Flexible.Force;
            prim.Flexible.Gravity = this.Flexible.Gravity;
            prim.Flexible.Softness = this.Flexible.Softness;
            prim.Flexible.Tension = this.Flexible.Tension;
            prim.Flexible.Wind = this.Flexible.Wind;
        }

        if (this.Light != null)
        {
            prim.Light = new LightData();
            prim.Light.Color = this.Light.Color;
            prim.Light.Cutoff = this.Light.Cutoff;
            prim.Light.Falloff = this.Light.Falloff;
            prim.Light.Intensity = this.Light.Intensity;
            prim.Light.Radius = this.Light.Radius;
        }

        if (this.Particles != null)
        {
            prim.ParticleSys = new ParticleSystem();
            prim.ParticleSys.AngularVelocity = this.Particles.AngularVelocity;
            prim.ParticleSys.PartAcceleration = this.Particles.Acceleration;
            prim.ParticleSys.BurstPartCount = (byte)this.Particles.BurstParticleCount;
            prim.ParticleSys.BurstRate = this.Particles.BurstRadius;
            prim.ParticleSys.BurstRate = this.Particles.BurstRate;
            prim.ParticleSys.BurstSpeedMax = this.Particles.BurstSpeedMax;
            prim.ParticleSys.BurstSpeedMin = this.Particles.BurstSpeedMin;
            prim.ParticleSys.PartDataFlags = ParticleSystem.ParticleDataFlags.get((long)this.Particles.DataFlags);
            prim.ParticleSys.PartFlags = this.Particles.Flags;
            prim.ParticleSys.InnerAngle = this.Particles.InnerAngle;
            prim.ParticleSys.MaxAge = this.Particles.MaxAge;
            prim.ParticleSys.OuterAngle = this.Particles.OuterAngle;
            prim.ParticleSys.PartEndColor = this.Particles.ParticleEndColor;
            prim.ParticleSys.PartEndScaleX = this.Particles.ParticleEndScale.X;
            prim.ParticleSys.PartEndScaleY = this.Particles.ParticleEndScale.Y;
            prim.ParticleSys.MaxAge = this.Particles.ParticleMaxAge;
            prim.ParticleSys.PartStartColor = this.Particles.ParticleStartColor;
            prim.ParticleSys.PartStartScaleX = this.Particles.ParticleStartScale.X;
            prim.ParticleSys.PartStartScaleY = this.Particles.ParticleStartScale.Y;
            prim.ParticleSys.Pattern = ParticleSystem.SourcePattern.get((byte)this.Particles.Pattern);
            prim.ParticleSys.StartAge = this.Particles.StartAge;
            prim.ParticleSys.Target = this.Particles.TargetID;
            prim.ParticleSys.Texture = this.Particles.TextureID;
        }

        if (this.Sculpt != null)
        {
            prim.Sculpt = new SculptData();
            prim.Sculpt.SculptTexture = this.Sculpt.Texture;
            prim.Sculpt.setType(SculptType.get((byte)this.Sculpt.Type));
        }

        return prim;
    }
  }
}
