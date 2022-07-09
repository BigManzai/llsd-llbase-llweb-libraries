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
package com.ngt.jopenmetaverse.shared.types;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.protocol.primitives.ParticleSystem.ParticleDataFlags;


public class EnumsPrimitive
{
    /// <summary>
    /// Identifier code for primitive types
    /// </summary>
    public static enum PCode
    {
        /// <summary>None</summary>
        None ((byte)0),
        /// <summary>A Primitive</summary>
        Prim ((byte)9),
        /// <summary>A Avatar</summary>
        Avatar ((byte)47),
        /// <summary>Linden grass</summary>
        Grass ((byte)95),
        /// <summary>Linden tree</summary>
        NewTree ((byte)111),
        /// <summary>A primitive that acts as the source for a particle stream</summary>
        ParticleSystem ((byte)143),
        /// <summary>A Linden tree</summary>
        Tree ((byte)255);
        
  		private byte index;
  		PCode(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,PCode> lookup  = new HashMap<Byte,PCode>();

		static {
			for(PCode s : EnumSet.allOf(PCode.class))
				lookup.put(s.getIndex(), s);
		}

		public static PCode get(Byte index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// Primary parameters for primitives such as Physics Enabled or Phantom
    ///Flags
    /// </summary>
    public static enum PrimFlags
    {
        /// <summary>Deprecated</summary>
        None ((long)0),
        /// <summary>Whether physics are enabled for this object</summary>
        Physics ((long)0x00000001),
        /// <summary></summary>
        CreateSelected ((long)0x00000002),
        /// <summary></summary>
        ObjectModify ((long)0x00000004),
        /// <summary></summary>
        ObjectCopy ((long)0x00000008),
        /// <summary></summary>
        ObjectAnyOwner ((long)0x00000010),
        /// <summary></summary>
        ObjectYouOwner ((long)0x00000020),
        /// <summary></summary>
        Scripted ((long)0x00000040),
        /// <summary>Whether this object contains an active touch script</summary>
        Touch ((long)0x00000080),
        /// <summary></summary>
        ObjectMove ((long)0x00000100),
        /// <summary>Whether this object can receive payments</summary>
        Money ((long)0x00000200),
        /// <summary>Whether this object is phantom (no collisions)</summary>
        Phantom ((long)0x00000400),
        /// <summary></summary>
        InventoryEmpty ((long)0x00000800),
        /// <summary></summary>
        JointHinge ((long)0x00001000),
        /// <summary></summary>
        JointP2P ((long)0x00002000),
        /// <summary></summary>
        JointLP2P ((long)0x00004000),
        /// <summary>Deprecated</summary>
        JointWheel ((long)0x00008000),
        /// <summary></summary>
        AllowInventoryDrop ((long)0x00010000),
        /// <summary></summary>
        ObjectTransfer ((long)0x00020000),
        /// <summary></summary>
        ObjectGroupOwned ((long)0x00040000),
        /// <summary>Deprecated</summary>
        ObjectYouOfficer ((long)0x00080000),
        /// <summary></summary>
        CameraDecoupled ((long)0x00100000),
        /// <summary></summary>
        AnimSource ((long)0x00200000),
        /// <summary></summary>
        CameraSource ((long)0x00400000),
        /// <summary></summary>
        CastShadows ((long)0x00800000),
        /// <summary>Server flag, will not be sent to clients. Specifies that
        /// the object is destroyed when it touches a simulator edge</summary>
        DieAtEdge ((long)0x01000000),
        /// <summary>Server flag, will not be sent to clients. Specifies that
        /// the object will be returned to the owner's inventory when it
        /// touches a simulator edge</summary>
        ReturnAtEdge ((long)0x02000000),
        /// <summary>Server flag, will not be sent to clients.</summary>
        Sandbox ((long)0x04000000),
        /// <summary>Server flag, will not be sent to client. Specifies that
        /// the object is hovering/flying</summary>
        Flying ((long)0x08000000),
        /// <summary></summary>
        ObjectOwnerModify ((long)0x10000000),
        /// <summary></summary>
        TemporaryOnRez ((long)0x20000000),
        /// <summary></summary>
        Temporary ((long)0x40000000),
        /// <summary></summary>
        ZlibCompressed ((long)0x80000000);
        
  		private long index;
  		PrimFlags(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}
		
		private static final Map<Long,PrimFlags> lookup  = new HashMap<Long,PrimFlags>();
		
		static {
			for(PrimFlags s : EnumSet.allOf(PrimFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<PrimFlags> get(Long index)
		{
			EnumSet<PrimFlags> enumsSet = EnumSet.allOf(PrimFlags.class);
			for(Entry<Long,PrimFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().longValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}
		
		public static long getIndex(EnumSet<PrimFlags> enumSet)
		{
			long ret = 0;
			for(PrimFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}
		
		public static long or(EnumSet<PrimFlags> p1, EnumSet<PrimFlags> p2)
		{
			return PrimFlags.getIndex(p1) | PrimFlags.getIndex(p2);
		}

		public static long or(EnumSet<PrimFlags> p1, PrimFlags p2)
		{
			return PrimFlags.getIndex(p1) | p2.getIndex();
		}
		
		public static long and(EnumSet<PrimFlags> p1, EnumSet<PrimFlags> p2)
		{
			return PrimFlags.getIndex(p1) & PrimFlags.getIndex(p2);
		}
		
		public static long and(EnumSet<PrimFlags> p1, PrimFlags p2)
		{
			return PrimFlags.getIndex(p1) & p2.getIndex();
		}
		
    }

    /// <summary>
    /// Sound flags for sounds attached to primitives
    /// </summary>
    public static enum SoundFlags
    {
        /// <summary></summary>
        None ((byte)0),
        /// <summary></summary>
        Loop ((byte)0x01),
        /// <summary></summary>
        SyncMaster ((byte)0x02),
        /// <summary></summary>
        SyncSlave ((byte)0x04),
        /// <summary></summary>
        SyncPending ((byte)0x08),
        /// <summary></summary>
        Queue ((byte)0x10),
        /// <summary></summary>
        Stop ((byte)0x20);
 		private byte index;
 		SoundFlags(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,SoundFlags> lookup  = new HashMap<Byte,SoundFlags>();

		static {
			for(SoundFlags s : EnumSet.allOf(SoundFlags.class))
				lookup.put(s.getIndex(), s);
		}
		
		 public static EnumSet<SoundFlags> get(Byte index)
	        {
	                EnumSet<SoundFlags> enumsSet = EnumSet.allOf(SoundFlags.class);
	                for(Entry<Byte,SoundFlags> entry: lookup.entrySet())
	                {
	                        if((entry.getKey().byteValue() | index) != index)
	                        {
	                                enumsSet.remove(entry.getValue());
	                        }
	                }
	                return enumsSet;
	        }

	        public static byte getIndex(EnumSet<SoundFlags> enumSet)
	        {
	                byte ret = 0;
	                for(SoundFlags s: enumSet)
	                {
	                        ret |= s.getIndex();
	                }
	                return ret;
	        }		
    }

    public static enum ProfileCurve
    {
        Circle ((byte)0x00),
        Square ((byte)0x01),
        IsoTriangle ((byte)0x02),
        EqualTriangle ((byte)0x03),
        RightTriangle ((byte)0x04),
        HalfCircle ((byte)0x05);
 		private byte index;
 		ProfileCurve(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,ProfileCurve> lookup  = new HashMap<Byte,ProfileCurve>();

		static {
			for(ProfileCurve s : EnumSet.allOf(ProfileCurve.class))
				lookup.put(s.getIndex(), s);
		}

		public static ProfileCurve get(Byte index)
		{
			return lookup.get(index);
		}
    }

    public static enum HoleType 
    {
        Same ((byte)0x00),
        Circle ((byte)0x10),
        Square ((byte)0x20),
        Triangle ((byte)0x30);
 		private byte index;
 		HoleType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,HoleType> lookup  = new HashMap<Byte,HoleType>();

		static {
			for(HoleType s : EnumSet.allOf(HoleType.class))
				lookup.put(s.getIndex(), s);
		}

		public static HoleType get(Byte index)
		{
			return lookup.get(index);
		}		
    }

    public static enum PathCurve 
    {
        Line ((byte)0x10),
        Circle ((byte)0x20),
        Circle2 ((byte)0x30),
        Test ((byte)0x40),
        Flexible ((byte)0x80);
 		private byte index;
 		PathCurve(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,PathCurve> lookup  = new HashMap<Byte,PathCurve>();

		static {
			for(PathCurve s : EnumSet.allOf(PathCurve.class))
				lookup.put(s.getIndex(), s);
		}

		public static PathCurve get(Byte index)
		{
			return lookup.get(index);
		}		
    }

    /// <summary>
    /// Material type for a primitive
    /// </summary>
    public static enum Material
    {
        /// <summary></summary>
        Stone ((byte)0),
        /// <summary></summary>
        Metal ((byte)1),
        /// <summary></summary>
        Glass ((byte)2),
        /// <summary></summary>
        Wood ((byte)3),
        /// <summary></summary>
        Flesh ((byte)4),
        /// <summary></summary>
        Plastic ((byte)5),
        /// <summary></summary>
        Rubber ((byte)6),
        /// <summary></summary>
        Light ((byte)7);
 		private byte index;
 		Material(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,Material> lookup  = new HashMap<Byte,Material>();

		static {
			for(Material s : EnumSet.allOf(Material.class))
				lookup.put(s.getIndex(), s);
		}

		public static Material get(Byte index)
		{
			return lookup.get(index);
		}	
    }

    /// <summary>
    /// Used in a helper function to roughly determine prim shape
    /// </summary>
    public static enum PrimType
    {
        Unknown(0),
        Box(1),
        Cylinder(2),
        Prism(3),
        Sphere(4),
        Torus(5),
        Tube(6),
        Ring(7),
        Sculpt(8),
        Mesh(9);
 		private int index;
 		PrimType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}
    }

    /// <summary>
    /// Extra parameters for primitives, these flags are for features that have
    /// been added after the original ObjectFlags that has all eight bits 
    /// reserved already
    /// </summary>
    //[Flags]
    public static enum ExtraParamType
    {
        /// <summary>Whether this object has flexible parameters</summary>
        Flexible (0x10),
        /// <summary>Whether this object has light parameters</summary>
        Light (0x20),
        /// <summary>Whether this object is a sculpted prim</summary>
        Sculpt (0x30),
        /// <summary>Whether this object is a mesh</summary>
        Mesh (0x60);
 		private int index;
 		ExtraParamType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}
		
		private static final Map<Integer,ExtraParamType> lookup  = new HashMap<Integer,ExtraParamType>();

		static {
			for(ExtraParamType s : EnumSet.allOf(ExtraParamType.class))
				lookup.put(s.getIndex(), s);
		}

        public static EnumSet<ExtraParamType> get(Integer index)
        {
                EnumSet<ExtraParamType> enumsSet = EnumSet.allOf(ExtraParamType.class);
                for(Entry<Integer,ExtraParamType> entry: lookup.entrySet())
                {
                        if((entry.getKey().intValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }

        public static int getIndex(EnumSet<ExtraParamType> enumSet)
        {
                int ret = 0;
                for(ExtraParamType s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }
    }

    /// <summary>
    /// 
    /// </summary>
    public static enum JointType 
    {
        /// <summary></summary>
        Invalid ((byte)0),
        /// <summary></summary>
        Hinge ((byte)1),
        /// <summary></summary>
        Point ((byte)2)
        // <summary></summary>
        //[Obsolete]
        //LPoint = 3,
        //[Obsolete]
        //Wheel = 4
        ;
 		private byte index;
 		JointType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		private static final Map<Byte,JointType> lookup  = new HashMap<Byte,JointType>();

		static {
			for(JointType s : EnumSet.allOf(JointType.class))
				lookup.put(s.getIndex(), s);
		}

		public static JointType get(Byte index)
		{
			return lookup.get(index);
		}	
		
    }

    /// <summary>
    /// 
    /// </summary>
    public static enum SculptType 
    {
        /// <summary></summary>
        None ((byte)0),
        /// <summary></summary>
        Sphere ((byte)1),
        /// <summary></summary>
        Torus ((byte)2),
        /// <summary></summary>
        Plane ((byte)3),
        /// <summary></summary>
        Cylinder ((byte)4),
        /// <summary></summary>
        Mesh ((byte)5),
        /// <summary></summary>
        Invert ((byte)64),
        /// <summary></summary>
        Mirror ((byte)128);
 		private byte index;
 		SculptType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,SculptType> lookup  = new HashMap<Byte,SculptType>();

		static {
			for(SculptType s : EnumSet.allOf(SculptType.class))
				lookup.put(s.getIndex(), s);
		}

		public static SculptType get(Byte index)
		{
			return lookup.get(index);
		}	
    }

    /// <summary>
    /// 
    /// </summary>
    public static enum FaceType
    {
        /// <summary></summary>
        PathBegin (0x1 << 0),
        /// <summary></summary>
        PathEnd (0x1 << 1),
        /// <summary></summary>
        InnerSide (0x1 << 2),
        /// <summary></summary>
        ProfileBegin (0x1 << 3),
        /// <summary></summary>
        ProfileEnd (0x1 << 4),
        /// <summary></summary>
        OuterSide0 (0x1 << 5),
        /// <summary></summary>
        OuterSide1 (0x1 << 6),
        /// <summary></summary>
        OuterSide2 (0x1 << 7),
        /// <summary></summary>
        OuterSide3 (0x1 << 8);
 		private int index;
 		FaceType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}
    }

    /// <summary>
    /// 
    /// </summary>
    public static enum ObjectCategory
    {
        /// <summary></summary>
        Invalid (-1),
        /// <summary></summary>
        None (0),
        /// <summary></summary>
        Owner (1),
        /// <summary></summary>
        Group (2),
        /// <summary></summary>
        Other (3),
        /// <summary></summary>
        Selected (4),
        /// <summary></summary>
        Temporary (5);
 		private int index;
 		ObjectCategory(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}
		
		private static final Map<Integer,ObjectCategory> lookup  = new HashMap<Integer,ObjectCategory>();

		static {
			for(ObjectCategory s : EnumSet.allOf(ObjectCategory.class))
				lookup.put(s.getIndex(), s);
		}

		public static ObjectCategory get(Integer index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// Attachment points for objects on avatar bodies
    /// </summary>
    /// <remarks>
    /// Both InventoryObject and InventoryAttachment types can be attached
    ///</remarks>
    public static enum AttachmentPoint
    {
        /// <summary>Right hand if object was not previously attached</summary>
        //[EnumInfo(Text = "Default")]
        Default ((byte)0),
        /// <summary>Chest</summary>
        //[EnumInfo(Text = "Chest")]
        Chest ((byte)1),
        /// <summary>Skull</summary>
        //[EnumInfo(Text = "Head")]
        Skull ((byte)3),
        /// <summary>Left shoulder</summary>
        //[EnumInfo(Text = "Left Shoulder")]
        LeftShoulder ((byte)4),
        /// <summary>Right shoulder</summary>
        //[EnumInfo(Text = "Right Shoulder")]
        RightShoulder ((byte)5),
        /// <summary>Left hand</summary>
        //[EnumInfo(Text = "Left Hand")]
        LeftHand ((byte)6),
        /// <summary>Right hand</summary>
        //[EnumInfo(Text = "Right Hand")]
        RightHand ((byte)7),
        /// <summary>Left foot</summary>
        //[EnumInfo(Text = "Left Foot")]
        LeftFoot ((byte)8),
        /// <summary>Right foot</summary>
        //[EnumInfo(Text = "Right Foot")]
        RightFoot ((byte)9),
        /// <summary>Spine</summary>
        //[EnumInfo(Text = "Back")]
        Spine ((byte)10),
        /// <summary>Pelvis</summary>
        //[EnumInfo(Text = "Pelvis")]
        Pelvis ((byte)11),
        /// <summary>Mouth</summary>
        //[EnumInfo(Text = "Mouth")]
        Mouth ((byte)12),
        /// <summary>Chin</summary>
        //[EnumInfo(Text = "Chin")]
        Chin ((byte)13),
        /// <summary>Left ear</summary>
        //[EnumInfo(Text = "Left Ear")]
        LeftEar ((byte)14),
        /// <summary>Right ear</summary>
        //[EnumInfo(Text = "Right Ear")]
        RightEar ((byte)15),
        /// <summary>Left eyeball</summary>
        //[EnumInfo(Text = "Left Eye")]
        LeftEyeball ((byte)16),
        /// <summary>Right eyeball</summary>
        //[EnumInfo(Text = "Right Eye")]
        RightEyeball ((byte)17),
        /// <summary>Nose</summary>
        //[EnumInfo(Text = "Nose")]
        Nose ((byte)18),
        /// <summary>Right upper arm</summary>
        //[EnumInfo(Text = "Right Upper Arm")]
        RightUpperArm ((byte)19),
        /// <summary>Right forearm</summary>
        //[EnumInfo(Text = "Right Lower Arm")]
        RightForearm ((byte)20),
        /// <summary>Left upper arm</summary>
        //[EnumInfo(Text = "Left Upper Arm")]
        LeftUpperArm ((byte)21),
        /// <summary>Left forearm</summary>
        //[EnumInfo(Text = "Left Lower Arm")]
        LeftForearm ((byte)22),
        /// <summary>Right hip</summary>
        //[EnumInfo(Text = "Right Hip")]
        RightHip ((byte)23),
        /// <summary>Right upper leg</summary>
        //[EnumInfo(Text = "Right Upper Leg")]
        RightUpperLeg ((byte)24),
        /// <summary>Right lower leg</summary>
        //[EnumInfo(Text = "Right Lower Leg")]
        RightLowerLeg ((byte)25),
        /// <summary>Left hip</summary>
        //[EnumInfo(Text = "Left Hip")]
        LeftHip ((byte)26),
        /// <summary>Left upper leg</summary>
        //[EnumInfo(Text = "Left Hip")]
        LeftUpperLeg ((byte)27),
        /// <summary>Left lower leg</summary>
        //[EnumInfo(Text = "Left Lower Leg")]
        LeftLowerLeg ((byte)28),
        /// <summary>Stomach</summary>
        //[EnumInfo(Text = "Belly")]
        Stomach ((byte)29),
        /// <summary>Left pectoral</summary>
        //[EnumInfo(Text = "Left Pec")]
        LeftPec ((byte)30),
        /// <summary>Right pectoral</summary>
        //[EnumInfo(Text = "Right Pec")]
        RightPec ((byte)31),
        /// <summary>HUD Center position 2</summary>
        //[EnumInfo(Text = "HUD Center 2")]
        HUDCenter2 ((byte)32),
        /// <summary>HUD Top-right</summary>
        //[EnumInfo(Text = "HUD Top Right")]
        HUDTopRight ((byte)33),
        /// <summary>HUD Top</summary>
        //[EnumInfo(Text = "HUD Top Center")]
        HUDTop ((byte)34),
        /// <summary>HUD Top-left</summary>
        //[EnumInfo(Text = "HUD Top Left")]
        HUDTopLeft ((byte)35),
        /// <summary>HUD Center</summary>
        //[EnumInfo(Text = "HUD Center 1")]
        HUDCenter ((byte)36),
        /// <summary>HUD Bottom-left</summary>
        //[EnumInfo(Text = "HUD Bottom Left")]
        HUDBottomLeft ((byte)37),
        /// <summary>HUD Bottom</summary>
        //[EnumInfo(Text = "HUD Bottom")]
        HUDBottom ((byte)38),
        /// <summary>HUD Bottom-right</summary>
        //[EnumInfo(Text = "HUD Bottom Right")]
        HUDBottomRight ((byte)39);
 		private byte index;
 		AttachmentPoint(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,AttachmentPoint> lookup  = new HashMap<Byte,AttachmentPoint>();

		static {
			for(AttachmentPoint s : EnumSet.allOf(AttachmentPoint.class))
				lookup.put(s.getIndex(), s);
		}

		public static AttachmentPoint get(Byte index)
		{
			return lookup.get(index);
		}
		
    }

    /// <summary>
    /// Tree foliage types
    /// </summary>
    public static enum Tree
    {
        /// <summary>Pine1 tree</summary>
        Pine1 ((byte)0),
        /// <summary>Oak tree</summary>
        Oak ((byte)1),
        /// <summary>Tropical Bush1</summary>
        TropicalBush1 ((byte)2),
        /// <summary>Palm1 tree</summary>
        Palm1 ((byte)3),
        /// <summary>Dogwood tree</summary>
        Dogwood ((byte)4),
        /// <summary>Tropical Bush2</summary>
        TropicalBush2 ((byte)5),
        /// <summary>Palm2 tree</summary>
        Palm2 ((byte)6),
        /// <summary>Cypress1 tree</summary>
        Cypress1 ((byte)7),
        /// <summary>Cypress2 tree</summary>
        Cypress2 ((byte)8),
        /// <summary>Pine2 tree</summary>
        Pine2 ((byte)9),
        /// <summary>Plumeria</summary>
        Plumeria ((byte)10),
        /// <summary>Winter pinetree1</summary>
        WinterPine1 ((byte)11),
        /// <summary>Winter Aspen tree</summary>
        WinterAspen ((byte)12),
        /// <summary>Winter pinetree2</summary>
        WinterPine2 ((byte)13),
        /// <summary>Eucalyptus tree</summary>
        Eucalyptus ((byte)14),
        /// <summary>Fern</summary>
        Fern ((byte)15),
        /// <summary>Eelgrass</summary>
        Eelgrass ((byte)16),
        /// <summary>Sea Sword</summary>
        SeaSword ((byte)17),
        /// <summary>Kelp1 plant</summary>
        Kelp1 ((byte)18),
        /// <summary>Beach grass</summary>
        BeachGrass1 ((byte)19),
        /// <summary>Kelp2 plant</summary>
        Kelp2 ((byte)20);
 		private byte index;
 		Tree(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,Tree> lookup  = new HashMap<Byte,Tree>();

		static {
			for(Tree s : EnumSet.allOf(Tree.class))
				lookup.put(s.getIndex(), s);
		}

		public static Tree get(Byte index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// Grass foliage types
    /// </summary>
    public static enum Grass
    {
        /// <summary></summary>
        Grass0 ((byte)0),
        /// <summary></summary>
        Grass1 ((byte)1),
        /// <summary></summary>
        Grass2 ((byte)2),
        /// <summary></summary>
        Grass3 ((byte)3),
        /// <summary></summary>
        Grass4 ((byte)4),
        /// <summary></summary>
        Undergrowth1 ((byte)5);
 		private byte index;
 		Grass(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,Grass> lookup  = new HashMap<Byte,Grass>();

		static {
			for(Grass s : EnumSet.allOf(Grass.class))
				lookup.put(s.getIndex(), s);
		}

		public static Grass get(Byte index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// Action associated with clicking on an object
    /// </summary>
    public static enum ClickAction 
    {
        /// <summary>Touch object</summary>
        Touch ((byte)0),
        /// <summary>Sit on object</summary>
        Sit ((byte)1),
        /// <summary>Purchase object or contents</summary>
        Buy ((byte)2),
        /// <summary>Pay the object</summary>
        Pay ((byte)3),
        /// <summary>Open task inventory</summary>
        OpenTask ((byte)4),
        /// <summary>Play parcel media</summary>
        PlayMedia ((byte)5),
        /// <summary>Open parcel media</summary>
        OpenMedia ((byte)6);
        
 		private byte index;
 		ClickAction(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,ClickAction> lookup  = new HashMap<Byte,ClickAction>();

		static {
			for(ClickAction s : EnumSet.allOf(ClickAction.class))
				lookup.put(s.getIndex(), s);
		}

		public static ClickAction get(Byte index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// Type of physics representation used for this prim in the simulator
    /// </summary>
    public static enum PhysicsShapeType
    {
        /// <summary>Use prim physics form this object</summary>
        Prim ((byte)0),
        /// <summary>No physics, prim doesn't collide</summary>
        None ((byte)1),
        /// <summary>Use convex hull represantion of this prim</summary>
        ConvexHull ((byte)2);
 		private byte index;
 		
 		private static final Map<Byte,PhysicsShapeType> lookup  = new HashMap<Byte,PhysicsShapeType>();

 		static {
        for(PhysicsShapeType s : EnumSet.allOf(PhysicsShapeType.class))
             lookup.put(s.getIndex(), s);
 		}
 		
 		PhysicsShapeType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		public static PhysicsShapeType get(byte index)
		{
			return lookup.get(index);
		}
    }
}