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
package com.ngt.jopenmetaverse.shared.protocol.primitives;

import java.math.BigInteger;
import java.util.EnumSet;

import com.ngt.jopenmetaverse.shared.protocol.NameValue;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ExtraParamType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.HoleType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.JointType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.Material;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PCode;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PathCurve;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PrimFlags;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PrimType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ProfileCurve;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.SculptType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.Tree;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.SoundFlags;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ClickAction;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Vector4;
import com.ngt.jopenmetaverse.shared.util.Utils;


public class Primitive
{
    /// <summary>
    /// Current version of the media data for the prim
    /// </summary>
    public String MediaVersion = "";

    /// <summary>
    /// Array of media entries indexed by face number
    /// </summary>
    public MediaEntry[] FaceMedia;
    
    // Used for packing and unpacking parameters
    protected final static float CUT_QUANTA = 0.00002f;
    protected final static float SCALE_QUANTA = 0.01f;
    protected final static float SHEAR_QUANTA = 0.01f;
    protected final static float TAPER_QUANTA = 0.01f;
    protected final static float REV_QUANTA = 0.015f;
    protected final static float HOLLOW_QUANTA = 0.00002f;

    //region Public Members

    /// <summary></summary>
    public UUID ID;
    /// <summary></summary>
    public UUID GroupID;
    /// <summary></summary>
    public long LocalID;
    /// <summary></summary>
    public long ParentID;
    /// <summary></summary>
    public BigInteger RegionHandle;
    /// <summary></summary>
    //EnumSet<PrimFlags>
    public long Flags;
    /// <summary>Foliage type for this primitive. Only applicable if this
    /// primitive is foliage</summary>
    public Tree TreeSpecies;
    /// <summary>Unknown</summary>
    public byte[] ScratchPad;
    /// <summary></summary>
    public Vector3 Position;
    /// <summary></summary>
    public Vector3 Scale;
    /// <summary></summary>
    public Quaternion Rotation = Quaternion.Identity;
    /// <summary></summary>
    public Vector3 Velocity;
    /// <summary></summary>
    public Vector3 AngularVelocity;
    /// <summary></summary>
    public Vector3 Acceleration;
    /// <summary></summary>
    public Vector4 CollisionPlane;
    /// <summary></summary>
    public FlexibleData Flexible;
    /// <summary></summary>
    public LightData Light;
    /// <summary></summary>
    public SculptData Sculpt;
    /// <summary></summary>
    public ClickAction ClickAction;
    /// <summary></summary>
    public UUID Sound;
    /// <summary>Identifies the owner if audio or a particle system is
    /// active</summary>
    public UUID OwnerID;
    /// <summary></summary>
    //EnumSet<SoundFlags>
    public long SoundFlags;
    /// <summary></summary>
    public float SoundGain;
    /// <summary></summary>
    public float SoundRadius;
    /// <summary></summary>
    public String Text;
    /// <summary></summary>
    public Color4 TextColor;
    /// <summary></summary>
    public String MediaURL;
    /// <summary></summary>
    public JointType Joint;
    /// <summary></summary>
    public Vector3 JointPivot;
    /// <summary></summary>
    public Vector3 JointAxisOrAnchor;
    /// <summary></summary>
    public NameValue[] NameValues;
    /// <summary></summary>
    public ConstructionData PrimData = new ConstructionData();
    /// <summary></summary>
    public ObjectProperties Properties = new ObjectProperties();
    /// <summary>Objects physics engine propertis</summary>
    public PhysicsProperties PhysicsProps = new PhysicsProperties();
    /// <summary>Extra data about primitive</summary>
    public Object Tag;
    /// <summary>Indicates if prim is attached to an avatar</summary>
    public boolean IsAttachment;
    
    /// <summary></summary>
    public ParticleSystem ParticleSys;

    /// <summary></summary>
    public TextureEntry Textures;
    /// <summary></summary>
    public TextureAnimation TextureAnim;
    
    //endregion Public Members

    //region Properties

    /// <summary>Uses basic heuristics to estimate the primitive shape</summary>
    public PrimType getType()
    {
            if (Sculpt != null && Sculpt.getType() != SculptType.None && !Sculpt.SculptTexture.equals(UUID.Zero))
            {
                if (Sculpt.getType() == SculptType.Mesh)
                    return PrimType.Mesh;
                else
                    return PrimType.Sculpt;
            }

            boolean linearPath = (PrimData.PathCurve == PathCurve.Line || PrimData.PathCurve == PathCurve.Flexible);
            float scaleY = PrimData.PathScaleY;

            if (linearPath)
            {
            	System.out.println("Prim ID " + LocalID + " profilecurve " + PrimData.profileCurve);
                switch (ProfileCurve.get(PrimData.profileCurve))
                {
                    case Circle:
                        return PrimType.Cylinder;
                    case Square:
                        return PrimType.Box;
                    case IsoTriangle:
                    case EqualTriangle:
                    case RightTriangle:
                        return PrimType.Prism;
                    case HalfCircle:
                    default:
                        return PrimType.Unknown;
                }
            }
            else
            {
                switch (PrimData.PathCurve)
                {
                    case Flexible:
                        return PrimType.Unknown;
                    case Circle:
                        switch (ProfileCurve.get(PrimData.profileCurve))
                        {
                            case Circle:
                                if (scaleY > 0.75f)
                                    return PrimType.Sphere;
                                else
                                    return PrimType.Torus;
                            case HalfCircle:
                                return PrimType.Sphere;
                            case EqualTriangle:
                                return PrimType.Ring;
                            case Square:
                                if (scaleY <= 0.75f)
                                    return PrimType.Tube;
                                else
                                    return PrimType.Unknown;
                            default:
                                return PrimType.Unknown;
                        }
                    case Circle2:
                        if (PrimData.profileCurve == ProfileCurve.Circle.getIndex())
                            return PrimType.Sphere;
                        else
                            return PrimType.Unknown;
                    default:
                        return PrimType.Unknown;
                }
            }
        }

    //endregion Properties

    //region Constructors

    /// <summary>
    /// Default finalructor
    /// </summary>
    public Primitive()
    {
        // Default a few null property values to ""
        Text = "";
        MediaURL = "";
    }

    public Primitive(Primitive prim) throws Exception
    {
        ID = prim.ID;
        GroupID = prim.GroupID;
        LocalID = prim.LocalID;
        ParentID = prim.ParentID;
        RegionHandle = prim.RegionHandle;
        Flags = prim.Flags;
        TreeSpecies = prim.TreeSpecies;
        if (prim.ScratchPad != null)
        {
            ScratchPad = new byte[prim.ScratchPad.length];
            System.arraycopy(prim.ScratchPad, 0, ScratchPad, 0, ScratchPad.length);
        }
        else
            ScratchPad = Utils.EmptyBytes;
        Position = prim.Position;
        Scale = prim.Scale;
        Rotation = prim.Rotation;
        Velocity = prim.Velocity;
        AngularVelocity = prim.AngularVelocity;
        Acceleration = prim.Acceleration;
        CollisionPlane = prim.CollisionPlane;
        Flexible = prim.Flexible;
        Light = prim.Light;
        Sculpt = prim.Sculpt;
        ClickAction = prim.ClickAction;
        Sound = prim.Sound;
        OwnerID = prim.OwnerID;
        SoundFlags = prim.SoundFlags;
        SoundGain = prim.SoundGain;
        SoundRadius = prim.SoundRadius;
        Text = prim.Text;
        TextColor = prim.TextColor;
        MediaURL = prim.MediaURL;
        Joint = prim.Joint;
        JointPivot = prim.JointPivot;            
        JointAxisOrAnchor = prim.JointAxisOrAnchor;
        if (prim.NameValues != null)
        {
            if (NameValues == null || NameValues.length != prim.NameValues.length)
                NameValues = new NameValue[prim.NameValues.length];
//            Array.Copy(prim.NameValues, NameValues, prim.NameValues.length);
            System.arraycopy(prim.NameValues, 0, NameValues, 0, prim.NameValues.length);
        }
        else
            NameValues = null;
        PrimData = prim.PrimData;
        Properties = prim.Properties;
        // FIXME: Get a real copy consructor for TextureEntry instead of serializing to bytes and back
        if (prim.Textures != null)
        {
            byte[] textureBytes = prim.Textures.GetBytes();
            Textures = new TextureEntry(textureBytes, 0, textureBytes.length);
        }
        else
        {
            Textures = null;
        }
        TextureAnim = prim.TextureAnim;
        ParticleSys = prim.ParticleSys;
    }
    //endregion Constructors

    //region Public Methods

    public OSD GetOSD()
    {
        OSDMap path = new OSDMap(14);
        path.put("begin", OSD.FromReal(PrimData.PathBegin));
        path.put("curve", OSD.FromInteger((int)PrimData.PathCurve.getIndex()));
        path.put("end", OSD.FromReal(PrimData.PathEnd));
        path.put("radius_offset", OSD.FromReal(PrimData.PathRadiusOffset));
        path.put("revolutions", OSD.FromReal(PrimData.PathRevolutions));
        path.put("scale_x", OSD.FromReal(PrimData.PathScaleX));
        path.put("scale_y", OSD.FromReal(PrimData.PathScaleY));
        path.put("shear_x", OSD.FromReal(PrimData.PathShearX));
        path.put("shear_y", OSD.FromReal(PrimData.PathShearY));
        path.put("skew", OSD.FromReal(PrimData.PathSkew));
        path.put("taper_x", OSD.FromReal(PrimData.PathTaperX));
        path.put("taper_y", OSD.FromReal(PrimData.PathTaperY));
        path.put("twist", OSD.FromReal(PrimData.PathTwist));
        path.put("twist_begin", OSD.FromReal(PrimData.PathTwistBegin));

        OSDMap profile = new OSDMap(4);
        profile.put("begin", OSD.FromReal(PrimData.ProfileBegin));
        profile.put("curve", OSD.FromInteger((int)PrimData.profileCurve));
        profile.put("hole", OSD.FromInteger((int)PrimData.getProfileHole().getIndex()));
        profile.put("end", OSD.FromReal(PrimData.ProfileEnd));
        profile.put("hollow", OSD.FromReal(PrimData.ProfileHollow));

        OSDMap volume = new OSDMap(2);
        volume.put("path",  path);
        volume.put("profile",  profile);

        OSDMap prim = new OSDMap(20);
        if (Properties != null)
        {
            prim.put("name", OSD.FromString(Properties.Name));
            prim.put("description", OSD.FromString(Properties.Description));
        }
        else
        {
            prim.put("name", OSD.FromString("Object"));
            prim.put("description", OSD.FromString(""));
        }
        
        prim.put("phantom", OSD.FromBoolean(((Flags & PrimFlags.Phantom.getIndex()) != 0)));
        prim.put("physical", OSD.FromBoolean(((Flags & PrimFlags.Physics.getIndex()) != 0)));
        prim.put("position", OSD.FromVector3(Position));
        prim.put("rotation", OSD.FromQuaternion(Rotation));
        prim.put("scale", OSD.FromVector3(Scale));
        prim.put("pcode", OSD.FromInteger((int)PrimData.PCode.getIndex()));
        prim.put("material", OSD.FromInteger((int)PrimData.Material.getIndex()));
        prim.put("shadows", OSD.FromBoolean(((Flags & PrimFlags.CastShadows.getIndex()) != 0)));
        prim.put("state", OSD.FromInteger(PrimData.State));

        prim.put("id", OSD.FromUUID(ID));
        prim.put("localid", OSD.FromLong(LocalID));
        prim.put("parentid", OSD.FromLong(ParentID));

        prim.put("volume", volume);

        if (Textures != null)
            prim.put("textures", Textures.GetOSD());
        
        if (Light != null)
            prim.put("light", Light.GetOSD());

        if (Flexible != null)
            prim.put("flex", Flexible.GetOSD());

        if (Sculpt != null)
            prim.put("sculpt", Sculpt.GetOSD());

        return prim;
    }

    public static Primitive FromOSD(OSD osd)
    {
        Primitive prim = new Primitive();
        ConstructionData data = new ConstructionData();

        OSDMap map = (OSDMap)osd;
        OSDMap volume = (OSDMap)map.get("volume");
        OSDMap path = (OSDMap)volume.get("path");
        OSDMap profile = (OSDMap)volume.get("profile");

        //region Path/Profile

        data.profileCurve = (byte)0;
        data.Material = Material.get((byte)map.get("material").asInteger());
        data.PCode = PCode.get((byte)map.get("pcode").asInteger());
        data.State = (byte)map.get("state").asInteger();

        data.PathBegin = (float)path.get("begin").asReal();
        data.PathCurve = PathCurve.get((byte)path.get("curve").asInteger());
        data.PathEnd = (float)path.get("end").asReal();
        data.PathRadiusOffset = (float)path.get("radius_offset").asReal();
        data.PathRevolutions = (float)path.get("revolutions").asReal();
        data.PathScaleX = (float)path.get("scale_x").asReal();
        data.PathScaleY = (float)path.get("scale_y").asReal();
        data.PathShearX = (float)path.get("shear_x").asReal();
        data.PathShearY = (float)path.get("shear_y").asReal();
        data.PathSkew = (float)path.get("skew").asReal();
        data.PathTaperX = (float)path.get("taper_x").asReal();
        data.PathTaperY = (float)path.get("taper_y").asReal();
        data.PathTwist = (float)path.get("twist").asReal();
        data.PathTwistBegin = (float)path.get("twist_begin").asReal();

        data.ProfileBegin = (float)profile.get("begin").asReal();
        data.ProfileEnd = (float)profile.get("end").asReal();
        data.ProfileHollow = (float)profile.get("hollow").asReal();
        data.profileCurve = (byte)profile.get("curve").asInteger();
        data.setProfileHole(HoleType.get((byte)profile.get("hole").asInteger()));

        //endregion Path/Profile

        prim.PrimData = data;

        if (map.get("phantom").asBoolean())
            prim.Flags =  prim.Flags | PrimFlags.Phantom.getIndex();

        if (map.get("physical").asBoolean())
        	prim.Flags =  prim.Flags | PrimFlags.Physics.getIndex();

        if (map.get("shadows").asBoolean())
        	prim.Flags =  prim.Flags | PrimFlags.CastShadows.getIndex();

        prim.ID = map.get("id").asUUID();
        prim.LocalID = map.get("localid").asLong();
        prim.ParentID = map.get("parentid").asLong();
        prim.Position = ((OSDArray)map.get("position")).asVector3();
        prim.Rotation = ((OSDArray)map.get("rotation")).asQuaternion();
        prim.Scale = ((OSDArray)map.get("scale")).asVector3();
        
        if (map.get("flex").asBoolean())
            prim.Flexible = FlexibleData.FromOSD(map.get("flex"));
        
        if (map.get("light").asBoolean())
            prim.Light = LightData.FromOSD(map.get("light"));
        
        if (map.get("sculpt").asBoolean())
            prim.Sculpt = SculptData.FromOSD(map.get("sculpt"));
        
        prim.Textures = TextureEntry.FromOSD(map.get("textures"));
        prim.Properties = new ObjectProperties();

        if (!Utils.isNullOrEmpty(map.get("name").asString()))
        {
            prim.Properties.Name = map.get("name").asString();
        }

        if (!Utils.isNullOrEmpty(map.get("description").asString()))
        {
            prim.Properties.Description = map.get("description").asString();
        }

        return prim;
    }

    public int SetExtraParamsFromBytes(byte[] data, int pos)
    {
        int i = pos;
        int totallength = 1;

        if (data.length == 0 || pos >= data.length)
            return 0;

        byte extraParamCount = data[i++];

        for (int k = 0; k < extraParamCount; k++)
        {
            EnumSet<ExtraParamType> type = ExtraParamType.get(Utils.bytesToIntLit(data, i));
            i += 2;

            long paramlength = Utils.bytesToInt64Lit(data, i);
            i += 4;

            if (ExtraParamType.getIndex(type) == ExtraParamType.Flexible.getIndex())
                Flexible = new FlexibleData(data, i);
            else if (ExtraParamType.getIndex(type) == ExtraParamType.Light.getIndex())
                Light = new LightData(data, i);
            else if (ExtraParamType.getIndex(type) == ExtraParamType.Sculpt.getIndex() || ExtraParamType.getIndex(type) == ExtraParamType.Mesh.getIndex())
                Sculpt = new SculptData(data, i);

            i += (int)paramlength;
            totallength += (int)paramlength + 6;
        }

        return totallength;
    }

    public byte[] GetExtraParamsBytes()
    {
        byte[] flexible = null;
        byte[] light = null;
        byte[] sculpt = null;
        byte[] buffer = null;
        int size = 1;
        int pos = 0;
        byte count = 0;

        if (Flexible != null)
        {
            flexible = Flexible.GetBytes();
            size += flexible.length + 6;
            ++count;
        }
        if (Light != null)
        {
            light = Light.GetBytes();
            size += light.length + 6;
            ++count;
        }
        if (Sculpt != null)
        {
            sculpt = Sculpt.GetBytes();
            size += sculpt.length + 6;
            ++count;
        }

        buffer = new byte[size];
        buffer[0] = count;
        ++pos;

        if (flexible != null)
        {
        	//Copying the short bytes
            System.arraycopy(Utils.intToBytesLit(ExtraParamType.Flexible.getIndex()), 2, buffer, pos, 2);
            pos += 2;

            //coping the int bytes
            System.arraycopy(Utils.int64ToBytesLit((long)flexible.length), 4, buffer, pos, 4);
            pos += 4;

            System.arraycopy(flexible, 0, buffer, pos, flexible.length);
            pos += flexible.length;
        }
        if (light != null)
        {
            System.arraycopy(Utils.intToBytesLit(ExtraParamType.Light.getIndex()), 2, buffer, pos, 2);
            pos += 2;

            System.arraycopy(Utils.int64ToBytesLit((long)light.length), 4, buffer, pos, 4);
            pos += 4;

            System.arraycopy(light, 0, buffer, pos, light.length);
            pos += light.length;
        }
        if (sculpt != null)
        {
            if (Sculpt.getType() == SculptType.Mesh)
            {
                System.arraycopy(Utils.intToBytesLit(ExtraParamType.Mesh.getIndex()), 2, buffer, pos, 2);
            }
            else
            {
                System.arraycopy(Utils.intToBytesLit(ExtraParamType.Sculpt.getIndex()), 2, buffer, pos, 2);
            }
            pos += 2;

            System.arraycopy(Utils.int64ToBytesLit((long)sculpt.length), 0, buffer, pos, 4);
            pos += 4;

            System.arraycopy(sculpt, 0, buffer, pos, sculpt.length);
            pos += sculpt.length;
        }

        return buffer;
    }

    //endregion Public Methods

    //region Overrides

    public  boolean equals(Object obj)
    {
        return (obj instanceof Primitive) ? equals((Primitive)obj) : false;
    }

    public boolean equals(Primitive other)
    {
        return equals(this, other);
    }

    public  String toString()
    {
        switch (PrimData.PCode)
        {
            case Prim:
                return String.format("%d (%s)", getType().getIndex(), ID.toString());
            default:
                return String.format("%b (%s)", PrimData.PCode.getIndex(), ID.toString());
        }
    }

    @Override
    public  int hashCode()
    {
    	
    	return ID.hashCode();
//        return
//            Position.hashCode() ^
//            Velocity.hashCode() ^
//            Acceleration.hashCode() ^
//            Rotation.hashCode() ^
//            AngularVelocity.hashCode() ^
//            ClickAction.hashCode() ^
//            (Flexible != null ? Flexible.hashCode() : 0) ^
//            (Light != null ? Light.hashCode() : 0) ^
//            (Sculpt != null ? Sculpt.hashCode() : 0) ^
//            Flags.hashCode() ^
//            PrimData.Material.hashCode() ^
//            MediaURL.hashCode() ^
//            //TODO: NameValues?
//            (Properties != null ? Properties.OwnerID.hashCode() : 0) ^
//            new Long(ParentID).hashCode() ^
//            new Float(PrimData.PathBegin).hashCode() ^
//            PrimData.PathCurve.hashCode() ^
//            new Float(PrimData.PathEnd).hashCode() ^
//            new Float(PrimData.PathRadiusOffset).hashCode() ^
//            new Float(PrimData.PathRevolutions).hashCode() ^
//            new Float(PrimData.PathScaleX).hashCode() ^
//            new Float(PrimData.PathScaleY).hashCode() ^
//            new Float(PrimData.PathShearX).hashCode() ^
//            new Float(PrimData.PathShearY).hashCode() ^
//            new Float(PrimData.PathSkew).hashCode() ^
//            new Float(PrimData.PathTaperX).hashCode() ^
//            new Float(PrimData.PathTaperY).hashCode() ^
//            new Float(PrimData.PathTwist).hashCode() ^
//            new Float(PrimData.PathTwistBegin).hashCode() ^
//            PrimData.PCode.hashCode() ^
//            new Float(PrimData.ProfileBegin).hashCode() ^
//            new Byte(PrimData.profileCurve).hashCode() ^
//            new Float(PrimData.ProfileEnd).hashCode() ^
//            new Float(PrimData.ProfileHollow).hashCode() ^
//            ParticleSys.hashCode() ^
//            TextColor.hashCode() ^
//            TextureAnim.hashCode() ^
//            (Textures != null ? Textures.hashCode() : 0) ^
//            new Float(SoundRadius).hashCode() ^
//            Scale.hashCode() ^
//            Sound.hashCode() ^
//            new Byte(PrimData.State).hashCode() ^
//            Text.hashCode() ^
//            TreeSpecies.hashCode();
    }

    //endregion Overrides

    //region Operators

    public static boolean equals(Primitive lhs, Primitive rhs)
    {
        if ((Object)lhs == null || (Object)rhs == null)
        {
            return (Object)rhs == (Object)lhs;
        }
        return (lhs.ID.equals(rhs.ID));
    }

    public static boolean notEquals(Primitive lhs, Primitive rhs)
    {
        if ((Object)lhs == null || (Object)rhs == null)
        {
            return (Object)rhs != (Object)lhs;
        }
        return !(lhs.ID.equals(rhs.ID));
    }

    //endregion Operators

    //region Parameter Packing Methods

    public static int PackBeginCut(float beginCut)
    {
        return (int)Math.round(beginCut / CUT_QUANTA);
    }

    public static int PackEndCut(float endCut)
    {
        return (int)(50000 - (int)Math.round(endCut / CUT_QUANTA));
    }

    public static byte PackPathScale(float pathScale)
    {
        return (byte)(200 - (byte)Math.round(pathScale / SCALE_QUANTA));
    }

    public static byte PackPathShear(float pathShear)
    {
        return (byte)Math.round(pathShear / SHEAR_QUANTA);
    }

    /// <summary>
    /// Packs PathTwist, PathTwistBegin, PathRadiusOffset, and PathSkew
    /// parameters in to signed eight bit values
    /// </summary>
    /// <param name="pathTwist">Floating point parameter to pack</param>
    /// <returns>Signed eight bit value containing the packed parameter</returns>
    public static byte PackPathTwist(float pathTwist)
    {
        return (byte)Math.round(pathTwist / SCALE_QUANTA);
    }

    public static byte PackPathTaper(float pathTaper)
    {
        return (byte)Math.round(pathTaper / TAPER_QUANTA);
    }

    public static byte PackPathRevolutions(float pathRevolutions)
    {
        return (byte)Math.round((pathRevolutions - 1f) / REV_QUANTA);
    }

    public static int PackProfileHollow(float profileHollow)
    {
        return (int)Math.round(profileHollow / HOLLOW_QUANTA);
    }

    //endregion Parameter Packing Methods

    //region Parameter Unpacking Methods

    public static float UnpackBeginCut(int beginCut)
    {
        return (float)beginCut * CUT_QUANTA;
    }

    public static float UnpackEndCut(int endCut)
    {
        return (float)(50000 - endCut) * CUT_QUANTA;
    }

    public static float UnpackPathScale(byte pathScale)
    {
        return (float)(200 - Utils.ubyteToInt(pathScale)) * SCALE_QUANTA;
    }

    public static float UnpackPathShear(byte pathShear)
    {
        return (float)pathShear * SHEAR_QUANTA;
    }

    /// <summary>
    /// Unpacks PathTwist, PathTwistBegin, PathRadiusOffset, and PathSkew
    /// parameters from signed eight bit integers to floating point values
    /// </summary>
    /// <param name="pathTwist">Signed eight bit value to unpack</param>
    /// <returns>Unpacked floating point value</returns>
    public static float UnpackPathTwist(byte pathTwist)
    {
        return (float)pathTwist * SCALE_QUANTA;
    }

    public static float UnpackPathTaper(byte pathTaper)
    {
        return (float)pathTaper * TAPER_QUANTA;
    }

    public static float UnpackPathRevolutions(byte pathRevolutions)
    {
        return (float)Utils.ubyteToInt(pathRevolutions) * REV_QUANTA + 1f;
    }

    public static float UnpackProfileHollow(int profileHollow)
    {
        return (float)profileHollow * HOLLOW_QUANTA;
    }

    //endregion Parameter Unpacking Methods
}
