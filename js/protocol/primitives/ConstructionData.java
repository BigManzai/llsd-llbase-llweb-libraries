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

import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive;
import com.ngt.jopenmetaverse.shared.types.Vector2;
import com.ngt.jopenmetaverse.shared.util.Utils;

 /// <summary>
    /// Parameters used to finalruct a visual representation of a primitive
    /// </summary>
    public class ConstructionData
    {
        private final byte PROFILE_MASK = (byte)0x0F;
        private final byte HOLE_MASK = (byte)0xF0;

        /// <summary></summary>
        public byte profileCurve;
        /// <summary></summary>
        public EnumsPrimitive.PathCurve PathCurve;
        /// <summary></summary>
        public float PathEnd;
        /// <summary></summary>
        public float PathRadiusOffset;
        /// <summary></summary>
        public float PathSkew;
        /// <summary></summary>
        public float PathScaleX;
        /// <summary></summary>
        public float PathScaleY;
        /// <summary></summary>
        public float PathShearX;
        /// <summary></summary>
        public float PathShearY;
        /// <summary></summary>
        public float PathTaperX;
        /// <summary></summary>
        public float PathTaperY;
        /// <summary></summary>
        public float PathBegin;
        /// <summary></summary>
        public float PathTwist;
        /// <summary></summary>
        public float PathTwistBegin;
        /// <summary></summary>
        public float PathRevolutions;
        /// <summary></summary>
        public float ProfileBegin;
        /// <summary></summary>
        public float ProfileEnd;
        /// <summary></summary>
        public float ProfileHollow;

        /// <summary></summary>
        public EnumsPrimitive.Material Material;
        /// <summary></summary>
        public byte State;
        /// <summary></summary>
        public EnumsPrimitive.PCode PCode;

        //region Properties

        /// <summary>Attachment point to an avatar</summary>
        public EnumsPrimitive.AttachmentPoint getAttachmentPoint()
        {
            return EnumsPrimitive.AttachmentPoint.get(Utils.swapWords(State));
        }

        public void setAttachmentPoint(EnumsPrimitive.AttachmentPoint value)
        {
            State = (byte)Utils.swapWords((byte)value.getIndex());
        }
        
        /// <summary></summary>
        public EnumsPrimitive.ProfileCurve getProfileCurve()
        {
            return EnumsPrimitive.ProfileCurve.get((byte)(profileCurve & PROFILE_MASK));
        }
        
        public void setProfileCurve(EnumsPrimitive.ProfileCurve value)
            {
                profileCurve &= HOLE_MASK;
                profileCurve |= (byte)value.getIndex();
            }

        /// <summary></summary>
        public EnumsPrimitive.HoleType getProfileHole()
        {
            return EnumsPrimitive.HoleType.get((byte)(profileCurve & HOLE_MASK)); 
        }

        public void setProfileHole(EnumsPrimitive.HoleType value)
        {
                profileCurve &= PROFILE_MASK;
                profileCurve |= (byte)value.getIndex();
        }
        
        /// <summary></summary>
        public Vector2 getPathBeginScale()
        {
                Vector2 begin = new Vector2(1f, 1f);
                if (PathScaleX > 1f)
                    begin.X = 2f - PathScaleX;
                if (PathScaleY > 1f)
                    begin.Y = 2f - PathScaleY;
                return begin;
        }

        /// <summary></summary>
        public Vector2 getPathEndScale()
        {
                Vector2 end = new Vector2(1f, 1f);
                if (PathScaleX < 1f)
                    end.X = PathScaleX;
                if (PathScaleY < 1f)
                    end.Y = PathScaleY;
                return end;
            }

        //endregion Properties

        /// <summary>
        /// Calculdates hash code for prim finalruction data
        /// </summary>
        /// <returns>The has</returns>
        public  int hashCode()
        {
            return new Byte(profileCurve).hashCode()
                ^ PathCurve.hashCode()
                ^ new Float(PathEnd).hashCode()
                ^ new Float(PathRadiusOffset).hashCode()
                ^ new Float(PathSkew).hashCode()
                ^ new Float(PathScaleX).hashCode()
                ^ new Float(PathScaleY).hashCode()
                ^ new Float(PathShearX).hashCode()
                ^ new Float(PathShearY).hashCode()
                ^ new Float(PathTaperX).hashCode()
                ^ new Float(PathTaperY).hashCode()
                ^ new Float(PathBegin).hashCode()
                ^ new Float(PathTwist).hashCode()
                ^ new Float(PathTwistBegin).hashCode()
                ^ new Float(PathRevolutions).hashCode()
                ^ new Float(ProfileBegin).hashCode()
                ^ new Float(ProfileEnd).hashCode()
                ^ new Float(ProfileHollow).hashCode()
                ^ Material.hashCode()
                ^ new Byte(State).hashCode()
                ^ PCode.hashCode();
        }
    }
