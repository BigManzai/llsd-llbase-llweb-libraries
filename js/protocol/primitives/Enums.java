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

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;



public class Enums
{
    //region Enumerations

    /// <summary>
    /// The type of bump-mapping applied to a face
    /// </summary>
    public enum Bumpiness 
    {
        /// <summary></summary>
        None ((byte)0),
        /// <summary></summary>
        Brightness  ((byte)1),
        /// <summary></summary>
        Darkness  ((byte)2),
        /// <summary></summary>
        Woodgrain  ((byte)3),
        /// <summary></summary>
        Bark  ((byte)4),
        /// <summary></summary>
        Bricks  ((byte)5),
        /// <summary></summary>
        Checker  ((byte)6),
        /// <summary></summary>
        Concrete  ((byte)7),
        /// <summary></summary>
        Crustytile  ((byte)8),
        /// <summary></summary>
        Cutstone  ((byte)9),
        /// <summary></summary>
        Discs  ((byte)10),
        /// <summary></summary>
        Gravel  ((byte)11),
        /// <summary></summary>
        Petridish  ((byte)12),
        /// <summary></summary>
        Siding  ((byte)13),
        /// <summary></summary>
        Stonetile  ((byte)14),
        /// <summary></summary>
        Stucco  ((byte)15),
        /// <summary></summary>
        Suction  ((byte)16),
        /// <summary></summary>
        Weave  ((byte)17);
        
 		private byte index;
 		Bumpiness(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,Bumpiness> lookup  = new HashMap<Byte,Bumpiness>();

		static {
			for(Bumpiness s : EnumSet.allOf(Bumpiness.class))
				lookup.put(s.getIndex(), s);
		}

		public static Bumpiness get(Byte index)
		{
			return lookup.get(index);
		}
        
    }

    /// <summary>
    /// The level of shininess applied to a face
    /// </summary>
    public enum Shininess 
    {
        /// <summary></summary>
        None ((byte)0),
        /// <summary></summary>
        Low ((byte)0x40),
        /// <summary></summary>
        Medium ((byte)0x80),
        /// <summary></summary>
        High ((byte)0xC0);
        
        private byte index;
        Shininess(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,Shininess> lookup  = new HashMap<Byte,Shininess>();

		static {
			for(Shininess s : EnumSet.allOf(Shininess.class))
				lookup.put(s.getIndex(), s);
		}

		public static Shininess get(Byte index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// The texture mapping style used for a face
    /// </summary>
    public enum MappingType 
    {
        /// <summary></summary>
        Default ((byte)0),
        /// <summary></summary>
        Planar ((byte)2),
        /// <summary></summary>
        Spherical ((byte)4),
        /// <summary></summary>
        Cylindrical ((byte)6);
        private byte index;
        MappingType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,MappingType> lookup  = new HashMap<Byte,MappingType>();

		static {
			for(MappingType s : EnumSet.allOf(MappingType.class))
				lookup.put(s.getIndex(), s);
		}

		public static MappingType get(Byte index)
		{
			return lookup.get(index);
		}
    }

    /// <summary>
    /// Flags in the TextureEntry block that describe which properties are 
    /// set
    /// </summary>
    //[Flags]
    public enum TextureAttributes
    {
        /// <summary></summary>
        None (0L),
        /// <summary></summary>
        TextureID (1L << 0),
        /// <summary></summary>
        RGBA (1L << 1),
        /// <summary></summary>
        RepeatU (1L << 2),
        /// <summary></summary>
        RepeatV (1L << 3),
        /// <summary></summary>
        OffsetU (1L << 4),
        /// <summary></summary>
        OffsetV (1L << 5),
        /// <summary></summary>
        Rotation (1L << 6),
        /// <summary></summary>
        Material (1L << 7),
        /// <summary></summary>
        Media (1L << 8),
        /// <summary></summary>
        Glow (1L << 9),
        /// <summary></summary>
        All (0xFFFFFFFFL);
        
        private long index;
        TextureAttributes(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}
		
		private static final Map<Long,TextureAttributes> lookup  = new HashMap<Long,TextureAttributes>();

		static {
			for(TextureAttributes s : EnumSet.allOf(TextureAttributes.class))
				lookup.put(s.getIndex(), s);
		}

        public static EnumSet<TextureAttributes> get(Long index)
        {
                EnumSet<TextureAttributes> enumsSet = EnumSet.allOf(TextureAttributes.class);
                for(Entry<Long,TextureAttributes> entry: lookup.entrySet())
                {
                        if((entry.getKey().longValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }
        
        public static long getIndex(EnumSet<TextureAttributes> enumSet)
        {
                long ret = 0;
                for(TextureAttributes s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }
  
    }
    
    /// <summary>
    /// Texture animation mode
    /// </summary>
    //[Flags]
    public enum TextureAnimMode 
    {
        /// <summary>Disable texture animation</summary>
        ANIM_OFF (0x00L),
        /// <summary>Enable texture animation</summary>
        ANIM_ON (0x01L),
        /// <summary>Loop when animating textures</summary>
        LOOP (0x02L),
        /// <summary>Animate in reverse direction</summary>
        REVERSE (0x04L),
        /// <summary>Animate forward then reverse</summary>
        PING_PONG (0x08L),
        /// <summary>Slide texture smoothly instead of frame-stepping</summary>
        SMOOTH (0x10L),
        /// <summary>Rotate texture instead of using frames</summary>
        ROTATE (0x20L),
        /// <summary>Scale texture instead of using frames</summary>
        SCALE (0x40L);
        
        private long index;
        TextureAnimMode(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}
		
		private static final Map<Long,TextureAnimMode> lookup  = new HashMap<Long,TextureAnimMode>();

		static {
			for(TextureAnimMode s : EnumSet.allOf(TextureAnimMode.class))
				lookup.put(s.getIndex(), s);
		}

        public static EnumSet<TextureAnimMode> get(Long index)
        {
                EnumSet<TextureAnimMode> enumsSet = EnumSet.allOf(TextureAnimMode.class);
                for(Entry<Long,TextureAnimMode> entry: lookup.entrySet())
                {
                        if((entry.getKey().longValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }
        
        public static long getIndex(EnumSet<TextureAnimMode> enumSet)
        {
                long ret = 0;
                for(TextureAnimMode s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }
 
        
        public static long and(EnumSet<TextureAnimMode> enumSet, long v)
        {
        	return getIndex(enumSet) & v;
        }

        public static long and(EnumSet<TextureAnimMode> enumSet, TextureAnimMode v)
        {
        	return getIndex(enumSet) & v.getIndex();
        }
        
        public static long and(EnumSet<TextureAnimMode> enumSet1, EnumSet<TextureAnimMode> enumSet2)
        {
        	return getIndex(enumSet1) & getIndex(enumSet2);
        }
    }
    
}