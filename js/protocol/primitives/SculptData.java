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

import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.SculptType;
import com.ngt.jopenmetaverse.shared.types.UUID;



  /// <summary>
    /// Information on the sculpt properties of a sculpted primitive
    /// </summary>
    public class SculptData
    {
        public UUID SculptTexture;
        private byte type;

        public SculptType getType()
        {
            return SculptType.get((byte) (type & 7));
        }

        public void setType(SculptType value)
        {
            type = (byte)value.getIndex();
        }
        
        /// <summary>
        /// Render inside out (inverts the normals).
        /// </summary>
        public boolean getInvert()
        {
            return ((type & (byte)SculptType.Invert.getIndex()) != 0); 
        }

        /// <summary>
        /// Render an X axis mirror of the sculpty.
        /// </summary>
        public boolean getMirror()
        {
            return ((type & (byte)SculptType.Mirror.getIndex()) != 0);
        }            

        /// <summary>
        /// Default finalructor
        /// </summary>
        public SculptData()
        {
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="data"></param>
        /// <param name="pos"></param>
        public SculptData(byte[] data, int pos)
        {
            if (data.length >= 17)
            {
                SculptTexture = new UUID(data, pos);
                type = data[pos + 16];
            }
            else
            {
                SculptTexture = UUID.Zero;
                type = (byte)SculptType.None.getIndex();
            }
        }

        public byte[] GetBytes()
        {
            byte[] data = new byte[17];
            byte[] src = SculptTexture.GetBytes();
            System.arraycopy(src, 0, data, 0, src.length);
            data[16] = type;

            return data;
        }

        public OSD GetOSD()
        {
            OSDMap map = new OSDMap();

            map.put("texture", OSD.FromUUID(SculptTexture));
            map.put("type", OSD.FromInteger(type));

            return map;
        }

        public static SculptData FromOSD(OSD osd)
        {
            SculptData sculpt = new SculptData();

            if (osd.getType() == OSDType.Map)
            {
                OSDMap map = (OSDMap)osd;

                sculpt.SculptTexture = map.get("texture").asUUID();
                sculpt.type = (byte)map.get("type").asInteger();
            }

            return sculpt;
        }

        public  int hashCode()
        {
            return SculptTexture.hashCode() ^ new Byte(type).hashCode();
        }
    }