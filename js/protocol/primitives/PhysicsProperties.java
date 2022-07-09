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
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive;

   /// <summary>
    /// Describes physics attributes of the prim
    /// </summary>
    public class PhysicsProperties
    {
        /// <summary>Primitive's local ID</summary>
        public long LocalID;
        /// <summary>Density (1000 for normal density)</summary>
        public float Density;
        /// <summary>Friction</summary>
        public float Friction;
        /// <summary>Gravity multiplier (1 for normal gravity) </summary>
        public float GravityMultiplier;
        /// <summary>Type of physics representation of this primitive in the simulator</summary>
        public EnumsPrimitive.PhysicsShapeType PhysicsShapeType;
        /// <summary>Restitution</summary>
        public float Restitution;

        /// <summary>
        /// Creates PhysicsProperties from OSD
        /// </summary>
        /// <param name="osd">OSDMap with incoming data</param>
        /// <returns>Deserialized PhysicsProperties object</returns>
        public static PhysicsProperties FromOSD(OSD osd)
        {
            PhysicsProperties ret = new PhysicsProperties();

            if (osd instanceof OSDMap)
            {
                OSDMap map = (OSDMap)osd;
                ret.LocalID = map.get("LocalID").asLong();
                ret.Density = (float)map.get("Density").asReal();
                ret.Friction = (float)map.get("Friction").asReal();
                ret.GravityMultiplier = (float)map.get("GravityMultiplier").asReal();
                ret.Restitution = (float)map.get("Restitution").asReal();
                ret.PhysicsShapeType = EnumsPrimitive.PhysicsShapeType.get((byte)map.get("PhysicsShapeType").asInteger());
            }

            return ret;
        }

        /// <summary>
        /// Serializes PhysicsProperties to OSD
        /// </summary>
        /// <returns>OSDMap with serialized PhysicsProperties data</returns>
        public OSD GetOSD()
        {
            OSDMap map = new OSDMap(6);
            map.put("LocalID",OSD.FromLong(LocalID));
            map.put("Density",OSD.FromReal(Density));
            map.put("Friction",OSD.FromReal(Friction));
            map.put("GravityMultiplier",OSD.FromReal(GravityMultiplier));
            map.put("Restitution",OSD.FromReal(Restitution));
            map.put("PhysicsShapeType",OSD.FromInteger((int)PhysicsShapeType.getIndex()));
            return map;
        }
    }