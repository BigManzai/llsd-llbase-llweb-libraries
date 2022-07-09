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

import java.util.Date;

import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.types.Enums;

 /// <summary>
    /// Extended properties to describe an object
    /// </summary>
    public class ObjectProperties
    {
        /// <summary></summary>
        public UUID ObjectID;
        /// <summary></summary>
        public UUID CreatorID;
        /// <summary></summary>
        public UUID OwnerID;
        /// <summary></summary>
        public UUID GroupID;
        /// <summary></summary>
        public Date CreationDate;
        /// <summary></summary>
        public Permissions Permissions;
        /// <summary></summary>
        public int OwnershipCost;
        /// <summary></summary>
        public Enums.SaleType SaleType;
        /// <summary></summary>
        public int SalePrice;
        /// <summary></summary>
        public byte AggregatePerms;
        /// <summary></summary>
        public byte AggregatePermTextures;
        /// <summary></summary>
        public byte AggregatePermTexturesOwner;
        /// <summary></summary>
        public EnumsPrimitive.ObjectCategory Category;
        /// <summary></summary>
        public short InventorySerial;
        /// <summary></summary>
        public UUID ItemID;
        /// <summary></summary>
        public UUID FolderID;
        /// <summary></summary>
        public UUID FromTaskID;
        /// <summary></summary>
        public UUID LastOwnerID;
        /// <summary></summary>
        public String Name;
        /// <summary></summary>
        public String Description;
        /// <summary></summary>
        public String TouchName;
        /// <summary></summary>
        public String SitName;
        /// <summary></summary>
        public UUID[] TextureIDs;

        /// <summary>
        /// Default finalructor
        /// </summary>
        public ObjectProperties()
        {
            Name = "";
            Description = "";
            TouchName = "";
            SitName = "";
        }

        /// <summary>
        /// Set the properties that are set in an ObjectPropertiesFamily packet
        /// </summary>
        /// <param name="props"><seealso cref="ObjectProperties"/> that has
        /// been partially filled by an ObjectPropertiesFamily packet</param>
        public void SetFamilyProperties(ObjectProperties props)
        {
            ObjectID = props.ObjectID;
            OwnerID = props.OwnerID;
            GroupID = props.GroupID;
            Permissions = props.Permissions;
            OwnershipCost = props.OwnershipCost;
            SaleType = props.SaleType;
            SalePrice = props.SalePrice;
            Category = props.Category;
            LastOwnerID = props.LastOwnerID;
            Name = props.Name;
            Description = props.Description;
        }

        public byte[] GetTextureIDBytes()
        {
            if (TextureIDs == null || TextureIDs.length == 0)
                return Utils.EmptyBytes;

            byte[] bytes = new byte[16 * TextureIDs.length];
            for (int i = 0; i < TextureIDs.length; i++)
                TextureIDs[i].ToBytes(bytes, 16 * i);

            return bytes;
        }
    }