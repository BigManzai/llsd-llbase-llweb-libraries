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
package com.ngt.jopenmetaverse.shared.sim.inventory;

import java.util.Date;
import java.util.Map;

import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions;
import com.ngt.jopenmetaverse.shared.types.Enums;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;

/// <summary>
	/// An Item in Inventory
	/// </summary>
	public class InventoryItem extends InventoryBase
	{
		/// <summary>The <seealso cref="OpenMetaverse.UUID"/> of this item</summary>
		public UUID AssetUUID;
		/// <summary>The combined <seealso cref="OpenMetaverse.Permissions"/> of this item</summary>
		public Permissions Permissions;
		/// <summary>The type of item from <seealso cref="OpenMetaverse.AssetType"/></summary>
		public Enums.AssetType AssetType;
		/// <summary>The type of item from the <seealso cref="OpenMetaverse.InventoryType"/> enum</summary>
		public Enums.InventoryType InventoryType;
		/// <summary>The <seealso cref="OpenMetaverse.UUID"/> of the creator of this item</summary>
		public UUID CreatorID;
		/// <summary>A Description of this item</summary>
		public String Description;
		/// <summary>The <seealso cref="OpenMetaverse.Group"/>s <seealso cref="OpenMetaverse.UUID"/> this item is set to or owned by</summary>
		public UUID GroupID;
		/// <summary>If true, item is owned by a group</summary>
		public boolean GroupOwned;
		/// <summary>The price this item can be purchased for</summary>
		public int SalePrice;
		/// <summary>The type of sale from the <seealso cref="OpenMetaverse.SaleType"/> enum</summary>
		public Enums.SaleType SaleType;
		/// <summary>Combined flags from <seealso cref="OpenMetaverse.InventoryItemFlags"/></summary>
		//uint
		public long Flags;
		/// <summary>Time and date this inventory item was created, stored as
		/// UTC (Coordinated Universal Time)</summary>
		public Date CreationDate;
		/// <summary>Used to update the AssetID in requests sent to the server</summary>
		public UUID TransactionID;
		/// <summary>The <seealso cref="OpenMetaverse.UUID"/> of the previous owner of the item</summary>
		public UUID LastOwnerID;

		/// <summary>
		///  Construct a new InventoryItem object
		/// </summary>
		/// <param name="itemID">The <seealso cref="OpenMetaverse.UUID"/> of the item</param>
		public InventoryItem(UUID itemID)
		{ super(itemID);}

		/// <summary>
		/// Construct a new InventoryItem object of a specific Type
		/// </summary>
		/// <param name="type">The type of item from <seealso cref="OpenMetaverse.InventoryType"/></param>
		/// <param name="itemID"><seealso cref="OpenMetaverse.UUID"/> of the item</param>
		public InventoryItem(Enums.InventoryType type, UUID itemID) 
		{ 
			super(itemID);
			InventoryType = type; 
		}

		/// <summary>
		/// Indicates inventory item is a link
		/// </summary>
		/// <returns>True if inventory item is a link to another inventory item</returns>
		public boolean IsLink()
		{
			return AssetType == AssetType.Link || AssetType == AssetType.LinkFolder;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		@Override
		public Map<String, Object> getObjectData()
		{
			Map<String, Object> info = super.getObjectData();
			info.put("AssetUUID", AssetUUID);
			info.put("Permissions", Permissions);
			info.put("AssetType", AssetType);
			info.put("InventoryType", InventoryType);
			info.put("CreatorID", CreatorID);
			info.put("Description", Description);
			info.put("GroupID", GroupID);
			info.put("GroupOwned", GroupOwned);
			info.put("SalePrice", SalePrice);
			info.put("SaleType", SaleType);
			info.put("Flags", Flags);
			info.put("CreationDate", CreationDate);
			info.put("LastOwnerID", LastOwnerID);
			return info;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		public InventoryItem(Map<String, Object> info)
		{
			super(info);
			AssetUUID = (UUID)info.get("AssetUUID");
			Permissions = (Permissions)info.get("Permissions");
			AssetType = (AssetType)info.get("AssetType");
			InventoryType = (Enums.InventoryType)info.get("InventoryType");
			CreatorID = (UUID)info.get("CreatorID");
			Description = (String)info.get("Description");
			GroupID = (UUID)info.get("GroupID");
			GroupOwned = (Boolean)info.get("GroupOwned");
			SalePrice = (Integer)info.get("SalePrice");
			SaleType = SaleType.get((Byte)info.get("SaleType"));
			Flags = (Long)info.get("Flags");
			CreationDate = (Date)info.get("CreationDate");
			LastOwnerID = (UUID)info.get("LastOwnerID");
		}

		/// <summary>
		/// Generates a number corresponding to the value of the object to support the use of a hash table.
		/// Suitable for use in hashing algorithms and data structures such as a hash table
		/// </summary>
		/// <returns>A Hashcode of all the combined InventoryItem fields</returns>
		@Override
		public int hashCode()
		{
			return AssetUUID.hashCode() ^ Permissions.hashCode() ^ AssetType.hashCode() ^
					InventoryType.hashCode() ^ Description.hashCode() ^ GroupID.hashCode() ^
					new Boolean(GroupOwned).hashCode() ^ new Integer(SalePrice).hashCode() ^ SaleType.hashCode() ^
					new Long(Flags).hashCode() ^ CreationDate.hashCode() ^ LastOwnerID.hashCode();
		}

		/// <summary>
		/// Compares an object
		/// </summary>
		/// <param name="o">The object to compare</param>
		/// <returns>true if comparison object matches</returns>
		@Override
		public boolean equals(Object o)
		{
			InventoryItem item = (InventoryItem)o;
			return item != null && equals(item);
		}

		/// <summary>
		/// Determine whether the specified <seealso cref="OpenMetaverse.InventoryBase"/> object is equal to the current object
		/// </summary>
		/// <param name="o">The <seealso cref="OpenMetaverse.InventoryBase"/> object to compare against</param>
		/// <returns>true if objects are the same</returns>
		@Override
		public boolean equals(InventoryBase o)
		{
			InventoryItem item = (InventoryItem)o;
			return item != null && equals(item);
		}

		/// <summary>
		/// Determine whether the specified <seealso cref="OpenMetaverse.InventoryItem"/> object is equal to the current object
		/// </summary>
		/// <param name="o">The <seealso cref="OpenMetaverse.InventoryItem"/> object to compare against</param>
		/// <returns>true if objects are the same</returns>
		public boolean equals(InventoryItem o)
		{
			return super.equals(o)
					&& o.AssetType == AssetType
					&& o.AssetUUID.equals(AssetUUID)
					&& o.CreationDate.equals(CreationDate)
					&& o.Description.equals(Description)
					&& o.Flags == Flags
					&& o.GroupID.equals(GroupID)
					&& o.GroupOwned == GroupOwned
					&& o.InventoryType == InventoryType
					&& o.Permissions.equals(Permissions)
					&& o.SalePrice == SalePrice
					&& o.SaleType == SaleType
					&& o.LastOwnerID.equals(LastOwnerID);
		}
	}
