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

import java.util.EnumSet;
import java.util.Map;

import com.ngt.jopenmetaverse.shared.sim.InventoryManager.InventoryItemFlags;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.AttachmentPoint;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>
/// InventoryObject Class contains details on a primitive or coalesced set of primitives
/// </summary>
public class InventoryObject extends InventoryItem
{
	/// <summary>
	/// Construct an InventoryObject object
	/// </summary>
	/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
	/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
	public InventoryObject(UUID itemID)
	{
		super(itemID);
		InventoryType = InventoryType.Object;
	}

	/// <summary>
	/// Construct an InventoryObject object from a serialization stream
	/// </summary>
	public InventoryObject(Map<String, Object> info)

	{
		super(info);
		InventoryType = InventoryType.Object;
	}

	/// <summary>
	/// Gets or sets the upper byte of the Flags value
	/// </summary>
	public EnumSet<InventoryItemFlags> getItemFlags()
	{return InventoryItemFlags.get(Flags & ~0xFF);}
	
	public void setItemFlags(EnumSet<InventoryItemFlags> value)
	{Flags = InventoryItemFlags.getIndex(value) | (Flags & 0xFF);}


	/// <summary>
	/// Gets or sets the object attachment point, the lower byte of the Flags value
	/// </summary>
	public AttachmentPoint getAttachPoint()
	{return AttachmentPoint.get((byte)(Flags & 0xFF));}
	
	public void setAttachPoint(AttachmentPoint value)
	{Flags = (long)value.getIndex() | (Flags & 0xFFFFFF00);}

}
