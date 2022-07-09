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
import java.util.Map;

import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>
	/// InventorySound Class representing a playable sound
	/// </summary>
	public class InventorySound extends InventoryItem
	{
		/// <summary>
		/// Construct an InventorySound object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventorySound(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Sound;
		}

		/// <summary>
		/// Construct an InventorySound object from a serialization stream
		/// </summary>
		public InventorySound(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Sound;
		}
	}

