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
package com.ngt.jopenmetaverse.shared.sim.events.im;

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Enums.InventoryType;

/// <summary>
/// Callback when an inventory object is accepted and received from a
/// task inventory. This is the callback in which you actually get
/// the ItemID, as in ObjectOfferedCallback it is null when received
/// from a task.
/// </summary>
public class TaskItemReceivedEventArgs extends EventArgs
{
	private UUID m_ItemID;
	private UUID m_FolderID;
	private UUID m_CreatorID;
	private UUID m_AssetID;
	private InventoryType m_Type;

	public UUID getItemID() {return m_ItemID;}
	public UUID getFolderID() {return m_FolderID;}
	public UUID getCreatorID() {return m_CreatorID;}
	public UUID getAssetID() {return m_AssetID;}
	public InventoryType getType() {return m_Type;}

	public TaskItemReceivedEventArgs(UUID itemID, UUID folderID, UUID creatorID, UUID assetID, InventoryType type)
	{
		this.m_ItemID = itemID;
		this.m_FolderID = folderID;
		this.m_CreatorID = creatorID;
		this.m_AssetID = assetID;
		this.m_Type = type;
	}
}
