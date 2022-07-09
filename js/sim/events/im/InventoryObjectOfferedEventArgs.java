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

import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessage;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;

public class InventoryObjectOfferedEventArgs extends EventArgs
{
	private InstantMessage m_Offer;
	private AssetType m_AssetType;
	private UUID m_ObjectID;
	private boolean m_FromTask;
	private boolean accept;
	private UUID folderID;
	
	//TODO need to verify
//	/// <summary>Set to true to accept offer, false to decline it</summary>
//	public boolean Accept { get; set; }
//	/// <summary>The folder to accept the inventory into, if null default folder for <see cref="AssetType"/> will be used</summary>
//	public UUID FolderID { get; set; }


	public boolean isAccept() {
		return accept;
	}
	/// <summary>Set to true to accept offer, false to decline it</summary>
	public void setAccept(boolean accept) {
		this.accept = accept;
	}
	public UUID getFolderID() {
		return folderID;
	}
	/// <summary>The folder to accept the inventory into, if null default folder for <see cref="AssetType"/> will be used</summary>
	public void setFolderID(UUID folderID) {
		this.folderID = folderID;
	}
	
	public InstantMessage getOffer() {return m_Offer;}		
	public AssetType getAssetType() {return m_AssetType;}
	public UUID getObjectID() {return m_ObjectID;}
	public boolean getFromTask() {return m_FromTask;}

	public InventoryObjectOfferedEventArgs(InstantMessage offerDetails, AssetType type, UUID objectID, boolean fromTask, UUID folderID)
	{
		this.accept = false;
		this.folderID = folderID;
		this.m_Offer = offerDetails;
		this.m_AssetType = type;
		this.m_ObjectID = objectID;
		this.m_FromTask = fromTask;
	}
}
