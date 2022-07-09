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

import com.ngt.jopenmetaverse.shared.types.UUID;

public class InventoryUploadedAssetCallbackArg 
{
	boolean success;
	String status;
	UUID itemID;
	UUID assetID;
	
	public InventoryUploadedAssetCallbackArg(boolean success, String status,
			UUID itemID, UUID assetID) {
		super();
		this.success = success;
		this.status = status;
		this.itemID = itemID;
		this.assetID = assetID;
	}

	public boolean isSuccess() {
		return success;
	}

	public void setSuccess(boolean success) {
		this.success = success;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public UUID getItemID() {
		return itemID;
	}

	public void setItemID(UUID itemID) {
		this.itemID = itemID;
	}

	public UUID getAssetID() {
		return assetID;
	}

	public void setAssetID(UUID assetID) {
		this.assetID = assetID;
	}
}
