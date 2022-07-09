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

import java.util.List;

import com.ngt.jopenmetaverse.shared.types.UUID;

public class ScriptUpdatedCallbackArg {
	boolean uploadSuccess;
	String uploadStatus;
	boolean compileSuccess;
	List<String> compileMessages;
	UUID itemID;
	UUID assetID;
	
	public ScriptUpdatedCallbackArg(boolean uploadSuccess, String uploadStatus,
			boolean compileSuccess, List<String> compileMessages, UUID itemID,
			UUID assetID) {
		super();
		this.uploadSuccess = uploadSuccess;
		this.uploadStatus = uploadStatus;
		this.compileSuccess = compileSuccess;
		this.compileMessages = compileMessages;
		this.itemID = itemID;
		this.assetID = assetID;
	}
	public boolean isUploadSuccess() {
		return uploadSuccess;
	}
	public void setUploadSuccess(boolean uploadSuccess) {
		this.uploadSuccess = uploadSuccess;
	}
	public String getUploadStatus() {
		return uploadStatus;
	}
	public void setUploadStatus(String uploadStatus) {
		this.uploadStatus = uploadStatus;
	}
	public boolean isCompileSuccess() {
		return compileSuccess;
	}
	public void setCompileSuccess(boolean compileSuccess) {
		this.compileSuccess = compileSuccess;
	}
	public List<String> getCompileMessages() {
		return compileMessages;
	}
	public void setCompileMessages(List<String> compileMessages) {
		this.compileMessages = compileMessages;
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
