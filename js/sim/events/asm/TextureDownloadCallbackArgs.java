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
package com.ngt.jopenmetaverse.shared.sim.events.asm;

import com.ngt.jopenmetaverse.shared.sim.asset.AssetTexture;
import com.ngt.jopenmetaverse.shared.sim.asset.pipeline.TexturePipeline.TextureRequestState;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

public class TextureDownloadCallbackArgs extends EventArgs
{
	protected TextureRequestState state;
	protected AssetTexture assetTexture;
	
	public TextureDownloadCallbackArgs(TextureRequestState state,
			AssetTexture assetTexture) {
		super();
		this.state = state;
		this.assetTexture = assetTexture;
	}
	
	public TextureRequestState getState() {
		return state;
	}
	public void setState(TextureRequestState state) {
		this.state = state;
	}
	public AssetTexture getAssetTexture() {
		return assetTexture;
	}
	public void setAssetTexture(AssetTexture assetTexture) {
		this.assetTexture = assetTexture;
	}
}
