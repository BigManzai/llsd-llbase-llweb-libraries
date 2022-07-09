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
package com.ngt.jopenmetaverse.shared.sim.asset;

import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;

public abstract class Asset {
	  /// <summary>A byte array containing the raw asset data</summary>
    public byte[] AssetData;
    /// <summary>True if the asset it only stored on the server temporarily</summary>
    public boolean Temporary;
    /// <summary>A unique ID</summary>
    private UUID _AssetID;
    /// <summary>The assets unique ID</summary>
    public UUID getAssetID()
    {
        return _AssetID;
    }

    public void setAssetID(UUID value)
    {
        _AssetID = value;
    }
    
    /// <summary>
    /// The "type" of asset, Notecard, Animation, etc
    /// </summary>
    public abstract AssetType getAssetType();

    /// <summary>
    /// Construct a new Asset object
    /// </summary>
    public Asset() { }

    /// <summary>
    /// Construct a new Asset object
    /// </summary>
    /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
    /// <param name="assetData">A byte array containing the raw asset data</param>
    public Asset(UUID assetID, byte[] assetData)
    {
        _AssetID = assetID;
        AssetData = assetData;
    }

    /// <summary>
    /// Regenerates the <code>AssetData</code> byte array from the properties 
    /// of the derived class.
    /// </summary>
    public abstract void Encode() throws Exception;

    /// <summary>
    /// Decodes the AssetData, placing it in appropriate properties of the derived
    /// class.
    /// </summary>
    /// <returns>True if the asset decoding succeeded, otherwise false</returns>
    public abstract boolean Decode() throws Exception;
}
