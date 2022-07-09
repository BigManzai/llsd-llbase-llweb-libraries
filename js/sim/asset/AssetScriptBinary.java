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
  /// <summary>
    /// Represents an AssetScriptBinary object containing the 
    /// LSO compiled bytecode of an LSL script
    /// </summary>
    public class AssetScriptBinary extends Asset
    {
        /// <summary>Override the base classes AssetType</summary>
    	@Override
        public AssetType getAssetType() { return AssetType.LSLBytecode; } 

        /// <summary>Initializes a new instance of an AssetScriptBinary object</summary>
        public AssetScriptBinary() { }

        /// <summary>Initializes a new instance of an AssetScriptBinary object with parameters</summary>
        /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
        /// <param name="assetData">A byte array containing the raw asset data</param>
        public AssetScriptBinary(UUID assetID, byte[] assetData)
        {
            super(assetID, assetData);
        }

        /// <summary>
        /// TODO: Encodes a scripts contents into a LSO Bytecode file
        /// </summary>
        @Override
        public  void Encode() { }

        /// <summary>
        /// TODO: Decode LSO Bytecode into a string
        /// </summary>
        /// <returns>true</returns>
        public boolean Decode() { return true; }
    }