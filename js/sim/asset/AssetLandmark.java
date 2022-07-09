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

import java.io.UnsupportedEncodingException;

import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.Utils;

   /// <summary>
    /// Represents a Landmark with RegionID and Position vector
    /// </summary>
    public class AssetLandmark extends Asset
    {
        /// <summary>Override the base classes AssetType</summary>
    	@Override
        public AssetType getAssetType() { return AssetType.Landmark; } 

        /// <summary>UUID of the Landmark target region</summary>
        public UUID RegionID = UUID.Zero;
        /// <summary> Local position of the target </summary>
        public Vector3 Position = Vector3.Zero;

        /// <summary>Construct an Asset of type Landmark</summary>
        public AssetLandmark() { }

        /// <summary>
        /// Construct an Asset object of type Landmark
        /// </summary>
        /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
        /// <param name="assetData">A byte array containing the raw asset data</param>
        public AssetLandmark(UUID assetID, byte[] assetData)
        {
            super(assetID, assetData);
        }

        /// <summary>
        /// Encode the raw contents of a string with the specific Landmark format
        /// </summary>
        @Override
        public void Encode()
        {
            String temp = "Landmark version 2\n";
            temp += "region_id " + RegionID + "\n";
            temp += String.format("local_pos %f %f %f\n", Position.X, Position.Y, Position.Z);
            AssetData = Utils.stringToBytesWithTrailingNullByte(temp);
        }

        /// <summary>
        /// Decode the raw asset data, populating the RegionID and Position
        /// </summary>
        /// <returns>true if the AssetData was successfully decoded to a UUID and Vector</returns>
        @Override
        public boolean Decode() throws UnsupportedEncodingException
        {
            String text = Utils.bytesWithTrailingNullByteToString(AssetData);
            if (text.toLowerCase().contains("landmark version 2"))
            {
                RegionID = new UUID(text.substring(text.indexOf("region_id") + 10, 36));
                String vecDelim = " ";
                String[] vecStrings = text.substring(text.indexOf("local_pos") + 10).split(vecDelim);
                if (vecStrings.length == 3)
                {
                    Position = new Vector3(Float.parseFloat(vecStrings[0]), Float.parseFloat(vecStrings[1]),Float.parseFloat(vecStrings[2]));
                    return true;
                }
            }
            return false;
        }
    }