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

import com.ngt.jopenmetaverse.shared.sim.imaging.ManagedImage;
import com.ngt.jopenmetaverse.shared.sim.imaging.OpenJPEGFactory;
import com.ngt.jopenmetaverse.shared.sim.imaging.ManagedImage.ImageChannels;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
 /// <summary>
    /// Represents a texture
    /// </summary>
    public class AssetTexture extends Asset
    {    	
		/// <summary>Override the base classes AssetType</summary>
    	@Override
        public AssetType getAssetType() { return AssetType.Texture; } 

        /// <summary>A <seealso cref="ManagedImage"/> object containing image data</summary>
        public ManagedImage Image;

        //TODO do we require following
//        /// <summary></summary>
//        public OpenJPEG.J2KLayerInfo[] LayerInfo;

        /// <summary></summary>
        public int Components;

        /// <summary>Initializes a new instance of an AssetTexture object</summary>
        public AssetTexture() { }

        /// <summary>
        /// Initializes a new instance of an AssetTexture object
        /// </summary>
        /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
        /// <param name="assetData">A byte array containing the raw asset data</param>
        public AssetTexture(UUID assetID, byte[] assetData) 
        {super(assetID, assetData);  }

        /// <summary>
        /// Initializes a new instance of an AssetTexture object
        /// </summary>
        /// <param name="image">A <seealso cref="ManagedImage"/> object containing texture data</param>
        public AssetTexture(ManagedImage image)
        {
            Image = image;
            Components = 0;
            if ((ImageChannels.and(Image.Channels,  ManagedImage.ImageChannels.Color) != 0))
                Components += 3;
            if ((ImageChannels.and(Image.Channels , ManagedImage.ImageChannels.Gray) != 0))
                ++Components;
            if ((ImageChannels.and(Image.Channels , ManagedImage.ImageChannels.Bump) != 0))
                ++Components;
            if ((ImageChannels.and(Image.Channels , ManagedImage.ImageChannels.Alpha) != 0))
                ++Components;
        }

        /// <summary>
        /// Populates the <seealso cref="AssetData"/> byte array with a JPEG2000
        /// encoded image created from the data in <seealso cref="Image"/>
        /// </summary>AssetTexture
        @Override
        public void Encode() throws Exception
        {
            AssetData = OpenJPEGFactory.getIntance().Encode(Image);
        }

        /// <summary>
        /// Decodes the JPEG2000 data in <code>AssetData</code> to the
        /// <seealso cref="ManagedImage"/> object <seealso cref="Image"/>
        /// </summary>
        /// <returns>True if the decoding was successful, otherwise false</returns>
        @Override
        public boolean Decode() throws Exception
        {
            if (AssetData != null && AssetData.length > 0)
            {
                this.Components = 0;

                if ((Image = OpenJPEGFactory.getIntance().DecodeToImage(AssetData))!=null)
                {
                    if ((ImageChannels.and(Image.Channels,  ManagedImage.ImageChannels.Color) != 0))
                        Components += 3;
                    if ((ImageChannels.and(Image.Channels,  ManagedImage.ImageChannels.Gray) != 0))
                        ++Components;
                    if ((ImageChannels.and(Image.Channels,  ManagedImage.ImageChannels.Bump) != 0))
                        ++Components;
                    if ((ImageChannels.and(Image.Channels,  ManagedImage.ImageChannels.Alpha) != 0))
                        ++Components;

                    return true;
                }
            }
            return false;
        }

        //TODO do we need following
//        /// <summary>
//        /// Decodes the begin and end byte positions for each quality layer in
//        /// the image
//        /// </summary>
//        /// <returns></returns>
//        public boolean DecodeLayerBoundaries()
//        {
//            return OpenJPEG.DecodeLayerBoundaries(AssetData, out LayerInfo, out Components);
//        }
    }