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
package com.ngt.jopenmetaverse.shared.sim.imaging;

import com.ngt.jopenmetaverse.shared.sim.imaging.ManagedImage.ImageChannels;

public abstract class AbstractOpenJPEGDecoder implements IOpenJPEG
{
	public DecodedTgaImage DecodeToTgaImage(byte[] imageData) throws Exception
	{
	ManagedImage mi = OpenJPEGFactory.getIntance().DecodeToImage(imageData);
      if (mi == null)
    	  return null;

      DecodedTgaImage cachedImage = new DecodedTgaImage();
      
      cachedImage.hasAlpha = false;
      cachedImage.fullAlpha = false;
      cachedImage.isMask = false;
		if ((ImageChannels.and(mi.Channels,  ManagedImage.ImageChannels.Alpha)) != 0)
      {
			cachedImage.fullAlpha = true;
			cachedImage.isMask = true;

          // Do we really have alpha, is it all full alpha, or is it a mask
          for (int i = 0; i < mi.Alpha.length; i++)
          {
              if (mi.Alpha[i] < 255)
              {
            	  cachedImage.hasAlpha = true;
              }
              if (mi.Alpha[i] != 0)
              {
            	  cachedImage.fullAlpha = false;
              }
              if (mi.Alpha[i] != 0 && mi.Alpha[i] != 255)
              {
            	  cachedImage.isMask = false;
              }
          }

          if (!cachedImage.hasAlpha)
          {
				mi.ConvertChannels(ImageChannels.get(ImageChannels.and(mi.Channels, ~ManagedImage.ImageChannels.Alpha.getIndex())));
//              mi.ConvertChannels(mi.Channels & ~ManagedImage.ImageChannels.Alpha);
          }
          
          cachedImage.data = mi.ExportTGA();
          
      }
        return cachedImage;
	}
	
}
