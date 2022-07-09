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

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

// <summary>Provides data for ImageReceiveProgress event</summary>
public class ImageReceiveProgressEventArgs extends EventArgs
{
    private UUID m_ImageID;
    private int m_Received;
    private int m_Total;

    /// <summary>UUID of the image that is in progress</summary>
    public UUID getImageID() {return m_ImageID;} 

    /// <summary>Number of bytes received so far</summary>
    public int getReceived() {return m_Received;} 

    /// <summary>Image size in bytes</summary>
    public int getTotal() {return m_Total;} 

    public ImageReceiveProgressEventArgs(UUID imageID, int received, int total)
    {
        this.m_ImageID = imageID;
        this.m_Received = received;
        this.m_Total = total;
    }
}