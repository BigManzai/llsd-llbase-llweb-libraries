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

import com.ngt.jopenmetaverse.shared.types.Enums;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// 
/// </summary>
public class Transfer
{
    public UUID ID;
    public int Size;
    public byte[] AssetData = Utils.EmptyBytes;
    //int
    public int Transferred;
    public boolean Success;
    public Enums.AssetType AssetType;

    //int
    private long transferStart;

    /// <summary>Number of milliseconds passed since the last transfer
    /// packet was received</summary>
    public long getTimeSinceLastPacket()
    {
        return Utils.getUnixTime() - transferStart; 
    }
    public void setTimeSinceLastPacket(long value)
    {
        transferStart = Utils.getUnixTime() + value; 
    }

    public Transfer()
    {
        AssetData = Utils.EmptyBytes;
        transferStart = Utils.getUnixTime();
    }
}