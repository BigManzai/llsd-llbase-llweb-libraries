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
package com.ngt.jopenmetaverse.shared.sim.events.nm;

import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

public class PacketSentEventArgs extends EventArgs  
{
	private  byte[] m_Data;
    private  int m_SentBytes;
    private Simulator m_Simulator;

    public byte[] getData() { return m_Data; } 
    public int getSentBytes() { return m_SentBytes; } 
    public Simulator getSimulator() { return m_Simulator; } 

    public PacketSentEventArgs(byte[] data, int bytesSent, Simulator simulator)
    {
        this.m_Data = data;
        this.m_SentBytes = bytesSent;
        this.m_Simulator = simulator;
    }
}
