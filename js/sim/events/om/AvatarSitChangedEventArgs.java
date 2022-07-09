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
package com.ngt.jopenmetaverse.shared.sim.events.om;

import com.ngt.jopenmetaverse.shared.sim.Avatar;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

/// <summary>
/// Provides updates sit position data
/// </summary>
public class AvatarSitChangedEventArgs extends EventArgs
{
    private  Simulator m_Simulator;
    private  Avatar m_Avatar;
    //uint
    private  long m_SittingOn;
    //uint
    private  long m_OldSeat;

    /// <summary>Get the simulator the object is located</summary>
    public Simulator getSimulator() {return m_Simulator;}
    /// <summary></summary>
    public Avatar getAvatar() {return m_Avatar;}
    /// <summary></summary>
    public long getSittingOn() {return m_SittingOn;}
    /// <summary></summary>
    public long getOldSeat() {return m_OldSeat;}

    public AvatarSitChangedEventArgs(Simulator simulator, Avatar avatar, long sittingOn, long oldSeat)
    {
        this.m_Simulator = simulator;
        this.m_Avatar = avatar;
        this.m_SittingOn = sittingOn;
        this.m_OldSeat = oldSeat;
    }
}
