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

import com.ngt.jopenmetaverse.shared.protocol.primitives.PhysicsProperties;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

/// <summary>
/// Set when simulator sends us infomation on primitive's physical properties
/// </summary>
public class PhysicsPropertiesEventArgs extends EventArgs
{
    /// <summary>Simulator where the message originated</summary>
    public Simulator Simulator;
    /// <summary>Updated physical properties</summary>
    public PhysicsProperties PhysicsProperties;

    /// <summary>
    /// Constructor
    /// </summary>
    /// <param name="sim">Simulator where the message originated</param>
    /// <param name="props">Updated physical properties</param>
    public PhysicsPropertiesEventArgs(Simulator sim, PhysicsProperties props)
    {
        Simulator = sim;
        PhysicsProperties = props;
    }
}
