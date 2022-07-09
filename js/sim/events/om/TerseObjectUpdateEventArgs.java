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

import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.sim.ObjectManager.ObjectMovementUpdate;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;


/// <summary>Provides primitive data containing updated location, velocity, rotation, textures for the <see cref="ObjectManager.TerseObjectUpdate"/> event</summary>
/// <remarks><para>The <see cref="ObjectManager.TerseObjectUpdate"/> event occurs when the simulator sends updated location, velocity, rotation, etc</para>        
/// </remarks>
public class TerseObjectUpdateEventArgs extends EventArgs
{
    private  Simulator m_Simulator;
    private  Primitive m_Prim;
    private  ObjectMovementUpdate m_Update;
    //ushort
    private  int m_TimeDilation;

    /// <summary>Get the simulator the object is located</summary>
    public Simulator getSimulator() {return m_Simulator;}
    /// <summary>Get the primitive details</summary>
    public Primitive getPrim() {return m_Prim;}
    /// <summary></summary>
    public ObjectMovementUpdate getUpdate() {return m_Update;}
    /// <summary></summary>
    //ushort
    public int getTimeDilation() {return m_TimeDilation;}

    public TerseObjectUpdateEventArgs(Simulator simulator, Primitive prim, ObjectMovementUpdate update, int timeDilation)
    {
        this.m_Simulator = simulator;
        this.m_Prim = prim;
        this.m_Update = update;
        this.m_TimeDilation = timeDilation;
    }
}
