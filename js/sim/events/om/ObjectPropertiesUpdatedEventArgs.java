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

import com.ngt.jopenmetaverse.shared.protocol.primitives.ObjectProperties;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;


/// <summary>Provides additional primitive data for the <see cref="ObjectManager.ObjectPropertiesUpdated"/> event</summary>
/// <remarks><para>The <see cref="ObjectManager.ObjectPropertiesUpdated"/> event occurs when the simulator sends
/// an <see cref="ObjectPropertiesPacket"/> containing additional details for a Primitive or Foliage data that is currently
/// being tracked in the <see cref="Simulator.ObjectsPrimitives"/> dictionary</para>
/// <para>The <see cref="ObjectManager.ObjectPropertiesUpdated"/> event is also raised when a <see cref="ObjectManager.SelectObject"/> request is
/// made and <see cref="Settings.OBJECT_TRACKING"/> is enabled</para>    
/// </remarks>    
public class ObjectPropertiesUpdatedEventArgs extends EventArgs
{

    private  Simulator m_Simulator;
    private  Primitive m_Prim;
    private  ObjectProperties m_Properties;

    /// <summary>Get the simulator the object is located</summary>
    public Simulator getSimulator() {return m_Simulator;}
    /// <summary>Get the primitive details</summary>
    public Primitive getPrim() {return m_Prim;}
    /// <summary>Get the primitive properties</summary>
    public ObjectProperties getProperties() {return m_Properties;}

    /// <summary>
    /// Construct a new instance of the ObjectPropertiesUpdatedEvenrArgs class
    /// </summary>                
    /// <param name="simulator">The simulator the object is located</param>
    /// <param name="prim">The Primitive</param>
    /// <param name="props">The primitive Properties</param>
    public ObjectPropertiesUpdatedEventArgs(Simulator simulator, Primitive prim, ObjectProperties props)
    {
        this.m_Simulator = simulator;
        this.m_Prim = prim;
        this.m_Properties = props;
    }
}
