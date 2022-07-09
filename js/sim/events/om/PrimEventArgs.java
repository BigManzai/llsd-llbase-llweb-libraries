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
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;


/// <summary>Provides data for the <see cref="ObjectManager.ObjectUpdate"/> event</summary>
/// <remarks><para>The <see cref="ObjectManager.ObjectUpdate"/> event occurs when the simulator sends
/// an <see cref="ObjectUpdatePacket"/> containing a Primitive, Foliage or Attachment data</para>
/// <para>Note 1: The <see cref="ObjectManager.ObjectUpdate"/> event will not be raised when the object is an Avatar</para>
/// <para>Note 2: It is possible for the <see cref="ObjectManager.ObjectUpdate"/> to be 
/// raised twice for the same object if for example the primitive moved to a new simulator, then returned to the current simulator or
/// if an Avatar crosses the border into a new simulator and returns to the current simulator</para>
/// </remarks>
/// <example>
/// The following code example uses the <see cref="PrimEventArgs.Prim"/>, <see cref="PrimEventArgs.Simulator"/>, and <see cref="PrimEventArgs.IsAttachment"/>
/// properties to display new Primitives and Attachments on the <see cref="Console"/> window.
/// <code>
///     // Subscribe to the event that gives us prim and foliage information
///     Client.Objects.ObjectUpdate += Objects_ObjectUpdate;
///     
///
///     private void Objects_ObjectUpdate(Object sender, PrimEventArgs e)
///     {
///         Console.WriteLine("Primitive {0} {1} in {2} is an attachment {3}", e.Prim.ID, e.Prim.LocalID, e.Simulator.Name, e.IsAttachment);
///     }
/// </code>
/// </example>
/// <seealso cref="ObjectManager.ObjectUpdate"/>
/// <seealso cref="ObjectManager.AvatarUpdate"/>
/// <seealso cref="AvatarUpdateEventArgs"/>
public class PrimEventArgs extends EventArgs
{
    private  Simulator m_Simulator;
    private  boolean m_IsNew;
    private  boolean m_IsAttachment;
    private  Primitive m_Prim;
    //ushort
    private  int m_TimeDilation;

    /// <summary>Get the simulator the <see cref="Primitive"/> originated from</summary>
    public Simulator getSimulator() {return m_Simulator;}
    /// <summary>Get the <see cref="Primitive"/> details</summary>
    public Primitive getPrim() {return m_Prim;}
    /// <summary>true if the <see cref="Primitive"/> did not exist in the dictionary before this update (always true if object tracking has been disabled)</summary>
    public boolean getIsNew() {return m_IsNew;}
    /// <summary>true if the <see cref="Primitive"/> is attached to an <see cref="Avatar"/></summary>
    public boolean getIsAttachment() {return m_IsAttachment;}
    /// <summary>Get the simulator Time Dilation</summary>
    public int getTimeDilation() {return m_TimeDilation;}

    /// <summary>
    /// Construct a new instance of the PrimEventArgs class
    /// </summary>
    /// <param name="simulator">The simulator the object originated from</param>
    /// <param name="prim">The Primitive</param>
    /// <param name="timeDilation">The simulator time dilation</param>
    /// <param name="isNew">The prim was not in the dictionary before this update</param>
    /// <param name="isAttachment">true if the primitive represents an attachment to an agent</param>
    public PrimEventArgs(Simulator simulator, Primitive prim, int timeDilation, boolean isNew, boolean isAttachment)
    {
        this.m_Simulator = simulator;
        this.m_IsNew = isNew;
        this.m_IsAttachment = isAttachment;
        this.m_Prim = prim;
        this.m_TimeDilation = timeDilation;
    }
}
