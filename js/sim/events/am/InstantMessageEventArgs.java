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
package com.ngt.jopenmetaverse.shared.sim.events.am;

import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessage;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

/// <summary>The date received from an ImprovedInstantMessage</summary>
public class InstantMessageEventArgs extends EventArgs
{
	private  InstantMessage m_IM;
	private  Simulator m_Simulator;

	/// <summary>Get the InstantMessage object</summary>
	public InstantMessage getIM() { return m_IM; } 
	/// <summary>Get the simulator where the InstantMessage origniated</summary>
	public Simulator getSimulator() { return m_Simulator; } 

	/// <summary>
	/// Construct a new instance of the InstantMessageEventArgs object
	/// </summary>
	/// <param name="im">the InstantMessage object</param>
	/// <param name="simulator">the simulator where the InstantMessage origniated</param>
	public InstantMessageEventArgs(InstantMessage im, Simulator simulator)
	{
		this.m_IM = im;
		this.m_Simulator = simulator;
	}
}
