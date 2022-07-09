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

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

/// <summary>Data sent by the simulator containing urgent messages</summary>
public class AlertMessageEventArgs extends EventArgs
{
	private  String m_Message;

	/// <summary>Get the alert message</summary>
	public String getMessage() {return m_Message;}

	/// <summary>
	/// Construct a new instance of the AlertMessageEventArgs class
	/// </summary>
	/// <param name="message">The alert message</param>
	public AlertMessageEventArgs(String message)
	{
		this.m_Message = message;
	}
}
