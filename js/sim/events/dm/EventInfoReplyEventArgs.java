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
package com.ngt.jopenmetaverse.shared.sim.events.dm;

import com.ngt.jopenmetaverse.shared.sim.DirectoryManager;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

/// <summary>Contains the Event data returned from the data server from an EventInfoRequest</summary>
public class EventInfoReplyEventArgs extends EventArgs
{
	private DirectoryManager.EventInfo m_MatchedEvent;

	/// <summary>
	/// A single EventInfo object containing the details of an event
	/// </summary>
	public DirectoryManager.EventInfo getMatchedEvent() { return m_MatchedEvent; } 

	/// <summary>Construct a new instance of the EventInfoReplyEventArgs class</summary>
	/// <param name="matchedEvent">A single EventInfo object containing the details of an event</param>
	public EventInfoReplyEventArgs(DirectoryManager.EventInfo matchedEvent)
	{
		this.m_MatchedEvent = matchedEvent;
	}        
}
