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

import java.util.List;

import com.ngt.jopenmetaverse.shared.sim.DirectoryManager;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Contains the "Event" detail data returned from the data server</summary>
public class DirEventsReplyEventArgs extends EventArgs
{
	private UUID m_QueryID;
	/// <summary>The ID returned by <see cref="DirectoryManager.StartEventsSearch"/></summary>
	public UUID getQueryID() { return m_QueryID; } 

	private List<DirectoryManager.EventsSearchData> m_matchedEvents;

	/// <summary>A list of "Events" returned by the data server</summary>
	public List<DirectoryManager.EventsSearchData> getMatchedEvents() {  return m_matchedEvents; } 

	/// <summary>Construct a new instance of the DirEventsReplyEventArgs class</summary>
	/// <param name="queryID">The ID of the query returned by the data server. 
	/// This will correlate to the ID returned by the <see cref="StartEventsSearch"/> method</param>
	/// <param name="matchedEvents">A list containing the "Events" returned by the search query</param>
	public DirEventsReplyEventArgs(UUID queryID, List<DirectoryManager.EventsSearchData> matchedEvents)
	{
		this.m_QueryID = queryID;
		this.m_matchedEvents = matchedEvents;
	}
}
