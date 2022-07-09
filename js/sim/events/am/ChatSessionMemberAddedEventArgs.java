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
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Data sent when an agent joins a chat session your agent is currently participating in</summary>
public class ChatSessionMemberAddedEventArgs extends EventArgs
{
	private  UUID m_SessionID;
	private  UUID m_AgentID;

	/// <summary>Get the ID of the chat session</summary>
	public UUID getSessionID() {return m_SessionID;}
	/// <summary>Get the ID of the agent that joined</summary>
	public UUID getAgentID() {return m_AgentID;}

	/// <summary>
	/// Construct a new instance of the ChatSessionMemberAddedEventArgs object
	/// </summary>
	/// <param name="sessionID">The ID of the chat session</param>
	/// <param name="agentID">The ID of the agent joining</param>
	public ChatSessionMemberAddedEventArgs(UUID sessionID, UUID agentID)
	{
		this.m_SessionID = sessionID;
		this.m_AgentID = agentID;
	}
}
