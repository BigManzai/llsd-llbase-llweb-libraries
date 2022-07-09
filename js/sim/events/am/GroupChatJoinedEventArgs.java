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

/// <summary>Data sent from the simulator when your agent joins a group chat session</summary>
	public class GroupChatJoinedEventArgs extends EventArgs
	{
		private  UUID m_SessionID;
		private  String m_SessionName;
		private  UUID m_TmpSessionID;
		private  boolean m_Success;

		/// <summary>Get the ID of the group chat session</summary>
		public UUID getSessionID() {return m_SessionID;}
		/// <summary>Get the name of the session</summary>
		public String getSessionName() {return m_SessionName;}
		/// <summary>Get the temporary session ID used for establishing new sessions</summary>
		public UUID getTmpSessionID() {return m_TmpSessionID;}
		/// <summary>True if your agent successfully joined the session</summary>
		public boolean getSuccess() {return m_Success;}

		/// <summary>
		/// Construct a new instance of the GroupChatJoinedEventArgs class
		/// </summary>
		/// <param name="groupChatSessionID">The ID of the session</param>
		/// <param name="sessionName">The name of the session</param>
		/// <param name="tmpSessionID">A temporary session id used for establishing new sessions</param>
		/// <param name="success">True of your agent successfully joined the session</param>
		public GroupChatJoinedEventArgs(UUID groupChatSessionID, String sessionName, UUID tmpSessionID, boolean success)
		{
			this.m_SessionID = groupChatSessionID;
			this.m_SessionName = sessionName;
			this.m_TmpSessionID = tmpSessionID;
			this.m_Success = success;
		}
	}
