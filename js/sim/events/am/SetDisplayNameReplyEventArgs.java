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

import com.ngt.jopenmetaverse.shared.sim.AvatarManager.AgentDisplayName;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

/// <summary>Event arguments with the result of setting display name operation</summary>
	public class SetDisplayNameReplyEventArgs extends EventArgs
	{
		private  int m_Status;
		private  String m_Reason;
		private  AgentDisplayName m_DisplayName;

		/// <summary>Status code, 200 indicates settign display name was successful</summary>
		public int getStatus() {return m_Status;}

		/// <summary>Textual description of the status</summary>
		public String getReason() {return m_Reason;}

		/// <summary>Details of the newly set display name</summary>
		public AgentDisplayName getDisplayName() {return m_DisplayName;}

		/// <summary>Default constructor</summary>
		public SetDisplayNameReplyEventArgs(int status, String reason, AgentDisplayName displayName)
		{
			m_Status = status;
			m_Reason = reason;
			m_DisplayName = displayName;
		}
	}
