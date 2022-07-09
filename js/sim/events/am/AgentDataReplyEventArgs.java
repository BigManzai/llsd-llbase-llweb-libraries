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

import java.util.EnumSet;

import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupPowers;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Data sent from the simulator containing information about your agent and active group information</summary>
	public class AgentDataReplyEventArgs extends EventArgs
	{
		private  String m_FirstName;
		private  String m_LastName;
		private  UUID m_ActiveGroupID;
		private  String m_GroupTitle;
		private  EnumSet<GroupPowers> m_GroupPowers;
		private  String m_GroupName;

		/// <summary>Get the agents first name</summary>
		public String getFirstName() {return m_FirstName;}
		/// <summary>Get the agents last name</summary>
		public String getLastName() {return m_LastName;}
		/// <summary>Get the active group ID of your agent</summary>
		public UUID getActiveGroupID() {return m_ActiveGroupID;}
		/// <summary>Get the active groups title of your agent</summary>
		public String getGroupTitle() {return m_GroupTitle;}
		/// <summary>Get the combined group powers of your agent</summary>
		public EnumSet<GroupPowers> getGroupPowers() {return m_GroupPowers;}
		/// <summary>Get the active group name of your agent</summary>
		public String getGroupName() {return m_GroupName;}

		/// <summary>
		/// Construct a new instance of the AgentDataReplyEventArgs object
		/// </summary>
		/// <param name="firstName">The agents first name</param>
		/// <param name="lastName">The agents last name</param>
		/// <param name="activeGroupID">The agents active group ID</param>
		/// <param name="groupTitle">The group title of the agents active group</param>
		/// <param name="groupPowers">The combined group powers the agent has in the active group</param>
		/// <param name="groupName">The name of the group the agent has currently active</param>
		public AgentDataReplyEventArgs(String firstName, String lastName,
				UUID activeGroupID, String groupTitle,
				EnumSet<GroupPowers> groupPowers, String groupName) {
			this.m_FirstName = firstName;
			this.m_LastName = lastName;
			this.m_ActiveGroupID = activeGroupID;
			this.m_GroupTitle = groupTitle;
			this.m_GroupPowers = groupPowers;
			this.m_GroupName = groupName;		}
	}
