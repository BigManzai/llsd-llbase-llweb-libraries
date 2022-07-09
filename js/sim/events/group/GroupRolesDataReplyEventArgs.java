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
package com.ngt.jopenmetaverse.shared.sim.events.group;

import java.util.Map;
import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupRole;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

public class GroupRolesDataReplyEventArgs extends EventArgs
    {
        private  UUID m_RequestID;
        private  UUID m_GroupID;
        private Map<UUID, GroupRole> m_Roles;

        /// <summary>Get the ID as returned by the request to correlate
        /// this result set and the request</summary>
        public UUID getRequestID() {return m_RequestID; }
        /// <summary>Get the ID of the group</summary>
        public UUID getGroupID() {return m_GroupID; }
        /// <summary>Get the dictionary containing the roles</summary>
        public Map<UUID, GroupRole> getRoles() {return m_Roles; }

        /// <summary>Construct a new instance of the GroupRolesDataReplyEventArgs class</summary>
        /// <param name="requestID">The ID as returned by the request to correlate
        /// this result set and the request</param>
        /// <param name="groupID">The ID of the group</param>
        /// <param name="roles">The dictionary containing the roles</param>
        public GroupRolesDataReplyEventArgs(UUID requestID, UUID groupID, Map<UUID, GroupRole> roles)
        {
            this.m_RequestID = requestID;
            this.m_GroupID = groupID;
            this.m_Roles = roles;
        }
    }

    /// <summary>Represents the Role to Member mappings for a group</summary>
    