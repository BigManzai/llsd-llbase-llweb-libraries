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

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import java.util.Map;
import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupTitle;
import com.ngt.jopenmetaverse.shared.types.UUID;

public class GroupTitlesReplyEventArgs extends EventArgs
    {
        private  UUID m_RequestID;
        private  UUID m_GroupID;
        private Map<UUID, GroupTitle> m_Titles;

        /// <summary>Get the ID as returned by the request to correlate
        /// this result set and the request</summary>
        public UUID getRequestID() {return m_RequestID; }
        /// <summary>Get the ID of the group</summary>
        public UUID getGroupID() {return m_GroupID; }
        /// <summary>Get the titles</summary>
        public Map<UUID, GroupTitle> getTitles() {return m_Titles; }

        /// <summary>Construct a new instance of the GroupTitlesReplyEventArgs class</summary>
        /// <param name="requestID">The ID as returned by the request to correlate
        /// this result set and the request</param>
        /// <param name="groupID">The ID of the group</param>
        /// <param name="titles">The titles</param>
        public GroupTitlesReplyEventArgs(UUID requestID, UUID groupID, Map<UUID, GroupTitle> titles)
        {
            this.m_RequestID = requestID;
            this.m_GroupID = groupID;
            this.m_Titles = titles;
        }
    }

    /// <summary>Represents the summary data for a group</summary>
    