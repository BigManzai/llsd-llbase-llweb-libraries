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

import java.util.List;
import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupNoticesListEntry;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

public class GroupNoticesListReplyEventArgs extends EventArgs
    {
        private  UUID m_GroupID;
        private  List<GroupNoticesListEntry> m_Notices;

        /// <summary>Get the ID of the group</summary>
        public UUID getGroupID() {return m_GroupID; }
        /// <summary>Get the notices list</summary>
        public List<GroupNoticesListEntry> getNotices() {return m_Notices; }

        /// <summary>Construct a new instance of the GroupNoticesListReplyEventArgs class</summary>
        /// <param name="groupID">The ID of the group</param>
        /// <param name="notices">The list containing active notices</param>
        public GroupNoticesListReplyEventArgs(UUID groupID, List<GroupNoticesListEntry> notices)
        {
            m_GroupID = groupID;
            m_Notices = notices;
        }
    }
    
    /// <summary>Represents the profile of a group</summary>
    