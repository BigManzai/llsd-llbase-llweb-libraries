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

import com.ngt.jopenmetaverse.shared.sim.GroupManager.Group;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
  /// <summary>Contains the current groups your agent is a member of</summary>
    public class CurrentGroupsEventArgs extends EventArgs
    {
        private Map<UUID, Group> m_Groups;

        /// <summary>Get the current groups your agent is a member of</summary>
        public Map<UUID, Group> getGroups() {return m_Groups; }

        /// <summary>Construct a new instance of the CurrentGroupsEventArgs class</summary>
        /// <param name="groups">The current groups your agent is a member of</param>
        public CurrentGroupsEventArgs(Map<UUID, Group> groups)
        {
            this.m_Groups = groups;
        }
    }
    
    /// <summary>A Dictionary of group names, where the Key is the groups ID and the value is the groups name</summary>
    