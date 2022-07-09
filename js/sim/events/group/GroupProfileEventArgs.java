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

import com.ngt.jopenmetaverse.shared.sim.GroupManager.Group;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

public class GroupProfileEventArgs extends EventArgs
    {
        private  Group m_Group;

        /// <summary>Get the group profile</summary>
        public Group getGroup() {return m_Group; }

        /// <summary>Construct a new instance of the GroupProfileEventArgs class</summary>
        /// <param name="group">The group profile</param>
        public GroupProfileEventArgs(Group group)
        {
            this.m_Group = group;
        }
    }

    /// <summary>
    /// Provides notification of a group invitation request sent by another Avatar
    /// </summary>
    /// <remarks>The <see cref="GroupInvitation"/> invitation is raised when another avatar makes an offer for our avatar
    /// to join a group.</remarks>
    