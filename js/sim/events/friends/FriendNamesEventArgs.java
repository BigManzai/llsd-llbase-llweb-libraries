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
package com.ngt.jopenmetaverse.shared.sim.events.friends;

import java.util.Map;

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Contains Friend Names</summary>
    public class FriendNamesEventArgs extends  EventArgs
    {
        private  Map<UUID, String> m_Names;

        /// <summary>A dictionary where the Key is the ID of the Agent, 
        /// and the Value is a String containing their name</summary>
        public Map<UUID, String> getNames() {return m_Names;}

        /// <summary>
        /// Construct a new instance of the FriendNamesEventArgs class
        /// </summary>
        /// <param name="names">A dictionary where the Key is the ID of the Agent, 
        /// and the Value is a String containing their name</param>
        public FriendNamesEventArgs(Map<UUID, String> names)
        {
            this.m_Names = names;
        }
    }