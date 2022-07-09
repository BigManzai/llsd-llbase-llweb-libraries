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

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
   /// <summary>Sent when another agent requests a friendship with our agent</summary>
    public class FriendshipOfferedEventArgs extends  EventArgs
    {
        private  UUID m_AgentID;
        private  String m_AgentName;
        private  UUID m_SessionID;

        /// <summary>Get the ID of the agent requesting friendship</summary>
        public UUID getAgentID() {return m_AgentID;}
        /// <summary>Get the name of the agent requesting friendship</summary>
        public String getAgentName() {return m_AgentName;}
        /// <summary>Get the ID of the session, used in accepting or declining the 
        /// friendship offer</summary>
        public UUID getSessionID() {return m_SessionID;}

        /// <summary>
        /// Construct a new instance of the FriendshipOfferedEventArgs class
        /// </summary>
        /// <param name="agentID">The ID of the agent requesting friendship</param>
        /// <param name="agentName">The name of the agent requesting friendship</param>
        /// <param name="imSessionID">The ID of the session, used in accepting or declining the 
        /// friendship offer</param>
        public FriendshipOfferedEventArgs(UUID agentID, String agentName, UUID imSessionID)
        {
            this.m_AgentID = agentID;
            this.m_AgentName = agentName;
            this.m_SessionID = imSessionID;
        }
    }