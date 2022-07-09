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


import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

public class GroupInvitationEventArgs extends EventArgs
    {
        private  UUID m_FromAgentID;
        private  String m_FromAgentName;
        private  String m_Message;
        private  Simulator m_Simulator;
        private boolean m_accept;
        
        /// <summary>The ID of the Avatar sending the group invitation</summary>
        public UUID getAgentID() {return m_FromAgentID; }
        /// <summary>The name of the Avatar sending the group invitation</summary>
        public String getFromName() {return m_FromAgentName; }
        /// <summary>A message containing the request information which includes
        /// the name of the group, the groups charter and the fee to join details</summary>
        public String getMessage() {return m_Message; }
        /// <summary>The Simulator</summary>
        public Simulator getSimulator() {return m_Simulator; }

        /// <summary>Set to true to accept invitation, false to decline</summary>
        public boolean getAccept() { return m_accept; }
        public void setAccept(boolean value) { m_accept = value; }

        
        public GroupInvitationEventArgs(Simulator simulator, UUID agentID, String agentName, String message)
        {
            this.m_Simulator = simulator;
            this.m_FromAgentID = agentID;
            this.m_FromAgentName = agentName;
            this.m_Message = message;
        }
    }
