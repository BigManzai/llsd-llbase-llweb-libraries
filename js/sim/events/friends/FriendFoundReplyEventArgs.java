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

import java.math.BigInteger;

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
 /// <summary>
    /// Data sent in response to a <see cref="FindFriend"/> request which contains the information to allow us to map the friends location
    /// </summary>
    public class FriendFoundReplyEventArgs extends  EventArgs
    {
        private  UUID m_AgentID;
        //ulong
        private  BigInteger m_RegionHandle;
        private  Vector3 m_Location;

        /// <summary>Get the ID of the agent we have received location information for</summary>
        public UUID getAgentID() {return m_AgentID;}
        /// <summary>Get the region handle where our mapped friend is located</summary>
        public BigInteger getRegionHandle() {return m_RegionHandle;}
        /// <summary>Get the simulator local position where our friend is located</summary>
        public Vector3 getLocation() {return m_Location;}

        /// <summary>
        /// Construct a new instance of the FriendFoundReplyEventArgs class
        /// </summary>
        /// <param name="agentID">The ID of the agent we have requested location information for</param>
        /// <param name="regionHandle">The region handle where our friend is located</param>
        /// <param name="location">The simulator local position our friend is located</param>
        public FriendFoundReplyEventArgs(UUID agentID, BigInteger regionHandle, Vector3 location)
        {
            this.m_AgentID = agentID;
            this.m_RegionHandle = regionHandle;
            this.m_Location = location;
        }
    }