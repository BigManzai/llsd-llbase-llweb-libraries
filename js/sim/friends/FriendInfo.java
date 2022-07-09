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
package com.ngt.jopenmetaverse.shared.sim.friends;

import java.util.EnumSet;

import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;
  /// <summary>
    /// This class holds information about an avatar in the friends list.  There are two ways 
    /// to interface to this class.  The first is through the set of boolean properties.  This is the typical
    /// way clients of this class will use it.  The second interface is through two bitflag properties,
    /// TheirFriendsRights and MyFriendsRights
    /// </summary>
    public class FriendInfo
    {
        private UUID m_id;
        private String m_name;
        private boolean m_isOnline;
        private boolean m_canSeeMeOnline;
        private boolean m_canSeeMeOnMap;
        private boolean m_canModifyMyObjects;
        private boolean m_canSeeThemOnline;
        private boolean m_canSeeThemOnMap;
        private boolean m_canModifyTheirObjects;

        //region Properties

        /// <summary>
        /// System ID of the avatar
        /// </summary>
        public UUID getUUID() {  return m_id; } 

        /// <summary>
        /// full name of the avatar
        /// </summary>
        public String getName()
        {
            return m_name; 
        }

        public void setName(String value)
        {
            m_name = value; 
        }
        
        /// <summary>
        /// True if the avatar is online
        /// </summary>
        public boolean isOnline()
        {
            return m_isOnline; 
        }

        public void setIsOnline(boolean value)
        {
            m_isOnline = value; 
        }
        
        /// <summary>
        /// True if the friend can see if I am online
        /// </summary>
        public boolean canSeeMeOnline()
        {
            return m_canSeeMeOnline; 
        }

        public void setCanSeeMeOnline(boolean value)
        {
                m_canSeeMeOnline = value;

                // if I can't see them online, then I can't see them on the map
                if (!m_canSeeMeOnline)
                    m_canSeeMeOnMap = false;
            
        }
        
        /// <summary>
        /// True if the friend can see me on the map 
        /// </summary>
        public boolean canSeeMeOnMap()
        {
            return m_canSeeMeOnMap; 
        }

        
        public void setCanSeeMeOnMap(boolean value)
        {
                // if I can't see them online, then I can't see them on the map
                if (m_canSeeMeOnline)
                    m_canSeeMeOnMap = value;
            
        }
        
        /// <summary>
        /// True if the freind can modify my objects
        /// </summary>
        public boolean canModifyMyObjects()
        {
            return m_canModifyMyObjects;
        }

        public void setCanModifyMyObjects(boolean value)
        {
            m_canModifyMyObjects = value; 
        }
        
        /// <summary>
        /// True if I can see if my friend is online
        /// </summary>
        public boolean getCanSeeThemOnline() { return m_canSeeThemOnline; } 

        /// <summary>
        /// True if I can see if my friend is on the map
        /// </summary>
        public boolean getCanSeeThemOnMap() { return m_canSeeThemOnMap; } 

        /// <summary>
        /// True if I can modify my friend's objects
        /// </summary>
        public boolean getCanModifyTheirObjects() { return m_canModifyTheirObjects; } 

        /// <summary>
        /// My friend's rights represented as bitmapped flags
        /// </summary>
        public EnumSet<FriendRights> getTheirFriendRights()
        {
        		EnumSet<FriendRights> results = FriendRights.get(FriendRights.None.getIndex());
                if (m_canSeeMeOnline)
                    results = FriendRights.get(FriendRights.or(results, FriendRights.CanSeeOnline));
                if (m_canSeeMeOnMap)
                    results = FriendRights.get(FriendRights.or(results, FriendRights.CanSeeOnMap));
                if (m_canModifyMyObjects)
                    results = FriendRights.get(FriendRights.or(results, FriendRights.CanModifyObjects));
                return results;
        }

        public void setTheirFriendRights(EnumSet<FriendRights> value)
        {
                m_canSeeMeOnline = (FriendRights.and(value, FriendRights.CanSeeOnline)) != 0;
                m_canSeeMeOnMap = (FriendRights.and(value, FriendRights.CanSeeOnMap)) != 0;
                m_canModifyMyObjects = (FriendRights.and(value, FriendRights.CanModifyObjects)) != 0;
        }
        
        
        /// <summary>
        /// My rights represented as bitmapped flags
        /// </summary>
        public EnumSet<FriendRights> getMyFriendRights()
        {
//                EnumSet<FriendRights> results = FriendRights.None;
//                if (m_canSeeThemOnline)
//                    results |= FriendRights.CanSeeOnline;
//                if (m_canSeeThemOnMap)
//                    results |= FriendRights.CanSeeOnMap;
//                if (m_canModifyTheirObjects)
//                    results |= FriendRights.CanModifyObjects;

        		EnumSet<FriendRights> results = FriendRights.get(FriendRights.None.getIndex());
                if (m_canSeeThemOnline)
                    results = FriendRights.get(FriendRights.or(results, FriendRights.CanSeeOnline));
                if (m_canSeeThemOnMap)
                    results = FriendRights.get(FriendRights.or(results, FriendRights.CanSeeOnMap));
                if (m_canModifyTheirObjects)
                    results = FriendRights.get(FriendRights.or(results, FriendRights.CanModifyObjects));
                
                return results;
        }

        public void setMyFriendRights(EnumSet<FriendRights> value)
        {
                m_canSeeThemOnline = (FriendRights.and(value, FriendRights.CanSeeOnline)) != 0;
                m_canSeeThemOnMap = (FriendRights.and(value, FriendRights.CanSeeOnMap)) != 0;
                m_canModifyTheirObjects = (FriendRights.and(value, FriendRights.CanModifyObjects)) != 0;
        }
        
        
        //endregion Properties

        /// <summary>
        /// Used internally when building the initial list of friends at login time
        /// </summary>
        /// <param name="id">System ID of the avatar being prepesented</param>
        /// <param name="theirRights">Rights the friend has to see you online and to modify your objects</param>
        /// <param name="myRights">Rights you have to see your friend online and to modify their objects</param>
        public FriendInfo(UUID id, EnumSet<FriendRights> theirRights, EnumSet<FriendRights> myRights)
        {
            m_id = id;
            m_canSeeMeOnline = (FriendRights.and(theirRights, FriendRights.CanSeeOnline)) != 0;
            m_canSeeMeOnMap = (FriendRights.and(theirRights, FriendRights.CanSeeOnMap)) != 0;
            m_canModifyMyObjects = (FriendRights.and(theirRights, FriendRights.CanModifyObjects)) != 0;

            m_canSeeThemOnline = (FriendRights.and(myRights, FriendRights.CanSeeOnline)) != 0;
            m_canSeeThemOnMap = (FriendRights.and(myRights, FriendRights.CanSeeOnMap)) != 0;
            m_canModifyTheirObjects = (FriendRights.and(myRights, FriendRights.CanModifyObjects)) != 0;
        }

		/// <summary>
        /// FriendInfo represented as a string
        /// </summary>
        /// <returns>A String reprentation of both my rights and my friends rights</returns>
        @Override
        public String toString()
        {
            if (!Utils.isNullOrEmpty(m_name))
                return String.format("%s (Their Rights: %s, My Rights: %s)", m_name, 
                		getTheirFriendRights(), getMyFriendRights());
            else
                return String.format("%s (Their Rights: %s, My Rights: %s)", m_id,
                		getTheirFriendRights(), getMyFriendRights());
        }
    }