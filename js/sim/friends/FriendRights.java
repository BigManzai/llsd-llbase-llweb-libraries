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
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
  public enum FriendRights
    {
        /// <summary>The avatar has no rights</summary>
        None(0),
        /// <summary>The avatar can see the online status of the target avatar</summary>
        CanSeeOnline (1),
        /// <summary>The avatar can see the location of the target avatar on the map</summary>
        CanSeeOnMap (2),
        /// <summary>The avatar can modify the ojects of the target avatar </summary>
        CanModifyObjects (4);
    	private int index;
		private static final Map<Integer,FriendRights> lookup  = new HashMap<Integer,FriendRights>();


		FriendRights(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		static {
			for(FriendRights s : EnumSet.allOf(FriendRights.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<FriendRights> get(Integer index)
		{
			EnumSet<FriendRights> enumsSet = EnumSet.allOf(FriendRights.class);
			for(Entry<Integer,FriendRights> entry: lookup.entrySet())
			{
				if((entry.getKey().intValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static int getIndex(EnumSet<FriendRights> enumSet)
		{
			int ret = 0;
			for(FriendRights s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}
		
		
		public static boolean equals(EnumSet<FriendRights> s, EnumSet<FriendRights> p)
		{
			return FriendRights.getIndex(s) == FriendRights.getIndex(p);
		}

		public static int and(EnumSet<FriendRights> s, EnumSet<FriendRights> p)
		{
			return FriendRights.getIndex(s) & FriendRights.getIndex(p);
		}

		public static int and(EnumSet<FriendRights> s, int i)
		{
			return FriendRights.getIndex(s) & i;
		}
		
		public static int xor(EnumSet<FriendRights> s, EnumSet<FriendRights> p)
		{
			return FriendRights.getIndex(s) ^ FriendRights.getIndex(p);
		}

		public static int or(EnumSet<FriendRights> s, EnumSet<FriendRights> p)
		{
			return FriendRights.getIndex(s) | FriendRights.getIndex(p);
		}

		public static int or(EnumSet<FriendRights> s, FriendRights p)
		{
			return FriendRights.getIndex(s) | p.getIndex();
		}
		
		public static int and(EnumSet<FriendRights> s, FriendRights p)
		{
			return (FriendRights.getIndex(s) & p.getIndex());
		}
		
    }