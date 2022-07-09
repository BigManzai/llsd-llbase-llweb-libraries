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
package com.ngt.jopenmetaverse.shared.protocol.primitives;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;

/// <summary>
/// 
/// </summary>
//[Serializable()]
public class Permissions
{
	public EnumSet<PermissionMask> BaseMask;
	public EnumSet<PermissionMask> EveryoneMask;
	public EnumSet<PermissionMask> GroupMask;
	public EnumSet<PermissionMask> NextOwnerMask;
	public EnumSet<PermissionMask> OwnerMask;

	/// <summary>
	/// 
	/// </summary>

	public enum PermissionMask
	{
		None   ((long)0),
		Transfer    ((long)1 << 13),
		Modify      ((long)1 << 14),
		Copy        ((long)1 << 15),
		//[Obsolete]
		//EnterParcel = 1 << 16,
		//[Obsolete]
		//Terraform   = 1 << 17,
		//[Obsolete]
		//OwnerDebit  = 1 << 18,
		Move        ((long)1 << 19),
		Damage      ((long)1 << 20),
		All         ((long)0x7FFFFFFF);

		private long index;
		private static final Map<Long,PermissionMask> lookup  = new HashMap<Long,PermissionMask>();


		PermissionMask(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}

		static {
			for(PermissionMask s : EnumSet.allOf(PermissionMask.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<PermissionMask> get(Long index)
		{
			EnumSet<PermissionMask> enumsSet = EnumSet.allOf(PermissionMask.class);
			for(Entry<Long,PermissionMask> entry: lookup.entrySet())
			{
				if((entry.getKey().longValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static long getIndex(EnumSet<PermissionMask> enumSet)
		{
			long ret = 0;
			for(PermissionMask s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}

	/// <summary>
	/// 
	/// </summary>

	public enum PermissionWho 
	{
		/// <summary></summary>
		Base ((byte)0x01),
		/// <summary></summary>
		Owner ((byte)0x02),
		/// <summary></summary>
		Group ((byte)0x04),
		/// <summary></summary>
		Everyone ((byte)0x08),
		/// <summary></summary>
		NextOwner ((byte)0x10),
		/// <summary></summary>
		All ((byte)0x1F);

		private byte index;

		public byte getIndex()
		{
			return index;
		}

		PermissionWho(byte index)
		{
			this.index = index;
		}  

		private static final Map<Byte,PermissionWho> lookup  = new HashMap<Byte,PermissionWho>();

		static {
			for(PermissionWho s : EnumSet.allOf(PermissionWho.class))
				lookup.put(s.getIndex(), s);
		} 

		public static EnumSet<PermissionWho> get(Byte index)
		{
			EnumSet<PermissionWho> enumsSet = EnumSet.allOf(PermissionWho.class);
			for(Entry<Byte,PermissionWho> entry: lookup.entrySet())
			{
				if((entry.getKey().byteValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static byte getIndex(EnumSet<PermissionWho> enumSet)
		{
			byte ret = 0;
			for(PermissionWho s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}


	public Permissions()
	{
		this(0,0,0,0,0);
	}

	public Permissions(long baseMask, long everyoneMask, long groupMask, long nextOwnerMask, long ownerMask)
	{
		BaseMask = PermissionMask.get(baseMask);
		EveryoneMask = PermissionMask.get(everyoneMask);
		GroupMask = PermissionMask.get(groupMask);
		NextOwnerMask = PermissionMask.get(nextOwnerMask);
		OwnerMask = PermissionMask.get(ownerMask);
	}

	public Permissions GetNextPermissions()
	{
		long nextMask = PermissionMask.getIndex(NextOwnerMask);

		return new Permissions(
				PermissionMask.getIndex(BaseMask) & nextMask,
				PermissionMask.getIndex(EveryoneMask) & nextMask,
				PermissionMask.getIndex(GroupMask) & nextMask,
				PermissionMask.getIndex(NextOwnerMask),
				PermissionMask.getIndex(OwnerMask) & nextMask
				);
	}

	public OSD GetOSD()
	{
		OSDMap permissions = new OSDMap(5);
		permissions.put("base_mask", OSD.FromLong(PermissionMask.getIndex(BaseMask)));
		permissions.put("everyone_mask", OSD.FromLong(PermissionMask.getIndex(EveryoneMask)));
		permissions.put("group_mask", OSD.FromLong(PermissionMask.getIndex(GroupMask)));
		permissions.put("next_owner_mask", OSD.FromLong(PermissionMask.getIndex(NextOwnerMask)));
		permissions.put("owner_mask", OSD.FromLong(PermissionMask.getIndex(OwnerMask)));
		return permissions;
	}

	public static Permissions FromOSD(OSD llsd)
	{
		Permissions permissions = new Permissions();
		OSDMap map =  (OSDMap) llsd;

		if (map != null)
		{
			permissions.BaseMask = PermissionMask.get(map.get("base_mask").asLong());
			permissions.EveryoneMask = PermissionMask.get(map.get("everyone_mask").asLong());
			permissions.GroupMask = PermissionMask.get(map.get("group_mask").asLong());
			permissions.NextOwnerMask = PermissionMask.get(map.get("next_owner_mask").asLong());
			permissions.OwnerMask = PermissionMask.get(map.get("owner_mask").asLong());
		}

		return permissions;
	}

	public String ToString()
	{
		return String.format("Base: %d, Everyone: %d, Group: %d, NextOwner: %d, Owner: %d",
				BaseMask, EveryoneMask, GroupMask, NextOwnerMask, OwnerMask);
	}

	public  int hashCode()
	{
		return BaseMask.hashCode() ^ EveryoneMask.hashCode() ^ GroupMask.hashCode() ^
				NextOwnerMask.hashCode() ^ OwnerMask.hashCode();
	}

	public boolean equals(Object obj)
	{
		return (obj instanceof Permissions) ? this == (Permissions)obj : false;
	}

	public boolean equals(Permissions other)
	{
		return equals(this, other);
	}

	public static boolean equals(Permissions lhs, Permissions rhs)
	{
		return (PermissionMask.getIndex(lhs.BaseMask) == PermissionMask.getIndex(rhs.BaseMask)) 
				&& (PermissionMask.getIndex(lhs.EveryoneMask) == PermissionMask.getIndex((rhs.EveryoneMask)) &&
				(PermissionMask.getIndex(lhs.GroupMask) == PermissionMask.getIndex(rhs.GroupMask)) 
				&& (PermissionMask.getIndex(lhs.NextOwnerMask) == PermissionMask.getIndex(rhs.NextOwnerMask)) &&
				(PermissionMask.getIndex(lhs.OwnerMask) == PermissionMask.getIndex(rhs.OwnerMask)));
	}

	public static boolean notEquals(Permissions lhs, Permissions rhs)
	{
		return !equals(lhs, rhs);
	}

	public static boolean hasPermissions(EnumSet<PermissionMask> perms, EnumSet<PermissionMask> checkPerms)
	{
		return (PermissionMask.getIndex(perms) & PermissionMask.getIndex(checkPerms)) == PermissionMask.getIndex(checkPerms);
	}

	public static final Permissions NoPermissions = new Permissions();
	public static final Permissions FullPermissions = new Permissions((long)PermissionMask.All.getIndex(), (long)PermissionMask.All.getIndex(),
			(long)PermissionMask.All.getIndex(), (long)PermissionMask.All.getIndex(), (long)PermissionMask.All.getIndex());
}
