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
package com.ngt.jopenmetaverse.shared.sim;

import java.io.Serializable;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observer;
import java.util.Map.Entry;
import java.util.Observable;

import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessage;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageDialog;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageOnline;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.InstantMessageEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.XferReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.im.*;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventObservableArg;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.inventory.Inventory;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryBase;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryCallingCard;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryItem;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryObject;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventorySound;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryTexture;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseCallbackArg;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseData;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.CopyInventoryFromNotecardMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ScriptRunningReplyMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.UpdateScriptAgentRequestMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.UpdateScriptTaskUpdateMessage;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.Enums;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.Enums.InventoryType;
import com.ngt.jopenmetaverse.shared.types.Enums.SaleType;
import com.ngt.jopenmetaverse.shared.types.Enums.WearableType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.AttachmentPoint;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.exception.NotImplementedException;
import com.ngt.jopenmetaverse.shared.exception.nm.InventoryException;
import com.ngt.jopenmetaverse.shared.protocol.BulkUpdateInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.CopyInventoryFromNotecardPacket;
import com.ngt.jopenmetaverse.shared.protocol.CopyInventoryItemPacket;
import com.ngt.jopenmetaverse.shared.protocol.CreateInventoryFolderPacket;
import com.ngt.jopenmetaverse.shared.protocol.CreateInventoryItemPacket;
import com.ngt.jopenmetaverse.shared.protocol.DeRezObjectPacket;
import com.ngt.jopenmetaverse.shared.protocol.FetchInventoryDescendentsPacket;
import com.ngt.jopenmetaverse.shared.protocol.FetchInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.FetchInventoryReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GetScriptRunningPacket;
import com.ngt.jopenmetaverse.shared.protocol.ImprovedInstantMessagePacket;
import com.ngt.jopenmetaverse.shared.protocol.InventoryDescendentsPacket;
import com.ngt.jopenmetaverse.shared.protocol.LinkInventoryItemPacket;
import com.ngt.jopenmetaverse.shared.protocol.MoveInventoryFolderPacket;
import com.ngt.jopenmetaverse.shared.protocol.MoveInventoryItemPacket;
import com.ngt.jopenmetaverse.shared.protocol.MoveTaskInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.PurgeInventoryDescendentsPacket;
import com.ngt.jopenmetaverse.shared.protocol.RemoveInventoryObjectsPacket;
import com.ngt.jopenmetaverse.shared.protocol.RemoveTaskInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.ReplyTaskInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.RequestTaskInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.RezObjectPacket;
import com.ngt.jopenmetaverse.shared.protocol.RezRestoreToWorldPacket;
import com.ngt.jopenmetaverse.shared.protocol.RezScriptPacket;
import com.ngt.jopenmetaverse.shared.protocol.SaveAssetIntoInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.SetScriptRunningPacket;
import com.ngt.jopenmetaverse.shared.protocol.UpdateCreateInventoryItemPacket;
import com.ngt.jopenmetaverse.shared.protocol.UpdateInventoryFolderPacket;
import com.ngt.jopenmetaverse.shared.protocol.UpdateInventoryItemPacket;
import com.ngt.jopenmetaverse.shared.protocol.UpdateTaskInventoryPacket;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions.PermissionMask;


/// <summary>
/// Tools for dealing with agents inventory
/// </summary>
public class InventoryManager {
	//region Enums

	//	    [Flags]
	public static enum InventorySortOrder 
	{
		/// <summary>Sort by name</summary>
		ByName(0),
		/// <summary>Sort by date</summary>
		ByDate(1),
		/// <summary>Sort folders by name, regardless of whether items are
		/// sorted by name or date</summary>
		FoldersByName(2),
		/// <summary>Place system folders at the top</summary>
		SystemFoldersToTop(4);
		private int index;
		InventorySortOrder(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,InventorySortOrder> lookup  = new HashMap<Integer,InventorySortOrder>();

		static {
			for(InventorySortOrder s : EnumSet.allOf(InventorySortOrder.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<InventorySortOrder> get(Integer index)
		{
			EnumSet<InventorySortOrder> enumsSet = EnumSet.allOf(InventorySortOrder.class);
			for(Entry<Integer,InventorySortOrder> entry: lookup.entrySet())
			{
				if((entry.getKey().intValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static int getIndex(EnumSet<InventorySortOrder> enumSet)
		{
			int ret = 0;
			for(InventorySortOrder s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}
	}

	/// <summary>
	/// Possible destinations for DeRezObject request
	/// </summary>
	public static enum DeRezDestination
	{
		//byte
		/// <summary></summary>
		AgentInventorySave ((byte) 0),
		/// <summary>Copy from in-world to agent inventory</summary>
		AgentInventoryCopy ((byte) 1),
		/// <summary>Derez to TaskInventory</summary>
		TaskInventory ((byte) 2),
		/// <summary></summary>
		Attachment ((byte) 3),
		/// <summary>Take Object</summary>
		AgentInventoryTake ((byte) 4),
		/// <summary></summary>
		ForceToGodInventory ((byte) 5),
		/// <summary>Delete Object</summary>
		TrashFolder ((byte) 6),
		/// <summary>Put an avatar attachment into agent inventory</summary>
		AttachmentToInventory ((byte) 7),
		/// <summary></summary>
		AttachmentExists ((byte) 8),
		/// <summary>Return an object back to the owner's inventory</summary>
		ReturnToOwner ((byte) 9),
		/// <summary>Return a deeded object back to the last owner's inventory</summary>
		ReturnToLastOwner ((byte)10);
		private byte index;
		DeRezDestination(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,DeRezDestination> lookup  = new HashMap<Byte,DeRezDestination>();

		static {
			for(DeRezDestination s : EnumSet.allOf(DeRezDestination.class))
				lookup.put(s.getIndex(), s);
		}

		public static DeRezDestination get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Upper half of the Flags field for inventory items
	/// </summary>
	//	    [Flags]
	public static enum InventoryItemFlags 
	{
		//uint
		None ((long) 0),
		/// <summary>Indicates that the NextOwner permission will be set to the
		/// most restrictive set of permissions found in the object set
		/// (including linkset items and object inventory items) on next rez</summary>
		ObjectSlamPerm ((long) 0x100),
		/// <summary>Indicates that the object sale information has been
		/// changed</summary>
		ObjectSlamSale ((long) 0x1000),
		/// <summary>If set, and a slam bit is set, indicates BaseMask will be overwritten on Rez</summary>
		ObjectOverwriteBase ((long) 0x010000),
		/// <summary>If set, and a slam bit is set, indicates OwnerMask will be overwritten on Rez</summary>
		ObjectOverwriteOwner ((long) 0x020000),
		/// <summary>If set, and a slam bit is set, indicates GroupMask will be overwritten on Rez</summary>
		ObjectOverwriteGroup ((long) 0x040000),
		/// <summary>If set, and a slam bit is set, indicates EveryoneMask will be overwritten on Rez</summary>
		ObjectOverwriteEveryone ((long) 0x080000),
		/// <summary>If set, and a slam bit is set, indicates NextOwnerMask will be overwritten on Rez</summary>
		ObjectOverwriteNextOwner ((long) 0x100000),
		/// <summary>Indicates whether this object is composed of multiple
		/// items or not</summary>
		ObjectHasMultipleItems ((long) 0x200000),
		/// <summary>Indicates that the asset is only referenced by this
		/// inventory item. If this item is deleted or updated to reference a
		/// new assetID, the asset can be deleted</summary>
		SharedSingleReference((long)0x40000000);
		private long index;
		InventoryItemFlags(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}

		private static final Map<Long,InventoryItemFlags> lookup  = new HashMap<Long,InventoryItemFlags>();

		static {
			for(InventoryItemFlags s : EnumSet.allOf(InventoryItemFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<InventoryItemFlags> get(Long index)
		{
			EnumSet<InventoryItemFlags> enumsSet = EnumSet.allOf(InventoryItemFlags.class);
			for(Entry<Long,InventoryItemFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().longValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static long getIndex(EnumSet<InventoryItemFlags> enumSet)
		{
			long ret = 0;
			for(InventoryItemFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}
	}

	//endregion Enums

	//region Inventory Object Classes




	/// <summary>
	/// InventoryLandmark Class, contains details on a specific location
	/// </summary>
	public static class InventoryLandmark extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryLandmark object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryLandmark(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Landmark;
		}

		/// <summary>
		/// Construct an InventoryLandmark object from a serialization stream
		/// </summary>
		public InventoryLandmark(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Landmark;

		}

		/// <summary>
		/// Landmarks use the InventoryItemFlags struct and will have a flag of 1 set if they have been visited
		/// </summary>
		public boolean getLandmarkVisited()
		{
			return (Flags & 1) != 0; 
		}

		public void setLandmarkVisited(boolean value)
		{
			if (value) Flags |= (long)1;
			else Flags &= (long)0;
		}
	}

	/// <summary>
	/// InventoryNotecard Class, contains details on an encoded text document
	/// </summary>
	public static class InventoryNotecard extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryNotecard object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryNotecard(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Notecard;
		}

		/// <summary>
		/// Construct an InventoryNotecard object from a serialization stream
		/// </summary>
		public InventoryNotecard(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Notecard;
		}
	}

	/// <summary>
	/// InventoryCategory Class
	/// </summary>
	/// <remarks>TODO: Is this even used for anything?</remarks>
	public static class InventoryCategory extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryCategory object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryCategory(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Category;
		}

		/// <summary>
		/// Construct an InventoryCategory object from a serialization stream
		/// </summary>
		public InventoryCategory(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Category;
		}
	}

	/// <summary>
	/// InventoryLSL Class, represents a Linden Scripting Language object
	/// </summary>
	public static class InventoryLSL extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryLSL object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryLSL(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.LSL;
		}

		/// <summary>
		/// Construct an InventoryLSL object from a serialization stream
		/// </summary>
		public InventoryLSL(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.LSL;
		}
	}

	/// <summary>
	/// InventorySnapshot Class, an image taken with the viewer
	/// </summary>
	public static class InventorySnapshot extends InventoryItem
	{
		/// <summary>
		/// Construct an InventorySnapshot object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventorySnapshot(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Snapshot;
		}

		/// <summary>
		/// Construct an InventorySnapshot object from a serialization stream
		/// </summary>
		public InventorySnapshot(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Snapshot;
		}
	}

	/// <summary>
	/// InventoryAttachment Class, contains details on an attachable object
	/// </summary>
	public static class InventoryAttachment extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryAttachment object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryAttachment(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Attachment;
		}

		/// <summary>
		/// Construct an InventoryAttachment object from a serialization stream
		/// </summary>
		public InventoryAttachment(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Attachment;
		}

		/// <summary>
		/// Get the last AttachmentPoint this object was attached to
		/// </summary>
		public AttachmentPoint getAttachmentPoint()
		{return AttachmentPoint.get((byte)Flags);}
		public void setAttachmentPoint(AttachmentPoint value)
		{Flags = value.getIndex();}

	}

	/// <summary>
	/// InventoryWearable Class, details on a clothing item or body part
	/// </summary>
	public static class InventoryWearable extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryWearable object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryWearable(UUID itemID)
		{ 
			super(itemID); 
			InventoryType = InventoryType.Wearable; 
		}

		/// <summary>
		/// Construct an InventoryWearable object from a serialization stream
		/// </summary>
		public InventoryWearable(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Wearable;
		}

		/// <summary>
		/// The <seealso cref="OpenMetaverse.WearableType"/>, Skin, Shape, Skirt, Etc
		/// </summary>
		public WearableType getWearableType()
		{return WearableType.get((byte)Flags);}
		public void setWearableType(WearableType value)
		{Flags = value.getIndex();}


	}

	/// <summary>
	/// InventoryAnimation Class, A bvh encoded object which animates an avatar
	/// </summary>
	public static class InventoryAnimation extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryAnimation object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryAnimation(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Animation;
		}

		/// <summary>
		/// Construct an InventoryAnimation object from a serialization stream
		/// </summary>
		public InventoryAnimation(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Animation;
		}


	}

	/// <summary>
	/// InventoryGesture Class, details on a series of animations, sounds, and actions
	/// </summary>
	public static class InventoryGesture extends InventoryItem
	{
		/// <summary>
		/// Construct an InventoryGesture object
		/// </summary>
		/// <param name="itemID">A <seealso cref="OpenMetaverse.UUID"/> which becomes the 
		/// <seealso cref="OpenMetaverse.InventoryItem"/> objects AssetUUID</param>
		public InventoryGesture(UUID itemID)

		{
			super(itemID);
			InventoryType = InventoryType.Gesture;
		}

		/// <summary>
		/// Construct an InventoryGesture object from a serialization stream
		/// </summary>
		public InventoryGesture(Map<String, Object> info)

		{
			super(info);
			InventoryType = InventoryType.Gesture;
		}
	}

	/// <summary>
	/// A folder contains <seealso cref="T:OpenMetaverse.InventoryItem"/>s and has certain attributes specific 
	/// to itself
	/// </summary>
	public static class InventoryFolder extends InventoryBase
	{
		/// <summary>The Preferred <seealso cref="T:OpenMetaverse.AssetType"/> for a folder.</summary>
		public AssetType PreferredType;
		/// <summary>The Version of this folder</summary>
		public int Version;
		/// <summary>Number of child items this folder contains.</summary>
		public int DescendentCount;

		/// <summary>
		/// Constructor
		/// </summary>
		/// <param name="itemID">UUID of the folder</param>
		public InventoryFolder(UUID itemID)

		{
			super(itemID);
			PreferredType = AssetType.Unknown;
			Version = 1;
			DescendentCount = 0;
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		@Override
		public String toString()
		{
			return Name;
		}

		/// <summary>
		/// Get Serilization data for this InventoryFolder object
		/// </summary>
		@Override 
		public Map<String, Object> getObjectData()
		{
			Map<String, Object> info = super.getObjectData();
			info.put("PreferredType", PreferredType);
			info.put("Version", Version);
			info.put("DescendentCount", DescendentCount);
			return info;
		}

		/// <summary>
		/// Construct an InventoryFolder object from a serialization stream
		/// </summary>
		public InventoryFolder(Map<String, Object> info)

		{
			super(info);
			PreferredType = AssetType.get((Byte)info.get("PreferredType"));
			Version = (Integer)info.get("Version");
			DescendentCount = (Integer)info.get("DescendentCount");
		}

		/// <summary>
		/// 
		/// </summary>
		/// <returns></returns>
		@Override
		public  int hashCode()
		{
			return PreferredType.hashCode() ^ new Integer(Version).hashCode() ^ new Integer(DescendentCount).hashCode();
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="o"></param>
		/// <returns></returns>
		@Override
		public  boolean equals(Object o)
		{
			InventoryFolder folder = (InventoryFolder)o ;
			return folder != null && equals(folder);
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="o"></param>
		/// <returns></returns>
		@Override
		public  boolean equals(InventoryBase o)
		{
			InventoryFolder folder = (InventoryFolder)o;
			return folder != null && equals(folder);
		}

		/// <summary>
		/// 
		/// </summary>
		/// <param name="o"></param>
		/// <returns></returns>
		public boolean equals(InventoryFolder o)
		{
			return super.equals((InventoryBase)o)
					&& o.DescendentCount == DescendentCount
					&& o.PreferredType == PreferredType
					&& o.Version == Version;
		}
	}

	//endregion Inventory Object Classes

	/// <summary>Used for converting shadow_id to asset_id</summary>
	public static final UUID MAGIC_ID = new UUID("3c115e51-04f4-523c-9fa6-98aff1034730");

	//struct
	public static class InventorySearch
	{
		public UUID Folder;
		public UUID Owner;
		public String[] Path;
		public int Level;
	}

	//	//region Delegates

	private EventObservable<ItemReceivedEventArgs> onItemReceived = new EventObservable<ItemReceivedEventArgs>();
	public void registerOnItemReceived(EventObserver<ItemReceivedEventArgs> o)
	{
		onItemReceived.addObserver(o);
	}
	public void unregisterOnItemReceived(EventObserver<ItemReceivedEventArgs> o) 
	{
		onItemReceived.deleteObserver(o);
	}
	private EventObservable<FolderUpdatedEventArgs> onFolderUpdated = new EventObservable<FolderUpdatedEventArgs>();
	public void registerOnFolderUpdated(EventObserver<FolderUpdatedEventArgs> o)
	{
		onFolderUpdated.addObserver(o);
	}
	public void unregisterOnFolderUpdated(EventObserver<FolderUpdatedEventArgs> o) 
	{
		onFolderUpdated.deleteObserver(o);
	}
	private EventObservable<InventoryObjectOfferedEventArgs> onInventoryObjectOffered = new EventObservable<InventoryObjectOfferedEventArgs>();
	public void registerOnInventoryObjectOffered(EventObserver<InventoryObjectOfferedEventArgs> o)
	{
		onInventoryObjectOffered.addObserver(o);
	}
	public void unregisterOnInventoryObjectOffered(EventObserver<InventoryObjectOfferedEventArgs> o) 
	{
		onInventoryObjectOffered.deleteObserver(o);
	}
	private EventObservable<TaskItemReceivedEventArgs> onTaskItemReceived = new EventObservable<TaskItemReceivedEventArgs>();
	public void registerOnTaskItemReceived(EventObserver<TaskItemReceivedEventArgs> o)
	{
		onTaskItemReceived.addObserver(o);
	}
	public void unregisterOnTaskItemReceived(EventObserver<TaskItemReceivedEventArgs> o) 
	{
		onTaskItemReceived.deleteObserver(o);
	}

	private EventObservable<FindObjectByPathReplyEventArgs> onFindObjectByPathReply = new EventObservable<FindObjectByPathReplyEventArgs>();
	public void registerOnFindObjectByPathReply(EventObserver<FindObjectByPathReplyEventArgs> o)
	{
		onFindObjectByPathReply.addObserver(o);
	}
	public void unregisterOnFindObjectByPathReply(EventObserver<FindObjectByPathReplyEventArgs> o) 
	{
		onFindObjectByPathReply.deleteObserver(o);
	}
	private EventObservable<TaskInventoryReplyEventArgs> onTaskInventoryReply = new EventObservable<TaskInventoryReplyEventArgs>();
	public void registerOnTaskInventoryReply(EventObserver<TaskInventoryReplyEventArgs> o)
	{
		onTaskInventoryReply.addObserver(o);
	}
	public void unregisterOnTaskInventoryReply(EventObserver<TaskInventoryReplyEventArgs> o) 
	{
		onTaskInventoryReply.deleteObserver(o);
	}
	private EventObservable<SaveAssetToInventoryEventArgs> onSaveAssetToInventory = new EventObservable<SaveAssetToInventoryEventArgs>();
	public void registerOnSaveAssetToInventory(EventObserver<SaveAssetToInventoryEventArgs> o)
	{
		onSaveAssetToInventory.addObserver(o);
	}
	public void unregisterOnSaveAssetToInventory(EventObserver<SaveAssetToInventoryEventArgs> o) 
	{
		onSaveAssetToInventory.deleteObserver(o);
	}
	private EventObservable<ScriptRunningReplyEventArgs> onScriptRunningReply = new EventObservable<ScriptRunningReplyEventArgs>();
	public void registerOnScriptRunningReply(EventObserver<ScriptRunningReplyEventArgs> o)
	{
		onScriptRunningReply.addObserver(o);
	}
	public void unregisterOnScriptRunningReply(EventObserver<ScriptRunningReplyEventArgs> o) 
	{
		onScriptRunningReply.deleteObserver(o);
	}

	//
	//	/// <summary>
	//	/// Callback for inventory item creation finishing
	//	/// </summary>
	//	/// <param name="success">Whether the request to create an inventory
	//	/// item succeeded or not</param>
	//	/// <param name="item">Inventory item being created. If success is
	//	/// false this will be null</param>
	//	public delegate void ItemCreatedCallback(boolean success, InventoryItem item);
	//
	//	/// <summary>
	//	/// Callback for an inventory item being create from an uploaded asset
	//	/// </summary>
	//	/// <param name="success">true if inventory item creation was successful</param>
	//	/// <param name="status"></param>
	//	/// <param name="itemID"></param>
	//	/// <param name="assetID"></param>
	//	public delegate void ItemCreatedFromAssetCallback(boolean success,String status, UUID itemID, UUID assetID);
	//
	//	/// <summary>
	//	/// 
	//	/// </summary>
	//	/// <param name="item"></param>
	//	public delegate void ItemCopiedCallback(InventoryBase item);
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<ItemReceivedEventArgs> m_ItemReceived;
	//
	//	///<summary>Raises the ItemReceived Event</summary>
	//	/// <param name="e">A ItemReceivedEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnItemReceived(ItemReceivedEventArgs e)
	//	{
	//		EventHandler<ItemReceivedEventArgs> handler = m_ItemReceived;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_ItemReceivedLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// ...</summary>
	//	public event EventHandler<ItemReceivedEventArgs> ItemReceived 	
	//	{
	//		add { synchronized(m_ItemReceivedLock) { m_ItemReceived += value; } }
	//		remove { synchronized(m_ItemReceivedLock) { m_ItemReceived -= value; } }
	//	}
	//
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<FolderUpdatedEventArgs> m_FolderUpdated;
	//
	//	///<summary>Raises the FolderUpdated Event</summary>
	//	/// <param name="e">A FolderUpdatedEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnFolderUpdated(FolderUpdatedEventArgs e)
	//	{
	//		EventHandler<FolderUpdatedEventArgs> handler = m_FolderUpdated;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_FolderUpdatedLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// ...</summary>
	//	public event EventHandler<FolderUpdatedEventArgs> FolderUpdated 
	//	{
	//		add { synchronized(m_FolderUpdatedLock) { m_FolderUpdated += value; } }
	//		remove { synchronized(m_FolderUpdatedLock) { m_FolderUpdated -= value; } }
	//	}
	//
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<InventoryObjectOfferedEventArgs> m_InventoryObjectOffered;
	//
	//	///<summary>Raises the InventoryObjectOffered Event</summary>
	//	/// <param name="e">A InventoryObjectOfferedEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnInventoryObjectOffered(InventoryObjectOfferedEventArgs e)
	//	{
	//		EventHandler<InventoryObjectOfferedEventArgs> handler = m_InventoryObjectOffered;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_InventoryObjectOfferedLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// an inventory object sent by another avatar or primitive</summary>
	//	public event EventHandler<InventoryObjectOfferedEventArgs> InventoryObjectOffered 
	//	{
	//		add { synchronized(m_InventoryObjectOfferedLock) { m_InventoryObjectOffered += value; } }
	//		remove { synchronized(m_InventoryObjectOfferedLock) { m_InventoryObjectOffered -= value; } }
	//	}
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<TaskItemReceivedEventArgs> m_TaskItemReceived;
	//
	//	///<summary>Raises the TaskItemReceived Event</summary>
	//	/// <param name="e">A TaskItemReceivedEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnTaskItemReceived(TaskItemReceivedEventArgs e)
	//	{
	//		EventHandler<TaskItemReceivedEventArgs> handler = m_TaskItemReceived;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_TaskItemReceivedLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// ...</summary>
	//	public event EventHandler<TaskItemReceivedEventArgs> TaskItemReceived 
	//	{
	//		add { synchronized(m_TaskItemReceivedLock) { m_TaskItemReceived += value; } }
	//		remove { synchronized(m_TaskItemReceivedLock) { m_TaskItemReceived -= value; } }
	//	}
	//
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<FindObjectByPathReplyEventArgs> m_FindObjectByPathReply;
	//
	//	///<summary>Raises the FindObjectByPath Event</summary>
	//	/// <param name="e">A FindObjectByPathEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnFindObjectByPathReply(FindObjectByPathReplyEventArgs e)
	//	{
	//		EventHandler<FindObjectByPathReplyEventArgs> handler = m_FindObjectByPathReply;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_FindObjectByPathReplyLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// ...</summary>
	//	public event EventHandler<FindObjectByPathReplyEventArgs> FindObjectByPathReply 
	//	{
	//		add { synchronized(m_FindObjectByPathReplyLock) { m_FindObjectByPathReply += value; } }
	//		remove { synchronized(m_FindObjectByPathReplyLock) { m_FindObjectByPathReply -= value; } }
	//	}
	//
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<TaskInventoryReplyEventArgs> m_TaskInventoryReply;
	//
	//	///<summary>Raises the TaskInventoryReply Event</summary>
	//	/// <param name="e">A TaskInventoryReplyEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnTaskInventoryReply(TaskInventoryReplyEventArgs e)
	//	{
	//		EventHandler<TaskInventoryReplyEventArgs> handler = m_TaskInventoryReply;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_TaskInventoryReplyLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// ...</summary>
	//	public event EventHandler<TaskInventoryReplyEventArgs> TaskInventoryReply 
	//	{
	//		add { synchronized(m_TaskInventoryReplyLock) { m_TaskInventoryReply += value; } }
	//		remove { synchronized(m_TaskInventoryReplyLock) { m_TaskInventoryReply -= value; } }
	//	}
	//
	//	/// <summary>
	//	/// Reply received when uploading an inventory asset
	//	/// </summary>
	//	/// <param name="success">Has upload been successful</param>
	//	/// <param name="status">Error message if upload failed</param>
	//	/// <param name="itemID">Inventory asset UUID</param>
	//	/// <param name="assetID">New asset UUID</param>
	//	public delegate void InventoryUploadedAssetCallback(boolean success,String status, UUID itemID, UUID assetID);
	//
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<SaveAssetToInventoryEventArgs> m_SaveAssetToInventory;
	//
	//	///<summary>Raises the SaveAssetToInventory Event</summary>
	//	/// <param name="e">A SaveAssetToInventoryEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnSaveAssetToInventory(SaveAssetToInventoryEventArgs e)
	//	{
	//		EventHandler<SaveAssetToInventoryEventArgs> handler = m_SaveAssetToInventory;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_SaveAssetToInventoryLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// ...</summary>
	//	public event EventHandler<SaveAssetToInventoryEventArgs> SaveAssetToInventory 
	//	{
	//		add { synchronized(m_SaveAssetToInventoryLock) { m_SaveAssetToInventory += value; } }
	//		remove { synchronized(m_SaveAssetToInventoryLock) { m_SaveAssetToInventory -= value; } }
	//	}
	//
	//	/// <summary>
	//	/// Delegate that is invoked when script upload is completed
	//	/// </summary>
	//	/// <param name="uploadSuccess">Has upload succeded (note, there still might be compile errors)</param>
	//	/// <param name="uploadStatus">Upload status message</param>
	//	/// <param name="compileSuccess">Is compilation successful</param>
	//	/// <param name="compileMessages">If compilation failed, list of error messages, null on compilation success</param>
	//	/// <param name="itemID">Script inventory UUID</param>
	//	/// <param name="assetID">Script's new asset UUID</param>
	//	public delegate void ScriptUpdatedCallback(boolean uploadSuccess,String uploadStatus, boolean compileSuccess, List<String> compileMessages, UUID itemID, UUID assetID);
	//
	//	/// <summary>The event subscribers, null of no subscribers</summary>
	//	private EventHandler<ScriptRunningReplyEventArgs> m_ScriptRunningReply;
	//
	//	///<summary>Raises the ScriptRunningReply Event</summary>
	//	/// <param name="e">A ScriptRunningReplyEventArgs object containing
	//	/// the data sent from the simulator</param>
	//	protected virtual void OnScriptRunningReply(ScriptRunningReplyEventArgs e)
	//	{
	//		EventHandler<ScriptRunningReplyEventArgs> handler = m_ScriptRunningReply;
	//		if (handler != null)
	//			handler(this, e);
	//	}
	//
	//	/// <summary>Thread sync lock object</summary>
	//	private object m_ScriptRunningReplyLock = new object();
	//
	//	/// <summary>Raised when the simulator sends us data containing
	//	/// ...</summary>
	//	public event EventHandler<ScriptRunningReplyEventArgs> ScriptRunningReply 
	//	{
	//		add { synchronized(m_ScriptRunningReplyLock) { m_ScriptRunningReply += value; } }
	//		remove { synchronized(m_ScriptRunningReplyLock) { m_ScriptRunningReply -= value; } }
	//	}
	//	//endregion Delegates

	//region String Arrays

	/// <summary>Partial mapping of AssetTypes to folder names</summary>
	private final static String[] _NewFolderNames =	{
		"Textures",
		"Sounds",
		"Calling Cards",
		"Landmarks",
		"Scripts",
		"Clothing",
		"Objects",
		"Notecards",
		"New Folder",
		"Inventory",
		"Scripts",
		"Scripts",
		"Uncompressed Images",
		"Body Parts",
		"Trash",
		"Photo Album",
		"Lost And Found",
		"Uncompressed Sounds",
		"Uncompressed Images",
		"Uncompressed Images",
		"Animations",
		"Gestures"
	};

	//endregion String Arrays

	private GridClient Client;
	private Inventory _Store;
	//private Random _RandNumbers = new Random();
	private Object _CallbacksLock = new Object();
	//uint
	private int _CallbackPos;
	private Map<Long, EventObserver<ItemCreatedCallbackArg>> _ItemCreatedCallbacks = new HashMap<Long, EventObserver<ItemCreatedCallbackArg>>();
	private Map<Long, EventObserver<ItemCopiedCallbackArg>> _ItemCopiedCallbacks = new HashMap<Long, EventObserver<ItemCopiedCallbackArg>>();
	private List<InventorySearch> _Searches = new ArrayList<InventorySearch>();

	//region Properties

	/// <summary>
	/// Get this agents Inventory data
	/// </summary>
	public Inventory getStore() { return _Store; } 

	//endregion Properties

	/// <summary>
	/// Default constructor
	/// </summary>
	/// <param name="client">Reference to the GridClient object</param>
	public InventoryManager(GridClient client)
	{
		Client = client;

		// Client.network.RegisterCallback(PacketType.UpdateCreateInventoryItem, UpdateCreateInventoryItemHandler);
		Client.network.RegisterCallback(PacketType.UpdateCreateInventoryItem, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ UpdateCreateInventoryItemHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		// Client.network.RegisterCallback(PacketType.SaveAssetIntoInventory, SaveAssetIntoInventoryHandler);

		Client.network.RegisterCallback(PacketType.SaveAssetIntoInventory, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ SaveAssetIntoInventoryHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		// Client.network.RegisterCallback(PacketType.BulkUpdateInventory, BulkUpdateInventoryHandler);

		Client.network.RegisterCallback(PacketType.BulkUpdateInventory, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ BulkUpdateInventoryHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		// Client.network.RegisterCallback(PacketType.MoveInventoryItem, MoveInventoryItemHandler);

		Client.network.RegisterCallback(PacketType.MoveInventoryItem, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ MoveInventoryItemHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		// Client.network.RegisterCallback(PacketType.InventoryDescendents, InventoryDescendentsHandler);

		Client.network.RegisterCallback(PacketType.InventoryDescendents, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ InventoryDescendentsHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		// Client.network.RegisterCallback(PacketType.FetchInventoryReply, FetchInventoryReplyHandler);

		Client.network.RegisterCallback(PacketType.FetchInventoryReply, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ FetchInventoryReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		// Client.network.RegisterCallback(PacketType.ReplyTaskInventory, ReplyTaskInventoryHandler);

		Client.network.RegisterCallback(PacketType.ReplyTaskInventory, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ReplyTaskInventoryHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		// Client.network.RegisterEventCallback("ScriptRunningReply", new Caps.EventQueueCallback(ScriptRunningReplyMessageHandler);
		Client.network.RegisterEventCallback("ScriptRunningReply", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ ScriptRunningReplyMessageHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);

		// Watch for inventory given to us through instant message            
		//				Client.self.IM += Self_IM;
		Client.self.registerIM(new EventObserver<InstantMessageEventArgs>()
				{
			@Override
			public void handleEvent(Observable o,
					InstantMessageEventArgs arg) {
				try {
					Self_IM(o, arg);
				} catch (InventoryException e) {
					JLogger.warn(Utils.getExceptionStackTraceAsString(e));
				}
			}
				});

		// Register extra parameters with login and parse the inventory data that comes back
		String[] options = new String[] {
				"inventory-root", "inventory-skeleton", "inventory-lib-root",
				"inventory-lib-owner", "inventory-skel-lib"};

		Client.network.RegisterLoginResponseCallback(new EventObserver<LoginResponseCallbackArg>()
				{
			@Override
			public void handleEvent(Observable o, LoginResponseCallbackArg arg) {
				Network_OnLoginResponse(arg.isLoginSuccess(), arg.isRedirect(), arg.getMessage(),
						arg.getReason(), arg.getReplyData());
			}
				}, options);
	}
	//	//region Fetch

	/// <summary>
	/// Fetch an inventory item from the dataserver
	/// </summary>
	/// <param name="itemID">The items <seealso cref="UUID"/></param>
	/// <param name="ownerID">The item Owners <seealso cref="OpenMetaverse.UUID"/></param>
	/// <param name="timeoutMS">a integer representing the number of milliseconds to wait for results</param>
	/// <returns>An <seealso cref="InventoryItem"/> object on success, or null if no item was found</returns>
	/// <remarks>Items will also be sent to the <seealso cref="InventoryManager.OnItemReceived"/> event</remarks>
	public InventoryItem FetchItem(final UUID itemID, UUID ownerID, int timeoutMS) throws InterruptedException
	{
		final AutoResetEvent fetchEvent = new AutoResetEvent(false);
		InventoryItem fetchedItem = null;

		final InventoryItem[] fetchedItemArray = new InventoryItem[]{null}; 
		EventObserver<ItemReceivedEventArgs> callback =
				new EventObserver<ItemReceivedEventArgs>()
				{

			@Override
			public void handleEvent(Observable o, ItemReceivedEventArgs e) {
				if (e.getItem().UUID.equals(itemID))
				{
					fetchedItemArray[0] = e.getItem();
					fetchEvent.set();
				}
			}						
				};
				fetchedItem = fetchedItemArray[0];

				onItemReceived.addObserver(callback);
				RequestFetchInventory(itemID, ownerID);

				fetchEvent.waitOne(timeoutMS);
				onItemReceived.deleteObserver(callback);

				return fetchedItem;
	}

	/// <summary>
	/// Request A single inventory item
	/// </summary>
	/// <param name="itemID">The items <seealso cref="OpenMetaverse.UUID"/></param>
	/// <param name="ownerID">The item Owners <seealso cref="OpenMetaverse.UUID"/></param>
	/// <seealso cref="InventoryManager.OnItemReceived"/>
	public void RequestFetchInventory(UUID itemID, UUID ownerID)
	{
		FetchInventoryPacket fetch = new FetchInventoryPacket();
		fetch.AgentData = new FetchInventoryPacket.AgentDataBlock();
		fetch.AgentData.AgentID = Client.self.getAgentID();
		fetch.AgentData.SessionID = Client.self.getSessionID();

		fetch.InventoryData = new FetchInventoryPacket.InventoryDataBlock[1];
		fetch.InventoryData[0] = new FetchInventoryPacket.InventoryDataBlock();
		fetch.InventoryData[0].ItemID = itemID;
		fetch.InventoryData[0].OwnerID = ownerID;

		Client.network.SendPacket(fetch);
	}
	//
	/// <summary>
	/// Request inventory items
	/// </summary>
	/// <param name="itemIDs">Inventory items to request</param>
	/// <param name="ownerIDs">Owners of the inventory items</param>
	/// <seealso cref="InventoryManager.OnItemReceived"/>
	public void RequestFetchInventory(List<UUID> itemIDs, List<UUID> ownerIDs)
	{
		if (itemIDs.size() != ownerIDs.size())
			throw new IllegalArgumentException("itemIDs and ownerIDs must contain the same number of entries");

		FetchInventoryPacket fetch = new FetchInventoryPacket();
		fetch.AgentData = new FetchInventoryPacket.AgentDataBlock();
		fetch.AgentData.AgentID = Client.self.getAgentID();
		fetch.AgentData.SessionID = Client.self.getSessionID();

		fetch.InventoryData = new FetchInventoryPacket.InventoryDataBlock[itemIDs.size()];
		for (int i = 0; i < itemIDs.size(); i++)
		{
			fetch.InventoryData[i] = new FetchInventoryPacket.InventoryDataBlock();
			fetch.InventoryData[i].ItemID = itemIDs.get(i);
			fetch.InventoryData[i].OwnerID = ownerIDs.get(i);
		}

		Client.network.SendPacket(fetch);
	}

	/// <summary>
	/// Get contents of a folder
	/// </summary>
	/// <param name="folder">The <seealso cref="UUID"/> of the folder to search</param>
	/// <param name="owner">The <seealso cref="UUID"/> of the folders owner</param>
	/// <param name="folders">true to retrieve folders</param>
	/// <param name="items">true to retrieve items</param>
	/// <param name="order">sort order to return results in</param>
	/// <param name="timeoutMS">a integer representing the number of milliseconds to wait for results</param>
	/// <returns>A list of inventory items matching search criteria within folder</returns>
	/// <seealso cref="InventoryManager.RequestFolderContents"/>
	/// <remarks>InventoryFolder.DescendentCount will only be accurate if both folders and items are
	/// requested</remarks>
	public List<InventoryBase> FolderContents(final UUID folder, UUID owner, boolean folders, boolean items,
			InventorySortOrder order, int timeoutMS) throws InterruptedException, InventoryException
			{
		List<InventoryBase> objects = null;
		final AutoResetEvent fetchEvent = new AutoResetEvent(false);

		EventObserver<FolderUpdatedEventArgs> callback = new EventObserver<FolderUpdatedEventArgs>() {  
			@Override
			public void handleEvent(Observable o, FolderUpdatedEventArgs e) 
			{
				if (e.getFolderID().equals(folder) 	&& _Store.get(folder) instanceof InventoryFolder)
				{
					try{
						// InventoryDescendentsHandler only stores DescendendCount if both folders and items are fetched.
						if (_Store.GetContents(folder).size() >= ((InventoryFolder)_Store.get(folder)).DescendentCount)
						{
							fetchEvent.set();
						}
					}
					catch(InventoryException ex)
					{ JLogger.error("Error in Inventory " + Utils.getExceptionStackTraceAsString(ex));}
				}
				else
				{
					fetchEvent.set();
				}
			}};

			onFolderUpdated.addObserver(callback);

			RequestFolderContents(folder, owner, folders, items, order);
			if (fetchEvent.waitOne(timeoutMS))
				objects = _Store.GetContents(folder);

			onFolderUpdated.deleteObserver(callback);

			return objects;
			}

	/// <summary>
	/// Request the contents of an inventory folder
	/// </summary>
	/// <param name="folder">The folder to search</param>
	/// <param name="owner">The folder owners <seealso cref="UUID"/></param>
	/// <param name="folders">true to return <seealso cref="InventoryManager.InventoryFolder"/>s contained in folder</param>
	/// <param name="items">true to return <seealso cref="InventoryManager.InventoryItem"/>s containd in folder</param>
	/// <param name="order">the sort order to return items in</param>
	/// <seealso cref="InventoryManager.FolderContents"/>
	public void RequestFolderContents(UUID folder, UUID owner, boolean folders, boolean items,
			InventorySortOrder order)
	{
		FetchInventoryDescendentsPacket fetch = new FetchInventoryDescendentsPacket();
		fetch.AgentData.AgentID = Client.self.getAgentID();
		fetch.AgentData.SessionID = Client.self.getSessionID();

		fetch.InventoryData.FetchFolders = folders;
		fetch.InventoryData.FetchItems = items;
		fetch.InventoryData.FolderID = folder;
		fetch.InventoryData.OwnerID = owner;
		fetch.InventoryData.SortOrder = (int)order.getIndex();

		Client.network.SendPacket(fetch);
	}

	/// <summary>
	/// Request the contents of an inventory folder using HTTP capabilities
	/// </summary>
	/// <param name="folderID">The folder to search</param>
	/// <param name="ownerID">The folder owners <seealso cref="UUID"/></param>
	/// <param name="fetchFolders">true to return <seealso cref="InventoryManager.InventoryFolder"/>s contained in folder</param>
	/// <param name="fetchItems">true to return <seealso cref="InventoryManager.InventoryItem"/>s containd in folder</param>
	/// <param name="order">the sort order to return items in</param>
	/// <seealso cref="InventoryManager.FolderContents"/>
	public void RequestFolderContentsCap(final UUID folderID, UUID ownerID, boolean fetchFolders, boolean fetchItems,
			InventorySortOrder order)
	{
		URI url = null;

		if (Client.network.getCurrentSim().Caps == null ||
				null == (url = Client.network.getCurrentSim().Caps.CapabilityURI("FetchInventoryDescendents2")))
		{
			JLogger.warn("FetchInventoryDescendents2 capability not available in the current sim");
			onFolderUpdated.raiseEvent(new FolderUpdatedEventArgs(folderID, false));
			return;
		}

		try
		{
			CapsHttpClient request = new CapsHttpClient(url);
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					CapsHttpClient client = arg.getClient();
					OSD result = arg.getResult();
					Exception error = arg.getError();

					try
					{
						if (error != null) throw error;

						OSDArray fetchedFolders = (OSDArray)(((OSDMap)result).get("folders"));
						for (int fetchedFolderNr = 0; fetchedFolderNr < fetchedFolders.count(); fetchedFolderNr++)
						{
							OSDMap res = (OSDMap)fetchedFolders.get(fetchedFolderNr);
							InventoryFolder fetchedFolder = null;

							if (_Store.contains(res.get("folder_id").asUUID())
									&& _Store.get(res.get("folder_id").asUUID()) instanceof InventoryFolder)
							{
								fetchedFolder = (InventoryFolder)_Store.get(res.get("folder_id").asUUID());
							}
							else
							{
								fetchedFolder = new InventoryFolder(res.get("folder_id").asUUID());
								_Store.put(res.get("folder_id").asUUID(), fetchedFolder);
							}
							fetchedFolder.DescendentCount = res.get("descendents").asInteger();
							fetchedFolder.Version = res.get("version").asInteger();
							fetchedFolder.OwnerID = res.get("owner_id").asUUID();
							_Store.GetNodeFor(fetchedFolder.UUID).setNeedsUpdate(false);

							// Do we have any descendants
							if (fetchedFolder.DescendentCount > 0)
							{
								// Fetch descendent folders
								if (res.get("categories") instanceof OSDArray)
								{
									OSDArray folders = (OSDArray)res.get("categories");
									for (int i = 0; i < folders.count(); i++)
									{
										OSDMap descFolder = (OSDMap)folders.get(i);
										InventoryFolder folder;
										if (!_Store.contains(descFolder.get("category_id").asUUID()))
										{
											folder = new InventoryFolder(descFolder.get("category_id").asUUID());
											folder.ParentUUID = descFolder.get("parent_id").asUUID();
											_Store.put((descFolder.get("category_id")).asUUID(), folder);
										}
										else
										{
											folder = (InventoryFolder)_Store.get(descFolder.get("category_id").asUUID());
										}

										folder.OwnerID = descFolder.get("agent_id").asUUID();
										folder.ParentUUID = descFolder.get("parent_id").asUUID();
										folder.Name = descFolder.get("name").asString();
										folder.Version = descFolder.get("version").asInteger();
										AssetType a;

										folder.PreferredType = AssetType.get((byte)descFolder.get("type_default").asInteger());
									}

									// Fetch descendent items
									OSDArray items = (OSDArray)res.get("items");
									for (int i = 0; i < items.count(); i++)
									{
										OSDMap descItem = (OSDMap)items.get(i);
										InventoryType a;
										InventoryType type = InventoryType.get((byte)descItem.get("inv_type").asInteger());
										if (type == InventoryType.Texture && (AssetType.get((byte)descItem.get("type").asInteger()) == AssetType.Object))
										{
											type = InventoryType.Attachment;
										}
										InventoryItem item = CreateInventoryItem(type, descItem.get("item_id").asUUID());

										item.ParentUUID = descItem.get("parent_id").asUUID();
										item.Name = descItem.get("name").asString();
										item.Description = descItem.get("desc").asString();
										item.OwnerID = descItem.get("agent_id").asUUID();
										item.AssetUUID = descItem.get("asset_id").asUUID();
										item.AssetType = AssetType.get((byte)descItem.get("type").asInteger());
										item.CreationDate =  Utils.unixTimeToDate(descItem.get("created_at").asLong());
										item.Flags = descItem.get("flags").asLong();

										OSDMap perms = (OSDMap)descItem.get("permissions");
										item.CreatorID = perms.get("creator_id").asUUID();
										item.LastOwnerID = perms.get("last_owner_id").asUUID();
										item.Permissions = new Permissions(perms.get("base_mask").asLong(), perms.get("everyone_mask").asLong(), perms.get("group_mask").asLong(), perms.get("next_owner_mask").asLong(), perms.get("owner_mask").asLong());
										item.GroupOwned = perms.get("is_owner_group").asBoolean();
										item.GroupID = perms.get("group_id").asUUID();

										OSDMap sale = (OSDMap)descItem.get("sale_info");
										item.SalePrice = sale.get("sale_price").asInteger();
										item.SaleType = SaleType.get((byte)sale.get("sale_type").asInteger());

										_Store.put(item.UUID, item);
									}
								}
							}

							onFolderUpdated.raiseEvent(new FolderUpdatedEventArgs(res.get("folder_id").asUUID(), true));
						}
					}
					catch (Exception exc)
					{
						JLogger.warn(String.format("Failed to fetch inventory descendants for folder id %s: %s\n%s", folderID.toString(), exc.getMessage(), Utils.getExceptionStackTraceAsString(exc)));
						onFolderUpdated.raiseEvent(new FolderUpdatedEventArgs(folderID, false));
						return;
					}

				}
					}
					);
			// Construct request
			OSDMap requestedFolder = new OSDMap(1);
			requestedFolder.put("folder_id", OSD.FromUUID(folderID));
			requestedFolder.put("owner_id", OSD.FromUUID(ownerID));
			requestedFolder.put("fetch_folders", OSD.FromBoolean(fetchFolders));
			requestedFolder.put("fetch_items", OSD.FromBoolean(fetchItems));
			requestedFolder.put("sort_order", OSD.FromInteger((int)order.getIndex()));

			OSDArray requestedFolders = new OSDArray(1);
			requestedFolders.add(requestedFolder);
			OSDMap req = new OSDMap(1);
			req.put("folders", requestedFolders);

			request.BeginGetResponse(req, OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		catch (Exception ex)
		{
			JLogger.warn(String.format("Failed to fetch inventory descendants for folder id %s: %s\n%s", folderID, ex.getMessage(), Utils.getExceptionStackTraceAsString(ex)));
			onFolderUpdated.raiseEvent(new FolderUpdatedEventArgs(folderID, false));
			return;
		}
	}

	//endregion Fetch

	//region Find

	/// <summary>
	/// Returns the UUID of the folder (category) that defaults to
	/// containing 'type'. The folder is not necessarily only for that
	/// type
	/// </summary>
	/// <remarks>This will return the root folder if one does not exist</remarks>
	/// <param name="type"></param>
	/// <returns>The UUID of the desired folder if found, the UUID of the RootFolder
	/// if not found, or UUID.Zero on failure</returns>
	public UUID FindFolderForType(AssetType type) throws InventoryException
	{
		if (_Store == null)
		{
			JLogger.error("Inventory is null, FindFolderForType() lookup cannot continue");
			return UUID.Zero;
		}

		// Folders go in the root
		if (type == AssetType.Folder)
			return _Store.getRootFolder().UUID;

		// Loop through each top-level directory and check if PreferredType
		// matches the requested type
		List<InventoryBase> contents = _Store.GetContents(_Store.getRootFolder().UUID);
		for(InventoryBase inv : contents)
		{
			if (inv instanceof InventoryFolder)
			{
				InventoryFolder folder = (InventoryFolder)inv;

				if (folder.PreferredType == type)
					return folder.UUID;
			}
		}

		// No match found, return Root Folder ID
		return _Store.getRootFolder().UUID;
	}

	/// <summary>
	/// Find an object in inventory using a specific path to search
	/// </summary>
	/// <param name="baseFolder">The folder to begin the search in</param>
	/// <param name="inventoryOwner">The object owners <seealso cref="UUID"/></param>
	/// <param name="path">AString path to search</param>
	/// <param name="timeoutMS">milliseconds to wait for a reply</param>
	/// <returns>Found items <seealso cref="UUID"/> or <seealso cref="UUID.Zero"/> if 
	/// timeout occurs or item is not found</returns>
	public UUID FindObjectByPath(UUID baseFolder, UUID inventoryOwner, final String path, int timeoutMS) throws InterruptedException
	{
		final AutoResetEvent findEvent = new AutoResetEvent(false);
		final UUID[] foundItem = new UUID[] {UUID.Zero};

		EventObserver<FindObjectByPathReplyEventArgs> callback = new EventObserver<FindObjectByPathReplyEventArgs>()
				{

			@Override
			public void handleEvent(Observable o,
					FindObjectByPathReplyEventArgs e) {
				if (e.getPath().equals(path))
				{
					foundItem[0] = e.getInventoryObjectID();
					findEvent.set();
				}
			}};
			onFindObjectByPathReply.addObserver(callback);

			RequestFindObjectByPath(baseFolder, inventoryOwner, path);
			findEvent.waitOne(timeoutMS);

			onFindObjectByPathReply.deleteObserver(callback);

			return foundItem[0];

	}

	/// <summary>
	/// Find inventory items by path
	/// </summary>
	/// <param name="baseFolder">The folder to begin the search in</param>
	/// <param name="inventoryOwner">The object owners <seealso cref="UUID"/></param>
	/// <param name="path">A String path to search, folders/objects separated by a '/'</param>
	/// <remarks>Results are sent to the <seealso cref="InventoryManager.OnFindObjectByPath"/> event</remarks>
	public void RequestFindObjectByPath(UUID baseFolder, UUID inventoryOwner, String path)
	{
		if (path == null || path.length() == 0)
			throw new IllegalArgumentException("Empty path is not supported");

		// Store this search
		InventorySearch search = new InventorySearch();
		search.Folder = baseFolder;
		search.Owner = inventoryOwner;
		search.Path = path.split("/");
		search.Level = 0;
		synchronized(_Searches){ _Searches.add(search);}

		// Start the search
		RequestFolderContents(baseFolder, inventoryOwner, true, true, InventorySortOrder.ByName);
	}

	/// <summary>
	/// Search inventory Store object for an item or folder
	/// </summary>
	/// <param name="baseFolder">The folder to begin the search in</param>
	/// <param name="path">An array which creates a path to search</param>
	/// <param name="level">Number of levels below baseFolder to conduct searches</param>
	/// <param name="firstOnly">if True, will stop searching after first match is found</param>
	/// <returns>A list of inventory items found</returns>
	public List<InventoryBase> LocalFind(UUID baseFolder, String[] path, int level, boolean firstOnly) throws InventoryException
	{
		List<InventoryBase> objects = new ArrayList<InventoryBase>();
		//List<InventoryFolder> folders = new ArrayList<InventoryFolder>();
		List<InventoryBase> contents = _Store.GetContents(baseFolder);

		for(InventoryBase inv : contents)
		{
			if (inv.Name.compareTo(path[level]) == 0)
			{
				if (level == path.length - 1)
				{
					objects.add(inv);
					if (firstOnly) return objects;
				}
				else if (inv instanceof InventoryFolder)
					objects.addAll(LocalFind(inv.UUID, path, level + 1, firstOnly));
			}
		}

		return objects;
	}

	//endregion Find

	//region Move/Rename

	/// <summary>
	/// Move an inventory item or folder to a new location
	/// </summary>
	/// <param name="item">The <seealso cref="T:InventoryBase"/> item or folder to move</param>
	/// <param name="newParent">The <seealso cref="T:InventoryFolder"/> to move item or folder to</param>
	public void Move(InventoryBase item, InventoryFolder newParent)
	{
		if (item instanceof InventoryFolder)
			MoveFolder(item.UUID, newParent.UUID);
		else
			MoveItem(item.UUID, newParent.UUID);
	}

	/// <summary>
	/// Move an inventory item or folder to a new location and change its name
	/// </summary>
	/// <param name="item">The <seealso cref="T:InventoryBase"/> item or folder to move</param>
	/// <param name="newParent">The <seealso cref="T:InventoryFolder"/> to move item or folder to</param>
	/// <param name="newName">The name to change the item or folder to</param>
	public void Move(InventoryBase item, InventoryFolder newParent, String newName)
	{
		if (item instanceof InventoryFolder)
			MoveFolder(item.UUID, newParent.UUID, newName);
		else
			MoveItem(item.UUID, newParent.UUID, newName);
	}

	/// <summary>
	/// Move and rename a folder
	/// </summary>
	/// <param name="folderID">The source folders <seealso cref="UUID"/></param>
	/// <param name="newparentID">The destination folders <seealso cref="UUID"/></param>
	/// <param name="newName">The name to change the folder to</param>
	public void MoveFolder(UUID folderID, UUID newparentID, String newName)
	{
		UpdateFolderProperties(folderID, newparentID, newName, AssetType.Unknown);
	}

	/// <summary>
	/// Update folder properties
	/// </summary>
	/// <param name="folderID"><seealso cref="UUID"/> of the folder to update</param>
	/// <param name="parentID">Sets folder's parent to <seealso cref="UUID"/></param>
	/// <param name="name">Folder name</param>
	/// <param name="type">Folder type</param>
	public void UpdateFolderProperties(UUID folderID, UUID parentID, String name, AssetType type)
	{
		synchronized(_Store)
		{
			if (_Store.contains(folderID))
			{
				InventoryFolder inv = (InventoryFolder)_Store.get(folderID);
				inv.Name = name;
				inv.ParentUUID = parentID;
				inv.PreferredType = type;
				_Store.UpdateNodeFor(inv);
			}
		}

		UpdateInventoryFolderPacket invFolder = new UpdateInventoryFolderPacket();
		invFolder.AgentData.AgentID = Client.self.getAgentID();
		invFolder.AgentData.SessionID = Client.self.getSessionID();
		invFolder.FolderData = new UpdateInventoryFolderPacket.FolderDataBlock[1];
		invFolder.FolderData[0] = new UpdateInventoryFolderPacket.FolderDataBlock();
		invFolder.FolderData[0].FolderID = folderID;
		invFolder.FolderData[0].ParentID = parentID;
		invFolder.FolderData[0].Name = Utils.stringToBytesWithTrailingNullByte(name);
		invFolder.FolderData[0].Type = (byte)type.getIndex();

		Client.network.SendPacket(invFolder);
	}

	/// <summary>
	/// Move a folder
	/// </summary>
	/// <param name="folderID">The source folders <seealso cref="UUID"/></param>
	/// <param name="newParentID">The destination folders <seealso cref="UUID"/></param>
	public void MoveFolder(UUID folderID, UUID newParentID)
	{
		synchronized(_Store)
		{
			if (_Store.contains(folderID))
			{
				InventoryBase inv = _Store.get(folderID);
				inv.ParentUUID = newParentID;
				_Store.UpdateNodeFor(inv);
			}
		}

		MoveInventoryFolderPacket move = new MoveInventoryFolderPacket();
		move.AgentData.AgentID = Client.self.getAgentID();
		move.AgentData.SessionID = Client.self.getSessionID();
		move.AgentData.Stamp = false; //FIXME: ??

		move.InventoryData = new MoveInventoryFolderPacket.InventoryDataBlock[1];
		move.InventoryData[0] = new MoveInventoryFolderPacket.InventoryDataBlock();
		move.InventoryData[0].FolderID = folderID;
		move.InventoryData[0].ParentID = newParentID;

		Client.network.SendPacket(move);
	}

	/// <summary>
	/// Move multiple folders, the keys in the Dictionary parameter,
	/// to a new parents, the value of that folder's key.
	/// </summary>
	/// <param name="foldersNewParents">A Dictionary containing the 
	/// <seealso cref="UUID"/> of the source as the key, and the 
	/// <seealso cref="UUID"/> of the destination as the value</param>
	public void MoveFolders(Map<UUID, UUID> foldersNewParents)
	{
		// FIXME: Use two List<UUID> to stay consistent

		synchronized(_Store)
		{
			for(Entry<UUID, UUID> entry : foldersNewParents.entrySet())
			{
				if (_Store.contains(entry.getKey()))
				{
					InventoryBase inv = _Store.get(entry.getKey());
					inv.ParentUUID = entry.getValue();
					_Store.UpdateNodeFor(inv);
				}
			}
		}

		//TODO: Test if this truly supports multiple-folder move
		MoveInventoryFolderPacket move = new MoveInventoryFolderPacket();
		move.AgentData.AgentID = Client.self.getAgentID();
		move.AgentData.SessionID = Client.self.getSessionID();
		move.AgentData.Stamp = false; //FIXME: ??

		move.InventoryData = new MoveInventoryFolderPacket.InventoryDataBlock[foldersNewParents.size()];

		int index = 0;
		for(Entry<UUID, UUID> folder : foldersNewParents.entrySet())
		{
			MoveInventoryFolderPacket.InventoryDataBlock block = new MoveInventoryFolderPacket.InventoryDataBlock();
			block.FolderID = folder.getKey();
			block.ParentID = folder.getValue();
			move.InventoryData[index++] = block;
		}

		Client.network.SendPacket(move);
	}


	/// <summary>
	/// Move an inventory item to a new folder
	/// </summary>
	/// <param name="itemID">The <seealso cref="UUID"/> of the source item to move</param>
	/// <param name="folderID">The <seealso cref="UUID"/> of the destination folder</param>
	public void MoveItem(UUID itemID, UUID folderID)
	{
		MoveItem(itemID, folderID, "");
	}

	/// <summary>
	/// Move and rename an inventory item
	/// </summary>
	/// <param name="itemID">The <seealso cref="UUID"/> of the source item to move</param>
	/// <param name="folderID">The <seealso cref="UUID"/> of the destination folder</param>
	/// <param name="newName">The name to change the folder to</param>
	public void MoveItem(UUID itemID, UUID folderID, String newName)
	{
		synchronized(_Store)
		{
			if (_Store.contains(itemID))
			{
				InventoryBase inv = _Store.get(itemID);
				inv.Name = newName;
				inv.ParentUUID = folderID;
				_Store.UpdateNodeFor(inv);
			}
		}

		MoveInventoryItemPacket move = new MoveInventoryItemPacket();
		move.AgentData.AgentID = Client.self.getAgentID();
		move.AgentData.SessionID = Client.self.getSessionID();
		move.AgentData.Stamp = false; //FIXME: ??

		move.InventoryData = new MoveInventoryItemPacket.InventoryDataBlock[1];
		move.InventoryData[0] = new MoveInventoryItemPacket.InventoryDataBlock();
		move.InventoryData[0].ItemID = itemID;
		move.InventoryData[0].FolderID = folderID;
		move.InventoryData[0].NewName = Utils.stringToBytesWithTrailingNullByte(newName);

		Client.network.SendPacket(move);
	}

	/// <summary>
	/// Move multiple inventory items to new locations
	/// </summary>
	/// <param name="itemsNewParents">A Dictionary containing the 
	/// <seealso cref="UUID"/> of the source item as the key, and the 
	/// <seealso cref="UUID"/> of the destination folder as the value</param>
	public void MoveItems(Map<UUID, UUID> itemsNewParents)
	{
		synchronized(_Store)
		{
			for(Entry<UUID, UUID> entry : itemsNewParents.entrySet())
			{
				if (_Store.contains(entry.getKey()))
				{
					InventoryBase inv = _Store.get(entry.getKey());
					inv.ParentUUID = entry.getValue();
					_Store.UpdateNodeFor(inv);
				}
			}
		}

		MoveInventoryItemPacket move = new MoveInventoryItemPacket();
		move.AgentData.AgentID = Client.self.getAgentID();
		move.AgentData.SessionID = Client.self.getSessionID();
		move.AgentData.Stamp = false; //FIXME: ??

		move.InventoryData = new MoveInventoryItemPacket.InventoryDataBlock[itemsNewParents.size()];

		int index = 0;
		for(Entry<UUID, UUID> entry : itemsNewParents.entrySet())
		{
			MoveInventoryItemPacket.InventoryDataBlock block = new MoveInventoryItemPacket.InventoryDataBlock();
			block.ItemID = entry.getKey();
			block.FolderID = entry.getValue();
			block.NewName = Utils.EmptyBytes;
			move.InventoryData[index++] = block;
		}

		Client.network.SendPacket(move);
	}

	//endregion Move

	//region Remove

	/// <summary>
	/// Remove descendants of a folder
	/// </summary>
	/// <param name="folder">The <seealso cref="UUID"/> of the folder</param>
	public void RemoveDescendants(UUID folder) throws InventoryException
	{
		PurgeInventoryDescendentsPacket purge = new PurgeInventoryDescendentsPacket();
		purge.AgentData.AgentID = Client.self.getAgentID();
		purge.AgentData.SessionID = Client.self.getSessionID();
		purge.InventoryData.FolderID = folder;
		Client.network.SendPacket(purge);

		// Update our local copy
		synchronized(_Store)
		{
			if (_Store.contains(folder))
			{
				List<InventoryBase> contents = _Store.GetContents(folder);
				for(InventoryBase obj : contents)
				{
					_Store.RemoveNodeFor(obj);
				}
			}
		}
	}

	/// <summary>
	/// Remove a single item from inventory
	/// </summary>
	/// <param name="item">The <seealso cref="UUID"/> of the inventory item to remove</param>
	public void RemoveItem(UUID item)
	{
		List<UUID> items = new ArrayList<UUID>(1);
		items.add(item);

		Remove(items, null);
	}

	/// <summary>
	/// Remove a folder from inventory
	/// </summary>
	/// <param name="folder">The <seealso cref="UUID"/> of the folder to remove</param>
	public void RemoveFolder(UUID folder)
	{
		List<UUID> folders = new ArrayList<UUID>(1);
		folders.add(folder);

		Remove(null, folders);
	}

	/// <summary>
	/// Remove multiple items or folders from inventory
	/// </summary>
	/// <param name="items">A List containing the <seealso cref="UUID"/>s of items to remove</param>
	/// <param name="folders">A List containing the <seealso cref="UUID"/>s of the folders to remove</param>
	public void Remove(List<UUID> items, List<UUID> folders)
	{
		if ((items == null || items.size() == 0) && (folders == null || folders.size() == 0))
			return;

		RemoveInventoryObjectsPacket rem = new RemoveInventoryObjectsPacket();
		rem.AgentData.AgentID = Client.self.getAgentID();
		rem.AgentData.SessionID = Client.self.getSessionID();

		if (items == null || items.size() == 0)
		{
			// To indicate that we want no items removed:
			rem.ItemData = new RemoveInventoryObjectsPacket.ItemDataBlock[1];
			rem.ItemData[0] = new RemoveInventoryObjectsPacket.ItemDataBlock();
			rem.ItemData[0].ItemID = UUID.Zero;
		}
		else
		{
			synchronized(_Store)
			{
				rem.ItemData = new RemoveInventoryObjectsPacket.ItemDataBlock[items.size()];
				for (int i = 0; i < items.size(); i++)
				{
					rem.ItemData[i] = new RemoveInventoryObjectsPacket.ItemDataBlock();
					rem.ItemData[i].ItemID = items.get(i);

					// Update local copy
					if (_Store.contains(items.get(i)))
						_Store.RemoveNodeFor(_Store.get(items.get(i)));
				}
			}
		}

		if (folders == null || folders.size() == 0)
		{
			// To indicate we want no folders removed:
			rem.FolderData = new RemoveInventoryObjectsPacket.FolderDataBlock[1];
			rem.FolderData[0] = new RemoveInventoryObjectsPacket.FolderDataBlock();
			rem.FolderData[0].FolderID = UUID.Zero;
		}
		else
		{
			synchronized(_Store)
			{
				rem.FolderData = new RemoveInventoryObjectsPacket.FolderDataBlock[folders.size()];
				for (int i = 0; i < folders.size(); i++)
				{
					rem.FolderData[i] = new RemoveInventoryObjectsPacket.FolderDataBlock();
					rem.FolderData[i].FolderID = folders.get(i);

					// Update local copy
					if (_Store.contains(folders.get(i)))
						_Store.RemoveNodeFor(_Store.get(folders.get(i)));
				}
			}
		}
		Client.network.SendPacket(rem);
	}

	/// <summary>
	/// Empty the Lost and Found folder
	/// </summary>
	public void EmptyLostAndFound() throws InventoryException
	{
		EmptySystemFolder(AssetType.LostAndFoundFolder);
	}

	/// <summary>
	/// Empty the Trash folder
	/// </summary>
	public void EmptyTrash() throws InventoryException
	{
		EmptySystemFolder(AssetType.TrashFolder);
	}

	private void EmptySystemFolder(AssetType folderType) throws InventoryException
	{
		List<InventoryBase> items = _Store.GetContents(_Store.getRootFolder());

		UUID folderKey = UUID.Zero;
		for(InventoryBase item : items)
		{
			if (((InventoryFolder)item) != null)
			{
				InventoryFolder folder =(InventoryFolder)item;
				if (folder.PreferredType == folderType)
				{
					folderKey = folder.UUID;
					break;
				}
			}
		}
		items = _Store.GetContents(folderKey);
		List<UUID> remItems = new ArrayList<UUID>();
		List<UUID> remFolders = new ArrayList<UUID>();
		for(InventoryBase item : items)
		{
			if (((InventoryFolder)item) != null)
			{
				remFolders.add(item.UUID);
			}
			else
			{
				remItems.add(item.UUID);
			}
		}
		Remove(remItems, remFolders);
	}
	//endregion Remove

	//region Create

	/// <summary>
	/// 
	/// </summary>
	/// <param name="parentFolder"></param>
	/// <param name="name"></param>
	/// <param name="description"></param>
	/// <param name="type"></param>
	/// <param name="assetTransactionID">Proper use is to upload the inventory's asset first, then provide the Asset's TransactionID here.</param>
	/// <param name="invType"></param>
	/// <param name="nextOwnerMask"></param>
	/// <param name="callback"></param>
	public void RequestCreateItem(UUID parentFolder, String name, String description, AssetType type, UUID assetTransactionID,
			InventoryType invType, PermissionMask nextOwnerMask, EventObserver<ItemCreatedCallbackArg> callback)
	{
		// Even though WearableType 0 is Shape, in this context it is treated as NOT_WEARABLE
		RequestCreateItem(parentFolder, name, description, type, assetTransactionID, invType, Enums.WearableType.get((byte)0), nextOwnerMask,
				callback);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="parentFolder"></param>
	/// <param name="name"></param>
	/// <param name="description"></param>
	/// <param name="type"></param>
	/// <param name="assetTransactionID">Proper use is to upload the inventory's asset first, then provide the Asset's TransactionID here.</param>
	/// <param name="invType"></param>
	/// <param name="wearableType"></param>
	/// <param name="nextOwnerMask"></param>
	/// <param name="callback"></param>
	public void RequestCreateItem(UUID parentFolder, String name, String description, AssetType type, UUID assetTransactionID,
			InventoryType invType, WearableType wearableType, PermissionMask nextOwnerMask, EventObserver<ItemCreatedCallbackArg> callback)
	{
		CreateInventoryItemPacket create = new CreateInventoryItemPacket();
		create.AgentData.AgentID = Client.self.getAgentID();
		create.AgentData.SessionID = Client.self.getSessionID();

		create.InventoryBlock.CallbackID = RegisterItemCreatedCallback(callback);
		create.InventoryBlock.FolderID = parentFolder;
		create.InventoryBlock.TransactionID = assetTransactionID;
		create.InventoryBlock.NextOwnerMask = nextOwnerMask.getIndex();
		create.InventoryBlock.Type = (byte)type.getIndex();
		create.InventoryBlock.InvType = (byte)invType.getIndex();
		create.InventoryBlock.WearableType = (byte)wearableType.getIndex();
		create.InventoryBlock.Name = Utils.stringToBytesWithTrailingNullByte(name);
		create.InventoryBlock.Description = Utils.stringToBytesWithTrailingNullByte(description);

		Client.network.SendPacket(create);
	}

	/// <summary>
	/// Creates a new inventory folder
	/// </summary>
	/// <param name="parentID">ID of the folder to put this folder in</param>
	/// <param name="name">Name of the folder to create</param>
	/// <returns>The UUID of the newly created folder</returns>
	public UUID CreateFolder(UUID parentID, String name)
	{
		return CreateFolder(parentID, name, AssetType.Unknown);
	}

	/// <summary>
	/// Creates a new inventory folder
	/// </summary>
	/// <param name="parentID">ID of the folder to put this folder in</param>
	/// <param name="name">Name of the folder to create</param>
	/// <param name="preferredType">Sets this folder as the default folder
	/// for new assets of the specified type. Use <code>AssetType.Unknown</code>
	/// to create a normal folder, otherwise it will likely create a
	/// duplicate of an existing folder type</param>
	/// <returns>The UUID of the newly created folder</returns>
	/// <remarks>If you specify a preferred type of <code>AsseType.Folder</code>
	/// it will create a new root folder which may likely cause all sorts
	/// of strange problems</remarks>
	public UUID CreateFolder(UUID parentID, String name, AssetType preferredType)
	{
		UUID id = UUID.Random();

		// Assign a folder name if one is not already set
		if (Utils.isNullOrEmpty(name))
		{
			if (preferredType.getIndex() >= AssetType.Texture.getIndex() && preferredType.getIndex() <= AssetType.Gesture.getIndex())
			{
				name = _NewFolderNames[(int)preferredType.getIndex()];
			}
			else
			{
				name = "New Folder";
			}
		}

		// Create the new folder locally
		InventoryFolder newFolder = new InventoryFolder(id);
		newFolder.Version = 1;
		newFolder.DescendentCount = 0;
		newFolder.ParentUUID = parentID;
		newFolder.PreferredType = preferredType;
		newFolder.Name = name;
		newFolder.OwnerID = Client.self.getAgentID();

		// Update the local store
		//			try { _
		_Store.put(newFolder.UUID, newFolder); 
		//			}
		//			catch (InventoryException ie) { JLogger.warn(Utils.getExceptionStackTraceAsString(ie)); }

		// Create the create folder packet and send it
		CreateInventoryFolderPacket create = new CreateInventoryFolderPacket();
		create.AgentData.AgentID = Client.self.getAgentID();
		create.AgentData.SessionID = Client.self.getSessionID();

		create.FolderData.FolderID = id;
		create.FolderData.ParentID = parentID;
		create.FolderData.Type = (byte)preferredType.getIndex();
		create.FolderData.Name = Utils.stringToBytesWithTrailingNullByte(name);

		Client.network.SendPacket(create);

		return id;
	}

	/// <summary>
	/// Create an inventory item and upload asset data
	/// </summary>
	/// <param name="data">Asset data</param>
	/// <param name="name">Inventory item name</param>
	/// <param name="description">Inventory item description</param>
	/// <param name="assetType">Asset type</param>
	/// <param name="invType">Inventory type</param>
	/// <param name="folderID">Put newly created inventory in this folder</param>
	/// <param name="callback">Delegate that will receive feedback on success or failure</param>
	public void RequestCreateItemFromAsset(byte[] data, String name, String description, AssetType assetType,
			InventoryType invType, UUID folderID, ItemCreatedFromAssetCallbackArg callback) throws Exception
			{
		Permissions permissions = new Permissions();
		permissions.EveryoneMask = PermissionMask.get(PermissionMask.None.getIndex());
		permissions.GroupMask = PermissionMask.get(PermissionMask.None.getIndex());
		permissions.NextOwnerMask = PermissionMask.get(PermissionMask.All.getIndex());

		RequestCreateItemFromAsset(data, name, description, assetType, invType, folderID, permissions, callback);
			}

	/// <summary>
	/// Create an inventory item and upload asset data
	/// </summary>
	/// <param name="data">Asset data</param>
	/// <param name="name">Inventory item name</param>
	/// <param name="description">Inventory item description</param>
	/// <param name="assetType">Asset type</param>
	/// <param name="invType">Inventory type</param>
	/// <param name="folderID">Put newly created inventory in this folder</param>
	/// <param name="permissions">Permission of the newly created item 
	/// (EveryoneMask, GroupMask, and NextOwnerMask of Permissions struct are supported)</param>
	/// <param name="callback">Delegate that will receive feedback on success or failure</param>
	public void RequestCreateItemFromAsset(byte[] data, String name, String description, AssetType assetType,
			InventoryType invType, UUID folderID, Permissions permissions, ItemCreatedFromAssetCallbackArg callback) throws Exception
			{
		if (Client.network.getCurrentSim() == null || Client.network.getCurrentSim().Caps == null)
			throw new Exception("NewFileAgentInventory capability is not currently available");

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("NewFileAgentInventory");

		if (url != null)
		{
			OSDMap query = new OSDMap();
			query.put("folder_id", OSD.FromUUID(folderID));
			query.put("asset_type", OSD.FromString(Utils.AssetTypeToString(assetType)));
			query.put("inventory_type", OSD.FromString(Utils.InventoryTypeToString(invType)));
			query.put("name", OSD.FromString(name));
			query.put("description", OSD.FromString(description));
			query.put("everyone_mask", OSD.FromInteger((int)PermissionMask.getIndex(permissions.EveryoneMask)));
			query.put("group_mask", OSD.FromInteger((int)PermissionMask.getIndex(permissions.GroupMask)));
			query.put("next_owner_mask", OSD.FromInteger((int)PermissionMask.getIndex(permissions.NextOwnerMask)));
			query.put("expected_upload_cost", OSD.FromInteger(Client.settings.UPLOAD_COST()));

			// Make the request
			CapsHttpClient request = new CapsHttpClient(url);
			//				request.OnComplete += CreateItemFromAssetResponse;
			//				request.UserData = new object[] { callback, data, Client.settings.CAPS_TIMEOUT, query };

			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					try {
						CreateItemFromAssetResponse(arg.getClient(), arg.getResult(), arg.getError());
					} catch (URISyntaxException e) {
						Utils.getExceptionStackTraceAsString(e);
					}
				}
					});
			request.setUserData(new Object[] { new Object[] { callback, data, Client.settings.CAPS_TIMEOUT, query } });
			request.BeginGetResponse(query, OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);				
		}
		else
		{
			throw new Exception("NewFileAgentInventory capability is not currently available");
		}
			}

	/// <summary>
	/// Creates inventory link to another inventory item or folder
	/// </summary>
	/// <param name="folderID">Put newly created link in folder with this UUID</param>
	/// <param name="bse">Inventory item or folder</param>
	/// <param name="callback">Method to call upon creation of the link</param>
	public void CreateLink(UUID folderID, InventoryBase bse, EventObserver<ItemCreatedCallbackArg> callback)
	{
		if (bse instanceof InventoryFolder)
		{
			InventoryFolder folder = (InventoryFolder)bse;
			CreateLink(folderID, folder, callback);
		}
		else if (bse instanceof InventoryItem)
		{
			InventoryItem item = (InventoryItem)bse;
			CreateLink(folderID, item.UUID, item.Name, item.Description, AssetType.Link, item.InventoryType, UUID.Random(), callback);
		}
	}

	/// <summary>
	/// Creates inventory link to another inventory item
	/// </summary>
	/// <param name="folderID">Put newly created link in folder with this UUID</param>
	/// <param name="item">Original inventory item</param>
	/// <param name="callback">Method to call upon creation of the link</param>
	public void CreateLink(UUID folderID, InventoryItem item, EventObserver<ItemCreatedCallbackArg> callback)
	{
		CreateLink(folderID, item.UUID, item.Name, item.Description, AssetType.Link, item.InventoryType, UUID.Random(), callback);
	}

	/// <summary>
	/// Creates inventory link to another inventory folder
	/// </summary>
	/// <param name="folderID">Put newly created link in folder with this UUID</param>
	/// <param name="folder">Original inventory folder</param>
	/// <param name="callback">Method to call upon creation of the link</param>
	public void CreateLink(UUID folderID, InventoryFolder folder, EventObserver<ItemCreatedCallbackArg> callback)
	{
		CreateLink(folderID, folder.UUID, folder.Name, "", AssetType.LinkFolder, InventoryType.Folder, UUID.Random(), callback);
	}

	/// <summary>
	/// Creates inventory link to another inventory item or folder
	/// </summary>
	/// <param name="folderID">Put newly created link in folder with this UUID</param>
	/// <param name="itemID">Original item's UUID</param>
	/// <param name="name">Name</param>
	/// <param name="description">Description</param>
	/// <param name="assetType">Asset Type</param>
	/// <param name="invType">Inventory Type</param>
	/// <param name="transactionID">Transaction UUID</param>
	/// <param name="callback">Method to call upon creation of the link</param>
	public void CreateLink(UUID folderID, UUID itemID, String name, String description, AssetType assetType, InventoryType invType, UUID transactionID, EventObserver<ItemCreatedCallbackArg> callback)
	{
		LinkInventoryItemPacket create = new LinkInventoryItemPacket();
		create.AgentData.AgentID = Client.self.getAgentID();
		create.AgentData.SessionID = Client.self.getSessionID();

		create.InventoryBlock.CallbackID = RegisterItemCreatedCallback(callback);
		create.InventoryBlock.FolderID = folderID;
		create.InventoryBlock.TransactionID = transactionID;
		create.InventoryBlock.OldItemID = itemID;
		create.InventoryBlock.Type = (byte)assetType.getIndex();
		create.InventoryBlock.InvType = (byte)invType.getIndex();
		create.InventoryBlock.Name = Utils.stringToBytesWithTrailingNullByte(name);
		create.InventoryBlock.Description = Utils.stringToBytesWithTrailingNullByte(description);

		Client.network.SendPacket(create);
	}

	//endregion Create

	//region Copy

	/// <summary>
	/// 
	/// </summary>
	/// <param name="item"></param>
	/// <param name="newParent"></param>
	/// <param name="newName"></param>
	/// <param name="callback"></param>
	public void RequestCopyItem(UUID item, UUID newParent, String newName, EventObserver<ItemCopiedCallbackArg> callback)
	{
		RequestCopyItem(item, newParent, newName, Client.self.getAgentID(), callback);
	}


	//		protected EventObservable<ItemCreatedCallbackArg> ItemCreatedCallback = new EventObservable<ItemCreatedCallbackArg>(); 
	//		protected EventObservable<ItemCreatedFromAssetCallbackArg> ItemCreatedFromAssetCallback = new EventObservable<ItemCreatedFromAssetCallbackArg>(); 
	//		protected EventObservable<ItemCopiedCallbackArg> ItemCopiedCallback = new EventObservable<ItemCopiedCallbackArg>(); 

	/// <summary>
	/// 
	/// </summary>
	/// <param name="item"></param>
	/// <param name="newParent"></param>
	/// <param name="newName"></param>
	/// <param name="oldOwnerID"></param>
	/// <param name="callback"></param>
	public void RequestCopyItem(UUID item, UUID newParent, String newName, UUID oldOwnerID,
			EventObserver<ItemCopiedCallbackArg> callback)
	{
		List<UUID> items = new ArrayList<UUID>(1);
		items.add(item);

		List<UUID> folders = new ArrayList<UUID>(1);
		folders.add(newParent);

		List<String> names = new ArrayList<String>(1);
		names.add(newName);

		RequestCopyItems(items, folders, names, oldOwnerID, callback);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="items"></param>
	/// <param name="targetFolders"></param>
	/// <param name="newNames"></param>
	/// <param name="oldOwnerID"></param>
	/// <param name="callback"></param>
	public void RequestCopyItems(List<UUID> items, List<UUID> targetFolders, List<String> newNames,
			UUID oldOwnerID, EventObserver<ItemCopiedCallbackArg> callback)
	{
		if (items.size() != targetFolders.size() || (newNames != null && items.size() != newNames.size()))
			throw new IllegalArgumentException("All list arguments must have an equal number of entries");

		long callbackID = RegisterItemsCopiedCallback(callback);

		CopyInventoryItemPacket copy = new CopyInventoryItemPacket();
		copy.AgentData.AgentID = Client.self.getAgentID();
		copy.AgentData.SessionID = Client.self.getSessionID();

		copy.InventoryData = new CopyInventoryItemPacket.InventoryDataBlock[items.size()];
		for (int i = 0; i < items.size(); ++i)
		{
			copy.InventoryData[i] = new CopyInventoryItemPacket.InventoryDataBlock();
			copy.InventoryData[i].CallbackID = callbackID;
			copy.InventoryData[i].NewFolderID = targetFolders.get(i);
			copy.InventoryData[i].OldAgentID = oldOwnerID;
			copy.InventoryData[i].OldItemID = items.get(i);

			if (newNames != null && !Utils.isNullOrEmpty(newNames.get(i)))
				copy.InventoryData[i].NewName = Utils.stringToBytesWithTrailingNullByte(newNames.get(i));
			else
				copy.InventoryData[i].NewName = Utils.EmptyBytes;
		}

		Client.network.SendPacket(copy);
	}

	/// <summary>
	/// Request a copy of an asset embedded within a notecard
	/// </summary>
	/// <param name="objectID">Usually UUID.Zero for copying an asset from a notecard</param>
	/// <param name="notecardID">UUID of the notecard to request an asset from</param>
	/// <param name="folderID">Target folder for asset to go to in your inventory</param>
	/// <param name="itemID">UUID of the embedded asset</param>
	/// <param name="callback">callback to run when item is copied to inventory</param>
	public void RequestCopyItemFromNotecard(UUID objectID, UUID notecardID, UUID folderID, UUID itemID, EventObserver<ItemCopiedCallbackArg> callback) throws Exception
	{
		_ItemCopiedCallbacks.put(0L, callback); //Notecards always use callback ID 0

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("CopyInventoryFromNotecard");

		if (url != null)
		{
			CopyInventoryFromNotecardMessage message = new CopyInventoryFromNotecardMessage();
			message.CallbackID = 0;
			message.FolderID = folderID;
			message.ItemID = itemID;
			message.NotecardID = notecardID;
			message.ObjectID = objectID;

			CapsHttpClient request = new CapsHttpClient(url);
			request.BeginGetResponse(message.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			CopyInventoryFromNotecardPacket copy = new CopyInventoryFromNotecardPacket();
			copy.AgentData.AgentID = Client.self.getAgentID();
			copy.AgentData.SessionID = Client.self.getSessionID();

			copy.NotecardData.ObjectID = objectID;
			copy.NotecardData.NotecardItemID = notecardID;

			copy.InventoryData = new CopyInventoryFromNotecardPacket.InventoryDataBlock[1];
			copy.InventoryData[0] = new CopyInventoryFromNotecardPacket.InventoryDataBlock();
			copy.InventoryData[0].FolderID = folderID;
			copy.InventoryData[0].ItemID = itemID;

			Client.network.SendPacket(copy);
		}
	}


	//endregion Copy

	//region Update

	/// <summary>
	/// 
	/// </summary>
	/// <param name="item"></param>
	public void RequestUpdateItem(InventoryItem item)
	{
		List<InventoryItem> items = new ArrayList<InventoryItem>(1);
		items.add(item);

		RequestUpdateItems(items, UUID.Random());
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="items"></param>
	public void RequestUpdateItems(List<InventoryItem> items)
	{
		RequestUpdateItems(items, UUID.Random());
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="items"></param>
	/// <param name="transactionID"></param>
	public void RequestUpdateItems(List<InventoryItem> items, UUID transactionID)
	{
		UpdateInventoryItemPacket update = new UpdateInventoryItemPacket();
		update.AgentData.AgentID = Client.self.getAgentID();
		update.AgentData.SessionID = Client.self.getSessionID();
		update.AgentData.TransactionID = transactionID;

		update.InventoryData = new UpdateInventoryItemPacket.InventoryDataBlock[items.size()];
		for (int i = 0; i < items.size(); i++)
		{
			InventoryItem item = items.get(i);

			UpdateInventoryItemPacket.InventoryDataBlock block = new UpdateInventoryItemPacket.InventoryDataBlock();
			block.BaseMask = (long)PermissionMask.getIndex(item.Permissions.BaseMask);
			block.CRC = ItemCRC(item);
			//TODO need to check if the data should be in Integer
			block.CreationDate = (int)Utils.dateToUnixTime(item.CreationDate);
			block.CreatorID = item.CreatorID;
			block.Description = Utils.stringToBytesWithTrailingNullByte(item.Description);
			block.EveryoneMask = (long)PermissionMask.getIndex(item.Permissions.EveryoneMask);
			block.Flags = (long)item.Flags;
			block.FolderID = item.ParentUUID;
			block.GroupID = item.GroupID;
			block.GroupMask = (long)PermissionMask.getIndex(item.Permissions.GroupMask);
			block.GroupOwned = item.GroupOwned;
			block.InvType = (byte)item.InventoryType.getIndex();
			block.ItemID = item.UUID;
			block.Name = Utils.stringToBytesWithTrailingNullByte(item.Name);
			block.NextOwnerMask = (long)PermissionMask.getIndex(item.Permissions.NextOwnerMask);
			block.OwnerID = item.OwnerID;
			block.OwnerMask = (long)PermissionMask.getIndex(item.Permissions.OwnerMask);
			block.SalePrice = item.SalePrice;
			block.SaleType = (byte)item.SaleType.getIndex();
			block.TransactionID = item.TransactionID;
			block.Type = (byte)item.AssetType.getIndex();

			update.InventoryData[i] = block;
		}

		Client.network.SendPacket(update);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="data"></param>
	/// <param name="notecardID"></param>
	/// <param name="callback"></param>
	public void RequestUploadNotecardAsset(byte[] data, 
			UUID notecardID, EventObserver<InventoryUploadedAssetCallbackArg> callback) throws Exception
			{
		if (Client.network.getCurrentSim() == null || Client.network.getCurrentSim().Caps == null)
			throw new Exception("UpdateNotecardAgentInventory capability is not currently available");

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("UpdateNotecardAgentInventory");

		if (url != null)
		{
			OSDMap query = new OSDMap();
			query.put("item_id", OSD.FromUUID(notecardID));

			// Make the request
			CapsHttpClient request = new CapsHttpClient(url);
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					UploadInventoryAssetResponse(arg.getClient(), arg.getResult(), arg.getError());
				}
					});
			//				request.OnComplete += UploadInventoryAssetResponse;
			//				request.setUserData(new Object[] { new Entry<EventObserver<InventoryUploadedAssetCallbackArg>, byte[]>(callback, data), notecardID });
			request.setUserData(new Object[] {callback, data, notecardID });
			request.BeginGetResponse(query, OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			throw new Exception("UpdateNotecardAgentInventory capability is not currently available");
		}
			}

	/// <summary>
	/// Save changes to notecard embedded in object contents
	/// </summary>
	/// <param name="data">Encoded notecard asset data</param>
	/// <param name="notecardID">Notecard UUID</param>
	/// <param name="taskID">Object's UUID</param>
	/// <param name="callback">Called upon finish of the upload with status information</param>
	public void RequestUpdateNotecardTask(byte[] data, UUID notecardID, 
			UUID taskID, EventObserver<InventoryUploadedAssetCallbackArg> callback) throws Exception
			{
		if (Client.network.getCurrentSim() == null || Client.network.getCurrentSim().Caps == null)
			throw new Exception("UpdateNotecardTaskInventory capability is not currently available");

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("UpdateNotecardTaskInventory");

		if (url != null)
		{
			OSDMap query = new OSDMap();
			query.put("item_id", OSD.FromUUID(notecardID));
			query.put("task_id", OSD.FromUUID(taskID));

			// Make the request
			CapsHttpClient request = new CapsHttpClient(url);
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					UploadInventoryAssetResponse(arg.getClient(), arg.getResult(), arg.getError());
				}
					});
			//				request.OnComplete += UploadInventoryAssetResponse;
			//				request.setUserData(new Object[] { new Entry<EventObserver<InventoryUploadedAssetCallbackArg>, byte[]>(callback, data), notecardID });
			request.setUserData(new Object[] {callback, data, notecardID });
			request.BeginGetResponse(query, OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			throw new Exception("UpdateNotecardTaskInventory capability is not currently available");
		}
			}

	/// <summary>
	/// Upload new gesture asset for an inventory gesture item
	/// </summary>
	/// <param name="data">Encoded gesture asset</param>
	/// <param name="gestureID">Gesture inventory UUID</param>
	/// <param name="callback">Callback whick will be called when upload is complete</param>
	public void RequestUploadGestureAsset(byte[] data, 
			UUID gestureID, EventObserver<InventoryUploadedAssetCallbackArg> callback) throws Exception
			{
		if (Client.network.getCurrentSim() == null || Client.network.getCurrentSim().Caps == null)
			throw new Exception("UpdateGestureAgentInventory capability is not currently available");

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("UpdateGestureAgentInventory");

		if (url != null)
		{
			OSDMap query = new OSDMap();
			query.put("item_id", OSD.FromUUID(gestureID));

			// Make the request
			CapsHttpClient request = new CapsHttpClient(url);
			//				request.OnComplete += UploadInventoryAssetResponse;
			//				request.UserData = new object[] { new Entry<InventoryUploadedAssetCallback, byte[]>(callback, data), gestureID };
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					UploadInventoryAssetResponse(arg.getClient(), arg.getResult(), arg.getError());
				}
					});
			//				request.OnComplete += UploadInventoryAssetResponse;
			request.setUserData(new Object[] { callback, data, gestureID });
			//				request.setUserData(new Object[] { new Entry<EventObserver<InventoryUploadedAssetCallbackArg>, byte[]>(callback, data), gestureID });
			request.BeginGetResponse(query, OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			throw new Exception("UpdateGestureAgentInventory capability is not currently available");
		}
			}

	/// <summary>
	/// Update an existing script in an agents Inventory
	/// </summary>
	/// <param name="data">A byte[] array containing the encoded scripts contents</param>
	/// <param name="itemID">the itemID of the script</param>
	/// <param name="mono">if true, sets the script content to run on the mono interpreter</param>
	/// <param name="callback"></param>
	public void RequestUpdateScriptAgentInventory(byte[] data, UUID itemID, 
			boolean mono, EventObserver<ScriptUpdatedCallbackArg> callback) throws Exception
			{
		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("UpdateScriptAgent");

		if (url != null)
		{
			UpdateScriptAgentRequestMessage msg = new UpdateScriptAgentRequestMessage();
			msg.ItemID = itemID;
			msg.Target = mono ? "mono" : "lsl2";

			CapsHttpClient request = new CapsHttpClient(url);
			//				request.OnComplete += new CapsHttpClient.CompleteCallback(UpdateScriptAgentInventoryResponse);
			//				request.UserData = new object[2] { new Entry<ScriptUpdatedCallback, byte[]>(callback, data), itemID };
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					try {
						UpdateScriptAgentInventoryResponse(arg.getClient(), arg.getResult(), arg.getError());
					} catch (URISyntaxException e) {
						JLogger.error(Utils.getExceptionStackTraceAsString(e));
					}
				}
					});
			//				request.OnComplete += UploadInventoryAssetResponse;
			request.setUserData(new Object[] {callback, data, itemID });
			//				request.setUserData(new Object[] { new Entry<EventObserver<ScriptUpdatedCallbackArg>, byte[]>(callback, data), itemID });
			request.BeginGetResponse(msg.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			throw new Exception("UpdateScriptAgent capability is not currently available");
		}
			}

	/// <summary>
	/// Update an existing script in an task Inventory
	/// </summary>
	/// <param name="data">A byte[] array containing the encoded scripts contents</param>
	/// <param name="itemID">the itemID of the script</param>
	/// <param name="taskID">UUID of the prim containting the script</param>
	/// <param name="mono">if true, sets the script content to run on the mono interpreter</param>
	/// <param name="running">if true, sets the script to running</param>
	/// <param name="callback"></param>
	public void RequestUpdateScriptTask(byte[] data, UUID itemID,
			UUID taskID, boolean mono, boolean running, EventObserver<ScriptUpdatedCallbackArg> callback) throws Exception
			{
		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("UpdateScriptTask");

		if (url != null)
		{
			UpdateScriptTaskUpdateMessage msg = new UpdateScriptTaskUpdateMessage();
			msg.ItemID = itemID;
			msg.TaskID = taskID;
			msg.ScriptRunning = running;
			msg.Target = mono ? "mono" : "lsl2";

			CapsHttpClient request = new CapsHttpClient(url);
			//				request.OnComplete += new CapsHttpClient.CompleteCallback(UpdateScriptAgentInventoryResponse);
			//				request.UserData = new object[2] { new Entry<ScriptUpdatedCallback, byte[]>(callback, data), itemID };
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg arg) {
					try {
						UpdateScriptAgentInventoryResponse(arg.getClient(), arg.getResult(), arg.getError());
					} catch (URISyntaxException e) {
						Utils.getExceptionStackTraceAsString(e);
					}
				}
					});
			//				request.OnComplete += UploadInventoryAssetResponse;
			//				request.setUserData(new Object[] { new Entry<EventObserver<ScriptUpdatedCallbackArg>, byte[]>(callback, data), itemID });
			request.setUserData(new Object[] { callback, data, itemID });
			request.BeginGetResponse(msg.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			throw new Exception("UpdateScriptTask capability is not currently available");
		}
			}
	//endregion Update

	//region Rez/Give

	/// <summary>
	/// Rez an object from inventory
	/// </summary>
	/// <param name="simulator">Simulator to place object in</param>
	/// <param name="rotation">Rotation of the object when rezzed</param>
	/// <param name="position">Vector of where to place object</param>
	/// <param name="item">InventoryItem object containing item details</param>
	public UUID RequestRezFromInventory(Simulator simulator, Quaternion rotation, Vector3 position,
			InventoryItem item)
	{
		return RequestRezFromInventory(simulator, rotation, position, item, Client.self.getActiveGroup(),
				UUID.Random(), true);
	}

	/// <summary>
	/// Rez an object from inventory
	/// </summary>
	/// <param name="simulator">Simulator to place object in</param>
	/// <param name="rotation">Rotation of the object when rezzed</param>
	/// <param name="position">Vector of where to place object</param>
	/// <param name="item">InventoryItem object containing item details</param>
	/// <param name="groupOwner">UUID of group to own the object</param>
	public UUID RequestRezFromInventory(Simulator simulator, Quaternion rotation, Vector3 position,
			InventoryItem item, UUID groupOwner)
	{
		return RequestRezFromInventory(simulator, rotation, position, item, groupOwner, UUID.Random(), true);
	}

	/// <summary>
	/// Rez an object from inventory
	/// </summary>
	/// <param name="simulator">Simulator to place object in</param>
	/// <param name="rotation">Rotation of the object when rezzed</param>
	/// <param name="position">Vector of where to place object</param>
	/// <param name="item">InventoryItem object containing item details</param>
	/// <param name="groupOwner">UUID of group to own the object</param>        
	/// <param name="queryID">User defined queryID to correlate replies</param>
	/// <param name="rezSelected">If set to true, the CreateSelected flag
	/// will be set on the rezzed object</param>        
	public UUID RequestRezFromInventory(Simulator simulator, Quaternion rotation, Vector3 position,
			InventoryItem item, UUID groupOwner, UUID queryID, boolean rezSelected)
	{
		RezObjectPacket add = new RezObjectPacket();

		add.AgentData.AgentID = Client.self.getAgentID();
		add.AgentData.SessionID = Client.self.getSessionID();
		add.AgentData.GroupID = groupOwner;

		add.RezData.FromTaskID = UUID.Zero;
		add.RezData.BypassRaycast = 1;
		add.RezData.RayStart = position;
		add.RezData.RayEnd = position;
		add.RezData.RayTargetID = UUID.Zero;
		add.RezData.RayEndIsIntersection = false;
		add.RezData.RezSelected = rezSelected;
		add.RezData.RemoveItem = false;
		add.RezData.ItemFlags = (long)item.Flags;
		add.RezData.GroupMask = (long)PermissionMask.getIndex(item.Permissions.GroupMask);
		add.RezData.EveryoneMask = (long)PermissionMask.getIndex(item.Permissions.EveryoneMask);
		add.RezData.NextOwnerMask = (long)PermissionMask.getIndex(item.Permissions.NextOwnerMask);

		add.InventoryData.ItemID = item.UUID;
		add.InventoryData.FolderID = item.ParentUUID;
		add.InventoryData.CreatorID = item.CreatorID;
		add.InventoryData.OwnerID = item.OwnerID;
		add.InventoryData.GroupID = item.GroupID;
		add.InventoryData.BaseMask = (long)PermissionMask.getIndex(item.Permissions.BaseMask);
		add.InventoryData.OwnerMask = (long)PermissionMask.getIndex(item.Permissions.OwnerMask);
		add.InventoryData.GroupMask = (long)PermissionMask.getIndex(item.Permissions.GroupMask);
		add.InventoryData.EveryoneMask = (long)PermissionMask.getIndex(item.Permissions.EveryoneMask);
		add.InventoryData.NextOwnerMask = (long)PermissionMask.getIndex(item.Permissions.NextOwnerMask);
		add.InventoryData.GroupOwned = item.GroupOwned;
		add.InventoryData.TransactionID = queryID;
		add.InventoryData.Type = (byte)item.InventoryType.getIndex();
		add.InventoryData.InvType = (byte)item.InventoryType.getIndex();
		add.InventoryData.Flags = (long)item.Flags;
		add.InventoryData.SaleType = (byte)item.SaleType.getIndex();
		add.InventoryData.SalePrice = item.SalePrice;
		add.InventoryData.Name = Utils.stringToBytesWithTrailingNullByte(item.Name);
		add.InventoryData.Description = Utils.stringToBytesWithTrailingNullByte(item.Description);
		add.InventoryData.CreationDate = (int)Utils.dateToUnixTime(item.CreationDate);

		Client.network.SendPacket(add, simulator);

		// Remove from store if the item is no copy
		if (_Store.Items.containsKey(item.UUID) && _Store.get(item.UUID) instanceof InventoryItem)
		{
			InventoryItem invItem = (InventoryItem)_Store.get(item.UUID);
			if ((PermissionMask.getIndex(invItem.Permissions.OwnerMask) & PermissionMask.Copy.getIndex()) == PermissionMask.None.getIndex())
			{
				_Store.RemoveNodeFor(invItem);
			}
		}

		return queryID;
	}

	/// <summary>
	/// DeRez an object from the simulator to the agents Objects folder in the agents Inventory
	/// </summary>
	/// <param name="objectLocalID">The simulator Local ID of the object</param>
	/// <remarks>If objectLocalID is a child primitive in a linkset, the entire linkset will be derezzed</remarks>
	public void RequestDeRezToInventory(long objectLocalID) throws InventoryException
	{
		RequestDeRezToInventory(objectLocalID, DeRezDestination.AgentInventoryTake,
				Client.inventory.FindFolderForType(AssetType.Object), UUID.Random());
	}

	/// <summary>
	/// DeRez an object from the simulator and return to inventory
	/// </summary>
	/// <param name="objectLocalID">The simulator Local ID of the object</param>
	/// <param name="destType">The type of destination from the <seealso cref="DeRezDestination"/> enum</param>
	/// <param name="destFolder">The destination inventory folders <seealso cref="UUID"/> -or- 
	/// if DeRezzing object to a tasks Inventory, the Tasks <seealso cref="UUID"/></param>
	/// <param name="transactionID">The transaction ID for this request which
	/// can be used to correlate this request with other packets</param>
	/// <remarks>If objectLocalID is a child primitive in a linkset, the entire linkset will be derezzed</remarks>
	public void RequestDeRezToInventory(long objectLocalID, DeRezDestination destType, UUID destFolder, UUID transactionID)
	{
		DeRezObjectPacket take = new DeRezObjectPacket();

		take.AgentData.AgentID = Client.self.getAgentID();
		take.AgentData.SessionID = Client.self.getSessionID();
		take.AgentBlock = new DeRezObjectPacket.AgentBlockBlock();
		take.AgentBlock.GroupID = UUID.Zero;
		take.AgentBlock.Destination = (byte)destType.getIndex();
		take.AgentBlock.DestinationID = destFolder;
		take.AgentBlock.PacketCount = 1;
		take.AgentBlock.PacketNumber = 1;
		take.AgentBlock.TransactionID = transactionID;

		take.ObjectData = new DeRezObjectPacket.ObjectDataBlock[1];
		take.ObjectData[0] = new DeRezObjectPacket.ObjectDataBlock();
		take.ObjectData[0].ObjectLocalID = objectLocalID;

		Client.network.SendPacket(take);
	}

	/// <summary>
	/// Rez an item from inventory to its previous simulator location
	/// </summary>
	/// <param name="simulator"></param>
	/// <param name="item"></param>
	/// <param name="queryID"></param>
	/// <returns></returns>
	public UUID RequestRestoreRezFromInventory(Simulator simulator, InventoryItem item, UUID queryID)
	{
		RezRestoreToWorldPacket add = new RezRestoreToWorldPacket();

		add.AgentData.AgentID = Client.self.getAgentID();
		add.AgentData.SessionID = Client.self.getSessionID();

		add.InventoryData.ItemID = item.UUID;
		add.InventoryData.FolderID = item.ParentUUID;
		add.InventoryData.CreatorID = item.CreatorID;
		add.InventoryData.OwnerID = item.OwnerID;
		add.InventoryData.GroupID = item.GroupID;
		add.InventoryData.BaseMask = (long)PermissionMask.getIndex(item.Permissions.BaseMask);
		add.InventoryData.OwnerMask = (long)PermissionMask.getIndex(item.Permissions.OwnerMask);
		add.InventoryData.GroupMask = (long)PermissionMask.getIndex(item.Permissions.GroupMask);
		add.InventoryData.EveryoneMask = (long)PermissionMask.getIndex(item.Permissions.EveryoneMask);
		add.InventoryData.NextOwnerMask = (long)PermissionMask.getIndex(item.Permissions.NextOwnerMask);
		add.InventoryData.GroupOwned = item.GroupOwned;
		add.InventoryData.TransactionID = queryID;
		add.InventoryData.Type = (byte)item.InventoryType.getIndex();
		add.InventoryData.InvType = (byte)item.InventoryType.getIndex();
		add.InventoryData.Flags = (long)item.Flags;
		add.InventoryData.SaleType = (byte)item.SaleType.getIndex();
		add.InventoryData.SalePrice = item.SalePrice;
		add.InventoryData.Name = Utils.stringToBytesWithTrailingNullByte(item.Name);
		add.InventoryData.Description = Utils.stringToBytesWithTrailingNullByte(item.Description);
		add.InventoryData.CreationDate = (int)Utils.dateToUnixTime(item.CreationDate);

		Client.network.SendPacket(add, simulator);

		return queryID;
	}

	/// <summary>
	/// Give an inventory item to another avatar
	/// </summary>
	/// <param name="itemID">The <seealso cref="UUID"/> of the item to give</param>
	/// <param name="itemName">The name of the item</param>
	/// <param name="assetType">The type of the item from the <seealso cref="AssetType"/> enum</param>
	/// <param name="recipient">The <seealso cref="UUID"/> of the recipient</param>
	/// <param name="doEffect">true to generate a beameffect during transfer</param>
	public void GiveItem(UUID itemID, String itemName, AssetType assetType, UUID recipient,
			boolean doEffect)
	{
		byte[] bucket;


		bucket = new byte[17];
		bucket[0] = (byte)assetType.getIndex();
		Utils.arraycopy(itemID.GetBytes(), 0, bucket, 1, 16);

		Client.self.InstantMessage(
				Client.self.getName(),
				recipient,
				itemName,
				UUID.Random(),
				InstantMessageDialog.InventoryOffered,
				InstantMessageOnline.Online,
				Client.self.getSimPosition(),
				Client.network.getCurrentSim().ID,
				bucket);

		if (doEffect)
		{
			Client.self.BeamEffect(Client.self.getAgentID(), recipient, Vector3d.Zero,
					Client.settings.DEFAULT_EFFECT_COLOR, 1f, UUID.Random());
		}

		// Remove from store if the item is no copy
		if (_Store.Items.containsKey(itemID) && _Store.get(itemID) instanceof InventoryItem)
		{
			InventoryItem invItem = (InventoryItem)_Store.get(itemID);
			if ((PermissionMask.getIndex(invItem.Permissions.OwnerMask) & PermissionMask.Copy.getIndex()) == PermissionMask.None.getIndex())
			{
				_Store.RemoveNodeFor(invItem);
			}
		}
	}

	/// <summary>
	/// Give an inventory Folder with contents to another avatar
	/// </summary>
	/// <param name="folderID">The <seealso cref="UUID"/> of the Folder to give</param>
	/// <param name="folderName">The name of the folder</param>
	/// <param name="assetType">The type of the item from the <seealso cref="AssetType"/> enum</param>
	/// <param name="recipient">The <seealso cref="UUID"/> of the recipient</param>
	/// <param name="doEffect">true to generate a beameffect during transfer</param>
	public void GiveFolder(UUID folderID, String folderName, AssetType assetType, UUID recipient,
			boolean doEffect) throws InterruptedException, InventoryException
			{
		byte[] bucket;

		List<InventoryItem> folderContents = new ArrayList<InventoryItem>();


		for(InventoryBase ib: Client.inventory.FolderContents(folderID, 
				Client.self.getAgentID(), false, true, InventorySortOrder.ByDate, 1000 * 15))
		{
			folderContents.add(Client.inventory.FetchItem(ib.UUID, Client.self.getAgentID(), 1000 * 10));
		}

		bucket = new byte[17 * (folderContents.size() + 1)];

		//Add parent folder (first item in bucket)
		bucket[0] = (byte)assetType.getIndex();
		Utils.arraycopy(folderID.GetBytes(), 0, bucket, 1, 16);

		//Add contents to bucket after folder
		for (int i = 1; i <= folderContents.size(); ++i)
		{
			bucket[i * 17] = (byte)folderContents.get(i - 1).AssetType.getIndex();
			Utils.arraycopy(folderContents.get(i - 1).UUID.GetBytes(), 0, bucket, i * 17 + 1, 16);
		}

		Client.self.InstantMessage(
				Client.self.getName(),
				recipient,
				folderName,
				UUID.Random(),
				InstantMessageDialog.InventoryOffered,
				InstantMessageOnline.Online,
				Client.self.getSimPosition(),
				Client.network.getCurrentSim().ID,
				bucket);

		if (doEffect)
		{
			Client.self.BeamEffect(Client.self.getAgentID(), recipient, Vector3d.Zero,
					Client.settings.DEFAULT_EFFECT_COLOR, 1f, UUID.Random());
		}

		// Remove from store if items were no copy
		for (int i = 0; i < folderContents.size(); i++)
		{

			if (_Store.Items.containsKey(folderContents.get(i).UUID) 
					&& _Store.get(folderContents.get(i).UUID) instanceof InventoryItem)
			{
				InventoryItem invItem = (InventoryItem)_Store.get(folderContents.get(i).UUID);
				if ((PermissionMask.getIndex(invItem.Permissions.OwnerMask) & PermissionMask.Copy.getIndex()) == PermissionMask.None.getIndex())
				{
					_Store.RemoveNodeFor(invItem);
				}
			}

		}
			}

	//endregion Rez/Give

	//region Task

	/// <summary>
	/// Copy or move an <see cref="InventoryItem"/> from agent inventory to a task (primitive) inventory
	/// </summary>
	/// <param name="objectLocalID">The target object</param>
	/// <param name="item">The item to copy or move from inventory</param>
	/// <returns></returns>
	/// <remarks>For items with copy permissions a copy of the item is placed in the tasks inventory,
	/// for no-copy items the object is moved to the tasks inventory</remarks>
	// DocTODO: what does the return UUID correlate to if anything?
	public UUID UpdateTaskInventory(long objectLocalID, InventoryItem item)
	{
		UUID transactionID = UUID.Random();

		UpdateTaskInventoryPacket update = new UpdateTaskInventoryPacket();
		update.AgentData.AgentID = Client.self.getAgentID();
		update.AgentData.SessionID = Client.self.getSessionID();
		update.UpdateData.Key = 0;
		update.UpdateData.LocalID = objectLocalID;

		update.InventoryData.ItemID = item.UUID;
		update.InventoryData.FolderID = item.ParentUUID;
		update.InventoryData.CreatorID = item.CreatorID;
		update.InventoryData.OwnerID = item.OwnerID;
		update.InventoryData.GroupID = item.GroupID;
		update.InventoryData.BaseMask = (long)PermissionMask.getIndex(item.Permissions.BaseMask);
		update.InventoryData.OwnerMask = (long)PermissionMask.getIndex(item.Permissions.OwnerMask);
		update.InventoryData.GroupMask = (long)PermissionMask.getIndex(item.Permissions.GroupMask);
		update.InventoryData.EveryoneMask = (long)PermissionMask.getIndex(item.Permissions.EveryoneMask);
		update.InventoryData.NextOwnerMask = (long)PermissionMask.getIndex(item.Permissions.NextOwnerMask);
		update.InventoryData.GroupOwned = item.GroupOwned;
		update.InventoryData.TransactionID = transactionID;
		update.InventoryData.Type = (byte)item.AssetType.getIndex();
		update.InventoryData.InvType = (byte)item.InventoryType.getIndex();
		update.InventoryData.Flags = (long)item.Flags;
		update.InventoryData.SaleType = (byte)item.SaleType.getIndex();
		update.InventoryData.SalePrice = item.SalePrice;
		update.InventoryData.Name = Utils.stringToBytesWithTrailingNullByte(item.Name);
		update.InventoryData.Description = Utils.stringToBytesWithTrailingNullByte(item.Description);
		update.InventoryData.CreationDate = (int)Utils.dateToUnixTime(item.CreationDate);
		update.InventoryData.CRC = ItemCRC(item);

		Client.network.SendPacket(update);

		return transactionID;
	}

	/// <summary>
	/// Retrieve a listing of the items contained in a task (Primitive)
	/// </summary>
	/// <param name="objectID">The tasks <seealso cref="UUID"/></param>
	/// <param name="objectLocalID">The tasks simulator local ID</param>
	/// <param name="timeoutMS">milliseconds to wait for reply from simulator</param>
	/// <returns>A list containing the inventory items inside the task or null
	/// if a timeout occurs</returns>
	/// <remarks>This request blocks until the response from the simulator arrives 
	/// or timeoutMS is exceeded</remarks>
	public List<InventoryBase> GetTaskInventory(final UUID objectID, long objectLocalID, int timeoutMS) throws InterruptedException, UnsupportedEncodingException
	{
		final String[] filename = new String[]{null};
		final AutoResetEvent taskReplyEvent = new AutoResetEvent(false);

		EventObserver<TaskInventoryReplyEventArgs> callback = new EventObserver<TaskInventoryReplyEventArgs>()
				//					delegate(Object sender, TaskInventoryReplyEventArgs e)
				{
			@Override
			public void handleEvent(Observable o,
					TaskInventoryReplyEventArgs e) {
				if (e.getItemID().equals(objectID))
				{
					filename[0] = e.getAssetFilename();
					taskReplyEvent.set();
				}							
			}

				};

				onTaskInventoryReply.addObserver(callback);

				RequestTaskInventory(objectLocalID);

				if (taskReplyEvent.waitOne(timeoutMS))
				{
					onTaskInventoryReply.deleteObserver(callback);

					if (!Utils.isNullOrEmpty(filename[0]))
					{
						final byte[][] assetData = new byte[][]{null};
						final BigInteger[] xferID = new BigInteger[]{new BigInteger("0")};
						final AutoResetEvent taskDownloadEvent = new AutoResetEvent(false);

						EventObserver<XferReceivedEventArgs> xferCallback = new EventObserver<XferReceivedEventArgs>()
								//									delegate(Object sender, XferReceivedEventArgs e)
								{
							@Override
							public void handleEvent(Observable o,
									XferReceivedEventArgs e) {
								if (e.getXfer().XferID.equals(xferID[0]))
								{
									assetData[0] = e.getXfer().AssetData;
									taskDownloadEvent.set();
								}}
								};
								Client.assets.registerOnXferReceived(xferCallback);

								// Start the actual asset xfer
								xferID[0] = Client.assets.RequestAssetXfer(filename[0], true, false, UUID.Zero, AssetType.Unknown, true);

								if (taskDownloadEvent.waitOne(timeoutMS))
								{
									Client.assets.unregisterOnXferReceived(xferCallback);

									String taskList = Utils.bytesWithTrailingNullByteToString(assetData[0]);
									return ParseTaskInventory(taskList);
								}
								else
								{
									JLogger.warn("Timed out waiting for task inventory download for " + filename[0]);
									Client.assets.unregisterOnXferReceived(xferCallback);
									return null;
								}
					}
					else
					{
						JLogger.debug("Task is empty for " + objectLocalID);
						return new ArrayList<InventoryBase>(0);
					}
				}
				else
				{
					JLogger.warn("Timed out waiting for task inventory reply for " + objectLocalID);
					onTaskInventoryReply.deleteObserver(callback);
					return null;
				}
	}

	/// <summary>
	/// Request the contents of a tasks (primitives) inventory from the 
	/// current simulator
	/// </summary>
	/// <param name="objectLocalID">The LocalID of the object</param>
	/// <seealso cref="TaskInventoryReply"/>
	public void RequestTaskInventory(long objectLocalID)
	{
		RequestTaskInventory(objectLocalID, Client.network.getCurrentSim());
	}

	/// <summary>
	/// Request the contents of a tasks (primitives) inventory
	/// </summary>
	/// <param name="objectLocalID">The simulator Local ID of the object</param>
	/// <param name="simulator">A reference to the simulator object that contains the object</param>
	/// <seealso cref="TaskInventoryReply"/>
	public void RequestTaskInventory(long objectLocalID, Simulator simulator)
	{
		RequestTaskInventoryPacket request = new RequestTaskInventoryPacket();
		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();
		request.InventoryData.LocalID = objectLocalID;

		Client.network.SendPacket(request, simulator);
	}

	/// <summary>
	/// Move an item from a tasks (Primitive) inventory to the specified folder in the avatars inventory
	/// </summary>
	/// <param name="objectLocalID">LocalID of the object in the simulator</param>
	/// <param name="taskItemID">UUID of the task item to move</param>
	/// <param name="inventoryFolderID">The ID of the destination folder in this agents inventory</param>
	/// <param name="simulator">Simulator Object</param>
	/// <remarks>Raises the <see cref="OnTaskItemReceived"/> event</remarks>
	public void MoveTaskInventory(long objectLocalID, UUID taskItemID, UUID inventoryFolderID, Simulator simulator)
	{
		MoveTaskInventoryPacket request = new MoveTaskInventoryPacket();
		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();

		request.AgentData.FolderID = inventoryFolderID;

		request.InventoryData.ItemID = taskItemID;
		request.InventoryData.LocalID = objectLocalID;

		Client.network.SendPacket(request, simulator);
	}

	/// <summary>
	/// Remove an item from an objects (Prim) Inventory
	/// </summary>
	/// <param name="objectLocalID">LocalID of the object in the simulator</param>
	/// <param name="taskItemID">UUID of the task item to remove</param>
	/// <param name="simulator">Simulator Object</param>
	/// <remarks>You can confirm the removal by comparing the tasks inventory serial before and after the 
	/// request with the <see cref="RequestTaskInventory"/> request combined with
	/// the <seealso cref="TaskInventoryReply"/> event</remarks>
	public void RemoveTaskInventory(long objectLocalID, UUID taskItemID, Simulator simulator)
	{
		RemoveTaskInventoryPacket remove = new RemoveTaskInventoryPacket();
		remove.AgentData.AgentID = Client.self.getAgentID();
		remove.AgentData.SessionID = Client.self.getSessionID();

		remove.InventoryData.ItemID = taskItemID;
		remove.InventoryData.LocalID = objectLocalID;

		Client.network.SendPacket(remove, simulator);
	}

	/// <summary>
	/// Copy an InventoryScript item from the Agents Inventory into a primitives task inventory
	/// </summary>
	/// <param name="objectLocalID">An unsigned integer representing a primitive being simulated</param>
	/// <param name="item">An <seealso cref="InventoryItem"/> which represents a script object from the agents inventory</param>
	/// <param name="enableScript">true to set the scripts running state to enabled</param>
	/// <returns>A Unique Transaction ID</returns>
	/// <example>
	/// The following example shows the basic steps necessary to copy a script from the agents inventory into a tasks inventory
	/// and assumes the script exists in the agents inventory.
	/// <code>
	///    uint primID = 95899503; // Fake prim ID
	///    UUID scriptID = UUID.Parse("92a7fe8a-e949-dd39-a8d8-1681d8673232"); // Fake Script UUID in Inventory
	///
	///    Client.Inventory.FolderContents(Client.Inventory.FindFolderForType(AssetType.LSLText), Client.self.getAgentID(), 
	///        false, true, InventorySortOrder.ByName, 10000);
	///
	///    Client.Inventory.RezScript(primID, (InventoryItem)Client.Inventory.Store.get(scriptID));
	/// </code>
	/// </example>
	// DocTODO: what does the return UUID correlate to if anything?
	public UUID CopyScriptToTask(long objectLocalID, InventoryItem item, boolean enableScript)
	{
		UUID transactionID = UUID.Random();

		RezScriptPacket ScriptPacket = new RezScriptPacket();
		ScriptPacket.AgentData.AgentID = Client.self.getAgentID();
		ScriptPacket.AgentData.SessionID = Client.self.getSessionID();

		ScriptPacket.UpdateBlock.ObjectLocalID = objectLocalID;
		ScriptPacket.UpdateBlock.Enabled = enableScript;

		ScriptPacket.InventoryBlock.ItemID = item.UUID;
		ScriptPacket.InventoryBlock.FolderID = item.ParentUUID;
		ScriptPacket.InventoryBlock.CreatorID = item.CreatorID;
		ScriptPacket.InventoryBlock.OwnerID = item.OwnerID;
		ScriptPacket.InventoryBlock.GroupID = item.GroupID;
		ScriptPacket.InventoryBlock.BaseMask = (long)PermissionMask.getIndex(item.Permissions.BaseMask);
		ScriptPacket.InventoryBlock.OwnerMask = (long)PermissionMask.getIndex(item.Permissions.OwnerMask);
		ScriptPacket.InventoryBlock.GroupMask = (long)PermissionMask.getIndex(item.Permissions.GroupMask);
		ScriptPacket.InventoryBlock.EveryoneMask = (long)PermissionMask.getIndex(item.Permissions.EveryoneMask);
		ScriptPacket.InventoryBlock.NextOwnerMask = (long)PermissionMask.getIndex(item.Permissions.NextOwnerMask);
		ScriptPacket.InventoryBlock.GroupOwned = item.GroupOwned;
		ScriptPacket.InventoryBlock.TransactionID = transactionID;
		ScriptPacket.InventoryBlock.Type = (byte)item.AssetType.getIndex();
		ScriptPacket.InventoryBlock.InvType = (byte)item.InventoryType.getIndex();
		ScriptPacket.InventoryBlock.Flags = (long)item.Flags;
		ScriptPacket.InventoryBlock.SaleType = (byte)item.SaleType.getIndex();
		ScriptPacket.InventoryBlock.SalePrice = item.SalePrice;
		ScriptPacket.InventoryBlock.Name = Utils.stringToBytesWithTrailingNullByte(item.Name);
		ScriptPacket.InventoryBlock.Description = Utils.stringToBytesWithTrailingNullByte(item.Description);
		ScriptPacket.InventoryBlock.CreationDate = (int)Utils.dateToUnixTime(item.CreationDate);
		ScriptPacket.InventoryBlock.CRC = ItemCRC(item);

		Client.network.SendPacket(ScriptPacket);

		return transactionID;
	}


	/// <summary>
	/// Request the running status of a script contained in a task (primitive) inventory
	/// </summary>
	/// <param name="objectID">The ID of the primitive containing the script</param>
	/// <param name="scriptID">The ID of the script</param>
	/// <remarks>The <see cref="ScriptRunningReply"/> event can be used to obtain the results of the 
	/// request</remarks>
	/// <seealso cref="ScriptRunningReply"/>
	public void RequestGetScriptRunning(UUID objectID, UUID scriptID)
	{
		GetScriptRunningPacket request = new GetScriptRunningPacket();
		request.Script.ObjectID = objectID;
		request.Script.ItemID = scriptID;

		Client.network.SendPacket(request);
	}

	/// <summary>
	/// Send a request to set the running state of a script contained in a task (primitive) inventory
	/// </summary>
	/// <param name="objectID">The ID of the primitive containing the script</param>
	/// <param name="scriptID">The ID of the script</param>
	/// <param name="running">true to set the script running, false to stop a running script</param>
	/// <remarks>To verify the change you can use the <see cref="RequestGetScriptRunning"/> method combined
	/// with the <see cref="ScriptRunningReply"/> event</remarks>
	public void RequestSetScriptRunning(UUID objectID, UUID scriptID, boolean running)
	{
		SetScriptRunningPacket request = new SetScriptRunningPacket();
		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();
		request.Script.Running = running;
		request.Script.ItemID = scriptID;
		request.Script.ObjectID = objectID;

		Client.network.SendPacket(request);
	}

	//endregion Task

	//region Helper Functions

	private long RegisterItemCreatedCallback(EventObserver<ItemCreatedCallbackArg> callback)
	{
		synchronized(_CallbacksLock)
		{
			if (_CallbackPos == Integer.MAX_VALUE)
				_CallbackPos = 0;

			_CallbackPos++;

			if (_ItemCreatedCallbacks.containsKey(_CallbackPos))
				JLogger.warn("Overwriting an existing ItemCreatedCallback");

			_ItemCreatedCallbacks.put((long)_CallbackPos, callback);

			return _CallbackPos;
		}
	}

	private long RegisterItemsCopiedCallback(EventObserver<ItemCopiedCallbackArg> callback)
	{
		synchronized(_CallbacksLock)
		{
			if (_CallbackPos == Integer.MAX_VALUE)
				_CallbackPos = 0;

			_CallbackPos++;

			if (_ItemCopiedCallbacks.containsKey(_CallbackPos))
				JLogger.warn("Overwriting an existing ItemsCopiedCallback");

			_ItemCopiedCallbacks.put((long)_CallbackPos, callback);

			return _CallbackPos;
		}
	}

	/// <summary>
	/// Create a CRC from an InventoryItem
	/// </summary>
	/// <param name="iitem">The source InventoryItem</param>
	/// <returns>A uint representing the source InventoryItem as a CRC</returns>
	public static long ItemCRC(InventoryItem iitem)
	{
		long CRC = 0;

		// IDs
		CRC += iitem.AssetUUID.CRC(); // AssetID
		CRC += iitem.ParentUUID.CRC(); // FolderID
		CRC += iitem.UUID.CRC(); // ItemID

		// Permission stuff
		CRC += iitem.CreatorID.CRC(); // CreatorID
		CRC += iitem.OwnerID.CRC(); // OwnerID
		CRC += iitem.GroupID.CRC(); // GroupID

		// CRC += another 4 words which always seem to be zero -- unclear if this is a UUID or what
		CRC += (long)PermissionMask.getIndex(iitem.Permissions.OwnerMask); //owner_mask;      // Either owner_mask or next_owner_mask may need to be
		CRC += (long)PermissionMask.getIndex(iitem.Permissions.NextOwnerMask); //next_owner_mask; // switched with base_mask -- 2 values go here and in my
		CRC += (long)PermissionMask.getIndex(iitem.Permissions.EveryoneMask); //everyone_mask;   // study item, the three were identical.
		CRC += (long)PermissionMask.getIndex(iitem.Permissions.GroupMask); //group_mask;

		// The rest of the CRC fields
		CRC += (long)iitem.Flags; // Flags
		CRC += (long)iitem.InventoryType.getIndex(); // InvType
		CRC += (long)iitem.AssetType.getIndex(); // Type 
		CRC += (long)Utils.dateToUnixTime(iitem.CreationDate); // CreationDate
		CRC += (long)iitem.SalePrice;    // SalePrice
		CRC += (long)((long)iitem.SaleType.getIndex() * 0x07073096); // SaleType

		return CRC;
	}

	/// <summary>
	/// Reverses a cheesy XORing with a fixed UUID to convert a shadow_id to an asset_id
	/// </summary>
	/// <param name="shadowID">Obfuscated shadow_id value</param>
	/// <returns>Deobfuscated asset_id value</returns>
	public static UUID DecryptShadowID(UUID shadowID)
	{
		return UUID.xor(shadowID, MAGIC_ID);
	}

	/// <summary>
	/// Does a cheesy XORing with a fixed UUID to convert an asset_id to a shadow_id
	/// </summary>
	/// <param name="assetID">asset_id value to obfuscate</param>
	/// <returns>Obfuscated shadow_id value</returns>
	public static UUID EncryptAssetID(UUID assetID)
	{
		return UUID.xor(assetID, MAGIC_ID);
	}

	/// <summary>
	/// Wrapper for creating a new <seealso cref="InventoryItem"/> object
	/// </summary>
	/// <param name="type">The type of item from the <seealso cref="InventoryType"/> enum</param>
	/// <param name="id">The <seealso cref="UUID"/> of the newly created object</param>
	/// <returns>An <seealso cref="InventoryItem"/> object with the type and id passed</returns>
	public static InventoryItem CreateInventoryItem(InventoryType type, UUID id)
	{
		switch (type)
		{
		case Texture: return new InventoryTexture(id);
		case Sound: return new InventorySound(id);
		case CallingCard: return new InventoryCallingCard(id);
		case Landmark: return new InventoryLandmark(id);
		case Object: return new InventoryObject(id);
		case Notecard: return new InventoryNotecard(id);
		case Category: return new InventoryCategory(id);
		case LSL: return new InventoryLSL(id);
		case Snapshot: return new InventorySnapshot(id);
		case Attachment: return new InventoryAttachment(id);
		case Wearable: return new InventoryWearable(id);
		case Animation: return new InventoryAnimation(id);
		case Gesture: return new InventoryGesture(id);
		default: return new InventoryItem(type, id);
		}
	}

	private InventoryItem SafeCreateInventoryItem(InventoryType InvType, UUID ItemID)
	{
		InventoryItem ret = null;

		if (_Store.contains(ItemID))
			ret = (InventoryItem)_Store.get(ItemID);

		if (ret == null)
			ret = CreateInventoryItem(InvType, ItemID);

		return ret;
	}

	private static boolean ParseLine(String line, String[] key, String[] value)
	{
		// Clean up and convert tabs to spaces
		line = line.trim();
		line = line.replace('\t', ' ');

		// Shrink all whitespace down to single spaces
		while (line.indexOf("  ") > 0)
			line = line.replace("  ", " ");

		if (line.length() > 2)
		{
			int sep = line.indexOf(' ');
			if (sep > 0)
			{
				key[0] = line.substring(0, sep);
				value[0] = line.substring(sep + 1);

				return true;
			}
		}
		else if (line.length() == 1)
		{
			key[0] = line;
			value[0] = "";
			return true;
		}

		key = null;
		value = null;
		return false;
	}

			/// <summary>
			/// Parse the results of a RequestTaskInventory() response
			/// </summary>
			/// <param name="taskData">A String which contains the data from the task reply</param>
			/// <returns>A List containing the items contained within the tasks inventory</returns>
			public static List<InventoryBase> ParseTaskInventory(String taskData)
			{
				List<InventoryBase> items = new ArrayList<InventoryBase>();
				int lineNum = 0;
				String[] lines = taskData.replace("\r\n", "\n").split("\n");
		
				while (lineNum < lines.length)
				{
					String[] key = new String[1];
					String[] value = new String[1];
					if (ParseLine(lines[lineNum++], key, value))
					{
						if (key[0].equals("inv_object"))
						{
							//region inv_object
		
							// In practice this appears to only be used for folders
							UUID itemID = UUID.Zero;
							UUID parentID = UUID.Zero;
							String name = "";
							AssetType assetType = AssetType.Unknown;
		
							while (lineNum < lines.length)
							{
								if (ParseLine(lines[lineNum++], key, value))
								{
									if (key[0].equals("{"))
									{
										continue;
									}
									else if (key[0].equals("}"))
									{
										break;
									}
									else if (key[0].equals("obj_id"))
									{
										itemID= UUID.Parse(value[0]);
									}
									else if (key[0].equals("parent_id"))
									{
										parentID= UUID.Parse(value[0]);
									}
									else if (key[0].equals("type"))
									{
										assetType = Utils.StringToAssetType(value[0]);
									}
									else if (key[0].equals("name"))
									{
										name = value[0].substring(0, value[0].indexOf('|'));
									}
								}
							}
		
							if (assetType == AssetType.Folder)
							{
								InventoryFolder folder = new InventoryFolder(itemID);
								folder.Name = name;
								folder.ParentUUID = parentID;
		
								items.add(folder);
							}
							else
							{
								InventoryItem item = new InventoryItem(itemID);
								item.Name = name;
								item.ParentUUID = parentID;
								item.AssetType = assetType;
		
								items.add(item);
							}
		
							//endregion inv_object
						}
						else if (key.equals("inv_item"))
						{
							//region inv_item
		
							// Any inventory item that links to an assetID, has permissions, etc
							UUID itemID = UUID.Zero;
							UUID assetID = UUID.Zero;
							UUID parentID = UUID.Zero;
							UUID creatorID = UUID.Zero;
							UUID ownerID = UUID.Zero;
							UUID lastOwnerID = UUID.Zero;
							UUID groupID = UUID.Zero;
							boolean groupOwned = false;
							String name = "";
							String desc = "";
							AssetType assetType = AssetType.Unknown;
							InventoryType inventoryType = InventoryType.Unknown;
							Date creationDate = Utils.Epoch;
							//uint
							long flags = 0;
							Permissions perms = Permissions.NoPermissions;
							SaleType saleType = SaleType.Not;
							int salePrice = 0;
		
							while (lineNum < lines.length)
							{
								if (ParseLine(lines[lineNum++], key, value))
								{
									if (key[0].equals("{"))
									{
										continue;
									}
									else if (key[0].equals("}"))
									{
										break;
									}
									else if (key[0].equals("item_id"))
									{
										itemID = UUID.Parse(value[0]);
									}
									else if (key[0].equals("parent_id"))
									{
										parentID = UUID.Parse(value[0]);
									}
									else if (key[0].equals("permissions"))
									{
										//region permissions
		
										while (lineNum < lines.length)
										{
											if (ParseLine(lines[lineNum++], key, value))
											{
												if (key[0].equals("{"))
												{
													continue;
												}
												else if (key[0].equals("}"))
												{
													break;
												}
												//TODO need to verify following
												else if (key[0].equals("creator_mask"))
												{
													// Deprecated
													long val;
													if ((val = Utils.hexStringToUInt(value[0], false)) >=0)
														perms.BaseMask = PermissionMask.get(val);
												}
												else if (key[0].equals("base_mask"))
												{
													long val;
													if ((val = Utils.hexStringToUInt(value[0], false)) >=0)
														perms.BaseMask = PermissionMask.get(val);
												}
												else if (key[0].equals("owner_mask"))
												{
													long val;
													if ((val = Utils.hexStringToUInt(value[0], false)) >=0)
														perms.OwnerMask = PermissionMask.get(val);
												}
												else if (key[0].equals("group_mask"))
												{
													long val;
													if ((val = Utils.hexStringToUInt(value[0], false)) >=0)
														perms.GroupMask = PermissionMask.get(val);
												}
												else if (key[0].equals("everyone_mask"))
												{
													long val;
													if ((val = Utils.hexStringToUInt(value[0], false)) >=0)
														perms.EveryoneMask = PermissionMask.get(val);
												}
												else if (key[0].equals("next_owner_mask"))
												{
													long val;
													if ((val = Utils.hexStringToUInt(value[0], false)) >=0)
														perms.NextOwnerMask = PermissionMask.get(val);
												}
												else if (key[0].equals("creator_id"))
												{
		
													creatorID = UUID.Parse(value[0]);
												}
												else if (key[0].equals("owner_id"))
												{
													ownerID = UUID.Parse(value[0]);
												}
												else if (key[0].equals("last_owner_id"))
												{
													lastOwnerID = UUID.Parse(value[0]);
												}
												else if (key[0].equals("group_id"))
												{
													groupID = UUID.Parse(value[0]);
												}
												else if (key[0].equals("group_owned"))
												{
													long val;
													if ((val = Utils.hexStringToUInt(value[0], false)) >=0)
														groupOwned = (val != 0);
												}
											}
										}
		
										//endregion permissions
									}
									else if (key[0].equals("sale_info"))
									{
										//region sale_info
		
										while (lineNum < lines.length)
										{
											if (ParseLine(lines[lineNum++], key, value))
											{
												if (key[0].equals("{"))
												{
													continue;
												}
												else if (key[0].equals("}"))
												{
													break;
												}
												else if (key[0].equals("sale_type"))
												{
													saleType = Utils.StringToSaleType(value[0]);
												}
												else if (key[0].equals("sale_price"))
												{
													salePrice = (int)Utils.hexStringToUInt(value[0], false);
												}
											}
										}
		
										//endregion sale_info
									}
									else if (key[0].equals("shadow_id"))
									{
										UUID[] shadowID = new UUID[0];
										if (Utils.tryParseUUID(value[0], shadowID))
											assetID = DecryptShadowID(shadowID[0]);
									}
									else if (key[0].equals("asset_id"))
									{
										assetID = UUID.Parse(value[0]);
									}
									else if (key[0].equals("type"))
									{
										assetType = Utils.StringToAssetType(value[0]);
									}
									else if (key[0].equals("inv_type"))
									{
										inventoryType = Utils.StringToInventoryType(value[0]);
									}
									else if (key[0].equals("flags"))
									{
										long[] a = new long[1];
										Utils.tryParseLong(value[0], a);
										flags = a[1];
									}
									else if (key[0].equals("name"))
									{
										name = value[0].substring(0, value[0].indexOf('|'));
									}
									else if (key[0].equals("desc"))
									{
										desc = value[0].substring(0, value[0].indexOf('|'));
									}
									else if (key[0].equals("creation_date"))
									{
										long[] timestamp = new long[1];
										if (Utils.tryParseLong(value[0], timestamp))
											creationDate = Utils.unixTimeToDate(timestamp[0]);
										else
											JLogger.warn("Failed to parse creation_date " + value[0]);
									}
								}
							}
		
							InventoryItem item = CreateInventoryItem(inventoryType, itemID);
							item.AssetUUID = assetID;
							item.AssetType = assetType;
							item.CreationDate = creationDate;
							item.CreatorID = creatorID;
							item.Description = desc;
							item.Flags = flags;
							item.GroupID = groupID;
							item.GroupOwned = groupOwned;
							item.Name = name;
							item.OwnerID = ownerID;
							item.LastOwnerID = lastOwnerID;
							item.ParentUUID = parentID;
							item.Permissions = perms;
							item.SalePrice = salePrice;
							item.SaleType = saleType;
		
							items.add(item);
		
							//endregion inv_item
						}
						else
						{
							JLogger.error("Unrecognized token " + key + " in: " + "\n" + taskData);
						}
					}
				}
		
				return items;
			}

	//endregion Helper Functions

	//region Internal Callbacks

	void Self_IM(Object sender, InstantMessageEventArgs e) throws InventoryException
	{
		// TODO: MainAvatar.InstantMessageDialog.GroupNotice can also be an inventory offer, should we
		// handle it here?

		if (onInventoryObjectOffered != null &&
				(e.getIM().Dialog == InstantMessageDialog.InventoryOffered
				|| e.getIM().Dialog == InstantMessageDialog.TaskInventoryOffered))
		{
			AssetType type = AssetType.Unknown;
			UUID objectID = UUID.Zero;
			boolean fromTask = false;

			if (e.getIM().Dialog == InstantMessageDialog.InventoryOffered)
			{
				if (e.getIM().BinaryBucket.length == 17)
				{
					type = AssetType.get(e.getIM().BinaryBucket[0]);
					objectID = new UUID(e.getIM().BinaryBucket, 1);
					fromTask = false;
				}
				else
				{
					JLogger.warn("Malformed inventory offer from agent");
					return;
				}
			}
			else if (e.getIM().Dialog == InstantMessageDialog.TaskInventoryOffered)
			{
				if (e.getIM().BinaryBucket.length == 1)
				{
					type = AssetType.get(e.getIM().BinaryBucket[0]);
					fromTask = true;
				}
				else
				{
					JLogger.warn("Malformed inventory offer from object");
					return;
				}
			}

			// Find the folder where this is going to go
			UUID destinationFolderID = FindFolderForType(type);

			// Fire the callback
			try
			{
				ImprovedInstantMessagePacket imp = new ImprovedInstantMessagePacket();
				imp.AgentData.AgentID = Client.self.getAgentID();
				imp.AgentData.SessionID = Client.self.getSessionID();
				imp.MessageBlock.FromGroup = false;
				imp.MessageBlock.ToAgentID = e.getIM().FromAgentID;
				imp.MessageBlock.Offline = 0;
				imp.MessageBlock.ID = e.getIM().IMSessionID;
				imp.MessageBlock.Timestamp = 0;
				imp.MessageBlock.FromAgentName = Utils.stringToBytesWithTrailingNullByte(Client.self.getName());
				imp.MessageBlock.Message = Utils.EmptyBytes;
				imp.MessageBlock.ParentEstateID = 0;
				imp.MessageBlock.RegionID = UUID.Zero;
				imp.MessageBlock.Position = Client.self.getSimPosition();

				InventoryObjectOfferedEventArgs args = new InventoryObjectOfferedEventArgs(e.getIM(), type, objectID, fromTask, destinationFolderID);

				onInventoryObjectOffered.raiseEvent(args);

				if (args.isAccept())
				{
					// Accept the inventory offer
					switch (e.getIM().Dialog)
					{
					case InventoryOffered:
						imp.MessageBlock.Dialog = (byte)InstantMessageDialog.InventoryAccepted.getIndex();
						break;
					case TaskInventoryOffered:
						imp.MessageBlock.Dialog = (byte)InstantMessageDialog.TaskInventoryAccepted.getIndex();
						break;
					case GroupNotice:
						imp.MessageBlock.Dialog = (byte)InstantMessageDialog.GroupNoticeInventoryAccepted.getIndex();
						break;
					}
					imp.MessageBlock.BinaryBucket = args.getFolderID().GetBytes();
				}
				else
				{
					// Decline the inventory offer
					switch (e.getIM().Dialog)
					{
					case InventoryOffered:
						imp.MessageBlock.Dialog = (byte)InstantMessageDialog.InventoryDeclined.getIndex();
						break;
					case TaskInventoryOffered:
						imp.MessageBlock.Dialog = (byte)InstantMessageDialog.TaskInventoryDeclined.getIndex();
						break;
					case GroupNotice:
						imp.MessageBlock.Dialog = (byte)InstantMessageDialog.GroupNoticeInventoryDeclined.getIndex();
						break;
					}

					imp.MessageBlock.BinaryBucket = Utils.EmptyBytes;
				}

				Client.network.SendPacket(imp, e.getSimulator());
			}
			catch (Exception ex)
			{
				JLogger.error(Utils.getExceptionStackTraceAsString(ex));
			}
		}
	}

	private void CreateItemFromAssetResponse(CapsHttpClient client, OSD result, Exception error) throws URISyntaxException
	{
		Object[] args = (Object[])client.getUserData();
		EventObserver<ItemCreatedFromAssetCallbackArg> callback 
		= (EventObserver<ItemCreatedFromAssetCallbackArg>)args[0];
		byte[] itemData = (byte[])args[1];
		int millisecondsTimeout = (Integer)args[2];
		OSDMap request = (OSDMap)args[3];

		if (result == null)
		{
			try { callback.handleEvent(null, new ItemCreatedFromAssetCallbackArg(false, error.getMessage(), UUID.Zero, UUID.Zero)); }
			catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
			return;
		}

		OSDMap contents = (OSDMap)result;

		String status = contents.get("state").asString().toLowerCase();

		if (status.equals("upload"))
		{
			String uploadURL = contents.get("uploader").asString();

			JLogger.debug("CreateItemFromAsset: uploading to " + uploadURL);

			// This makes the assumption that all uploads go to getCurrentSim(), to avoid
			// the problem of HttpRequestState not knowing anything about simulators
			CapsHttpClient upload = new CapsHttpClient(new URI(uploadURL));

			//				upload.OnComplete += CreateItemFromAssetResponse;
			upload.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg e) {
					try {
						CreateItemFromAssetResponse(e.getClient(), e.getResult(), e.getError());
					} catch (URISyntaxException ex) {
						JLogger.error(Utils.getExceptionStackTraceAsString(ex));
					}
				}
					});

			upload.setUserData(new Object[] { callback, itemData, millisecondsTimeout, request });
			upload.BeginGetResponse(itemData, "application/octet-stream", (int)millisecondsTimeout);
		}
		else if (status.equals("complete"))
		{
			JLogger.debug("CreateItemFromAsset: completed");

			if (contents.containsKey("new_inventory_item") && contents.containsKey("new_asset"))
			{
				// Request full update on the item in order to update the local store
				RequestFetchInventory(contents.get("new_inventory_item").asUUID(), Client.self.getAgentID());

				try { callback.handleEvent(null, new ItemCreatedFromAssetCallbackArg(true, "", contents.get("new_inventory_item").asUUID(), contents.get("new_asset").asUUID())); }
				catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
			}
			else
			{
				try { callback.handleEvent(null, new ItemCreatedFromAssetCallbackArg(false, "Failed to parse asset and item UUIDs", UUID.Zero, UUID.Zero)); }
				catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
			}
		}
		else
		{
			// Failure
			try { callback.handleEvent(null, new ItemCreatedFromAssetCallbackArg(false, status, UUID.Zero, UUID.Zero)); }
			catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
		}
	}

	private void Network_OnLoginResponse(boolean loginSuccess, 
			boolean redirect, String message, String reason, LoginResponseData replyData)
	{
		if (loginSuccess)
		{
			// Initialize the store here so we know who owns it:
			_Store = new Inventory(Client, this, replyData.AgentID);
			JLogger.debug("Setting InventoryRoot to " + replyData.InventoryRoot.toString());
			InventoryFolder rootFolder = new InventoryFolder(replyData.InventoryRoot);
			rootFolder.Name = "";
			rootFolder.ParentUUID = UUID.Zero;
			_Store.setRootFolder(rootFolder);

			for (int i = 0; i < replyData.InventorySkeleton.length; i++)
				_Store.UpdateNodeFor(replyData.InventorySkeleton[i]);

			InventoryFolder libraryRootFolder = new InventoryFolder(replyData.LibraryRoot);
			libraryRootFolder.Name = "";
			libraryRootFolder.ParentUUID = UUID.Zero;
			_Store.setLibraryFolder(libraryRootFolder);

			for (int i = 0; i < replyData.LibrarySkeleton.length; i++)
				_Store.UpdateNodeFor(replyData.LibrarySkeleton[i]);
		}
	}

	private void UploadInventoryAssetResponse(CapsHttpClient client, OSD result, Exception error)
	{
		OSDMap contents = (OSDMap)result;
		Object[] userData = (Object[])client.getUserData();
		//			Entry<EventObserver<InventoryUploadedAssetCallbackArg>, byte[]> kvp = (Entry<EventObserver<InventoryUploadedAssetCallbackArg>, byte[]>)(((Object[])client.getUserData())[0]);
		EventObserver<InventoryUploadedAssetCallbackArg> callback = (EventObserver<InventoryUploadedAssetCallbackArg>) userData[0];
		byte[] itemData = (byte[])userData[1];

		if (error == null && contents != null)
		{
			String status = contents.get("state").asString();

			if (status.equals("upload"))
			{
				URI uploadURL = contents.get("uploader").asUri();

				if (uploadURL != null)
				{
					// This makes the assumption that all uploads go to getCurrentSim(), to avoid
					// the problem of HttpRequestState not knowing anything about simulators
					CapsHttpClient upload = new CapsHttpClient(uploadURL);
					//						upload.OnComplete += UploadInventoryAssetResponse;

					upload.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
							{
						@Override
						public void handleEvent(Observable o,
								CapsHttpRequestCompletedArg e) {
							UploadInventoryAssetResponse(e.getClient(), e.getResult(), e.getError());
						}
							});

					upload.setUserData(new Object[] { userData[0], userData[1], userData[2] });
					upload.BeginGetResponse(itemData, "application/octet-stream", Client.settings.CAPS_TIMEOUT);
				}
				else
				{
					try { callback.handleEvent(null, new InventoryUploadedAssetCallbackArg(false, "Missing uploader URL", UUID.Zero, UUID.Zero)); }
					catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
				}
			}
			else if (status.equals("complete"))
			{
				if (contents.containsKey("new_asset"))
				{
					// Request full item update so we keep store in sync
					RequestFetchInventory((UUID)userData[2], contents.get("new_asset").asUUID());

					try { callback.handleEvent(null, new InventoryUploadedAssetCallbackArg(true, "", (UUID)userData[2], contents.get("new_asset").asUUID())); }
					catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
				}
				else
				{
					try { callback.handleEvent(null, new InventoryUploadedAssetCallbackArg(false, "Failed to parse asset and item UUIDs", UUID.Zero, UUID.Zero)); }
					catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
				}
			}
			else
			{
				try { callback.handleEvent(null, new InventoryUploadedAssetCallbackArg(false, status, UUID.Zero, UUID.Zero)); }
				catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
			}
		}
		else
		{
			String message = "Unrecognized or empty response";

			if (error != null)
			{
				//					if (error instanceof WebException)
				//						message = ((HttpWebResponse)((WebException)error).Response).StatusDescription;
				//	
				//					if (message == null || message == "None")
				//						message = error.Message;
				message = error.getMessage();
			}

			try { callback.handleEvent(null, new InventoryUploadedAssetCallbackArg(false, message, UUID.Zero, UUID.Zero)); }
			catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
		}
	}

	private void UpdateScriptAgentInventoryResponse(CapsHttpClient client, OSD result, Exception error) throws URISyntaxException
	{
		Object[] userData = (Object[])client.getUserData();
		//			Entry<ScriptUpdatedCallback, byte[]> kvp = (Entry<ScriptUpdatedCallback, byte[]>)(((Object[])client.UserData)[0]);
		EventObserver<ScriptUpdatedCallbackArg> callback = (EventObserver<ScriptUpdatedCallbackArg>)userData[0];
		byte[] itemData = (byte[])userData[1];

		if (result == null)
		{
			try { callback.handleEvent(null, new ScriptUpdatedCallbackArg(false, error.getMessage(), false, null, UUID.Zero, UUID.Zero)); }
			catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
			return;
		}

		OSDMap contents = (OSDMap)result;

		String status = contents.get("state").asString();
		if (status.equals("upload"))
		{
			String uploadURL = contents.get("uploader").asString();

			CapsHttpClient upload = new CapsHttpClient(new URI(uploadURL));
			//				upload.OnComplete += new CapsHttpClient.CompleteCallback(UpdateScriptAgentInventoryResponse);

			upload.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				@Override
				public void handleEvent(Observable o,
						CapsHttpRequestCompletedArg e) {
					try {
						UpdateScriptAgentInventoryResponse(e.getClient(), e.getResult(), e.getError());
					} catch (URISyntaxException e1) {
						JLogger.error(Utils.getExceptionStackTraceAsString(e1));
					}
				}
					});

			upload.setUserData(new Object[] { userData[0], userData[1], (UUID)userData[2] });
			upload.BeginGetResponse(itemData, "application/octet-stream", Client.settings.CAPS_TIMEOUT);
		}
		else if (status.equals("complete") && callback != null)
		{
			if (contents.containsKey("new_asset"))
			{
				// Request full item update so we keep store in sync
				RequestFetchInventory((UUID)userData[2], contents.get("new_asset").asUUID());


				try
				{
					List<String> compileErrors = null;

					if (contents.containsKey("errors"))
					{
						OSDArray errors = (OSDArray)contents.get("errors");
						compileErrors = new ArrayList<String>(errors.count());

						for (int i = 0; i < errors.count(); i++)
						{
							compileErrors.add(errors.get(i).asString());
						}
					}

					callback.handleEvent(null, new ScriptUpdatedCallbackArg(true,
							status,
							contents.get("compiled").asBoolean(),
							compileErrors,
							(UUID)userData[2],
							contents.get("new_asset").asUUID()));
				}
				catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
			}
			else
			{
				try { callback.handleEvent(null, new ScriptUpdatedCallbackArg(false, 
						"Failed to parse asset UUID", false, null, UUID.Zero, UUID.Zero)); }
				catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
			}
		}
		else if (callback != null)
		{
			try { callback.handleEvent(null, new ScriptUpdatedCallbackArg(false, status, false, null, UUID.Zero, UUID.Zero)); }
			catch (Exception e) { JLogger.error(Utils.getExceptionStackTraceAsString(e)); }
		}
	}
	//endregion Internal Handlers

	//region Packet Handlers

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs Object containing the packet data</param>
	protected void SaveAssetIntoInventoryHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (onSaveAssetToInventory != null)
		{
			Packet packet = e.getPacket();

			SaveAssetIntoInventoryPacket save = (SaveAssetIntoInventoryPacket)packet;
			onSaveAssetToInventory.raiseEvent(new SaveAssetToInventoryEventArgs(save.InventoryData.ItemID, save.InventoryData.NewAssetID));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs Object containing the packet data</param>
	protected void InventoryDescendentsHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException, InventoryException
	{
		Packet packet = e.getPacket();

		InventoryDescendentsPacket reply = (InventoryDescendentsPacket)packet;

		if (reply.AgentData.Descendents > 0)
		{
			// InventoryDescendantsReply sends a null folder if the parent doesnt contain any folders
			if (!reply.FolderData[0].FolderID.equals(UUID.Zero))
			{
				// Iterate folders in this packet
				for (int i = 0; i < reply.FolderData.length; i++)
				{
					// If folder already exists then ignore, we assume the version cache
					// logic is working and if the folder is stale then it should not be present.

					if (!_Store.contains(reply.FolderData[i].FolderID))
					{
						InventoryFolder folder = new InventoryFolder(reply.FolderData[i].FolderID);
						folder.ParentUUID = reply.FolderData[i].ParentID;
						folder.Name = Utils.bytesWithTrailingNullByteToString(reply.FolderData[i].Name);
						folder.PreferredType = AssetType.get(reply.FolderData[i].Type);
						folder.OwnerID = reply.AgentData.OwnerID;

						_Store.put(folder.UUID, folder);
					}
				}
			}

			// InventoryDescendantsReply sends a null item if the parent doesnt contain any items.
			if (!reply.ItemData[0].ItemID.equals(UUID.Zero))
			{
				// Iterate items in this packet
				for (int i = 0; i < reply.ItemData.length; i++)
				{
					if (reply.ItemData[i].ItemID != UUID.Zero)
					{
						InventoryItem item;
						/* 
						 * Objects that have been attached in-world prior to being stored on the 
						 * asset server are stored with the InventoryType of 0 (Texture) 
						 * instead of 17 (Attachment) 
						 * 
						 * This corrects that behavior by forcing Object Asset types that have an 
						 * invalid InventoryType with the proper InventoryType of Attachment.
						 */
						if (AssetType.get(reply.ItemData[i].Type) == AssetType.Object
								&& InventoryType.get(reply.ItemData[i].InvType) == InventoryType.Texture)
						{
							item = CreateInventoryItem(InventoryType.Attachment, reply.ItemData[i].ItemID);
							item.InventoryType = InventoryType.Attachment;
						}
						else
						{
							item = CreateInventoryItem(InventoryType.get(reply.ItemData[i].InvType), reply.ItemData[i].ItemID);
							item.InventoryType = InventoryType.get(reply.ItemData[i].InvType);
						}

						item.ParentUUID = reply.ItemData[i].FolderID;
						item.CreatorID = reply.ItemData[i].CreatorID;
						item.AssetType = AssetType.get(reply.ItemData[i].Type);
						item.AssetUUID = reply.ItemData[i].AssetID;
						item.CreationDate = Utils.unixTimeToDate((long)reply.ItemData[i].CreationDate);
						item.Description = Utils.bytesWithTrailingNullByteToString(reply.ItemData[i].Description);
						item.Flags = reply.ItemData[i].Flags;
						item.Name = Utils.bytesWithTrailingNullByteToString(reply.ItemData[i].Name);
						item.GroupID = reply.ItemData[i].GroupID;
						item.GroupOwned = reply.ItemData[i].GroupOwned;
						item.Permissions = new Permissions(
								reply.ItemData[i].BaseMask,
								reply.ItemData[i].EveryoneMask,
								reply.ItemData[i].GroupMask,
								reply.ItemData[i].NextOwnerMask,
								reply.ItemData[i].OwnerMask);
						item.SalePrice = reply.ItemData[i].SalePrice;
						item.SaleType = SaleType.get(reply.ItemData[i].SaleType);
						item.OwnerID = reply.AgentData.OwnerID;

						_Store.put(item.UUID, item);
					}
				}
			}
		}

		InventoryFolder parentFolder = null;

		JLogger.debug("Store Contains Root Folder? " + _Store.contains(reply.AgentData.FolderID));
		if(_Store.contains(reply.AgentData.FolderID))
			JLogger.debug("Store Contains Root Folder? " + _Store.get(reply.AgentData.FolderID).getClass().toString());


		if (_Store.contains(reply.AgentData.FolderID) &&
				_Store.get(reply.AgentData.FolderID) instanceof InventoryFolder)
		{
			parentFolder = (InventoryFolder)_Store.get(reply.AgentData.FolderID);
		}
		else
		{
			JLogger.error("Don't have a reference to FolderID " + reply.AgentData.FolderID.toString() +
					" or it is not a folder");
			return;
		}

		if (reply.AgentData.Version < parentFolder.Version)
		{
			JLogger.warn("Got an outdated InventoryDescendents packet for folder " + parentFolder.Name +
					", this version = " + reply.AgentData.Version + ", latest version = " + parentFolder.Version);
			return;
		}

		parentFolder.Version = reply.AgentData.Version;
		// FIXME: reply.AgentData.Descendants is not parentFolder.DescendentCount if we didn't 
		// request items and folders
		parentFolder.DescendentCount = reply.AgentData.Descendents;
		_Store.GetNodeFor(reply.AgentData.FolderID).setNeedsUpdate(false);

		//region FindObjectsByPath Handling

		if (_Searches.size() > 0)
		{
			synchronized(_Searches)
			{
				boolean keepSearching = true;
				while(keepSearching)
				{
					keepSearching = false;
					// Iterate over all of the outstanding searches
					for (int i = 0; i < _Searches.size(); i++)
					{
						InventorySearch search = _Searches.get(i);
						List<InventoryBase> folderContents = _Store.GetContents(search.Folder);

						// Iterate over all of the inventory Objects in the base search folder
						for (int j = 0; j < folderContents.size(); j++)
						{
							// Check if this inventory Object matches the current path node
							if (folderContents.get(j).Name.equals(search.Path[search.Level]))
							{
								if (search.Level == search.Path.length - 1)
								{
									JLogger.debug("Finished path search of " + "/" + search.Path);

									// This is the last node in the path, fire the callback and clean up
									if (onFindObjectByPathReply != null)
									{
										onFindObjectByPathReply.raiseEvent(new FindObjectByPathReplyEventArgs("/" + search.Path,
												folderContents.get(j).UUID));
									}

									// Remove this entry and restart the loop since we are changing the collection size
									_Searches.remove(i);
									keepSearching = true;
									break;
								}
								else
								{
									// We found a match but it is not the end of the path, request the next level
									JLogger.debug(String.format("Matched level %s/%s in a path search of %s",
											search.Level, search.Path.length - 1, "/" + search.Path));

									search.Folder = folderContents.get(j).UUID;
									search.Level++;
									_Searches.add(i, search);

									RequestFolderContents(search.Folder, search.Owner, true, true,
											InventorySortOrder.ByName);
								}
							}
						}
					}
				}
			}
		}

		//endregion FindObjectsByPath Handling

		// Callback for inventory folder contents being updated
		onFolderUpdated.raiseEvent(new FolderUpdatedEventArgs(parentFolder.UUID, true));
	}

	/// <summary>
	/// UpdateCreateInventoryItem packets are received when a new inventory item 
	/// is created. This may occur when an object that's rezzed in world is
	/// taken into inventory, when an item is created using the CreateInventoryItem
	/// packet, or when an object is purchased
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void UpdateCreateInventoryItemHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException, InventoryException
	{
		Packet packet = e.getPacket();

		UpdateCreateInventoryItemPacket reply = (UpdateCreateInventoryItemPacket)packet;

		for(UpdateCreateInventoryItemPacket.InventoryDataBlock dataBlock : reply.InventoryData)
		{
			if (dataBlock.InvType == (byte)InventoryType.Folder.getIndex())
			{
				JLogger.error("Received InventoryFolder in an UpdateCreateInventoryItem packet, this should not happen!");
				continue;
			}

			InventoryItem item = CreateInventoryItem(InventoryType.get(dataBlock.InvType), dataBlock.ItemID);
			item.AssetType = AssetType.get(dataBlock.Type);
			item.AssetUUID = dataBlock.AssetID;
			item.CreationDate = Utils.unixTimeToDate(dataBlock.CreationDate);
			item.CreatorID = dataBlock.CreatorID;
			item.Description = Utils.bytesWithTrailingNullByteToString(dataBlock.Description);
			item.Flags = dataBlock.Flags;
			item.GroupID = dataBlock.GroupID;
			item.GroupOwned = dataBlock.GroupOwned;
			item.Name = Utils.bytesWithTrailingNullByteToString(dataBlock.Name);
			item.OwnerID = dataBlock.OwnerID;
			item.ParentUUID = dataBlock.FolderID;
			item.Permissions = new Permissions(
					dataBlock.BaseMask,
					dataBlock.EveryoneMask,
					dataBlock.GroupMask,
					dataBlock.NextOwnerMask,
					dataBlock.OwnerMask);
			item.SalePrice = dataBlock.SalePrice;
			item.SaleType = SaleType.get(dataBlock.SaleType);

			/* 
			 * When attaching new objects, an UpdateCreateInventoryItem packet will be
			 * returned by the server that has a FolderID/ParentUUID of zero. It is up
			 * to the client to make sure that the item gets a good folder, otherwise
			 * it will end up inaccesible in inventory.
			 */
			if (item.ParentUUID.equals(UUID.Zero))
			{
				// assign default folder for type
				item.ParentUUID = FindFolderForType(item.AssetType);

				JLogger.info("Received an item through UpdateCreateInventoryItem with no parent folder, assigning to folder " +
						item.ParentUUID);

				// send update to the sim
				RequestUpdateItem(item);
			}

			// Update the local copy
			_Store.put(item.UUID, item);

			// Look for an "item created" callback
			EventObserver<ItemCreatedCallbackArg> createdCallback;
			if ((createdCallback = _ItemCreatedCallbacks.get(dataBlock.CallbackID))!=null)
			{
				_ItemCreatedCallbacks.remove(dataBlock.CallbackID);

				try { createdCallback.handleEvent(null, new ItemCreatedCallbackArg(true, item)); }
				catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
			}

			// TODO: Is this callback even triggered when items are copied?
			// Look for an "item copied" callback
			EventObserver<ItemCopiedCallbackArg> copyCallback;
			if ((copyCallback = _ItemCopiedCallbacks.get(dataBlock.CallbackID))!=null)
			{
				_ItemCopiedCallbacks.remove(dataBlock.CallbackID);

				try { copyCallback.handleEvent(null, new ItemCopiedCallbackArg(item)); }
				catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
			}

			//This is triggered when an item is received from a task
			if (onTaskItemReceived != null)
			{
				onTaskItemReceived.raiseEvent(new TaskItemReceivedEventArgs(item.UUID, dataBlock.FolderID,
						item.CreatorID, item.AssetUUID, item.InventoryType));
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void MoveInventoryItemHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();

		MoveInventoryItemPacket move = (MoveInventoryItemPacket)packet;

		for (int i = 0; i < move.InventoryData.length; i++)
		{
			// FIXME: Do something here
			String newName = Utils.bytesWithTrailingNullByteToString(move.InventoryData[i].NewName);

			JLogger.warn(String.format(
					"MoveInventoryItemHandler: Item {0} is moving to Folder {1} with new name \"{2}\". Someone write this function!",
					move.InventoryData[i].ItemID.toString(), move.InventoryData[i].FolderID.toString(),
					newName));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void BulkUpdateInventoryHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();

		BulkUpdateInventoryPacket update = (BulkUpdateInventoryPacket)packet;

		if (update.FolderData.length > 0 && update.FolderData[0].FolderID != UUID.Zero)
		{
			for(BulkUpdateInventoryPacket.FolderDataBlock dataBlock : update.FolderData)
			{
				if (!_Store.contains(dataBlock.FolderID))
					JLogger.warn("Received BulkUpdate for unknown folder: " + dataBlock.FolderID);

				InventoryFolder folder = new InventoryFolder(dataBlock.FolderID);
				folder.Name = Utils.bytesWithTrailingNullByteToString(dataBlock.Name);
				folder.OwnerID = update.AgentData.AgentID;
				folder.ParentUUID = dataBlock.ParentID;
				_Store.put(folder.UUID, folder);
			}
		}

		if (update.ItemData.length > 0 && update.ItemData[0].ItemID != UUID.Zero)
		{
			for (int i = 0; i < update.ItemData.length; i++)
			{
				BulkUpdateInventoryPacket.ItemDataBlock dataBlock = update.ItemData[i];

				// If we are given a folder of items, the item information might arrive before the folder
				// (parent) is in the store
				if (!_Store.contains(dataBlock.ItemID))
					JLogger.warn("Received BulkUpdate for unknown item: " + dataBlock.ItemID);

				InventoryItem item = SafeCreateInventoryItem(InventoryType.get(dataBlock.InvType), dataBlock.ItemID);

				item.AssetType = AssetType.get(dataBlock.Type);
				if (dataBlock.AssetID != UUID.Zero) item.AssetUUID = dataBlock.AssetID;
				item.CreationDate = Utils.unixTimeToDate(dataBlock.CreationDate);
				item.CreatorID = dataBlock.CreatorID;
				item.Description = Utils.bytesWithTrailingNullByteToString(dataBlock.Description);
				item.Flags = dataBlock.Flags;
				item.GroupID = dataBlock.GroupID;
				item.GroupOwned = dataBlock.GroupOwned;
				item.Name = Utils.bytesWithTrailingNullByteToString(dataBlock.Name);
				item.OwnerID = dataBlock.OwnerID;
				item.ParentUUID = dataBlock.FolderID;
				item.Permissions = new Permissions(
						dataBlock.BaseMask,
						dataBlock.EveryoneMask,
						dataBlock.GroupMask,
						dataBlock.NextOwnerMask,
						dataBlock.OwnerMask);
				item.SalePrice = dataBlock.SalePrice;
				item.SaleType = SaleType.get(dataBlock.SaleType);

				_Store.put(item.UUID, item);

				// Look for an "item created" callback
				EventObserver<ItemCreatedCallbackArg> callback;
				if ((callback = _ItemCreatedCallbacks.get(dataBlock.CallbackID)) !=null)
				{
					_ItemCreatedCallbacks.remove(dataBlock.CallbackID);

					try { callback.handleEvent(null, new ItemCreatedCallbackArg(true, item)); }
					catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
				}

				// Look for an "item copied" callback
				EventObserver<ItemCopiedCallbackArg> copyCallback;
				if ((copyCallback = _ItemCopiedCallbacks.get(dataBlock.CallbackID))!=null)
				{
					_ItemCopiedCallbacks.remove(dataBlock.CallbackID);

					try { copyCallback.handleEvent(null, new ItemCopiedCallbackArg(item)); }
					catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
				}
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void FetchInventoryReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();

		FetchInventoryReplyPacket reply = (FetchInventoryReplyPacket)packet;

		for(FetchInventoryReplyPacket.InventoryDataBlock dataBlock : reply.InventoryData)
		{
			if (dataBlock.InvType == (byte)InventoryType.Folder.getIndex())
			{
				JLogger.error("Received FetchInventoryReply for an inventory folder, this should not happen!"
						);
				continue;
			}

			InventoryItem item = CreateInventoryItem(InventoryType.get(dataBlock.InvType), dataBlock.ItemID);
			item.AssetType = AssetType.get(dataBlock.Type);
			item.AssetUUID = dataBlock.AssetID;
			item.CreationDate = Utils.unixTimeToDate(dataBlock.CreationDate);
			item.CreatorID = dataBlock.CreatorID;
			item.Description = Utils.bytesWithTrailingNullByteToString(dataBlock.Description);
			item.Flags = dataBlock.Flags;
			item.GroupID = dataBlock.GroupID;
			item.GroupOwned = dataBlock.GroupOwned;
			item.InventoryType = InventoryType.get(dataBlock.InvType);
			item.Name = Utils.bytesWithTrailingNullByteToString(dataBlock.Name);
			item.OwnerID = dataBlock.OwnerID;
			item.ParentUUID = dataBlock.FolderID;
			item.Permissions = new Permissions(
					dataBlock.BaseMask,
					dataBlock.EveryoneMask,
					dataBlock.GroupMask,
					dataBlock.NextOwnerMask,
					dataBlock.OwnerMask);
			item.SalePrice = dataBlock.SalePrice;
			item.SaleType = SaleType.get(dataBlock.SaleType);
			item.UUID = dataBlock.ItemID;

			_Store.put(item.UUID, item);

			// Fire the callback for an item being fetched
			if (onItemReceived != null)
			{
				onItemReceived.raiseEvent(new ItemReceivedEventArgs(item));
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ReplyTaskInventoryHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (onTaskInventoryReply != null)
		{
			Packet packet = e.getPacket();

			ReplyTaskInventoryPacket reply = (ReplyTaskInventoryPacket)packet;

			onTaskInventoryReply.raiseEvent(new TaskInventoryReplyEventArgs(reply.InventoryData.TaskID, reply.InventoryData.Serial,
					Utils.bytesWithTrailingNullByteToString(reply.InventoryData.Filename)));
		}
	}

	protected void ScriptRunningReplyMessageHandler(String capsKey, IMessage message, Simulator simulator)
	{
		if (onScriptRunningReply != null)
		{
			ScriptRunningReplyMessage msg = (ScriptRunningReplyMessage)message;
			onScriptRunningReply.raiseEvent(new ScriptRunningReplyEventArgs(msg.ObjectID, msg.ItemID, msg.Mono, msg.Running));
		}
	}

	//endregion Packet Handlers

}
