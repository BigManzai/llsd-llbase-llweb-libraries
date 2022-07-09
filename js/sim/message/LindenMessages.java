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
package com.ngt.jopenmetaverse.shared.sim.message;

import java.io.Serializable;
import java.math.BigInteger;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.lang.NotImplementedException;

import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.AttachmentPoint;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ExtraParamType;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Enums.Bumpiness;
import com.ngt.jopenmetaverse.shared.protocol.primitives.MediaEntry;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions.PermissionMask;
import com.ngt.jopenmetaverse.shared.protocol.primitives.PhysicsProperties;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageDialog;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageOnline;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.TeleportFlags;
import com.ngt.jopenmetaverse.shared.sim.AvatarManager.AgentDisplayName;
import com.ngt.jopenmetaverse.shared.sim.ParcelManager.ParcelCategory;
import com.ngt.jopenmetaverse.shared.sim.ParcelManager.ParcelFlags;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.structureddata.OSDUUID;
import com.ngt.jopenmetaverse.shared.sim.ParcelManager.*;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.Enums.InventoryType;
import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupPowers;


public class LindenMessages 
{
	/// <summary>
	/// Sent to the client to indicate a teleport request has completed
	/// </summary>
	public static class TeleportFinishMessage implements IMessage
	{
		/// <summary>The <see cref="UUID"/> of the agent</summary>
		public UUID AgentID;
		/// <summary></summary>
		public int LocationID;
		/// <summary>The simulators handle the agent teleported to</summary>
		//ulong
		public BigInteger RegionHandle;
		/// <summary>A Uri which contains a list of Capabilities the simulator supports</summary>
		public URI SeedCapability;
		/// <summary>Indicates the level of access required
		/// to access the simulator, or the content rating, or the simulators 
		/// map status</summary>
		public Simulator.SimAccess SimAccess;
		/// <summary>The IP Address of the simulator</summary>
		public InetAddress IP;
		/// <summary>The UDP Port the simulator will listen for UDP traffic on</summary>
		public int Port;
		/// <summary>Status flags indicating the state of the Agent upon arrival, Flying, etc.</summary>
		public EnumSet<TeleportFlags> Flags;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);

			OSDArray infoArray = new OSDArray(1);

			OSDMap info = new OSDMap(8);
			info.put("AgentID", OSD.FromUUID(AgentID));
			info.put("LocationID", OSD.FromInteger(LocationID)); // Unused by the client
			info.put("RegionHandle", OSD.FromULong(RegionHandle));
			info.put("SeedCapability", OSD.FromUri(SeedCapability));
			info.put("SimAccess", OSD.FromInteger(SimAccess.getIndex()));
			info.put("SimIP", MessageUtils.FromIP(IP));
			info.put("SimPort", OSD.FromInteger(Port));
			info.put("TeleportFlags", OSD.FromUInteger(TeleportFlags.getIndex(Flags)));

			infoArray.add(info);

			map.put("Info", infoArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map) throws Exception
		{
			OSDArray array = (OSDArray)map.get("Info");
			OSDMap blockMap = (OSDMap)array.get(0);

			AgentID = blockMap.get("AgentID").asUUID();
			LocationID = blockMap.get("LocationID").asInteger();
			RegionHandle = blockMap.get("RegionHandle").asULong();
			SeedCapability = blockMap.get("SeedCapability").asUri();
			SimAccess = Simulator.SimAccess.get((short)blockMap.get("SimAccess").asInteger());
			IP = MessageUtils.ToIP(blockMap.get("SimIP"));
			Port = blockMap.get("SimPort").asInteger();
			Flags = TeleportFlags.get(blockMap.get("TeleportFlags").asUInteger());
		}
	}

	/// <summary>
	/// Sent to the viewer when a neighboring simulator is requesting the agent make a connection to it.
	/// </summary>
	public static class EstablishAgentCommunicationMessage implements IMessage
	{
		public UUID AgentID;
		public InetAddress Address;
		public int Port;
		public URI SeedCapability;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);
			map.put("agent-id", OSD.FromUUID(AgentID));
			map.put("sim-ip-and-port", OSD.FromString(String.format("%s:%s", Address, Port)));
			map.put("seed-capability", OSD.FromUri(SeedCapability));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map) throws UnknownHostException
		{
			String ipAndPort = map.get("sim-ip-and-port").asString();
			int i = ipAndPort.indexOf(':');

			AgentID = map.get("agent-id").asUUID();
			Address = Inet4Address.getByName(ipAndPort.substring(0, i));
			Port = Integer.parseInt((ipAndPort.substring(i + 1)));
			SeedCapability = map.get("seed-capability").asUri();
		}
	}

	public static class CrossedRegionMessage implements IMessage
	{
		public Vector3 LookAt;
		public Vector3 Position;
		public UUID AgentID;
		public UUID SessionID;
		//ulong
		public BigInteger RegionHandle;
		public URI SeedCapability;
		public InetAddress IP;
		public int Port;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);

			OSDArray infoArray = new OSDArray(1);
			OSDMap infoMap = new OSDMap(2);
			infoMap.put("LookAt", OSD.FromVector3(LookAt));
			infoMap.put("Position", OSD.FromVector3(Position));
			infoArray.add(infoMap);
			map.put("Info", infoArray);

			OSDArray agentDataArray = new OSDArray(1);
			OSDMap agentDataMap = new OSDMap(2);
			agentDataMap.put("AgentID",  OSD.FromUUID(AgentID));
			agentDataMap.put("SessionID", OSD.FromUUID(SessionID));
			agentDataArray.add(agentDataMap);
			map.put("AgentData", agentDataArray);

			OSDArray regionDataArray = new OSDArray(1);
			OSDMap regionDataMap = new OSDMap(4);
			regionDataMap.put("RegionHandle", OSD.FromULong(RegionHandle));
			regionDataMap.put("SeedCapability", OSD.FromUri(SeedCapability));
			regionDataMap.put("SimIP", MessageUtils.FromIP(IP));
			regionDataMap.put("SimPort", OSD.FromInteger(Port));
			regionDataArray.add(regionDataMap);
			map.put("RegionData", regionDataArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map) throws UnknownHostException
		{
			OSDMap infoMap = (OSDMap)((OSDArray)map.get("Info")).get(0);
			LookAt = infoMap.get("LookAt").asVector3();
			Position = infoMap.get("Position").asVector3();

			OSDMap agentDataMap = (OSDMap)((OSDArray)map.get("AgentData")).get(0);
			AgentID = agentDataMap.get("AgentID").asUUID();
			SessionID = agentDataMap.get("SessionID").asUUID();

			OSDMap regionDataMap = (OSDMap)((OSDArray)map.get("RegionData")).get(0);
			RegionHandle = regionDataMap.get("RegionHandle").asULong();
			SeedCapability = regionDataMap.get("SeedCapability").asUri();
			IP = MessageUtils.ToIP(regionDataMap.get("SimIP"));
			Port = regionDataMap.get("SimPort").asInteger();
		}
	}

	public static class EnableSimulatorMessage implements IMessage
	{
		public static class SimulatorInfoBlock
		{
			//ulong
			public BigInteger RegionHandle;
			public InetAddress IP;
			public int Port;
		}

		public SimulatorInfoBlock[] Simulators;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);

			OSDArray array = new OSDArray(Simulators.length);
			for (int i = 0; i < Simulators.length; i++)
			{
				SimulatorInfoBlock block = Simulators[i];

				OSDMap blockMap = new OSDMap(3);
				blockMap.put("Handle", OSD.FromULong(block.RegionHandle));
				blockMap.put("IP", MessageUtils.FromIP(block.IP));
				blockMap.put("Port", OSD.FromInteger(block.Port));
				array.add(blockMap);
			}

			map.put("SimulatorInfo", array);
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map) throws UnknownHostException
		{
			OSDArray array = (OSDArray)map.get("SimulatorInfo");
			Simulators = new SimulatorInfoBlock[array.count()];

			for (int i = 0; i < array.count(); i++)
			{
				OSDMap blockMap = (OSDMap)array.get(i);

				SimulatorInfoBlock block = new SimulatorInfoBlock();
				block.RegionHandle = blockMap.get("Handle").asULong();
				block.IP = MessageUtils.ToIP(blockMap.get("IP"));
				block.Port = blockMap.get("Port").asInteger();
				Simulators[i] = block;
			}
		}
	}

	/// <summary>
	/// A message sent to the client which indicates a teleport request has failed
	/// and contains some information on why it failed
	/// </summary>
	public static class TeleportFailedMessage implements IMessage
	{
		/// <summary></summary>
		public String  ExtraParams;
		/// <summary>A string key of the reason the teleport failed e.g. CouldntTPCloser
		/// Which could be used to look up a value in a dictionary or enum</summary>
		public String  MessageKey;
		/// <summary>The <see cref="UUID"/> of the Agent</summary>
		public UUID AgentID;
		/// <summary>A string human readable message containing the reason </summary>
		/// <remarks>An example: Could not teleport closer to destination</remarks>
		public String  Reason;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);

			OSDMap alertInfoMap = new OSDMap(2);

			alertInfoMap.put("ExtraParams", OSD.FromString(ExtraParams));
			alertInfoMap.put("Message", OSD.FromString(MessageKey));
			OSDArray alertArray = new OSDArray();
			alertArray.add(alertInfoMap);
			map.put("AlertInfo",  alertArray);

			OSDMap infoMap = new OSDMap(2);
			infoMap.put("AgentID", OSD.FromUUID(AgentID));
			infoMap.put("Reason", OSD.FromString(Reason));
			OSDArray infoArray = new OSDArray();
			infoArray.add(infoMap);
			map.put("Info", infoArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{

			OSDArray alertInfoArray = (OSDArray)map.get("AlertInfo");

			OSDMap alertInfoMap = (OSDMap)alertInfoArray.get(0);
			ExtraParams = alertInfoMap.get("ExtraParams").asString();
			MessageKey = alertInfoMap.get("Message").asString();

			OSDArray infoArray = (OSDArray)map.get("Info");
			OSDMap infoMap = (OSDMap)infoArray.get(0);
			AgentID = infoMap.get("AgentID").asUUID();
			Reason = infoMap.get("Reason").asString();
		}
	}

	public static class LandStatReplyMessage implements IMessage
	{
		//uint
		public long ReportType;
		public long RequestFlags;
		public long TotalObjectCount;

		public static class ReportDataBlock
		{
			public Vector3 Location;
			public String  OwnerName;
			public float Score;
			public UUID TaskID;
			//uint
			public long TaskLocalID;
			public String  TaskName;
			public float MonoScore;
			public Date TimeStamp;
		}

		public ReportDataBlock[] ReportDataBlocks;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);

			OSDMap requestDataMap = new OSDMap(3);
			requestDataMap.put("ReportType", OSD.FromUInteger(this.ReportType));
			requestDataMap.put("RequestFlags", OSD.FromUInteger(this.RequestFlags));
			requestDataMap.put("TotalObjectCount", OSD.FromUInteger(this.TotalObjectCount));

			OSDArray requestDatArray = new OSDArray();
			requestDatArray.add(requestDataMap);
			map.put("RequestData", requestDatArray);

			OSDArray reportDataArray = new OSDArray();
			OSDArray dataExtendedArray = new OSDArray();
			for (int i = 0; i < ReportDataBlocks.length; i++)
			{
				OSDMap reportMap = new OSDMap(8);
				reportMap.put("LocationX", OSD.FromReal(ReportDataBlocks[i].Location.X));
				reportMap.put("LocationY", OSD.FromReal(ReportDataBlocks[i].Location.Y));
				reportMap.put("LocationZ", OSD.FromReal(ReportDataBlocks[i].Location.Z));
				reportMap.put("OwnerName", OSD.FromString(ReportDataBlocks[i].OwnerName));
				reportMap.put("Score", OSD.FromReal(ReportDataBlocks[i].Score));
				reportMap.put("TaskID", OSD.FromUUID(ReportDataBlocks[i].TaskID));
				reportMap.put("TaskLocalID", OSD.FromReal(ReportDataBlocks[i].TaskLocalID));
				reportMap.put("TaskName", OSD.FromString(ReportDataBlocks[i].TaskName));
				reportDataArray.add(reportMap);

				OSDMap extendedMap = new OSDMap(2);
				extendedMap.put("MonoScore", OSD.FromReal(ReportDataBlocks[i].MonoScore));
				extendedMap.put("TimeStamp", OSD.FromDate(ReportDataBlocks[i].TimeStamp));
				dataExtendedArray.add(extendedMap);
			}

			map.put("ReportData", reportDataArray);
			map.put("DataExtended", dataExtendedArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{

			OSDArray requestDataArray = (OSDArray)map.get("RequestData");
			OSDMap requestMap = (OSDMap)requestDataArray.get(0);

			this.ReportType = requestMap.get("ReportType").asUInteger();
			this.RequestFlags = requestMap.get("RequestFlags").asUInteger();
			this.TotalObjectCount = requestMap.get("TotalObjectCount").asUInteger();

			if(TotalObjectCount < 1)
			{
				ReportDataBlocks = new ReportDataBlock[0];
				return;
			}

			OSDArray dataArray = (OSDArray)map.get("ReportData");
			OSDArray dataExtendedArray = (OSDArray)map.get("DataExtended");

			ReportDataBlocks = new ReportDataBlock[dataArray.count()];
			for (int i = 0; i < dataArray.count(); i++)
			{
				OSDMap blockMap = (OSDMap)dataArray.get(i);
				OSDMap extMap = (OSDMap)dataExtendedArray.get(i);
				ReportDataBlock block = new ReportDataBlock();
				block.Location = new Vector3(
						(float)blockMap.get("LocationX").asReal(),
						(float)blockMap.get("LocationY").asReal(),
						(float)blockMap.get("LocationZ").asReal());
				block.OwnerName = blockMap.get("OwnerName").asString();
				block.Score = (float)blockMap.get("Score").asReal();
				block.TaskID = blockMap.get("TaskID").asUUID();
				block.TaskLocalID = blockMap.get("TaskLocalID").asUInteger();
				block.TaskName = blockMap.get("TaskName").asString();
				block.MonoScore = (float)extMap.get("MonoScore").asReal();
				block.TimeStamp = Utils.unixTimeToDate(extMap.get("TimeStamp").asUInteger());

				ReportDataBlocks[i] = block;
			}
		}
	}

	//endregion

	//region Parcel Messages

	/// <summary>
	/// Contains a list of prim owner information for a specific parcel in a simulator
	/// </summary>
	/// <remarks>
	/// A Simulator will always return at least 1 entry
	/// If agent does not have proper permission the OwnerID will be UUID.Zero
	/// If agent does not have proper permission OR there are no primitives on parcel
	/// the DataBlocksExtended map will not be sent from the simulator
	/// </remarks>
	public static class ParcelObjectOwnersReplyMessage implements IMessage
	{
		/// <summary>
		/// Prim ownership information for a specified owner on a single parcel
		/// </summary>
		public static class PrimOwner
		{
			/// <summary>The <see cref="UUID"/> of the prim owner, 
			/// UUID.Zero if agent has no permission to view prim owner information</summary>
			public UUID OwnerID;
			/// <summary>The total number of prims</summary>
			public int Count;
			/// <summary>True if the OwnerID is a <see cref="Group"/></summary>
			public boolean IsGroupOwned;
			/// <summary>True if the owner is online 
			/// <remarks>This is no longer used by the LL Simulators</remarks></summary>
			public boolean OnlineStatus;
			/// <summary>The date the most recent prim was rezzed</summary>
			public Date TimeStamp;
		}

		/// <summary>An Array of <see cref="PrimOwner"/> objects</summary>
		public PrimOwner[] PrimOwnersBlock;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDArray dataArray = new OSDArray(PrimOwnersBlock.length);
			OSDArray dataExtendedArray = new OSDArray();

			for (int i = 0; i < PrimOwnersBlock.length; i++)
			{
				OSDMap dataMap = new OSDMap(4);
				dataMap.put("OwnerID", OSD.FromUUID(PrimOwnersBlock[i].OwnerID));
				dataMap.put("Count", OSD.FromInteger(PrimOwnersBlock[i].Count));
				dataMap.put("IsGroupOwned", OSD.FromBoolean(PrimOwnersBlock[i].IsGroupOwned));
				dataMap.put("OnlineStatus", OSD.FromBoolean(PrimOwnersBlock[i].OnlineStatus));
				dataArray.add(dataMap);

				OSDMap dataExtendedMap = new OSDMap(1);
				dataExtendedMap.put("TimeStamp", OSD.FromDate(PrimOwnersBlock[i].TimeStamp));
				dataExtendedArray.add(dataExtendedMap);
			}

			OSDMap map = new OSDMap();
			map.put("Data", dataArray);
			if (dataExtendedArray.count() > 0)
				map.put("DataExtended", dataExtendedArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDArray dataArray = (OSDArray)map.get("Data");

			// DataExtended is optional, will not exist of parcel contains zero prims
			OSDArray dataExtendedArray;
			if (map.containsKey("DataExtended"))
			{
				dataExtendedArray = (OSDArray)map.get("DataExtended");
			}
			else
			{
				dataExtendedArray = new OSDArray();
			}

			PrimOwnersBlock = new PrimOwner[dataArray.count()];

			for (int i = 0; i < dataArray.count(); i++)
			{
				OSDMap dataMap = (OSDMap)dataArray.get(i);
				PrimOwner block = new PrimOwner();
				block.OwnerID = dataMap.get("OwnerID").asUUID();
				block.Count = dataMap.get("Count").asInteger();
				block.IsGroupOwned = dataMap.get("IsGroupOwned").asBoolean();
				block.OnlineStatus = dataMap.get("OnlineStatus").asBoolean(); // deprecated

				/* if the agent has no permissions, or there are no prims, the counts
				 * should not match up, so we don't decode the DataExtended map */
				if (dataExtendedArray.count() == dataArray.count())
				{
					OSDMap dataExtendedMap = (OSDMap)dataExtendedArray.get(i);
					block.TimeStamp = Utils.unixTimeToDate(dataExtendedMap.get("TimeStamp").asUInteger());
				}

				PrimOwnersBlock[i] = block;
			}
		}
	}

	/// <summary>
	/// The details of a single parcel in a region, also contains some regionwide globals
	/// </summary>
	public static class ParcelPropertiesMessage implements IMessage, Serializable
	{
		/// <summary>Simulator-local ID of this parcel</summary>
		public int LocalID;
		/// <summary>Maximum corner of the axis-aligned bounding box for this
		/// parcel</summary>
		public Vector3 AABBMax;
		/// <summary>Minimum corner of the axis-aligned bounding box for this
		/// parcel</summary>
		public Vector3 AABBMin;
		/// <summary>Total parcel land area</summary>
		public int Area;
		/// <summary></summary>
		//unit
		public long AuctionID;
		/// <summary>Key of authorized buyer</summary>
		public UUID AuthBuyerID;
		/// <summary>Bitmap describing land layout in 4x4m squares across the 
		/// entire region</summary>
		public byte[] Bitmap;
		/// <summary></summary>
		public ParcelCategory Category;
		/// <summary>Date land was claimed</summary>
		public Date ClaimDate;
		/// <summary>Appears to always be zero</summary>
		public int ClaimPrice;
		/// <summary>Parcel Description</summary>
		public String  Desc;
		/// <summary></summary>
		public EnumSet<ParcelFlags> parcelFlags;
		/// <summary></summary>
		public UUID GroupID;
		/// <summary>Total number of primitives owned by the parcel group on 
		/// this parcel</summary>
		public int GroupPrims;
		/// <summary>Whether the land is deeded to a group or not</summary>
		public boolean IsGroupOwned;
		/// <summary></summary>
		public LandingType landingType;
		/// <summary>Maximum number of primitives this parcel supports</summary>
		public int MaxPrims;
		/// <summary>The Asset UUID of the Texture which when applied to a 
		/// primitive will display the media</summary>
		public UUID MediaID;
		/// <summary>A URL which points to any Quicktime supported media type</summary>
		public String  MediaURL;
		/// <summary>A byte, if 0x1 viewer should auto scale media to fit object</summary>
		public boolean MediaAutoScale;
		/// <summary>URL For Music Stream</summary>
		public String  MusicURL;
		/// <summary>Parcel Name</summary>
		public String  Name;
		/// <summary>Autoreturn value in minutes for others' objects</summary>
		public int OtherCleanTime;
		/// <summary></summary>
		public int OtherCount;
		/// <summary>Total number of other primitives on this parcel</summary>
		public int OtherPrims;
		/// <summary>UUID of the owner of this parcel</summary>
		public UUID OwnerID;
		/// <summary>Total number of primitives owned by the parcel owner on 
		/// this parcel</summary>
		public int OwnerPrims;
		/// <summary></summary>
		public float ParcelPrimBonus;
		/// <summary>How long is pass valid for</summary>
		public float PassHours;
		/// <summary>Price for a temporary pass</summary>
		public int PassPrice;
		/// <summary></summary>
		public int PublicCount;
		/// <summary>Disallows people outside the parcel from being able to see in</summary>
		public boolean Privacy;
		/// <summary></summary>
		public boolean RegionDenyAnonymous;
		/// <summary></summary>
		public boolean RegionDenyIdentified;
		/// <summary></summary>
		public boolean RegionDenyTransacted;
		/// <summary>True if the region denies access to age unverified users</summary>
		public boolean RegionDenyAgeUnverified;
		/// <summary></summary>
		public boolean RegionPushOverride;
		/// <summary>This field is no longer used</summary>
		public int RentPrice;
		/// The result of a request for parcel properties
		public ParcelResult RequestResult;
		/// <summary>Sale price of the parcel, only useful if ForSale is set</summary>
		/// <remarks>The SalePrice will remain the same after an ownership
		/// transfer (sale), so it can be used to see the purchase price after
		/// a sale if the new owner has not changed it</remarks>
		public int SalePrice;
		/// <summary>
		/// Number of primitives your avatar is currently
		/// selecting and sitting on in this parcel
		/// </summary>
		public int SelectedPrims;
		/// <summary></summary>
		public int SelfCount;
		/// <summary>
		/// A number which increments by 1, starting at 0 for each ParcelProperties request. 
		/// Can be overriden by specifying the sequenceID with the ParcelPropertiesRequest being sent. 
		/// a Negative number indicates the action in <seealso cref="ParcelPropertiesStatus"/> has occurred. 
		/// </summary>
		public int SequenceID;
		/// <summary>Maximum primitives across the entire simulator</summary>
		public int SimWideMaxPrims;
		/// <summary>Total primitives across the entire simulator</summary>
		public int SimWideTotalPrims;
		/// <summary></summary>
		public boolean SnapSelection;
		/// <summary>Key of parcel snapshot</summary>
		public UUID SnapshotID;
		/// <summary>Parcel ownership status</summary>
		public ParcelStatus Status;
		/// <summary>Total number of primitives on this parcel</summary>
		public int TotalPrims;
		/// <summary></summary>
		public Vector3 UserLocation;
		/// <summary></summary>
		public Vector3 UserLookAt;
		/// <summary>A description of the media</summary>
		public String  MediaDesc;
		/// <summary>An Integer which represents the height of the media</summary>
		public int MediaHeight;
		/// <summary>An integer which represents the width of the media</summary>
		public int MediaWidth;
		/// <summary>A boolean, if true the viewer should loop the media</summary>
		public boolean MediaLoop;
		/// <summary>A string which contains the mime type of the media</summary>
		public String  MediaType;
		/// <summary>true to obscure (hide) media url</summary>
		public boolean ObscureMedia;
		/// <summary>true to obscure (hide) music url</summary>
		public boolean ObscureMusic;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);

			OSDArray dataArray = new OSDArray(1);
			OSDMap parcelDataMap = new OSDMap(47);
			parcelDataMap.put("LocalID", OSD.FromInteger(LocalID));
			parcelDataMap.put("AABBMax", OSD.FromVector3(AABBMax));
			parcelDataMap.put("AABBMin", OSD.FromVector3(AABBMin));
			parcelDataMap.put("Area", OSD.FromInteger(Area));
			parcelDataMap.put("AuctionID", OSD.FromInteger((int)AuctionID));
			parcelDataMap.put("AuthBuyerID", OSD.FromUUID(AuthBuyerID));
			parcelDataMap.put("Bitmap", OSD.FromBinary(Bitmap));
			parcelDataMap.put("Category", OSD.FromInteger((int)Category.getIndex()));
			parcelDataMap.put("ClaimDate", OSD.FromDate(ClaimDate));
			parcelDataMap.put("ClaimPrice", OSD.FromInteger(ClaimPrice));
			parcelDataMap.put("Desc", OSD.FromString(Desc));
			parcelDataMap.put("ParcelFlags", OSD.FromUInteger(ParcelFlags.getIndex(parcelFlags)));
			parcelDataMap.put("GroupID", OSD.FromUUID(GroupID));
			parcelDataMap.put("GroupPrims", OSD.FromInteger(GroupPrims));
			parcelDataMap.put("IsGroupOwned", OSD.FromBoolean(IsGroupOwned));
			parcelDataMap.put("LandingType", OSD.FromInteger((int)landingType.getIndex()));
			parcelDataMap.put("MaxPrims", OSD.FromInteger(MaxPrims));
			parcelDataMap.put("MediaID", OSD.FromUUID(MediaID));
			parcelDataMap.put("MediaURL", OSD.FromString(MediaURL));
			parcelDataMap.put("MediaAutoScale", OSD.FromBoolean(MediaAutoScale));
			parcelDataMap.put("MusicURL", OSD.FromString(MusicURL));
			parcelDataMap.put("Name", OSD.FromString(Name));
			parcelDataMap.put("OtherCleanTime", OSD.FromInteger(OtherCleanTime));
			parcelDataMap.put("OtherCount", OSD.FromInteger(OtherCount));
			parcelDataMap.put("OtherPrims", OSD.FromInteger(OtherPrims));
			parcelDataMap.put("OwnerID", OSD.FromUUID(OwnerID));
			parcelDataMap.put("OwnerPrims", OSD.FromInteger(OwnerPrims));
			parcelDataMap.put("ParcelPrimBonus", OSD.FromReal((float)ParcelPrimBonus));
			parcelDataMap.put("PassHours", OSD.FromReal((float)PassHours));
			parcelDataMap.put("PassPrice", OSD.FromInteger(PassPrice));
			parcelDataMap.put("PublicCount", OSD.FromInteger(PublicCount));
			parcelDataMap.put("Privacy", OSD.FromBoolean(Privacy));
			parcelDataMap.put("RegionDenyAnonymous", OSD.FromBoolean(RegionDenyAnonymous));
			parcelDataMap.put("RegionDenyIdentified", OSD.FromBoolean(RegionDenyIdentified));
			parcelDataMap.put("RegionDenyTransacted", OSD.FromBoolean(RegionDenyTransacted));
			parcelDataMap.put("RegionPushOverride", OSD.FromBoolean(RegionPushOverride));
			parcelDataMap.put("RentPrice", OSD.FromInteger(RentPrice));
			parcelDataMap.put("RequestResult", OSD.FromInteger((int)RequestResult.getIndex()));
			parcelDataMap.put("SalePrice", OSD.FromInteger(SalePrice));
			parcelDataMap.put("SelectedPrims", OSD.FromInteger(SelectedPrims));
			parcelDataMap.put("SelfCount", OSD.FromInteger(SelfCount));
			parcelDataMap.put("SequenceID", OSD.FromInteger(SequenceID));
			parcelDataMap.put("SimWideMaxPrims", OSD.FromInteger(SimWideMaxPrims));
			parcelDataMap.put("SimWideTotalPrims", OSD.FromInteger(SimWideTotalPrims));
			parcelDataMap.put("SnapSelection", OSD.FromBoolean(SnapSelection));
			parcelDataMap.put("SnapshotID", OSD.FromUUID(SnapshotID));
			parcelDataMap.put("Status", OSD.FromInteger((int)Status.getIndex()));
			parcelDataMap.put("TotalPrims", OSD.FromInteger(TotalPrims));
			parcelDataMap.put("UserLocation", OSD.FromVector3(UserLocation));
			parcelDataMap.put("UserLookAt", OSD.FromVector3(UserLookAt));
			dataArray.add(parcelDataMap);
			map.put("ParcelData", dataArray);

			OSDArray mediaDataArray = new OSDArray(1);
			OSDMap mediaDataMap = new OSDMap(7);
			mediaDataMap.put("MediaDesc", OSD.FromString(MediaDesc));
			mediaDataMap.put("MediaHeight", OSD.FromInteger(MediaHeight));
			mediaDataMap.put("MediaWidth", OSD.FromInteger(MediaWidth));
			mediaDataMap.put("MediaLoop", OSD.FromBoolean(MediaLoop));
			mediaDataMap.put("MediaType", OSD.FromString(MediaType));
			mediaDataMap.put("ObscureMedia", OSD.FromBoolean(ObscureMedia));
			mediaDataMap.put("ObscureMusic", OSD.FromBoolean(ObscureMusic));
			mediaDataArray.add(mediaDataMap);
			map.put("MediaData", mediaDataArray);

			OSDArray ageVerificationBlockArray = new OSDArray(1);
			OSDMap ageVerificationBlockMap = new OSDMap(1);
			ageVerificationBlockMap.put("RegionDenyAgeUnverified", OSD.FromBoolean(RegionDenyAgeUnverified));
			ageVerificationBlockArray.add(ageVerificationBlockMap);
			map.put("AgeVerificationBlock", ageVerificationBlockArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDMap parcelDataMap = (OSDMap)((OSDArray)map.get("ParcelData")).get(0);
			LocalID = parcelDataMap.get("LocalID").asInteger();
			AABBMax = parcelDataMap.get("AABBMax").asVector3();
			AABBMin = parcelDataMap.get("AABBMin").asVector3();
			Area = parcelDataMap.get("Area").asInteger();
			AuctionID = (long)parcelDataMap.get("AuctionID").asInteger();
			AuthBuyerID = parcelDataMap.get("AuthBuyerID").asUUID();
			Bitmap = parcelDataMap.get("Bitmap").asBinary();
			Category = ParcelCategory.get((byte)parcelDataMap.get("Category").asInteger());
			ClaimDate = Utils.unixTimeToDate((long)parcelDataMap.get("ClaimDate").asInteger());
			ClaimPrice = parcelDataMap.get("ClaimPrice").asInteger();
			Desc = parcelDataMap.get("Desc").asString();

			// LL sends this as binary, we'll convert it here
			if (parcelDataMap.get("ParcelFlags").getType().equals(OSDType.Binary))
			{
				byte[] bytes = parcelDataMap.get("ParcelFlags").asBinary();
				//                if (BitConverter.IsLittleEndian)
				//                    Array.Reverse(bytes);
				parcelFlags = ParcelFlags.get(Utils.bytesToUInt(bytes));
				//                ParcelFlags = (ParcelFlags)BitConverter.ToUInt32(bytes, 0);
			}
			else
			{
				parcelFlags = ParcelFlags.get(parcelDataMap.get("ParcelFlags").asUInteger());
			}
			GroupID = parcelDataMap.get("GroupID").asUUID();
			GroupPrims = parcelDataMap.get("GroupPrims").asInteger();
			IsGroupOwned = parcelDataMap.get("IsGroupOwned").asBoolean();
			landingType = LandingType.get((byte)parcelDataMap.get("LandingType").asInteger());
			MaxPrims = parcelDataMap.get("MaxPrims").asInteger();
			MediaID = parcelDataMap.get("MediaID").asUUID();
			MediaURL = parcelDataMap.get("MediaURL").asString();
			MediaAutoScale = parcelDataMap.get("MediaAutoScale").asBoolean(); // 0x1 = yes
			MusicURL = parcelDataMap.get("MusicURL").asString();
			Name = parcelDataMap.get("Name").asString();
			OtherCleanTime = parcelDataMap.get("OtherCleanTime").asInteger();
			OtherCount = parcelDataMap.get("OtherCount").asInteger();
			OtherPrims = parcelDataMap.get("OtherPrims").asInteger();
			OwnerID = parcelDataMap.get("OwnerID").asUUID();
			OwnerPrims = parcelDataMap.get("OwnerPrims").asInteger();
			ParcelPrimBonus = (float)parcelDataMap.get("ParcelPrimBonus").asReal();
			PassHours = (float)parcelDataMap.get("PassHours").asReal();
			PassPrice = parcelDataMap.get("PassPrice").asInteger();
			PublicCount = parcelDataMap.get("PublicCount").asInteger();
			Privacy = parcelDataMap.get("Privacy").asBoolean();
			RegionDenyAnonymous = parcelDataMap.get("RegionDenyAnonymous").asBoolean();
			RegionDenyIdentified = parcelDataMap.get("RegionDenyIdentified").asBoolean();
			RegionDenyTransacted = parcelDataMap.get("RegionDenyTransacted").asBoolean();
			RegionPushOverride = parcelDataMap.get("RegionPushOverride").asBoolean();
			RentPrice = parcelDataMap.get("RentPrice").asInteger();
			RequestResult = ParcelResult.get(parcelDataMap.get("RequestResult").asInteger());
			SalePrice = parcelDataMap.get("SalePrice").asInteger();
			SelectedPrims = parcelDataMap.get("SelectedPrims").asInteger();
			SelfCount = parcelDataMap.get("SelfCount").asInteger();
			SequenceID = parcelDataMap.get("SequenceID").asInteger();
			SimWideMaxPrims = parcelDataMap.get("SimWideMaxPrims").asInteger();
			SimWideTotalPrims = parcelDataMap.get("SimWideTotalPrims").asInteger();
			SnapSelection = parcelDataMap.get("SnapSelection").asBoolean();
			SnapshotID = parcelDataMap.get("SnapshotID").asUUID();
			Status = ParcelStatus.get((byte)parcelDataMap.get("Status").asInteger());
			TotalPrims = parcelDataMap.get("TotalPrims").asInteger();
			UserLocation = parcelDataMap.get("UserLocation").asVector3();
			UserLookAt = parcelDataMap.get("UserLookAt").asVector3();

			if (map.containsKey("MediaData")) // temporary, OpenSim doesn't send this block
			{
				OSDMap mediaDataMap = (OSDMap)((OSDArray)map.get("MediaData")).get(0);
				MediaDesc = mediaDataMap.get("MediaDesc").asString();
				MediaHeight = mediaDataMap.get("MediaHeight").asInteger();
				MediaWidth = mediaDataMap.get("MediaWidth").asInteger();
				MediaLoop = mediaDataMap.get("MediaLoop").asBoolean();
				MediaType = mediaDataMap.get("MediaType").asString();
				ObscureMedia = mediaDataMap.get("ObscureMedia").asBoolean();
				ObscureMusic = mediaDataMap.get("ObscureMusic").asBoolean();
			}

			OSDMap ageVerificationBlockMap = (OSDMap)((OSDArray)map.get("AgeVerificationBlock")).get(0);
			RegionDenyAgeUnverified = ageVerificationBlockMap.get("RegionDenyAgeUnverified").asBoolean();
		}
	}

	/// <summary>A message sent from the viewer to the simulator to updated a specific parcels settings</summary>
	public static class ParcelPropertiesUpdateMessage implements IMessage
	{
		/// <summary>The <seealso cref="UUID"/> of the agent authorized to purchase this
		/// parcel of land or a NULL <seealso cref="UUID"/> if the sale is authorized to anyone</summary>
		public UUID AuthBuyerID;
		/// <summary>true to enable auto scaling of the parcel media</summary>
		public boolean MediaAutoScale;
		/// <summary>The category of this parcel used when search is enabled to restrict
		/// search results</summary>
		public ParcelCategory Category;
		/// <summary>A string containing the description to set</summary>
		public String  Desc;
		/// <summary>The <seealso cref="UUID"/> of the <seealso cref="Group"/> which allows for additional
		/// powers and restrictions.</summary>
		public UUID GroupID;
		/// <summary>The <seealso cref="LandingType"/> which specifies how avatars which teleport
		/// to this parcel are handled</summary>
		public LandingType Landing;
		/// <summary>The LocalID of the parcel to update settings on</summary>
		public int LocalID;
		/// <summary>A string containing the description of the media which can be played
		/// to visitors</summary>
		public String  MediaDesc;
		/// <summary></summary>
		public int MediaHeight;
		/// <summary></summary>
		public boolean MediaLoop;
		/// <summary></summary>
		public UUID MediaID;
		/// <summary></summary>
		public String  MediaType;
		/// <summary></summary>
		public String  MediaURL;
		/// <summary></summary>
		public int MediaWidth;
		/// <summary></summary>
		public String  MusicURL;
		/// <summary></summary>
		public String  Name;
		/// <summary></summary>
		public boolean ObscureMedia;
		/// <summary></summary>
		public boolean ObscureMusic;
		/// <summary></summary>
		public EnumSet<ParcelFlags> parcelFlags;
		/// <summary></summary>
		public float PassHours;
		/// <summary></summary>
		//uint
		public long PassPrice;
		/// <summary></summary>
		public boolean Privacy;
		/// <summary></summary>
		//uint
		public long SalePrice;
		/// <summary></summary>
		public UUID SnapshotID;
		/// <summary></summary>
		public Vector3 UserLocation;
		/// <summary></summary>
		public Vector3 UserLookAt;

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			AuthBuyerID = map.get("auth_buyer_id").asUUID();
			MediaAutoScale = map.get("auto_scale").asBoolean();
			Category = ParcelCategory.get((byte)map.get("category").asInteger());
			Desc = map.get("description").asString();
			GroupID = map.get("group_id").asUUID();
			Landing = LandingType.get((byte)map.get("landing_type").asUInteger());
			LocalID = map.get("local_id").asInteger();
			MediaDesc = map.get("media_desc").asString();
			MediaHeight = map.get("media_height").asInteger();
			MediaLoop = map.get("media_loop").asBoolean();
			MediaID = map.get("media_id").asUUID();
			MediaType = map.get("media_type").asString();
			MediaURL = map.get("media_url").asString();
			MediaWidth = map.get("media_width").asInteger();
			MusicURL = map.get("music_url").asString();
			Name = map.get("name").asString();
			ObscureMedia = map.get("obscure_media").asBoolean();
			ObscureMusic = map.get("obscure_music").asBoolean();
			parcelFlags = ParcelFlags.get(map.get("parcel_flags").asUInteger());
			PassHours = (float)map.get("pass_hours").asReal();
			PassPrice = map.get("pass_price").asUInteger();
			Privacy = map.get("privacy").asBoolean();
			SalePrice = map.get("sale_price").asUInteger();
			SnapshotID = map.get("snapshot_id").asUUID();
			UserLocation = map.get("user_location").asVector3();
			UserLookAt = map.get("user_look_at").asVector3();
		}

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();
			map.put("auth_buyer_id", OSD.FromUUID(AuthBuyerID));
			map.put("auto_scale", OSD.FromBoolean(MediaAutoScale));
			map.put("category", OSD.FromInteger((byte)Category.getIndex()));
			map.put("description", OSD.FromString(Desc));
			map.put("flags", OSD.FromBinary(Utils.EmptyBytes));
			map.put("group_id", OSD.FromUUID(GroupID));
			map.put("landing_type", OSD.FromInteger((byte)Landing.getIndex()));
			map.put("local_id", OSD.FromInteger(LocalID));
			map.put("media_desc", OSD.FromString(MediaDesc));
			map.put("media_height", OSD.FromInteger(MediaHeight));
			map.put("media_id", OSD.FromUUID(MediaID));
			map.put("media_loop", OSD.FromBoolean(MediaLoop));
			map.put("media_type", OSD.FromString(MediaType));
			map.put("media_url", OSD.FromString(MediaURL));
			map.put("media_width", OSD.FromInteger(MediaWidth));
			map.put("music_url", OSD.FromString(MusicURL));
			map.put("name", OSD.FromString(Name));
			map.put("obscure_media", OSD.FromBoolean(ObscureMedia));
			map.put("obscure_music", OSD.FromBoolean(ObscureMusic));
			map.put("parcel_flags", OSD.FromUInteger(ParcelFlags.getIndex(parcelFlags)));
			map.put("pass_hours", OSD.FromReal(PassHours));
			map.put("privacy", OSD.FromBoolean(Privacy));
			map.put("pass_price", OSD.FromInteger((int)PassPrice));
			map.put("sale_price", OSD.FromInteger((int)SalePrice));
			map.put("snapshot_id", OSD.FromUUID(SnapshotID));
			map.put("user_location", OSD.FromVector3(UserLocation));
			map.put("user_look_at", OSD.FromVector3(UserLookAt));

			return map;
		}
	}

	/// <summary>Base class used for the RemoteParcelRequest message</summary>
	public static abstract class RemoteParcelRequestBlock implements Serializable
	{
		public abstract OSDMap Serialize();
		public abstract void Deserialize(OSDMap map);
	}

	/// <summary>
	/// A message sent from the viewer to the simulator to request information
	/// on a remote parcel
	/// </summary>
	public static class RemoteParcelRequestRequest extends RemoteParcelRequestBlock
	{
		/// <summary>Local sim position of the parcel we are looking up</summary>
		public Vector3 Location;
		/// <summary>Region handle of the parcel we are looking up</summary>
		//ulong
		public BigInteger RegionHandle;
		/// <summary>Region <see cref="UUID"/> of the parcel we are looking up</summary>
		public UUID RegionID;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);
			map.put("location", OSD.FromVector3(Location));
			map.put("region_handle", OSD.FromULong(RegionHandle));
			map.put("region_id", OSD.FromUUID(RegionID));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		@Override
		public void Deserialize(OSDMap map)
		{
			Location = map.get("location").asVector3();
			RegionHandle = map.get("region_handle").asULong();
			RegionID = map.get("region_id").asUUID();
		}
	}

	/// <summary>
	/// A message sent from the simulator to the viewer in response to a <see cref="RemoteParcelRequestRequest"/> 
	/// which will contain parcel information
	/// </summary>
	//    [Serializable]
	public static class RemoteParcelRequestReply extends RemoteParcelRequestBlock implements Serializable
	{
		/// <summary>The grid-wide unique parcel ID</summary>
		public UUID ParcelID;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			map.put("parcel_id", OSD.FromUUID(ParcelID));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		@Override
		public void Deserialize(OSDMap map)
		{
			if (map == null || !map.containsKey("parcel_id"))
				ParcelID =  UUID.Zero;
			else
				ParcelID = map.get("parcel_id").asUUID();
		}
	}

	/// <summary>
	/// A message containing a request for a remote parcel from a viewer, or a response
	/// from the simulator to that request
	/// </summary>
	//    [Serializable]
	public static class RemoteParcelRequestMessage implements IMessage, Serializable
	{
		/// <summary>The request or response details block</summary>
		public RemoteParcelRequestBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("parcel_id"))
				Request = new RemoteParcelRequestReply();
			else if (map.containsKey("location"))
				Request = new RemoteParcelRequestRequest();
			else
				JLogger.warn("Unable to deserialize RemoteParcelRequest: No message handler exists for method: " + map.asString());

			if (Request != null)
				Request.Deserialize(map);
		}
	}
	//endregion


	//region Inventory Messages

	public static class NewFileAgentInventoryMessage implements IMessage
	{
		public UUID FolderID;
		public AssetType assetType;
		public InventoryType inventoryType;
		public String  Name;
		public String  Description;
		public EnumSet<PermissionMask> EveryoneMask;
		public EnumSet<PermissionMask> GroupMask;
		public EnumSet<PermissionMask> NextOwnerMask;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(5);
			map.put("folder_id", OSD.FromUUID(FolderID));
			//            map.put("asset_type", OSD.FromString(Utils.AssetTypeToString(AssetType)));
			//            map.put("inventory_type", OSD.FromString(Utils.InventoryTypeToString(InventoryType)));
			map.put("asset_type", OSD.FromString(assetType.toString()));
			map.put("inventory_type", OSD.FromString(inventoryType.toString()));
			map.put("name", OSD.FromString(Name));
			map.put("description", OSD.FromString(Description));
			map.put("everyone_mask", OSD.FromInteger((int)PermissionMask.getIndex(EveryoneMask)));
			map.put("group_mask", OSD.FromInteger((int)PermissionMask.getIndex(GroupMask)));
			map.put("next_owner_mask", OSD.FromInteger((int)PermissionMask.getIndex(NextOwnerMask)));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			FolderID = map.get("folder_id").asUUID();
			assetType = AssetType.valueOf(map.get("asset_type").asString());
			inventoryType = InventoryType.valueOf(map.get("inventory_type").asString());
			Name = map.get("name").asString();
			Description = map.get("description").asString();
			EveryoneMask = PermissionMask.get((long)map.get("everyone_mask").asInteger());
			GroupMask = PermissionMask.get((long)map.get("group_mask").asInteger());
			NextOwnerMask = PermissionMask.get((long)map.get("next_owner_mask").asInteger());
		}
	}

	public static class NewFileAgentInventoryReplyMessage implements IMessage
	{
		public String  State;
		public URI Uploader;

		public NewFileAgentInventoryReplyMessage()
		{
			State = "upload";
		}

		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();
			map.put("state", OSD.FromString(State));
			map.put("uploader", OSD.FromUri(Uploader));

			return map;
		}

		public void Deserialize(OSDMap map)
		{
			State = map.get("state").asString();
			Uploader = map.get("uploader").asUri();
		}
	}

	public static class NewFileAgentInventoryVariablePriceMessage implements IMessage
	{
		public UUID FolderID;
		public AssetType assetType;
		public InventoryType inventoryType;
		public String  Name;
		public String  Description;
		public EnumSet<PermissionMask> EveryoneMask;
		public EnumSet<PermissionMask> GroupMask;
		public EnumSet<PermissionMask> NextOwnerMask;
		// TODOextends Asset_resources?

				/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();
			map.put("folder_id", OSD.FromUUID(FolderID));
			map.put("asset_type", OSD.FromString(assetType.toString()));
			map.put("inventory_type", OSD.FromString(inventoryType.toString()));
			map.put("name", OSD.FromString(Name));
			map.put("description", OSD.FromString(Description));
			map.put("everyone_mask", OSD.FromInteger((int)PermissionMask.getIndex(EveryoneMask)));
			map.put("group_mask", OSD.FromInteger((int)PermissionMask.getIndex(GroupMask)));
			map.put("next_owner_mask", OSD.FromInteger((int)PermissionMask.getIndex(NextOwnerMask)));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			FolderID = map.get("folder_id").asUUID();
			assetType = AssetType.valueOf((map.get("asset_type").asString()));
			inventoryType = InventoryType.valueOf((map.get("inventory_type").asString()));
			Name = map.get("name").asString();
			Description = map.get("description").asString();
			EveryoneMask = PermissionMask.get((long)map.get("everyone_mask").asInteger());
			GroupMask = PermissionMask.get((long)map.get("group_mask").asInteger());
			NextOwnerMask = PermissionMask.get((long)map.get("next_owner_mask").asInteger());
		}
	}

	public static class NewFileAgentInventoryVariablePriceReplyMessage implements IMessage
	{
		public int ResourceCost;
		public String  State;
		public int UploadPrice;
		public URI Rsvp;

		public NewFileAgentInventoryVariablePriceReplyMessage()
		{
			State = "confirm_upload";
		}

		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();
			map.put("resource_cost", OSD.FromInteger(ResourceCost));
			map.put("state", OSD.FromString(State));
			map.put("upload_price", OSD.FromInteger(UploadPrice));
			map.put("rsvp", OSD.FromUri(Rsvp));

			return map;
		}

		public void Deserialize(OSDMap map)
		{
			ResourceCost = map.get("resource_cost").asInteger();
			State = map.get("state").asString();
			UploadPrice = map.get("upload_price").asInteger();
			Rsvp = map.get("rsvp").asUri();
		}
	}

	public static class NewFileAgentInventoryUploadReplyMessage implements IMessage
	{
		public UUID NewInventoryItem;
		public UUID NewAsset;
		public String  State;
		public EnumSet<PermissionMask> NewBaseMask;
		public EnumSet<PermissionMask> NewEveryoneMask;
		public EnumSet<PermissionMask> NewOwnerMask;
		public EnumSet<PermissionMask> NewNextOwnerMask;

		public NewFileAgentInventoryUploadReplyMessage()
		{
			State = "complete";
		}

		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();
			map.put("new_inventory_item", OSD.FromUUID(NewInventoryItem));
			map.put("new_asset", OSD.FromUUID(NewAsset));
			map.put("state", OSD.FromString(State));
			map.put("new_base_mask", OSD.FromInteger((int)PermissionMask.getIndex(NewBaseMask)));
			map.put("new_everyone_mask", OSD.FromInteger((int)PermissionMask.getIndex(NewEveryoneMask)));
			map.put("new_owner_mask", OSD.FromInteger((int)PermissionMask.getIndex(NewOwnerMask)));
			map.put("new_next_owner_mask", OSD.FromInteger((int)PermissionMask.getIndex(NewNextOwnerMask)));

			return map;
		}

		public void Deserialize(OSDMap map)
		{
			NewInventoryItem = map.get("new_inventory_item").asUUID();
			NewAsset = map.get("new_asset").asUUID();
			State = map.get("state").asString();
			NewBaseMask = PermissionMask.get((long)map.get("new_base_mask").asInteger());
			NewEveryoneMask = PermissionMask.get((long)map.get("new_everyone_mask").asInteger());
			NewOwnerMask = PermissionMask.get((long)map.get("new_owner_mask").asInteger());
			NewNextOwnerMask = PermissionMask.get((long)map.get("new_next_owner_mask").asInteger());
		}
	}

	//endregion

	//region Agent Messages

	/// <summary>
	/// A message sent from the simulator to an agent which contains
	/// the groups the agent is in
	/// </summary>
	public static class AgentGroupDataUpdateMessage implements IMessage
	{
		/// <summary>The Agent receiving the message</summary>
		public UUID AgentID;

		/// <summary>Group Details specific to the agent</summary>
		public static class GroupData
		{
			/// <summary>true of the agent accepts group notices</summary>
			public boolean AcceptNotices;
			/// <summary>The agents tier contribution to the group</summary>
			public int Contribution;
			/// <summary>The Groups <seealso cref="UUID"/></summary>
			public UUID GroupID;
			/// <summary>The <seealso cref="UUID"/> of the groups insignia</summary>
			public UUID GroupInsigniaID;
			/// <summary>The name of the group</summary>
			public String  GroupName;
			/// <summary>The aggregate permissions the agent has in the group for all roles the agent
			/// is assigned</summary>
			public EnumSet<GroupPowers> GroupPowers;
		}

		/// <summary>An optional block containing additional agent specific information</summary>
		public static class NewGroupData
		{
			/// <summary>true of the agent allows this group to be
			/// listed in their profile</summary>
			public boolean ListInProfile;
		}

		/// <summary>An array containing <seealso cref="GroupData"/> information
		/// for each <see cref="Group"/> the agent is a member of</summary>
		public GroupData[] GroupDataBlock;
		/// <summary>An array containing <seealso cref="NewGroupData"/> information
		/// for each <see cref="Group"/> the agent is a member of</summary>
		public NewGroupData[] NewGroupDataBlock;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);

			OSDMap agent = new OSDMap(1);
			agent.put("AgentID", OSD.FromUUID(AgentID));

			OSDArray agentArray = new OSDArray();
			agentArray.add(agent);

			map.put("AgentData", agentArray);

			OSDArray groupDataArray = new OSDArray(GroupDataBlock.length);

			for (int i = 0; i < GroupDataBlock.length; i++)
			{
				OSDMap group = new OSDMap(6);
				group.put("AcceptNotices", OSD.FromBoolean(GroupDataBlock[i].AcceptNotices));
				group.put("Contribution", OSD.FromInteger(GroupDataBlock[i].Contribution));
				group.put("GroupID", OSD.FromUUID(GroupDataBlock[i].GroupID));
				group.put("GroupInsigniaID", OSD.FromUUID(GroupDataBlock[i].GroupInsigniaID));
				group.put("GroupName", OSD.FromString(GroupDataBlock[i].GroupName));

				group.put("GroupPowers", OSD.FromLong(GroupPowers.getIndex(GroupDataBlock[i].GroupPowers)));
				groupDataArray.add(group);
			}

			map.put("GroupData", groupDataArray);

			OSDArray newGroupDataArray = new OSDArray(NewGroupDataBlock.length);

			for (int i = 0; i < NewGroupDataBlock.length; i++)
			{
				OSDMap group = new OSDMap(1);
				group.put("ListInProfile", OSD.FromBoolean(NewGroupDataBlock[i].ListInProfile));
				newGroupDataArray.add(group);
			}

			map.put("NewGroupData", newGroupDataArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDArray agentArray = (OSDArray)map.get("AgentData");
			OSDMap agentMap = (OSDMap)agentArray.get(0);
			AgentID = agentMap.get("AgentID").asUUID();

			OSDArray groupArray = (OSDArray)map.get("GroupData");

			GroupDataBlock = new GroupData[groupArray.count()];

			for (int i = 0; i < groupArray.count(); i++)
			{
				OSDMap groupMap = (OSDMap)groupArray.get(i);

				GroupData groupData = new GroupData();

				groupData.GroupID = groupMap.get("GroupID").asUUID();
				groupData.Contribution = groupMap.get("Contribution").asInteger();
				groupData.GroupInsigniaID = groupMap.get("GroupInsigniaID").asUUID();
				groupData.GroupName = groupMap.get("GroupName").asString();
				groupData.GroupPowers = GroupPowers.get(groupMap.get("GroupPowers").asLong());
				groupData.AcceptNotices = groupMap.get("AcceptNotices").asBoolean();
				GroupDataBlock[i] = groupData;
			}

			// If request for current groups came very close to login
			// the Linden sim will not include the NewGroupData block, but
			// it will instead set all ListInProfile fields to false
			if (map.containsKey("NewGroupData"))
			{
				OSDArray newGroupArray = (OSDArray)map.get("NewGroupData");

				NewGroupDataBlock = new NewGroupData[newGroupArray.count()];

				for (int i = 0; i < newGroupArray.count(); i++)
				{
					OSDMap newGroupMap = (OSDMap)newGroupArray.get(i);
					NewGroupData newGroupData = new NewGroupData();
					newGroupData.ListInProfile = newGroupMap.get("ListInProfile").asBoolean();
					NewGroupDataBlock[i] = newGroupData;
				}
			}
			else
			{
				NewGroupDataBlock = new NewGroupData[GroupDataBlock.length];
				for (int i = 0; i < NewGroupDataBlock.length; i++)
				{
					NewGroupData newGroupData = new NewGroupData();
					newGroupData.ListInProfile = false;
					NewGroupDataBlock[i] = newGroupData;
				}
			}
		}
	}

	/// <summary>
	/// A message sent from the viewer to the simulator which 
	/// specifies the language and permissions for others to detect
	/// the language specified
	/// </summary>
	public static class UpdateAgentLanguageMessage implements IMessage
	{
		/// <summary>A string containng the default language 
		/// to use for the agent</summary>
		public String  Language;
		/// <summary>true of others are allowed to
		/// know the language setting</summary>
		public boolean LanguagePublic;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);

			map.put("language", OSD.FromString(Language));
			map.put("language_is_public", OSD.FromBoolean(LanguagePublic));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			LanguagePublic = map.get("language_is_public").asBoolean();
			Language = map.get("language").asString();
		}
	}

	/// <summary>
	/// An EventQueue message sent from the simulator to an agent when the agent
	/// leaves a group
	/// </summary>
	public static class AgentDropGroupMessage implements IMessage
	{
		/// <summary>An object containing the Agents UUID, and the Groups UUID</summary>
		public static class AgentData
		{
			/// <summary>The ID of the Agent leaving the group</summary>
			public UUID AgentID;
			/// <summary>The GroupID the Agent is leaving</summary>
			public UUID GroupID;
		}

		/// <summary>
		/// An Array containing the AgentID and GroupID
		/// </summary>
		public AgentData[] AgentDataBlock;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);

			OSDArray agentDataArray = new OSDArray(AgentDataBlock.length);

			for (int i = 0; i < AgentDataBlock.length; i++)
			{
				OSDMap agentMap = new OSDMap(2);
				agentMap.put("AgentID", OSD.FromUUID(AgentDataBlock[i].AgentID));
				agentMap.put("GroupID", OSD.FromUUID(AgentDataBlock[i].GroupID));
				agentDataArray.add(agentMap);
			}
			map.put("AgentData", agentDataArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDArray agentDataArray = (OSDArray)map.get("AgentData");

			AgentDataBlock = new AgentData[agentDataArray.count()];

			for (int i = 0; i < agentDataArray.count(); i++)
			{
				OSDMap agentMap = (OSDMap)agentDataArray.get(i);
				AgentData agentData = new AgentData();

				agentData.AgentID = agentMap.get("AgentID").asUUID();
				agentData.GroupID = agentMap.get("GroupID").asUUID();

				AgentDataBlock[i] = agentData;
			}
		}
	}

	/// <summary>Base class for Asset uploads/results via Capabilities</summary>
	public static abstract class AssetUploaderBlock
	{
		/// <summary>
		/// The request state
		/// </summary>
		public String  State;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public abstract OSDMap Serialize();

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public abstract void Deserialize(OSDMap map);
	}

	/// <summary>
	/// A message sent from the viewer to the simulator to request a temporary upload capability
	/// which allows an asset to be uploaded
	/// </summary>
	public static class UploaderRequestUpload extends AssetUploaderBlock
	{
		/// <summary>The Capability URL sent by the simulator to upload the baked texture to</summary>
		public URI Url;

		public UploaderRequestUpload()
		{
			State = "upload";
		}

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("state", OSD.FromString(State));
			map.put("uploader", OSD.FromUri(Url));

			return map;
		}

		@Override
		public void Deserialize(OSDMap map)
		{
			Url = map.get("uploader").asUri();
			State = map.get("state").asString();
		}
	}

	/// <summary>
	/// A message sent from the simulator that will inform the agent the upload is complete, 
	/// and the UUID of the uploaded asset
	/// </summary>
	public static class UploaderRequestComplete extends AssetUploaderBlock
	{
		/// <summary>The uploaded texture asset ID</summary>
		public UUID AssetID;

		public UploaderRequestComplete()
		{
			State = "complete";
		}

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("state", OSD.FromString(State));
			map.put("new_asset", OSD.FromUUID(AssetID));

			return map;
		}

		@Override
		public void Deserialize(OSDMap map)
		{
			AssetID = map.get("new_asset").asUUID();
			State = map.get("state").asString();
		}
	}

	/// <summary>
	/// A message sent from the viewer to the simulator to request a temporary
	/// capability URI which is used to upload an agents baked appearance textures
	/// </summary>
	public static class UploadBakedTextureMessage implements IMessage
	{
		/// <summary>Object containing request or response</summary>
		public AssetUploaderBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("state") && map.get("state").asString().equals("upload"))
				Request = new UploaderRequestUpload();
			else if (map.containsKey("state") && map.get("state").asString().equals("complete"))
				Request = new UploaderRequestComplete();
			else
				JLogger.warn("Unable to deserialize UploadBakedTexture: No message handler exists for state " + map.get("state").asString());

			if (Request != null)
				Request.Deserialize(map);
		}
	}
	//endregion

	//region Voice Messages
	/// <summary>
	/// A message sent from the simulator which indicates the minimum version required for 
	/// using voice chat
	/// </summary>
	public static class RequiredVoiceVersionMessage implements IMessage
	{
		/// <summary>Major Version Required</summary>
		public int MajorVersion;
		/// <summary>Minor version required</summary>
		public int MinorVersion;
		/// <summary>The name of the region sending the version requrements</summary>
		public String  RegionName;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(4);
			map.put("major_version", OSD.FromInteger(MajorVersion));
			map.put("minor_version", OSD.FromInteger(MinorVersion));
			map.put("region_name", OSD.FromString(RegionName));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			MajorVersion = map.get("major_version").asInteger();
			MinorVersion = map.get("minor_version").asInteger();
			RegionName = map.get("region_name").asString();
		}
	}

	/// <summary>
	/// A message sent from the simulator to the viewer containing the 
	/// voice server URI
	/// </summary>
	public static class ParcelVoiceInfoRequestMessage implements IMessage
	{
		/// <summary>The Parcel ID which the voice server URI applies</summary>
		public int ParcelID;
		/// <summary>The name of the region</summary>
		public String  RegionName;
		/// <summary>A uri containing the server/channel information
		/// which the viewer can utilize to participate in voice conversations</summary>
		public URI SipChannelUri;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);
			map.put("parcel_local_id", OSD.FromInteger(ParcelID));
			map.put("region_name", OSD.FromString(RegionName));

			OSDMap vcMap = new OSDMap(1);
			vcMap.put("channel_uri", OSD.FromUri(SipChannelUri));

			map.put("voice_credentials", vcMap);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			ParcelID = map.get("parcel_local_id").asInteger();
			RegionName = map.get("region_name").asString();

			OSDMap vcMap = (OSDMap)map.get("voice_credentials");
			SipChannelUri = vcMap.get("channel_uri").asUri();
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public static class ProvisionVoiceAccountRequestMessage implements IMessage
	{
		/// <summary></summary>
		public String  Password;
		/// <summary></summary>
		public String  Username;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);

			map.put("username", OSD.FromString(Username));
			map.put("password", OSD.FromString(Password));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			Username = map.get("username").asString();
			Password = map.get("password").asString();
		}
	}

	//endregion

	//region Script/Notecards Messages
	/// <summary>
	/// A message sent by the viewer to the simulator to request a temporary
	/// capability for a script contained with in a Tasks inventory to be updated
	/// </summary>
	public static class UploadScriptTaskMessage implements IMessage
	{
		/// <summary>Object containing request or response</summary>
		public AssetUploaderBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("state") && map.get("state").equals("upload"))
				Request = new UploaderRequestUpload();
			else if (map.containsKey("state") && map.get("state").equals("complete"))
				Request = new UploaderRequestComplete();
			else
				JLogger.warn("Unable to deserialize UploadScriptTask: No message handler exists for state " + map.get("state").asString());

			Request.Deserialize(map);
		}
	}

	/// <summary>
	/// A message sent from the simulator to the viewer to indicate
	/// a Tasks scripts status.
	/// </summary>
	public static class ScriptRunningReplyMessage implements IMessage
	{
		/// <summary>The Asset ID of the script</summary>
		public UUID ItemID;
		/// <summary>True of the script is compiled/ran using the mono interpreter, false indicates it 
		/// uses the older less efficient lsl2 interprter</summary>
		public boolean Mono;
		/// <summary>The Task containing the scripts <seealso cref="UUID"/></summary>
		public UUID ObjectID;
		/// <summary>true of the script is in a running state</summary>
		public boolean Running;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);

			OSDMap scriptMap = new OSDMap(4);
			scriptMap.put("ItemID", OSD.FromUUID(ItemID));
			scriptMap.put("Mono", OSD.FromBoolean(Mono));
			scriptMap.put("ObjectID", OSD.FromUUID(ObjectID));
			scriptMap.put("Running", OSD.FromBoolean(Running));

			OSDArray scriptArray = new OSDArray(1);
			scriptArray.add((OSD)scriptMap);

			map.put("Script", scriptArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDArray scriptArray = (OSDArray)map.get("Script");

			OSDMap scriptMap = (OSDMap)scriptArray.get(0);

			ItemID = scriptMap.get("ItemID").asUUID();
			Mono = scriptMap.get("Mono").asBoolean();
			ObjectID = scriptMap.get("ObjectID").asUUID();
			Running = scriptMap.get("Running").asBoolean();
		}
	}

	/// <summary>
	/// A message containing the request/response used for updating a gesture
	/// contained with an agents inventory
	/// </summary>
	public static class UpdateGestureAgentInventoryMessage implements IMessage
	{
		/// <summary>Object containing request or response</summary>
		public AssetUploaderBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("item_id"))
				Request = new UpdateAgentInventoryRequestMessage();
			else if (map.containsKey("state") && map.get("state").asString().equals("upload"))
				Request = new UploaderRequestUpload();
			else if (map.containsKey("state") && map.get("state").asString().equals("complete"))
				Request = new UploaderRequestComplete();
			else
				JLogger.warn("Unable to deserialize UpdateGestureAgentInventory: No message handler exists: " + map.asString());

			if (Request != null)
				Request.Deserialize(map);
		}
	}

	/// <summary>
	/// A message request/response which is used to update a notecard contained within
	/// a tasks inventory
	/// </summary>
	public static class UpdateNotecardTaskInventoryMessage implements IMessage
	{
		/// <summary>The <seealso cref="UUID"/> of the Task containing the notecard asset to update</summary>
		public UUID TaskID;
		/// <summary>The notecard assets <seealso cref="UUID"/> contained in the tasks inventory</summary>
		public UUID ItemID;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			map.put("task_id", OSD.FromUUID(TaskID));
			map.put("item_id", OSD.FromUUID(ItemID));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			TaskID = map.get("task_id").asUUID();
			ItemID = map.get("item_id").asUUID();
		}
	}

	// TODO: Add Test
	/// <summary>
	/// A reusable class containing a message sent from the viewer to the simulator to request a temporary uploader capability
	/// which is used to update an asset in an agents inventory
	/// </summary>
	public static class UpdateAgentInventoryRequestMessage extends AssetUploaderBlock
	{
		/// <summary>
		/// The Notecard AssetID to replace
		/// </summary>
		public UUID ItemID;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			map.put("item_id", OSD.FromUUID(ItemID));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		@Override
		public void Deserialize(OSDMap map)
		{
			ItemID = map.get("item_id").asUUID();
		}
	}

	/// <summary>
	/// A message containing the request/response used for updating a notecard
	/// contained with an agents inventory
	/// </summary>
	public static class UpdateNotecardAgentInventoryMessage implements IMessage
	{
		/// <summary>Object containing request or response</summary>
		public AssetUploaderBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("item_id"))
				Request = new UpdateAgentInventoryRequestMessage();
			else if (map.containsKey("state") && map.get("state").asString().equals("upload"))
				Request = new UploaderRequestUpload();
			else if (map.containsKey("state") && map.get("state").asString().equals("complete"))
				Request = new UploaderRequestComplete();
			else
				JLogger.warn("Unable to deserialize UpdateNotecardAgentInventory: No message handler exists for state " + map.get("state").asString());

			if (Request != null)
				Request.Deserialize(map);
		}
	}

	public static class CopyInventoryFromNotecardMessage implements IMessage
	{
		public int CallbackID;
		public UUID FolderID;
		public UUID ItemID;
		public UUID NotecardID;
		public UUID ObjectID;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(5);
			map.put("callback-id", OSD.FromInteger(CallbackID));
			map.put("folder-id", OSD.FromUUID(FolderID));
			map.put("item-id", OSD.FromUUID(ItemID));
			map.put("notecard-id", OSD.FromUUID(NotecardID));
			map.put("object-id", OSD.FromUUID(ObjectID));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			CallbackID = map.get("callback-id").asInteger();
			FolderID = map.get("folder-id").asUUID();
			ItemID = map.get("item-id").asUUID();
			NotecardID = map.get("notecard-id").asUUID();
			ObjectID = map.get("object-id").asUUID();
		}
	}

	/// <summary>
	/// A message sent from the simulator to the viewer which indicates
	/// an error occurred while attempting to update a script in an agents or tasks 
	/// inventory
	/// </summary>
	public static class UploaderScriptRequestError extends AssetUploaderBlock
	{
		/// <summary>true of the script was successfully compiled by the simulator</summary>
		public boolean Compiled;
		/// <summary>A string containing the error which occured while trying
		/// to update the script</summary>
		public String  Error;
		/// <summary>A new AssetID assigned to the script</summary>
		public UUID AssetID;

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(4);
			map.put("state", OSD.FromString(State));
			map.put("new_asset", OSD.FromUUID(AssetID));
			map.put("compiled", OSD.FromBoolean(Compiled));

			OSDArray errorsArray = new OSDArray();
			errorsArray.add(OSD.FromString(Error));


			map.put("errors", errorsArray);
			return map;
		}

		@Override
		public void Deserialize(OSDMap map)
		{
			AssetID = map.get("new_asset").asUUID();
			Compiled = map.get("compiled").asBoolean();
			State = map.get("state").asString();

			OSDArray errorsArray = (OSDArray)map.get("errors");
			Error = errorsArray.get(0).asString();
		}
	}

	/// <summary>
	/// A message sent from the viewer to the simulator
	/// requesting the update of an existing script contained
	/// within a tasks inventory
	/// </summary
	public static class UpdateScriptTaskUpdateMessage extends AssetUploaderBlock
	{
		/// <summary>if true, set the script mode to running</summary>
		public boolean ScriptRunning;
		/// <summary>The scripts InventoryItem ItemID to update</summary>
		public UUID ItemID;
		/// <summary>A lowercase string containing either "mono" or "lsl2" which 
		/// specifies the script is compiled and ran on the mono runtime, or the older
		/// lsl runtime</summary>
		public String  Target; // mono or lsl2
		/// <summary>The tasks <see cref="UUID"/> which contains the script to update</summary>
		public UUID TaskID;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(4);
			map.put("is_script_running", OSD.FromBoolean(ScriptRunning));
			map.put("item_id", OSD.FromUUID(ItemID));
			map.put("target", OSD.FromString(Target));
			map.put("task_id", OSD.FromUUID(TaskID));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		@Override
		public void Deserialize(OSDMap map)
		{
			ScriptRunning = map.get("is_script_running").asBoolean();
			ItemID = map.get("item_id").asUUID();
			Target = map.get("target").asString();
			TaskID = map.get("task_id").asUUID();
		}
	}

	/// <summary>
	/// A message containing either the request or response used in updating a script inside
	/// a tasks inventory
	/// </summary>
	public static class UpdateScriptTaskMessage implements IMessage
	{
		/// <summary>Object containing request or response</summary>
		public AssetUploaderBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("task_id"))
				Request = new UpdateScriptTaskUpdateMessage();
			else if (map.containsKey("state") && map.get("state").asString().equals("upload"))
				Request = new UploaderRequestUpload();
			else if (map.containsKey("state") && map.get("state").asString().equals("complete")
					&& map.containsKey("errors"))
				Request = new UploaderScriptRequestError();
			else if (map.containsKey("state") && map.get("state").asString().equals("complete"))
				Request = new UploaderRequestScriptComplete();
			else
				JLogger.warn("Unable to deserialize UpdateScriptTaskMessage: No message handler exists for state " + map.get("state").asString());

			if (Request != null)
				Request.Deserialize(map);
		}
	}

	/// <summary>
	/// Response from the simulator to notify the viewer the upload is completed, and
	/// the UUID of the script asset and its compiled status
	/// </summary>
	public static class UploaderRequestScriptComplete extends AssetUploaderBlock
	{
		/// <summary>The uploaded texture asset ID</summary>
		public UUID AssetID;
		/// <summary>true of the script was compiled successfully</summary>
		public boolean Compiled;

		public UploaderRequestScriptComplete()
		{
			State = "complete";
		}

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("state", OSD.FromString(State));
			map.put("new_asset", OSD.FromUUID(AssetID));
			map.put("compiled", OSD.FromBoolean(Compiled));
			return map;
		}

		@Override
		public void Deserialize(OSDMap map)
		{
			AssetID = map.get("new_asset").asUUID();
			Compiled = map.get("compiled").asBoolean();
		}
	}

	/// <summary>
	/// A message sent from a viewer to the simulator requesting a temporary uploader capability
	/// used to update a script contained in an agents inventory
	/// </summary>
	public static class UpdateScriptAgentRequestMessage extends AssetUploaderBlock
	{
		/// <summary>The existing asset if of the script in the agents inventory to replace</summary>
		public UUID ItemID;
		/// <summary>The language of the script</summary>
		/// <remarks>Defaults to lsl version 2, "mono" might be another possible option</remarks>
		public String  Target = "lsl2"; // lsl2

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("item_id", OSD.FromUUID(ItemID));
			map.put("target", OSD.FromString(Target));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			ItemID = map.get("item_id").asUUID();
			Target = map.get("target").asString();
		}
	}

	/// <summary>
	/// A message containing either the request or response used in updating a script inside
	/// an agents inventory
	/// </summary>
	public static class UpdateScriptAgentMessage implements IMessage
	{
		/// <summary>Object containing request or response</summary>
		public AssetUploaderBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("item_id"))
				Request = new UpdateScriptAgentRequestMessage();
			else if (map.containsKey("errors"))
				Request = new UploaderScriptRequestError();
			else if (map.containsKey("state") && map.get("state").asString().equals("upload"))
				Request = new UploaderRequestUpload();
			else if (map.containsKey("state") && map.get("state").asString().equals("complete"))
				Request = new UploaderRequestScriptComplete();
			else
				JLogger.warn("Unable to deserialize UpdateScriptAgent: No message handler exists for state " + map.get("state").asString());

			if (Request != null)
				Request.Deserialize(map);
		}
	}


	public static class SendPostcardMessage implements IMessage
	{
		public String  FromEmail;
		public String  Message;
		public String  FromName;
		public Vector3 GlobalPosition;
		public String  Subject;
		public String  ToEmail;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(6);
			map.put("from", OSD.FromString(FromEmail));
			map.put("msg", OSD.FromString(Message));
			map.put("name", OSD.FromString(FromName));
			map.put("pos-global", OSD.FromVector3(GlobalPosition));
			map.put("subject", OSD.FromString(Subject));
			map.put("to", OSD.FromString(ToEmail));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			FromEmail = map.get("from").asString();
			Message = map.get("msg").asString();
			FromName = map.get("name").asString();
			GlobalPosition = map.get("pos-global").asVector3();
			Subject = map.get("subject").asString();
			ToEmail = map.get("to").asString();
		}
	}

	//endregion

	//region Grid/Maps

	/// <summary>Base class for Map Layers via Capabilities</summary>
	public static abstract class MapLayerMessageBase
	{
		/// <summary></summary>
		public int Flags;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public abstract OSDMap Serialize();

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public abstract void Deserialize(OSDMap map);
	}

	/// <summary>
	/// Sent by an agent to the capabilities server to request map layers
	/// </summary>
	public static class MapLayerRequestVariant extends MapLayerMessageBase
	{

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			map.put("Flags", OSD.FromInteger(Flags));
			return map;
		}

		@Override
		public void Deserialize(OSDMap map)
		{
			Flags = map.get("Flags").asInteger();
		}
	}

	/// <summary>
	/// A message sent from the simulator to the viewer which contains an array of map images and their grid coordinates
	/// </summary>
	public static class MapLayerReplyVariant extends MapLayerMessageBase
	{
		/// <summary>
		/// An object containing map location details
		/// </summary>
		public static class LayerData
		{
			/// <summary>The Asset ID of the regions tile overlay</summary>
			public UUID ImageID;
			/// <summary>The grid location of the southern border of the map tile</summary>
			public int Bottom;
			/// <summary>The grid location of the western border of the map tile</summary>
			public int Left;
			/// <summary>The grid location of the eastern border of the map tile</summary>
			public int Right;
			/// <summary>The grid location of the northern border of the map tile</summary>
			public int Top;
		}

		/// <summary>An array containing LayerData items</summary>
		public LayerData[] LayerDataBlocks;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			OSDMap agentMap = new OSDMap(1);
			agentMap.put("Flags", OSD.FromInteger(Flags));
			map.put("AgentData", agentMap);

			OSDArray layerArray = new OSDArray(LayerDataBlocks.length);

			for (int i = 0; i < LayerDataBlocks.length; i++)
			{
				OSDMap layerMap = new OSDMap(5);
				layerMap.put("ImageID", OSD.FromUUID(LayerDataBlocks[i].ImageID));
				layerMap.put("Bottom", OSD.FromInteger(LayerDataBlocks[i].Bottom));
				layerMap.put("Left", OSD.FromInteger(LayerDataBlocks[i].Left));
				layerMap.put("Top", OSD.FromInteger(LayerDataBlocks[i].Top));
				layerMap.put("Right", OSD.FromInteger(LayerDataBlocks[i].Right));

				layerArray.add(layerMap);
			}

			map.put("LayerData", layerArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			OSDMap agentMap = (OSDMap)map.get("AgentData");
			Flags = agentMap.get("Flags").asInteger();

			OSDArray layerArray = (OSDArray)map.get("LayerData");

			LayerDataBlocks = new LayerData[layerArray.count()];

			for (int i = 0; i < LayerDataBlocks.length; i++)
			{
				OSDMap layerMap = (OSDMap)layerArray.get(i);

				LayerData layer = new LayerData();
				layer.ImageID = layerMap.get("ImageID").asUUID();
				layer.Top = layerMap.get("Top").asInteger();
				layer.Right = layerMap.get("Right").asInteger();
				layer.Left = layerMap.get("Left").asInteger();
				layer.Bottom = layerMap.get("Bottom").asInteger();

				LayerDataBlocks[i] = layer;
			}
		}
	}

	public static class MapLayerMessage implements IMessage
	{
		/// <summary>Object containing request or response</summary>
		public MapLayerMessageBase Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("LayerData"))
				Request = new MapLayerReplyVariant();
			else if (map.containsKey("Flags"))
				Request = new MapLayerRequestVariant();
			else
				JLogger.warn("Unable to deserialize MapLayerMessage: No message handler exists");

			if (Request != null)
				Request.Deserialize(map);
		}
	}

	//endregion

	//region Session/Communication

	/// <summary>
	/// New as of 1.23 RC1, no details yet.
	/// </summary>
	public static class ProductInfoRequestMessage implements IMessage
	{
		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			throw new NotImplementedException();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			throw new NotImplementedException();
		}
	}

	//region ChatSessionRequestMessage


	public static abstract class SearchStatRequestBlock
	{
		public abstract OSDMap Serialize();
		public abstract void Deserialize(OSDMap map);
	}

	// variant A - the request to the simulator
	public static class SearchStatRequestRequest extends SearchStatRequestBlock
	{
		public UUID ClassifiedID;


		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			map.put("classified_id", OSD.FromUUID(ClassifiedID));
			return map;
		}


		@Override
		public void Deserialize(OSDMap map)
		{
			ClassifiedID = map.get("classified_id").asUUID();
		}
	}

	public static class SearchStatRequestReply extends SearchStatRequestBlock
	{
		public int MapClicks;
		public int ProfileClicks;
		public int SearchMapClicks;
		public int SearchProfileClicks;
		public int SearchTeleportClicks;
		public int TeleportClicks;


		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(6);
			map.put("map_clicks", OSD.FromInteger(MapClicks));
			map.put("profile_clicks", OSD.FromInteger(ProfileClicks));
			map.put("search_map_clicks", OSD.FromInteger(SearchMapClicks));
			map.put("search_profile_clicks", OSD.FromInteger(SearchProfileClicks));
			map.put("search_teleport_clicks", OSD.FromInteger(SearchTeleportClicks));
			map.put("teleport_clicks", OSD.FromInteger(TeleportClicks));
			return map;
		}


		@Override
		public void Deserialize(OSDMap map)
		{
			MapClicks = map.get("map_clicks").asInteger();
			ProfileClicks = map.get("profile_clicks").asInteger();
			SearchMapClicks = map.get("search_map_clicks").asInteger();
			SearchProfileClicks = map.get("search_profile_clicks").asInteger();
			SearchTeleportClicks = map.get("search_teleport_clicks").asInteger();
			TeleportClicks = map.get("teleport_clicks").asInteger();
		}
	}

	public static class SearchStatRequestMessage implements IMessage
	{
		public SearchStatRequestBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("map_clicks"))
				Request = new SearchStatRequestReply();
			else if (map.containsKey("classified_id"))
				Request = new SearchStatRequestRequest();
			else
				JLogger.warn("Unable to deserialize SearchStatRequest: No message handler exists for method " + map.get("method").asString());

			Request.Deserialize(map);
		}
	}

	public static abstract class ChatSessionRequestBlock
	{
		/// <summary>A string containing the method used</summary>
		public String  Method;

		public abstract OSDMap Serialize();
		public abstract void Deserialize(OSDMap map);
	}

	/// <summary>
	/// A request sent from an agent to the Simulator to begin a new conference.
	/// Contains a list of Agents which will be included in the conference
	/// </summary>    
	public static class ChatSessionRequestStartConference extends ChatSessionRequestBlock
	{
		/// <summary>An array containing the <see cref="UUID"/> of the agents invited to this conference</summary>
		public UUID[] AgentsBlock;
		/// <summary>The conferences Session ID</summary>
		public UUID SessionID;

		public ChatSessionRequestStartConference()
		{
			Method = "start conference";
		}

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);
			map.put("method", OSD.FromString(Method));
			OSDArray agentsArray = new OSDArray();
			for (int i = 0; i < AgentsBlock.length; i++)
			{
				agentsArray.add(OSD.FromUUID(AgentsBlock[i]));
			}
			map.put("params", agentsArray);
			map.put("session-id", OSD.FromUUID(SessionID));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			Method = map.get("method").asString();
			OSDArray agentsArray = (OSDArray)map.get("params");

			AgentsBlock = new UUID[agentsArray.count()];

			for (int i = 0; i < agentsArray.count(); i++)
			{
				AgentsBlock[i] = agentsArray.get(i).asUUID();
			}

			SessionID = map.get("session-id").asUUID();
		}
	}

	/// <summary>
	/// A moderation request sent from a conference moderator
	/// Contains an agent and an optional action to take
	/// </summary>    
	public static class ChatSessionRequestMuteUpdate extends ChatSessionRequestBlock
	{
		/// <summary>The Session ID</summary>
		public UUID SessionID;
		/// <summary></summary>
		public UUID AgentID;
		/// <summary>A list containing Key/Value pairs, known valid values:
		/// key: text value: true/false - allow/disallow specified agents ability to use text in session
		/// key: voice value: true/false - allow/disallow specified agents ability to use voice in session
		/// </summary>
		/// <remarks>"text" or "voice"</remarks>
		public String  RequestKey;
		/// <summary></summary>
		public boolean RequestValue;

		public ChatSessionRequestMuteUpdate()
		{
			Method = "mute update";
		}

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);
			map.put("method", OSD.FromString(Method));

			OSDMap muteMap = new OSDMap(1);
			muteMap.put(RequestKey, OSD.FromBoolean(RequestValue));

			OSDMap paramMap = new OSDMap(2);
			paramMap.put("agent_id", OSD.FromUUID(AgentID));
			paramMap.put("mute_info", muteMap);

			map.put("params", paramMap);
			map.put("session-id", OSD.FromUUID(SessionID));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			Method = map.get("method").asString();
			SessionID = map.get("session-id").asUUID();

			OSDMap paramsMap = (OSDMap)map.get("params");
			OSDMap muteMap = (OSDMap)paramsMap.get("mute_info");

			AgentID = paramsMap.get("agent_id").asUUID();

			if (muteMap.containsKey("text"))
				RequestKey = "text";
			else if (muteMap.containsKey("voice"))
				RequestKey = "voice";

			RequestValue = muteMap.get(RequestKey).asBoolean();
		}
	}

	/// <summary>
	/// A message sent from the agent to the simulator which tells the 
	/// simulator we've accepted a conference invitation
	/// </summary>
	public static class ChatSessionAcceptInvitation extends ChatSessionRequestBlock
	{
		/// <summary>The conference SessionID</summary>
		public UUID SessionID;

		public ChatSessionAcceptInvitation()
		{
			Method = "accept invitation";
		}

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("method", OSD.FromString(Method));
			map.put("session-id", OSD.FromUUID(SessionID));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			Method = map.get("method").asString();
			SessionID = map.get("session-id").asUUID();
		}
	}

	public static class ChatSessionRequestMessage implements IMessage
	{
		public ChatSessionRequestBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("method") && map.get("method").asString().equals("start conference"))
				Request = new ChatSessionRequestStartConference();
			else if (map.containsKey("method") && map.get("method").asString().equals("mute update"))
				Request = new ChatSessionRequestMuteUpdate();
			else if (map.containsKey("method") && map.get("method").asString().equals("accept invitation"))
				Request = new ChatSessionAcceptInvitation();
			else
				JLogger.warn("Unable to deserialize ChatSessionRequest: No message handler exists for method " + map.get("method").asString());

			Request.Deserialize(map);
		}
	}

	//endregion

	public static class ChatterboxSessionEventReplyMessage implements IMessage
	{
		public UUID SessionID;
		public boolean Success;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("success", OSD.FromBoolean(Success));
			map.put("session_id", OSD.FromUUID(SessionID)); // FIXME: Verify this is correct map name

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			Success = map.get("success").asBoolean();
			SessionID = map.get("session_id").asUUID();
		}
	}

	public static class ChatterBoxSessionStartReplyMessage implements IMessage
	{
		public UUID SessionID;
		public UUID TempSessionID;
		public boolean Success;

		public String  SessionName;
		// FIXME: Replace int with an enum
		public int Type;
		public boolean VoiceEnabled;
		public boolean ModeratedVoice;

		/* Is Text moderation possible? */

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap moderatedMap = new OSDMap(1);
			moderatedMap.put("voice", OSD.FromBoolean(ModeratedVoice));

			OSDMap sessionMap = new OSDMap(4);
			sessionMap.put("type", OSD.FromInteger(Type));
			sessionMap.put("session_name", OSD.FromString(SessionName));
			sessionMap.put("voice_enabled", OSD.FromBoolean(VoiceEnabled));
			sessionMap.put("moderated_mode", moderatedMap);

			OSDMap map = new OSDMap(4);
			map.put("session_id", OSD.FromUUID(SessionID));
			map.put("temp_session_id", OSD.FromUUID(TempSessionID));
			map.put("success", OSD.FromBoolean(Success));
			map.put("session_info", sessionMap);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			SessionID = map.get("session_id").asUUID();
			TempSessionID = map.get("temp_session_id").asUUID();
			Success = map.get("success").asBoolean();

			if (Success)
			{
				OSDMap sessionMap = (OSDMap)map.get("session_info");
				SessionName = sessionMap.get("session_name").asString();
				Type = sessionMap.get("type").asInteger();
				VoiceEnabled = sessionMap.get("voice_enabled").asBoolean();

				OSDMap moderatedModeMap = (OSDMap)sessionMap.get("moderated_mode");
				ModeratedVoice = moderatedModeMap.get("voice").asBoolean();
			}
		}
	}

	public static class ChatterBoxInvitationMessage implements IMessage
	{
		/// <summary>Key of sender</summary>
		public UUID FromAgentID;
		/// <summary>Name of sender</summary>
		public String  FromAgentName;
		/// <summary>Key of destination avatar</summary>
		public UUID ToAgentID;
		/// <summary>ID of originating estate</summary>
		//uint
		public long ParentEstateID;
		/// <summary>Key of originating region</summary>
		public UUID RegionID;
		/// <summary>Coordinates in originating region</summary>
		public Vector3 Position;
		/// <summary>Instant message type</summary>
		public InstantMessageDialog Dialog;
		/// <summary>Group IM session toggle</summary>
		public boolean GroupIM;
		/// <summary>Key of IM session, for Group Messages, the groups UUID</summary>
		public UUID IMSessionID;
		/// <summary>Timestamp of the instant message</summary>
		public Date Timestamp;
		/// <summary>Instant message text</summary>
		public String  Message;
		/// <summary>Whether this message is held for offline avatars</summary>
		public InstantMessageOnline Offline;
		/// <summary>Context specific packed data</summary>
		public byte[] BinaryBucket;
		/// <summary>Is this invitation for voice group/conference chat</summary>
		public boolean Voice;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap dataMap = new OSDMap(3);
			dataMap.put("timestamp", OSD.FromDate(Timestamp));
			dataMap.put("type", OSD.FromInteger((int)Dialog.getIndex()));
			dataMap.put("binary_bucket", OSD.FromBinary(BinaryBucket));

			OSDMap paramsMap = new OSDMap(11);
			paramsMap.put("from_id", OSD.FromUUID(FromAgentID));
			paramsMap.put("from_name", OSD.FromString(FromAgentName));
			paramsMap.put("to_id", OSD.FromUUID(ToAgentID));
			paramsMap.put("parent_estate_id", OSD.FromInteger((int)ParentEstateID));
			paramsMap.put("region_id", OSD.FromUUID(RegionID));
			paramsMap.put("position", OSD.FromVector3(Position));
			paramsMap.put("from_group", OSD.FromBoolean(GroupIM));
			paramsMap.put("id", OSD.FromUUID(IMSessionID));
			paramsMap.put("message", OSD.FromString(Message));
			paramsMap.put("offline", OSD.FromInteger(Offline.getIndex()));

			paramsMap.put("data", dataMap);

			OSDMap imMap = new OSDMap(1);
			imMap.put("message_params", paramsMap);

			OSDMap map = new OSDMap(1);
			map.put("instantmessage", imMap);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("voice"))
			{
				FromAgentID = map.get("from_id").asUUID();
				FromAgentName = map.get("from_name").asString();
				IMSessionID = map.get("session_id").asUUID();
				BinaryBucket = Utils.stringToBytesWithTrailingNullByte(map.get("session_name").asString());
				Voice = true;
			}
			else
			{
				OSDMap im = (OSDMap)map.get("instantmessage");
				OSDMap msg = (OSDMap)im.get("message_params");
				OSDMap msgdata = (OSDMap)msg.get("data");

				FromAgentID = msg.get("from_id").asUUID();
				FromAgentName = msg.get("from_name").asString();
				ToAgentID = msg.get("to_id").asUUID();
				ParentEstateID = (long)msg.get("parent_estate_id").asInteger();
				RegionID = msg.get("region_id").asUUID();
				Position = msg.get("position").asVector3();
				GroupIM = msg.get("from_group").asBoolean();
				IMSessionID = msg.get("id").asUUID();
				Message = msg.get("message").asString();
				Offline = InstantMessageOnline.get(msg.get("offline").asInteger());
				Dialog = InstantMessageDialog.get((byte)msgdata.get("type").asInteger());
				BinaryBucket = msgdata.get("binary_bucket").asBinary();
				Timestamp = msgdata.get("timestamp").asDate();
				Voice = false;
			}
		}
	}

	public static class RegionInfoMessage implements IMessage
	{
		public int ParcelLocalID;
		public String  RegionName;
		public String  ChannelUri;

		//region IMessage Members

		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);
			map.put("parcel_local_id", OSD.FromInteger(ParcelLocalID));
			map.put("region_name", OSD.FromString(RegionName));
			OSDMap voiceMap = new OSDMap(1);
			voiceMap.put("channel_uri", OSD.FromString(ChannelUri));
			map.put("voice_credentials", voiceMap);
			return map;
		}

		public void Deserialize(OSDMap map)
		{
			this.ParcelLocalID = map.get("parcel_local_id").asInteger();
			this.RegionName = map.get("region_name").asString();
			OSDMap voiceMap = (OSDMap)map.get("voice_credentials");
			this.ChannelUri = voiceMap.get("channel_uri").asString();
		}

		//endregion
	}

	/// <summary>
	/// Sent from the simulator to the viewer.
	/// 
	/// When an agent initially joins a session the AgentUpdatesBlock object will contain a list of session members including
	/// a boolean indicating they can use voice chat in this session, a boolean indicating they are allowed to moderate 
	/// this session, and lastly a string which indicates another agent is entering the session with the Transition set to "ENTER"
	/// 
	/// During the session lifetime updates on individuals are sent. During the update the booleans sent during the initial join are
	/// excluded with the exception of the Transition field. This indicates a new user entering or exiting the session with
	/// the string "ENTER" or "LEAVE" respectively.
	/// </summary>
	public static class ChatterBoxSessionAgentListUpdatesMessage implements IMessage
	{
		// initial when agent joins session
		// <llsd><map><key>events</key><array><map><key>body</key><map><key>agent_updates</key><map><key>32939971-a520-4b52-8ca5-6085d0e39933</key><map><key>info</key><map><key>can_voice_chat</key><boolean>1</boolean><key>is_moderator</key><boolean>1</boolean></map><key>transition</key><string>ENTER</string></map><key>ca00e3e1-0fdb-4136-8ed4-0aab739b29e8</key><map><key>info</key><map><key>can_voice_chat</key><boolean>1</boolean><key>is_moderator</key><boolean>0</boolean></map><key>transition</key><string>ENTER</string></map></map><key>session_id</key><string>be7a1def-bd8a-5043-5d5b-49e3805adf6b</string><key>updates</key><map><key>32939971-a520-4b52-8ca5-6085d0e39933</key><string>ENTER</string><key>ca00e3e1-0fdb-4136-8ed4-0aab739b29e8</key><string>ENTER</string></map></map><key>message</key><string>ChatterBoxSessionAgentListUpdates</string></map><map><key>body</key><map><key>agent_updates</key><map><key>32939971-a520-4b52-8ca5-6085d0e39933</key><map><key>info</key><map><key>can_voice_chat</key><boolean>1</boolean><key>is_moderator</key><boolean>1</boolean></map></map></map><key>session_id</key><string>be7a1def-bd8a-5043-5d5b-49e3805adf6b</string><key>updates</key><map /></map><key>message</key><string>ChatterBoxSessionAgentListUpdates</string></map></array><key>id</key><integer>5</integer></map></llsd>

		// a message containing only moderator updates
		// <llsd><map><key>events</key><array><map><key>body</key><map><key>agent_updates</key><map><key>ca00e3e1-0fdb-4136-8ed4-0aab739b29e8</key><map><key>info</key><map><key>mutes</key><map><key>text</key><boolean>1</boolean></map></map></map></map><key>session_id</key><string>be7a1def-bd8a-5043-5d5b-49e3805adf6b</string><key>updates</key><map /></map><key>message</key><string>ChatterBoxSessionAgentListUpdates</string></map></array><key>id</key><integer>7</integer></map></llsd>

		public UUID SessionID;

		public static class AgentUpdatesBlock
		{
			public UUID AgentID;

			public boolean CanVoiceChat;
			public boolean IsModerator;
			// transition "transition" = "ENTER" or "LEAVE"
			public String  Transition;   //  TODO: switch to an enum "ENTER" or "LEAVE"

			public boolean MuteText;
			public boolean MuteVoice;
		}

		public AgentUpdatesBlock[] Updates;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();

			OSDMap agent_updatesMap = new OSDMap(1);
			for (int i = 0; i < Updates.length; i++)
			{
				OSDMap mutesMap = new OSDMap(2);
				mutesMap.put("text", OSD.FromBoolean(Updates[i].MuteText));
				mutesMap.put("voice", OSD.FromBoolean(Updates[i].MuteVoice));

				OSDMap infoMap = new OSDMap(4);
				infoMap.put("can_voice_chat", OSD.FromBoolean((boolean)Updates[i].CanVoiceChat));
				infoMap.put("is_moderator", OSD.FromBoolean((boolean)Updates[i].IsModerator));
				infoMap.put("mutes", mutesMap);

				OSDMap imap = new OSDMap(1);
				imap.put("info", infoMap);
				imap.put("transition", OSD.FromString(Updates[i].Transition));

				agent_updatesMap.put(Updates[i].AgentID.toString(), imap);
			}

			map.put("agent_updates", agent_updatesMap);

			map.put("session_id", OSD.FromUUID(SessionID));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{

			OSDMap agent_updates = (OSDMap)map.get("agent_updates");
			SessionID = map.get("session_id").asUUID();

			List<AgentUpdatesBlock> updatesList = new ArrayList<AgentUpdatesBlock>();

			for(Entry<String, OSD> kvp :agent_updates.entrySet())
			{

				if (kvp.getKey().equals("updates"))
				{
					// This appears to be redundant and duplicated by the info block, more dumps will confirm this
					/* <key>32939971-a520-4b52-8ca5-6085d0e39933</key>
                            <string>ENTER</string> */
				}
				else if (kvp.getKey().equals("session_id"))
				{
					// I am making the assumption that each osdmap will contain the information for a 
					// single session. This is how the map appears to read however more dumps should be taken
					// to confirm this.
					/* <key>session_id</key>
                            <string>984f6a1e-4ceb-6366-8d5e-a18c6819c6f7</string> */

				}
				else  // key is an agent uuid (we hope!)
				{
					// should be the agents uuid as the key, and "info" as the datablock
					/* <key>32939971-a520-4b52-8ca5-6085d0e39933</key>
                            <map>
                                <key>info</key>
                                    <map>
                                        <key>can_voice_chat</key>
                                            <boolean>1</boolean>
                                        <key>is_moderator</key>
                                            <boolean>1</boolean>
                                    </map>
                                <key>transition</key>
                                    <string>ENTER</string>
                            </map>*/
					AgentUpdatesBlock block = new AgentUpdatesBlock();
					block.AgentID = UUID.Parse(kvp.getKey());

					OSDMap infoMap = (OSDMap)kvp.getValue();

					OSDMap agentPermsMap = (OSDMap)infoMap.get("info");

					block.CanVoiceChat = agentPermsMap.get("can_voice_chat").asBoolean();
					block.IsModerator = agentPermsMap.get("is_moderator").asBoolean();

					block.Transition = infoMap.get("transition").asString();

					if (agentPermsMap.containsKey("mutes"))
					{
						OSDMap mutesMap = (OSDMap)agentPermsMap.get("mutes");
						block.MuteText = mutesMap.get("text").asBoolean();
						block.MuteVoice = mutesMap.get("voice").asBoolean();
					}
					updatesList.add(block);
				}
			}

			Updates = new AgentUpdatesBlock[updatesList.size()];

			for (int i = 0; i < updatesList.size(); i++)
			{
				AgentUpdatesBlock block = new AgentUpdatesBlock();
				block.AgentID = updatesList.get(i).AgentID;
				block.CanVoiceChat = updatesList.get(i).CanVoiceChat;
				block.IsModerator = updatesList.get(i).IsModerator;
				block.MuteText = updatesList.get(i).MuteText;
				block.MuteVoice = updatesList.get(i).MuteVoice;
				block.Transition = updatesList.get(i).Transition;
				Updates[i] = block;
			}
		}
	}

	/// <summary>
	/// An EventQueue message sent when the agent is forcibly removed from a chatterbox session
	/// </summary>
	public static class ForceCloseChatterBoxSessionMessage implements IMessage
	{
		/// <summary>
		/// A string containing the reason the agent was removed
		/// </summary>
		public String  Reason;
		/// <summary>
		/// The ChatterBoxSession's SessionID
		/// </summary>
		public UUID SessionID;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("reason", OSD.FromString(Reason));
			map.put("session_id", OSD.FromUUID(SessionID));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			Reason = map.get("reason").asString();
			SessionID = map.get("session_id").asUUID();
		}
	}

	//endregion

	//region EventQueue

	public static abstract class EventMessageBlock
	{
		public abstract OSDMap Serialize();
		public abstract void Deserialize(OSDMap map);
	}

	public static class EventQueueAck extends EventMessageBlock
	{
		public int AckID;
		public boolean Done;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();
			map.put("ack", OSD.FromInteger(AckID));
			map.put("done", OSD.FromBoolean(Done));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			AckID = map.get("ack").asInteger();
			Done = map.get("done").asBoolean();
		}
	}

	public static class EventQueueEvent extends EventMessageBlock
	{
		public static class QueueEvent
		{
			public IMessage EventMessage;
			public String  MessageKey;
		}

		public int Sequence;
		public QueueEvent[] MessageEvents;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);

			OSDArray eventsArray = new OSDArray();

			for (int i = 0; i < MessageEvents.length; i++)
			{
				OSDMap eventMap = new OSDMap(2);
				eventMap.put("body", MessageEvents[i].EventMessage.Serialize());
				eventMap.put("message", OSD.FromString(MessageEvents[i].MessageKey));
				eventsArray.add(eventMap);
			}

			map.put("events", eventsArray);
			map.put("id", OSD.FromInteger(Sequence));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			Sequence = map.get("id").asInteger();
			OSDArray arrayEvents = (OSDArray)map.get("events");

			MessageEvents = new QueueEvent[arrayEvents.count()];

			for (int i = 0; i < arrayEvents.count(); i++)
			{
				OSDMap eventMap = (OSDMap)arrayEvents.get(i);
				QueueEvent ev = new QueueEvent();

				ev.MessageKey = eventMap.get("message").asString();
				ev.EventMessage = MessageUtils.DecodeEvent(ev.MessageKey, (OSDMap)eventMap.get("body"));
				MessageEvents[i] = ev;
			}
		}
	}

	public static class EventQueueGetMessage implements IMessage
	{
		public EventMessageBlock Messages;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Messages.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("ack"))
				Messages = new EventQueueAck();
			else if (map.containsKey("events"))
				Messages = new EventQueueEvent();
			else
				JLogger.warn("Unable to deserialize EventQueueGetMessage: No message handler exists for event");

			Messages.Deserialize(map);
		}
	}

	//endregion

	//region Stats Messages

	public static class ViewerStatsMessage implements IMessage
	{
		public int AgentsInView;
		public float AgentFPS;
		public String  AgentLanguage;
		public float AgentMemoryUsed;
		public float MetersTraveled;
		public float AgentPing;
		public int RegionsVisited;
		public float AgentRuntime;
		public float SimulatorFPS;
		public Date AgentStartTime;
		public String  AgentVersion;

		public float object_kbytes;
		public float texture_kbytes;
		public float world_kbytes;

		public float MiscVersion;
		public boolean VertexBuffersEnabled;

		public UUID SessionID;

		public int StatsDropped;
		public int StatsFailedResends;
		public int FailuresInvalid;
		public int FailuresOffCircuit;
		public int FailuresResent;
		public int FailuresSendPacket;

		public int MiscInt1;
		public int MiscInt2;
		public String  MiscString1;

		public int InCompressedPackets;
		public float InKbytes;
		public float InPackets;
		public float InSavings;

		public int OutCompressedPackets;
		public float OutKbytes;
		public float OutPackets;
		public float OutSavings;

		public String  SystemCPU;
		public String  SystemGPU;
		public int SystemGPUClass;
		public String  SystemGPUVendor;
		public String  SystemGPUVersion;
		public String  SystemOS;
		public int SystemInstalledRam;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(5);
			map.put("session_id", OSD.FromUUID(SessionID));

			OSDMap agentMap = new OSDMap(11);
			agentMap.put("agents_in_view", OSD.FromInteger(AgentsInView));
			agentMap.put("fps", OSD.FromReal(AgentFPS));
			agentMap.put("language", OSD.FromString(AgentLanguage));
			agentMap.put("mem_use", OSD.FromReal(AgentMemoryUsed));
			agentMap.put("meters_traveled", OSD.FromReal(MetersTraveled));
			agentMap.put("ping", OSD.FromReal(AgentPing));
			agentMap.put("regions_visited", OSD.FromInteger(RegionsVisited));
			agentMap.put("run_time", OSD.FromReal(AgentRuntime));
			agentMap.put("sim_fps", OSD.FromReal(SimulatorFPS));
			agentMap.put("start_time", OSD.FromUInteger(Utils.dateToUnixTime(AgentStartTime)));
			agentMap.put("version", OSD.FromString(AgentVersion));
			map.put("agent", agentMap);


			OSDMap downloadsMap = new OSDMap(3); // downloads
			downloadsMap.put("object_kbytes", OSD.FromReal(object_kbytes));
			downloadsMap.put("texture_kbytes", OSD.FromReal(texture_kbytes));
			downloadsMap.put("world_kbytes", OSD.FromReal(world_kbytes));
			map.put("downloads", downloadsMap);

			OSDMap miscMap = new OSDMap(2);
			miscMap.put("Version", OSD.FromReal(MiscVersion));
			miscMap.put("Vertex Buffers Enabled", OSD.FromBoolean(VertexBuffersEnabled));
			map.put("misc", miscMap);

			OSDMap statsMap = new OSDMap(2);

			OSDMap failuresMap = new OSDMap(6);
			failuresMap.put("dropped", OSD.FromInteger(StatsDropped));
			failuresMap.put("failed_resends", OSD.FromInteger(StatsFailedResends));
			failuresMap.put("invalid", OSD.FromInteger(FailuresInvalid));
			failuresMap.put("off_circuit", OSD.FromInteger(FailuresOffCircuit));
			failuresMap.put("resent", OSD.FromInteger(FailuresResent));
			failuresMap.put("send_packet", OSD.FromInteger(FailuresSendPacket));
			statsMap.put("failures", failuresMap);

			OSDMap statsMiscMap = new OSDMap(3);
			statsMiscMap.put("int_1", OSD.FromInteger(MiscInt1));
			statsMiscMap.put("int_2", OSD.FromInteger(MiscInt2));
			statsMiscMap.put("string_1", OSD.FromString(MiscString1));
			statsMap.put("misc", statsMiscMap);

			OSDMap netMap = new OSDMap(3);

			// in
			OSDMap netInMap = new OSDMap(4);
			netInMap.put("compressed_packets", OSD.FromInteger(InCompressedPackets));
			netInMap.put("kbytes", OSD.FromReal(InKbytes));
			netInMap.put("packets", OSD.FromReal(InPackets));
			netInMap.put("savings", OSD.FromReal(InSavings));
			netMap.put("in", netInMap);
			// out
			OSDMap netOutMap = new OSDMap(4);
			netOutMap.put("compressed_packets", OSD.FromInteger(OutCompressedPackets));
			netOutMap.put("kbytes", OSD.FromReal(OutKbytes));
			netOutMap.put("packets", OSD.FromReal(OutPackets));
			netOutMap.put("savings", OSD.FromReal(OutSavings));
			netMap.put("out", netOutMap);

			statsMap.put("net", netMap);

			//system
			OSDMap systemStatsMap = new OSDMap(7);
			systemStatsMap.put("cpu", OSD.FromString(SystemCPU));
			systemStatsMap.put("gpu", OSD.FromString(SystemGPU));
			systemStatsMap.put("gpu_class", OSD.FromInteger(SystemGPUClass));
			systemStatsMap.put("gpu_vendor", OSD.FromString(SystemGPUVendor));
			systemStatsMap.put("gpu_version", OSD.FromString(SystemGPUVersion));
			systemStatsMap.put("os", OSD.FromString(SystemOS));
			systemStatsMap.put("ram", OSD.FromInteger(SystemInstalledRam));
			map.put("system", systemStatsMap);

			map.put("stats", statsMap);
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			SessionID = map.get("session_id").asUUID();

			OSDMap agentMap = (OSDMap)map.get("agent");
			AgentsInView = agentMap.get("agents_in_view").asInteger();
			AgentFPS = (float)agentMap.get("fps").asReal();
			AgentLanguage = agentMap.get("language").asString();
			AgentMemoryUsed = (float)agentMap.get("mem_use").asReal();
			MetersTraveled = agentMap.get("meters_traveled").asInteger();
			AgentPing = (float)agentMap.get("ping").asReal();
			RegionsVisited = agentMap.get("regions_visited").asInteger();
			AgentRuntime = (float)agentMap.get("run_time").asReal();
			SimulatorFPS = (float)agentMap.get("sim_fps").asReal();
			AgentStartTime = Utils.unixTimeToDate(agentMap.get("start_time").asUInteger());
			AgentVersion = agentMap.get("version").asString();

			OSDMap downloadsMap = (OSDMap)map.get("downloads");
			object_kbytes = (float)downloadsMap.get("object_kbytes").asReal();
			texture_kbytes = (float)downloadsMap.get("texture_kbytes").asReal();
			world_kbytes = (float)downloadsMap.get("world_kbytes").asReal();

			OSDMap miscMap = (OSDMap)map.get("misc");
			MiscVersion = (float)miscMap.get("Version").asReal();
			VertexBuffersEnabled = miscMap.get("Vertex Buffers Enabled").asBoolean();

			OSDMap statsMap = (OSDMap)map.get("stats");
			OSDMap failuresMap = (OSDMap)statsMap.get("failures");
			StatsDropped = failuresMap.get("dropped").asInteger();
			StatsFailedResends = failuresMap.get("failed_resends").asInteger();
			FailuresInvalid = failuresMap.get("invalid").asInteger();
			FailuresOffCircuit = failuresMap.get("off_circuit").asInteger();
			FailuresResent = failuresMap.get("resent").asInteger();
			FailuresSendPacket = failuresMap.get("send_packet").asInteger();

			OSDMap statsMiscMap = (OSDMap)statsMap.get("misc");
			MiscInt1 = statsMiscMap.get("int_1").asInteger();
			MiscInt2 = statsMiscMap.get("int_2").asInteger();
			MiscString1 = statsMiscMap.get("string_1").asString();
			OSDMap netMap = (OSDMap)statsMap.get("net");
			// in
			OSDMap netInMap = (OSDMap)netMap.get("in");
			InCompressedPackets = netInMap.get("compressed_packets").asInteger();
			InKbytes = netInMap.get("kbytes").asInteger();
			InPackets = netInMap.get("packets").asInteger();
			InSavings = netInMap.get("savings").asInteger();
			// out
			OSDMap netOutMap = (OSDMap)netMap.get("out");
			OutCompressedPackets = netOutMap.get("compressed_packets").asInteger();
			OutKbytes = netOutMap.get("kbytes").asInteger();
			OutPackets = netOutMap.get("packets").asInteger();
			OutSavings = netOutMap.get("savings").asInteger();

			//system
			OSDMap systemStatsMap = (OSDMap)map.get("system");
			SystemCPU = systemStatsMap.get("cpu").asString();
			SystemGPU = systemStatsMap.get("gpu").asString();
			SystemGPUClass = systemStatsMap.get("gpu_class").asInteger();
			SystemGPUVendor = systemStatsMap.get("gpu_vendor").asString();
			SystemGPUVersion = systemStatsMap.get("gpu_version").asString();
			SystemOS = systemStatsMap.get("os").asString();
			SystemInstalledRam = systemStatsMap.get("ram").asInteger();
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public static class PlacesReplyMessage implements IMessage
	{
		public UUID AgentID;
		public UUID QueryID;
		public UUID TransactionID;

		public static class QueryData
		{
			public int ActualArea;
			public int BillableArea;
			public String  Description;
			public float Dwell;
			public int Flags;
			public float GlobalX;
			public float GlobalY;
			public float GlobalZ;
			public String  Name;
			public UUID OwnerID;
			public String  SimName;
			public UUID SnapShotID;
			public String  ProductSku;
			public int Price;
		}

		public QueryData[] QueryDataBlocks;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);

			// add the AgentData map
			OSDMap agentIDmap = new OSDMap(2);
			agentIDmap.put("AgentID", OSD.FromUUID(AgentID));
			agentIDmap.put("QueryID", OSD.FromUUID(QueryID));

			OSDArray agentDataArray = new OSDArray();
			agentDataArray.add(agentIDmap);

			map.put("AgentData", agentDataArray);

			// add the QueryData map
			OSDArray dataBlocksArray = new OSDArray(QueryDataBlocks.length);
			for (int i = 0; i < QueryDataBlocks.length; i++)
			{
				OSDMap queryDataMap = new OSDMap(14);
				queryDataMap.put("ActualArea", OSD.FromInteger(QueryDataBlocks[i].ActualArea));
				queryDataMap.put("BillableArea", OSD.FromInteger(QueryDataBlocks[i].BillableArea));
				queryDataMap.put("Desc", OSD.FromString(QueryDataBlocks[i].Description));
				queryDataMap.put("Dwell", OSD.FromReal(QueryDataBlocks[i].Dwell));
				queryDataMap.put("Flags", OSD.FromInteger(QueryDataBlocks[i].Flags));
				queryDataMap.put("GlobalX", OSD.FromReal(QueryDataBlocks[i].GlobalX));
				queryDataMap.put("GlobalY", OSD.FromReal(QueryDataBlocks[i].GlobalY));
				queryDataMap.put("GlobalZ", OSD.FromReal(QueryDataBlocks[i].GlobalZ));
				queryDataMap.put("Name", OSD.FromString(QueryDataBlocks[i].Name));
				queryDataMap.put("OwnerID", OSD.FromUUID(QueryDataBlocks[i].OwnerID));
				queryDataMap.put("Price", OSD.FromInteger(QueryDataBlocks[i].Price));
				queryDataMap.put("SimName", OSD.FromString(QueryDataBlocks[i].SimName));
				queryDataMap.put("SnapshotID", OSD.FromUUID(QueryDataBlocks[i].SnapShotID));
				queryDataMap.put("ProductSKU", OSD.FromString(QueryDataBlocks[i].ProductSku));
				dataBlocksArray.add(queryDataMap);
			}

			map.put("QueryData", dataBlocksArray);

			// add the TransactionData map
			OSDMap transMap = new OSDMap(1);
			transMap.put("TransactionID", OSD.FromUUID(TransactionID));
			OSDArray transArray = new OSDArray();
			transArray.add(transMap);
			map.put("TransactionData", transArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDArray agentDataArray = (OSDArray)map.get("AgentData");

			OSDMap agentDataMap = (OSDMap)agentDataArray.get(0);
			AgentID = agentDataMap.get("AgentID").asUUID();
			QueryID = agentDataMap.get("QueryID").asUUID();


			OSDArray dataBlocksArray = (OSDArray)map.get("QueryData");
			QueryDataBlocks = new QueryData[dataBlocksArray.count()];
			for (int i = 0; i < dataBlocksArray.count(); i++)
			{
				OSDMap dataMap = (OSDMap)dataBlocksArray.get(i);
				QueryData data = new QueryData();
				data.ActualArea = dataMap.get("ActualArea").asInteger();
				data.BillableArea = dataMap.get("BillableArea").asInteger();
				data.Description = dataMap.get("Desc").asString();
				data.Dwell = (float)dataMap.get("Dwell").asReal();
				data.Flags = dataMap.get("Flags").asInteger();
				data.GlobalX = (float)dataMap.get("GlobalX").asReal();
				data.GlobalY = (float)dataMap.get("GlobalY").asReal();
				data.GlobalZ = (float)dataMap.get("GlobalZ").asReal();
				data.Name = dataMap.get("Name").asString();
				data.OwnerID = dataMap.get("OwnerID").asUUID();
				data.Price = dataMap.get("Price").asInteger();
				data.SimName = dataMap.get("SimName").asString();
				data.SnapShotID = dataMap.get("SnapshotID").asUUID();
				data.ProductSku = dataMap.get("ProductSKU").asString();
				QueryDataBlocks[i] = data;
			}

			OSDArray transactionArray = (OSDArray)map.get("TransactionData");
			OSDMap transactionDataMap = (OSDMap)transactionArray.get(0);
			TransactionID = transactionDataMap.get("TransactionID").asUUID();
		}
	}

	public static class UpdateAgentInformationMessage implements IMessage
	{
		public String  MaxAccess; // PG, A, or M

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			OSDMap prefsMap = new OSDMap(1);
			prefsMap.put("max", OSD.FromString(MaxAccess));
			map.put("access_prefs", prefsMap);
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDMap prefsMap = (OSDMap)map.get("access_prefs");
			MaxAccess = prefsMap.get("max").asString();
		}
	}

	//    [Serializable]
	public static class DirLandReplyMessage implements IMessage, Serializable
	{
		public UUID AgentID;
		public UUID QueryID;

		//        [Serializable]
		public static class QueryReply implements Serializable
		{
			public int ActualArea;
			public boolean Auction;
			public boolean ForSale;
			public String  Name;
			public UUID ParcelID;
			public String  ProductSku;
			public int SalePrice;
		}

		public QueryReply[] QueryReplies;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);

			OSDMap agentMap = new OSDMap(1);
			agentMap.put("AgentID", OSD.FromUUID(AgentID));
			OSDArray agentDataArray = new OSDArray(1);
			agentDataArray.add(agentMap);
			map.put("AgentData", agentDataArray);

			OSDMap queryMap = new OSDMap(1);
			queryMap.put("QueryID", OSD.FromUUID(QueryID));
			OSDArray queryDataArray = new OSDArray(1);
			queryDataArray.add(queryMap);
			map.put("QueryData", queryDataArray);

			OSDArray queryReplyArray = new OSDArray();
			for (int i = 0; i < QueryReplies.length; i++)
			{
				OSDMap queryReply = new OSDMap(100);
				queryReply.put("ActualArea", OSD.FromInteger(QueryReplies[i].ActualArea));
				queryReply.put("Auction", OSD.FromBoolean(QueryReplies[i].Auction));
				queryReply.put("ForSale", OSD.FromBoolean(QueryReplies[i].ForSale));
				queryReply.put("Name", OSD.FromString(QueryReplies[i].Name));
				queryReply.put("ParcelID", OSD.FromUUID(QueryReplies[i].ParcelID));
				queryReply.put("ProductSKU", OSD.FromString(QueryReplies[i].ProductSku));
				queryReply.put("SalePrice", OSD.FromInteger(QueryReplies[i].SalePrice));

				queryReplyArray.add(queryReply);
			}
			map.put("QueryReplies", queryReplyArray);

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			OSDArray agentDataArray = (OSDArray)map.get("AgentData");
			OSDMap agentDataMap = (OSDMap)agentDataArray.get(0);
			AgentID = agentDataMap.get("AgentID").asUUID();

			OSDArray queryDataArray = (OSDArray)map.get("QueryData");
			OSDMap queryDataMap = (OSDMap)queryDataArray.get(0);
			QueryID = queryDataMap.get("QueryID").asUUID();

			OSDArray queryRepliesArray = (OSDArray)map.get("QueryReplies");

			QueryReplies = new QueryReply[queryRepliesArray.count()];
			for (int i = 0; i < queryRepliesArray.count(); i++)
			{
				QueryReply reply = new QueryReply();
				OSDMap replyMap = (OSDMap)queryRepliesArray.get(i);
				reply.ActualArea = replyMap.get("ActualArea").asInteger();
				reply.Auction = replyMap.get("Auction").asBoolean();
				reply.ForSale = replyMap.get("ForSale").asBoolean();
				reply.Name = replyMap.get("Name").asString();
				reply.ParcelID = replyMap.get("ParcelID").asUUID();
				reply.ProductSku = replyMap.get("ProductSKU").asString();
				reply.SalePrice = replyMap.get("SalePrice").asInteger();

				QueryReplies[i] = reply;
			}
		}
	}

	//endregion

	//region Object Messages

	public static class UploadObjectAssetMessage implements IMessage
	{
		public static class Object
		{
			public static class Face
			{
				public Bumpiness Bump;
				public Color4 Color;
				public boolean Fullbright;
				public float Glow;
				public UUID ImageID;
				public float ImageRot;
				public int MediaFlags;
				public float OffsetS;
				public float OffsetT;
				public float ScaleS;
				public float ScaleT;

				public OSDMap Serialize()
				{
					OSDMap map = new OSDMap();
					map.put("bump", OSD.FromInteger((int)Bump.getIndex()));
					map.put("colors", OSD.FromColor4(Color));
					map.put("fullbright", OSD.FromBoolean(Fullbright));
					map.put("glow", OSD.FromReal(Glow));
					map.put("imageid", OSD.FromUUID(ImageID));
					map.put("imagerot", OSD.FromReal(ImageRot));
					map.put("media_flags", OSD.FromInteger(MediaFlags));
					map.put("offsets", OSD.FromReal(OffsetS));
					map.put("offsett", OSD.FromReal(OffsetT));
					map.put("scales", OSD.FromReal(ScaleS));
					map.put("scalet", OSD.FromReal(ScaleT));

					return map;
				}

				public void Deserialize(OSDMap map)
				{
					Bump = Bumpiness.get((byte)map.get("bump").asInteger());
					Color = map.get("colors").asColor4();
					Fullbright = map.get("fullbright").asBoolean();
					Glow = (float)map.get("glow").asReal();
					ImageID = map.get("imageid").asUUID();
					ImageRot = (float)map.get("imagerot").asReal();
					MediaFlags = map.get("media_flags").asInteger();
					OffsetS = (float)map.get("offsets").asReal();
					OffsetT = (float)map.get("offsett").asReal();
					ScaleS = (float)map.get("scales").asReal();
					ScaleT = (float)map.get("scalet").asReal();
				}
			}

			public static class ExtraParam
			{
				public EnumSet<ExtraParamType> Type;
				public byte[] ExtraParamData;

				public OSDMap Serialize()
				{
					OSDMap map = new OSDMap();
					map.put("extra_parameter", OSD.FromInteger((int)ExtraParamType.getIndex(Type)));
					map.put("param_data", OSD.FromBinary(ExtraParamData));

					return map;
				}

				public void Deserialize(OSDMap map)
				{
					Type = ExtraParamType.get(map.get("extra_parameter").asInteger());
					ExtraParamData = map.get("param_data").asBinary();
				}
			}

			public Face[] Faces;
			public ExtraParam[] ExtraParams;
			public UUID GroupID;
			public EnumsPrimitive.Material Material;
			public String  Name;
			public Vector3 Position;
			public Quaternion Rotation;
			public Vector3 Scale;
			public float PathBegin;
			public int PathCurve;
			public float PathEnd;
			public float RadiusOffset;
			public float Revolutions;
			public float ScaleX;
			public float ScaleY;
			public float ShearX;
			public float ShearY;
			public float Skew;
			public float TaperX;
			public float TaperY;
			public float Twist;
			public float TwistBegin;
			public float ProfileBegin;
			public int ProfileCurve;
			public float ProfileEnd;
			public float ProfileHollow;
			public UUID SculptID;
			public EnumsPrimitive.SculptType SculptType;

			public OSDMap Serialize()
			{
				OSDMap map = new OSDMap();

				map.put("group-id", OSD.FromUUID(GroupID));
				map.put("material", OSD.FromInteger((int)Material.getIndex()));
				map.put("name", OSD.FromString(Name));
				map.put("pos", OSD.FromVector3(Position));
				map.put("rotation", OSD.FromQuaternion(Rotation));
				map.put("scale", OSD.FromVector3(Scale));

				// Extra params
				OSDArray extraParams = new OSDArray();
				if (ExtraParams != null)
				{
					for (int i = 0; i < ExtraParams.length; i++)
						extraParams.add(ExtraParams[i].Serialize());
				}
				map.put("extra_parameters", extraParams);

				// Faces
				OSDArray faces = new OSDArray();
				if (Faces != null)
				{
					for (int i = 0; i < Faces.length; i++)
						faces.add(Faces[i].Serialize());
				}
				map.put("facelist", faces);

				// Shape
				OSDMap shape = new OSDMap();
				OSDMap path = new OSDMap();
				path.put("begin", OSD.FromReal(PathBegin));
				path.put("curve", OSD.FromInteger(PathCurve));
				path.put("end", OSD.FromReal(PathEnd));
				path.put("radius_offset", OSD.FromReal(RadiusOffset));
				path.put("revolutions", OSD.FromReal(Revolutions));
				path.put("scale_x", OSD.FromReal(ScaleX));
				path.put("scale_y", OSD.FromReal(ScaleY));
				path.put("shear_x", OSD.FromReal(ShearX));
				path.put("shear_y", OSD.FromReal(ShearY));
				path.put("skew", OSD.FromReal(Skew));
				path.put("taper_x", OSD.FromReal(TaperX));
				path.put("taper_y", OSD.FromReal(TaperY));
				path.put("twist", OSD.FromReal(Twist));
				path.put("twist_begin", OSD.FromReal(TwistBegin));
				shape.put("path", path);
				OSDMap profile = new OSDMap();
				profile.put("begin", OSD.FromReal(ProfileBegin));
				profile.put("curve", OSD.FromInteger(ProfileCurve));
				profile.put("end", OSD.FromReal(ProfileEnd));
				profile.put("hollow", OSD.FromReal(ProfileHollow));
				shape.put("profile", profile);
				OSDMap sculpt = new OSDMap();
				sculpt.put("id", OSD.FromUUID(SculptID));
				sculpt.put("type", OSD.FromInteger((int)SculptType.getIndex()));
				shape.put("sculpt", sculpt);
				map.put("shape", shape);

				return map;
			}

			public void Deserialize(OSDMap map)
			{
				GroupID = map.get("group-id").asUUID();
				Material = Material.get((byte)map.get("material").asInteger());
				Name = map.get("name").asString();
				Position = map.get("pos").asVector3();
				Rotation = map.get("rotation").asQuaternion();
				Scale = map.get("scale").asVector3();

				// Extra params
				OSDArray extraParams = (OSDArray) map.get("extra_parameters");
				if (extraParams != null)
				{
					ExtraParams = new ExtraParam[extraParams.count()];
					for (int i = 0; i < extraParams.count(); i++)
					{
						ExtraParam extraParam = new ExtraParam();
						extraParam.Deserialize((OSDMap)extraParams.get(i));
						ExtraParams[i] = extraParam;
					}
				}
				else
				{
					ExtraParams = new ExtraParam[0];
				}

				// Faces
				OSDArray faces = (OSDArray)map.get("facelist");
				if (faces != null)
				{
					Faces = new Face[faces.count()];
					for (int i = 0; i < faces.count(); i++)
					{
						Face face = new Face();
						face.Deserialize((OSDMap)faces.get(i));
						Faces[i] = face;
					}
				}
				else
				{
					Faces = new Face[0];
				}

				// Shape
				OSDMap shape = (OSDMap)map.get("shape");
				OSDMap path = (OSDMap)shape.get("path");
				PathBegin = (float)path.get("begin").asReal();
				PathCurve = path.get("curve").asInteger();
				PathEnd = (float)path.get("end").asReal();
				RadiusOffset = (float)path.get("radius_offset").asReal();
				Revolutions = (float)path.get("revolutions").asReal();
				ScaleX = (float)path.get("scale_x").asReal();
				ScaleY = (float)path.get("scale_y").asReal();
				ShearX = (float)path.get("shear_x").asReal();
				ShearY = (float)path.get("shear_y").asReal();
				Skew = (float)path.get("skew").asReal();
				TaperX = (float)path.get("taper_x").asReal();
				TaperY = (float)path.get("taper_y").asReal();
				Twist = (float)path.get("twist").asReal();
				TwistBegin = (float)path.get("twist_begin").asReal();

				OSDMap profile = (OSDMap)shape.get("profile");
				ProfileBegin = (float)profile.get("begin").asReal();
				ProfileCurve = profile.get("curve").asInteger();
				ProfileEnd = (float)profile.get("end").asReal();
				ProfileHollow = (float)profile.get("hollow").asReal();

				OSDMap sculpt = (OSDMap)shape.get("sculpt");
				if (sculpt != null)
				{
					SculptID = sculpt.get("id").asUUID();
					SculptType = SculptType.get((byte)sculpt.get("type").asInteger());
				}
				else
				{
					SculptID = UUID.Zero;
					SculptType = SculptType.get((byte)0);
				}
			}
		}

		public Object[] Objects;

		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap();
			OSDArray array = new OSDArray();

			if (Objects != null)
			{
				for (int i = 0; i < Objects.length; i++)
					array.add(Objects[i].Serialize());
			}

			map.put("objects", array);
			return map;
		}

		public void Deserialize(OSDMap map)
		{
			OSDArray array = (OSDArray)map.get("objects");

			if (array != null)
			{
				Objects = new Object[array.count()];

				for (int i = 0; i < array.count(); i++)
				{
					Object obj = new Object();
					OSDMap objMap = (OSDMap)array.get(i);

					if (objMap != null)
						obj.Deserialize(objMap);

					Objects[i] = obj;
				}
			}
			else
			{
				Objects = new Object[0];
			}
		}
	}

	/// <summary>
	/// Event Queue message describing physics engine attributes of a list of objects
	/// Sim sends these when object is selected
	/// </summary>
	public static class ObjectPhysicsPropertiesMessage implements IMessage
	{
		/// <summary> Array with the list of physics properties</summary>
		public PhysicsProperties[] ObjectPhysicsProperties;

		/// <summary>
		/// Serializes the message
		/// </summary>
		/// <returns>Serialized OSD</returns>
		public OSDMap Serialize()
		{
			OSDMap ret = new OSDMap();
			OSDArray array = new OSDArray();

			for (int i = 0; i < ObjectPhysicsProperties.length; i++)
			{
				array.add(ObjectPhysicsProperties[i].GetOSD());
			}

			ret.put("ObjectData", array);
			return ret;

		}

		/// <summary>
		/// Deseializes the message
		/// </summary>
		/// <param name="map">Incoming data to deserialize</param>
		public void Deserialize(OSDMap map)
		{
			OSDArray array = (OSDArray)map.get("ObjectData");
			if (array != null)
			{
				ObjectPhysicsProperties = new PhysicsProperties[array.count()];

				for (int i = 0; i < array.count(); i++)
				{
					ObjectPhysicsProperties[i] = PhysicsProperties.FromOSD(array.get(i));
				}
			}
			else
			{
				ObjectPhysicsProperties = new PhysicsProperties[0];
			}
		}
	}

	//endregion Object Messages

	//region Object Media Messages
	/// <summary>
	/// A message sent from the viewer to the simulator which 
	/// specifies that the user has changed current URL
	/// of the specific media on a prim face
	/// </summary>
	public static class ObjectMediaNavigateMessage implements IMessage
	{
		/// <summary>
		/// New URL
		/// </summary>
		public String  URL;

		/// <summary>
		/// Prim UUID where navigation occured
		/// </summary>
		public UUID PrimID;

		/// <summary>
		/// Face index
		/// </summary>
		public int Face;
		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(3);

			map.put("current_url", OSD.FromString(URL));
			map.put("object_id", OSD.FromUUID(PrimID));
			map.put("texture_index", OSD.FromInteger(Face));

			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			URL = map.get("current_url").asString();
			PrimID = map.get("object_id").asUUID();
			Face = map.get("texture_index").asInteger();
		}
	}


	/// <summary>Base class used for the ObjectMedia message</summary>
	//    [Serializable]
	public static abstract class ObjectMediaBlock implements Serializable
	{
		public abstract OSDMap Serialize();
		public abstract void Deserialize(OSDMap map);
	}

	/// <summary>
	/// Message used to retrive prim media data
	/// </summary>
	public static class ObjectMediaRequest extends ObjectMediaBlock
	{
		/// <summary>
		/// Prim UUID
		/// </summary>
		public UUID PrimID;

		/// <summary>
		/// Requested operation, either GET or UPDATE
		/// </summary>
		public String  Verb = "GET"; // "GET" or "UPDATE"

		/// <summary>
		/// Serialize object
		/// </summary>
		/// <returns>Serialized object as OSDMap</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("object_id", OSD.FromUUID(PrimID));
			map.put("verb", OSD.FromString(Verb));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			PrimID = map.get("object_id").asUUID();
			Verb = map.get("verb").asString();
		}
	}


	/// <summary>
	/// Message used to update prim media data
	/// </summary>
	public static class ObjectMediaResponse extends ObjectMediaBlock
	{
		/// <summary>
		/// Prim UUID
		/// </summary>
		public UUID PrimID;

		/// <summary>
		/// Array of media entries indexed by face number
		/// </summary>
		public MediaEntry[] FaceMedia;

		/// <summary>
		/// Media version string
		/// </summary>
		public String  Version; // String in this format: x-mv:0000000016/00000000-0000-0000-0000-000000000000

		/// <summary>
		/// Serialize object
		/// </summary>
		/// <returns>Serialized object as OSDMap</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("object_id", OSD.FromUUID(PrimID));

			if (FaceMedia == null)
			{
				map.put("object_media_data", new OSDArray());
			}
			else
			{
				OSDArray mediaData = new OSDArray(FaceMedia.length);

				for (int i = 0; i < FaceMedia.length; i++)
				{
					if (FaceMedia[i] == null)
						mediaData.add(new OSD());
					else
						mediaData.add(FaceMedia[i].GetOSD());
				}

				map.put("object_media_data", mediaData);
			}

			map.put("object_media_version", OSD.FromString(Version));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			PrimID = map.get("object_id").asUUID();

			if (map.get("object_media_data").getType().equals(OSDType.Array))
			{
				OSDArray mediaData = (OSDArray)map.get("object_media_data");
				if (mediaData.count() > 0)
				{
					FaceMedia = new MediaEntry[mediaData.count()];
					for (int i = 0; i < mediaData.count(); i++)
					{
						if (mediaData.get(i).getType().equals(OSDType.Map))
						{
							FaceMedia[i] = MediaEntry.FromOSD(mediaData.get(i));
						}
					}
				}
			}
			Version = map.get("object_media_version").asString();
		}
	}


	/// <summary>
	/// Message used to update prim media data
	/// </summary>
	public static class ObjectMediaUpdate extends ObjectMediaBlock
	{
		/// <summary>
		/// Prim UUID
		/// </summary>
		public UUID PrimID;

		/// <summary>
		/// Array of media entries indexed by face number
		/// </summary>
		public MediaEntry[] FaceMedia;

		/// <summary>
		/// Requested operation, either GET or UPDATE
		/// </summary>
		public String  Verb = "UPDATE"; // "GET" or "UPDATE"

		/// <summary>
		/// Serialize object
		/// </summary>
		/// <returns>Serialized object as OSDMap</returns>

		@Override
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(2);
			map.put("object_id", OSD.FromUUID(PrimID));

			if (FaceMedia == null)
			{
				map.put("object_media_data", new OSDArray());
			}
			else
			{
				OSDArray mediaData = new OSDArray(FaceMedia.length);

				for (int i = 0; i < FaceMedia.length; i++)
				{
					if (FaceMedia[i] == null)
						mediaData.add(new OSD());
					else
						mediaData.add(FaceMedia[i].GetOSD());
				}

				map.put("object_media_data", mediaData);
			}

			map.put("verb", OSD.FromString(Verb));
			return map;
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			PrimID = map.get("object_id").asUUID();

			if (map.get("object_media_data").getType().equals(OSDType.Array))
			{
				OSDArray mediaData = (OSDArray)map.get("object_media_data");
				if (mediaData.count() > 0)
				{
					FaceMedia = new MediaEntry[mediaData.count()];
					for (int i = 0; i < mediaData.count(); i++)
					{
						if (mediaData.get(i).getType().equals(OSDType.Map))
						{
							FaceMedia[i] = MediaEntry.FromOSD(mediaData.get(i));
						}
					}
				}
			}
			Verb = map.get("verb").asString();
		}
	}

	/// <summary>
	/// Message for setting or getting per face MediaEntry
	/// </summary>
//	[Serializable]
			public static class ObjectMediaMessage implements IMessage, Serializable
			{
		/// <summary>The request or response details block</summary>
		public ObjectMediaBlock Request;

		/// <summary>
		/// Serialize the object
		/// </summary>
		/// <returns>An <see cref="OSDMap"/> containing the objects data</returns>
		public OSDMap Serialize()
		{
			return Request.Serialize();
		}

		/// <summary>
		/// Deserialize the message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("verb"))
			{
				if (map.get("verb").asString().equals("GET"))
					Request = new ObjectMediaRequest();
				else if (map.get("verb").asString().equals("UPDATE"))
					Request = new ObjectMediaUpdate();
			}
			else if (map.containsKey("object_media_version"))
				Request = new ObjectMediaResponse();
			else
				JLogger.warn("Unable to deserialize ObjectMedia: No message handler exists for method: " + map.asString());

			if (Request != null)
				Request.Deserialize(map);
		}
			}
	//endregion Object Media Messages

	//region Resource usage
	/// <summary>Details about object resource usage</summary>
	public static class ObjectResourcesDetail
	{
		/// <summary>Object UUID</summary>
		public UUID ID;
		/// <summary>Object name</summary>
		public String  Name;
		/// <summary>Indicates if object is group owned</summary>
		public boolean GroupOwned;
		/// <summary>Locatio of the object</summary>
		public Vector3d Location;
		/// <summary>Object owner</summary>
		public UUID OwnerID;
		/// <summary>Resource usage, keys are resource names, values are resource usage for that specific resource</summary>
		public Map<String, Integer> Resources;

		/// <summary>
		/// Deserializes object from OSD
		/// </summary>
		/// <param name="obj">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap obj)
		{
			ID = obj.get("id").asUUID();
			Name = obj.get("name").asString();
			Location = obj.get("location").asVector3d();
			GroupOwned = obj.get("is_group_owned").asBoolean();
			OwnerID = obj.get("owner_id").asUUID();
			OSDMap resources = (OSDMap)obj.get("resources");
			Resources = new HashMap<String, Integer>(resources.keys().size());
			for (Entry<String, OSD> kvp : resources.entrySet())
			{
				Resources.put(kvp.getKey(), kvp.getValue().asInteger());
			}
		}

		/// <summary>
		/// Makes an instance based on deserialized data
		/// </summary>
		/// <param name="osd"><see cref="OSD"/> serialized data</param>
		/// <returns>Instance containg deserialized data</returns>
		public static ObjectResourcesDetail FromOSD(OSD osd)
		{
			ObjectResourcesDetail res = new ObjectResourcesDetail();
			res.Deserialize((OSDMap)osd);
			return res;
		}
	}

	/// <summary>Details about parcel resource usage</summary>
	public static class ParcelResourcesDetail
	{
		/// <summary>Parcel UUID</summary>
		public UUID ID;
		/// <summary>Parcel local ID</summary>
		public int LocalID;
		/// <summary>Parcel name</summary>
		public String  Name;
		/// <summary>Indicates if parcel is group owned</summary>
		public boolean GroupOwned;
		/// <summary>Parcel owner</summary>
		public UUID OwnerID;
		/// <summary>Array of <see cref="ObjectResourcesDetail"/> containing per object resource usage</summary>
		public ObjectResourcesDetail[] Objects;

		/// <summary>
		/// Deserializes object from OSD
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			ID = map.get("id").asUUID();
			LocalID = map.get("local_id").asInteger();
			Name = map.get("name").asString();
			GroupOwned = map.get("is_group_owned").asBoolean();
			OwnerID = map.get("owner_id").asUUID();

			OSDArray objectsOSD = (OSDArray)map.get("objects");
			Objects = new ObjectResourcesDetail[objectsOSD.count()];

			for (int i = 0; i < objectsOSD.count(); i++)
			{
				Objects[i] = ObjectResourcesDetail.FromOSD(objectsOSD.get(i));
			}
		}

		/// <summary>
		/// Makes an instance based on deserialized data
		/// </summary>
		/// <param name="osd"><see cref="OSD"/> serialized data</param>
		/// <returns>Instance containg deserialized data</returns>
		public static ParcelResourcesDetail FromOSD(OSD osd)
		{
			ParcelResourcesDetail res = new ParcelResourcesDetail();
			res.Deserialize((OSDMap)osd);
			return res;
		}
	}

	/// <summary>Resource usage base class, both agent and parcel resource
	/// usage contains summary information</summary>
	public static abstract class BaseResourcesInfo implements IMessage
	{
		/// <summary>Summary of available resources, keys are resource names,
		/// values are resource usage for that specific resource</summary>
		public Map<String, Integer> SummaryAvailable;
		/// <summary>Summary resource usage, keys are resource names,
		/// values are resource usage for that specific resource</summary>
		public Map<String, Integer> SummaryUsed;

		/// <summary>
		/// Serializes object
		/// </summary>
		/// <returns><see cref="OSDMap"/> serialized data</returns>
		public OSDMap Serialize()
		{
			throw new NotImplementedException();
		}

		/// <summary>
		/// Deserializes object from OSD
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			SummaryAvailable = new HashMap<String, Integer>();
			SummaryUsed = new HashMap<String, Integer>();

			OSDMap summary = (OSDMap)map.get("summary");
			OSDArray available = (OSDArray)summary.get("available");
			OSDArray used = (OSDArray)summary.get("used");

			for (int i = 0; i < available.count(); i++)
			{
				OSDMap limit = (OSDMap)available.get(i);
				SummaryAvailable.put(limit.get("type").asString(), limit.get("amount").asInteger());
			}

			for (int i = 0; i < used.count(); i++)
			{
				OSDMap limit = (OSDMap)used.get(i);
				SummaryUsed.put(limit.get("type").asString(), limit.get("amount").asInteger());
			}
		}
	}

	/// <summary>Agent resource usage</summary>
	public static class AttachmentResourcesMessage extends BaseResourcesInfo
	{
		/// <summary>Per attachment point object resource usage</summary>
		public Map<AttachmentPoint, ObjectResourcesDetail[]> Attachments;

		/// <summary>
		/// Deserializes object from OSD
		/// </summary>
		/// <param name="osd">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap osd)
		{
			super.Deserialize(osd);
			OSDArray attachments = (OSDArray)((OSDMap)osd).get("attachments");
			Attachments = new HashMap<AttachmentPoint, ObjectResourcesDetail[]>();

			for (int i = 0; i < attachments.count(); i++)
			{
				OSDMap attachment = (OSDMap)attachments.get(i);
				AttachmentPoint pt = AttachmentPoint.valueOf((attachment.get("location").asString()));

				OSDArray objectsOSD = (OSDArray)attachment.get("objects");
				ObjectResourcesDetail[] objects = new ObjectResourcesDetail[objectsOSD.count()];

				for (int j = 0; j < objects.length; j++)
				{
					objects[j] = ObjectResourcesDetail.FromOSD(objectsOSD.get(j));
				}

				Attachments.put(pt, objects);
			}
		}

		/// <summary>
		/// Makes an instance based on deserialized data
		/// </summary>
		/// <param name="osd"><see cref="OSD"/> serialized data</param>
		/// <returns>Instance containg deserialized data</returns>
		public static AttachmentResourcesMessage FromOSD(OSD osd)
		{
			AttachmentResourcesMessage res = new AttachmentResourcesMessage();
			res.Deserialize((OSDMap)osd);
			return res;
		}

		/// <summary>
		/// Detects which class handles deserialization of this message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		/// <returns>Object capable of decoding this message</returns>
		public static IMessage GetMessageHandler(OSDMap map)
		{
			if (map == null)
			{
				return null;
			}
			else
			{
				return new AttachmentResourcesMessage();
			}
		}
	}

	/// <summary>Request message for parcel resource usage</summary>
	public static class LandResourcesRequest implements IMessage
	{
		/// <summary>UUID of the parel to request resource usage info</summary>
		public UUID ParcelID;

		/// <summary>
		/// Serializes object
		/// </summary>
		/// <returns><see cref="OSDMap"/> serialized data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			map.put("parcel_id", OSD.FromUUID(ParcelID));
			return map;
		}

		/// <summary>
		/// Deserializes object from OSD
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map)
		{
			ParcelID = map.get("parcel_id").asUUID();
		}
	}

	/// <summary>Response message for parcel resource usage</summary>
	public static class LandResourcesMessage implements IMessage
	{
		/// <summary>URL where parcel resource usage details can be retrieved</summary>
		public URI ScriptResourceDetails;
		/// <summary>URL where parcel resource usage summary can be retrieved</summary>
		public URI ScriptResourceSummary;

		/// <summary>
		/// Serializes object
		/// </summary>
		/// <returns><see cref="OSDMap"/> serialized data</returns>
		public OSDMap Serialize()
		{
			OSDMap map = new OSDMap(1);
			if (ScriptResourceSummary != null)
			{
				map.put("ScriptResourceSummary", OSD.FromString(ScriptResourceSummary.toString()));
			}

			if (ScriptResourceDetails != null)
			{
				map.put("ScriptResourceDetails", OSD.FromString(ScriptResourceDetails.toString()));
			}
			return map;
		}

		/// <summary>
		/// Deserializes object from OSD
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		public void Deserialize(OSDMap map) throws URISyntaxException
		{
			if (map.containsKey("ScriptResourceSummary"))
			{
				ScriptResourceSummary = new URI(map.get("ScriptResourceSummary").asString());
			}
			if (map.containsKey("ScriptResourceDetails"))
			{
				ScriptResourceDetails = new URI(map.get("ScriptResourceDetails").asString());
			}
		}

		/// <summary>
		/// Detects which class handles deserialization of this message
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>
		/// <returns>Object capable of decoding this message</returns>
		public static IMessage GetMessageHandler(OSDMap map)
		{
			if (map.containsKey("parcel_id"))
			{
				return new LandResourcesRequest();
			}
			else if (map.containsKey("ScriptResourceSummary"))
			{
				return new LandResourcesMessage();
			}
			return null;
		}
	}

	/// <summary>Parcel resource usage</summary>
	public static class LandResourcesInfo extends BaseResourcesInfo
	{
		/// <summary>Array of <see cref="ParcelResourcesDetail"/> containing per percal resource usage</summary>
		public ParcelResourcesDetail[] Parcels;

		/// <summary>
		/// Deserializes object from OSD
		/// </summary>
		/// <param name="map">An <see cref="OSDMap"/> containing the data</param>

		@Override
		public void Deserialize(OSDMap map)
		{
			if (map.containsKey("summary"))
			{
				super.Deserialize(map);
			}
			else if (map.containsKey("parcels"))
			{
				OSDArray parcelsOSD = (OSDArray)map.get("parcels");
				Parcels = new ParcelResourcesDetail[parcelsOSD.count()];

				for (int i = 0; i < parcelsOSD.count(); i++)
				{
					Parcels[i] = ParcelResourcesDetail.FromOSD(parcelsOSD.get(i));
				}
			}
		}
	}

	//endregion Resource usage

	//region Display names
	/// <summary>
	/// Reply to request for bunch if display names
	/// </summary>
	public static class GetDisplayNamesMessage implements IMessage
	{
		/// <summary> Current display name </summary>
		public AgentDisplayName[] Agents = new AgentDisplayName[0];

		/// <summary> Following UUIDs failed to return a valid display name </summary>
		public UUID[] BadIDs = new UUID[0];

		/// <summary>
		/// Serializes the message
		/// </summary>
		/// <returns>OSD containting the messaage</returns>
		public OSDMap Serialize()
		{
			OSDArray agents = new OSDArray();

			if (Agents != null && Agents.length > 0)
			{
				for (int i=0; i<Agents.length; i++)
				{
					agents.add(Agents[i].GetOSD());
				}
			}

			OSDArray badIDs = new OSDArray();
			if (BadIDs != null && BadIDs.length > 0)
			{
				for (int i=0; i<BadIDs.length; i++)
				{
					badIDs.add(new OSDUUID(BadIDs[i]));
				}
			}

			OSDMap ret = new OSDMap();
			ret.put("agents", agents);
			ret.put("bad_ids", badIDs);
			return ret;
		}

		public void Deserialize(OSDMap map)
		{
			if (map.get("agents").getType().equals(OSDType.Array))
			{
				OSDArray osdAgents = (OSDArray) map.get("agents");

				if (osdAgents.count() > 0)
				{
					Agents = new AgentDisplayName[osdAgents.count()];

					for (int i = 0; i < osdAgents.count(); i++)
					{
						Agents[i] = AgentDisplayName.FromOSD(osdAgents.get(i));
					}
				}
			}

			if (map.get("bad_ids").getType().equals(OSDType.Array))
			{
				OSDArray osdBadIDs = (OSDArray) map.get("bad_ids");
				if (osdBadIDs.count() > 0)
				{
					BadIDs = new UUID[osdBadIDs.count()];

					for (int i=0; i<osdBadIDs.count(); i++)
					{
						BadIDs[i] = osdBadIDs.get(i).asUUID();
					}
				}
			}
		}
	}

	/// <summary>
	/// Message sent when requesting change of the display name
	/// </summary>
	public static class SetDisplayNameMessage implements IMessage
	{
		/// <summary> Current display name </summary>
		public String  OldDisplayName;

		/// <summary> Desired new display name </summary>
		public String  NewDisplayName;

		/// <summary>
		/// Serializes the message
		/// </summary>
		/// <returns>OSD containting the messaage</returns>
		public OSDMap Serialize()
		{
			OSDArray names = new OSDArray(2);
					
			names.add(OSD.FromString(OldDisplayName));
			names.add(OSD.FromString(NewDisplayName));

			OSDMap name = new OSDMap();
			name.put("display_name", names);
			return name;
		}

		public void Deserialize(OSDMap map)
		{
			OSDArray names = (OSDArray)map.get("display_name");
			OldDisplayName = names.get(0).asString();
			NewDisplayName = names.get(1).asString();
		}
	}

	/// <summary>
	/// Message recieved in response to request to change display name
	/// </summary>
	public static class SetDisplayNameReplyMessage implements IMessage
	{
		/// <summary> New display name </summary>
		public AgentDisplayName DisplayName;

		/// <summary> String message indicating the result of the operation </summary>
		public String  Reason;

		/// <summary> Numerical code of the result, 200 indicates success </summary>
		public int Status;

		/// <summary>
		/// Serializes the message
		/// </summary>
		/// <returns>OSD containting the messaage</returns>
		public OSDMap Serialize()
		{
			OSDMap agent = (OSDMap)DisplayName.GetOSD();
			OSDMap ret = new OSDMap();
			ret.put("content", agent);
			ret.put("reason", OSD.FromString(Reason));
			ret.put("status", OSD.FromInteger((Status)));
			return ret;
		}

		public void Deserialize(OSDMap map)
		{
			OSDMap agent = (OSDMap)map.get("content");
			DisplayName = AgentDisplayName.FromOSD(agent);
			Reason = map.get("reason").asString();
			Status = map.get("status").asInteger();
		}
	}

	/// <summary>
	/// Message recieved when someone nearby changes their display name
	/// </summary>
	public static class DisplayNameUpdateMessage implements IMessage
	{
		/// <summary> Previous display name, empty string if default </summary>
		public String  OldDisplayName;

		/// <summary> New display name </summary>
		public AgentDisplayName DisplayName;

		/// <summary>
		/// Serializes the message
		/// </summary>
		/// <returns>OSD containting the messaage</returns>
		public OSDMap Serialize()
		{
			OSDMap agent = (OSDMap)DisplayName.GetOSD();
			agent.put("old_display_name", OSD.FromString(OldDisplayName));
			OSDMap ret = new OSDMap();
			ret.put("agent", agent);
			return ret;
		}

		public void Deserialize(OSDMap map)
		{
			OSDMap agent = (OSDMap)map.get("agent");
			DisplayName = AgentDisplayName.FromOSD(agent);
			OldDisplayName = agent.get("old_display_name").asString();
		}
	}
	//    endregion Display names
}
