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
package com.ngt.jopenmetaverse.shared.sim.login;

import java.io.IOException;
import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.InetAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import java.util.logging.Logger;
import com.ngt.jopenmetaverse.shared.sim.InventoryManager.InventoryFolder;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDException;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.NotationalLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.PlatformUtils;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class LoginResponseData {

	Logger logger = Logger.getLogger(getClass().toString());
	
	/// <summary>true, false, indeterminate</summary>
	//[XmlRpcMember("login")]
	public String Login;
	public boolean Success;
	public String Reason;
	/// <summary>Login message of the day</summary>
	public String Message;
	public UUID AgentID;
	public UUID SessionID;
	public UUID SecureSessionID;
	public String FirstName;
	public String LastName;
	public String StartLocation;
	/// <summary>M or PG, also agent_region_access and agent_access_max</summary>
	public String AgentAccess;
	public Vector3 LookAt;
	//ulong
	public BigInteger HomeRegion;
	public Vector3 HomePosition;
	public Vector3 HomeLookAt;
	public int CircuitCode;
	public int RegionX;
	public int RegionY;
	public int SimPort;
	public InetAddress SimIP;
	public String SeedCapability;
	public BuddyListEntry[] BuddyList;
	public int SecondsSinceEpoch;
	public String UDPBlacklist;

	//region Inventory

	public UUID InventoryRoot;
	public UUID LibraryRoot;
	public InventoryFolder[] InventorySkeleton;
	public InventoryFolder[] LibrarySkeleton;
	public UUID LibraryOwner;

	//endregion

	//region Redirection

	public String NextMethod;
	public String NextUrl;
	public String[] NextOptions;
	public int NextDuration;

	//endregion

	// These aren't currently being utilized by the library
	public String AgentAccessMax;
	public String AgentRegionAccess;
	public int AOTransition;
	public String InventoryHost;
	public int MaxAgentGroups;
	public String OpenIDUrl;
	public String XMPPHost;

	/// <summary>
	/// Parse LLSD Login Reply Data
	/// </summary>
	/// <param name="reply">An <seealso cref="OSDMap"/> 
	/// contaning the login response data</param>
	/// <remarks>XML-RPC logins do not require this as XML-RPC.NET 
	/// automatically populates the struct properly using attributes</remarks>
	public void Parse(OSDMap reply) throws OSDException, IOException
	{
		try
		{
			AgentID = ParseUUID("agent_id", reply);
			SessionID = ParseUUID("session_id", reply);
			SecureSessionID = ParseUUID("secure_session_id", reply);
//			FirstName = ParseString("first_name", reply).trim('"');
//			LastName = ParseString("last_name", reply).Trim('"');
			//TODO need to verify the difference
			FirstName = ParseString("first_name", reply).trim();
			LastName = ParseString("last_name", reply).trim();
			StartLocation = ParseString("start_location", reply);
			AgentAccess = ParseString("agent_access", reply);
			LookAt = ParseVector3("look_at", reply);
			Reason = ParseString("reason", reply);
			Message = ParseString("message", reply);

			Login = reply.get("login").asString();
			Success = reply.get("login").asBoolean();
		}
		catch (OSDException e)
		{
			logger.warning("Login server returned (some) invalid data: " + e.getMessage());
		}

		// Home
		OSDMap home = null;
		OSD osdHome = NotationalLLSDOSDParser.DeserializeLLSDNotation(reply.get("home").asString());

		if (osdHome.getType().equals(OSDType.Map))
		{
			home = (OSDMap)osdHome;

			OSD homeRegion = null;
			if ( ((homeRegion = home.get("region_handle")) !=null) && homeRegion.getType().equals(OSDType.Array))
			{
				OSDArray homeArray = (OSDArray)homeRegion;
				if (homeArray.count() == 2)
					HomeRegion = new BigInteger(Utils.int64ToBytes(Utils.uintsToLong((long)homeArray.get(0).asInteger(), (long)homeArray.get(1).asInteger())));
				else
					HomeRegion = new BigInteger("0");
			}

			HomePosition = ParseVector3("position", home);
			HomeLookAt = ParseVector3("look_at", home);
		}
		else
		{
			HomeRegion = new BigInteger("0");
			HomePosition = Vector3.Zero;
			HomeLookAt = Vector3.Zero;
		}

		CircuitCode = (int)ParseUInt("circuit_code", reply);
		RegionX = (int)ParseUInt("region_x", reply);
		RegionY = (int)ParseUInt("region_y", reply);
		SimPort = (short)ParseUInt("sim_port", reply);
		String simIP = ParseString("sim_ip", reply);
		InetAddress[] simIParray = new InetAddress[1];
		if(PlatformUtils.tryParseInetAddress(simIP, simIParray))
			SimIP = simIParray[0];
		else
			simIParray = null;
		
		
		SeedCapability = ParseString("seed_capability", reply);

		// Buddy list
		OSD buddyLLSD = null;
		if ( ((buddyLLSD = reply.get("buddy-list"))!=null) && buddyLLSD.getType().equals(OSDType.Array))
		{
			List<BuddyListEntry> buddys = new ArrayList<BuddyListEntry>();
			OSDArray buddyArray = (OSDArray)buddyLLSD;
			for (int i = 0; i < buddyArray.count(); i++)
			{
				if (buddyArray.get(i).getType().equals(OSDType.Map))
				{
					BuddyListEntry bud = new BuddyListEntry();
					OSDMap buddy = (OSDMap)buddyArray.get(i);

					bud.buddy_id = buddy.get("buddy_id").asString();
					bud.buddy_rights_given = (int)ParseUInt("buddy_rights_given", buddy);
					bud.buddy_rights_has = (int)ParseUInt("buddy_rights_has", buddy);

					buddys.add(bud);
				}
				BuddyList = buddys.toArray(new BuddyListEntry[0]);
			}
		}

		SecondsSinceEpoch = (int)ParseUInt("seconds_since_epoch", reply);

		InventoryRoot = ParseMappedUUID("inventory-root", "folder_id", reply);
		InventorySkeleton = ParseInventorySkeleton("inventory-skeleton", reply);

		LibraryOwner = ParseMappedUUID("inventory-lib-owner", "agent_id", reply);
		LibraryRoot = ParseMappedUUID("inventory-lib-root", "folder_id", reply);
		LibrarySkeleton = ParseInventorySkeleton("inventory-skel-lib", reply);
	}

	public void Parse(Map<String, Object> reply) throws OSDException, IOException
	{
		try
		{
			AgentID = ParseUUID("agent_id", reply);
			SessionID = ParseUUID("session_id", reply);
			SecureSessionID = ParseUUID("secure_session_id", reply);
			FirstName = ParseString("first_name", reply).trim();
			LastName = ParseString("last_name", reply).trim();
			// "first_login" for brand new accounts
			StartLocation = ParseString("start_location", reply);
			AgentAccess = ParseString("agent_access", reply);
			LookAt = ParseVector3("look_at", reply);
			Reason = ParseString("reason", reply);
			Message = ParseString("message", reply);
			
			if (reply.containsKey("login"))
			{
				Login = (String)reply.get("login");
				Success = Login.equals("true");

				// Parse redirect options
				if (Login.equals("indeterminate"))
				{
					NextUrl = ParseString("next_url", reply);
					NextDuration = (int)ParseUInt("next_duration", reply);
					NextMethod = ParseString("next_method", reply);
					NextOptions = (String[])((List)reply.get("next_options")).toArray(new String[0]);
				}
			}
		}
		catch (Exception e)
		{
			logger.warning("Login server returned (some) invalid data: " + e.getMessage());
		}
		if (!Success)
			return;

		// Home
		OSDMap home = null;
		if (reply.containsKey("home"))
		{
			OSD osdHome = NotationalLLSDOSDParser.DeserializeLLSDNotation(reply.get("home").toString());

			if (osdHome.getType() == OSDType.Map)
			{
				home = (OSDMap)osdHome;

				OSD homeRegion = home.get("region_handle");
				if (homeRegion !=null && homeRegion.getType().equals(OSDType.Array))
				{
					OSDArray homeArray = (OSDArray)homeRegion;
					if (homeArray.count() == 2)
						HomeRegion = Utils.uintsToULong(homeArray.get(0).asUInteger(),
								homeArray.get(1).asUInteger());
					else
						HomeRegion = BigInteger.ZERO;
				}

				HomePosition = ParseVector3("position", home);
				HomeLookAt = ParseVector3("look_at", home);
			}
		}
		else
		{
			HomeRegion = BigInteger.ZERO;
			HomePosition = Vector3.Zero;
			HomeLookAt = Vector3.Zero;
		}

		CircuitCode = (int)ParseUInt("circuit_code", reply);
		RegionX = (int)ParseUInt("region_x", reply);
		RegionY = (int)ParseUInt("region_y", reply);
		SimPort = (short)ParseUInt("sim_port", reply);
		String simIP = ParseString("sim_ip", reply);
		
//		IPAddress.TryParse(simIP, out SimIP);
		
		InetAddress[] simIParray = new InetAddress[1];
		if(PlatformUtils.tryParseInetAddress(simIP, simIParray))
			SimIP = simIParray[0];
		else
			simIParray = null;
		
		SeedCapability = ParseString("seed_capability", reply);

		// Buddy list
		if (reply.containsKey("buddy-list") && reply.get("buddy-list") instanceof Object[])
		{
			Vector<BuddyListEntry> buddys = new Vector<BuddyListEntry>();

			Object[] buddyArray = (Object[])reply.get("buddy-list");
			for (int i = 0; i < buddyArray.length; i++)
			{
				if (buddyArray[i] instanceof Map)
				{
					BuddyListEntry bud = new BuddyListEntry();
					Map<String, Object> buddy = (Map<String, Object> )buddyArray[i];

					bud.buddy_id = ParseString("buddy_id", buddy);
					bud.buddy_rights_given = (int)ParseUInt("buddy_rights_given", buddy);
					bud.buddy_rights_has = (int)ParseUInt("buddy_rights_has", buddy);

					buddys.add(bud);
				}
			}

			BuddyList = buddys.toArray(new BuddyListEntry[0]);
//			System.out.println("Buddy List: " + BuddyList.toString());
		}

		SecondsSinceEpoch = (int)ParseUInt("seconds_since_epoch", reply);

		InventoryRoot = ParseMappedUUID("inventory-root", "folder_id", reply);
		InventorySkeleton = ParseInventorySkeleton("inventory-skeleton", reply);

		LibraryOwner = ParseMappedUUID("inventory-lib-owner", "agent_id", reply);
		LibraryRoot = ParseMappedUUID("inventory-lib-root", "folder_id", reply);
		LibrarySkeleton = ParseInventorySkeleton("inventory-skel-lib", reply);

		// UDP Blacklist
		if (reply.containsKey("udp_blacklist"))
		{
			UDPBlacklist = ParseString("udp_blacklist", reply);
		}

		if (reply.containsKey("max-agent-groups"))
		{
			MaxAgentGroups = (int)ParseUInt("max-agent-groups", reply);
		}
		else
		{
			MaxAgentGroups = -1;
		}

		if (reply.containsKey("openid_url"))
		{
			OpenIDUrl = ParseString("openid_url", reply);
		}

		if (reply.containsKey("xmpp_host"))
		{
			XMPPHost = ParseString("xmpp_host", reply);
		}

	}

	//region Parsing Helpers
	public static long ParseUInt(String key, OSDMap reply)
	{
		OSD osd;
		if ((osd = reply.get(key)) != null)
			return osd.asLong();
		else
			return 0;
	}

	public static long ParseUInt(String key, Map reply)
	{
		if (reply.containsKey(key))
		{
			Object value = reply.get(key);
			if (value instanceof Integer)
				return (Integer)value & Long.MAX_VALUE;
		}

		return 0;
	}

	public static UUID ParseUUID(String key, OSDMap reply)
	{
		OSD osd;
		if (((osd = reply.get(key)) != null))
			return osd.asUUID();
		else
			return new UUID();
	}

	public static UUID ParseUUID(String key, Map reply)
	{
		if (reply.containsKey(key))
		{
			UUID[] value = new UUID[1];
			if (UUID.TryParse((String)reply.get(key), value))
				return value[0];
		}

		return UUID.Zero;
	}

	public static String ParseString(String key, OSDMap reply)
	{
		OSD osd;
		if (((osd = reply.get(key)) != null))
			return osd.asString();
		else
			return "";
	}

	public static String ParseString(String key, Map reply)
	{
		if (reply.containsKey(key))
			return String.format("%s", reply.get(key));

		return "";
	}

	public static Vector3 ParseVector3(String key, OSDMap reply) throws OSDException, IOException
	{
		OSD osd;
		if (((osd = reply.get(key)) != null))
		{
			if (osd.getType().equals(OSDType.Array))
			{
				return ((OSDArray)osd).asVector3();
			}
			else if (osd.getType().equals(OSDType.String))
			{
				OSDArray array = (OSDArray)NotationalLLSDOSDParser.DeserializeLLSDNotation(osd.asString());
				return array.asVector3();
			}
		}

		return Vector3.Zero;
	}

	public static Vector3 ParseVector3(String key, Map reply) throws OSDException, IOException
	{
		if (reply.containsKey(key))
		{
			Object value = reply.get(key);
			
			if (value instanceof Object[])
			{
				Object[] list = (Object[])value;
				if (list.length == 3)
				{
					float[][] xyz =  new float[3][1];
					
					Utils.tryParseFloat((String)list[0], xyz[0]);
					Utils.tryParseFloat((String)list[1], xyz[1]);
					Utils.tryParseFloat((String)list[2], xyz[2]);

					return new Vector3(xyz[0][0], xyz[1][0], xyz[2][0]);
				}
			}
			else if (value instanceof String)
			{
				OSDArray array = (OSDArray)NotationalLLSDOSDParser.DeserializeLLSDNotation((String)value);
				return array.asVector3();
			}
			else
				JLogger.warn("ParseVector3 Got unknown type for key " + key + " " + value.toString());
		}

		return Vector3.Zero;
	}

	public static UUID ParseMappedUUID(String key, String key2, OSDMap reply)
	{
		OSD folderOSD;
		if (((folderOSD= reply.get(key))!=null) && folderOSD.getType().equals(OSDType.Array))
		{
			OSDArray array = (OSDArray)folderOSD;
			if (array.count() == 1 && array.get(0).getType().equals(OSDType.Map))
			{
				OSDMap map = (OSDMap)array.get(0);
				OSD folder;
				if ((folder = map.get(key2))!=null )
					return folder.asUUID();
			}
		}
		
		return UUID.Zero;
	}

	public static UUID ParseMappedUUID(String key, String key2, Map reply)
	{
		if (reply.containsKey(key) && reply.get(key) instanceof Object[])
		{
			Object[] array = (Object[])reply.get(key);
			if (array.length == 1 && array[0] instanceof Map)
			{
				Map map = (Map)array[0];
				return ParseUUID(key2, map);
			}
		}
		else
			JLogger.warn("ParseVector3 Got unknown type for key " + key + " ");

		return UUID.Zero;
	}

	public static InventoryFolder[] ParseInventoryFolders(String key, UUID owner, OSDMap reply)
	{
		List<InventoryFolder> folders = new ArrayList<InventoryFolder>();

		OSD skeleton;
		if (((skeleton = reply.get(key))!=null) && skeleton.getType().equals(OSDType.Array))
		{
			OSDArray array = (OSDArray)skeleton;

			for (int i = 0; i < array.count(); i++)
			{
				if (array.get(i).getType().equals(OSDType.Map))
				{
					OSDMap map = (OSDMap)array.get(i);
					InventoryFolder folder = new InventoryFolder(map.get("folder_id").asUUID());
					folder.PreferredType = AssetType.get((byte)map.get("type_default").asInteger());
					folder.Version = map.get("version").asInteger();
					folder.OwnerID = owner;
					folder.ParentUUID = map.get("parent_id").asUUID();
					folder.Name = map.get("name").asString();

					folders.add(folder);
				}
			}
		}

		return folders.toArray(new InventoryFolder[0]);
	}

	public InventoryFolder[] ParseInventorySkeleton(String key, OSDMap reply)
	{
		List<InventoryFolder> folders = new ArrayList<InventoryFolder>();
		OSD skeleton = reply.get(key);
		if (skeleton != null && skeleton.getType() == OSDType.Array)
		{
			OSDArray array = (OSDArray)skeleton;
			for (int i = 0; i < array.count(); i++)
			{
				if (array.get(i).getType().equals(OSDType.Map))
				{
					OSDMap map = (OSDMap)array.get(i);
					InventoryFolder folder = new InventoryFolder(map.get("folder_id").asUUID());
					folder.Name = map.get("name").asString();
					folder.ParentUUID = map.get("parent_id").asUUID();
					folder.PreferredType = AssetType.get((byte)map.get("type_default").asInteger());
					folder.Version = map.get("version").asInteger();
					folders.add(folder);
				}
			}
		}
		return folders.toArray(new InventoryFolder[0]);
	}

	public InventoryFolder[] ParseInventorySkeleton(String key, Map reply)
	{
		UUID ownerID;
		if (key.equals("inventory-skel-lib"))
			ownerID = LibraryOwner;
		else
			ownerID = AgentID;

		List<InventoryFolder> folders = new ArrayList<InventoryFolder>();

		if (reply.containsKey(key) && reply.get(key) instanceof Object[])
		{
			Object[] array = (Object[])reply.get(key);
			for (int i = 0; i < array.length; i++)
			{
				if (array[i] instanceof Map)
				{
					Map map = (Map)array[i];
					InventoryFolder folder = new InventoryFolder(ParseUUID("folder_id", map));
					folder.Name = ParseString("name", map);
					folder.ParentUUID = ParseUUID("parent_id", map);
					folder.PreferredType = AssetType.get((byte)ParseUInt("type_default", map));
					folder.Version = (int)ParseUInt("version", map);
					folder.OwnerID = ownerID;

					folders.add(folder);
				}
			}
		}
		else
			JLogger.warn("ParseVector3 Got unknown type for key " + key );
		

		return folders.toArray(new InventoryFolder[0]);
	}

	//endregion Parsing Helpers
	
	public Map<String, Object> getMap() throws IllegalArgumentException, IllegalAccessException {
	    Map<String, Object> result = new HashMap<String, Object>();
	    Field[] declaredFields = getClass().getDeclaredFields();
	    for (Field field : declaredFields) {
	        result.put(field.getName(), field.get(this));
//	        System.out.println(field.getName() + " " + field.get(this));
	    }
	    return result;
	}
	
}
