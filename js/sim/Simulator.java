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

import java.math.BigInteger;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.Set;
import java.util.SortedMap;
import java.util.Timer;
import java.util.TimerTask;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;

import com.ngt.jopenmetaverse.shared.protocol.AgentPausePacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentResumePacket;
import com.ngt.jopenmetaverse.shared.protocol.CloseCircuitPacket;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.protocol.MalformedDataException;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketAckPacket;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.StartPingCheckPacket;
import com.ngt.jopenmetaverse.shared.protocol.UseCircuitCodePacket;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.sim.ParcelManager.Parcel;
import com.ngt.jopenmetaverse.shared.sim.TerrainCompressor.TerrainPatch;
import com.ngt.jopenmetaverse.shared.sim.buffers.UDPPacketBuffer;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.Caps;
import com.ngt.jopenmetaverse.shared.sim.stats.UtilizationStatistics;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector2;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class Simulator extends UDPBase
{
	//region Enums

	/// <summary>
	/// Simulator (region) properties
	/// </summary>
	public static enum RegionFlags
	{
		/// <summary>No flags set</summary>
		None(0),
		/// <summary>Agents can take damage and be killed</summary>
		AllowDamage(1 << 0),
		/// <summary>Landmarks can be created here</summary>
		AllowLandmark(1 << 1),
		/// <summary>Home position can be set in this sim</summary>
		AllowSetHome(1 << 2),
		/// <summary>Home position is reset when an agent teleports away</summary>
		ResetHomeOnTeleport(1 << 3),
		/// <summary>Sun does not move</summary>
		SunFixed(1 << 4),
		/// <summary>No object, land, etc. taxes</summary>
		TaxFree(1 << 5),
		/// <summary>Disable heightmap alterations (agents can still plant
		/// foliage)</summary>
		BlockTerraform(1 << 6),
		/// <summary>Land cannot be released, sold, or purchased</summary>
		BlockLandResell(1 << 7),
		/// <summary>All content is wiped nightly</summary>
		Sandbox(1 << 8),
		/// <summary>Unknown: Related to the availability of an overview world map tile.(Think mainland images when zoomed out.)</summary>
		NullLayer(1 << 9),
		/// <summary>Unknown: Related to region debug flags. Possibly to skip processing of agent interaction with world. </summary>
		SkipAgentAction( 1 << 10),
		/// <summary>Region does not update agent prim interest lists. Internal debugging option.</summary>
		SkipUpdateInterestList( 1 << 11),
		/// <summary>No collision detection for non-agent objects</summary>
		SkipCollisions( 1 << 12),
		/// <summary>No scripts are ran</summary>
		SkipScripts( 1 << 13),
		/// <summary>All physics processing is turned off</summary>
		SkipPhysics( 1 << 14),
		/// <summary>Region can be seen from other regions on world map. (Legacy world map option?) </summary>
		ExternallyVisible( 1 << 15),
		/// <summary>Region can be seen from mainland on world map. (Legacy world map option?) </summary>
		MainlandVisible( 1 << 16),
		/// <summary>Agents not explicitly on the access list can visit the region. </summary>
		PublicAllowed( 1 << 17),
		/// <summary>Traffic calculations are not run across entire region, overrides parcel settings. </summary>
		BlockDwell( 1 << 18),
		/// <summary>Flight is disabled (not currently enforced by the sim)</summary>
		NoFly( 1 << 19),
		/// <summary>Allow direct (p2p) teleporting</summary>
		AllowDirectTeleport( 1 << 20),
		/// <summary>Estate owner has temporarily disabled scripting</summary>
		EstateSkipScripts( 1 << 21),
		/// <summary>Restricts the usage of the LSL llPushObject function, applies to whole region.</summary>
		RestrictPushObject( 1 << 22),
		/// <summary>Deny agents with no payment info on file</summary>
		DenyAnonymous( 1 << 23),
		/// <summary>Deny agents with payment info on file</summary>
		DenyIdentified( 1 << 24),
		/// <summary>Deny agents who have made a monetary transaction</summary>
		DenyTransacted( 1 << 25),
		/// <summary>Parcels within the region may be joined or divided by anyone, not just estate owners/managers. </summary>
		AllowParcelChanges( 1 << 26),
		/// <summary>Abuse reports sent from within this region are sent to the estate owner defined email. </summary>
		AbuseEmailToEstateOwner( 1 << 27),
		/// <summary>Region is Voice Enabled</summary>
		AllowVoice( 1 << 28),
		/// <summary>Removes the ability from parcel owners to set their parcels to show in search.</summary>
		BlockParcelSearch( 1 << 29),
		/// <summary>Deny agents who have not been age verified from entering the region.</summary>
		DenyAgeUnverified( 1 << 30);

		private int index;
		RegionFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}  

		private static final Map<Integer,RegionFlags> lookup  = new HashMap<Integer,RegionFlags>();

		static {
			for(RegionFlags s : EnumSet.allOf(RegionFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<RegionFlags> get(Integer index)
        {
                EnumSet<RegionFlags> enumsSet = EnumSet.allOf(RegionFlags.class);
                for(Entry<Integer,RegionFlags> entry: lookup.entrySet())
                {
                        if((entry.getKey().longValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }
        
        public static int getIndex(EnumSet<RegionFlags> enumSet)
        {
                int ret = 0;
                for(RegionFlags s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }
	

	}

	/// <summary>
	/// Access level for a simulator
	/// </summary>
	public static enum SimAccess
	{
		//Represent one byte in size
		/// <summary>Unknown or invalid access level</summary>
		Unknown ((short)0),
		/// <summary>Trial accounts allowed</summary>
		Trial ((short)7),
		/// <summary>PG rating</summary>
		PG ((short)13),
		/// <summary>Mature rating</summary>
		Mature ((short)21),
		/// <summary>Adult rating</summary>
		Adult ((short)42),
		/// <summary>Simulator is offline</summary>
		Down ((short)254),
		/// <summary>Simulator does not exist</summary>
		NonExistent ((short)255);

		private short index;
		SimAccess(short index)
		{
			this.index = index;
		}     

		public short getIndex()
		{
			return index;
		}  

		private static final Map<Short,SimAccess> lookup  = new HashMap<Short,SimAccess>();

		static {
			for(SimAccess s : EnumSet.allOf(SimAccess.class))
				lookup.put(s.getIndex(), s);
		}

		public static SimAccess get(Short index)
		{
			return lookup.get(index);
		}	
	}

	//endregion Enums

	//region Structs
	/// <summary>
	/// Simulator Statistics
	/// </summary>
	public static class SimStats
	{
		/// <summary>Total number of packets sent by this simulator to this agent</summary>
		public AtomicLong SentPackets = new AtomicLong(0);
		/// <summary>Total number of packets received by this simulator to this agent</summary>
		public AtomicLong RecvPackets = new AtomicLong(0);
		/// <summary>Total number of bytes sent by this simulator to this agent</summary>
		public AtomicLong SentBytes = new AtomicLong(0);
		/// <summary>Total number of bytes received by this simulator to this agent</summary>
		public AtomicLong RecvBytes = new AtomicLong(0);
		/// <summary>Time in seconds agent has been connected to simulator</summary>
		public long ConnectTime;
		/// <summary>Total number of packets that have been resent</summary>
		public AtomicInteger ResentPackets = new AtomicInteger(0);
		/// <summary>Total number of resent packets recieved</summary>
		public AtomicInteger ReceivedResends = new AtomicInteger(0);
		/// <summary>Total number of pings sent to this simulator by this agent</summary>
		public AtomicInteger SentPings  = new AtomicInteger(0);
		/// <summary>Total number of ping replies sent to this agent by this simulator</summary>
		public int ReceivedPongs;
		/// <summary>
		/// Incoming bytes per second
		/// </summary>
		/// <remarks>It would be nice to have this claculated on the fly, but
		/// this is far, far easier</remarks>
		public int IncomingBPS;
		/// <summary>
		/// Outgoing bytes per second
		/// </summary>
		/// <remarks>It would be nice to have this claculated on the fly, but
		/// this is far, far easier</remarks>
		public int OutgoingBPS;
		/// <summary>Time last ping was sent</summary>
		public long LastPingSent;
		/// <summary>ID of last Ping sent</summary>
		public byte LastPingID;
		/// <summary></summary>
		public long LastLag;
		/// <summary></summary>
		public int MissedPings;
		/// <summary>Current time dilation of this simulator</summary>
		public float Dilation;
		/// <summary>Current Frames per second of simulator</summary>
		public int FPS;
		/// <summary>Current Physics frames per second of simulator</summary>
		public float PhysicsFPS;
		/// <summary></summary>
		public float AgentUpdates;
		/// <summary></summary>
		public float FrameTime;
		/// <summary></summary>
		public float NetTime;
		/// <summary></summary>
		public float PhysicsTime;
		/// <summary></summary>
		public float ImageTime;
		/// <summary></summary>
		public float ScriptTime;
		/// <summary></summary>
		public float AgentTime;
		/// <summary></summary>
		public float OtherTime;
		/// <summary>Total number of objects Simulator is simulating</summary>
		public int Objects;
		/// <summary>Total number of Active (Scripted) objects running</summary>
		public int ScriptedObjects;
		/// <summary>Number of agents currently in this simulator</summary>
		public int Agents;
		/// <summary>Number of agents in neighbor simulators</summary>
		public int ChildAgents;
		/// <summary>Number of Active scripts running in this simulator</summary>
		public int ActiveScripts;
		/// <summary></summary>
		public int LSLIPS;
		/// <summary></summary>
		public int INPPS;
		/// <summary></summary>
		public int OUTPPS;
		/// <summary>Number of downloads pending</summary>
		public int PendingDownloads;
		/// <summary>Number of uploads pending</summary>
		public int PendingUploads;
		/// <summary></summary>
		public int VirtualSize;
		/// <summary></summary>
		public int ResidentSize;
		/// <summary>Number of local uploads pending</summary>
		public int PendingLocalUploads;
		/// <summary>Unacknowledged bytes in queue</summary>
		public int UnackedBytes;


	}

	//endregion Structs

	//region Public Members        
	/// <summary>A public reference to the client that this Simulator object
	/// is attached to</summary>
	public GridClient Client;
	/// <summary>A Unique Cache identifier for this simulator</summary>
	public UUID ID = UUID.Zero;
	/// <summary>The capabilities for this simulator</summary>
	public Caps Caps = null;
	/// <summary></summary>
	//ulong
	public BigInteger Handle;
	/// <summary>The current version of software this simulator is running</summary>
	public String SimVersion = "";
	/// <summary></summary>
	public String Name = "";
	/// <summary>A 64x64 grid of parcel coloring values. The values stored 
	/// in this array are of the <seealso cref="ParcelArrayType"/> type</summary>
	public byte[] ParcelOverlay = new byte[4096];
	/// <summary></summary>
	public int ParcelOverlaysReceived;
	/// <summary></summary>
	public float TerrainHeightRange00;
	/// <summary></summary>
	public float TerrainHeightRange01;
	/// <summary></summary>
	public float TerrainHeightRange10;
	/// <summary></summary>
	public float TerrainHeightRange11;
	/// <summary></summary>
	public float TerrainStartHeight00;
	/// <summary></summary>
	public float TerrainStartHeight01;
	/// <summary></summary>
	public float TerrainStartHeight10;
	/// <summary></summary>
	public float TerrainStartHeight11;
	/// <summary></summary>
	public float WaterHeight;
	/// <summary></summary>
	public UUID SimOwner = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainBase0 = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainBase1 = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainBase2 = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainBase3 = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainDetail0 = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainDetail1 = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainDetail2 = UUID.Zero;
	/// <summary></summary>
	public UUID TerrainDetail3 = UUID.Zero;
	/// <summary>true if your agent has Estate Manager rights on this region</summary>
	public boolean IsEstateManager;
	/// <summary></summary>
	public EnumSet<RegionFlags> Flags;
	/// <summary></summary>
	public SimAccess Access;
	/// <summary></summary>
	public float BillableFactor;
	/// <summary>Statistics information for this simulator and the
	/// connection to the simulator, calculated by the simulator itself
	/// and the library</summary>
	public SimStats Stats = new SimStats();
	/// <summary>The regions Unique ID</summary>
	public UUID RegionID = UUID.Zero;
	/// <summary>The physical data center the simulator is located</summary>
	/// <remarks>Known values are:
	/// <list type="table">
	/// <item>Dallas</item>
	/// <item>Chandler</item>
	/// <item>SF</item>
	/// </list>
	/// </remarks>
	public String ColoLocation;
	/// <summary>The CPU Class of the simulator</summary>
	/// <remarks>Most full mainland/estate sims appear to be 5,
	/// Homesteads and Openspace appear to be 501</remarks>
	public int CPUClass;
	/// <summary>The number of regions sharing the same CPU as this one</summary>
	/// <remarks>"Full Sims" appear to be 1, Homesteads appear to be 4</remarks>
	public int CPURatio;
	/// <summary>The billing product name</summary>
	/// <remarks>Known values are:
	/// <list type="table">
	/// <item>Mainland / Full Region (Sku: 023)</item>
	/// <item>Estate / Full Region (Sku: 024)</item>
	/// <item>Estate / Openspace (Sku: 027)</item>
	/// <item>Estate / Homestead (Sku: 029)</item>
	/// <item>Mainland / Homestead (Sku: 129) (Linden Owned)</item>
	/// <item>Mainland / Linden Homes (Sku: 131)</item>
	/// </list>
	/// </remarks>
	public String ProductName;
	/// <summary>The billing product SKU</summary>
	/// <remarks>Known values are:
	/// <list type="table">
	/// <item>023 Mainland / Full Region</item>
	/// <item>024 Estate / Full Region</item>
	/// <item>027 Estate / Openspace</item>
	/// <item>029 Estate / Homestead</item>
	/// <item>129 Mainland / Homestead (Linden Owned)</item>
	/// <item>131 Linden Homes / Full Region</item>
	/// </list>
	/// </remarks>
	public String ProductSku;

	/// <summary>The current sequence number for packets sent to this
	/// simulator. Must be Interlocked before modifying. Only
	/// useful for applications manipulating sequence numbers</summary>
	public AtomicLong Sequence = new AtomicLong(0);

	/// <summary>
	/// A thread-safe dictionary containing avatars in a simulator        
	/// </summary>
	//<uint, Avatar>
	public InternalDictionary<Long, Avatar> ObjectsAvatars = new InternalDictionary<Long, Avatar>();

	/// <summary>
	/// A thread-safe dictionary containing primitives in a simulator
	/// </summary>
	//uint
	public PrimitiveDictionary ObjectsPrimitives = new PrimitiveDictionary();

	public TerrainPatch[] Terrain;

	public  Vector2[] WindSpeeds;

	/// <summary>
	/// Provides access to an internal thread-safe dictionary containing parcel
	/// information found in this simulator
	/// </summary>
	public InternalDictionary<Integer, Parcel> Parcels = new InternalDictionary<Integer, Parcel>();

	/// <summary>
	/// Provides access to an internal thread-safe multidimensional array containing a x,y grid mapped
	/// to each 64x64 parcel's LocalID.
	/// </summary>
	public int[][] getParcelMap()
	{
		synchronized(this)
		{
			return _ParcelMap;
		}
	}

	public void setParcelMap(int[][] value)
	{
		synchronized (this)
		{
			_ParcelMap = value;
		}
	}

	/// <summary>
	/// Checks simulator parcel map to make sure it has downloaded all data successfully
	/// </summary>
	/// <returns>true if map is full (contains no 0's)</returns>
	public boolean IsParcelMapFull()
	{
		for (int y = 0; y < 64; y++)
		{
			for (int x = 0; x < 64; x++)
			{
				if (this._ParcelMap[y][x] == 0)
					return false;
			}
		}
		return true;
	}

	/// <summary>
	/// Is it safe to send agent updates to this sim
	/// AgentMovementComplete message received
	/// </summary>
	public boolean AgentMovementComplete;

	//endregion Public Members

	//region Properties

	/// <summary>The IP address and port of the server</summary>
	public InetSocketAddress getIPEndPoint() { return remoteEndPoint; } 
	/// <summary>Whether there is a working connection to the simulator or 
	/// not</summary>
	public boolean getConnected() { return connected; } 
	/// <summary>Coarse locations of avatars in this simulator</summary>
	public InternalDictionary<UUID, Vector3> getAvatarPositions() { return avatarPositions; } 
	/// <summary>AvatarPositions key representing TrackAgent target</summary>
	public UUID getPreyID() { return preyID; }
	/// <summary>Indicates if UDP connection to the sim is fully established</summary>
	public boolean getHandshakeComplete() { return handshakeComplete; } 

	//endregion Properties

	//region Internal/Private Members
	/// <summary>Used internally to track sim disconnections</summary>
	public boolean DisconnectCandidate = false;
	/// <summary>Event that is triggered when the simulator successfully
	/// establishes a connection</summary>


	AutoResetEvent ConnectedEvent = new AutoResetEvent(false);



	/// <summary>Whether this sim is currently connected or not. Hooked up
	/// to the property Connected</summary>
	public boolean connected;
	/// <summary>Coarse locations of avatars in this simulator</summary>
	public InternalDictionary<UUID, Vector3> avatarPositions = new InternalDictionary<UUID, Vector3>();
	/// <summary>AvatarPositions key representing TrackAgent target</summary>
	UUID preyID = UUID.Zero;
	/// <summary>Sequence numbers of packets we've received
	/// (for duplicate checking)</summary>
	IncomingPacketIDCollection PacketArchive;
	/// <summary>Packets we sent out that need ACKs from the simulator</summary>
	SortedMap<Long, NetworkManager.OutgoingPacket> NeedAck = new TreeMap<Long, NetworkManager.OutgoingPacket>();
	/// <summary>Sequence number for pause/resume</summary>
	AtomicInteger pauseSerial = new AtomicInteger(0);
	/// <summary>Indicates if UDP connection to the sim is fully established</summary>
	boolean handshakeComplete;

	private NetworkManager Network;
	private Queue<Long> InBytes, OutBytes;
	// ACKs that are queued up to be sent to the simulator
	//	private LocklessQueue<uint> PendingAcks = new LocklessQueue<uint>();
	private Queue<Long> PendingAcks = new ConcurrentLinkedQueue<Long>();

	private Timer AckTimer;
	private Timer PingTimer;
	private Timer StatsTimer;

	// simulator <> parcel LocalID Map
	private int[][] _ParcelMap = new int[64][64];
	//internal
	boolean DownloadingParcelMap = false;

	private AutoResetEvent GotUseCircuitCodeAck = new AutoResetEvent(false);


	//endregion Internal/Private Members



	/// <summary>
	/// 
	/// </summary>
	/// <param name="client">Reference to the GridClient object</param>
	/// <param name="address">IPEndPoint of the simulator</param>
	/// <param name="handle">handle of the simulator</param>
	public Simulator(GridClient client, InetSocketAddress address, BigInteger handle)
	{
		super(address);
		Client = client;            

		Handle = handle;
		Network = Client.network;
		PacketArchive = new IncomingPacketIDCollection(Settings.PACKET_ARCHIVE_SIZE);

		InBytes = new LinkedBlockingQueue<Long>();
		OutBytes = new LinkedBlockingQueue<Long>();

		//		InBytes = Collections.synchronizedList(new LinkedList<Long>(Client.settings.STATS_QUEUE_SIZE));
		//		OutBytes = Collections.synchronizedList(new ArrayList<Long>(Client.settings.STATS_QUEUE_SIZE));

		if (client.settings.STORE_LAND_PATCHES)
		{
			Terrain = new TerrainPatch[16 * 16];
			WindSpeeds = new Vector2[16 * 16];
		}
	}

	/// <summary>
	/// Called when this Simulator object is being destroyed
	/// </summary>
	public void Dispose()
	{
		Dispose(true);
	}

	public void Dispose(boolean disposing)
	{
		if (disposing)
		{
			if (AckTimer != null)
				AckTimer.cancel();
			if (PingTimer != null)
				PingTimer.cancel();
			if (StatsTimer != null)
				StatsTimer.cancel();
			//TODO Need to close ConnectedEvent gracefully
			//		                if (ConnectedEvent != null)
			//		                    ConnectedEvent.close();

			// Force all the CAPS connections closed for this simulator
			if (Caps != null)
				Caps.Disconnect(true);
		}
	}

	/// <summary>
	/// Attempt to connect to this simulator
	/// </summary>
	/// <param name="moveToSim">Whether to move our agent in to this sim or not</param>
	/// <returns>True if the connection succeeded or connection status is
	/// unknown, false if there was a failure</returns>
	public boolean Connect(boolean moveToSim)
	{
		handshakeComplete = false;

		if (connected)
		{
			UseCircuitCode(true);
			if (moveToSim) Client.self.CompleteAgentMovement(this);
			return true;
		}

		//region Start Timers

		// Timer for sending out queued packet acknowledgements
		//		if (AckTimer == null)
		//			AckTimer = new Timer(AckTimer_Elapsed, null, Settings.NETWORK_TICK_INTERVAL, Timeout.Infinite);

		scheduleAckTimer(Settings.NETWORK_TICK_INTERVAL);

		// Timer for recording simulator connection statistics
		//		if (StatsTimer == null)
		//			StatsTimer = new Timer(StatsTimer_Elapsed, null, 1000, 1000);
		if (StatsTimer == null)
		{
			StatsTimer = new Timer();
			StatsTimer.schedule(new TimerTask(){
				@Override
				public void run() {
					StatsTimer_Elapsed(null);
				}				
			}, 1000, 1000);
		}

		// Timer for periodically pinging the simulator
		//		if (PingTimer == null && Client.settings.SEND_PINGS)
		//			PingTimer = new Timer(PingTimer_Elapsed, null, Settings.PING_INTERVAL, Settings.PING_INTERVAL);

		if (PingTimer == null && Client.settings.SEND_PINGS)
		{
			PingTimer = new Timer();
			PingTimer.schedule(new TimerTask(){
				@Override
				public void run() {
					PingTimer_Elapsed(null);
				}	
			}, Settings.PING_INTERVAL, Settings.PING_INTERVAL);
		}


		//endregion Start Timers

		JLogger.info("Connecting to " + this.toString());

		try
		{
			// Create the UDP connection
			Start();

			// Mark ourselves as connected before firing everything else up
			connected = true;

			// Initiate connection
			UseCircuitCode(true);

			Stats.ConnectTime = Utils.getUnixTime();

			// Move our agent in to the sim to complete the connection
			if (moveToSim) Client.self.CompleteAgentMovement(this);

			if (!ConnectedEvent.waitOne(Client.settings.SIMULATOR_TIMEOUT))
			{
				JLogger.warn("Giving up on waiting for RegionHandshake for " + this.toString());
			}

			if (Client.settings.SEND_AGENT_THROTTLE)
				Client.throttle.Set(this);

			if (Client.settings.SEND_AGENT_UPDATES)
				Client.self.Movement.SendUpdate(true, this);

			return true;
		}
		catch (Exception e)
		{
			JLogger.error(Utils.getExceptionStackTraceAsString(e));
		}

		return false;
	}

	private void scheduleAckTimer(long delay)
	{
		if (AckTimer == null)
		{
			AckTimer = new Timer();
			AckTimer.schedule(new TimerTask()
			{
				@Override
				public void run() {
					AckTimer_Elapsed(null);

				}
			}, delay);
		}
	}


	private void reScheduleAckTimer(long delay)
	{
		if (AckTimer != null)
		{
			AckTimer.cancel();
			AckTimer = null;
		}
		scheduleAckTimer(delay);
	}



	/// <summary>
	/// Initiates connection to the simulator
	/// </summary>
	/// <param name="waitForAck">Should we block until ack for this packet is recieved</param>
	public void UseCircuitCode(boolean waitForAck)
	{
		// Send the UseCircuitCode packet to initiate the connection
		UseCircuitCodePacket use = new UseCircuitCodePacket();
		use.CircuitCode.Code = Network.getCircuitCode();
		use.CircuitCode.ID = Client.self.getAgentID();
		use.CircuitCode.SessionID = Client.self.getSessionID();
		JLogger.info("Got Circuit Code: " + Network.getCircuitCode());

		if (waitForAck)
		{
			GotUseCircuitCodeAck.reset();
		}

		// Send the initial packet out
		SendPacket(use);

		if (waitForAck)
		{
			try {
				JLogger.info("Going to wait to get ACK for UseCircuitCode packet: timeout: " + Client.settings.SIMULATOR_TIMEOUT);
				if (!GotUseCircuitCodeAck.waitOne(Client.settings.SIMULATOR_TIMEOUT))
				{
					JLogger.error("Failed to get ACK for UseCircuitCode packet");
				}
				else
					JLogger.info("Got ACK for UseCircuitCode packet");
			} 
			catch (InterruptedException e) {
				JLogger.error("Got Interrupted while geting ACK for UseCircuitCode packet");
			}
		}
	}

	public void SetSeedCaps(String seedcaps) throws Exception
	{
		if (Caps != null)
		{
			if (Caps._SeedCapsURI == seedcaps) return;

			JLogger.warn("Unexpected change of seed capability");
			Caps.Disconnect(true);
			Caps = null;
		}

		if (Client.settings.ENABLE_CAPS)
		{
			// Connect to the new CAPS system
			if (!Utils.isNullOrEmpty(seedcaps))
				Caps = new Caps(this, seedcaps);
			else
				JLogger.error("Setting up a sim without a valid capabilities server!");
		}

	}

	/// <summary>
	/// Disconnect from this simulator
	/// </summary>
	public void Disconnect(boolean sendCloseCircuit)
	{
		if (connected)
		{
			connected = false;

			// Destroy the timers
			if (AckTimer != null) AckTimer.cancel();
			if (StatsTimer != null) StatsTimer.cancel();
			if (PingTimer != null) PingTimer.cancel();

			AckTimer = null;
			StatsTimer = null;
			PingTimer = null;

			// Kill the current CAPS system
			if (Caps != null)
			{
				Caps.Disconnect(true);
				Caps = null;
			}

			if (sendCloseCircuit)
			{
				// Try to send the CloseCircuit notice
				CloseCircuitPacket close = new CloseCircuitPacket();

				UDPPacketBuffer buf = new UDPPacketBuffer(remoteEndPoint);
				byte[] data = close.ToBytes();
				Utils.arraycopy(data, 0, buf.getData(), 0, data.length);
				buf.setDataLength(data.length);

				AsyncBeginSend(buf);
			}

			// Shut the socket communication down
			Stop();
		}
	}

	/// <summary>
	/// Instructs the simulator to stop sending update (and possibly other) packets
	/// </summary>
	public void Pause()
	{
		AgentPausePacket pause = new AgentPausePacket();
		pause.AgentData.AgentID = Client.self.getAgentID();
		pause.AgentData.SessionID = Client.self.getSessionID();
		//			            pause.AgentData.SerialNum = (uint)Interlocked.Exchange(ref pauseSerial, pauseSerial + 1);
		pause.AgentData.SerialNum = pauseSerial.getAndIncrement();

		Client.network.SendPacket(pause, this);
	}

	/// <summary>
	/// Instructs the simulator to resume sending update packets (unpause)
	/// </summary>
	public void Resume()
	{
		AgentResumePacket resume = new AgentResumePacket();
		resume.AgentData.AgentID = Client.self.getAgentID();
		resume.AgentData.SessionID = Client.self.getSessionID();
		resume.AgentData.SerialNum = pauseSerial.getAndIncrement();

		Client.network.SendPacket(resume, this);
	}

	/// <summary>
	/// Retrieve the terrain height at a given coordinate
	/// </summary>
	/// <param name="x">Sim X coordinate, valid range is from 0 to 255</param>
	/// <param name="y">Sim Y coordinate, valid range is from 0 to 255</param>
	/// <param name="height">The terrain height at the given point if the
	/// lookup was successful, otherwise 0.0f</param>
	/// <returns>True if the lookup was successful, otherwise false</returns>
	public boolean TerrainHeightAtPoint(int x, int y, float[] height)
	{
		if (Terrain != null && x >= 0 && x < 256 && y >= 0 && y < 256)
		{
			int patchX = x / 16;
			int patchY = y / 16;
			x = x % 16;
			y = y % 16;

			TerrainPatch patch = Terrain[patchY * 16 + patchX];
			if (patch != null)
			{
				height[0] = patch.Data[y * 16 + x];
				return true;
			}
		}

		height[0] = 0.0f;
		return false;
	}

	//region Packet Sending

	/// <summary>
	/// Sends a packet
	/// </summary>
	/// <param name="packet">Packet to be sent</param>
	public void SendPacket(Packet packet)
	{
		// DEBUG: This can go away after we are sure nothing in the library is trying to do this
		if (packet.header.AppendedAcks || (packet.header.AckList != null && packet.header.AckList.length > 0))
			JLogger.error("Attempting to send packet " + packet.Type + " with ACKs appended before serialization");

		JLogger.debug("Attempting to send packet " + packet.Type );
		
		
		if (packet.HasVariableBlocks)
		{
			byte[][] datas;
			try { datas = packet.ToBytesMultiple(); }
			catch (NullPointerException e)
			{
				JLogger.warn("Failed to serialize " + packet.Type + " packet to one or more payloads due to a missing block or field. StackTrace: \n" + Utils.getExceptionStackTraceAsString(e));
				return;
			}
			int packetCount = datas.length;

			if (packetCount > 1)
				JLogger.debug("Split " + packet.Type + " packet into " + packetCount + " packets");

			JLogger.debug("Sending Multi Block Packets of count " +  packetCount);
			
			
			for (int i = 0; i < packetCount; i++)
			{
				byte[] data = datas[i];
				//JLogger.debug("Sending Multi Block Packet of length" +  data.length + "\n" + Utils.bytesToHexDebugString(data, ""));
				SendPacketData(data, data.length, packet.Type, packet.header.Zerocoded);
			}
		}
		else
		{
			byte[] data = packet.ToBytes();
			SendPacketData(data, data.length, packet.Type, packet.header.Zerocoded);
		}
	}

	public void SendPacketData(byte[] data, int dataLength, PacketType type, boolean doZerocode)
	{
		UDPPacketBuffer buffer = new UDPPacketBuffer(remoteEndPoint, Packet.MTU);

		// Zerocode if needed
		if (doZerocode)
		{
			try { dataLength = Helpers.ZeroEncode(data, dataLength, buffer.getData()); }
			catch (IndexOutOfBoundsException e)
			{
				// The packet grew larger than Packet.MTU bytes while zerocoding.
				// Remove the MSG_ZEROCODED flag and send the unencoded data
				// instead
				data[0] = (byte)(data[0] & ~Helpers.MSG_ZEROCODED);
				Utils.arraycopy(data, 0, buffer.getData(), 0, dataLength);
			}
		}
		else
		{
			Utils.arraycopy(data, 0, buffer.getData(), 0, dataLength);
		}
		buffer.setDataLength(dataLength);

		//region Queue or Send

		NetworkManager.OutgoingPacket outgoingPacket = new NetworkManager.OutgoingPacket(this, buffer, type);

		// Send ACK and logout packets directly, everything else goes through the queue
		if (Client.settings.THROTTLE_OUTGOING_PACKETS == false ||
				type == PacketType.PacketAck ||
				type == PacketType.LogoutRequest)
		{
			SendPacketFinal(outgoingPacket);
		}
		else
		{
			Network.PacketOutbox.offer(outgoingPacket);
		}

		//endregion Queue or Send

		//region Stats Tracking
		if (Client.settings.TRACK_UTILIZATION)
		{
			Client.stats.Update(type.toString(), UtilizationStatistics.Type.Packet, dataLength, 0);
		}
		//endregion
	}

	protected void SendPacketFinal(NetworkManager.OutgoingPacket outgoingPacket)
	{
		UDPPacketBuffer buffer = outgoingPacket.Buffer;
		byte flags = buffer.getData()[0];
		boolean isResend = (flags & Helpers.MSG_RESENT) != 0;
		boolean isReliable = (flags & Helpers.MSG_RELIABLE) != 0;

		// Keep track of when this packet was sent out (right now)
		outgoingPacket.TickCount = Utils.getUnixTime();

		//region ACK Appending

		int dataLength = buffer.getDataLength();

		// Keep appending ACKs until there is no room left in the packet or there are
		// no more ACKs to append
		//uint
		long ackCount = 0;
		//uint
		Long ack;
		while (dataLength + 5 < Packet.MTU && ((ack = PendingAcks.poll())!=null))
		{
			Utils.uintToBytes(ack, buffer.getData(), dataLength);
			dataLength += 4;
			++ackCount;
		}

		if (ackCount > 0)
		{
			// Set the last byte of the packet equal to the number of appended ACKs
			buffer.getData()[dataLength++] = (byte)ackCount;
			// Set the appended ACKs flag on this packet
			buffer.getData()[0] |= Helpers.MSG_APPENDED_ACKS;
		}

		buffer.setDataLength(dataLength);

		//endregion ACK Appending

		if (!isResend)
		{
			// Not a resend, assign a new sequence number
			//uint
			long sequenceNumber = Sequence.incrementAndGet();
			Utils.uintToBytes(sequenceNumber, buffer.getData(), 1);
			outgoingPacket.SequenceNumber = sequenceNumber;

			if (isReliable)
			{
				// Add this packet to the list of ACK responses we are waiting on from the server
				synchronized (NeedAck) 
				{NeedAck.put(sequenceNumber, outgoingPacket);}
			}
		}

		// Put the UDP payload on the wire
		AsyncBeginSend(buffer);
	}

	/// <summary>
	/// 
	/// </summary>
	public void SendPing()
	{
		//uint
		long oldestUnacked = 0;

		// Get the oldest NeedAck value, the first entry in the sorted dictionary
		synchronized (NeedAck)
		{
			if (NeedAck.size() > 0)
			{
				//<uint, NetworkManager.OutgoingPacket>
				//		                    SortedMap<Long, NetworkManager.OutgoingPacket>.KeyCollection.Enumerator en = NeedAck.firstKey();
				//		                    en.MoveNext();
				//TODO verify the logic
				oldestUnacked = NeedAck.firstKey();
				NeedAck.remove(oldestUnacked);
			}
		}

		//if (oldestUnacked != 0)
		//    Logger.DebugLog("Sending ping with oldestUnacked=" + oldestUnacked);

		StartPingCheckPacket ping = new StartPingCheckPacket();
		ping.PingID.PingID = Stats.LastPingID++;
		ping.PingID.OldestUnacked = oldestUnacked;
		ping.header.Reliable = false;
		SendPacket(ping);
		Stats.LastPingSent = Utils.getUnixTime();
	}

	//endregion Packet Sending

	/// <summary>
	/// Returns Simulator Name as a String
	/// </summary>
	/// <returns></returns>
	@Override
	public String toString()
	{
		if (!Utils.isNullOrEmpty(Name))
			return String.format("%s (%s)", Name, remoteEndPoint.toString());
		else
			return String.format("(%s)", remoteEndPoint.toString());
	}

	/// <summary>
	/// 
	/// </summary>
	/// <returns></returns>
	@Override
	public int hashCode()
	{
		return Handle.hashCode();
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="obj"></param>
	/// <returns></returns>
	@Override
	public boolean equals(Object obj)
	{
		if (obj == null || !(obj instanceof Simulator))
			return false;

		Simulator sim = (Simulator)obj ;		            
		return (remoteEndPoint.equals(sim.remoteEndPoint));
	}

	@Override
	protected void PacketReceived(UDPPacketBuffer buffer)
	{
		Packet packet = null;

		// Check if this packet came from the server we expected it to come from
		if (!remoteEndPoint.getAddress().equals(((InetSocketAddress)buffer.getRemoteEndPoint()).getAddress()))
		{
			JLogger.warn("Received " + buffer.getDataLength() + " bytes of data from unrecognized source " +
					((InetSocketAddress)buffer.getRemoteEndPoint()).toString());
			return;
		}

		// Update the disconnect flag so this sim doesn't time out
		DisconnectCandidate = false;

		//region Packet Decoding

		int[] packetEnd = new int[]{buffer.getDataLength() - 1};

		try
		{
			packet = Packet.BuildPacket(buffer.getData(), packetEnd,
					// Only allocate a buffer for zerodecoding if the packet is zerocoded
					((buffer.getData()[0] & Helpers.MSG_ZEROCODED) != 0) ? new byte[8192] : null);
		}
		catch (MalformedDataException e)
		{
			JLogger.error(String.format("Malformed data, cannot parse packet:\n%s\nException: %s",
					Utils.bytesToHexDebugString(buffer.getData(), buffer.getDataLength(), null),
					Utils.getExceptionStackTraceAsString(e)));
		}

		// Fail-safe check
		if (packet == null)
		{
			JLogger.warn("Couldn't build a message from the incoming data");
			return;
		}

//		Interlocked.Add(ref Stats.RecvBytes, buffer.DataLength);
//		Interlocked.Increment(ref Stats.RecvPackets);
		Stats.RecvBytes.addAndGet(buffer.getDataLength());
		Stats.RecvPackets.incrementAndGet();

		//endregion Packet Decoding

		if (packet.header.Resent)
//			Interlocked.Increment(ref Stats.ReceivedResends);
		Stats.ReceivedResends.incrementAndGet();
		//region ACK Receiving

		// Handle appended ACKs
		if (packet.header.AppendedAcks && packet.header.AckList != null)
		{
			synchronized (NeedAck)
			{
				for (int i = 0; i < packet.header.AckList.length; i++)
				{
					if (NeedAck.containsKey(packet.header.AckList[i]) 
							&& NeedAck.get(packet.header.AckList[i]).Type.equals(PacketType.UseCircuitCode))
					{
						GotUseCircuitCodeAck.set();
					}
					NeedAck.remove(packet.header.AckList[i]);
				}
			}
		}

		// Handle PacketAck packets
		if (packet.Type == PacketType.PacketAck)
		{
			PacketAckPacket ackPacket = (PacketAckPacket)packet;

			synchronized (NeedAck)
			{
				for (int i = 0; i < ackPacket.Packets.length; i++)
				{
					if (NeedAck.containsKey(ackPacket.Packets[i].ID) 
							&& NeedAck.get(ackPacket.Packets[i].ID).Type.equals(PacketType.UseCircuitCode))
					{
						GotUseCircuitCodeAck.set();
					}
					NeedAck.remove(ackPacket.Packets[i].ID);
				}
			}
		}

		//endregion ACK Receiving

		if (packet.header.Reliable)
		{
			//region ACK Sending

			// Add this packet to the list of ACKs that need to be sent out
			//uint
			long sequence = (long)packet.header.Sequence;
			PendingAcks.offer(sequence);

			// Send out ACKs if we have a lot of them
			if (PendingAcks.size() >= Client.settings.MAX_PENDING_ACKS)
				SendAcks();

			//endregion ACK Sending

			// Check the archive of received packet IDs to see whether we already received this packet
			if (!PacketArchive.offer(packet.header.Sequence))
			{
				if (packet.header.Resent)
					JLogger.debug("Received a resend of already processed packet #" + packet.header.Sequence + ", type: " + packet.Type);
				else
					JLogger.warn("Received a duplicate (not marked as resend) of packet #" + packet.header.Sequence + ", type: " + packet.Type);

				// Avoid firing a callback twice for the same packet
				return;
			}
		}

		//region Inbox Insertion

		NetworkManager.IncomingPacket incomingPacket = new NetworkManager.IncomingPacket();
		incomingPacket.simulator = this;
		incomingPacket.packet = packet;

		Network.PacketInbox.offer(incomingPacket);

		//endregion Inbox Insertion

		//region Stats Tracking
		if (Client.settings.TRACK_UTILIZATION)
		{
			Client.stats.Update(packet.Type.toString(), 
					UtilizationStatistics.Type.Packet, 0, packet.getLength());
		}
		//endregion
	}

	@Override
	protected  void PacketSent(UDPPacketBuffer buffer, int bytesSent)
	{
		// Stats tracking
//		Interlocked.Add(ref Stats.SentBytes, bytesSent);
//		Interlocked.Increment(ref Stats.SentPackets);
		Stats.SentBytes.addAndGet(bytesSent);
		Stats.SentPackets.incrementAndGet();
		Client.network.RaisePacketSentEvent(buffer.getData(), bytesSent, this);
	}


	/// <summary>
	/// Sends out pending acknowledgements
	/// </summary>
	/// <returns>Number of ACKs sent</returns>
	private int SendAcks()
	{
		//uint
		int ackCount = 0;
		Iterator<Long> itr = PendingAcks.iterator();
		if (itr.hasNext())
		{
			long ack = itr.next();

			List<PacketAckPacket.PacketsBlock> blocks = new ArrayList<PacketAckPacket.PacketsBlock>();
			PacketAckPacket.PacketsBlock block = new PacketAckPacket.PacketsBlock();
			block.ID = ack;
			blocks.add(block);

			while (itr.hasNext())
			{
				ack = itr.next();
				block = new PacketAckPacket.PacketsBlock();
				block.ID = ack;
				blocks.add(block);
			}

			PacketAckPacket packet = new PacketAckPacket();
			packet.header.Reliable = false;
			packet.Packets = blocks.toArray(new PacketAckPacket.PacketsBlock[0]);

			ackCount = blocks.size();
			SendPacket(packet);
		}

		return ackCount;
	}

	/// <summary>
	/// Resend unacknowledged packets
	/// </summary>
	private void ResendUnacked()
	{
		if (NeedAck.size() > 0)
		{
			NetworkManager.OutgoingPacket[] array;

			synchronized (NeedAck)
			{
				// Create a temporary copy of the outgoing packets array to iterate over
				array = new NetworkManager.OutgoingPacket[NeedAck.size()];
				array = NeedAck.values().toArray(new NetworkManager.OutgoingPacket[0]);
				//		                    NeedAck.Values.CopyTo(array, 0);
			}

			//			int now = Environment.TickCount;
			long now = Utils.getUnixTime();

			// Resend packets
			for (int i = 0; i < array.length; i++)
			{
				NetworkManager.OutgoingPacket outgoing = array[i];

				if (outgoing.TickCount != 0 && now - outgoing.TickCount > Client.settings.RESEND_TIMEOUT)
				{
					if (outgoing.ResendCount.get() < Client.settings.MAX_RESEND_COUNT)
					{
						if (Client.settings.LOG_RESENDS)
						{
							JLogger.debug(String.format("Resending packet %s, %sms have passed",
									outgoing.SequenceNumber, now - outgoing.TickCount));
						}

						// The TickCount will be set to the current time when the packet
						// is actually sent out again
						outgoing.TickCount = 0;

						// Set the resent flag
						outgoing.Buffer.getData()[0] = (byte)(outgoing.Buffer.getData()[0] | Helpers.MSG_RESENT);


						//						Interlocked.Increment(ref outgoing.ResendCount);
						//						Interlocked.Increment(ref Stats.ResentPackets);

						// Stats tracking
						outgoing.ResendCount.incrementAndGet();
						Stats.ResentPackets.incrementAndGet();

						SendPacketFinal(outgoing);
					}
					else
					{
						JLogger.debug(String.format("Dropping packet %s after %s failed attempts",
								outgoing.SequenceNumber, outgoing.ResendCount));

						synchronized (NeedAck) 
						{NeedAck.remove(outgoing.SequenceNumber);}
					}
				}
			}
		}
	}

	private void AckTimer_Elapsed(Object obj)
	{
		try
		{
		SendAcks();
		ResendUnacked();
		}
		catch(Exception e) { JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

		// Start the ACK handling functions again after NETWORK_TICK_INTERVAL milliseconds
		if (null == AckTimer) return;
		try { reScheduleAckTimer(Settings.NETWORK_TICK_INTERVAL); }
		catch (Exception e) { JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
	}

	private void StatsTimer_Elapsed(Object obj)
	{
		long old_in = 0, old_out = 0;
		long recv = Stats.RecvBytes.longValue();
		long sent = Stats.SentBytes.longValue();

		if (InBytes.size() >= Client.settings.STATS_QUEUE_SIZE)
			old_in = InBytes.poll();
		if (OutBytes.size() >= Client.settings.STATS_QUEUE_SIZE)
			old_out = OutBytes.poll();

		InBytes.offer(recv);
		OutBytes.offer(sent);

		if (old_in > 0 && old_out > 0)
		{
			Stats.IncomingBPS = (int)(recv - old_in) / Client.settings.STATS_QUEUE_SIZE;
			Stats.OutgoingBPS = (int)(sent - old_out) / Client.settings.STATS_QUEUE_SIZE;
			//Client.Log("Incoming: " + IncomingBPS + " Out: " + OutgoingBPS +
			//    " Lag: " + LastLag + " Pings: " + ReceivedPongs +
			//    "/" + SentPings, Helpers.LogLevel.Debug); 
		}
	}

	private void PingTimer_Elapsed(Object obj)
	{
		SendPing();
		Stats.SentPings.incrementAndGet();
	}

	public final static class IncomingPacketIDCollection
	{
		//uint[]
		private long[] Items;
		//Set<uint>
		Set<Long> hashSet;
		int first;
		int next;
		int capacity;

		public IncomingPacketIDCollection(int capacity)
		{
			this.capacity = capacity;
			Items = new long[capacity];
			hashSet = new HashSet<Long>();
		}

		public boolean offer(long ack)
		{
			synchronized (hashSet)
			{
				if (hashSet.add(ack))
				{
					Items[next] = ack;
					next = (next + 1) % capacity;
					if (next == first)
					{
						hashSet.remove(Items[first]);
						first = (first + 1) % capacity;
					}

					return true;
				}
			}

			return false;
		}
	}
}

