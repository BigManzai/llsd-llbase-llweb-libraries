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

import java.net.Inet4Address;
import java.net.InetAddress;
import java.util.Observable;

import com.ngt.jopenmetaverse.shared.protocol.EconomyDataPacket;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

	/// <summary>
    /// Class for controlling various system settings.
    /// </summary>
    /// <remarks>Some values are readonly because they affect things that
    /// happen when the GridClient object is initialized, so changing them at 
    /// runtime won't do any good. Non-readonly values may affect things that 
    /// happen at login or dynamically</remarks>
    public class Settings 
    {
        //region Login/Networking Settings

        /// <summary>Main grid login server</summary>
        public final static String AGNI_LOGIN_SERVER = "https://login.agni.lindenlab.com/cgi-bin/login.cgi";

        /// <summary>Beta grid login server</summary>
        public final String ADITI_LOGIN_SERVER = "https://login.aditi.lindenlab.com/cgi-bin/login.cgi";

        //Local Development Server
        public final String LOCAL_LOGIN_SERVER = "http://127.0.0.1:9000/";
        
        /// <summary>The relative directory where external resources are kept</summary>
        public static String RESOURCE_DIR = "openmetaverse_data";

        /// <summary>IP Address the client will bind to</summary>
        public static InetAddress BIND_ADDR;

        static {
        	try {
        		BIND_ADDR = Inet4Address.getByName("0.0.0.0");
        	}
        	catch(Exception e)
        	{
        		JLogger.error(Utils.getExceptionStackTraceAsString(e));
        		BIND_ADDR = null;
        	}
        }
        
        public static int UDP_BIND_PORT = 0;
        
        /// <summary>Login server to connect to</summary>
//        public String LOGIN_SERVER = AGNI_LOGIN_SERVER;
        public String LOGIN_SERVER = LOCAL_LOGIN_SERVER;
        
        /// <summary>Use XML-RPC Login or LLSD Login, default is XML-RPC Login</summary>
        //TODO Need to change following to false as default
        public boolean USE_LLSD_LOGIN = true;
        //endregion
        //region Inventory
        /// <summary>
        /// InventoryManager requests inventory information on login,
        /// GridClient initializes an Inventory store for main inventory.
        /// </summary>
        public final boolean ENABLE_INVENTORY_STORE = true;
        /// <summary>
        /// InventoryManager requests library information on login,
        /// GridClient initializes an Inventory store for the library.
        /// </summary>
        public final boolean ENABLE_LIBRARY_STORE = true;
        //endregion
        //region Timeouts and Intervals

        /// <summary>Number of milliseconds before an asset transfer will time
        /// out</summary>
        public int TRANSFER_TIMEOUT = 90 * 1000;

        /// <summary>Number of milliseconds before a teleport attempt will time
        /// out</summary>
        public int TELEPORT_TIMEOUT = 40 * 1000;

        /// <summary>Number of milliseconds before NetworkManager.Logout() will
        /// time out</summary>
        public int LOGOUT_TIMEOUT = 5 * 1000;

        /// <summary>Number of milliseconds before a CAPS call will time out</summary>
        /// <remarks>Setting this too low will cause web requests time out and
        /// possibly retry repeatedly</remarks>
        public int CAPS_TIMEOUT = 60 * 1000;

        /// <summary>Number of milliseconds for xml-rpc to timeout</summary>
        public int LOGIN_TIMEOUT = 60 * 1000;

        /// <summary>Milliseconds before a packet is assumed lost and resent</summary>
        public int RESEND_TIMEOUT = 4000;

        /// <summary>Milliseconds without receiving a packet before the 
        /// connection to a simulator is assumed lost</summary>
        public int SIMULATOR_TIMEOUT = 30 * 1000;

        /// <summary>Milliseconds to wait for a simulator info request through
        /// the grid interface</summary>
        public int MAP_REQUEST_TIMEOUT = 5 * 1000;

        /// <summary>Number of milliseconds between sending pings to each sim</summary>
        public final static int PING_INTERVAL = 2200;

        /// <summary>Number of milliseconds between sending camera updates</summary>
        public final static int DEFAULT_AGENT_UPDATE_INTERVAL = 500;

        /// <summary>Number of milliseconds between updating the current
        /// positions of moving, non-accelerating and non-colliding objects</summary>
        public final static int INTERPOLATION_INTERVAL = 250;

        /// <summary>Millisecond interval between ticks, where all ACKs are 
        /// sent out and the age of unACKed packets is checked</summary>
        public final static int NETWORK_TICK_INTERVAL = 500;

        //endregion
        //region Sizes

        /// <summary>The initial size of the packet inbox, where packets are
        /// stored before processing</summary>
        public final static int PACKET_INBOX_SIZE = 100;
        /// <summary>Maximum size of packet that we want to send over the wire</summary>
        public final static int MAX_PACKET_SIZE = 1200;
        /// <summary>The maximum value of a packet sequence number before it
        /// rolls over back to one</summary>
        public final static int MAX_SEQUENCE = 0xFFFFFF;
        /// <summary>The maximum size of the sequence number archive, used to
        /// check for resent and/or duplicate packets</summary>
        public final static int PACKET_ARCHIVE_SIZE = 200;
        /// <summary>Maximum number of queued ACKs to be sent before SendAcks()
        /// is forced</summary>
        public int MAX_PENDING_ACKS = 10;
        /// <summary>Network stats queue length (seconds)</summary>
        public int STATS_QUEUE_SIZE = 5;

        //endregion
        //region Configuration options (mostly booleaneans)

        /// <summary>Enable/disable storing terrain heightmaps in the 
        /// TerrainManager</summary>
        public  boolean STORE_LAND_PATCHES = false;

        /// <summary>Enable/disable sending periodic camera updates</summary>
        public  boolean SEND_AGENT_UPDATES = true;

        /// <summary>Enable/disable automatically setting agent appearance at
        /// login and after sim crossing</summary>
        public boolean SEND_AGENT_APPEARANCE = true;

        /// <summary>Enable/disable automatically setting the bandwidth throttle
        /// after connecting to each simulator</summary>
        /// <remarks>The default throttle uses the equivalent of the maximum
        /// bandwidth setting in the official client. If you do not set a
        /// throttle your connection will by default be throttled well below
        /// the minimum values and you may experience connection problems</remarks>
        public boolean SEND_AGENT_THROTTLE = true;

        /// <summary>Enable/disable the sending of pings to monitor lag and 
        /// packet loss</summary>
        public boolean SEND_PINGS = true;

        /// <summary>Should we connect to multiple sims? This will allow
        /// viewing in to neighboring simulators and sim crossings
        /// (Experimental)</summary>
        public boolean MULTIPLE_SIMS = true;

        /// <summary>If true, all object update packets will be decoded in to
        /// native objects. If false, only updates for our own agent will be
        /// decoded. Registering an event handler will force objects for that
        /// type to always be decoded. If this is disabled the object tracking
        /// will have missing or partial prim and avatar information</summary>
        public  boolean ALWAYS_DECODE_OBJECTS = true;

        /// <summary>If true, when a cached object check is received from the
        /// server the full object info will automatically be requested</summary>
        public  boolean ALWAYS_REQUEST_OBJECTS = true;

        /// <summary>Whether to establish connections to HTTP capabilities
        /// servers for simulators</summary>
        public boolean ENABLE_CAPS = true;

        /// <summary>Whether to decode sim stats</summary>
        public boolean ENABLE_SIMSTATS = true;

        /// <summary>The capabilities servers are currently designed to
        /// periodically return a 502 error which signals for the client to
        /// re-establish a connection. Set this to true to log those 502 errors</summary>
        public boolean LOG_ALL_CAPS_ERRORS = false;

        /// <summary>If true, any reference received for a folder or item
        /// the library is not aware of will automatically be fetched</summary>
        public boolean FETCH_MISSING_INVENTORY = true;

        /// <summary>If true, and <code>SEND_AGENT_UPDATES</code> is true,
        /// AgentUpdate packets will continuously be sent out to give the bot
        /// smoother movement and autopiloting</summary>
        public boolean DISABLE_AGENT_UPDATE_DUPLICATE_CHECK = true;

        /// <summary>If true, currently visible avatars will be stored
        /// in dictionaries inside <code>Simulator.ObjectAvatars</code>.
        /// If false, a new Avatar or Primitive object will be created
        /// each time an object update packet is received</summary>
        public boolean AVATAR_TRACKING = true;

        /// <summary>If true, currently visible avatars will be stored
        /// in dictionaries inside <code>Simulator.ObjectPrimitives</code>.
        /// If false, a new Avatar or Primitive object will be created
        /// each time an object update packet is received</summary>
        public boolean OBJECT_TRACKING = true;

        /// <summary>If true, position and velocity will periodically be
        /// interpolated (extrapolated, technically) for objects and 
        /// avatars that are being tracked by the library. This is
        /// necessary to increase the accuracy of speed and position
        /// estimates for simulated objects</summary>
        public boolean USE_INTERPOLATION_TIMER = true;

        /// <summary>
        /// If true, utilization statistics will be tracked. There is a minor penalty
        /// in CPU time for enabling this option.
        /// </summary>
        public boolean TRACK_UTILIZATION = false;
        //endregion
        //region Parcel Tracking

        /// <summary>If true, parcel details will be stored in the 
        /// <code>Simulator.Parcels</code> dictionary as they are received</summary>
        public boolean PARCEL_TRACKING = true;

        /// <summary>
        /// If true, an incoming parcel properties reply will automatically send
        /// a request for the parcel access list
        /// </summary>
        public boolean ALWAYS_REQUEST_PARCEL_ACL = true;

        /// <summary>
        /// if true, an incoming parcel properties reply will automatically send 
        /// a request for the traffic count.
        /// </summary>
        public boolean ALWAYS_REQUEST_PARCEL_DWELL = true;

        //endregion
        //region Asset Cache

        /// <summary>
        /// If true, images, and other assets downloaded from the server 
        /// will be cached in a local directory
        /// </summary>
        public  boolean USE_ASSET_CACHE = true;

        /// <summary>Path to store cached texture data</summary>
        public String ASSET_CACHE_DIR = RESOURCE_DIR + "/cache";
        
        /// <summary>Maximum size cached files are allowed to take on disk (bytes)</summary>
        public long ASSET_CACHE_MAX_SIZE = 1024 * 1024 * 1024; // 1GB

        //endregion
        //region Misc

        /// <summary>Default color used for viewer particle effects</summary>
        public Color4 DEFAULT_EFFECT_COLOR = new Color4(1, 0, 0, 1);

        /// <summary>Cost of uploading an asset</summary>
        /// <remarks>Read-only since this value is dynamically fetched at login</remarks>
        public int UPLOAD_COST() 
        { 
        	return priceUpload; 
        }

        /// <summary>Maximum number of times to resend a failed packet</summary>
        public int MAX_RESEND_COUNT = 3;

        /// <summary>Throttle outgoing packet rate</summary>
        public boolean THROTTLE_OUTGOING_PACKETS = true;

        /// <summary>UUID of a texture used by some viewers to indentify type of client used</summary>
        public UUID CLIENT_IDENTIFICATION_TAG = UUID.Zero;

        //endregion
        //region Texture Pipeline

        /// <summary>
        /// Download textures using GetTexture capability when available
        /// </summary>
        public boolean USE_HTTP_TEXTURES = true;

        /// <summary>The maximum number of concurrent texture downloads allowed</summary>
        /// <remarks>Increasing this number will not necessarily increase texture retrieval times due to
        /// simulator throttles</remarks>
        public int MAX_CONCURRENT_TEXTURE_DOWNLOADS = 4;

        /// <summary>
        /// The Refresh timer inteval is used to set the delay between checks for stalled texture downloads
        /// </summary>
        /// <remarks>This is a static variable which applies to all instances</remarks>
        public static float PIPELINE_REFRESH_INTERVAL = 500.0f;

        /// <summary>
        /// Textures taking longer than this value will be flagged as timed out and removed from the pipeline
        /// </summary>
        public static int PIPELINE_REQUEST_TIMEOUT = 45*1000;
        //endregion

        //region Logging Configuration

        /// <summary>
        /// Get or set the minimum log level to output to the console by default
        /// 
        /// If the library is not compiled with DEBUG defined and this level is set to DEBUG
        /// You will get no output on the console. This behavior can be overriden by creating
        /// a logger configuration file for log4net
        /// </summary>
        public static Helpers.LogLevel LOG_LEVEL = Helpers.LogLevel.Debug;

        /// <summary>Attach avatar names to log messages</summary>
        public boolean LOG_NAMES = true;

        /// <summary>Log packet retransmission info</summary>
        public static boolean LOG_RESENDS = true;

        //endregion
        //region Private Fields

        private GridClient client;
        private int priceUpload = 0;

        /// <summary>Constructor</summary>
        /// <param name="client">Reference to a GridClient object</param>
        public Settings(GridClient client)
        {
            this.client = client;
//            this.client.network.RegisterCallback(PacketType.EconomyData, EconomyDataHandler);            
            this.client.network.RegisterCallback(PacketType.EconomyData, new EventObserver<PacketReceivedEventArgs>(){
    			@Override
    			public void handleEvent(Observable o, PacketReceivedEventArgs arg) {
    				EconomyDataHandler(o, arg);
    			}
              });
            
        }

        //endregion
        //region Packet Callbacks

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void EconomyDataHandler(Object sender, PacketReceivedEventArgs e)
        {
            EconomyDataPacket econ = (EconomyDataPacket)e.getPacket();
            priceUpload = econ.info.PriceUpload;
        }
        //endregion
    }
