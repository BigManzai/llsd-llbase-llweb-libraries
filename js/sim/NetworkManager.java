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

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;
import java.util.Timer;
import java.util.TimerTask;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;

import org.apache.xmlrpc.client.XmlRpcClient;
import org.apache.xmlrpc.client.XmlRpcClientConfigImpl;

import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestProgressArg;
import com.ngt.jopenmetaverse.shared.protocol.CompletePingCheckPacket;
import com.ngt.jopenmetaverse.shared.protocol.EconomyDataRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.KickUserPacket;
import com.ngt.jopenmetaverse.shared.protocol.LogoutReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.LogoutRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.RegionHandshakePacket;
import com.ngt.jopenmetaverse.shared.protocol.RegionHandshakeReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.SimStatsPacket;
import com.ngt.jopenmetaverse.shared.protocol.StartPingCheckPacket;
import com.ngt.jopenmetaverse.shared.sim.Simulator.RegionFlags;
import com.ngt.jopenmetaverse.shared.sim.Simulator.SimAccess;
import com.ngt.jopenmetaverse.shared.sim.buffers.UDPPacketBuffer;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventDictionary;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventObservableArg;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketEventDictionary;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.nm.DisconnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.EventQueueRunningEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.LoggedOutEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.PacketSentEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.SimChangedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.SimConnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.SimConnectingEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.SimDisconnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.login.LoginParams;
import com.ngt.jopenmetaverse.shared.sim.login.LoginProgressEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseCallbackArg;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseData;
import com.ngt.jopenmetaverse.shared.sim.login.LoginStatus;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.EnableSimulatorMessage;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.PlatformUtils;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class NetworkManager {

	Logger logger = Logger.getLogger(getClass().toString());
	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();

	private GridClient client;
	private Timer DisconnectTimer;
	private long circuitCode;
	private Simulator _CurrentSim = null;
	private boolean connected = false;

	/// <summary>All of the simulators we are currently connected to</summary>
	public List<Simulator> Simulators = new ArrayList<Simulator>();

	/// <summary>Handlers for incoming capability events</summary>
	protected CapsEventDictionary CapsEvents;
	/// <summary>Handlers for incoming packets</summary>
	protected PacketEventDictionary PacketEvents;
	/// <summary>Incoming packets that are awaiting handling</summary>
	public BlockingQueue<IncomingPacket> PacketInbox = new ArrayBlockingQueue<IncomingPacket>(Settings.PACKET_INBOX_SIZE);
	/// <summary>Outgoing packets that are awaiting handling</summary>
	protected BlockingQueue<OutgoingPacket> PacketOutbox = new ArrayBlockingQueue<OutgoingPacket>(Settings.PACKET_INBOX_SIZE);

	public NetworkManager(GridClient client)
	{
		this.client = client;
		PacketEvents = new PacketEventDictionary(client);
		CapsEvents = new CapsEventDictionary(client);

		// Register internal CAPS callbacks
		RegisterEventCallback("EnableSimulator", new EventObserver<CapsEventObservableArg>()
				{
			@Override
			public void handleEvent(Observable o,
					CapsEventObservableArg arg) {
				try {
					EnableSimulatorHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());
				} catch (Exception e) {
					JLogger.error("Error in EnableSimulatorHandler: " + Utils.getExceptionStackTraceAsString(e));
				}
			}
				});

		// Register the internal callbacks
		//		RegisterCallback(PacketType.RegionHandshake, RegionHandshakeHandler);
		RegisterCallback(PacketType.RegionHandshake, new EventObserver<PacketReceivedEventArgs>(){
			@Override
			public void handleEvent(Observable arg0, PacketReceivedEventArgs arg1) {
				try {
					RegionHandshakeHandler(arg0, (PacketReceivedEventArgs)arg1);
				} catch (UnsupportedEncodingException e) {
					JLogger.error("Error in RegionHandshakeHandler: " + Utils.getExceptionStackTraceAsString(e));
				}
			}});

		//		RegisterCallback(PacketType.StartPingCheck, StartPingCheckHandler, false);
		RegisterCallback(PacketType.StartPingCheck, new EventObserver<PacketReceivedEventArgs>(){
			@Override
			public void handleEvent(Observable arg0, PacketReceivedEventArgs arg1) {
				StartPingCheckHandler(arg0, (PacketReceivedEventArgs)arg1);
			}}, false);

		//		RegisterCallback(PacketType.DisableSimulator, DisableSimulatorHandler);
		RegisterCallback(PacketType.DisableSimulator, new EventObserver<PacketReceivedEventArgs>(){
			@Override
			public void handleEvent(Observable arg0, PacketReceivedEventArgs arg1) {
				DisableSimulatorHandler(arg0, (PacketReceivedEventArgs)arg1);
			}});

		//		RegisterCallback(PacketType.KickUser, KickUserHandler);
		RegisterCallback(PacketType.KickUser, new EventObserver<PacketReceivedEventArgs>(){
			@Override
			public void handleEvent(Observable arg0, PacketReceivedEventArgs arg1) {
				try {
					KickUserHandler(arg0, (PacketReceivedEventArgs)arg1);
				} catch (UnsupportedEncodingException e) {
					JLogger.error("Error in KickUserHandler: " + Utils.getExceptionStackTraceAsString(e));
				}
			}});

		//		RegisterCallback(PacketType.LogoutReply, LogoutReplyHandler);
		RegisterCallback(PacketType.LogoutReply, new EventObserver<PacketReceivedEventArgs>(){
			@Override
			public void handleEvent(Observable arg0, PacketReceivedEventArgs arg1) {
				LogoutReplyHandler(arg0, (PacketReceivedEventArgs)arg1);
			}});

		//		RegisterCallback(PacketType.CompletePingCheck, CompletePingCheckHandler, false);
		RegisterCallback(PacketType.CompletePingCheck, new EventObserver<PacketReceivedEventArgs>(){
			@Override
			public void handleEvent(Observable arg0, PacketReceivedEventArgs arg1) {
				CompletePingCheckHandler(arg0, (PacketReceivedEventArgs)arg1);
			}}, false);

		//		RegisterCallback(PacketType.SimStats, SimStatsHandler, false);
		RegisterCallback(PacketType.SimStats, new EventObserver<PacketReceivedEventArgs>(){
			@Override
			public void handleEvent(Observable arg0, PacketReceivedEventArgs arg1) {
				SimStatsHandler(arg0, (PacketReceivedEventArgs)arg1);
			}}, false);

		//TODO Need to handle following
		// GLOBAL SETTING: Don't force Expect-100: Continue headers on HTTP POST calls
		//		ServicePointManager.Expect100Continue = false;

	}

	public long getCircuitCode() {
		return circuitCode;
	}
	public void setCircuitCode(long circuitCode) {
		this.circuitCode = circuitCode;
	}

	/// <summary>The simulator that the logged in avatar is currently 
	/// occupying</summary>
	public Simulator getCurrentSim()
	{
		return _CurrentSim; 
	}

	public void setCurrentSim(Simulator value)
	{
		_CurrentSim = value;
	}

	public CapsEventDictionary getCapsEvents() {
		return CapsEvents;
	}

	public void setCapsEvents(CapsEventDictionary capsEvents) {
		CapsEvents = capsEvents;
	}

	/// <summary>Shows whether the network layer is logged in to the
	/// grid or not</summary>
	public boolean getConnected() { return connected; } 
	/// <summary>Number of packets in the incoming queue</summary>
	public int getInboxCount() 
	{ return PacketInbox.size(); } 
	/// <summary>Number of packets in the outgoing queue</summary>
	public int getOutboxCount() 
	{ return PacketOutbox.size(); } 

	public class LoginProgressObserver implements Observer
	{
		public void update(Observable arg0, Object arg1) {
			CapsHttpRequestProgressArg rcha = (CapsHttpRequestProgressArg) arg1;
			//			System.out.println("Download Progress, bytes recieved: "  + rcha.getBytesReceived() + " total bytes revieved: " + rcha.getTotalBytesToReceive());
			//TODO implement
		}	
	}	

	public class LoginCompletedObserver extends  EventObserver<CapsHttpRequestCompletedArg>
	{
		public void handleEvent(Observable arg0, CapsHttpRequestCompletedArg arg1) {
			//			System.out.println("RequestCompletedObserver called ...");
			CapsHttpRequestCompletedArg rcha = (CapsHttpRequestCompletedArg) arg1;
			OSD osd =  null;
			if(rcha.getError() == null)
			{
				osd = rcha.getResult();

			}
			LoginReplyLLSDHandler(rcha.getClient(), osd, rcha.getError());
		}	
	}	

	//region Public Members
	/// <summary>Seed CAPS URL returned from the login server</summary>
	public String LoginSeedCapability = "";
	/// <summary>Current state of logging in</summary>
	public LoginStatus getLoginStatusCode() { return InternalStatusCode;  }
	/// <summary>Upon login failure, contains a short string key for the
	/// type of login error that occurred</summary>
	public String getLoginErrorKey() { return InternalErrorKey;  }
	/// <summary>The raw XML-RPC reply from the login server, exactly as it
	/// was received (minus the HTTP header)</summary>
	public String getRawLoginReply() { return InternalRawLoginReply; } 
	/// <summary>During login this contains a descriptive version of 
	/// LoginStatusCode. After a successful login this will contain the 
	/// message of the day, and after a failed login a descriptive error 
	/// message will be returned</summary>
	public String getLoginMessage() { return InternalLoginMessage; } 
	/// <summary>Maximum number of groups an agent can belong to, -1 for unlimited</summary>
	public int MaxAgentGroups = -1;
	/// <summary>XMPP server to connect to for Group chat and IM services</summary>
	public String XMPPHost;
	//endregion

	//region Private Members
	private LoginParams CurrentContext = null;
	private AutoResetEvent LoginEvent = new AutoResetEvent(false);
	private LoginStatus InternalStatusCode = LoginStatus.None;
	private String InternalErrorKey = "";
	private String InternalLoginMessage = "";
	private String InternalRawLoginReply = "";

	private Map<Observer, String[]> CallbackOptions = new HashMap<Observer, String[]>();

	/// <summary>A list of packets obtained during the login process which 
	/// networkmanager will log but not process</summary>
	private final List<String> UDPBlacklist = new ArrayList<String>();
	//endregion

	//Start Enums

	/// <summary>
	/// Explains why a simulator or the grid disconnected from us
	/// </summary>
	public enum DisconnectType
	{
		/// <summary>The client requested the logout or simulator disconnect</summary>
		ClientInitiated,
		/// <summary>The server notified us that it is disconnecting</summary>
		ServerInitiated,
		/// <summary>Either a socket was closed or network traffic timed out</summary>
		NetworkTimeout,
		/// <summary>The last active simulator shut down</summary>
		SimShutdown
	}

	//endre Enums

	//region Structs

	/// <summary>
	/// Holds a simulator reference and a decoded packet, these structs are put in
	/// the packet inbox for event handling
	/// </summary>
	public static class IncomingPacket
	{
		/// <summary>Reference to the simulator that this packet came from</summary>
		public Simulator simulator;
		/// <summary>Packet that needs to be processed</summary>
		public Packet packet;

		public IncomingPacket() {
			super();
		}

		public IncomingPacket(Simulator simulator, Packet packet)
		{
			this.simulator = simulator;
			this.packet = packet;
		}
	}

	/// <summary>
	/// Holds a simulator reference and a serialized packet, these structs are put in
	/// the packet outbox for sending
	/// </summary>
	public static class OutgoingPacket
	{
		/// <summary>Reference to the simulator this packet is destined for</summary>
		public Simulator Simulator;
		/// <summary>Packet that needs to be sent</summary>
		public UDPPacketBuffer Buffer;
		/// <summary>Sequence number of the wrapped packet</summary>
		//Uint
		public long SequenceNumber;
		/// <summary>Number of times this packet has been resent</summary>
		public AtomicInteger ResendCount = new AtomicInteger(0);
		/// <summary>Environment.TickCount when this packet was last sent over the wire</summary>
		public long TickCount;
		/// <summary>Type of the packet</summary>
		public PacketType Type;

		public OutgoingPacket(Simulator simulator, UDPPacketBuffer buffer, PacketType type)
		{
			Simulator = simulator;
			Buffer = buffer;
			this.Type = type;
		}
	}
	//End classes

	//Start Event Observables

	private EventObservable<LoginResponseCallbackArg> OnLoginResponse = new EventObservable<LoginResponseCallbackArg>();
	private EventObservable<LoginProgressEventArgs> OnLoginProgress = new EventObservable<LoginProgressEventArgs>();
	//    private EventObservable<Object> internalRequestCompletedObservable = new EventObservable<Object>();

	//OnPacketSent Observable
	private EventObservable<PacketSentEventArgs> OnPacketSent = new EventObservable<PacketSentEventArgs>();

	//OnLoggedOut Observable
	private EventObservable<LoggedOutEventArgs> OnLoggedOut = new EventObservable<LoggedOutEventArgs>();

	//OnSimConnecting Observable
	private EventObservable<SimConnectingEventArgs> OnSimConnecting = new EventObservable<SimConnectingEventArgs>();

	//OnSimConnected Observable
	private EventObservable<SimConnectedEventArgs> OnSimConnected = new EventObservable<SimConnectedEventArgs>();

	//OnSimDisconnected Observable
	private EventObservable<SimDisconnectedEventArgs> OnSimDisconnected = new EventObservable<SimDisconnectedEventArgs>();

	//OnDisconnected Observable
	private EventObservable<DisconnectedEventArgs> OnDisconnected = new EventObservable<DisconnectedEventArgs>();

	//OnSimChanged Observable
	private EventObservable<SimChangedEventArgs> OnSimChanged = new EventObservable<SimChangedEventArgs>();

	//OnEventQueueRunning Observable
	private EventObservable<EventQueueRunningEventArgs> OnEventQueueRunning = new EventObservable<EventQueueRunningEventArgs>();


	public void RegisterLoginResponseCallback(EventObserver<LoginResponseCallbackArg> callback)
	{
		RegisterLoginResponseCallback(callback, null);
	}

	public void RegisterLoginResponseCallback(EventObserver<LoginResponseCallbackArg> callback, String[] options)
	{
		CallbackOptions.put(callback, options);
		OnLoginResponse.addObserver(callback);
	}

	public void UnregisterLoginResponseCallback(EventObserver<LoginResponseCallbackArg> callback)
	{
		CallbackOptions.remove(callback);
		OnLoginResponse.deleteObserver(callback);
	}

	public void RegisterLoginProgressCallback(EventObserver<LoginProgressEventArgs> callback)
	{
		OnLoginProgress.addObserver(callback);
	}

	public void UnregisterLoginProgressCallback(EventObserver<LoginProgressEventArgs> callback)
	{
		OnLoginProgress.deleteObserver(callback);
	}

	public void RegisterOnPacketSentCallback(Observer callback)
	{
		OnPacketSent.addObserver(callback);
	}

	public void UnregisterOnPacketSentCallback(Observer callback)
	{
		OnPacketSent.deleteObserver(callback);
	}

	public void RegisterOnLoggedOutCallback(Observer callback)
	{
		OnLoggedOut.addObserver(callback);
	}

	public void UnregisterOnLoggedOutCallback(Observer callback)
	{
		OnLoggedOut.deleteObserver(callback);
	}

	public void RegisterOnSimConnectingCallback(Observer callback)
	{
		OnSimConnecting.addObserver(callback);
	}

	public void UnregisterOnSimConnectingCallback(Observer callback)
	{
		OnSimConnecting.deleteObserver(callback);
	}

	public void RegisterOnSimConnectedCallback(Observer callback)
	{
		OnSimConnected.addObserver(callback);
	}

	public void UnregisterOnSimConnectedCallback(Observer callback)
	{
		OnSimConnected.deleteObserver(callback);
	}

	public void RegisterOnOnSimDisconnectedCallback(Observer callback)
	{
		OnSimDisconnected.addObserver(callback);
	}

	public void UnregisterOnOnSimDisconnectedCallback(Observer callback)
	{
		OnSimDisconnected.deleteObserver(callback);
	}

	public void RegisterOnSimChangedCallback(Observer callback)
	{
		OnSimChanged.addObserver(callback);
	}

	public void UnregisterOnSimChangedCallback(Observer callback)
	{
		OnSimChanged.deleteObserver(callback);
	}

	public void RegisterOnEventQueueRunningCallback(Observer callback)
	{
		OnEventQueueRunning.addObserver(callback);
	}

	public void UnregisterOnEventQueueRunningCallback(Observer callback)
	{
		OnEventQueueRunning.deleteObserver(callback);
	}

	public void RegisterOnDisconnectedCallback(Observer callback)
	{
		OnDisconnected.addObserver(callback);
	}

	public void UnregisterOnDisconnectedCallback(Observer callback)
	{
		OnDisconnected.deleteObserver(callback);
	}

	//End Event Observables


	//region Public Methods

	/// <summary>
	/// Register an event handler for a packet. This is a low level event
	/// interface and should only be used if you are doing something not
	/// supported in the library
	/// </summary>
	/// <param name="type">Packet type to trigger events for</param>
	/// <param name="callback">Callback to fire when a packet of this type
	/// is received</param>
	public void RegisterCallback(PacketType type, EventObserver<PacketReceivedEventArgs> callback)
	{
		RegisterCallback(type, callback, true);
	}

	/// <summary>
	/// Register an event handler for a packet. This is a low level event
	/// interface and should only be used if you are doing something not
	/// supported in the library
	/// </summary>
	/// <param name="type">Packet type to trigger events for</param>
	/// <param name="callback">Callback to fire when a packet of this type
	/// is received</param>
	/// <param name="isAsync">True if the callback should be ran 
	/// asynchronously. Only set this to false (synchronous for callbacks 
	/// that will always complete quickly)</param>
	/// <remarks>If any callback for a packet type is marked as 
	/// asynchronous, all callbacks for that packet type will be fired
	/// asynchronously</remarks>
	public void RegisterCallback(PacketType type, EventObserver<PacketReceivedEventArgs> callback, boolean isAsync)
	{
		PacketEvents.RegisterEvent(type, callback, isAsync);
	}

	/// <summary>
	/// Unregister an event handler for a packet. This is a low level event
	/// interface and should only be used if you are doing something not 
	/// supported in the library
	/// </summary>
	/// <param name="type">Packet type this callback is registered with</param>
	/// <param name="callback">Callback to stop firing events for</param>
	public void UnregisterCallback(PacketType type, Observer callback)
	{
		PacketEvents.UnregisterEvent(type, callback);
	}

	/// <summary>
	/// Register a CAPS event handler. This is a low level event interface
	/// and should only be used if you are doing something not supported in
	/// the library
	/// </summary>
	/// <param name="capsEvent">Name of the CAPS event to register a handler for</param>
	/// <param name="callback">Callback to fire when a CAPS event is received</param>
	public void RegisterEventCallback(String capsEvent, EventObserver<CapsEventObservableArg> callback)
	{
		CapsEvents.RegisterEvent(capsEvent, callback);
	}

	/// <summary>
	/// Unregister a CAPS event handler. This is a low level event interface
	/// and should only be used if you are doing something not supported in
	/// the library
	/// </summary>
	/// <param name="capsEvent">Name of the CAPS event this callback is
	/// registered with</param>
	/// <param name="callback">Callback to stop firing events for</param>
	public void UnregisterEventCallback(String capsEvent, EventObserver<CapsEventObservableArg> callback)
	{
		CapsEvents.UnregisterEvent(capsEvent, callback);
	}




	/* Start Login Related Region*/

	/// <summary>
	/// Generate sane default values for a login request
	/// </summary>
	/// <param name="firstName">Account first name</param>
	/// <param name="lastName">Account last name</param>
	/// <param name="password">Account password</param>
	/// <param name="userAgent">Client application name</param>
	/// <param name="userVersion">Client application version</param>
	/// <returns>A populated <seealso cref="LoginParams"/> struct containing
	/// sane defaults</returns>
	public LoginParams DefaultLoginParams(String firstName, String lastName, String password,
			String userAgent, String userVersion)
	{
		return new LoginParams(client, firstName, lastName, password, userAgent, userVersion);
	}

	/// <summary>
	/// Simplified login that takes the most common and required fields
	/// </summary>
	/// <param name="firstName">Account first name</param>
	/// <param name="lastName">Account last name</param>
	/// <param name="password">Account password</param>
	/// <param name="userAgent">Client application name</param>
	/// <param name="userVersion">Client application version</param>
	/// <returns>Whether the login was successful or not. On failure the
	/// LoginErrorKey string will contain the error code and LoginMessage
	/// will contain a description of the error</returns>
	public boolean Login(String firstName, String lastName, String password, String userAgent, String userVersion) throws Exception
	{
		return Login(firstName, lastName, password, userAgent, "last", userVersion);
	}

	/// <summary>
	/// Simplified login that takes the most common fields along with a
	/// starting location URI, and can accept an MD5 string instead of a
	/// plaintext password
	/// </summary>
	/// <param name="firstName">Account first name</param>
	/// <param name="lastName">Account last name</param>
	/// <param name="password">Account password or MD5 hash of the password
	/// such as $1$1682a1e45e9f957dcdf0bb56eb43319c</param>
	/// <param name="userAgent">Client application name</param>
	/// <param name="start">Starting location URI that can be built with
	/// StartLocation()</param>
	/// <param name="userVersion">Client application version</param>
	/// <returns>Whether the login was successful or not. On failure the
	/// LoginErrorKey string will contain the error code and LoginMessage
	/// will contain a description of the error</returns>
	public boolean Login(String firstName, String lastName, String password, String userAgent, String start,
			String userVersion) throws Exception
			{
		LoginParams loginParams = DefaultLoginParams(firstName, lastName, password, userAgent, userVersion);
		loginParams.Start = start;

		return Login(loginParams);
			}

	/// <summary>
	/// Login that takes a struct of all the values that will be passed to
	/// the login server
	/// </summary>
	/// <param name="loginParams">The values that will be passed to the login
	/// server, all fields must be set even if they are ""</param>
	/// <returns>Whether the login was successful or not. On failure the
	/// LoginErrorKey string will contain the error code and LoginMessage
	/// will contain a description of the error</returns>
	public boolean Login(LoginParams loginParams) throws Exception
	{
		BeginLogin(loginParams);

		//        LoginEvent.WaitOne(loginParams.Timeout, false);
		//        waitForRequestCompletion(loginParams.Timeout);
		JLogger.info("Thread going to wait for long timout: " + loginParams.Timeout);
		LoginEvent.waitOne(loginParams.Timeout);
		JLogger.info("Thread got login response");

		if (CurrentContext != null)
		{
			CurrentContext = null; // Will force any pending callbacks to bail out early
			InternalStatusCode = LoginStatus.Failed;
			InternalLoginMessage = "Timed out";
			return false;
		}

		return (InternalStatusCode == LoginStatus.Success);
	}

	public void BeginLogin(LoginParams loginParams) throws Exception
	{
		// FIXME: Now that we're using CAPS we could cancel the current login and start a new one
		if (CurrentContext != null)
			throw new Exception("Login already in progress");

		//        LoginEvent.Reset();
		LoginEvent.reset();
		CurrentContext = loginParams;

		BeginLogin();
	}

	
	/// <summary>
	/// Build a start location URI for passing to the Login function
	/// </summary>
	/// <param name="sim">Name of the simulator to start in</param>
	/// <param name="x">X coordinate to start at</param>
	/// <param name="y">Y coordinate to start at</param>
	/// <param name="z">Z coordinate to start at</param>
	/// <returns>String with a URI that can be used to login to a specified
	/// location</returns>
	public static String StartLocation(String sim, int x, int y, int z)
	{
		return String.format("uri:%s&%d&%d&%d", sim, x, y, z);
	}

	//endregion

	//region Private Methods

	private void BeginLogin() throws Exception
	{
		final LoginParams loginParams = CurrentContext;
		// Generate a random ID to identify this login attempt
		loginParams.LoginID = UUID.Random();
		CurrentContext = loginParams;

		//region Sanity Check loginParams

		if (loginParams.Options == null)
			loginParams.Options = new ArrayList<String>().toArray(new String[0]);

		// Convert the password to MD5 if it isn't already
		if (loginParams.Password.length() != 35 && !loginParams.Password.startsWith("$1$"))
			loginParams.Password = "$1$" + Utils.MD5(loginParams.Password);

		if (loginParams.ViewerDigest == null)
			loginParams.ViewerDigest = "";

		if (loginParams.Version == null)
			loginParams.Version = "";

		if (loginParams.UserAgent == null)
			loginParams.UserAgent = "";

		if (loginParams.Platform == null)
			loginParams.Platform = "";

		if (loginParams.MAC == null)
			loginParams.MAC = "";

		if (loginParams.Channel == null)
			loginParams.Channel = "";

		if (loginParams.Author == null)
			loginParams.Author = "";

		//endregion

		logger.info(loginParams.Password);

		// TODO: Allow a user callback to be defined for handling the cert
		//TODO need to implement certificate policy
		//        ServicePointManager.CertificatePolicy = new TrustAllCertificatePolicy();

		if(client.settings.USE_LLSD_LOGIN)
		{
			//region LLSD Based Login

			// Create the CAPS login structure
			OSDMap loginLLSD = new OSDMap();
			loginLLSD.put("first",  OSD.FromString(loginParams.FirstName));
			loginLLSD.put("last", OSD.FromString(loginParams.LastName));
			loginLLSD.put("passwd",  OSD.FromString(loginParams.Password));
			loginLLSD.put("start",  OSD.FromString(loginParams.Start));
			loginLLSD.put("channel",  OSD.FromString(loginParams.Channel));
			loginLLSD.put("version",  OSD.FromString(loginParams.Version));
			loginLLSD.put("platform",  OSD.FromString(loginParams.Platform));
			loginLLSD.put("mac",  OSD.FromString(loginParams.MAC));
			loginLLSD.put("agree_to_tos",  OSD.FromBoolean(loginParams.AgreeToTos));
			loginLLSD.put("read_critical",  OSD.FromBoolean(loginParams.ReadCritical));
			loginLLSD.put("viewer_digest",  OSD.FromString(loginParams.ViewerDigest));
			loginLLSD.put("id0",  OSD.FromString(loginParams.ID0));

			// Create the options LLSD array
			OSDArray optionsOSD = new OSDArray();
			for (int i = 0; i < loginParams.Options.length; i++)
				optionsOSD.add(OSD.FromString(loginParams.Options[i]));

			for(String[] callbackOpts :CallbackOptions.values())
			{
				if (callbackOpts != null)
				{
					for (int i = 0; i < callbackOpts.length; i++)
					{
						if (!optionsOSD.contains(callbackOpts[i]))
							optionsOSD.add(OSD.FromString(callbackOpts[i]));
					}
				}
			}
			loginLLSD.put("options",  optionsOSD);

			// Make the CAPS POST for login
			URI loginUri;
			try
			{
				loginUri = new URI(loginParams.URI);
			}
			catch (Exception ex)
			{
				logger.warning((String.format("Failed to parse login URI %s, %s", loginParams.URI.toString(), 
						ex.getMessage())));
				return;
			}

			CapsHttpClient loginRequest = new CapsHttpClient(loginUri);
			LoginCompletedObserver loginCompletedObserver = new LoginCompletedObserver();
			//            loginRequest.OnComplete += new CapsClient.CompleteCallback(LoginReplyLLSDHandler);
			loginRequest.addRequestCompleteObserver(loginCompletedObserver);
			loginRequest.setUserData(CurrentContext);
			UpdateLoginStatus(LoginStatus.ConnectingToLogin, String.format("Logging in as %s %s...", loginParams.FirstName, loginParams.LastName));
			loginRequest.BeginGetResponse(loginLLSD, OSDFormat.Xml, client.settings.CAPS_TIMEOUT);

			//endregion
		}
		else
		{
			//XML-RPC Based Login Code

			// Create the Hashtable for XmlRpcCs
			Map<String, Object> loginXmlRpc = new HashMap<String, Object>();
			loginXmlRpc.put("first", loginParams.FirstName);
			loginXmlRpc.put("last", loginParams.LastName);
			loginXmlRpc.put("passwd", loginParams.Password);
			loginXmlRpc.put("start", loginParams.Start);
			loginXmlRpc.put("channel", loginParams.Channel);
			loginXmlRpc.put("version", loginParams.Version);
			loginXmlRpc.put("platform", loginParams.Platform);
			loginXmlRpc.put("mac", loginParams.MAC);
			if (loginParams.AgreeToTos)
				loginXmlRpc.put("agree_to_tos", "true");
			if (loginParams.ReadCritical)
				loginXmlRpc.put("read_critical", "true");
			loginXmlRpc.put("id0", loginParams.ID0);
			loginXmlRpc.put("last_exec_event", 0);

			// Create the options array
			List<String> options = new ArrayList<String>();
			for (int i = 0; i < loginParams.Options.length; i++)
				options.add(loginParams.Options[i]);

			for (String[] callbackOpts : CallbackOptions.values())
			{
				if (callbackOpts != null)
				{
					for (int i = 0; i < callbackOpts.length; i++)
					{
						if (!options.contains(callbackOpts[i]))
							options.add(callbackOpts[i]);
					}
				}
			}
			loginXmlRpc.put("options", options);

			try
			{
				final ArrayList<Object> loginArray = new ArrayList<Object>(1);
				loginArray.add(loginXmlRpc);
//				XmlRpcClient request = new XmlRpcClient(CurrentContext.MethodName, loginArray);
				final XmlRpcClient request = new XmlRpcClient();
			    XmlRpcClientConfigImpl config = new XmlRpcClientConfigImpl();
			    config.setServerURL(new URL(CurrentContext.URI));
			    request.setConfig(config);

				// Start the request
				threadPool.execute(new Runnable(){
					public void run()
					{
						try
						{
							Object response = request.execute(CurrentContext.MethodName, loginArray);
					        
							//System.out.println("Results:" + response);
							
							LoginReplyXmlRpcHandler(response ,loginParams);
						}
						catch (Exception e)
						{
							UpdateLoginStatus(LoginStatus.Failed, "Error opening the login server connection: " + e.getMessage());
						}
					}
				});
			}
			catch (Exception e)
			{
				UpdateLoginStatus(LoginStatus.Failed, "Error opening the login server connection: " + e);
			}

			//endregion
		}
	}

	private void UpdateLoginStatus(LoginStatus status, String message)
	{
		InternalStatusCode = status;
		InternalLoginMessage = message;

		logger.info("Login status: " + status.toString() + ": " + message);

		// If we reached a login resolution trigger the event
		if (status == LoginStatus.Success || status == LoginStatus.Failed)
		{
			CurrentContext = null;
			//Raise the event to wake up main waiting thread
			LoginEvent.set();
		}

		// Fire the login status callback
		if (OnLoginProgress != null)
		{
			//            OnLoginProgress(new LoginProgressEventArgs(status, message, InternalErrorKey));
			OnLoginProgress.raiseEvent((new LoginProgressEventArgs(status, message, InternalErrorKey)));
		}
	}


	//	/// <summary>
	//	/// LoginParams and the initial login XmlRpcRequest were made on a remote machine.
	//	/// This method now initializes libomv with the results.
	//	/// </summary>
	//	public void RemoteLoginHandler(LoginResponseData response, LoginParams newContext) 
	//	{
	//		CurrentContext = newContext;
	//		LoginReplyXmlRpcHandler(response, newContext);
	//	}


	    /// <summary>
	    /// Handles response from XML-RPC login replies
	    /// </summary>
	    private void LoginReplyXmlRpcHandler(Object response, LoginParams context) throws Exception
	    {
			JLogger.debug("Parsing from Object Response");
	        LoginResponseData reply = new LoginResponseData();
	        
//	        System.out.println(response.getClass().toString());
	        
	        // Fetch the login response
	        if (response == null || !(response instanceof Map))
	        {
	            UpdateLoginStatus(LoginStatus.Failed, "Invalid or missing login response from the server");
	            JLogger.warn("Invalid or missing login response from the server");
	            return;
	        }
	
	        try
	        {
	            reply.Parse((Map)response);
	            if (!context.LoginID.equals(CurrentContext.LoginID))
	            {
	                JLogger.error("Login response does not match login request. Only one login can be attempted at a time");
	                return;
	            }
	        }
	        catch (Exception e)
	        {
	            UpdateLoginStatus(LoginStatus.Failed, "Error retrieving the login response from the server: " + e.getMessage());
	            JLogger.warn("Login response failure: " + Utils.getExceptionStackTraceAsString(e));
	            return;
	        }
	        JLogger.debug("Parsed Login Response...");
			LoginReplyXmlRpcHandler(reply, context);
		}
	
	
		/// <summary>
		/// Handles response from XML-RPC login replies with already parsed LoginResponseData
		/// </summary>
		private void LoginReplyXmlRpcHandler(LoginResponseData reply, LoginParams context) throws Exception
		{
			JLogger.debug("Parsing from LoginResponseData");
			 
			//reply.getMap(reply);
			
			int simPort = 0;
			long regionX = 0;
			long regionY = 0;
			String reason = reply.Reason;
	        String message = reply.Message;
	
	        if (reply.Login.equalsIgnoreCase("true"))
	        {
	            // Remove the quotes around our first name.
	            if (reply.FirstName.indexOf(0) == '"')
	                reply.FirstName = reply.FirstName.substring(1);
	            if (reply.FirstName.indexOf(reply.FirstName.length() - 1) == '"')
	                reply.FirstName = reply.FirstName.substring(0, reply.FirstName.length() - 1);
	
	            //region Critical Information
	
	            try
	            {
	                // Networking
	                client.network.circuitCode = reply.CircuitCode & Long.MAX_VALUE;
	                regionX = reply.RegionX & Long.MAX_VALUE;
	                regionY = reply.RegionY & Long.MAX_VALUE;
	                simPort = reply.SimPort & Integer.MAX_VALUE;
	                LoginSeedCapability = reply.SeedCapability;
	            }
	            catch (Exception e)
	            {
	                UpdateLoginStatus(LoginStatus.Failed, "Login server failed to return critical information " + e.getMessage());
	                return;
	            }
	
	            //endregion Critical Information
	
	            /* Add any blacklisted UDP packets to the blacklist
	             * for exclusion from packet processing */
	            
	            if (reply.UDPBlacklist != null)
	                for(String p: reply.UDPBlacklist.split(","))
	                {
	                	UDPBlacklist.add(p);
	                }
	
	            
	            // Misc:
	            MaxAgentGroups = reply.MaxAgentGroups;
	            XMPPHost = reply.XMPPHost;
	
	            //uint timestamp = (uint)reply.seconds_since_epoch;
	            //DateTime time = Helpers.UnixTimeToDateTime(timestamp); // TODO: Do something with this?
	
	            // Unhandled:
	            // reply.gestures
	            // reply.event_categories
	            // reply.classified_categories
	            // reply.event_notifications
	            // reply.ui_config
	            // reply.login_flags
	            // reply.global_textures
	            // reply.inventory_lib_root
	            // reply.inventory_lib_owner
	            // reply.inventory_skeleton
	            // reply.inventory_skel_lib
	            // reply.initial_outfit
	        }
	
	        boolean redirect = (reply.Login.equals("indeterminate"));
	
	        try
	        {
	            if (OnLoginResponse != null)
	            {
	                try 
	                {
						LoginResponseCallbackArg lrg = new LoginResponseCallbackArg(reply.Success, redirect, 
								message, reason, reply);
						OnLoginResponse.raiseEvent(lrg);
//	                	OnLoginResponse(reply.Success, redirect, message, reason, reply); 
	                }
	                catch (Exception ex) 
	                { 
	                	JLogger.error(Utils.getExceptionStackTraceAsString(ex)); 
	                }
	            }
	        }
	        catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
	
	        // Make the next network jump, if needed
	        if (redirect)
	        {
	            UpdateLoginStatus(LoginStatus.Redirecting, "Redirecting login...");
	            LoginParams loginParams = CurrentContext;
	            loginParams.URI = reply.NextUrl;
	            loginParams.MethodName = reply.NextMethod;
	            loginParams.Options = reply.NextOptions;
	
	            // Sleep for some amount of time while the servers work
	            int seconds = reply.NextDuration;
	            JLogger.info("Sleeping for " + seconds + " seconds during a login redirect");
	            PlatformUtils.sleep(seconds * 1000);
	
	            CurrentContext = loginParams;
	            BeginLogin();
	        }
	        else if (reply.Success)
	        {
	            UpdateLoginStatus(LoginStatus.ConnectingToSim, "Connecting to simulator...");
	
	            BigInteger handle = Utils.uintsToULong(regionX, regionY);
	
	            // Connect to the sim given in the login reply
	            if (Connect(reply.SimIP, simPort, handle, true, LoginSeedCapability) != null)
	            {
	                // Request the economy data right after login
	                SendPacket(new EconomyDataRequestPacket());
	
	                // Update the login message with the MOTD returned from the server
	                UpdateLoginStatus(LoginStatus.Success, message);
	            }
	            else
	            {
	                UpdateLoginStatus(LoginStatus.Failed, "Unable to connect to simulator");
	            }
	        }
	        else
	        {
	            // Make sure a usable error key is set
	
	            if (!Utils.isNullOrEmpty(reason))
	                InternalErrorKey = reason;
	            else
	                InternalErrorKey = "unknown";
	
	            UpdateLoginStatus(LoginStatus.Failed, message);
	        }
	    }

	/// <summary>
	/// Handle response from LLSD login replies
	/// </summary>
	/// <param name="client"></param>
	/// <param name="result"></param>
	/// <param name="error"></param>
	private void LoginReplyLLSDHandler(CapsHttpClient client, OSD result, Exception error) 
	{
		try
		{
			if (error == null)
			{
				if (result != null && result.getType().equals(OSDType.Map))
				{
					OSDMap map = (OSDMap)result;
					OSD osd;

					LoginResponseData data = new LoginResponseData();
					data.Parse(map);

//					data.getMap(data);
					
					if ((osd = map.get("login")) !=null)
					{
						boolean loginSuccess = osd.asBoolean();
						boolean redirect = (osd.asString().equals("indeterminate"));

						if (redirect)
						{
							// Login redirected

							// Make the next login URL jump
							UpdateLoginStatus(LoginStatus.Redirecting, data.Message);

							LoginParams loginParams = CurrentContext;
							loginParams.URI = LoginResponseData.ParseString("next_url", map);
							//CurrentContext.Params.MethodName = LoginResponseData.ParseString("next_method", map);

							// Sleep for some amount of time while the servers work
							int seconds = (int)LoginResponseData.ParseUInt("next_duration", map);
							logger.info("Sleeping for " + seconds + " seconds during a login redirect");
							Thread.sleep(seconds * 1000);

							// Ignore next_options for now
							CurrentContext = loginParams;

							BeginLogin();
						}
						else if (loginSuccess)
						{
							// Login succeeded

							// Fire the login callback
							if (OnLoginResponse != null)
							{
								try 
								{
									LoginResponseCallbackArg lrg = new LoginResponseCallbackArg(loginSuccess, redirect, data.Message, data.Reason, data);
									OnLoginResponse.raiseEvent(lrg);
									//                            	OnLoginResponse(loginSuccess, redirect, data.Message, data.Reason, data); 
								}
								catch (Exception ex) { logger.warning(Utils.getExceptionStackTraceAsString(ex)); }
							}

							// These parameters are stored in NetworkManager, so instead of registering
							// another callback for them we just set the values here
							circuitCode = (long)data.CircuitCode;
							LoginSeedCapability = data.SeedCapability;

							UpdateLoginStatus(LoginStatus.ConnectingToSim, "Connecting to simulator...");

							//ulong
							BigInteger handle = new BigInteger(Utils.int64ToBytes(Utils.uintsToLong((long)data.RegionX, 
									(long)data.RegionY)));

							if (data.SimIP != null && data.SimPort != 0)
							{
								// Connect to the sim given in the login reply
								if (Connect(data.SimIP, data.SimPort, handle, true, LoginSeedCapability) != null)
								{
									// Request the economy data right after login
									SendPacket(new EconomyDataRequestPacket());

									// Update the login message with the MOTD returned from the server
									UpdateLoginStatus(LoginStatus.Success, data.Message);
								}
								else
								{
									UpdateLoginStatus(LoginStatus.Failed,
											"Unable to establish a UDP connection to the simulator");
								}
							}
							else
							{
								UpdateLoginStatus(LoginStatus.Failed,
										"Login server did not return a simulator address");
							}
						}
						else
						{
							// Login failed

							// Make sure a usable error key is set
							if (data.Reason != "")
								InternalErrorKey = data.Reason;
							else
								InternalErrorKey = "unknown";

							UpdateLoginStatus(LoginStatus.Failed, data.Message);
						}
					}
					else
					{
						// Got an LLSD map but no login value
						UpdateLoginStatus(LoginStatus.Failed, "login parameter missing in the response");
					}
				}
				else
				{
					// No LLSD response
					InternalErrorKey = "bad response";
					UpdateLoginStatus(LoginStatus.Failed, "Empty or unparseable login response");
				}
			}
			else
			{
				// Connection error
				InternalErrorKey = "no connection";
				UpdateLoginStatus(LoginStatus.Failed, error.getMessage());
			}
		}
		catch(Exception e)
		{
			// Connection error
			InternalErrorKey = "no connection";
			UpdateLoginStatus(LoginStatus.Failed, e.getMessage());
		}
	}

	//endregion


	/// <summary>
	/// Send a packet to the simulator the avatar is currently occupying
	/// </summary>
	/// <param name="packet">Packet to send</param>
	public void SendPacket(Packet packet)
	{
		// try CurrentSim, however directly after login this will
		// be null, so if it is, we'll try to find the first simulator
		// we're connected to in order to send the packet.
		Simulator simulator = getCurrentSim();

		if (simulator == null && client.network.Simulators.size() >= 1)
		{
			JLogger.debug("CurrentSim object was null, using first found connected simulator");
			simulator = client.network.Simulators.get(0);
		}            
		
		if (simulator != null && simulator.getConnected())
		{
			simulator.SendPacket(packet);
		}
		else
		{
			//throw new NotConnectedException("Packet received before simulator packet processing threads running, make certain you are completely logged in");
			JLogger.error("Packet received before simulator packet processing threads running, make certain you are completely logged in.");
		}
	}

	/// <summary>
	/// Send a packet to a specified simulator
	/// </summary>
	/// <param name="packet">Packet to send</param>
	/// <param name="simulator">Simulator to send the packet to</param>
	public void SendPacket(Packet packet, Simulator simulator)
	{
		if (simulator != null)
		{
			simulator.SendPacket(packet);
		}
		else
		{
			JLogger.error("Packet received before simulator packet processing threads running, make certain you are completely logged in");
		}
	}

	/// <summary>
	/// Connect to a simulator
	/// </summary>
	/// <param name="ip">IP address to connect to</param>
	/// <param name="port">Port to connect to</param>
	/// <param name="handle">Handle for this simulator, to identify its
	/// location in the grid</param>
	/// <param name="setDefault">Whether to set CurrentSim to this new
	/// connection, use this if the avatar is moving in to this simulator</param>
	/// <param name="seedcaps">URL of the capabilities server to use for
	/// this sim connection</param>
	/// <returns>A Simulator object on success, otherwise null</returns>
	public Simulator Connect(InetAddress ip, int port, BigInteger handle, boolean setDefault, String seedcaps) throws Exception
	{
		InetSocketAddress endPoint = new InetSocketAddress(ip, (int)port);
		return Connect(endPoint, handle, setDefault, seedcaps);
	}

	/// <summary>
	/// Connect to a simulator
	/// </summary>
	/// <param name="endPoint">IP address and port to connect to</param>
	/// <param name="handle">Handle for this simulator, to identify its
	/// location in the grid</param>
	/// <param name="setDefault">Whether to set CurrentSim to this new
	/// connection, use this if the avatar is moving in to this simulator</param>
	/// <param name="seedcaps">URL of the capabilities server to use for
	/// this sim connection</param>
	/// <returns>A Simulator object on success, otherwise null</returns>
	public Simulator Connect(InetSocketAddress endPoint, BigInteger handle, boolean setDefault, String seedcaps) throws Exception
	{
		Simulator simulator = FindSimulator(endPoint);

		if (simulator == null)
		{
			// We're not tracking this sim, create a new Simulator object
			simulator = new Simulator(client, endPoint, handle);

			// Immediately add this simulator to the list of current sims. It will be removed if the
			// connection fails
			synchronized (Simulators) {Simulators.add(simulator);}
		}

		if (!simulator.getConnected())
		{
			if (!connected)
			{
				// Mark that we are connecting/connected to the grid
				// 
				connected = true;
				threadPool.execute(new Runnable(){
					public void run()
					{
						IncomingPacketHandler();
					}
				});

				threadPool.execute(new Runnable(){
					public void run()
					{
						OutgoingPacketHandler();
					}
				});
			}

			// raise the SimConnecting event and allow any event
			// subscribers to cancel the connection
			if (OnSimConnecting != null)
			{
				SimConnectingEventArgs args = new SimConnectingEventArgs(simulator);
				OnSimConnecting.raiseEvent(args);

				if (args.getCancel())
				{
					// Callback is requesting that we abort this connection
					synchronized (Simulators)
					{
						Simulators.remove(simulator);
					}
					return null;
				}
			}

			// Attempt to establish a connection to the simulator
			if (simulator.Connect(setDefault))
			{
				if (DisconnectTimer == null)
				{
					// Start a timer that checks if we've been disconnected

					//                    DisconnectTimer = new Timer(new TimerCallback(DisconnectTimer_Elapsed), null,
					//                        client.settings.SIMULATOR_TIMEOUT, client.settings.SIMULATOR_TIMEOUT);

					DisconnectTimer = new Timer();
					DisconnectTimer.schedule(new TimerTask(){
						@Override
						public void run() {
							DisconnectTimer_Elapsed(null);
						}				
					}, client.settings.SIMULATOR_TIMEOUT, client.settings.SIMULATOR_TIMEOUT);

				}

				if (setDefault)
				{
					SetCurrentSim(simulator, seedcaps);
				}

				// Raise the SimConnected event
				if (OnSimConnected != null)
				{
					OnSimConnected.raiseEvent(new SimConnectedEventArgs(simulator));
				}

				// If enabled, send an AgentThrottle packet to the server to increase our bandwidth
				if (client.settings.SEND_AGENT_THROTTLE)
				{
					client.throttle.Set(simulator);
				}

				return simulator;
			}
			else
			{
				// Connection failed, remove this simulator from our list and destroy it
				synchronized (Simulators)
				{
					Simulators.remove(simulator);
				}                    

				return null;
			}
		}
		else if (setDefault)
		{
			// Move in to this simulator
			simulator.handshakeComplete = false;
			simulator.UseCircuitCode(true);
			client.self.CompleteAgentMovement(simulator);

			// We're already connected to this server, but need to set it to the default
			SetCurrentSim(simulator, seedcaps);

			// Send an initial AgentUpdate to complete our movement in to the sim
			if (client.settings.SEND_AGENT_UPDATES)
			{
				client.self.Movement.SendUpdate(true, simulator);
			}

			return simulator;
		}
		else
		{
			// Already connected to this simulator and wasn't asked to set it as the default,
			// just return a reference to the existing object
			return simulator;
		}
	}

	/// <summary>
	/// Initiate a blocking logout request. This will return when the logout
	/// handshake has completed or when <code>Settings.LOGOUT_TIMEOUT</code>
	/// has expired and the network layer is manually shut down
	/// </summary>
	public void Logout()
	{
		final AutoResetEvent logoutEvent = new AutoResetEvent(false);
		//        EventHandler<LoggedOutEventArgs> callback = delegate(Object sender, LoggedOutEventArgs e) { logoutEvent.Set(); };
		//
		//        LoggedOut += callback;

		EventObserver<LoggedOutEventArgs> callback = new EventObserver<LoggedOutEventArgs>(){
			@Override
			public void handleEvent(Observable o, LoggedOutEventArgs arg) {
				logoutEvent.set();
			}
		};

		OnLoggedOut.addObserver(callback);

		// Send the packet requesting a clean logout
		RequestLogout();

		// Wait for a logout response. If the response is received, shutdown
		// will be fired in the callback. Otherwise we fire it manually with
		// a NetworkTimeout type
		try {
			if (!logoutEvent.waitOne(client.settings.LOGOUT_TIMEOUT))
				Shutdown(DisconnectType.NetworkTimeout);
		} catch (InterruptedException e) {
			JLogger.warn("Thread got interruped while waiting for logout event");
			Shutdown(DisconnectType.NetworkTimeout);
		}

		//        LoggedOut -= callback;
		OnLoggedOut.deleteObserver(callback);
	}

	/// <summary>
	/// Initiate the logout process. Check if logout succeeded with the
	/// <code>OnLogoutReply</code> event, and if this does not fire the
	/// <code>Shutdown()</code> function needs to be manually called
	/// </summary>
	public void RequestLogout()
	{
		// No need to run the disconnect timer any more
		if (DisconnectTimer != null)
		{
			DisconnectTimer.cancel();
			DisconnectTimer = null;
		}

		// This will catch a Logout when the client is not logged in
		if (getCurrentSim() == null || !connected)
		{
			JLogger.warn("Ignoring RequestLogout(), client is already logged out");
			return;
		}

		JLogger.info("Logging out");

		// Send a logout request to the current sim
		LogoutRequestPacket logout = new LogoutRequestPacket();
		logout.AgentData.AgentID = client.self.getAgentID();
		logout.AgentData.SessionID = client.self.getSessionID();
		SendPacket(logout);
	}

	/// <summary>
	/// Close a connection to the given simulator
	/// </summary>
	/// <param name="simulator"></param>
	/// <param name="sendCloseCircuit"></param>
	public void DisconnectSim(Simulator simulator, boolean sendCloseCircuit)
	{
		if (simulator != null)
		{
			simulator.Disconnect(sendCloseCircuit);

			// Fire the SimDisconnected event if a handler is registered
			if (OnSimDisconnected != null)
			{
				OnSimDisconnected.raiseEvent(new SimDisconnectedEventArgs(simulator, DisconnectType.NetworkTimeout));
			}

			synchronized (Simulators) {Simulators.remove(simulator);}

			if (Simulators.size() == 0) Shutdown(DisconnectType.SimShutdown);
		}
		else
		{
			JLogger.warn("DisconnectSim() called with a null Simulator reference");
		}
	}


	/// <summary>
	/// Shutdown will disconnect all the sims except for the current sim
	/// first, and then kill the connection to CurrentSim. This should only
	/// be called if the logout process times out on <code>RequestLogout</code>
	/// </summary>
	/// <param name="type">Type of shutdown</param>
	public void Shutdown(DisconnectType type)
	{
		Shutdown(type, type.toString());
	}

	/// <summary>
	/// Shutdown will disconnect all the sims except for the current sim
	/// first, and then kill the connection to CurrentSim. This should only
	/// be called if the logout process times out on <code>RequestLogout</code>
	/// </summary>
	/// <param name="type">Type of shutdown</param>
	/// <param name="message">Shutdown message</param>
	public void Shutdown(DisconnectType type, String message)
	{
		JLogger.info("NetworkManager shutdown initiated");

		// Send a CloseCircuit packet to simulators if we are initiating the disconnect
		boolean sendCloseCircuit = (type == DisconnectType.ClientInitiated || type == DisconnectType.NetworkTimeout);

		synchronized (Simulators)
		{
			// Disconnect all simulators except the current one
			for (int i = 0; i < Simulators.size(); i++)
			{
				if (Simulators.get(i) != null && Simulators.get(i) != getCurrentSim())
				{
					Simulators.get(i).Disconnect(sendCloseCircuit);

					// Fire the SimDisconnected event if a handler is registered
					if (OnSimDisconnected != null)
					{
						OnSimDisconnected.raiseEvent(new SimDisconnectedEventArgs(Simulators.get(i), type));
					}
				}
			}

			Simulators.clear();
		}

		if (getCurrentSim() != null)
		{
			// Kill the connection to the curent simulator
			getCurrentSim().Disconnect(sendCloseCircuit);

			// Fire the SimDisconnected event if a handler is registered
			if (OnSimDisconnected != null)
			{
				OnSimDisconnected.raiseEvent(new SimDisconnectedEventArgs(getCurrentSim(), type));
			}
		}

		// Clear out all of the packets that never had time to process
		//        PacketInbox.Close();
		//        PacketOutbox.Close();
		PacketInbox.clear();
		PacketOutbox.clear();

		connected = false;

		// Fire the disconnected callback
		if (OnDisconnected != null)
		{
			OnDisconnected.raiseEvent(new DisconnectedEventArgs(type, message));
		}
	}

	/// <summary>
	/// Searches through the list of currently connected simulators to find
	/// one attached to the given InetSocketAddress
	/// </summary>
	/// <param name="endPoint">InetSocketAddress of the Simulator to search for</param>
	/// <returns>A Simulator reference on success, otherwise null</returns>
	public Simulator FindSimulator(InetSocketAddress endPoint)
	{
		synchronized (Simulators)
		{
			for (int i = 0; i < Simulators.size(); i++)
			{
				if (Simulators.get(i).getIPEndPoint().equals(endPoint))
					return Simulators.get(i);
			}
		}

		return null;
	}

	public void RaisePacketSentEvent(byte[] data, int bytesSent, Simulator simulator)
	{
		if (OnPacketSent != null)
		{
			OnPacketSent.raiseEvent(new PacketSentEventArgs(data, bytesSent, simulator));
		}
	}

	/// <summary>
	/// Fire an event when an event queue connects for capabilities
	/// </summary>
	/// <param name="simulator">Simulator the event queue is attached to</param>
	public void RaiseConnectedEvent(Simulator simulator)
	{
		if (OnEventQueueRunning != null)
		{
			OnEventQueueRunning.raiseEvent(new EventQueueRunningEventArgs(simulator));
		}
	}

	private void OutgoingPacketHandler()
	{
		OutgoingPacket outgoingPacket = null;
		Simulator simulator;

		// FIXME: This is kind of ridiculous. Port the HTB code from Simian over ASAP!
		//        System.Diagnostics.Stopwatch stopwatch = new System.Diagnostics.Stopwatch();

		long notedTime = System.currentTimeMillis();
		long currentTime = notedTime;
		while (connected)
		{
			try {
				if (((outgoingPacket = PacketOutbox.poll(100, TimeUnit.MILLISECONDS))!= null))
				{
					simulator = outgoingPacket.Simulator;

					JLogger.info(String.format("Sending packet %s to %s",
							outgoingPacket.Type, simulator.getIPEndPoint()));
					
					// Very primitive rate limiting, keeps a fixed buffer of time between each packet
					currentTime = System.currentTimeMillis();

					if (currentTime - notedTime < 10)
					{
						//Logger.DebugLog(String.Format("Rate limiting, last packet was {0}ms ago", ms));
						Thread.sleep(10 - (int)(currentTime - notedTime));
					}

					simulator.SendPacketFinal(outgoingPacket);
					notedTime = System.currentTimeMillis();
				}
			} catch (InterruptedException e) {
				JLogger.warn("Outgoing Packet Thread Got Interrupted.. Leaving...\n" 
						+ Utils.getExceptionStackTraceAsString(e));
				break;
			}
		}
	}

	private void IncomingPacketHandler()
	{
		IncomingPacket incomingPacket = new IncomingPacket();
		Packet packet = null;
		Simulator simulator = null;

		while (connected)
		{
			// Reset packet to null for the check below
			packet = null;
			try {
				if (((incomingPacket = PacketInbox.poll(100, TimeUnit.MILLISECONDS))!= null))
				{
					packet = incomingPacket.packet;
					simulator = incomingPacket.simulator;
					if (packet != null)
					{
						JLogger.info(String.format("Recieved packet %s from %s",
								packet.Type, simulator.getIPEndPoint()));
						// Skip blacklisted packets
						if (UDPBlacklist.contains(packet.Type.toString()))
						{
							JLogger.info(String.format("Discarding Blacklisted packet %s from %s",
									packet.Type, simulator.getIPEndPoint()));
							return;
						}

						// Fire the callback(s), if any
						PacketEvents.RaiseEvent(packet.Type, packet, simulator);
					}
				}
			} catch (InterruptedException e) {
				JLogger.warn("Outgoing Packet Thread Got Interrupted.. Leaving...\n" 
						+ Utils.getExceptionStackTraceAsString(e));
				break;
			}
		}
	}

	private void SetCurrentSim(Simulator simulator, String seedcaps) throws Exception 
	{
		if (simulator != getCurrentSim())
		{
			Simulator oldSim = getCurrentSim();
			synchronized (Simulators) {setCurrentSim(simulator);} // getCurrentSim() is synchronized against Simulators

			simulator.SetSeedCaps(seedcaps);

			// If the current simulator changed fire the callback
			if (OnSimChanged != null && simulator != oldSim)
			{
				OnSimChanged.raiseEvent(new SimChangedEventArgs(oldSim));
			}
		}
	}

	//region Timers

	private void DisconnectTimer_Elapsed(Object obj)
	{
		if (!connected || getCurrentSim() == null)
		{
			if (DisconnectTimer != null)
			{
				DisconnectTimer.cancel();
				DisconnectTimer = null;
			}
			connected = false;
		}
		else if (getCurrentSim().DisconnectCandidate)
		{
			// The currently occupied simulator hasn't sent us any traffic in a while, shutdown
			JLogger.warn("Network timeout for the current simulator (" +
					getCurrentSim().toString() + "), logging out");

			if (DisconnectTimer != null)
			{
				DisconnectTimer.cancel();
				DisconnectTimer = null;
			}

			connected = false;

			// Shutdown the network layer
			Shutdown(DisconnectType.NetworkTimeout);
		}
		else
		{
			// Mark the current simulator as potentially disconnected each time this timer fires.
			// If the timer is fired again before any packets are received, an actual disconnect
			// sequence will be triggered
			getCurrentSim().DisconnectCandidate = true;
		}
	}

	//endregion Timers

	//region Packet Callbacks

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void LogoutReplyHandler(Object sender, PacketReceivedEventArgs e)
	{
		LogoutReplyPacket logout = (LogoutReplyPacket)e.getPacket();

		if ((logout.AgentData.SessionID.equals(client.self.getSessionID())) && (logout.AgentData.AgentID.equals(client.self.getAgentID())))
		{
			JLogger.debug("Logout reply received");

			// Deal with callbacks, if any
			if (OnLoggedOut != null)
			{
				List<UUID> itemIDs = new ArrayList<UUID>();

				for (LogoutReplyPacket.InventoryDataBlock InventoryData :logout.InventoryData)
				{
					itemIDs.add(InventoryData.ItemID);
				}

				OnLoggedOut.raiseEvent(new LoggedOutEventArgs(itemIDs));
			}

			// If we are receiving a LogoutReply packet assume this is a client initiated shutdown
			Shutdown(DisconnectType.ClientInitiated);
		}
		else
		{
			JLogger.warn("Invalid Session or Agent ID received in Logout Reply... ignoring");
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void StartPingCheckHandler(Object sender, PacketReceivedEventArgs e)
	{
		StartPingCheckPacket incomingPing = (StartPingCheckPacket)e.getPacket();
		CompletePingCheckPacket ping = new CompletePingCheckPacket();
		ping.PingID.PingID = incomingPing.PingID.PingID;
		ping.header.Reliable = false;
		// TODO: We can use OldestUnacked to correct transmission errors
		//   I don't think that's right.  As far as I can tell, the Viewer
		//   only uses this to prune its duplicate-checking buffer. -bushing

		SendPacket(ping, e.getSimulator());
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void CompletePingCheckHandler(Object sender, PacketReceivedEventArgs e)
	{
		CompletePingCheckPacket pong = (CompletePingCheckPacket)e.getPacket();
		//String retval = "Pong2: " + (Environment.TickCount - e.getSimulator().Stats.LastPingSent);
		//if ((pong.PingID.PingID - e.getSimulator().Stats.LastPingID + 1) != 0)
		//    retval += " (gap of " + (pong.PingID.PingID - e.getSimulator().Stats.LastPingID + 1) + ")";

		e.getSimulator().Stats.LastLag = Utils.getUnixTime() - e.getSimulator().Stats.LastPingSent;
		e.getSimulator().Stats.ReceivedPongs++;
		//			Client.Log(retval, Helpers.LogLevel.Info);
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void SimStatsHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (!client.settings.ENABLE_SIMSTATS)
		{
			return;
		}
		SimStatsPacket stats = (SimStatsPacket)e.getPacket();
		for (int i = 0; i < stats.Stat.length; i++)
		{
			SimStatsPacket.StatBlock s = stats.Stat[i];
			switch ((int)s.StatID)
			{
			case 0:
				e.getSimulator().Stats.Dilation = s.StatValue;
				break;
			case 1:
				e.getSimulator().Stats.FPS = Math.round(s.StatValue);
				break;
			case 2:
				e.getSimulator().Stats.PhysicsFPS = s.StatValue;
				break;
			case 3:
				e.getSimulator().Stats.AgentUpdates = s.StatValue;
				break;
			case 4:
				e.getSimulator().Stats.FrameTime = s.StatValue;
				break;
			case 5:
				e.getSimulator().Stats.NetTime = s.StatValue;
				break;
			case 6:
				e.getSimulator().Stats.OtherTime = s.StatValue;
				break;
			case 7:
				e.getSimulator().Stats.PhysicsTime = s.StatValue;
				break;
			case 8:
				e.getSimulator().Stats.AgentTime = s.StatValue;
				break;
			case 9:
				e.getSimulator().Stats.ImageTime = s.StatValue;
				break;
			case 10:
				e.getSimulator().Stats.ScriptTime = s.StatValue;
				break;
			case 11:
				e.getSimulator().Stats.Objects = Math.round(s.StatValue);
				break;
			case 12:
				e.getSimulator().Stats.ScriptedObjects = Math.round(s.StatValue);
				break;
			case 13:
				e.getSimulator().Stats.Agents = Math.round(s.StatValue);
				break;
			case 14:
				e.getSimulator().Stats.ChildAgents = Math.round(s.StatValue);
				break;
			case 15:
				e.getSimulator().Stats.ActiveScripts = Math.round(s.StatValue);
				break;
			case 16:
				e.getSimulator().Stats.LSLIPS = Math.round(s.StatValue);
				break;
			case 17:
				e.getSimulator().Stats.INPPS = Math.round(s.StatValue);
				break;
			case 18:
				e.getSimulator().Stats.OUTPPS = Math.round(s.StatValue);
				break;
			case 19:
				e.getSimulator().Stats.PendingDownloads = Math.round(s.StatValue);
				break;
			case 20:
				e.getSimulator().Stats.PendingUploads = Math.round(s.StatValue);
				break;
			case 21:
				e.getSimulator().Stats.VirtualSize = Math.round(s.StatValue);
				break;
			case 22:
				e.getSimulator().Stats.ResidentSize = Math.round(s.StatValue);
				break;
			case 23:
				e.getSimulator().Stats.PendingLocalUploads = Math.round(s.StatValue);
				break;
			case 24:
				e.getSimulator().Stats.UnackedBytes = Math.round(s.StatValue);
				break;
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void RegionHandshakeHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		RegionHandshakePacket handshake = (RegionHandshakePacket)e.getPacket();
		Simulator simulator = e.getSimulator();
		e.getSimulator().ID = handshake.RegionInfo.CacheID;

		simulator.IsEstateManager = handshake.RegionInfo.IsEstateManager;
		simulator.Name = Utils.bytesWithTrailingNullByteToString(handshake.RegionInfo.SimName);
		simulator.SimOwner = handshake.RegionInfo.SimOwner;
		simulator.TerrainBase0 = handshake.RegionInfo.TerrainBase0;
		simulator.TerrainBase1 = handshake.RegionInfo.TerrainBase1;
		simulator.TerrainBase2 = handshake.RegionInfo.TerrainBase2;
		simulator.TerrainBase3 = handshake.RegionInfo.TerrainBase3;
		simulator.TerrainDetail0 = handshake.RegionInfo.TerrainDetail0;
		simulator.TerrainDetail1 = handshake.RegionInfo.TerrainDetail1;
		simulator.TerrainDetail2 = handshake.RegionInfo.TerrainDetail2;
		simulator.TerrainDetail3 = handshake.RegionInfo.TerrainDetail3;
		simulator.TerrainHeightRange00 = handshake.RegionInfo.TerrainHeightRange00;
		simulator.TerrainHeightRange01 = handshake.RegionInfo.TerrainHeightRange01;
		simulator.TerrainHeightRange10 = handshake.RegionInfo.TerrainHeightRange10;
		simulator.TerrainHeightRange11 = handshake.RegionInfo.TerrainHeightRange11;
		simulator.TerrainStartHeight00 = handshake.RegionInfo.TerrainStartHeight00;
		simulator.TerrainStartHeight01 = handshake.RegionInfo.TerrainStartHeight01;
		simulator.TerrainStartHeight10 = handshake.RegionInfo.TerrainStartHeight10;
		simulator.TerrainStartHeight11 = handshake.RegionInfo.TerrainStartHeight11;
		simulator.WaterHeight = handshake.RegionInfo.WaterHeight;
		simulator.Flags = RegionFlags.get((int)handshake.RegionInfo.RegionFlags);
		simulator.BillableFactor = handshake.RegionInfo.BillableFactor;
		simulator.Access = SimAccess.get((short)Utils.ubyteToInt(handshake.RegionInfo.SimAccess));

		simulator.RegionID = handshake.RegionInfo2.RegionID;
		simulator.ColoLocation = Utils.bytesWithTrailingNullByteToString(handshake.RegionInfo3.ColoName);
		simulator.CPUClass = handshake.RegionInfo3.CPUClassID;
		simulator.CPURatio = handshake.RegionInfo3.CPURatio;
		simulator.ProductName = Utils.bytesWithTrailingNullByteToString(handshake.RegionInfo3.ProductName);
		simulator.ProductSku = Utils.bytesWithTrailingNullByteToString(handshake.RegionInfo3.ProductSKU);

		// Send a RegionHandshakeReply
		RegionHandshakeReplyPacket reply = new RegionHandshakeReplyPacket();
		reply.AgentData.AgentID = client.self.getAgentID();
		reply.AgentData.SessionID = client.self.getSessionID();
		reply.RegionInfo.Flags = 0;
		SendPacket(reply, simulator);

		// We're officially connected to this sim
		simulator.connected = true;
		simulator.handshakeComplete = true;
		simulator.ConnectedEvent.set();
	}

	protected void EnableSimulatorHandler(String capsKey, IMessage message, Simulator simulator) throws Exception
	{
		if (!client.settings.MULTIPLE_SIMS) return;

		EnableSimulatorMessage msg = (EnableSimulatorMessage)message;

		for (int i = 0; i < msg.Simulators.length; i++)
		{
			InetAddress ip = msg.Simulators[i].IP;
			//ushort
			int port = msg.Simulators[i].Port;
			BigInteger handle = msg.Simulators[i].RegionHandle;

			InetSocketAddress endPoint = new InetSocketAddress(ip, port);

			if (FindSimulator(endPoint) != null) return;

			if (Connect(ip, port, handle, false, null) == null)
			{
				JLogger.error("Unabled to connect to new sim " + ip + ":" + port);
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void DisableSimulatorHandler(Object sender, PacketReceivedEventArgs e)
	{
		DisconnectSim(e.getSimulator(), false);
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void KickUserHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		String message = Utils.bytesWithTrailingNullByteToString(((KickUserPacket)e.getPacket()).UserInfo.Reason);

		// Shutdown the network layer
		Shutdown(DisconnectType.ServerInitiated, message);
	}
	//endregion Packet Callbacks
}
