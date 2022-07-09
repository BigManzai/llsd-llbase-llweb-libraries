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
package com.ngt.jopenmetaverse.shared.sim.events;

import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.Observable;
import java.util.Observer;

import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.cap.http.EventQueueClient;
import com.ngt.jopenmetaverse.shared.cap.http.EventQueueClientEventObservableArg;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.sim.NetworkManager;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.message.MessageUtils;
import com.ngt.jopenmetaverse.shared.sim.stats.UtilizationStatistics.Type;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// Capabilities is the name of the bi-directional HTTP REST protocol
/// used to communicate non real-time transactions such as teleporting or
/// group messaging
/// </summary>
public class Caps
{
	/// <summary>
	/// Triggered when an event is received via the EventQueueGet 
	/// capability
	/// </summary>
	/// <param name="capsKey">Event name</param>
	/// <param name="message">Decoded event data</param>
	/// <param name="simulator">The simulator that generated the event</param>
	//public delegate void EventQueueCallback(string message, StructuredData.OSD body, Simulator simulator);

//	public delegate void EventQueueCallback(string capsKey, IMessage message, Simulator simulator);

	protected EventObservable<CapsEventObservableArg> eventQueueObservable;


	public void addEventQueueObserver(EventObserver<CapsEventObservableArg> o)
	{
		eventQueueObservable.addObserver(o);
	}

	public void removeEventQueueObserver(EventObserver<CapsEventObservableArg> o)
	{
		eventQueueObservable.deleteObserver(o);
	}	        
	

	public EventObservable<CapsEventObservableArg> getEventQueueObservable() {
		return eventQueueObservable;
	}

	public void setEventQueueObservable(
			EventObservable<CapsEventObservableArg> eventQueueObservable) {
		this.eventQueueObservable = eventQueueObservable;
	}


	/// <summary>Reference to the simulator this system is connected to</summary>
	public Simulator Simulator;

	public String _SeedCapsURI;
	public Map<String, URI> _Caps = new HashMap<String, URI>();

	private CapsHttpClient _SeedRequest;
	private EventQueueClient _EventQueueCap = null;

	/// <summary>Capabilities URI this system was initialized with</summary>
	public String getSeedCapsURI() {  return _SeedCapsURI; } 

	/// <summary>Whether the capabilities event queue is connected and
	/// listening for incoming events</summary>
	public boolean isEventQueueRunning()
	{
		if (_EventQueueCap != null)
			return _EventQueueCap.getRunning();
		else
			return false;
	}

	/// <summary>
	/// Default constructor
	/// </summary>
	/// <param name="simulator"></param>
	/// <param name="seedcaps"></param>
	public Caps(Simulator simulator, String seedcaps) throws Exception
	{
		Simulator = simulator;
		_SeedCapsURI = seedcaps;

		MakeSeedRequest();
	}

	public void Disconnect(boolean immediate)
	{
		JLogger.info(String.format("Caps system for %s is %s", Simulator,
				(immediate ? "aborting" : "disconnecting")));

		if (_SeedRequest != null)
			_SeedRequest.Cancel();

		if (_EventQueueCap != null)
			_EventQueueCap.Stop(immediate);
	}

	/// <summary>
	/// Request the URI of a named capability
	/// </summary>
	/// <param name="capability">Name of the capability to request</param>
	/// <returns>The URI of the requested capability, or String.Empty if
	/// the capability does not exist</returns>
	public URI CapabilityURI(String capability)
	{
		URI cap;

		if ((cap = _Caps.get(capability))!=null)
			return cap;
		else
			return null;
	}

	private void MakeSeedRequest() throws Exception 
	{
		if (Simulator == null || !Simulator.Client.network.getConnected())
			return;

		// Create a request list
		OSDArray req = new OSDArray();
		// This list can be updated by using the following command to obtain a current list of capabilities the official linden viewer supports:
		// wget -q -O - http://bitbucket.org/lindenlab/viewer-development/raw/default/indra/newview/llviewerregion.cpp | grep 'capabilityNames.append'  | sed 's/^[ \t]*//;s/capabilityNames.append("/req.add(OSD.FromString("/'
		req.add(OSD.FromString("AttachmentResources"));
		req.add(OSD.FromString("AvatarPickerSearch"));
		req.add(OSD.FromString("ChatSessionRequest"));
		req.add(OSD.FromString("CopyInventoryFromNotecard"));
		req.add(OSD.FromString("DispatchRegionInfo"));
		req.add(OSD.FromString("EstateChangeInfo"));
		req.add(OSD.FromString("EventQueueGet"));
		req.add(OSD.FromString("ObjectMedia"));
		req.add(OSD.FromString("ObjectMediaNavigate"));
		req.add(OSD.FromString("FetchLib2"));
		req.add(OSD.FromString("FetchLibDescendents2"));
		req.add(OSD.FromString("FetchInventory2"));
		req.add(OSD.FromString("FetchInventoryDescendents2"));
		req.add(OSD.FromString("GetDisplayNames"));
		req.add(OSD.FromString("GetTexture"));
		req.add(OSD.FromString("GetMesh"));
		req.add(OSD.FromString("GetObjectCost"));
		req.add(OSD.FromString("GetObjectPhysicsData"));
		req.add(OSD.FromString("GroupProposalBallot"));
		req.add(OSD.FromString("HomeLocation"));
		req.add(OSD.FromString("LandResources"));
		req.add(OSD.FromString("MapLayer"));
		req.add(OSD.FromString("MapLayerGod"));
		req.add(OSD.FromString("NewFileAgentInventory"));
		req.add(OSD.FromString("NewFileAgentInventoryVariablePrice"));
		req.add(OSD.FromString("ObjectAdd"));
		req.add(OSD.FromString("ParcelPropertiesUpdate"));
		req.add(OSD.FromString("ParcelMediaURLFilterList"));
		req.add(OSD.FromString("ParcelNavigateMedia"));
		req.add(OSD.FromString("ParcelVoiceInfoRequest"));
		req.add(OSD.FromString("ProductInfoRequest"));
		req.add(OSD.FromString("ProvisionVoiceAccountRequest"));
		req.add(OSD.FromString("RemoteParcelRequest"));
		req.add(OSD.FromString("RequestTextureDownload"));
		req.add(OSD.FromString("SearchStatRequest"));
		req.add(OSD.FromString("SearchStatTracking"));
		req.add(OSD.FromString("SendPostcard"));
		req.add(OSD.FromString("SendUserReport"));
		req.add(OSD.FromString("SendUserReportWithScreenshot"));
		req.add(OSD.FromString("ServerReleaseNotes"));
		req.add(OSD.FromString("SimConsole"));
		req.add(OSD.FromString("SimulatorFeatures"));
		req.add(OSD.FromString("SetDisplayName"));
		req.add(OSD.FromString("SimConsoleAsync"));
		req.add(OSD.FromString("StartGroupProposal"));
		req.add(OSD.FromString("TextureStats"));
		req.add(OSD.FromString("UntrustedSimulatorMessage"));
		req.add(OSD.FromString("UpdateAgentInformation"));
		req.add(OSD.FromString("UpdateAgentLanguage"));
		req.add(OSD.FromString("UpdateGestureAgentInventory"));
		req.add(OSD.FromString("UpdateNotecardAgentInventory"));
		req.add(OSD.FromString("UpdateScriptAgent"));
		req.add(OSD.FromString("UpdateGestureTaskInventory"));
		req.add(OSD.FromString("UpdateNotecardTaskInventory"));
		req.add(OSD.FromString("UpdateScriptTask"));
		req.add(OSD.FromString("UploadBakedTexture"));
		req.add(OSD.FromString("UploadObjectAsset"));
		req.add(OSD.FromString("ViewerMetrics"));
		req.add(OSD.FromString("ViewerStartAuction"));
		req.add(OSD.FromString("ViewerStats"));

		_SeedRequest = new CapsHttpClient(new URI(_SeedCapsURI));
		_SeedRequest.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
				{
			public void handleEvent(Observable arg0, CapsHttpRequestCompletedArg arg1) {
				CapsHttpRequestCompletedArg obj1= (CapsHttpRequestCompletedArg) arg1;
				SeedRequestCompleteHandler(_SeedRequest, obj1.getResult(), obj1.getError());
			}
		});
		//	            _SeedRequest.OnComplete += new CapsHttpClient.CompleteCallback(SeedRequestCompleteHandler);
		_SeedRequest.BeginGetResponse(req, OSDFormat.Xml, Simulator.Client.settings.CAPS_TIMEOUT);
	}

	private void SeedRequestCompleteHandler(CapsHttpClient client, OSD result, Exception error)
	{
		if (result != null && result.getType().equals(OSDType.Map))
		{
			OSDMap respTable = (OSDMap)result;

			for (String cap : respTable.keys())
			{
				_Caps.put(cap, respTable.get(cap).asUri());
			}

			if (_Caps.containsKey("EventQueueGet"))
			{
				JLogger.debug("Starting event queue for " + Simulator.toString());

				_EventQueueCap = new EventQueueClient(_Caps.get("EventQueueGet"));

				_EventQueueCap.registerConnectedObserver(new Observer(){
					public void update(Observable o, Object arg) {
						EventQueueConnectedHandler();
					}

				});
				_EventQueueCap.registerEventObserver(new Observer(){
					public void update(Observable o, Object arg) {
						EventQueueClientEventObservableArg obj = (EventQueueClientEventObservableArg) arg;
						EventQueueEventHandler(obj.getEventName(), obj.getBody());
					}

				});

				//	                    _EventQueueCap.OnConnected += EventQueueConnectedHandler;
				//	                    _EventQueueCap.OnEvent += EventQueueEventHandler;
				try {
					_EventQueueCap.Start();
				} catch (Exception e) {
					JLogger.error("Error Encountered while starting EventQueueCap" +  Utils.getExceptionStackTraceAsString(e));
				}
			}
		}
		//TODO Handle retrying if the error is recoverable
//		else if (
//				error != null &&
//				error is WebException &&
//				((WebException)error).Response != null &&
//				((HttpWebResponse)((WebException)error).Response).StatusCode == HttpStatusCode.NotFound)
//		{
//			// 404 error
//			Logger.Log("Seed capability returned a 404, capability system is aborting", Helpers.LogLevel.Error);
//		}
//		else
//		{
//			// The initial CAPS connection failed, try again
//			MakeSeedRequest();
//		}
	}

	private void EventQueueConnectedHandler()
	{
		Simulator.Client.network.RaiseConnectedEvent(Simulator);
	}

	/// <summary>
	/// Process any incoming events, check to see if we have a message created for the event, 
	/// </summary>
	/// <param name="eventName"></param>
	/// <param name="body"></param>
	private void EventQueueEventHandler(String eventName, OSDMap body)
	{
		IMessage message = MessageUtils.DecodeEvent(eventName, body);
		if (message != null)
		{
			Simulator.Client.network.getCapsEvents().BeginRaiseEvent(eventName, message, Simulator);

			//region Stats Tracking
			if (Simulator.Client.settings.TRACK_UTILIZATION)
			{
				Simulator.Client.stats.Update(eventName, Type.Message, 0, body.toString().length());
			}
			//endregion
		}
		else
		{
			JLogger.warn("No Message handler exists for event " + eventName + ". Unable to decode. Will try Generic Handler next");
			JLogger.debug("Please report this information to http://jira.openmv.org/: \n" + body);

			// try generic decoder next which takes a caps event and tries to match it to an existing packet
			if (body.getType().equals(OSDType.Map))
			{
				OSDMap map = (OSDMap)body;
				Packet packet;
				try {
					packet = Packet.BuildPacket(eventName, map);
				} catch (Exception e) {
					JLogger.warn("Error in building packet for event " 
				+ eventName + Utils.getExceptionStackTraceAsString(e));
					packet = null;
				}
				
				if (packet != null)
				{
					NetworkManager.IncomingPacket incomingPacket = new NetworkManager.IncomingPacket();
					incomingPacket.simulator = Simulator;
					incomingPacket.packet = packet;

					JLogger.debug("Serializing " + packet.Type.toString() + " capability with generic handler");

					Simulator.Client.network.PacketInbox.add(incomingPacket);
				}
				else
				{
					JLogger.warn("No Packet or Message handler exists for " + eventName);
				}
			}
		}
	}        
}
