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
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;

import com.ngt.jopenmetaverse.shared.exception.nm.InventoryException;
import com.ngt.jopenmetaverse.shared.protocol.AcceptFriendshipPacket;
import com.ngt.jopenmetaverse.shared.protocol.ChangeUserRightsPacket;
import com.ngt.jopenmetaverse.shared.protocol.DeclineFriendshipPacket;
import com.ngt.jopenmetaverse.shared.protocol.FindAgentPacket;
import com.ngt.jopenmetaverse.shared.protocol.GenericMessagePacket;
import com.ngt.jopenmetaverse.shared.protocol.GrantUserRightsPacket;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.protocol.OfflineNotificationPacket;
import com.ngt.jopenmetaverse.shared.protocol.OnlineNotificationPacket;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.TerminateFriendshipPacket;
import com.ngt.jopenmetaverse.shared.protocol.TrackAgentPacket;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageDialog;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageOnline;
import com.ngt.jopenmetaverse.shared.sim.events.*;
import com.ngt.jopenmetaverse.shared.sim.events.am.InstantMessageEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.avm.UUIDNameReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.friends.*;
import com.ngt.jopenmetaverse.shared.sim.friends.FriendInfo;
import com.ngt.jopenmetaverse.shared.sim.friends.FriendRights;
import com.ngt.jopenmetaverse.shared.sim.login.BuddyListEntry;
import com.ngt.jopenmetaverse.shared.sim.login.LoginProgressEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseCallbackArg;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseData;
import com.ngt.jopenmetaverse.shared.sim.login.LoginStatus;
import com.ngt.jopenmetaverse.shared.types.Action;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;


/// <summary>
/// This class is used to add and remove avatars from your friends list and to manage their permission.  
/// </summary>
public class FriendsManager {

	private EventObservable<FriendInfoEventArgs> onFriendOnline = new EventObservable<FriendInfoEventArgs>();
	public void registerOnFriendOnline(EventObserver<FriendInfoEventArgs> o)
	{
		onFriendOnline.addObserver(o);
	}
	public void unregisterOnFriendOnline(EventObserver<FriendInfoEventArgs> o) 
	{
		onFriendOnline.deleteObserver(o);
	}
	private EventObservable<FriendInfoEventArgs> onFriendOffline = new EventObservable<FriendInfoEventArgs>();
	public void registerOnFriendOffline(EventObserver<FriendInfoEventArgs> o)
	{
		onFriendOffline.addObserver(o);
	}
	public void unregisterOnFriendOffline(EventObserver<FriendInfoEventArgs> o) 
	{
		onFriendOffline.deleteObserver(o);
	}
	private EventObservable<FriendInfoEventArgs> onFriendRightsUpdate = new EventObservable<FriendInfoEventArgs>();
	public void registerOnFriendRightsUpdate(EventObserver<FriendInfoEventArgs> o)
	{
		onFriendRightsUpdate.addObserver(o);
	}
	public void unregisterOnFriendRightsUpdate(EventObserver<FriendInfoEventArgs> o) 
	{
		onFriendRightsUpdate.deleteObserver(o);
	}
	private EventObservable<FriendNamesEventArgs> onFriendNames = new EventObservable<FriendNamesEventArgs>();
	public void registerOnFriendNames(EventObserver<FriendNamesEventArgs> o)
	{
		onFriendNames.addObserver(o);
	}
	public void unregisterOnFriendNames(EventObserver<FriendNamesEventArgs> o) 
	{
		onFriendNames.deleteObserver(o);
	}
	private EventObservable<FriendshipOfferedEventArgs> onFriendshipOffered = new EventObservable<FriendshipOfferedEventArgs>();
	public void registerOnFriendshipOffered(EventObserver<FriendshipOfferedEventArgs> o)
	{
		onFriendshipOffered.addObserver(o);
	}
	public void unregisterOnFriendshipOffered(EventObserver<FriendshipOfferedEventArgs> o) 
	{
		onFriendshipOffered.deleteObserver(o);
	}
	private EventObservable<FriendshipResponseEventArgs> onFriendshipResponse = new EventObservable<FriendshipResponseEventArgs>();
	public void registerOnFriendshipResponse(EventObserver<FriendshipResponseEventArgs> o)
	{
		onFriendshipResponse.addObserver(o);
	}
	public void unregisterOnFriendshipResponse(EventObserver<FriendshipResponseEventArgs> o) 
	{
		onFriendshipResponse.deleteObserver(o);
	}
	private EventObservable<FriendshipTerminatedEventArgs> onFriendshipTerminated = new EventObservable<FriendshipTerminatedEventArgs>();
	public void registerOnFriendshipTerminated(EventObserver<FriendshipTerminatedEventArgs> o)
	{
		onFriendshipTerminated.addObserver(o);
	}
	public void unregisterOnFriendshipTerminated(EventObserver<FriendshipTerminatedEventArgs> o) 
	{
		onFriendshipTerminated.deleteObserver(o);
	}
	private EventObservable<FriendFoundReplyEventArgs> onFriendFoundReply = new EventObservable<FriendFoundReplyEventArgs>();
	public void registerOnFriendFoundReply(EventObserver<FriendFoundReplyEventArgs> o)
	{
		onFriendFoundReply.addObserver(o);
	}
	public void unregisterOnFriendFoundReply(EventObserver<FriendFoundReplyEventArgs> o) 
	{
		onFriendFoundReply.deleteObserver(o);
	}

	
//        //region Delegates
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendInfoEventArgs> m_FriendOnline;
//
//        /// <summary>Raises the FriendOnline event</summary>
//        /// <param name="e">A FriendInfoEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendOnline(FriendInfoEventArgs e)
//        {
//            EventHandler<FriendInfoEventArgs> handler = m_FriendOnline;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendOnlineLock = new object();
//
//        /// <summary>Raised when the simulator sends notification one of the members in our friends list comes online</summary>
//        public event EventHandler<FriendInfoEventArgs> FriendOnline 
//        {
//            add { lock (m_FriendOnlineLock) { m_FriendOnline += value; } }
//            remove { lock (m_FriendOnlineLock) { m_FriendOnline -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendInfoEventArgs> m_FriendOffline;
//
//        /// <summary>Raises the FriendOffline event</summary>
//        /// <param name="e">A FriendInfoEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendOffline(FriendInfoEventArgs e)
//        {
//            EventHandler<FriendInfoEventArgs> handler = m_FriendOffline;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendOfflineLock = new object();
//
//        /// <summary>Raised when the simulator sends notification one of the members in our friends list goes offline</summary>
//        public event EventHandler<FriendInfoEventArgs> FriendOffline 
//        {
//            add { lock (m_FriendOfflineLock) { m_FriendOffline += value; } }
//            remove { lock (m_FriendOfflineLock) { m_FriendOffline -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendInfoEventArgs> m_FriendRights;
//
//        /// <summary>Raises the FriendRightsUpdate event</summary>
//        /// <param name="e">A FriendInfoEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendRights(FriendInfoEventArgs e)
//        {
//            EventHandler<FriendInfoEventArgs> handler = m_FriendRights;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendRightsLock = new object();
//
//        /// <summary>Raised when the simulator sends notification one of the members in our friends list grants or revokes permissions</summary>
//        public event EventHandler<FriendInfoEventArgs> FriendRightsUpdate 
//        {
//            add { lock (m_FriendRightsLock) { m_FriendRights += value; } }
//            remove { lock (m_FriendRightsLock) { m_FriendRights -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendNamesEventArgs> m_FriendNames;
//
//        /// <summary>Raises the FriendNames event</summary>
//        /// <param name="e">A FriendNamesEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendNames(FriendNamesEventArgs e)
//        {
//            EventHandler<FriendNamesEventArgs> handler = m_FriendNames;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendNamesLock = new object();
//
//        /// <summary>Raised when the simulator sends us the names on our friends list</summary>
//        public event EventHandler<FriendNamesEventArgs> FriendNames 
//        {
//            add { lock (m_FriendNamesLock) { m_FriendNames += value; } }
//            remove { lock (m_FriendNamesLock) { m_FriendNames -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendshipOfferedEventArgs> m_FriendshipOffered;
//
//        /// <summary>Raises the FriendshipOffered event</summary>
//        /// <param name="e">A FriendshipOfferedEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendshipOffered(FriendshipOfferedEventArgs e)
//        {
//            EventHandler<FriendshipOfferedEventArgs> handler = m_FriendshipOffered;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendshipOfferedLock = new object();
//
//        /// <summary>Raised when the simulator sends notification another agent is offering us friendship</summary>
//        public event EventHandler<FriendshipOfferedEventArgs> FriendshipOffered 
//        {
//            add { lock (m_FriendshipOfferedLock) { m_FriendshipOffered += value; } }
//            remove { lock (m_FriendshipOfferedLock) { m_FriendshipOffered -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendshipResponseEventArgs> m_FriendshipResponse;
//
//        /// <summary>Raises the FriendshipResponse event</summary>
//        /// <param name="e">A FriendshipResponseEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendshipResponse(FriendshipResponseEventArgs e)
//        {
//            EventHandler<FriendshipResponseEventArgs> handler = m_FriendshipResponse;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendshipResponseLock = new object();
//
//        /// <summary>Raised when a request we sent to friend another agent is accepted or declined</summary>
//        public event EventHandler<FriendshipResponseEventArgs> FriendshipResponse 
//        {
//            add { lock (m_FriendshipResponseLock) { m_FriendshipResponse += value; } }
//            remove { lock (m_FriendshipResponseLock) { m_FriendshipResponse -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendshipTerminatedEventArgs> m_FriendshipTerminated;
//
//        /// <summary>Raises the FriendshipTerminated event</summary>
//        /// <param name="e">A FriendshipTerminatedEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendshipTerminated(FriendshipTerminatedEventArgs e)
//        {
//            EventHandler<FriendshipTerminatedEventArgs> handler = m_FriendshipTerminated;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendshipTerminatedLock = new object();
//
//        /// <summary>Raised when the simulator sends notification one of the members in our friends list has terminated 
//        /// our friendship</summary>
//        public event EventHandler<FriendshipTerminatedEventArgs> FriendshipTerminated 
//        {
//            add { lock (m_FriendshipTerminatedLock) { m_FriendshipTerminated += value; } }
//            remove { lock (m_FriendshipTerminatedLock) { m_FriendshipTerminated -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<FriendFoundReplyEventArgs> m_FriendFound;
//
//        /// <summary>Raises the FriendFoundReply event</summary>
//        /// <param name="e">A FriendFoundReplyEventArgs object containing the
//        /// data returned from the data server</param>
//        protected virtual void OnFriendFoundReply(FriendFoundReplyEventArgs e)
//        {
//            EventHandler<FriendFoundReplyEventArgs> handler = m_FriendFound;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_FriendFoundLock = new object();
//
//        /// <summary>Raised when the simulator sends the location of a friend we have 
//        /// requested map location info for</summary>
//        public event EventHandler<FriendFoundReplyEventArgs> FriendFoundReply 
//        {
//            add { lock (m_FriendFoundLock) { m_FriendFound += value; } }
//            remove { lock (m_FriendFoundLock) { m_FriendFound -= value; } }
//        }
//
//        //endregion Delegates
//
//        //region Events
//
//        //endregion Events
//
        private GridClient Client;
        /// <summary>
        /// A dictionary of key/value pairs containing known friends of this avatar. 
        /// 
        /// The Key is the <seealso cref="UUID"/> of the friend, the value is a <seealso cref="FriendInfo"/>
        /// object that contains detailed information including permissions you have and have given to the friend
        /// </summary>
        public InternalDictionary<UUID, FriendInfo> FriendList = new InternalDictionary<UUID, FriendInfo>();

        /// <summary>
        /// A Dictionary of key/value pairs containing current pending frienship offers.
        /// 
        /// The key is the <seealso cref="UUID"/> of the avatar making the request, 
        /// the value is the <seealso cref="UUID"/> of the request which is used to accept
        /// or decline the friendship offer
        /// </summary>
        public InternalDictionary<UUID, UUID> FriendRequests = new InternalDictionary<UUID, UUID>();

        /// <summary>
        /// Internal constructor
        /// </summary>
        /// <param name="client">A reference to the GridClient Object</param>
        public FriendsManager(GridClient client)
        {
        	Client = client;

        	// Client.network.LoginProgress += Network_OnConnect;            
        	Client.network.RegisterLoginProgressCallback(new EventObserver<LoginProgressEventArgs>()
        			{
        		@Override
        		public void handleEvent(Observable o, LoginProgressEventArgs e) {
        			Network_OnConnect(o, e);
        		}});

        	// Client.avatars.UUIDNameReply += new EventHandler<UUIDNameReplyEventArgs>(Avatars_OnAvatarNames);
        	Client.avatars.registerOnUUIDNameReply(new EventObserver<UUIDNameReplyEventArgs>()
        			{
        		@Override
        		public void handleEvent(Observable sender,
        				UUIDNameReplyEventArgs arg) {
        			Avatars_OnAvatarNames(sender, arg);
        		}
        			}
        			);

        	// Client.self.IM += Self_IM;
        	Client.self.registerIM(new EventObserver<InstantMessageEventArgs>()
        			{
        		@Override
        		public void handleEvent(Observable o,
        				InstantMessageEventArgs arg) {
        			try {
        				Self_IM(o, arg);}
        			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        		}
        			});

        	// Client.network.RegisterCallback(PacketType.OnlineNotification, OnlineNotificationHandler);
        	Client.network.RegisterCallback(PacketType.OnlineNotification, new EventObserver<PacketReceivedEventArgs>()
        			{ 
        		@Override
        		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        			try{ OnlineNotificationHandler(o, arg);}
        			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        		}}
        			);
        	// Client.network.RegisterCallback(PacketType.OfflineNotification, OfflineNotificationHandler);

        	Client.network.RegisterCallback(PacketType.OfflineNotification, new EventObserver<PacketReceivedEventArgs>()
        			{ 
        		@Override
        		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        			try{ OfflineNotificationHandler(o, arg);}
        			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        		}}
        			);
        	// Client.network.RegisterCallback(PacketType.ChangeUserRights, ChangeUserRightsHandler);

        	Client.network.RegisterCallback(PacketType.ChangeUserRights, new EventObserver<PacketReceivedEventArgs>()
        			{ 
        		@Override
        		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        			try{ ChangeUserRightsHandler(o, arg);}
        			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        		}}
        			);
        	// Client.network.RegisterCallback(PacketType.TerminateFriendship, TerminateFriendshipHandler);

        	Client.network.RegisterCallback(PacketType.TerminateFriendship, new EventObserver<PacketReceivedEventArgs>()
        			{ 
        		@Override
        		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        			try{ TerminateFriendshipHandler(o, arg);}
        			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        		}}
        			);
        	// Client.network.RegisterCallback(PacketType.FindAgent, OnFindAgentReplyHandler);

        	Client.network.RegisterCallback(PacketType.FindAgent, new EventObserver<PacketReceivedEventArgs>()
        			{ 
        		@Override
        		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        			try{ OnFindAgentReplyHandler(o, arg);}
        			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        		}}
        			);

        	// Client.network.RegisterLoginResponseCallback(new NetworkManager.LoginResponseCallback(Network_OnLoginResponse),
        	//                new string[] { "buddy-list" });            
        	Client.network.RegisterLoginResponseCallback(new EventObserver<LoginResponseCallbackArg>()
        			{
        		public void handleEvent(Observable arg0, LoginResponseCallbackArg arg1) {
        			LoginResponseCallbackArg obj = (LoginResponseCallbackArg)arg1;
        			Network_OnLoginResponse(obj.isLoginSuccess(), obj.isRedirect(), 
        					obj.getMessage(), obj.getReason(), obj.getReplyData());
        		}	
        			}, new String[] { "buddy-list" });
        }

        //region Public Methods

        /// <summary>
        /// Accept a friendship request
        /// </summary>
        /// <param name="fromAgentID">agentID of avatatar to form friendship with</param>
        /// <param name="imSessionID">imSessionID of the friendship request message</param>
        public void AcceptFriendship(UUID fromAgentID, UUID imSessionID) throws InventoryException
        {
        	UUID callingCardFolder = Client.inventory.FindFolderForType(AssetType.CallingCard);

            AcceptFriendshipPacket request = new AcceptFriendshipPacket();
            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.TransactionBlock.TransactionID = imSessionID;
            request.FolderData = new AcceptFriendshipPacket.FolderDataBlock[1];
            request.FolderData[0] = new AcceptFriendshipPacket.FolderDataBlock();
            request.FolderData[0].FolderID = callingCardFolder;

            Client.network.SendPacket(request);

            FriendInfo friend = new FriendInfo(fromAgentID, FriendRights.get(FriendRights.CanSeeOnline.getIndex()),
            		FriendRights.get(FriendRights.CanSeeOnline.getIndex()));

            if (!FriendList.containsKey(fromAgentID))
                FriendList.add(friend.getUUID(), friend);

            if (FriendRequests.containsKey(fromAgentID))
                FriendRequests.remove(fromAgentID);

            Client.avatars.RequestAvatarName(fromAgentID);
        }

        /// <summary>
        /// Decline a friendship request
        /// </summary>
        /// <param name="fromAgentID"><seealso cref="UUID"/> of friend</param>
        /// <param name="imSessionID">imSessionID of the friendship request message</param>
        public void DeclineFriendship(UUID fromAgentID, UUID imSessionID)
        {
            DeclineFriendshipPacket request = new DeclineFriendshipPacket();
            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.TransactionBlock.TransactionID = imSessionID;
            Client.network.SendPacket(request);

            if (FriendRequests.containsKey(fromAgentID))
                FriendRequests.remove(fromAgentID);
        }

        /// <summary>
        /// Overload: Offer friendship to an avatar.
        /// </summary>
        /// <param name="agentID">System ID of the avatar you are offering friendship to</param>
        public void OfferFriendship(UUID agentID)
        {
            OfferFriendship(agentID, "Do ya wanna be my buddy?");
        }

        /// <summary>
        /// Offer friendship to an avatar.
        /// </summary>
        /// <param name="agentID">System ID of the avatar you are offering friendship to</param>
        /// <param name="message">A message to send with the request</param>
        public void OfferFriendship(UUID agentID, String message)
        {
            Client.self.InstantMessage(Client.self.getName(),
                agentID,
                message,
                UUID.Random(),
                InstantMessageDialog.FriendshipOffered,
                InstantMessageOnline.Offline,
                Client.self.getSimPosition(),
                Client.network.getCurrentSim().ID,
                null);
        }


        /// <summary>
        /// Terminate a friendship with an avatar
        /// </summary>
        /// <param name="agentID">System ID of the avatar you are terminating the friendship with</param>
        public void TerminateFriendship(UUID agentID)
        {
            if (FriendList.containsKey(agentID))
            {
                TerminateFriendshipPacket request = new TerminateFriendshipPacket();
                request.AgentData.AgentID = Client.self.getAgentID();
                request.AgentData.SessionID = Client.self.getSessionID();
                request.ExBlock.OtherID = agentID;

                Client.network.SendPacket(request);

                if (FriendList.containsKey(agentID))
                    FriendList.remove(agentID);
            }
        }
        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        private void TerminateFriendshipHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();
            TerminateFriendshipPacket itsOver = (TerminateFriendshipPacket)packet;
            String name = "";

            if (FriendList.containsKey(itsOver.ExBlock.OtherID))
            {
                name = FriendList.get(itsOver.ExBlock.OtherID).getName();
                FriendList.remove(itsOver.ExBlock.OtherID);
            }

            if (onFriendshipTerminated != null)
            {
                onFriendshipTerminated.raiseEvent(new FriendshipTerminatedEventArgs(itsOver.ExBlock.OtherID, name));
            }
        }

        /// <summary>
        /// Change the rights of a friend avatar.
        /// </summary>
        /// <param name="friendID">the <seealso cref="UUID"/> of the friend</param>
        /// <param name="rights">the new rights to give the friend</param>
        /// <remarks>This method will implicitly set the rights to those passed in the rights parameter.</remarks>
        public void GrantRights(UUID friendID, EnumSet<FriendRights> rights)
        {
            GrantUserRightsPacket request = new GrantUserRightsPacket();
            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.Rights = new GrantUserRightsPacket.RightsBlock[1];
            request.Rights[0] = new GrantUserRightsPacket.RightsBlock();
            request.Rights[0].AgentRelated = friendID;
            request.Rights[0].RelatedRights = FriendRights.getIndex(rights);

            Client.network.SendPacket(request);
        }

        /// <summary>
        /// Use to map a friends location on the grid.
        /// </summary>
        /// <param name="friendID">Friends UUID to find</param>
        /// <remarks><seealso cref="E:OnFriendFound"/></remarks>
        public void MapFriend(UUID friendID)
        {
            FindAgentPacket stalk = new FindAgentPacket();
            stalk.AgentBlock.Hunter = Client.self.getAgentID();
            stalk.AgentBlock.Prey = friendID;
            stalk.AgentBlock.SpaceIP = 0; // Will be filled in by the simulator
            stalk.LocationBlock = new FindAgentPacket.LocationBlockBlock[1];
            stalk.LocationBlock[0] = new FindAgentPacket.LocationBlockBlock();
            stalk.LocationBlock[0].GlobalX = 0.0; // Filled in by the simulator
            stalk.LocationBlock[0].GlobalY = 0.0;

            Client.network.SendPacket(stalk);
        }

        /// <summary>
        /// Use to track a friends movement on the grid
        /// </summary>
        /// <param name="friendID">Friends Key</param>
        public void TrackFriend(UUID friendID)
        {
            TrackAgentPacket stalk = new TrackAgentPacket();
            stalk.AgentData.AgentID = Client.self.getAgentID();
            stalk.AgentData.SessionID = Client.self.getSessionID();
            stalk.TargetData.PreyID = friendID;

            Client.network.SendPacket(stalk);
        }

        /// <summary>
        /// Ask for a notification of friend's online status
        /// </summary>
        /// <param name="friendID">Friend's UUID</param>
        public void RequestOnlineNotification(UUID friendID)
        {
            GenericMessagePacket gmp = new GenericMessagePacket();

            gmp.AgentData.AgentID = Client.self.getAgentID();
            gmp.AgentData.SessionID = Client.self.getSessionID();
            gmp.AgentData.TransactionID = UUID.Zero;

            gmp.MethodData.Method = Utils.stringToBytesWithTrailingNullByte("requestonlinenotification");
            gmp.MethodData.Invoice = UUID.Zero;
            gmp.ParamList = new GenericMessagePacket.ParamListBlock[1];
            gmp.ParamList[0] = new GenericMessagePacket.ParamListBlock();
            gmp.ParamList[0].Parameter = Utils.stringToBytesWithTrailingNullByte(friendID.toString());

            Client.network.SendPacket(gmp);
        }

        //endregion

        //region Internal events

        private void Network_OnConnect(Object sender, LoginProgressEventArgs e)
        {
            if (e.getStatus() != LoginStatus.Success)
            {
                return;
            }

            final List<UUID> names = new ArrayList<UUID>();

            if (FriendList.getCount() > 0)
            {
            	FriendList.foreach(new Action<Entry<UUID, FriendInfo>>()
            	{
					public void execute(Entry<UUID, FriendInfo> kvp) {
                        if (Utils.isNullOrEmpty(kvp.getValue().getName()))
                            names.add(kvp.getKey());
					}
            	}
            	);
                Client.avatars.RequestAvatarNames(names);
            }
        }


        /// <summary>
        /// This handles the asynchronous response of a RequestAvatarNames call.
        /// </summary>
        /// <param name="sender"></param>
        /// <param name="e">names cooresponding to the the list of IDs sent the the RequestAvatarNames call.</param>
        private void Avatars_OnAvatarNames(Object sender, UUIDNameReplyEventArgs e)
        {
            Map<UUID, String> newNames = new HashMap<UUID, String>();

            for (Entry<UUID, String> kvp : e.getNames().entrySet())
            {
                FriendInfo friend;
                synchronized (FriendList.getDictionary())
                {
                    if ((friend = FriendList.get(kvp.getKey()))!=null)
                    {
                        if (friend.getName() == null)
                            newNames.put(kvp.getKey(), kvp.getValue());

                        friend.setName(kvp.getValue());
                        FriendList.add(kvp.getKey(), friend);
                    }
                }
            }

            if (newNames.size() > 0 && onFriendNames != null)
            {
                onFriendNames.raiseEvent(new FriendNamesEventArgs(newNames));
            }
        }
        //endregion

        //region Packet Handlers

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void OnlineNotificationHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();
            if (packet.Type == PacketType.OnlineNotification)
            {
                OnlineNotificationPacket notification = ((OnlineNotificationPacket)packet);

                for (OnlineNotificationPacket.AgentBlockBlock block : notification.AgentBlock)
                {
                    FriendInfo friend;
                    synchronized (FriendList.getDictionary())
                    {
                        if (!FriendList.containsKey(block.AgentID))
                        {
                            friend = new FriendInfo(block.AgentID, FriendRights.get(FriendRights.CanSeeOnline.getIndex()),
                            		FriendRights.get(FriendRights.CanSeeOnline.getIndex()));
                            FriendList.add(block.AgentID, friend);
                        }
                        else
                        {
                            friend = FriendList.get(block.AgentID);
                        }
                    }

                    boolean doNotify = !friend.isOnline();
                    friend.setIsOnline(true);

                    if (onFriendOnline != null && doNotify)
                    {
                        onFriendOnline.raiseEvent(new FriendInfoEventArgs(friend));
                    }
                }
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void OfflineNotificationHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();
            if (packet.Type == PacketType.OfflineNotification)
            {
                OfflineNotificationPacket notification = (OfflineNotificationPacket)packet;

                for (OfflineNotificationPacket.AgentBlockBlock block : notification.AgentBlock)
                {
                    FriendInfo friend = new FriendInfo(block.AgentID, FriendRights.get(FriendRights.CanSeeOnline.getIndex()), 
                    		FriendRights.get(FriendRights.CanSeeOnline.getIndex()));

                    synchronized (FriendList.getDictionary())
                    {
                        if (!FriendList.containsKey(block.AgentID))
                            FriendList.add(block.AgentID, friend);

                        friend = FriendList.get(block.AgentID);
                    }

                    friend.setIsOnline(false);

                    if (onFriendOffline != null)
                    {
                        onFriendOffline.raiseEvent(new FriendInfoEventArgs(friend));
                    }
                }
            }
        }


        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        private void ChangeUserRightsHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();
            if (packet.Type == PacketType.ChangeUserRights)
            {
                FriendInfo friend;
                ChangeUserRightsPacket rights = (ChangeUserRightsPacket)packet;

                for (ChangeUserRightsPacket.RightsBlock block : rights.Rights)
                {
                    EnumSet<FriendRights> newRights = FriendRights.get(block.RelatedRights);
                    if ((friend = FriendList.get(block.AgentRelated) )!= null)
                    {
                        friend.setTheirFriendRights(newRights);
                        if (onFriendRightsUpdate != null)
                        {
                        	onFriendRightsUpdate.raiseEvent(new FriendInfoEventArgs(friend));
                        }
                    }
                    else if (block.AgentRelated.equals(Client.self.getAgentID()))
                    {
                        if (( friend = FriendList.get(rights.AgentData.AgentID))!=null)
                        {
                            friend.setMyFriendRights(newRights);
                            if (onFriendRightsUpdate != null)
                            {
                            	onFriendRightsUpdate.raiseEvent(new FriendInfoEventArgs(friend));
                            }
                        }
                    }
                }
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        public void OnFindAgentReplyHandler(Object sender, PacketReceivedEventArgs e)
        {
            if (onFriendFoundReply != null)
            {
                Packet packet = e.getPacket();
                FindAgentPacket reply = (FindAgentPacket)packet;

                float[] xy = new float[2];
                UUID prey = reply.AgentBlock.Prey;
                BigInteger regionHandle = Helpers.GlobalPosToRegionHandle((float)reply.LocationBlock[0].GlobalX,
                    (float)reply.LocationBlock[0].GlobalY, xy);
                Vector3 xyz = new Vector3(xy[0], xy[0], 0f);

                onFriendFoundReply.raiseEvent(new FriendFoundReplyEventArgs(prey, regionHandle, xyz));
            }
        }

        //endregion

        private void Self_IM(Object sender, InstantMessageEventArgs e)
        {
            if (e.getIM().Dialog == InstantMessageDialog.FriendshipOffered)
            {
                if (onFriendshipOffered != null)
                {
                    if (FriendRequests.containsKey(e.getIM().FromAgentID))
                        FriendRequests.add(e.getIM().FromAgentID,  e.getIM().IMSessionID);
                    else
                        FriendRequests.add(e.getIM().FromAgentID, e.getIM().IMSessionID);

                    onFriendshipOffered.raiseEvent(new FriendshipOfferedEventArgs(e.getIM().FromAgentID, e.getIM().FromAgentName, e.getIM().IMSessionID));
                }
            }
            else if (e.getIM().Dialog == InstantMessageDialog.FriendshipAccepted)
            {
                FriendInfo friend = new FriendInfo(e.getIM().FromAgentID, FriendRights.get(FriendRights.CanSeeOnline.getIndex()),
                		FriendRights.get(FriendRights.CanSeeOnline.getIndex()));
                friend.setName( e.getIM().FromAgentName);
                synchronized (FriendList.getDictionary()) 
                {FriendList.add(friend.getUUID(), friend);}

                if (onFriendshipResponse != null)
                {
                    onFriendshipResponse.raiseEvent(new FriendshipResponseEventArgs(e.getIM().FromAgentID, e.getIM().FromAgentName, true));
                }
                RequestOnlineNotification(e.getIM().FromAgentID);
            }
            else if (e.getIM().Dialog == InstantMessageDialog.FriendshipDeclined)
            {
                if (onFriendshipResponse != null)
                {
                    onFriendshipResponse.raiseEvent(new FriendshipResponseEventArgs(e.getIM().FromAgentID, e.getIM().FromAgentName, false));
                }
            }
        }

        /// <summary>
        /// Populate FriendList <seealso cref="InternalDictionary"/> with data from the login reply
        /// </summary>
        /// <param name="loginSuccess">true if login was successful</param>
        /// <param name="redirect">true if login request is requiring a redirect</param>
        /// <param name="message">A String containing the response to the login request</param>
        /// <param name="reason">A String containing the reason for the request</param>
        /// <param name="replyData">A <seealso cref="LoginResponseData"/> object containing the decoded 
        /// reply from the login server</param>
        private void Network_OnLoginResponse(boolean loginSuccess, boolean redirect, String message, String reason,
            LoginResponseData replyData)
        {
            int uuidLength = UUID.Zero.toString().length();

            if (loginSuccess && replyData.BuddyList != null)
            {
                for (BuddyListEntry buddy : replyData.BuddyList)
                {
                    UUID bubid;
                    String id = buddy.buddy_id.length() > uuidLength ? buddy.buddy_id.substring(0, uuidLength) : buddy.buddy_id;
                    if ((bubid = UUID.Parse(id))!=null)
                    {
                        synchronized (FriendList.getDictionary())
                        {
                            if (!FriendList.containsKey(bubid))
                            {
                                FriendList.add(bubid, new FriendInfo(bubid, 
                                    FriendRights.get(buddy.buddy_rights_given),
                                    FriendRights.get(buddy.buddy_rights_has)));
                            }
                        }
                    }
                }
            }
        }
    }