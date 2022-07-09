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

import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.*;

public class MessageUtils 
{
	public static InetAddress ToIP(OSD osd) throws UnknownHostException
    {
        byte[] binary = osd.asBinary();
        if (binary != null && binary.length == 4)
            return Inet4Address.getByAddress(binary);
        else
            return Inet4Address.getByName("*");
    }

    public static OSD FromIP(InetAddress address)
    {
        if (address != null && address.isAnyLocalAddress())
            return OSD.FromBinary(address.getAddress());
        else
            return new OSD();
    }

    public static Map<String, String> ToDictionaryString(OSD osd)
    {
        if (osd.getType().equals(OSDType.Map))
        {
            OSDMap map = (OSDMap)osd;
            Map<String, String> dict = new HashMap<String, String>(map.count());
            for(Entry<String, OSD> entry : map.entrySet())
                dict.put(entry.getKey(), entry.getValue().asString());
            return dict;
        }

        return new HashMap<String, String>(0);
    }

    public static Map<URI, URI> ToDictionaryUri(OSD osd) throws URISyntaxException
    {
        if (osd.getType().equals(OSDType.Map))
        {
            OSDMap map = (OSDMap)osd;
            Map<URI, URI> dict = new HashMap<URI, URI>(map.count());
            for(Entry<String, OSD> entry : map.entrySet())
                dict.put(new URI(entry.getKey()), entry.getValue().asUri());
            return dict;
        }

        return new HashMap<URI, URI>(0);
    }

    public static OSDMap FromDictionaryString(Map<String, String> dict)
    {
        if (dict != null)
        {
            OSDMap map = new OSDMap(dict.size());
            for(Entry<String, String> entry : dict.entrySet())
                map.put(entry.getKey(), OSD.FromString(entry.getValue()));
            return map;
        }

        return new OSDMap(0);
    }

    public static OSDMap FromDictionaryUri(Map<URI, URI> dict)
    {
        if (dict != null)
        {
            OSDMap map = new OSDMap(dict.size());
            for (Entry<URI, URI> entry : dict.entrySet())
                map.put(entry.getKey().toString(), OSD.FromUri(entry.getValue()));
            return map;
        }

        return new OSDMap(0);
    }
    
    /// <summary>
    /// Return a decoded capabilities message as a strongly typed object
    /// </summary>
    /// <param name=eventName>A string containing the name of the capabilities message key</param>
    /// <param name=map>An <see cref=OSDMap/> to decode</param>
    /// <returns>A strongly typed object containing the decoded information from the capabilities message, or null
    /// if no existing Message object exists for the specified event</returns>
    public static IMessage DecodeEvent(String eventName, OSDMap map)
    {
        IMessage message = null;

        switch (EventName.valueOf(eventName))
        {
            case AgentGroupDataUpdate: 
            	message = new AgentGroupDataUpdateMessage(); break;
            case AvatarGroupsReply: message = new AgentGroupDataUpdateMessage(); break; // OpenSim sends the above with the wrong? key
            case ParcelProperties: message = new ParcelPropertiesMessage(); break;
            case ParcelObjectOwnersReply: message = new ParcelObjectOwnersReplyMessage(); break;
            case TeleportFinish: message = new TeleportFinishMessage(); break;
            case EnableSimulator: message = new EnableSimulatorMessage(); break;
            case ParcelPropertiesUpdate: message = new ParcelPropertiesUpdateMessage(); break;
            case EstablishAgentCommunication: message = new EstablishAgentCommunicationMessage(); break;
            case ChatterBoxInvitation: message = new ChatterBoxInvitationMessage(); break;
            case ChatterBoxSessionEventReply: message = new ChatterboxSessionEventReplyMessage(); break;
            case ChatterBoxSessionStartReply: message = new ChatterBoxSessionStartReplyMessage(); break;
            case ChatterBoxSessionAgentListUpdates: message = new ChatterBoxSessionAgentListUpdatesMessage(); break;
            case RequiredVoiceVersion: message = new RequiredVoiceVersionMessage(); break;
            case MapLayer: message = new MapLayerMessage(); break;
            case ChatSessionRequest: message = new ChatSessionRequestMessage(); break;
            case CopyInventoryFromNotecard: message = new CopyInventoryFromNotecardMessage(); break;
            case ProvisionVoiceAccountRequest: message = new ProvisionVoiceAccountRequestMessage(); break;
            case Viewerstats: message = new ViewerStatsMessage(); break;
            case UpdateAgentLanguage: message = new UpdateAgentLanguageMessage(); break;
            case RemoteParcelRequest: message = new RemoteParcelRequestMessage(); break;
            case UpdateScriptTask: message = new UpdateScriptTaskMessage(); break;
            case UpdateScriptAgent: message = new UpdateScriptAgentMessage(); break;
            case SendPostcard: message = new SendPostcardMessage(); break;
            case UpdateGestureAgentInventory: message = new UpdateGestureAgentInventoryMessage(); break;
            case UpdateNotecardAgentInventory: message = new UpdateNotecardAgentInventoryMessage(); break;
            case LandStatReply: message = new LandStatReplyMessage(); break;
            case ParcelVoiceInfoRequest: message = new ParcelVoiceInfoRequestMessage(); break;
            case ViewerStats: message = new ViewerStatsMessage(); break;
            case EventQueueGet: message = new EventQueueGetMessage(); break;
            case CrossedRegion: message = new CrossedRegionMessage(); break;
            case TeleportFailed: message = new TeleportFailedMessage(); break;
            case PlacesReply: message = new PlacesReplyMessage(); break;
            case UpdateAgentInformation: message = new UpdateAgentInformationMessage(); break;
            case DirLandReply: message = new DirLandReplyMessage(); break;
            case ScriptRunningReply: message = new ScriptRunningReplyMessage(); break;
            case SearchStatRequest: message = new SearchStatRequestMessage(); break;
            case AgentDropGroup: message = new AgentDropGroupMessage(); break;
            case ForceCloseChatterBoxSession: message = new ForceCloseChatterBoxSessionMessage(); break;
            case UploadBakedTexture: message = new UploadBakedTextureMessage(); break;
            case RegionInfo: message = new RegionInfoMessage(); break;
            case ObjectMediaNavigate: message = new ObjectMediaNavigateMessage(); break;
            case ObjectMedia: message = new ObjectMediaMessage(); break;
            case AttachmentResources: message = AttachmentResourcesMessage.GetMessageHandler(map); break;
            case LandResources: message = LandResourcesMessage.GetMessageHandler(map); break;
            case GetDisplayNames: message = new GetDisplayNamesMessage(); break;
            case SetDisplayName: message = new SetDisplayNameMessage(); break;
            case SetDisplayNameReply: message = new SetDisplayNameReplyMessage(); break;
            case DisplayNameUpdate: message = new DisplayNameUpdateMessage(); break;
            //case ProductInfoRequest: message = new ProductInfoRequestMessage(); break;
            case ObjectPhysicsProperties: message = new ObjectPhysicsPropertiesMessage(); break;

            // Capabilities TODO:
            // DispatchRegionInfo
            // EstateChangeInfo
            // EventQueueGet
            // FetchInventoryDescendents
            // GroupProposalBallot
            // MapLayerGod
            // NewFileAgentInventory
            // RequestTextureDownload
            // SearchStatRequest
            // SearchStatTracking
            // SendUserReport
            // SendUserReportWithScreenshot
            // ServerReleaseNotes
            // StartGroupProposal
            // UpdateGestureTaskInventory
            // UpdateNotecardTaskInventory
            // ViewerStartAuction
            // UntrustedSimulatorMessage
        }

        if (message != null)
        {
            try
            {
                message.Deserialize(map);
                return message;
            }
            catch (Exception e)
            {
                JLogger.warn("Exception while trying to Deserialize"  + eventName.toString() + ":" + Utils.getExceptionStackTraceAsString(e));                    
            }

            return null;
        }
        else
        {
            return null;
        }
    }
}
