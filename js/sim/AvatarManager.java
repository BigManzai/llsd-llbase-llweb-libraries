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
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Observable;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.protocol.AvatarAnimationPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarAppearancePacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarClassifiedReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarGroupsReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarInterestsReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarPickerReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarPickerRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarPicksReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarPropertiesReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarPropertiesRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.ClassifiedInfoReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GenericMessagePacket;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.PickInfoReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.TrackAgentPacket;
import com.ngt.jopenmetaverse.shared.protocol.UUIDNameReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.UUIDNameRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.ViewerEffectPacket;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntry;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntryFace;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.Action;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.EffectType;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.LookAtType;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.PointAtType;
import com.ngt.jopenmetaverse.shared.sim.Avatar.ProfileFlags;
import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupPowers;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventObservableArg;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.avm.*;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.AgentGroupDataUpdateMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.DisplayNameUpdateMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.GetDisplayNamesMessage;

///// <summary>
///// Retrieve friend status notifications, and retrieve avatar names and
///// profiles
///// </summary>
public class AvatarManager {
    //region Structs
    /// <summary> Information about agents display name </summary>
    public static class AgentDisplayName
    {
        /// <summary> Agent UUID </summary>
        public UUID ID;
        /// <summary> Username </summary>
        public String UserName;
        /// <summary> Display name </summary>
        public String DisplayName;
        /// <summary> First name (legacy) </summary>
        public String LegacyFirstName;
        /// <summary> Last name (legacy) </summary>
        public String LegacyLastName;
        /// <summary> Full name (legacy) </summary>
        public String getLegacyFullName() { return String.format("%s %s", LegacyFirstName, LegacyLastName); }
        /// <summary> Is display name default display name </summary>
        public boolean IsDefaultDisplayName;
        /// <summary> Cache display name until </summary>
        public Date NextUpdate;

        /// <summary>
        /// Creates AgentDisplayName object from OSD
        /// </summary>
        /// <param name="data">Incoming OSD data</param>
        /// <returns>AgentDisplayName object</returns>
        public static AgentDisplayName FromOSD(OSD data)
        {
            AgentDisplayName ret = new AgentDisplayName();

            OSDMap map = (OSDMap)data;
            ret.ID = map.get("id").asUUID();
            ret.UserName = map.get("username").asString();
            ret.DisplayName = map.get("display_name").asString();
            ret.LegacyFirstName = map.get("legacy_first_name").asString();
            ret.LegacyLastName = map.get("legacy_last_name").asString();
            ret.IsDefaultDisplayName = map.get("is_display_name_default").asBoolean();
            ret.NextUpdate = map.get("display_name_next_update").asDate();

            return ret;
        }

        /// <summary>
        /// Return object as OSD map
        /// </summary>
        /// <returns>OSD containing agent's display name data</returns>
        public OSD GetOSD()
        {
            OSDMap map = new OSDMap();
            
            map.put("id", OSD.FromUUID(ID));
            map.put("username",  OSD.FromString(UserName));
            map.put("display_name", OSD.FromString(DisplayName));
            map.put("legacy_first_name",  OSD.FromString(LegacyFirstName));
            map.put("legacy_last_name", OSD.FromString(LegacyLastName));
            map.put("is_display_name_default", OSD.FromBoolean(IsDefaultDisplayName));
            map.put("display_name_next_update", OSD.FromDate(NextUpdate));
            
            return map;
        }

        @Override
        public String toString()
        {
            try {
				return Helpers.StructToString(this);
			} catch (Exception e){ 
				JLogger.warn(Utils.getExceptionStackTraceAsString(e));
				return e.getMessage();
			}
            //StringBuilder result = new StringBuilder();
            //result.AppendLine();
            //result.AppendFormat("{0, 30}: {1,-40} [{2}]" + Environment.NewLine, "ID", ID, "UUID");
            //result.AppendFormat("{0, 30}: {1,-40} [{2}]" + Environment.NewLine, "UserName", UserName, "string");
            //result.AppendFormat("{0, 30}: {1,-40} [{2}]" + Environment.NewLine, "DisplayName", DisplayName, "string");
            //result.AppendFormat("{0, 30}: {1,-40} [{2}]" + Environment.NewLine, "LegacyFirstName", LegacyFirstName, "string");
            //result.AppendFormat("{0, 30}: {1,-40} [{2}]" + Environment.NewLine, "LegaacyLastName", LegaacyLastName, "string");
            //result.AppendFormat("{0, 30}: {1,-40} [{2}]" + Environment.NewLine, "IsDefaultDisplayName", IsDefaultDisplayName, "bool");
            //result.AppendFormat("{0, 30}: {1,-40} [{2}]", "NextUpdate", NextUpdate, "DateTime");
            //return result.ToString();
        }
    }

    /// <summary>
    /// Holds group information for Avatars such as those you might find in a profile
    /// </summary>
    public class AvatarGroup
    {
        /// <summary>true of Avatar accepts group notices</summary>
        public boolean AcceptNotices;
        /// <summary>Groups Key</summary>
        public UUID GroupID;
        /// <summary>Texture Key for groups insignia</summary>
        public UUID GroupInsigniaID;
        /// <summary>Name of the group</summary>
        public String GroupName;
        /// <summary>Powers avatar has in the group</summary>
        public EnumSet<com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupPowers> GroupPowers;
        /// <summary>Avatars Currently selected title</summary>
        public String GroupTitle;
        /// <summary>true of Avatar has chosen to list this in their profile</summary>
        public boolean ListInProfile;
    }

    /// <summary>
    /// Contains an animation currently being played by an agent
    /// </summary>
    public class Animation
    {
        /// <summary>The ID of the animation asset</summary>
        public UUID AnimationID;
        /// <summary>A number to indicate start order of currently playing animations</summary>
        /// <remarks>On Linden Grids this number is unique per region, with OpenSim it is per client</remarks>
        public int AnimationSequence;
        /// <summary></summary>
        public UUID AnimationSourceObjectID;
    }

    /// <summary>
    /// Holds group information on an individual profile pick
    /// </summary>
    public class ProfilePick
    {
        public UUID PickID;
        public UUID CreatorID;
        public boolean TopPick;
        public UUID ParcelID;
        public String Name;
        public String Desc;
        public UUID SnapshotID;
        public String User;
        public String OriginalName;
        public String SimName;
        public Vector3d PosGlobal;
        public int SortOrder;
        public boolean Enabled;
    }

    public class ClassifiedAd
    {
        public UUID ClassifiedID;
        //uint
        public long Catagory;
        public UUID ParcelID;
        //uint
        public long ParentEstate;
        public UUID SnapShotID;
        public Vector3d Position;
        public byte ClassifiedFlags;
        public int Price;
        public String Name;
        public String Desc;
    }
    //endregion

    public final int MAX_UUIDS_PER_PACKET = 100;

    //region Events

    private EventObservable<AvatarAnimationEventArgs> onAvatarAnimation = new EventObservable<AvatarAnimationEventArgs>();
    public void registerOnAvatarAnimation(EventObserver<AvatarAnimationEventArgs> o)
    {
    	onAvatarAnimation.addObserver(o);
    }
    public void unregisterOnAvatarAnimation(EventObserver<AvatarAnimationEventArgs> o) 
    {
    	onAvatarAnimation.deleteObserver(o);
    }

    private EventObservable<AvatarAppearanceEventArgs> onAvatarAppearance = new EventObservable<AvatarAppearanceEventArgs>();
    public void registerOnAvatarAppearance(EventObserver<AvatarAppearanceEventArgs> o)
    {
    	onAvatarAppearance.addObserver(o);
    }
    public void unregisterOnAvatarAppearance(EventObserver<AvatarAppearanceEventArgs> o) 
    {
    	onAvatarAppearance.deleteObserver(o);
    }
    private EventObservable<UUIDNameReplyEventArgs> onUUIDNameReply = new EventObservable<UUIDNameReplyEventArgs>();
    public void registerOnUUIDNameReply(EventObserver<UUIDNameReplyEventArgs> o)
    {
    	onUUIDNameReply.addObserver(o);
    }
    public void unregisterOnUUIDNameReply(EventObserver<UUIDNameReplyEventArgs> o) 
    {
    	onUUIDNameReply.deleteObserver(o);
    }
    private EventObservable<AvatarInterestsReplyEventArgs> onAvatarInterestsReply = new EventObservable<AvatarInterestsReplyEventArgs>();
    public void registerOnAvatarInterestsReply(EventObserver<AvatarInterestsReplyEventArgs> o)
    {
    	onAvatarInterestsReply.addObserver(o);
    }
    public void unregisterOnAvatarInterestsReply(EventObserver<AvatarInterestsReplyEventArgs> o) 
    {
    	onAvatarInterestsReply.deleteObserver(o);
    }
    private EventObservable<AvatarPropertiesReplyEventArgs> onAvatarPropertiesReply = new EventObservable<AvatarPropertiesReplyEventArgs>();
    public void registerOnAvatarPropertiesReply(EventObserver<AvatarPropertiesReplyEventArgs> o)
    {
    	onAvatarPropertiesReply.addObserver(o);
    }
    public void unregisterOnAvatarPropertiesReply(EventObserver<AvatarPropertiesReplyEventArgs> o) 
    {
    	onAvatarPropertiesReply.deleteObserver(o);
    }
    private EventObservable<AvatarGroupsReplyEventArgs> onAvatarGroupsReply = new EventObservable<AvatarGroupsReplyEventArgs>();
    public void registerOnAvatarGroupsReply(EventObserver<AvatarGroupsReplyEventArgs> o)
    {
    	onAvatarGroupsReply.addObserver(o);
    }
    public void unregisterOnAvatarGroupsReply(EventObserver<AvatarGroupsReplyEventArgs> o) 
    {
    	onAvatarGroupsReply.deleteObserver(o);
    }
    private EventObservable<AvatarPickerReplyEventArgs> onAvatarPickerReply = new EventObservable<AvatarPickerReplyEventArgs>();
    public void registerOnAvatarPickerReply(EventObserver<AvatarPickerReplyEventArgs> o)
    {
    	onAvatarPickerReply.addObserver(o);
    }
    public void unregisterOnAvatarPickerReply(EventObserver<AvatarPickerReplyEventArgs> o) 
    {
    	onAvatarPickerReply.deleteObserver(o);
    }
    private EventObservable<ViewerEffectPointAtEventArgs> onViewerEffectPointAt = new EventObservable<ViewerEffectPointAtEventArgs>();
    public void registerOnViewerEffectPointAt(EventObserver<ViewerEffectPointAtEventArgs> o)
    {
    	onViewerEffectPointAt.addObserver(o);
    }
    public void unregisterOnViewerEffectPointAt(EventObserver<ViewerEffectPointAtEventArgs> o) 
    {
    	onViewerEffectPointAt.deleteObserver(o);
    }
    private EventObservable<ViewerEffectLookAtEventArgs> onViewerEffectLookAt = new EventObservable<ViewerEffectLookAtEventArgs>();
    public void registerOnViewerEffectLookAt(EventObserver<ViewerEffectLookAtEventArgs> o)
    {
    	onViewerEffectLookAt.addObserver(o);
    }
    public void unregisterOnViewerEffectLookAt(EventObserver<ViewerEffectLookAtEventArgs> o) 
    {
    	onViewerEffectLookAt.deleteObserver(o);
    }
    private EventObservable<ViewerEffectEventArgs> onViewerEffect = new EventObservable<ViewerEffectEventArgs>();
    public void registerOnViewerEffect(EventObserver<ViewerEffectEventArgs> o)
    {
    	onViewerEffect.addObserver(o);
    }
    public void unregisterOnViewerEffect(EventObserver<ViewerEffectEventArgs> o) 
    {
    	onViewerEffect.deleteObserver(o);
    }
    private EventObservable<AvatarPicksReplyEventArgs> onAvatarPicksReply = new EventObservable<AvatarPicksReplyEventArgs>();
    public void registerOnAvatarPicksReply(EventObserver<AvatarPicksReplyEventArgs> o)
    {
    	onAvatarPicksReply.addObserver(o);
    }
    public void unregisterOnAvatarPicksReply(EventObserver<AvatarPicksReplyEventArgs> o) 
    {
    	onAvatarPicksReply.deleteObserver(o);
    }
    private EventObservable<PickInfoReplyEventArgs> onPickInfoReply = new EventObservable<PickInfoReplyEventArgs>();
    public void registerOnPickInfoReply(EventObserver<PickInfoReplyEventArgs> o)
    {
    	onPickInfoReply.addObserver(o);
    }
    public void unregisterOnPickInfoReply(EventObserver<PickInfoReplyEventArgs> o) 
    {
    	onPickInfoReply.deleteObserver(o);
    }
    private EventObservable<AvatarClassifiedReplyEventArgs> onAvatarClassifiedReply = new EventObservable<AvatarClassifiedReplyEventArgs>();
    public void registerOnAvatarClassifiedReply(EventObserver<AvatarClassifiedReplyEventArgs> o)
    {
    	onAvatarClassifiedReply.addObserver(o);
    }
    public void unregisterOnAvatarClassifiedReply(EventObserver<AvatarClassifiedReplyEventArgs> o) 
    {
    	onAvatarClassifiedReply.deleteObserver(o);
    }
    private EventObservable<ClassifiedInfoReplyEventArgs> onClassifiedInfoReply = new EventObservable<ClassifiedInfoReplyEventArgs>();
    public void registerOnClassifiedInfoReply(EventObserver<ClassifiedInfoReplyEventArgs> o)
    {
    	onClassifiedInfoReply.addObserver(o);
    }
    public void unregisterOnClassifiedInfoReply(EventObserver<ClassifiedInfoReplyEventArgs> o) 
    {
    	onClassifiedInfoReply.deleteObserver(o);
    }
    private EventObservable<DisplayNameUpdateEventArgs> onDisplayNameUpdate = new EventObservable<DisplayNameUpdateEventArgs>();
    public void registerOnDisplayNameUpdate(EventObserver<DisplayNameUpdateEventArgs> o)
    {
    	onDisplayNameUpdate.addObserver(o);
    }
    public void unregisterOnDisplayNameUpdate(EventObserver<DisplayNameUpdateEventArgs> o) 
    {
    	onDisplayNameUpdate.deleteObserver(o);
    }
        
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarAnimationEventArgs> m_AvatarAnimation;
//
//        ///<summary>Raises the AvatarAnimation Event</summary>
//        /// <param name="e">An AvatarAnimationEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarAnimation(AvatarAnimationEventArgs e)
//        {
//            EventHandler<AvatarAnimationEventArgs> handler = m_AvatarAnimation;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarAnimationLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// an agents animation playlist</summary>
//        public event EventHandler<AvatarAnimationEventArgs> AvatarAnimation 
//        {
//            add { lock (m_AvatarAnimationLock) { m_AvatarAnimation += value; } }
//            remove { lock (m_AvatarAnimationLock) { m_AvatarAnimation -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarAppearanceEventArgs> m_AvatarAppearance;
//
//        ///<summary>Raises the AvatarAppearance Event</summary>
//        /// <param name="e">A AvatarAppearanceEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarAppearance(AvatarAppearanceEventArgs e)
//        {
//            EventHandler<AvatarAppearanceEventArgs> handler = m_AvatarAppearance;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarAppearanceLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the appearance information for an agent</summary>
//        public event EventHandler<AvatarAppearanceEventArgs> AvatarAppearance 
//        {
//            add { lock (m_AvatarAppearanceLock) { m_AvatarAppearance += value; } }
//            remove { lock (m_AvatarAppearanceLock) { m_AvatarAppearance -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<UUIDNameReplyEventArgs> m_UUIDNameReply;
//
//        ///<summary>Raises the UUIDNameReply Event</summary>
//        /// <param name="e">A UUIDNameReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnUUIDNameReply(UUIDNameReplyEventArgs e)
//        {
//            EventHandler<UUIDNameReplyEventArgs> handler = m_UUIDNameReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_UUIDNameReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// agent names/id values</summary>
//        public event EventHandler<UUIDNameReplyEventArgs> UUIDNameReply 
//        {
//            add { lock (m_UUIDNameReplyLock) { m_UUIDNameReply += value; } }
//            remove { lock (m_UUIDNameReplyLock) { m_UUIDNameReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarInterestsReplyEventArgs> m_AvatarInterestsReply;
//
//        ///<summary>Raises the AvatarInterestsReply Event</summary>
//        /// <param name="e">A AvatarInterestsReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarInterestsReply(AvatarInterestsReplyEventArgs e)
//        {
//            EventHandler<AvatarInterestsReplyEventArgs> handler = m_AvatarInterestsReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarInterestsReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the interests listed in an agents profile</summary>
//        public event EventHandler<AvatarInterestsReplyEventArgs> AvatarInterestsReply 
//        {
//            add { lock (m_AvatarInterestsReplyLock) { m_AvatarInterestsReply += value; } }
//            remove { lock (m_AvatarInterestsReplyLock) { m_AvatarInterestsReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarPropertiesReplyEventArgs> m_AvatarPropertiesReply;
//
//        ///<summary>Raises the AvatarPropertiesReply Event</summary>
//        /// <param name="e">A AvatarPropertiesReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarPropertiesReply(AvatarPropertiesReplyEventArgs e)
//        {
//            EventHandler<AvatarPropertiesReplyEventArgs> handler = m_AvatarPropertiesReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarPropertiesReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// profile property information for an agent</summary>
//        public event EventHandler<AvatarPropertiesReplyEventArgs> AvatarPropertiesReply 
//        {
//            add { lock (m_AvatarPropertiesReplyLock) { m_AvatarPropertiesReply += value; } }
//            remove { lock (m_AvatarPropertiesReplyLock) { m_AvatarPropertiesReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarGroupsReplyEventArgs> m_AvatarGroupsReply;
//
//        ///<summary>Raises the AvatarGroupsReply Event</summary>
//        /// <param name="e">A AvatarGroupsReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarGroupsReply(AvatarGroupsReplyEventArgs e)
//        {
//            EventHandler<AvatarGroupsReplyEventArgs> handler = m_AvatarGroupsReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarGroupsReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the group membership an agent is a member of</summary>
//        public event EventHandler<AvatarGroupsReplyEventArgs> AvatarGroupsReply 
//        {
//            add { lock (m_AvatarGroupsReplyLock) { m_AvatarGroupsReply += value; } }
//            remove { lock (m_AvatarGroupsReplyLock) { m_AvatarGroupsReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarPickerReplyEventArgs> m_AvatarPickerReply;
//
//        ///<summary>Raises the AvatarPickerReply Event</summary>
//        /// <param name="e">A AvatarPickerReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarPickerReply(AvatarPickerReplyEventArgs e)
//        {
//            EventHandler<AvatarPickerReplyEventArgs> handler = m_AvatarPickerReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarPickerReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// name/id pair</summary>
//        public event EventHandler<AvatarPickerReplyEventArgs> AvatarPickerReply 
//        {
//            add { lock (m_AvatarPickerReplyLock) { m_AvatarPickerReply += value; } }
//            remove { lock (m_AvatarPickerReplyLock) { m_AvatarPickerReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<ViewerEffectPointAtEventArgs> m_ViewerEffectPointAt;
//
//        ///<summary>Raises the ViewerEffectPointAt Event</summary>
//        /// <param name="e">A ViewerEffectPointAtEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnViewerEffectPointAt(ViewerEffectPointAtEventArgs e)
//        {
//            EventHandler<ViewerEffectPointAtEventArgs> handler = m_ViewerEffectPointAt;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_ViewerEffectPointAtLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the objects and effect when an agent is pointing at</summary>
//        public event EventHandler<ViewerEffectPointAtEventArgs> ViewerEffectPointAt 
//        {
//            add { lock (m_ViewerEffectPointAtLock) { m_ViewerEffectPointAt += value; } }
//            remove { lock (m_ViewerEffectPointAtLock) { m_ViewerEffectPointAt -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<ViewerEffectLookAtEventArgs> m_ViewerEffectLookAt;
//
//        ///<summary>Raises the ViewerEffectLookAt Event</summary>
//        /// <param name="e">A ViewerEffectLookAtEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnViewerEffectLookAt(ViewerEffectLookAtEventArgs e)
//        {
//            EventHandler<ViewerEffectLookAtEventArgs> handler = m_ViewerEffectLookAt;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_ViewerEffectLookAtLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the objects and effect when an agent is looking at</summary>
//        public event EventHandler<ViewerEffectLookAtEventArgs> ViewerEffectLookAt 
//        {
//            add { lock (m_ViewerEffectLookAtLock) { m_ViewerEffectLookAt += value; } }
//            remove { lock (m_ViewerEffectLookAtLock) { m_ViewerEffectLookAt -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<ViewerEffectEventArgs> m_ViewerEffect;
//
//        ///<summary>Raises the ViewerEffect Event</summary>
//        /// <param name="e">A ViewerEffectEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnViewerEffect(ViewerEffectEventArgs e)
//        {
//            EventHandler<ViewerEffectEventArgs> handler = m_ViewerEffect;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_ViewerEffectLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// an agents viewer effect information</summary>
//        public event EventHandler<ViewerEffectEventArgs> ViewerEffect 
//        {
//            add { lock (m_ViewerEffectLock) { m_ViewerEffect += value; } }
//            remove { lock (m_ViewerEffectLock) { m_ViewerEffect -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarPicksReplyEventArgs> m_AvatarPicksReply;
//
//        ///<summary>Raises the AvatarPicksReply Event</summary>
//        /// <param name="e">A AvatarPicksReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarPicksReply(AvatarPicksReplyEventArgs e)
//        {
//            EventHandler<AvatarPicksReplyEventArgs> handler = m_AvatarPicksReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarPicksReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the top picks from an agents profile</summary>
//        public event EventHandler<AvatarPicksReplyEventArgs> AvatarPicksReply 
//        {
//            add { lock (m_AvatarPicksReplyLock) { m_AvatarPicksReply += value; } }
//            remove { lock (m_AvatarPicksReplyLock) { m_AvatarPicksReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<PickInfoReplyEventArgs> m_PickInfoReply;
//
//        ///<summary>Raises the PickInfoReply Event</summary>
//        /// <param name="e">A PickInfoReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnPickInfoReply(PickInfoReplyEventArgs e)
//        {
//            EventHandler<PickInfoReplyEventArgs> handler = m_PickInfoReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_PickInfoReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the Pick details</summary>
//        public event EventHandler<PickInfoReplyEventArgs> PickInfoReply 
//        {
//            add { lock (m_PickInfoReplyLock) { m_PickInfoReply += value; } }
//            remove { lock (m_PickInfoReplyLock) { m_PickInfoReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<AvatarClassifiedReplyEventArgs> m_AvatarClassifiedReply;
//
//        ///<summary>Raises the AvatarClassifiedReply Event</summary>
//        /// <param name="e">A AvatarClassifiedReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnAvatarClassifiedReply(AvatarClassifiedReplyEventArgs e)
//        {
//            EventHandler<AvatarClassifiedReplyEventArgs> handler = m_AvatarClassifiedReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_AvatarClassifiedReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the classified ads an agent has placed</summary>
//        public event EventHandler<AvatarClassifiedReplyEventArgs> AvatarClassifiedReply 
//        {
//            add { lock (m_AvatarClassifiedReplyLock) { m_AvatarClassifiedReply += value; } }
//            remove { lock (m_AvatarClassifiedReplyLock) { m_AvatarClassifiedReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<ClassifiedInfoReplyEventArgs> m_ClassifiedInfoReply;
//
//        ///<summary>Raises the ClassifiedInfoReply Event</summary>
//        /// <param name="e">A ClassifiedInfoReplyEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnClassifiedInfoReply(ClassifiedInfoReplyEventArgs e)
//        {
//            EventHandler<ClassifiedInfoReplyEventArgs> handler = m_ClassifiedInfoReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_ClassifiedInfoReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the details of a classified ad</summary>
//        public event EventHandler<ClassifiedInfoReplyEventArgs> ClassifiedInfoReply 
//        {
//            add { lock (m_ClassifiedInfoReplyLock) { m_ClassifiedInfoReply += value; } }
//            remove { lock (m_ClassifiedInfoReplyLock) { m_ClassifiedInfoReply -= value; } }
//        }
//
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<DisplayNameUpdateEventArgs> m_DisplayNameUpdate;
//
//        ///<summary>Raises the DisplayNameUpdate Event</summary>
//        /// <param name="e">A DisplayNameUpdateEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnDisplayNameUpdate(DisplayNameUpdateEventArgs e)
//        {
//            EventHandler<DisplayNameUpdateEventArgs> handler = m_DisplayNameUpdate;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_DisplayNameUpdateLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// the details of display name change</summary>
//        public event EventHandler<DisplayNameUpdateEventArgs> DisplayNameUpdate 
//        {
//            add { lock (m_DisplayNameUpdateLock) { m_DisplayNameUpdate += value; } }
//            remove { lock (m_DisplayNameUpdateLock) { m_DisplayNameUpdate -= value; } }
//        }
//
        //endregion Events

    
    
        //region Delegates
        /// <summary>
        /// Callback giving results when fetching display names
        /// </summary>
        /// <param name="success">If the request was successful</param>
        /// <param name="names">Array of display names</param>
        /// <param name="badIDs">Array of UUIDs that could not be fetched</param>
    //TODO need to implement
//        public delegate void DisplayNamesCallback(bool success, AgentDisplayName[] names, UUID[] badIDs);
        //endregion Delegates

    private GridClient Client;

    /// <summary>
    /// Represents other avatars
    /// </summary>
    /// <param name="client"></param>
    public AvatarManager(GridClient client)
    {
    	Client = client;
    	// Avatar appearance callback
    	// Client.network.RegisterCallback(PacketType.AvatarAppearance, AvatarAppearanceHandler);

    	Client.network.RegisterCallback(PacketType.AvatarAppearance, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarAppearanceHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	//
    	// Avatar profile callbacks
    	// Client.network.RegisterCallback(PacketType.AvatarPropertiesReply, AvatarPropertiesHandler);

    	Client.network.RegisterCallback(PacketType.AvatarPropertiesReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarPropertiesHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	
    	// // Client.network.RegisterCallback(PacketType.AvatarStatisticsReply, AvatarStatisticsHandler);

//    	Client.network.RegisterCallback(PacketType.AvatarStatisticsReply, new EventObserver<PacketReceivedEventArgs>()
//    			{ 
//    		@Override
//    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
//    			try{ AvatarStatisticsHandler(o, arg);}
//    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
//    		}}
//    			);
    	
    	// Client.network.RegisterCallback(PacketType.AvatarInterestsReply, AvatarInterestsHandler);

    	Client.network.RegisterCallback(PacketType.AvatarInterestsReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarInterestsHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	//
    	// Avatar group callback
    	// Client.network.RegisterCallback(PacketType.AvatarGroupsReply, AvatarGroupsReplyHandler);

    	Client.network.RegisterCallback(PacketType.AvatarGroupsReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarGroupsReplyHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	// Client.network.RegisterEventCallback("AgentGroupDataUpdate", new Caps.EventQueueCallback(AvatarGroupsReplyMessageHandler);

    	Client.network.RegisterEventCallback("AgentGroupDataUpdate", new EventObserver<CapsEventObservableArg>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,CapsEventObservableArg arg) {
    			try{ AvatarGroupsReplyMessageHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	// Client.network.RegisterEventCallback("AvatarGroupsReply", new Caps.EventQueueCallback(AvatarGroupsReplyMessageHandler);

    	Client.network.RegisterEventCallback("AvatarGroupsReply", new EventObserver<CapsEventObservableArg>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,CapsEventObservableArg arg) {
    			try{ AvatarGroupsReplyMessageHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	//
    	// Viewer effect callback
    	// Client.network.RegisterCallback(PacketType.ViewerEffect, ViewerEffectHandler);

    	Client.network.RegisterCallback(PacketType.ViewerEffect, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ ViewerEffectHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	//
    	// Other callbacks
    	// Client.network.RegisterCallback(PacketType.UUIDNameReply, UUIDNameReplyHandler);

    	Client.network.RegisterCallback(PacketType.UUIDNameReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ UUIDNameReplyHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	// Client.network.RegisterCallback(PacketType.AvatarPickerReply, AvatarPickerReplyHandler);

    	Client.network.RegisterCallback(PacketType.AvatarPickerReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarPickerReplyHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	// Client.network.RegisterCallback(PacketType.AvatarAnimation, AvatarAnimationHandler);

    	Client.network.RegisterCallback(PacketType.AvatarAnimation, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarAnimationHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	//
    	// Picks callbacks
    	// Client.network.RegisterCallback(PacketType.AvatarPicksReply, AvatarPicksReplyHandler);

    	Client.network.RegisterCallback(PacketType.AvatarPicksReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarPicksReplyHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	// Client.network.RegisterCallback(PacketType.PickInfoReply, PickInfoReplyHandler);

    	Client.network.RegisterCallback(PacketType.PickInfoReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ PickInfoReplyHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	//
    	// Classifieds callbacks
    	// Client.network.RegisterCallback(PacketType.AvatarClassifiedReply, AvatarClassifiedReplyHandler);

    	Client.network.RegisterCallback(PacketType.AvatarClassifiedReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ AvatarClassifiedReplyHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	// Client.network.RegisterCallback(PacketType.ClassifiedInfoReply, ClassifiedInfoReplyHandler);

    	Client.network.RegisterCallback(PacketType.ClassifiedInfoReply, new EventObserver<PacketReceivedEventArgs>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
    			try{ ClassifiedInfoReplyHandler(o, arg);}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    	//
    	// Client.network.RegisterEventCallback("DisplayNameUpdate", new Caps.EventQueueCallback(DisplayNameUpdateMessageHandler);
    	Client.network.RegisterEventCallback("DisplayNameUpdate", new EventObserver<CapsEventObservableArg>()
    			{ 
    		@Override
    		public void handleEvent(Observable o,CapsEventObservableArg arg) {
    			try{ DisplayNameUpdateMessageHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
    			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
    		}}
    			);
    }

        /// <summary>Tracks the specified avatar on your map</summary>
        /// <param name="preyID">Avatar ID to track</param>
        public void RequestTrackAgent(UUID preyID)
        {
            TrackAgentPacket p = new TrackAgentPacket();
            p.AgentData.AgentID = Client.self.getAgentID();
            p.AgentData.SessionID = Client.self.getSessionID();
            p.TargetData.PreyID = preyID;
            Client.network.SendPacket(p);
        }

        /// <summary>
        /// Request a single avatar name
        /// </summary>
        /// <param name="id">The avatar key to retrieve a name for</param>
        public void RequestAvatarName(UUID id)
        {
            UUIDNameRequestPacket request = new UUIDNameRequestPacket();
            request.UUIDNameBlock = new UUIDNameRequestPacket.UUIDNameBlockBlock[1];
            request.UUIDNameBlock[0] = new UUIDNameRequestPacket.UUIDNameBlockBlock();
            request.UUIDNameBlock[0].ID = id;

            Client.network.SendPacket(request);
        }

        /// <summary>
        /// Request a list of avatar names
        /// </summary>
        /// <param name="ids">The avatar keys to retrieve names for</param>
        public void RequestAvatarNames(List<UUID> ids)
        {
            int m = MAX_UUIDS_PER_PACKET;
            int n = ids.size() / m; // Number of full requests to make
            int i = 0;

            UUIDNameRequestPacket request;

            for (int j = 0; j < n; j++)
            {
                request = new UUIDNameRequestPacket();
                request.UUIDNameBlock = new UUIDNameRequestPacket.UUIDNameBlockBlock[m];

                for (; i < (j + 1) * m; i++)
                {
                    request.UUIDNameBlock[i % m] = new UUIDNameRequestPacket.UUIDNameBlockBlock();
                    request.UUIDNameBlock[i % m].ID = ids.get(i);
                }

                Client.network.SendPacket(request);
            }

            // Get any remaining names after left after the full requests
            if (ids.size() > n * m)
            {
                request = new UUIDNameRequestPacket();
                request.UUIDNameBlock = new UUIDNameRequestPacket.UUIDNameBlockBlock[ids.size() - n * m];

                for (; i < ids.size(); i++)
                {
                    request.UUIDNameBlock[i % m] = new UUIDNameRequestPacket.UUIDNameBlockBlock();
                    request.UUIDNameBlock[i % m].ID = ids.get(i);
                }

                Client.network.SendPacket(request);
            }
        }

        /// <summary>
        /// Check if Display Names functionality is available
        /// </summary>
        /// <returns>True if Display name functionality is available</returns>
        public boolean DisplayNamesAvailable()
        {
            return (Client.network.getCurrentSim() != null && Client.network.getCurrentSim().Caps != null) && Client.network.getCurrentSim().Caps.CapabilityURI("GetDisplayNames") != null;
        }

        /// <summary>
        /// Request retrieval of display names (max 90 names per request)
        /// </summary>
        /// <param name="ids">List of UUIDs to lookup</param>
        /// <param name="callback">Callback to report result of the operation</param>
        public void GetDisplayNames(final List<UUID> ids, final EventObserver<DisplayNamesCallbackArgs> callback) throws URISyntaxException
        {
            if (!DisplayNamesAvailable() || ids.size() == 0)
            {
                callback.handleEvent(null, new DisplayNamesCallbackArgs(false, null, null));
            }

            StringBuilder query = new StringBuilder();
            for (int i = 0; i < ids.size() && i < 90; i++)
            {
                query.append(String.format("ids=%s", ids.get(i)));
                if (i < ids.size() - 1)
                {
                    query.append("&");
                }
            }

            URI uri = new URI(Client.network.getCurrentSim().Caps.CapabilityURI("GetDisplayNames").toString() + "/?" + query);

            CapsHttpClient cap = new CapsHttpClient(uri);
            
            cap.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>(){
				@Override
				public void handleEvent(Observable o, CapsHttpRequestCompletedArg arg) {
					// TODO Auto-generated method stub
					CapsHttpClient client = arg.getClient();
					OSD result = arg.getResult();
					Exception error = arg.getError();
					 try
                     {
                         if (error != null)
                             throw error;
                         GetDisplayNamesMessage msg = new GetDisplayNamesMessage();
                         msg.Deserialize((OSDMap) result);
                         callback.handleEvent(null, new DisplayNamesCallbackArgs(true, msg.Agents, msg.BadIDs));
                     }
                     catch (Exception ex)
                     {
                         JLogger.warn("Failed to call GetDisplayNames capability: " + Utils.getExceptionStackTraceAsString(ex));
//                         callback(false, null, null);
                         callback.handleEvent(null, new DisplayNamesCallbackArgs(false, null, null));
                     }
				}
            });
//            cap.OnComplete += (CapsHttpClient client, OSD result, Exception error) =>
//                                  {
//                                      try
//                                      {
//                                          if (error != null)
//                                              throw error;
//                                          GetDisplayNamesMessage msg = new GetDisplayNamesMessage();
//                                          msg.Deserialize((OSDMap) result);
//                                          callback(true, msg.Agents, msg.BadIDs);
//                                      }
//                                      catch (Exception ex)
//                                      {
//                                          Logger.Log("Failed to call GetDisplayNames capability: ",
//                                                     Helpers.LogLevel.Warning, Client, ex);
//                                          callback(false, null, null);
//                                      }
//                                  };
            cap.BeginGetResponse(null, "", Client.settings.CAPS_TIMEOUT);
        }

        /// <summary>
        /// Start a request for Avatar Properties
        /// </summary>
        /// <param name="avatarid"></param>
        public void RequestAvatarProperties(UUID avatarid)
        {
            AvatarPropertiesRequestPacket aprp = new AvatarPropertiesRequestPacket();

            aprp.AgentData.AgentID = Client.self.getAgentID();
            aprp.AgentData.SessionID = Client.self.getSessionID();
            aprp.AgentData.AvatarID = avatarid;

            Client.network.SendPacket(aprp);
        }

        /// <summary>
        /// Search for an avatar (first name, last name)
        /// </summary>
        /// <param name="name">The name to search for</param>
        /// <param name="queryID">An ID to associate with this query</param>
        public void RequestAvatarNameSearch(String name, UUID queryID)
        {
            AvatarPickerRequestPacket aprp = new AvatarPickerRequestPacket();

            aprp.AgentData.AgentID = Client.self.getAgentID();
            aprp.AgentData.SessionID = Client.self.getSessionID();
            aprp.AgentData.QueryID = queryID;
            aprp.Data.Name = Utils.stringToBytesWithTrailingNullByte(name);

            Client.network.SendPacket(aprp);
        }

        /// <summary>
        /// Start a request for Avatar Picks
        /// </summary>
        /// <param name="avatarid">UUID of the avatar</param>
        public void RequestAvatarPicks(UUID avatarid)
        {
            GenericMessagePacket gmp = new GenericMessagePacket();

            gmp.AgentData.AgentID = Client.self.getAgentID();
            gmp.AgentData.SessionID = Client.self.getSessionID();
            gmp.AgentData.TransactionID = UUID.Zero;

            gmp.MethodData.Method = Utils.stringToBytesWithTrailingNullByte("avatarpicksrequest");
            gmp.MethodData.Invoice = UUID.Zero;
            gmp.ParamList = new GenericMessagePacket.ParamListBlock[1];
            gmp.ParamList[0] = new GenericMessagePacket.ParamListBlock();
            gmp.ParamList[0].Parameter = Utils.stringToBytesWithTrailingNullByte(avatarid.toString());

            Client.network.SendPacket(gmp);
        }

        /// <summary>
        /// Start a request for Avatar Classifieds
        /// </summary>
        /// <param name="avatarid">UUID of the avatar</param>
        public void RequestAvatarClassified(UUID avatarid)
        {
            GenericMessagePacket gmp = new GenericMessagePacket();

            gmp.AgentData.AgentID = Client.self.getAgentID();
            gmp.AgentData.SessionID = Client.self.getSessionID();
            gmp.AgentData.TransactionID = UUID.Zero;

            gmp.MethodData.Method = Utils.stringToBytesWithTrailingNullByte("avatarclassifiedsrequest");
            gmp.MethodData.Invoice = UUID.Zero;
            gmp.ParamList = new GenericMessagePacket.ParamListBlock[1];
            gmp.ParamList[0] = new GenericMessagePacket.ParamListBlock();
            gmp.ParamList[0].Parameter = Utils.stringToBytesWithTrailingNullByte(avatarid.toString());

            Client.network.SendPacket(gmp);
        }

        /// <summary>
        /// Start a request for details of a specific profile pick
        /// </summary>
        /// <param name="avatarid">UUID of the avatar</param>
        /// <param name="pickid">UUID of the profile pick</param>
        public void RequestPickInfo(UUID avatarid, UUID pickid)
        {
            GenericMessagePacket gmp = new GenericMessagePacket();

            gmp.AgentData.AgentID = Client.self.getAgentID();
            gmp.AgentData.SessionID = Client.self.getSessionID();
            gmp.AgentData.TransactionID = UUID.Zero;

            gmp.MethodData.Method = Utils.stringToBytesWithTrailingNullByte("pickinforequest");
            gmp.MethodData.Invoice = UUID.Zero;
            gmp.ParamList = new GenericMessagePacket.ParamListBlock[2];
            gmp.ParamList[0] = new GenericMessagePacket.ParamListBlock();
            gmp.ParamList[0].Parameter = Utils.stringToBytesWithTrailingNullByte(avatarid.toString());
            gmp.ParamList[1] = new GenericMessagePacket.ParamListBlock();
            gmp.ParamList[1].Parameter = Utils.stringToBytesWithTrailingNullByte(pickid.toString());

            Client.network.SendPacket(gmp);
        }

        /// <summary>
        /// Start a request for details of a specific profile classified
        /// </summary>
        /// <param name="avatarid">UUID of the avatar</param>
        /// <param name="classifiedid">UUID of the profile classified</param>
        public void RequestClassifiedInfo(UUID avatarid, UUID classifiedid)
        {
            GenericMessagePacket gmp = new GenericMessagePacket();

            gmp.AgentData.AgentID = Client.self.getAgentID();
            gmp.AgentData.SessionID = Client.self.getSessionID();
            gmp.AgentData.TransactionID = UUID.Zero;

            gmp.MethodData.Method = Utils.stringToBytesWithTrailingNullByte("classifiedinforequest");
            gmp.MethodData.Invoice = UUID.Zero;
            gmp.ParamList = new GenericMessagePacket.ParamListBlock[2];
            gmp.ParamList[0] = new GenericMessagePacket.ParamListBlock();
            gmp.ParamList[0].Parameter = Utils.stringToBytesWithTrailingNullByte(avatarid.toString());
            gmp.ParamList[1] = new GenericMessagePacket.ParamListBlock();
            gmp.ParamList[1].Parameter = Utils.stringToBytesWithTrailingNullByte(classifiedid.toString());

            Client.network.SendPacket(gmp);
        }

        //region Packet Handlers

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void UUIDNameReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onUUIDNameReply != null)
            {
                Packet packet = e.getPacket();
                Map<UUID, String> names = new HashMap<UUID, String>();
                UUIDNameReplyPacket reply = (UUIDNameReplyPacket)packet;

                for (UUIDNameReplyPacket.UUIDNameBlockBlock block : reply.UUIDNameBlock)
                {
                    names.put(block.ID, Utils.bytesWithTrailingNullByteToString(block.FirstName) +
                        " " + Utils.bytesWithTrailingNullByteToString(block.LastName));
                }

                onUUIDNameReply.raiseEvent(new UUIDNameReplyEventArgs(names));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarAnimationHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();

            if (onAvatarAnimation != null)
            {
                AvatarAnimationPacket data = (AvatarAnimationPacket)packet;

                List<Animation> signaledAnimations = new ArrayList<Animation>(data.AnimationList.length);

                for (int i = 0; i < data.AnimationList.length; i++)
                {
                    Animation animation = new Animation();
                    animation.AnimationID = data.AnimationList[i].AnimID;
                    animation.AnimationSequence = data.AnimationList[i].AnimSequenceID;
                    if (i < data.AnimationSourceList.length)
                    {
                        animation.AnimationSourceObjectID = data.AnimationSourceList[i].ObjectID;
                    }

                    signaledAnimations.add(animation);
                }

                onAvatarAnimation.raiseEvent(new AvatarAnimationEventArgs(data.Sender.ID, signaledAnimations));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarAppearanceHandler(Object sender, PacketReceivedEventArgs e) throws Exception
        {
            if (onAvatarAppearance != null || Client.settings.AVATAR_TRACKING)
            {
                Packet packet = e.getPacket();
                Simulator simulator = e.getSimulator();

                final AvatarAppearancePacket appearance = (AvatarAppearancePacket)packet;

                List<Byte> visualParams = new ArrayList<Byte>();
                for (AvatarAppearancePacket.VisualParamBlock block : appearance.VisualParam)
                {
                    visualParams.add(block.ParamValue);
                }

                TextureEntry textureEntry = new TextureEntry(appearance.ObjectData.TextureEntry, 0,
                        appearance.ObjectData.TextureEntry.length);

                TextureEntryFace defaultTexture = textureEntry.DefaultTexture;
                TextureEntryFace[] faceTextures = textureEntry.FaceTextures;

//              Avatar av = simulator.ObjectsAvatars.Find((Avatar a) => { return a.ID == appearance.Sender.ID; });
                final Avatar[] tmpAvatar = new Avatar[]{null};
                simulator.ObjectsAvatars.foreach(new Action<Entry<Long, Avatar>>(){
					public void execute(Entry<Long, Avatar> e) {
						if(e.getValue().ID.equals(appearance.Sender.ID))
							tmpAvatar[0] = e.getValue();
					}	
                }
                );
                Avatar av = tmpAvatar[0];
                if (av != null)
                {
                    av.Textures = textureEntry;
                    //TODO need to do better
                    av.VisualParameters = new byte[visualParams.size()];
                    int i = 0;
                    for (Byte byte1 : visualParams)
                    {
                    	av.VisualParameters[i] = byte1;
                        i ++;
                    }
                }

                onAvatarAppearance.raiseEvent(new AvatarAppearanceEventArgs(simulator, appearance.Sender.ID, appearance.Sender.IsTrial,
                    defaultTexture, faceTextures, visualParams));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarPropertiesHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onAvatarPropertiesReply != null)
            {
                Packet packet = e.getPacket();
                AvatarPropertiesReplyPacket reply = (AvatarPropertiesReplyPacket)packet;
                Avatar.AvatarProperties properties = new Avatar.AvatarProperties();

                properties.ProfileImage = reply.PropertiesData.ImageID;
                properties.FirstLifeImage = reply.PropertiesData.FLImageID;
                properties.Partner = reply.PropertiesData.PartnerID;
                properties.AboutText = Utils.bytesWithTrailingNullByteToString(reply.PropertiesData.AboutText);
                properties.FirstLifeText = Utils.bytesWithTrailingNullByteToString(reply.PropertiesData.FLAboutText);
                properties.BornOn = Utils.bytesWithTrailingNullByteToString(reply.PropertiesData.BornOn);
                //properties.CharterMember = Utils.bytesWithTrailingNullByteToString(reply.PropertiesData.CharterMember);
                //uint
                long charter = Utils.bytesToUIntLit(reply.PropertiesData.CharterMember);
                if (charter == 0)
                {
                    properties.CharterMember = "Resident";
                }
                else if (charter == 2)
                {
                    properties.CharterMember = "Charter";
                }
                else if (charter == 3)
                {
                    properties.CharterMember = "Linden";
                }
                else
                {
                    properties.CharterMember = Utils.bytesWithTrailingNullByteToString(reply.PropertiesData.CharterMember);
                }
                properties.Flags = ProfileFlags.get(reply.PropertiesData.Flags);
                properties.ProfileURL = Utils.bytesWithTrailingNullByteToString(reply.PropertiesData.ProfileURL);

                onAvatarPropertiesReply.raiseEvent(new AvatarPropertiesReplyEventArgs(reply.AgentData.AvatarID, properties));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarInterestsHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onAvatarInterestsReply != null)
            {
                Packet packet = e.getPacket();

                AvatarInterestsReplyPacket airp = (AvatarInterestsReplyPacket)packet;
                Avatar.Interests interests = new Avatar.Interests();

                interests.WantToMask = airp.PropertiesData.WantToMask;
                interests.WantToText = Utils.bytesWithTrailingNullByteToString(airp.PropertiesData.WantToText);
                interests.SkillsMask = airp.PropertiesData.SkillsMask;
                interests.SkillsText = Utils.bytesWithTrailingNullByteToString(airp.PropertiesData.SkillsText);
                interests.LanguagesText = Utils.bytesWithTrailingNullByteToString(airp.PropertiesData.LanguagesText);

                onAvatarInterestsReply.raiseEvent(new AvatarInterestsReplyEventArgs(airp.AgentData.AvatarID, interests));
            }
        }

        /// <summary>
        /// EQ Message fired when someone nearby changes their display name
        /// </summary>
        /// <param name="capsKey">The message key</param>
        /// <param name="message">the IMessage object containing the deserialized data sent from the simulator</param>
        /// <param name="simulator">The <see cref="Simulator"/> which originated the packet</param>
        protected void DisplayNameUpdateMessageHandler(String capsKey, IMessage message, Simulator simulator)
        {
            if (onDisplayNameUpdate != null)
            {
                DisplayNameUpdateMessage msg = (DisplayNameUpdateMessage) message;
                onDisplayNameUpdate.raiseEvent(new DisplayNameUpdateEventArgs(msg.OldDisplayName, msg.DisplayName));
            }
        }

        /// <summary>
        /// Crossed region handler for message that comes across the EventQueue. Sent to an agent
        /// when the agent crosses a sim border into a new region.
        /// </summary>
        /// <param name="capsKey">The message key</param>
        /// <param name="message">the IMessage object containing the deserialized data sent from the simulator</param>
        /// <param name="simulator">The <see cref="Simulator"/> which originated the packet</param>
        protected void AvatarGroupsReplyMessageHandler(String capsKey, IMessage message, Simulator simulator)
        {
            AgentGroupDataUpdateMessage msg = (AgentGroupDataUpdateMessage)message;
            List<AvatarGroup> avatarGroups = new ArrayList<AvatarGroup>(msg.GroupDataBlock.length);
            for (int i = 0; i < msg.GroupDataBlock.length; i++)
            {
                AvatarGroup avatarGroup = new AvatarGroup();
                avatarGroup.AcceptNotices = msg.GroupDataBlock[i].AcceptNotices;
                avatarGroup.GroupID = msg.GroupDataBlock[i].GroupID;
                avatarGroup.GroupInsigniaID = msg.GroupDataBlock[i].GroupInsigniaID;
                avatarGroup.GroupName = msg.GroupDataBlock[i].GroupName;
                avatarGroup.GroupPowers = msg.GroupDataBlock[i].GroupPowers;
                avatarGroup.ListInProfile = msg.NewGroupDataBlock[i].ListInProfile;

                avatarGroups.add(avatarGroup);
            }

            onAvatarGroupsReply.raiseEvent(new AvatarGroupsReplyEventArgs(msg.AgentID, avatarGroups));
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarGroupsReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onAvatarGroupsReply != null)
            {
                Packet packet = e.getPacket();
                AvatarGroupsReplyPacket groups = (AvatarGroupsReplyPacket)packet;
                List<AvatarGroup> avatarGroups = new ArrayList<AvatarGroup>(groups.GroupData.length);

                for (int i = 0; i < groups.GroupData.length; i++)
                {
                    AvatarGroup avatarGroup = new AvatarGroup();

                    avatarGroup.AcceptNotices = groups.GroupData[i].AcceptNotices;
                    avatarGroup.GroupID = groups.GroupData[i].GroupID;
                    avatarGroup.GroupInsigniaID = groups.GroupData[i].GroupInsigniaID;
                    avatarGroup.GroupName = Utils.bytesWithTrailingNullByteToString(groups.GroupData[i].GroupName);
                    avatarGroup.GroupPowers = GroupPowers.get(groups.GroupData[i].GroupPowers.longValue());
                    avatarGroup.GroupTitle = Utils.bytesWithTrailingNullByteToString(groups.GroupData[i].GroupTitle);
                    avatarGroup.ListInProfile = groups.NewGroupData.ListInProfile;

                    avatarGroups.add(avatarGroup);
                }

                onAvatarGroupsReply.raiseEvent(new AvatarGroupsReplyEventArgs(groups.AgentData.AvatarID, avatarGroups));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarPickerReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onAvatarPickerReply != null)
            {
                Packet packet = e.getPacket();
                AvatarPickerReplyPacket reply = (AvatarPickerReplyPacket)packet;
                Map<UUID, String> avatars = new HashMap<UUID, String>();

                for (AvatarPickerReplyPacket.DataBlock block : reply.Data)
                {
                    avatars.put(block.AvatarID,  Utils.bytesWithTrailingNullByteToString(block.FirstName) +
                        " " + Utils.bytesWithTrailingNullByteToString(block.LastName));
                }
                onAvatarPickerReply.raiseEvent(new AvatarPickerReplyEventArgs(reply.AgentData.QueryID, avatars));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void ViewerEffectHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();
            ViewerEffectPacket effect = (ViewerEffectPacket)packet;

            for (ViewerEffectPacket.EffectBlock block : effect.Effect)
            {
                EffectType type = EffectType.get(block.Type);

                // Each ViewerEffect type uses it's own custom binary format for additional data. Fun eh?
                switch (type)
                {
                    case Text:
                        JLogger.warn("Received a ViewerEffect of type " + type.toString() + ", implement me!");
                        break;
                    case Icon:
                        JLogger.warn("Received a ViewerEffect of type " + type.toString() + ", implement me!");
                        break;
                    case Connector:
                        JLogger.warn("Received a ViewerEffect of type " + type.toString() + ", implement me!");
                        break;
                    case FlexibleObject:
                        JLogger.warn("Received a ViewerEffect of type " + type.toString() + ", implement me!");
                        break;
                    case AnimalControls:
                        JLogger.warn("Received a ViewerEffect of type " + type.toString() + ", implement me!");
                        break;
                    case AnimationObject:
                        JLogger.warn("Received a ViewerEffect of type " + type.toString() + ", implement me!");
                        break;
                    case Cloth:
                        JLogger.warn("Received a ViewerEffect of type " + type.toString() + ", implement me!");
                        break;
                    case Glow:
                        JLogger.warn("Received a Glow ViewerEffect which is not implemented yet");
                        break;
                    case Beam:
                    case Point:
                    case Trail:
                    case Sphere:
                    case Spiral:
                    case Edit:
                        if (onViewerEffect != null)
                        {
                            if (block.TypeData.length == 56)
                            {
                                UUID sourceAvatar = new UUID(block.TypeData, 0);
                                UUID targetObject = new UUID(block.TypeData, 16);
                                Vector3d targetPos = new Vector3d(block.TypeData, 32);
                                onViewerEffect.raiseEvent(new ViewerEffectEventArgs(type, sourceAvatar, targetObject, targetPos, block.Duration, block.ID));
                            }
                            else
                            {
                                JLogger.warn("Received a " + type.toString() +
                                    " ViewerEffect with an incorrect TypeData size of " +
                                    block.TypeData.length + " bytes");
                            }
                        }
                        break;
                    case LookAt:
                        if (onViewerEffectLookAt != null)
                        {
                            if (block.TypeData.length == 57)
                            {
                                UUID sourceAvatar = new UUID(block.TypeData, 0);
                                UUID targetObject = new UUID(block.TypeData, 16);
                                Vector3d targetPos = new Vector3d(block.TypeData, 32);
                                LookAtType lookAt = LookAtType.get(block.TypeData[56]);

                                onViewerEffectLookAt.raiseEvent(new ViewerEffectLookAtEventArgs(sourceAvatar, targetObject, targetPos, lookAt,
                                    block.Duration, block.ID));
                            }
                            else
                            {
                                JLogger.warn("Received a LookAt ViewerEffect with an incorrect TypeData size of " +
                                    block.TypeData.length + " bytes");
                            }
                        }
                        break;
                    case PointAt:
                        if (onViewerEffectPointAt != null)
                        {
                            if (block.TypeData.length == 57)
                            {
                                UUID sourceAvatar = new UUID(block.TypeData, 0);
                                UUID targetObject = new UUID(block.TypeData, 16);
                                Vector3d targetPos = new Vector3d(block.TypeData, 32);
                                PointAtType pointAt = PointAtType.get(block.TypeData[56]);

                                onViewerEffectPointAt.raiseEvent(new ViewerEffectPointAtEventArgs(e.getSimulator(), sourceAvatar, targetObject, targetPos,
                                    pointAt, block.Duration, block.ID));
                            }
                            else
                            {
                                JLogger.warn("Received a PointAt ViewerEffect with an incorrect TypeData size of " +
                                    block.TypeData.length + " bytes");
                            }
                        }
                        break;
                    default:
                        JLogger.warn("Received a ViewerEffect with an unknown type " + type);
                        break;
                }
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarPicksReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onAvatarPicksReply == null)
            {
                return;
            }
            Packet packet = e.getPacket();

            AvatarPicksReplyPacket p = (AvatarPicksReplyPacket)packet;
            Map<UUID, String> picks = new HashMap<UUID, String>();

            for (AvatarPicksReplyPacket.DataBlock b : p.Data)
            {
                picks.put(b.PickID, Utils.bytesWithTrailingNullByteToString(b.PickName));
            }

            onAvatarPicksReply.raiseEvent(new AvatarPicksReplyEventArgs(p.AgentData.TargetID, picks));
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void PickInfoReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onPickInfoReply != null)
            {
                Packet packet = e.getPacket();
                PickInfoReplyPacket p = (PickInfoReplyPacket)packet;
                ProfilePick ret = new ProfilePick();
                ret.CreatorID = p.Data.CreatorID;
                ret.Desc = Utils.bytesWithTrailingNullByteToString(p.Data.Desc);
                ret.Enabled = p.Data.Enabled;
                ret.Name = Utils.bytesWithTrailingNullByteToString(p.Data.Name);
                ret.OriginalName = Utils.bytesWithTrailingNullByteToString(p.Data.OriginalName);
                ret.ParcelID = p.Data.ParcelID;
                ret.PickID = p.Data.PickID;
                ret.PosGlobal = p.Data.PosGlobal;
                ret.SimName = Utils.bytesWithTrailingNullByteToString(p.Data.SimName);
                ret.SnapshotID = p.Data.SnapshotID;
                ret.SortOrder = p.Data.SortOrder;
                ret.TopPick = p.Data.TopPick;
                ret.User = Utils.bytesWithTrailingNullByteToString(p.Data.User);

                onPickInfoReply.raiseEvent(new PickInfoReplyEventArgs(ret.PickID, ret));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AvatarClassifiedReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onAvatarClassifiedReply != null)
            {
                Packet packet = e.getPacket();
                AvatarClassifiedReplyPacket p = (AvatarClassifiedReplyPacket)packet;
                Map<UUID, String> classifieds = new HashMap<UUID, String>();

                for (AvatarClassifiedReplyPacket.DataBlock b : p.Data)
                {
                    classifieds.put(b.ClassifiedID, Utils.bytesWithTrailingNullByteToString(b.Name));
                }

                onAvatarClassifiedReply.raiseEvent(new AvatarClassifiedReplyEventArgs(p.AgentData.TargetID, classifieds));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void ClassifiedInfoReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onAvatarClassifiedReply != null)
            {
                Packet packet = e.getPacket();
                ClassifiedInfoReplyPacket p = (ClassifiedInfoReplyPacket)packet;
                ClassifiedAd ret = new ClassifiedAd();
                ret.Desc = Utils.bytesWithTrailingNullByteToString(p.Data.Desc);
                ret.Name = Utils.bytesWithTrailingNullByteToString(p.Data.Name);
                ret.ParcelID = p.Data.ParcelID;
                ret.ClassifiedID = p.Data.ClassifiedID;
                ret.Position = p.Data.PosGlobal;
                ret.SnapShotID = p.Data.SnapshotID;
                ret.Price = p.Data.PriceForListing;
                ret.ParentEstate = p.Data.ParentEstate;
                ret.ClassifiedFlags = p.Data.ClassifiedFlags;
                ret.Catagory = p.Data.Category;

                onClassifiedInfoReply.raiseEvent(new ClassifiedInfoReplyEventArgs(ret.ClassifiedID, ret));
            }
        }

        //endregion Packet Handlers
}
