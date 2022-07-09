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
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;

import com.ngt.jopenmetaverse.shared.protocol.ActivateGroupPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentDataUpdateRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentDropGroupPacket;
import com.ngt.jopenmetaverse.shared.protocol.CreateGroupReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.CreateGroupRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.EjectGroupMemberReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.EjectGroupMemberRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupAccountSummaryReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupAccountSummaryRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupMembersReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupMembersRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupNoticeRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupNoticesListReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupNoticesListRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupProfileReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupProfileRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupRoleChangesPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupRoleDataReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupRoleDataRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupRoleMembersReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupRoleMembersRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupRoleUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupTitleUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupTitlesReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.GroupTitlesRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.InviteGroupRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.JoinGroupReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.JoinGroupRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.LeaveGroupReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.LeaveGroupRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.SetGroupAcceptNoticesPacket;
import com.ngt.jopenmetaverse.shared.protocol.SetGroupContributionPacket;
import com.ngt.jopenmetaverse.shared.protocol.StartGroupProposalPacket;
import com.ngt.jopenmetaverse.shared.protocol.UUIDGroupNameReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.UUIDGroupNameRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.UpdateGroupInfoPacket;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageDialog;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.InstantMessageOnline;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventObservableArg;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.InstantMessageEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.group.*;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.AgentDropGroupMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.AgentGroupDataUpdateMessage;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.XmlLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.types.Enums;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

///// <summary>
///// Handles all network traffic related to reading and writing group
///// information
///// </summary>
public class GroupManager {
    //region Structs

    /// <summary>
    /// Avatar group management
    /// </summary>
    public class GroupMember
    {
        /// <summary>Key of Group Member</summary>
        public UUID ID;
        /// <summary>Total land contribution</summary>
        public int Contribution;
        /// <summary>Online status information</summary>
        public String OnlineStatus;
        /// <summary>Abilities that the Group Member has</summary>
        public EnumSet<GroupPowers> Powers;
        /// <summary>Current group title</summary>
        public String Title;
        /// <summary>Is a group owner</summary>
        public boolean IsOwner;
    }

    /// <summary>
    /// Role manager for a group
    /// </summary>
    public class GroupRole
    {
        /// <summary>Key of the group</summary>
        public UUID GroupID;
        /// <summary>Key of Role</summary>
        public UUID ID;
        /// <summary>Name of Role</summary>
        public String Name;
        /// <summary>Group Title associated with Role</summary>
        public String Title;
        /// <summary>Description of Role</summary>
        public String Description;
        /// <summary>Abilities Associated with Role</summary>
        public EnumSet<GroupPowers> Powers;
        /// <summary>Returns the role's title</summary>
        /// <returns>The role's title</returns>
        @Override
        public String toString()
        {
            return Name;
        }
    }

    /// <summary>
    /// Class to represent Group Title
    /// </summary>
    public class GroupTitle
    {
        /// <summary>Key of the group</summary>
        public UUID GroupID;
        /// <summary>ID of the role title belongs to</summary>
        public UUID RoleID;
        /// <summary>Group Title</summary>
        
        public String Title;
        /// <summary>Whether title is Active</summary>
        public boolean Selected;
        /// <summary>Returns group title</summary>
        @Override
        public String toString()
        {
            return Title;
        }
    }

    /// <summary>
    /// Represents a group on the grid
    /// </summary>
    public class Group
    {
        /// <summary>Key of Group</summary>
        public UUID ID;
        /// <summary>Key of Group Insignia</summary>
        public UUID InsigniaID;
        /// <summary>Key of Group Founder</summary>
        public UUID FounderID;
        /// <summary>Key of Group Role for Owners</summary>
        public UUID OwnerRole;
        /// <summary>Name of Group</summary>
        public String Name;
        /// <summary>Text of Group Charter</summary>
        public String Charter;
        /// <summary>Title of "everyone" role</summary>
        public String MemberTitle;
        /// <summary>Is the group open for enrolement to everyone</summary>
        public boolean OpenEnrollment;
        /// <summary>Will group show up in search</summary>
        public boolean ShowInList;
        /// <summary></summary>
        public EnumSet<GroupPowers> Powers;
        /// <summary></summary>
        public boolean AcceptNotices;
        /// <summary></summary>
        public boolean AllowPublish;
        /// <summary>Is the group Mature</summary>
        public boolean MaturePublish;
        /// <summary>Cost of group membership</summary>
        public int MembershipFee;
        /// <summary></summary>
        public int Money;
        /// <summary></summary>
        public int Contribution;
        /// <summary>The total number of current members this group has</summary>
        public int GroupMembershipCount;
        /// <summary>The number of roles this group has configured</summary>
        public int GroupRolesCount;
        /// <summary>Show this group in agent's profile</summary>
        public boolean ListInProfile;

        /// <summary>Returns the name of the group</summary>
        /// <returns>A String containing the name of the group</returns>
        @Override
        public String toString()
        {
            return Name;
        }
    }

    /// <summary>
    /// A group Vote
    /// </summary>
    public class Vote
    {
        /// <summary>Key of Avatar who created Vote</summary>
        public UUID Candidate;
        /// <summary>Text of the Vote proposal</summary>
        public String VoteString;
        /// <summary>Total number of votes</summary>
        public int NumVotes;
    }

    /// <summary>
    /// A group proposal
    /// </summary>
    public class GroupProposal
    {
        /// <summary>The Text of the proposal</summary>
        public String VoteText;
        /// <summary>The minimum number of members that must vote before proposal passes or failes</summary>
        public int Quorum;
        /// <summary>The required ration of yes/no votes required for vote to pass</summary>
        /// <remarks>The three options are Simple Majority, 2/3 Majority, and Unanimous</remarks>
        /// TODO: this should be an enum
        public float Majority;
        /// <summary>The duration in days votes are accepted</summary>
        public int Duration;
    }

    /// <summary>
    /// 
    /// </summary>
    public class GroupAccountSummary
    {
        /// <summary></summary>
        public int IntervalDays;
        /// <summary></summary>
        public int CurrentInterval;
        /// <summary></summary>
        public String StartDate;
        /// <summary></summary>
        public int Balance;
        /// <summary></summary>
        public int TotalCredits;
        /// <summary></summary>
        public int TotalDebits;
        /// <summary></summary>
        public int ObjectTaxCurrent;
        /// <summary></summary>
        public int LightTaxCurrent;
        /// <summary></summary>
        public int LandTaxCurrent;
        /// <summary></summary>
        public int GroupTaxCurrent;
        /// <summary></summary>
        public int ParcelDirFeeCurrent;
        /// <summary></summary>
        public int ObjectTaxEstimate;
        /// <summary></summary>
        public int LightTaxEstimate;
        /// <summary></summary>
        public int LandTaxEstimate;
        /// <summary></summary>
        public int GroupTaxEstimate;
        /// <summary></summary>
        public int ParcelDirFeeEstimate;
        /// <summary></summary>
        public int NonExemptMembers;
        /// <summary></summary>
        public String LastTaxDate;
        /// <summary></summary>
        public String TaxDate;
    }

    /// <summary>
    /// Struct representing a group notice
    /// </summary>
    public class GroupNotice
    {
        /// <summary></summary>
        public String Subject;
        /// <summary></summary>
        public String Message;
        /// <summary></summary>
        public UUID AttachmentID;
        /// <summary></summary>
        public UUID OwnerID;

        /// <summary>
        /// 
        /// </summary>
        /// <returns></returns>
        public byte[] SerializeAttachment() throws Exception
        {
            if (OwnerID == UUID.Zero || AttachmentID == UUID.Zero)
                return Utils.EmptyBytes;

            OSDMap att = new OSDMap();
            att.put("item_id", OSD.FromUUID(AttachmentID));
            att.put("owner_id", OSD.FromUUID(OwnerID));

            return XmlLLSDOSDParser.SerializeLLSDXmlBytes(att);

            /*
            //I guess this is how this works, no gaurentees
            String lsd = "<llsd><item_id>" + AttachmentID.ToString() + "</item_id><owner_id>"
                + OwnerID.ToString() + "</owner_id></llsd>";
            return Utils.stringToBytesWithTrailingNullByte(lsd);
             */
        }
    }

    /// <summary>
    /// Struct representing a group notice list entry
    /// </summary>
    public class GroupNoticesListEntry
    {
        /// <summary>Notice ID</summary>
        public UUID NoticeID;
        /// <summary>Creation timestamp of notice</summary>
        //uint
        public long Timestamp;
        /// <summary>Agent name who created notice</summary>
        public String FromName;
        /// <summary>Notice subject</summary>
        public String Subject;
        /// <summary>Is there an attachment?</summary>
        public boolean HasAttachment;
        /// <summary>Attachment Type</summary>
        public Enums.AssetType AssetType;

    }

    /// <summary>
    /// Struct representing a member of a group chat session and their settings
    /// </summary>
    public static class ChatSessionMember
    {
        /// <summary>The <see cref="UUID"/> of the Avatar</summary>
        public UUID AvatarKey;
        /// <summary>True if user has voice chat enabled</summary>
        public boolean CanVoiceChat;
        /// <summary>True of Avatar has moderator abilities</summary>
        public boolean IsModerator;
        /// <summary>True if a moderator has muted this avatars chat</summary>
        public boolean MuteText;
        /// <summary>True if a moderator has muted this avatars voice</summary>
        public boolean MuteVoice;
    }

    //endregion Structs

    //region Enums

    /// <summary>
    /// Role update flags
    /// </summary>
    public enum GroupRoleUpdate
    {
        /// <summary></summary>
        NoUpdate((long)0),
        /// <summary></summary>
        UpdateData((long)1),
        /// <summary></summary>
        UpdatePowers((long)2),
        /// <summary></summary>
        UpdateAll((long)3),
        /// <summary></summary>
        Create((long)4),
        /// <summary></summary>
        Delete((long)5);
        private long index;
        GroupRoleUpdate(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,GroupRoleUpdate> lookup  = new HashMap<Long,GroupRoleUpdate>();

		static {
			for(GroupRoleUpdate s : EnumSet.allOf(GroupRoleUpdate.class))
				lookup.put(s.getIndex(), s);
		}

		public static GroupRoleUpdate get(Long index)
		{
			return lookup.get(index);
		}
    }

//    [Flags]
    public enum GroupPowers
    {
    	//ulong
        /// <summary></summary>
        None((long)0),

        // Membership
        /// <summary>Can send invitations to groups default role</summary>
        Invite((long)1L << 1),
        /// <summary>Can eject members from group</summary>
        Eject((long)1L << 2),
        /// <summary>Can toggle 'Open Enrollment' and change 'Signup fee'</summary>
        ChangeOptions((long)1L << 3),
        /// <summary>Member is visible in the public member list</summary>
        MemberVisible((long)1L << 47),

        // Roles
        /// <summary>Can create new roles</summary>
        CreateRole((long)1L << 4),
        /// <summary>Can delete existing roles</summary>
        DeleteRole((long)1L << 5),
        /// <summary>Can change Role names, titles and descriptions</summary>
        RoleProperties((long)1L << 6),
        /// <summary>Can assign other members to assigners role</summary>
        AssignMemberLimited((long)1L << 7),
        /// <summary>Can assign other members to any role</summary>
        AssignMember((long)1L << 8),
        /// <summary>Can remove members from roles</summary>
        RemoveMember((long)1L << 9),
        /// <summary>Can assign and remove abilities in roles</summary>
        ChangeActions((long)1L << 10),

        // Identity
        /// <summary>Can change group Charter, Insignia, 'Publish on the web' and which
        /// members are publicly visible in group member listings</summary>
        ChangeIdentity((long)1L << 11),

        // Parcel management
        /// <summary>Can buy land or deed land to group</summary>
        LandDeed((long)1L << 12),
        /// <summary>Can abandon group owned land to Governor Linden on mainland, or Estate owner for
        /// private estates</summary>
        LandRelease((long)1L << 13),
        /// <summary>Can set land for-sale information on group owned parcels</summary>
        LandSetSale((long)1L << 14),
        /// <summary>Can subdivide and join parcels</summary>
        LandDivideJoin((long)1L << 15),


        // Chat
        /// <summary>Can join group chat sessions</summary>
        JoinChat((long)1L << 16),
        /// <summary>Can use voice chat in Group Chat sessions</summary>
        AllowVoiceChat((long)1L << 27),
        /// <summary>Can moderate group chat sessions</summary>
        ModerateChat((long)1L << 37),

        // Parcel identity
        /// <summary>Can toggle "Show in Find Places" and set search category</summary>
        FindPlaces((long)1L << 17),
        /// <summary>Can change parcel name, description, and 'Publish on web' settings</summary>
        LandChangeIdentity((long)1L << 18),
        /// <summary>Can set the landing point and teleport routing on group land</summary>
        SetLandingPoint((long)1L << 19),

        // Parcel settings
        /// <summary>Can change music and media settings</summary>
        ChangeMedia((long)1L << 20),
        /// <summary>Can toggle 'Edit Terrain' option in Land settings</summary>
        LandEdit((long)1L << 21),
        /// <summary>Can toggle various About Land > Options settings</summary>
        LandOptions((long)1L << 22),

        // Parcel powers
        /// <summary>Can always terraform land, even if parcel settings have it turned off</summary>
        AllowEditLand((long)1L << 23),
        /// <summary>Can always fly while over group owned land</summary>
        AllowFly((long)1L << 24),
        /// <summary>Can always rez objects on group owned land</summary>
        AllowRez((long)1L << 25),
        /// <summary>Can always create landmarks for group owned parcels</summary>
        AllowLandmark((long)1L << 26),
        /// <summary>Can set home location on any group owned parcel</summary>
        AllowSetHome((long)1L << 28),


        // Parcel access
        /// <summary>Can modify public access settings for group owned parcels</summary>
        LandManageAllowed((long)1L << 29),
        /// <summary>Can manager parcel ban lists on group owned land</summary>
        LandManageBanned((long)1L << 30),
        /// <summary>Can manage pass list sales information</summary>
        LandManagePasses((long)1L << 31),
        /// <summary>Can eject and freeze other avatars on group owned land</summary>
        LandEjectAndFreeze((long)1L << 32),

        // Parcel content
        /// <summary>Can return objects set to group</summary>
        ReturnGroupSet((long)1L << 33),
        /// <summary>Can return non-group owned/set objects</summary>
        ReturnNonGroup((long)1L << 34),
        /// <summary>Can return group owned objects</summary>
        ReturnGroupOwned((long)1L << 48),

        /// <summary>Can landscape using Linden plants</summary>
        LandGardening((long)1L << 35),

        // Objects
        /// <summary>Can deed objects to group</summary>
        DeedObject((long)1L << 36),
        /// <summary>Can move group owned objects</summary>
        ObjectManipulate((long)1L << 38),
        /// <summary>Can set group owned objects for-sale</summary>
        ObjectSetForSale((long)1L << 39),

        /// <summary>Pay group liabilities and receive group dividends</summary>
        Accountable((long)1L << 40),

        // Notices and proposals
        /// <summary>Can send group notices</summary>
        SendNotices((long)1L << 42),
        /// <summary>Can receive group notices</summary>
        ReceiveNotices((long)1L << 43),
        /// <summary>Can create group proposals</summary>
        StartProposal((long)1L << 44),
        /// <summary>Can vote on group proposals</summary>
        VoteOnProposal((long)1L << 45);
    	private long index;
    	GroupPowers(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}
		
		private static final Map<Long,GroupPowers> lookup  = new HashMap<Long,GroupPowers>();
		
		static {
			for(GroupPowers s : EnumSet.allOf(GroupPowers.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<GroupPowers> get(Long index)
		{
			EnumSet<GroupPowers> enumsSet = EnumSet.allOf(GroupPowers.class);
			for(Entry<Long,GroupPowers> entry: lookup.entrySet())
			{
				if((entry.getKey().longValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}
		
		public static long getIndex(EnumSet<GroupPowers> enumSet)
		{
			long ret = 0;
			for(GroupPowers s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}
    }

    //endregion Enums

//        //region Delegates


    private EventObservable<CurrentGroupsEventArgs> onCurrentGroups = new EventObservable<CurrentGroupsEventArgs>();
    public void registerOnCurrentGroups(EventObserver<CurrentGroupsEventArgs> o)
    {
    	onCurrentGroups.addObserver(o);
    }
    public void unregisterOnCurrentGroups(EventObserver<CurrentGroupsEventArgs> o) 
    {
    	onCurrentGroups.deleteObserver(o);
    }
    private EventObservable<GroupNamesEventArgs> onGroupNamesReply = new EventObservable<GroupNamesEventArgs>();
    public void registerOnGroupNamesReply(EventObserver<GroupNamesEventArgs> o)
    {
    	onGroupNamesReply.addObserver(o);
    }
    public void unregisterOnGroupNamesReply(EventObserver<GroupNamesEventArgs> o) 
    {
    	onGroupNamesReply.deleteObserver(o);
    }
    private EventObservable<GroupProfileEventArgs> onGroupProfile = new EventObservable<GroupProfileEventArgs>();
    public void registerOnGroupProfile(EventObserver<GroupProfileEventArgs> o)
    {
    	onGroupProfile.addObserver(o);
    }
    public void unregisterOnGroupProfile(EventObserver<GroupProfileEventArgs> o) 
    {
    	onGroupProfile.deleteObserver(o);
    }
    private EventObservable<GroupMembersReplyEventArgs> onGroupMembersReply = new EventObservable<GroupMembersReplyEventArgs>();
    public void registerOnGroupMembersReply(EventObserver<GroupMembersReplyEventArgs> o)
    {
    	onGroupMembersReply.addObserver(o);
    }
    public void unregisterOnGroupMembersReply(EventObserver<GroupMembersReplyEventArgs> o) 
    {
    	onGroupMembersReply.deleteObserver(o);
    }
    private EventObservable<GroupRolesDataReplyEventArgs> onGroupRoleDataReply = new EventObservable<GroupRolesDataReplyEventArgs>();
    public void registerOnGroupRoleDataReply(EventObserver<GroupRolesDataReplyEventArgs> o)
    {
    	onGroupRoleDataReply.addObserver(o);
    }
    public void unregisterOnGroupRoleDataReply(EventObserver<GroupRolesDataReplyEventArgs> o) 
    {
    	onGroupRoleDataReply.deleteObserver(o);
    }
    private EventObservable<GroupRolesMembersReplyEventArgs> onGroupRoleMembersReply = new EventObservable<GroupRolesMembersReplyEventArgs>();
    public void registerOnGroupRoleMembersReply(EventObserver<GroupRolesMembersReplyEventArgs> o)
    {
    	onGroupRoleMembersReply.addObserver(o);
    }
    public void unregisterOnGroupRoleMembersReply(EventObserver<GroupRolesMembersReplyEventArgs> o) 
    {
    	onGroupRoleMembersReply.deleteObserver(o);
    }
    private EventObservable<GroupTitlesReplyEventArgs> onGroupTitlesReply = new EventObservable<GroupTitlesReplyEventArgs>();
    public void registerOnGroupTitlesReply(EventObserver<GroupTitlesReplyEventArgs> o)
    {
    	onGroupTitlesReply.addObserver(o);
    }
    public void unregisterOnGroupTitlesReply(EventObserver<GroupTitlesReplyEventArgs> o) 
    {
    	onGroupTitlesReply.deleteObserver(o);
    }
    private EventObservable<GroupAccountSummaryReplyEventArgs> onGroupAccountSummaryReply = new EventObservable<GroupAccountSummaryReplyEventArgs>();
    public void registerOnGroupAccountSummaryReply(EventObserver<GroupAccountSummaryReplyEventArgs> o)
    {
    	onGroupAccountSummaryReply.addObserver(o);
    }
    public void unregisterOnGroupAccountSummaryReply(EventObserver<GroupAccountSummaryReplyEventArgs> o) 
    {
    	onGroupAccountSummaryReply.deleteObserver(o);
    }
    private EventObservable<GroupCreatedReplyEventArgs> onGroupCreatedReply = new EventObservable<GroupCreatedReplyEventArgs>();
    public void registerOnGroupCreatedReply(EventObserver<GroupCreatedReplyEventArgs> o)
    {
    	onGroupCreatedReply.addObserver(o);
    }
    public void unregisterOnGroupCreatedReply(EventObserver<GroupCreatedReplyEventArgs> o) 
    {
    	onGroupCreatedReply.deleteObserver(o);
    }
    private EventObservable<GroupOperationEventArgs> onGroupJoinedReply = new EventObservable<GroupOperationEventArgs>();
    public void registerOnGroupJoinedReply(EventObserver<GroupOperationEventArgs> o)
    {
    	onGroupJoinedReply.addObserver(o);
    }
    public void unregisterOnGroupJoinedReply(EventObserver<GroupOperationEventArgs> o) 
    {
    	onGroupJoinedReply.deleteObserver(o);
    }
    private EventObservable<GroupOperationEventArgs> onGroupLeaveReply = new EventObservable<GroupOperationEventArgs>();
    public void registerOnGroupLeaveReply(EventObserver<GroupOperationEventArgs> o)
    {
    	onGroupLeaveReply.addObserver(o);
    }
    public void unregisterOnGroupLeaveReply(EventObserver<GroupOperationEventArgs> o) 
    {
    	onGroupLeaveReply.deleteObserver(o);
    }
    private EventObservable<GroupDroppedEventArgs> onGroupDropped = new EventObservable<GroupDroppedEventArgs>();
    public void registerOnGroupDropped(EventObserver<GroupDroppedEventArgs> o)
    {
    	onGroupDropped.addObserver(o);
    }
    public void unregisterOnGroupDropped(EventObserver<GroupDroppedEventArgs> o) 
    {
    	onGroupDropped.deleteObserver(o);
    }
    private EventObservable<GroupOperationEventArgs> onGroupMemberEjected = new EventObservable<GroupOperationEventArgs>();
    public void registerOnGroupMemberEjected(EventObserver<GroupOperationEventArgs> o)
    {
    	onGroupMemberEjected.addObserver(o);
    }
    public void unregisterOnGroupMemberEjected(EventObserver<GroupOperationEventArgs> o) 
    {
    	onGroupMemberEjected.deleteObserver(o);
    }
    private EventObservable<GroupNoticesListReplyEventArgs> onGroupNoticesListReply = new EventObservable<GroupNoticesListReplyEventArgs>();
    public void registerOnGroupNoticesListReply(EventObserver<GroupNoticesListReplyEventArgs> o)
    {
    	onGroupNoticesListReply.addObserver(o);
    }
    public void unregisterOnGroupNoticesListReply(EventObserver<GroupNoticesListReplyEventArgs> o) 
    {
    	onGroupNoticesListReply.deleteObserver(o);
    }
    private EventObservable<GroupInvitationEventArgs> onGroupInvitation = new EventObservable<GroupInvitationEventArgs>();
    public void registerOnGroupInvitation(EventObserver<GroupInvitationEventArgs> o)
    {
    	onGroupInvitation.addObserver(o);
    }
    public void unregisterOnGroupInvitation(EventObserver<GroupInvitationEventArgs> o) 
    {
    	onGroupInvitation.deleteObserver(o);
    }
    
    
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<CurrentGroupsEventArgs> m_CurrentGroups;
//
//        /// <summary>Raises the CurrentGroups event</summary>
//        /// <param name="e">A CurrentGroupsEventArgs object containing the
//        /// data sent from the simulator</param>
//        protected virtual void OnCurrentGroups(CurrentGroupsEventArgs e)
//        {
//            EventHandler<CurrentGroupsEventArgs> handler = m_CurrentGroups;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_CurrentGroupsLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// our current group membership</summary>
//        public event EventHandler<CurrentGroupsEventArgs> CurrentGroups 
//        {
//            add { lock (m_CurrentGroupsLock) { m_CurrentGroups += value; } }
//            remove { lock (m_CurrentGroupsLock) { m_CurrentGroups -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupNamesEventArgs> m_GroupNames;
//
//        /// <summary>Raises the GroupNamesReply event</summary>
//        /// <param name="e">A GroupNamesEventArgs object containing the
//        /// data response from the simulator</param>
//        protected virtual void OnGroupNamesReply(GroupNamesEventArgs e)
//        {
//            EventHandler<GroupNamesEventArgs> handler = m_GroupNames;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupNamesLock = new object();
//
//        /// <summary>Raised when the simulator responds to a RequestGroupName 
//        /// or RequestGroupNames request</summary>
//        public event EventHandler<GroupNamesEventArgs> GroupNamesReply 
//        {
//            add { lock (m_GroupNamesLock) { m_GroupNames += value; } }
//            remove { lock (m_GroupNamesLock) { m_GroupNames -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupProfileEventArgs> m_GroupProfile;
//
//        /// <summary>Raises the GroupProfile event</summary>
//        /// <param name="e">An GroupProfileEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupProfile(GroupProfileEventArgs e)
//        {
//            EventHandler<GroupProfileEventArgs> handler = m_GroupProfile;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupProfileLock = new object();
//
//        /// <summary>Raised when the simulator responds to a <see cref="RequestGroupProfile"/> request</summary>
//        public event EventHandler<GroupProfileEventArgs> GroupProfile 
//        {
//            add { lock (m_GroupProfileLock) { m_GroupProfile += value; } }
//            remove { lock (m_GroupProfileLock) { m_GroupProfile -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupMembersReplyEventArgs> m_GroupMembers;
//
//        /// <summary>Raises the GroupMembers event</summary>
//        /// <param name="e">A GroupMembersEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupMembersReply(GroupMembersReplyEventArgs e)
//        {
//            EventHandler<GroupMembersReplyEventArgs> handler = m_GroupMembers;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupMembersLock = new object();
//
//        /// <summary>Raised when the simulator responds to a <see cref="RequestGroupMembers"/> request</summary>
//        public event EventHandler<GroupMembersReplyEventArgs> GroupMembersReply 
//        {
//            add { lock (m_GroupMembersLock) { m_GroupMembers += value; } }
//            remove { lock (m_GroupMembersLock) { m_GroupMembers -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupRolesDataReplyEventArgs> m_GroupRoles;
//
//        /// <summary>Raises the GroupRolesDataReply event</summary>
//        /// <param name="e">A GroupRolesDataReplyEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupRoleDataReply(GroupRolesDataReplyEventArgs e)
//        {
//            EventHandler<GroupRolesDataReplyEventArgs> handler = m_GroupRoles;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupRolesLock = new object();
//
//        /// <summary>Raised when the simulator responds to a <see cref="RequestGroupRoleData"/> request</summary>
//        public event EventHandler<GroupRolesDataReplyEventArgs> GroupRoleDataReply 
//        {
//            add { lock (m_GroupRolesLock) { m_GroupRoles += value; } }
//            remove { lock (m_GroupRolesLock) { m_GroupRoles -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupRolesMembersReplyEventArgs> m_GroupRoleMembers;
//
//        /// <summary>Raises the GroupRoleMembersReply event</summary>
//        /// <param name="e">A GroupRolesRoleMembersReplyEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupRoleMembers(GroupRolesMembersReplyEventArgs e)
//        {
//            EventHandler<GroupRolesMembersReplyEventArgs> handler = m_GroupRoleMembers;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupRolesMembersLock = new object();
//
//        /// <summary>Raised when the simulator responds to a <see cref="RequestGroupRolesMembers"/> request</summary>
//        public event EventHandler<GroupRolesMembersReplyEventArgs> GroupRoleMembersReply 
//        {
//            add { lock (m_GroupRolesMembersLock) { m_GroupRoleMembers += value; } }
//            remove { lock (m_GroupRolesMembersLock) { m_GroupRoleMembers -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupTitlesReplyEventArgs> m_GroupTitles;
//
//
//        /// <summary>Raises the GroupTitlesReply event</summary>
//        /// <param name="e">A GroupTitlesReplyEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupTitles(GroupTitlesReplyEventArgs e)
//        {
//            EventHandler<GroupTitlesReplyEventArgs> handler = m_GroupTitles;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupTitlesLock = new object();
//
//        /// <summary>Raised when the simulator responds to a <see cref="RequestGroupTitles"/> request</summary>
//        public event EventHandler<GroupTitlesReplyEventArgs> GroupTitlesReply 
//        {
//            add { lock (m_GroupTitlesLock) { m_GroupTitles += value; } }
//            remove { lock (m_GroupTitlesLock) { m_GroupTitles -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupAccountSummaryReplyEventArgs> m_GroupAccountSummary;
//
//        /// <summary>Raises the GroupAccountSummary event</summary>
//        /// <param name="e">A GroupAccountSummaryReplyEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupAccountSummaryReply(GroupAccountSummaryReplyEventArgs e)
//        {
//            EventHandler<GroupAccountSummaryReplyEventArgs> handler = m_GroupAccountSummary;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupAccountSummaryLock = new object();
//
//        /// <summary>Raised when a response to a RequestGroupAccountSummary is returned
//        /// by the simulator</summary>
//        public event EventHandler<GroupAccountSummaryReplyEventArgs> GroupAccountSummaryReply 
//        {
//            add { lock (m_GroupAccountSummaryLock) { m_GroupAccountSummary += value; } }
//            remove { lock (m_GroupAccountSummaryLock) { m_GroupAccountSummary -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupCreatedReplyEventArgs> m_GroupCreated;
//
//        /// <summary>Raises the GroupCreated event</summary>
//        /// <param name="e">An GroupCreatedEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupCreatedReply(GroupCreatedReplyEventArgs e)
//        {
//            EventHandler<GroupCreatedReplyEventArgs> handler = m_GroupCreated;
//            if (handler != null)
//                handler(this, e);
//        }
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupCreatedLock = new object();
//
//        /// <summary>Raised when a request to create a group is successful</summary>
//        public event EventHandler<GroupCreatedReplyEventArgs> GroupCreatedReply 
//        {
//            add { lock (m_GroupCreatedLock) { m_GroupCreated += value; } }
//            remove { lock (m_GroupCreatedLock) { m_GroupCreated -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupOperationEventArgs> m_GroupJoined;
//
//        /// <summary>Raises the GroupJoined event</summary>
//        /// <param name="e">A GroupOperationEventArgs object containing the
//        /// result of the operation returned from the simulator</param>
//        protected virtual void OnGroupJoinedReply(GroupOperationEventArgs e)
//        {
//            EventHandler<GroupOperationEventArgs> handler = m_GroupJoined;
//            if (handler != null)
//                handler(this, e);
//        }
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupJoinedLock = new object();
//
//        /// <summary>Raised when a request to join a group either
//        /// fails or succeeds</summary>
//        public event EventHandler<GroupOperationEventArgs> GroupJoinedReply 
//        {
//            add { lock (m_GroupJoinedLock) { m_GroupJoined += value; } }
//            remove { lock (m_GroupJoinedLock) { m_GroupJoined -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupOperationEventArgs> m_GroupLeft;
//
//        /// <summary>Raises the GroupLeft event</summary>
//        /// <param name="e">A GroupOperationEventArgs object containing the
//        /// result of the operation returned from the simulator</param>
//        protected virtual void OnGroupLeaveReply(GroupOperationEventArgs e)
//        {
//            EventHandler<GroupOperationEventArgs> handler = m_GroupLeft;
//            if (handler != null)
//                handler(this, e);
//        }
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupLeftLock = new object();
//
//        /// <summary>Raised when a request to leave a group either
//        /// fails or succeeds</summary>
//        public event EventHandler<GroupOperationEventArgs> GroupLeaveReply 
//        {
//            add { lock (m_GroupLeftLock) { m_GroupLeft += value; } }
//            remove { lock (m_GroupLeftLock) { m_GroupLeft -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupDroppedEventArgs> m_GroupDropped;
//
//        /// <summary>Raises the GroupDropped event</summary>
//        /// <param name="e">An GroupDroppedEventArgs object containing the
//        /// the group your agent left</param>
//        protected virtual void OnGroupDropped(GroupDroppedEventArgs e)
//        {
//            EventHandler<GroupDroppedEventArgs> handler = m_GroupDropped;
//            if (handler != null)
//                handler(this, e);
//        }
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupDroppedLock = new object();
//
//        /// <summary>Raised when A group is removed from the group server</summary>
//        public event EventHandler<GroupDroppedEventArgs> GroupDropped 
//        {
//            add { lock (m_GroupDroppedLock) { m_GroupDropped += value; } }
//            remove { lock (m_GroupDroppedLock) { m_GroupDropped -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupOperationEventArgs> m_GroupMemberEjected;
//
//        /// <summary>Raises the GroupMemberEjected event</summary>
//        /// <param name="e">An GroupMemberEjectedEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupMemberEjected(GroupOperationEventArgs e)
//        {
//            EventHandler<GroupOperationEventArgs> handler = m_GroupMemberEjected;
//            if (handler != null)
//                handler(this, e);
//        }
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupMemberEjectedLock = new object();
//
//        /// <summary>Raised when a request to eject a member from a group either
//        /// fails or succeeds</summary>
//        public event EventHandler<GroupOperationEventArgs> GroupMemberEjected 
//        {
//            add { lock (m_GroupMemberEjectedLock) { m_GroupMemberEjected += value; } }
//            remove { lock (m_GroupMemberEjectedLock) { m_GroupMemberEjected -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupNoticesListReplyEventArgs> m_GroupNoticesListReply;
//
//        /// <summary>Raises the GroupNoticesListReply event</summary>
//        /// <param name="e">An GroupNoticesListReplyEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupNoticesListReply(GroupNoticesListReplyEventArgs e)
//        {
//            EventHandler<GroupNoticesListReplyEventArgs> handler = m_GroupNoticesListReply;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupNoticesListReplyLock = new object();
//
//        /// <summary>Raised when the simulator sends us group notices</summary>
//        /// <seealso cref="RequestGroupNoticesList"/>
//        public event EventHandler<GroupNoticesListReplyEventArgs> GroupNoticesListReply 
//        {
//            add { lock (m_GroupNoticesListReplyLock) { m_GroupNoticesListReply += value; } }
//            remove { lock (m_GroupNoticesListReplyLock) { m_GroupNoticesListReply -= value; } }
//        }
//
//        /// <summary>The event subscribers. null if no subcribers</summary>
//        private EventHandler<GroupInvitationEventArgs> m_GroupInvitation;
//
//        /// <summary>Raises the GroupInvitation event</summary>
//        /// <param name="e">An GroupInvitationEventArgs object containing the
//        /// data returned from the simulator</param>
//        protected virtual void OnGroupInvitation(GroupInvitationEventArgs e)
//        {
//            EventHandler<GroupInvitationEventArgs> handler = m_GroupInvitation;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private readonly object m_GroupInvitationLock = new object();
//
//        /// <summary>Raised when another agent invites our avatar to join a group</summary>
//        public event EventHandler<GroupInvitationEventArgs> GroupInvitation 
//        {
//            add { lock (m_GroupInvitationLock) { m_GroupInvitation += value; } }
//            remove { lock (m_GroupInvitationLock) { m_GroupInvitation -= value; } }
//        }
//        //endregion Delegates
//
//        //region Events
//
//        //endregion Events
//
        /// <summary>A reference to the current <seealso cref="GridClient"/> instance</summary>
        private GridClient Client;
        /// <summary>Currently-active group members requests</summary>
        private List<UUID> GroupMembersRequests;
        /// <summary>Currently-active group roles requests</summary>
        private List<UUID> GroupRolesRequests;
        /// <summary>Currently-active group role-member requests</summary>
        private List<UUID> GroupRolesMembersRequests;
        /// <summary>Dictionary keeping group members while request is in progress</summary>
        private InternalDictionary<UUID, Map<UUID, GroupMember>> TempGroupMembers;
        /// <summary>Dictionary keeping mebmer/role mapping while request is in progress</summary>
        private InternalDictionary<UUID, List<KeyValuePair<UUID, UUID>>> TempGroupRolesMembers;
        /// <summary>Dictionary keeping GroupRole information while request is in progress</summary>
        private InternalDictionary<UUID, Map<UUID, GroupRole>> TempGroupRoles;
        /// <summary>Caches group name lookups</summary>
        public InternalDictionary<UUID, String> GroupName2KeyCache;

        /// <summary>
        /// Construct a new instance of the GroupManager class
        /// </summary>
        /// <param name="client">A reference to the current <seealso cref="GridClient"/> instance</param>
        public GroupManager(GridClient client)
        {
            Client = client;

            TempGroupMembers = new InternalDictionary<UUID, Map<UUID, GroupMember>>();
            GroupMembersRequests = new ArrayList<UUID>();
            TempGroupRoles = new InternalDictionary<UUID, Map<UUID, GroupRole>>();
            GroupRolesRequests = new ArrayList<UUID>();
            TempGroupRolesMembers = new InternalDictionary<UUID, List<KeyValuePair<UUID, UUID>>>();
            GroupRolesMembersRequests = new ArrayList<UUID>();
            GroupName2KeyCache = new InternalDictionary<UUID, String>();

            // Client.Self.IM += Self_IM;
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
                        
            //Client.network.RegisterEventCallback("AgentGroupDataUpdate", new Caps.EventQueueCallback(AgentGroupDataUpdateMessageHandler);
        	Client.network.RegisterEventCallback("AgentGroupDataUpdate", new EventObserver<CapsEventObservableArg>()
        			{ 
        		@Override
        		public void handleEvent(Observable o,CapsEventObservableArg arg) {
        			try{ AgentGroupDataUpdateMessageHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
        			catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        		}}
        			);
        	
        	//deprecated in simulator v1.27
            //Client.network.RegisterCallback(PacketType.AgentDropGroup, AgentDropGroupHandler);
            Client.network.RegisterCallback(PacketType.AgentDropGroup, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ AgentDropGroupHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupTitlesReply, GroupTitlesReplyHandler);

            Client.network.RegisterCallback(PacketType.GroupTitlesReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupTitlesReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupProfileReply, GroupProfileReplyHandler);

            Client.network.RegisterCallback(PacketType.GroupProfileReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupProfileReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupMembersReply, GroupMembersHandler);

            Client.network.RegisterCallback(PacketType.GroupMembersReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupMembersHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupRoleDataReply, GroupRoleDataReplyHandler);

            Client.network.RegisterCallback(PacketType.GroupRoleDataReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupRoleDataReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupRoleMembersReply, GroupRoleMembersReplyHandler);

            Client.network.RegisterCallback(PacketType.GroupRoleMembersReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupRoleMembersReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupActiveProposalItemReply, GroupActiveProposalItemHandler);

            Client.network.RegisterCallback(PacketType.GroupActiveProposalItemReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupActiveProposalItemHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupVoteHistoryItemReply, GroupVoteHistoryItemHandler);

            Client.network.RegisterCallback(PacketType.GroupVoteHistoryItemReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupVoteHistoryItemHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupAccountSummaryReply, GroupAccountSummaryReplyHandler);

            Client.network.RegisterCallback(PacketType.GroupAccountSummaryReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupAccountSummaryReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.CreateGroupReply, CreateGroupReplyHandler);

            Client.network.RegisterCallback(PacketType.CreateGroupReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ CreateGroupReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.JoinGroupReply, JoinGroupReplyHandler);

            Client.network.RegisterCallback(PacketType.JoinGroupReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ JoinGroupReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.LeaveGroupReply, LeaveGroupReplyHandler);

            Client.network.RegisterCallback(PacketType.LeaveGroupReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ LeaveGroupReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.UUIDGroupNameReply, UUIDGroupNameReplyHandler);

            Client.network.RegisterCallback(PacketType.UUIDGroupNameReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ UUIDGroupNameReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.EjectGroupMemberReply, EjectGroupMemberReplyHandler);

            Client.network.RegisterCallback(PacketType.EjectGroupMemberReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ EjectGroupMemberReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            //Client.network.RegisterCallback(PacketType.GroupNoticesListReply, GroupNoticesListReplyHandler);

            Client.network.RegisterCallback(PacketType.GroupNoticesListReply, new EventObserver<PacketReceivedEventArgs>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
            		try{ GroupNoticesListReplyHandler(o, arg);}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
            
            //Client.network.RegisterEventCallback("AgentDropGroup", new Caps.EventQueueCallback(AgentDropGroupMessageHandler);
            Client.network.RegisterEventCallback("AgentDropGroup", new EventObserver<CapsEventObservableArg>()
            		{ 
            	@Override
            	public void handleEvent(Observable o,CapsEventObservableArg arg) {
            		try{ AgentDropGroupMessageHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
            		catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
            	}}
            		);
        }

        void Self_IM(Object sender, InstantMessageEventArgs e)
        {
        	if(onGroupInvitation != null && e.getIM().Dialog == InstantMessageDialog.GroupInvitation)
        	{
        		GroupInvitationEventArgs args = new GroupInvitationEventArgs(e.getSimulator()
                		, e.getIM().FromAgentID, e.getIM().FromAgentName, e.getIM().Message);
                onGroupInvitation.raiseEvent(args);

                if (args.getAccept())
                {
                    Client.self.InstantMessage("name", e.getIM().FromAgentID, "message", e.getIM().IMSessionID, InstantMessageDialog.GroupInvitationAccept,
                         InstantMessageOnline.Online, Client.self.getSimPosition(), UUID.Zero, Utils.EmptyBytes);
                }
                else
                {
                    Client.self.InstantMessage("name", e.getIM().FromAgentID, "message", e.getIM().IMSessionID, InstantMessageDialog.GroupInvitationDecline,
                         InstantMessageOnline.Online, Client.self.getSimPosition(), UUID.Zero, new byte[] { 0 });
                }            
            }
        }


        //region Public Methods

        /// <summary>
        /// Request a current list of groups the avatar is a member of.
        /// </summary>
        /// <remarks>CAPS Event Queue must be running for this to work since the results
        /// come across CAPS.</remarks>
        public void RequestCurrentGroups()
        {
            AgentDataUpdateRequestPacket request = new AgentDataUpdateRequestPacket();

            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();

            Client.network.SendPacket(request);
        }

        /// <summary>
        /// Lookup name of group based on groupID
        /// </summary>
        /// <param name="groupID">groupID of group to lookup name for.</param>
        public void RequestGroupName(UUID groupID)
        {
            // if we already have this in the cache, return from cache instead of making a request
            if (GroupName2KeyCache.containsKey(groupID))
            {
                Map<UUID, String> groupNames = new HashMap<UUID, String>();
                groupNames.put(groupID, GroupName2KeyCache.get(groupID));

                if (onGroupNamesReply != null)
                {
                    onGroupNamesReply.raiseEvent(new GroupNamesEventArgs(groupNames));
                }
            }

            else
            {
                UUIDGroupNameRequestPacket req = new UUIDGroupNameRequestPacket();
                UUIDGroupNameRequestPacket.UUIDNameBlockBlock[] block = new UUIDGroupNameRequestPacket.UUIDNameBlockBlock[1];
                block[0] = new UUIDGroupNameRequestPacket.UUIDNameBlockBlock();
                block[0].ID = groupID;
                req.UUIDNameBlock = block;
                Client.network.SendPacket(req);
            }
        }

        /// <summary>
        /// Request lookup of multiple group names
        /// </summary>
        /// <param name="groupIDs">List of group IDs to request.</param>
        public void RequestGroupNames(List<UUID> groupIDs)
        {
            Map<UUID, String> groupNames = new HashMap<UUID, String>();
            synchronized (GroupName2KeyCache.getDictionary())
            {
                for(UUID groupID : groupIDs)
                {
                    if (GroupName2KeyCache.containsKey(groupID))
                        groupNames.put(groupID, GroupName2KeyCache.get(groupID));
                }
            }

            if (groupIDs.size() > 0)
            {
                UUIDGroupNameRequestPacket req = new UUIDGroupNameRequestPacket();
                UUIDGroupNameRequestPacket.UUIDNameBlockBlock[] block = new UUIDGroupNameRequestPacket.UUIDNameBlockBlock[groupIDs.size()];

                for (int i = 0; i < groupIDs.size(); i++)
                {
                    block[i] = new UUIDGroupNameRequestPacket.UUIDNameBlockBlock();
                    block[i].ID = groupIDs.get(i);
                }

                req.UUIDNameBlock = block;
                Client.network.SendPacket(req);
            }

            // fire handler from cache
            if (groupNames.size() > 0 && onGroupNamesReply != null)
            {
                onGroupNamesReply.raiseEvent(new GroupNamesEventArgs(groupNames));
            }
        }

        /// <summary>Lookup group profile data such as name, enrollment, founder, logo, etc</summary>
        /// <remarks>Subscribe to <code>OnGroupProfile</code> event to receive the results.</remarks>
        /// <param name="group">group ID (UUID)</param>
        public void RequestGroupProfile(UUID group)
        {
            GroupProfileRequestPacket request = new GroupProfileRequestPacket();

            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.GroupData.GroupID = group;

            Client.network.SendPacket(request);
        }

        /// <summary>Request a list of group members.</summary>
        /// <remarks>Subscribe to <code>OnGroupMembers</code> event to receive the results.</remarks>
        /// <param name="group">group ID (UUID)</param>
        /// <returns>UUID of the request, use to index into cache</returns>
        public UUID RequestGroupMembers(UUID group)
        {
            UUID requestID = UUID.Random();
            synchronized (GroupMembersRequests) 
            {GroupMembersRequests.add(requestID);}

            GroupMembersRequestPacket request = new GroupMembersRequestPacket();

            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.GroupData.GroupID = group;
            request.GroupData.RequestID = requestID;

            Client.network.SendPacket(request);
            return requestID;
        }

        /// <summary>Request group roles</summary>
        /// <remarks>Subscribe to <code>OnGroupRoles</code> event to receive the results.</remarks>
        /// <param name="group">group ID (UUID)</param>
        /// <returns>UUID of the request, use to index into cache</returns>
        public UUID RequestGroupRoles(UUID group)
        {
            UUID requestID = UUID.Random();
            synchronized (GroupRolesRequests)
            {GroupRolesRequests.add(requestID);}

            GroupRoleDataRequestPacket request = new GroupRoleDataRequestPacket();

            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.GroupData.GroupID = group;
            request.GroupData.RequestID = requestID;

            Client.network.SendPacket(request);
            return requestID;
        }

        /// <summary>Request members (members,role) role mapping for a group.</summary>
        /// <remarks>Subscribe to <code>OnGroupRolesMembers</code> event to receive the results.</remarks>
        /// <param name="group">group ID (UUID)</param>
        /// <returns>UUID of the request, use to index into cache</returns>
        public UUID RequestGroupRolesMembers(UUID group)
        {
            UUID requestID = UUID.Random();
            synchronized (GroupRolesRequests) 
            {GroupRolesMembersRequests.add(requestID);}

            GroupRoleMembersRequestPacket request = new GroupRoleMembersRequestPacket();
            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.GroupData.GroupID = group;
            request.GroupData.RequestID = requestID;
            Client.network.SendPacket(request);
            return requestID;
        }

        /// <summary>Request a groups Titles</summary>
        /// <remarks>Subscribe to <code>OnGroupTitles</code> event to receive the results.</remarks>
        /// <param name="group">group ID (UUID)</param>
        /// <returns>UUID of the request, use to index into cache</returns>
        public UUID RequestGroupTitles(UUID group)
        {
            UUID requestID = UUID.Random();

            GroupTitlesRequestPacket request = new GroupTitlesRequestPacket();

            request.AgentData.AgentID = Client.self.getAgentID();
            request.AgentData.SessionID = Client.self.getSessionID();
            request.AgentData.GroupID = group;
            request.AgentData.RequestID = requestID;

            Client.network.SendPacket(request);
            return requestID;
        }

        /// <summary>Begin to get the group account summary</summary>
        /// <remarks>Subscribe to the <code>OnGroupAccountSummary</code> event to receive the results.</remarks>
        /// <param name="group">group ID (UUID)</param>
        /// <param name="intervalDays">How long of an interval</param>
        /// <param name="currentInterval">Which interval (0 for current, 1 for last)</param>
        public void RequestGroupAccountSummary(UUID group, int intervalDays, int currentInterval)
        {
            GroupAccountSummaryRequestPacket p = new GroupAccountSummaryRequestPacket();
            p.AgentData.AgentID = Client.self.getAgentID();
            p.AgentData.SessionID = Client.self.getSessionID();
            p.AgentData.GroupID = group;
            p.MoneyData.RequestID = UUID.Random();
            p.MoneyData.CurrentInterval = currentInterval;
            p.MoneyData.IntervalDays = intervalDays;
            Client.network.SendPacket(p);
        }

        /// <summary>Invites a user to a group</summary>
        /// <param name="group">The group to invite to</param>
        /// <param name="roles">A list of roles to invite a person to</param>
        /// <param name="personkey">Key of person to invite</param>
        public void Invite(UUID group, List<UUID> roles, UUID personkey)
        {
            InviteGroupRequestPacket igp = new InviteGroupRequestPacket();

            igp.AgentData = new InviteGroupRequestPacket.AgentDataBlock();
            igp.AgentData.AgentID = Client.self.getAgentID();
            igp.AgentData.SessionID = Client.self.getSessionID();

            igp.GroupData = new InviteGroupRequestPacket.GroupDataBlock();
            igp.GroupData.GroupID = group;

            igp.InviteData = new InviteGroupRequestPacket.InviteDataBlock[roles.size()];

            for (int i = 0; i < roles.size(); i++)
            {
                igp.InviteData[i] = new InviteGroupRequestPacket.InviteDataBlock();
                igp.InviteData[i].InviteeID = personkey;
                igp.InviteData[i].RoleID = roles.get(i);
            }

            Client.network.SendPacket(igp);
        }

        /// <summary>Set a group as the current active group</summary>
        /// <param name="id">group ID (UUID)</param>
        public void ActivateGroup(UUID id)
        {
            ActivateGroupPacket activate = new ActivateGroupPacket();
            activate.AgentData.AgentID = Client.self.getAgentID();
            activate.AgentData.SessionID = Client.self.getSessionID();
            activate.AgentData.GroupID = id;

            Client.network.SendPacket(activate);
        }

        /// <summary>Change the role that determines your active title</summary>
        /// <param name="group">Group ID to use</param>
        /// <param name="role">Role ID to change to</param>
        public void ActivateTitle(UUID group, UUID role)
        {
            GroupTitleUpdatePacket gtu = new GroupTitleUpdatePacket();
            gtu.AgentData.AgentID = Client.self.getAgentID();
            gtu.AgentData.SessionID = Client.self.getSessionID();
            gtu.AgentData.TitleRoleID = role;
            gtu.AgentData.GroupID = group;

            Client.network.SendPacket(gtu);
        }

        /// <summary>Set this avatar's tier contribution</summary>
        /// <param name="group">Group ID to change tier in</param>
        /// <param name="contribution">amount of tier to donate</param>
        public void SetGroupContribution(UUID group, int contribution)
        {
            SetGroupContributionPacket sgp = new SetGroupContributionPacket();
            sgp.AgentData.AgentID = Client.self.getAgentID();
            sgp.AgentData.SessionID = Client.self.getSessionID();
            sgp.Data.GroupID = group;
            sgp.Data.Contribution = contribution;

            Client.network.SendPacket(sgp);
        }

        /// <summary>
        /// Save wheather agent wants to accept group notices and list this group in their profile
        /// </summary>
        /// <param name="groupID">Group <see cref="UUID"/></param>
        /// <param name="acceptNotices">Accept notices from this group</param>
        /// <param name="listInProfile">List this group in the profile</param>
        public void SetGroupAcceptNotices(UUID groupID, boolean acceptNotices, boolean listInProfile)
        {
            SetGroupAcceptNoticesPacket p = new SetGroupAcceptNoticesPacket();
            p.AgentData.AgentID = Client.self.getAgentID();
            p.AgentData.SessionID = Client.self.getSessionID();
            p.Data.GroupID = groupID;
            p.Data.AcceptNotices = acceptNotices;
            p.NewData.ListInProfile = listInProfile;

            Client.network.SendPacket(p);
        }

        /// <summary>Request to join a group</summary>
        /// <remarks>Subscribe to <code>OnGroupJoined</code> event for confirmation.</remarks>
        /// <param name="id">group ID (UUID) to join.</param>
        public void RequestJoinGroup(UUID id)
        {
            JoinGroupRequestPacket join = new JoinGroupRequestPacket();
            join.AgentData.AgentID = Client.self.getAgentID();
            join.AgentData.SessionID = Client.self.getSessionID();

            join.GroupData.GroupID = id;

            Client.network.SendPacket(join);
        }

        /// <summary>
        /// Request to create a new group. If the group is successfully
        /// created, L$100 will automatically be deducted
        /// </summary>
        /// <remarks>Subscribe to <code>OnGroupCreated</code> event to receive confirmation.</remarks>
        /// <param name="group">Group struct containing the new group info</param>
        public void RequestCreateGroup(Group group)
        {
            CreateGroupRequestPacket cgrp = new CreateGroupRequestPacket();
            cgrp.AgentData = new CreateGroupRequestPacket.AgentDataBlock();
            cgrp.AgentData.AgentID = Client.self.getAgentID();
            cgrp.AgentData.SessionID = Client.self.getSessionID();

            cgrp.GroupData = new CreateGroupRequestPacket.GroupDataBlock();
            cgrp.GroupData.AllowPublish = group.AllowPublish;
            cgrp.GroupData.Charter = Utils.stringToBytesWithTrailingNullByte(group.Charter);
            cgrp.GroupData.InsigniaID = group.InsigniaID;
            cgrp.GroupData.MaturePublish = group.MaturePublish;
            cgrp.GroupData.MembershipFee = group.MembershipFee;
            cgrp.GroupData.Name = Utils.stringToBytesWithTrailingNullByte(group.Name);
            cgrp.GroupData.OpenEnrollment = group.OpenEnrollment;
            cgrp.GroupData.ShowInList = group.ShowInList;

            Client.network.SendPacket(cgrp);
        }

        /// <summary>Update a group's profile and other information</summary>
        /// <param name="id">Groups ID (UUID) to update.</param>
        /// <param name="group">Group struct to update.</param>
        public void UpdateGroup(UUID id, Group group)
        {
            UpdateGroupInfoPacket cgrp = new UpdateGroupInfoPacket();
            cgrp.AgentData = new UpdateGroupInfoPacket.AgentDataBlock();
            cgrp.AgentData.AgentID = Client.self.getAgentID();
            cgrp.AgentData.SessionID = Client.self.getSessionID();

            cgrp.GroupData = new UpdateGroupInfoPacket.GroupDataBlock();
            cgrp.GroupData.GroupID = id;
            cgrp.GroupData.AllowPublish = group.AllowPublish;
            cgrp.GroupData.Charter = Utils.stringToBytesWithTrailingNullByte(group.Charter);
            cgrp.GroupData.InsigniaID = group.InsigniaID;
            cgrp.GroupData.MaturePublish = group.MaturePublish;
            cgrp.GroupData.MembershipFee = group.MembershipFee;
            cgrp.GroupData.OpenEnrollment = group.OpenEnrollment;
            cgrp.GroupData.ShowInList = group.ShowInList;

            Client.network.SendPacket(cgrp);
        }

        /// <summary>Eject a user from a group</summary>
        /// <param name="group">Group ID to eject the user from</param>
        /// <param name="member">Avatar's key to eject</param>
        public void EjectUser(UUID group, UUID member)
        {
            EjectGroupMemberRequestPacket eject = new EjectGroupMemberRequestPacket();
            eject.AgentData = new EjectGroupMemberRequestPacket.AgentDataBlock();
            eject.AgentData.AgentID = Client.self.getAgentID();
            eject.AgentData.SessionID = Client.self.getSessionID();

            eject.GroupData = new EjectGroupMemberRequestPacket.GroupDataBlock();
            eject.GroupData.GroupID = group;

            eject.EjectData = new EjectGroupMemberRequestPacket.EjectDataBlock[1];
            eject.EjectData[0] = new EjectGroupMemberRequestPacket.EjectDataBlock();
            eject.EjectData[0].EjecteeID = member;

            Client.network.SendPacket(eject);
        }

        /// <summary>Update role information</summary>
        /// <param name="role">Modified role to be updated</param>
        public void UpdateRole(GroupRole role)
        {
            GroupRoleUpdatePacket gru = new GroupRoleUpdatePacket();
            gru.AgentData.AgentID = Client.self.getAgentID();
            gru.AgentData.SessionID = Client.self.getSessionID();
            gru.AgentData.GroupID = role.GroupID;
            gru.RoleData = new GroupRoleUpdatePacket.RoleDataBlock[1];
            gru.RoleData[0] = new GroupRoleUpdatePacket.RoleDataBlock();
            gru.RoleData[0].Name = Utils.stringToBytesWithTrailingNullByte(role.Name);
            gru.RoleData[0].Description = Utils.stringToBytesWithTrailingNullByte(role.Description);
            gru.RoleData[0].Powers = new BigInteger(Utils.int64ToBytes(GroupPowers.getIndex(role.Powers)));
            gru.RoleData[0].RoleID = role.ID;
            gru.RoleData[0].Title = Utils.stringToBytesWithTrailingNullByte(role.Title);
            gru.RoleData[0].UpdateType = (byte)GroupRoleUpdate.UpdateAll.getIndex();
            Client.network.SendPacket(gru);
        }

        /// <summary>Create a new group role</summary>
        /// <param name="group">Group ID to update</param>
        /// <param name="role">Role to create</param>
        public void CreateRole(UUID group, GroupRole role)
        {
            GroupRoleUpdatePacket gru = new GroupRoleUpdatePacket();
            gru.AgentData.AgentID = Client.self.getAgentID();
            gru.AgentData.SessionID = Client.self.getSessionID();
            gru.AgentData.GroupID = group;
            gru.RoleData = new GroupRoleUpdatePacket.RoleDataBlock[1];
            gru.RoleData[0] = new GroupRoleUpdatePacket.RoleDataBlock();
            gru.RoleData[0].RoleID = UUID.Random();
            gru.RoleData[0].Name = Utils.stringToBytesWithTrailingNullByte(role.Name);
            gru.RoleData[0].Description = Utils.stringToBytesWithTrailingNullByte(role.Description);
            gru.RoleData[0].Powers = new BigInteger(Utils.int64ToBytes(GroupPowers.getIndex(role.Powers)));
            gru.RoleData[0].Title = Utils.stringToBytesWithTrailingNullByte(role.Title);
            gru.RoleData[0].UpdateType = (byte)GroupRoleUpdate.Create.getIndex();
            Client.network.SendPacket(gru);
        }

        /// <summary>Delete a group role</summary>
        /// <param name="group">Group ID to update</param>
        /// <param name="roleID">Role to delete</param>
        public void DeleteRole(UUID group, UUID roleID)
        {
            GroupRoleUpdatePacket gru = new GroupRoleUpdatePacket();
            gru.AgentData.AgentID = Client.self.getAgentID();
            gru.AgentData.SessionID = Client.self.getSessionID();
            gru.AgentData.GroupID = group;
            gru.RoleData = new GroupRoleUpdatePacket.RoleDataBlock[1];
            gru.RoleData[0] = new GroupRoleUpdatePacket.RoleDataBlock();
            gru.RoleData[0].RoleID = roleID;
            gru.RoleData[0].Name = Utils.stringToBytesWithTrailingNullByte("");
            gru.RoleData[0].Description = Utils.stringToBytesWithTrailingNullByte("");
            gru.RoleData[0].Powers = new BigInteger("0");
            gru.RoleData[0].Title = Utils.stringToBytesWithTrailingNullByte("");
            gru.RoleData[0].UpdateType = (byte)GroupRoleUpdate.Delete.getIndex();
            Client.network.SendPacket(gru);
        }

        /// <summary>Remove an avatar from a role</summary>
        /// <param name="group">Group ID to update</param>
        /// <param name="role">Role ID to be removed from</param>
        /// <param name="member">Avatar's Key to remove</param>
        public void RemoveFromRole(UUID group, UUID role, UUID member)
        {
            GroupRoleChangesPacket grc = new GroupRoleChangesPacket();
            grc.AgentData.AgentID = Client.self.getAgentID();
            grc.AgentData.SessionID = Client.self.getSessionID();
            grc.AgentData.GroupID = group;
            grc.RoleChange = new GroupRoleChangesPacket.RoleChangeBlock[1];
            grc.RoleChange[0] = new GroupRoleChangesPacket.RoleChangeBlock();
            //Add to members and role
            grc.RoleChange[0].MemberID = member;
            grc.RoleChange[0].RoleID = role;
            //1 = Remove From Role TODO: this should be in an enum
            grc.RoleChange[0].Change = 1;
            Client.network.SendPacket(grc);
        }

        /// <summary>Assign an avatar to a role</summary>
        /// <param name="group">Group ID to update</param>
        /// <param name="role">Role ID to assign to</param>
        /// <param name="member">Avatar's ID to assign to role</param>
        public void AddToRole(UUID group, UUID role, UUID member)
        {
            GroupRoleChangesPacket grc = new GroupRoleChangesPacket();
            grc.AgentData.AgentID = Client.self.getAgentID();
            grc.AgentData.SessionID = Client.self.getSessionID();
            grc.AgentData.GroupID = group;
            grc.RoleChange = new GroupRoleChangesPacket.RoleChangeBlock[1];
            grc.RoleChange[0] = new GroupRoleChangesPacket.RoleChangeBlock();
            //Add to members and role
            grc.RoleChange[0].MemberID = member;
            grc.RoleChange[0].RoleID = role;
            //0 = Add to Role TODO: this should be in an enum
            grc.RoleChange[0].Change = 0;
            Client.network.SendPacket(grc);
        }

        /// <summary>Request the group notices list</summary>
        /// <param name="group">Group ID to fetch notices for</param>
        public void RequestGroupNoticesList(UUID group)
        {
            GroupNoticesListRequestPacket gnl = new GroupNoticesListRequestPacket();
            gnl.AgentData.AgentID = Client.self.getAgentID();
            gnl.AgentData.SessionID = Client.self.getSessionID();
            gnl.Data.GroupID = group;
            Client.network.SendPacket(gnl);
        }

        /// <summary>Request a group notice by key</summary>
        /// <param name="noticeID">ID of group notice</param>
        public void RequestGroupNotice(UUID noticeID)
        {
            GroupNoticeRequestPacket gnr = new GroupNoticeRequestPacket();
            gnr.AgentData.AgentID = Client.self.getAgentID();
            gnr.AgentData.SessionID = Client.self.getSessionID();
            gnr.Data.GroupNoticeID = noticeID;
            Client.network.SendPacket(gnr);
        }
        
        /// <summary>Send out a group notice</summary>
        /// <param name="group">Group ID to update</param>
        /// <param name="notice"><code>GroupNotice</code> structure containing notice data</param>
        public void SendGroupNotice(UUID group, GroupNotice notice) throws Exception
        {
            Client.self.InstantMessage(Client.self.getName(), group, notice.Subject + "|" + notice.Message,
                UUID.Zero, InstantMessageDialog.GroupNotice, InstantMessageOnline.Online,
                Vector3.Zero, UUID.Zero, notice.SerializeAttachment());
        }

        /// <summary>Start a group proposal (vote)</summary>
        /// <param name="group">The Group ID to send proposal to</param>
        /// <param name="prop"><code>GroupProposal</code> structure containing the proposal</param>
        public void StartProposal(UUID group, GroupProposal prop)
        {
            StartGroupProposalPacket p = new StartGroupProposalPacket();
            p.AgentData.AgentID = Client.self.getAgentID();
            p.AgentData.SessionID = Client.self.getSessionID();
            p.ProposalData.GroupID = group;
            p.ProposalData.ProposalText = Utils.stringToBytesWithTrailingNullByte(prop.VoteText);
            p.ProposalData.Quorum = prop.Quorum;
            p.ProposalData.Majority = prop.Majority;
            p.ProposalData.Duration = prop.Duration;
            Client.network.SendPacket(p);
        }

        /// <summary>Request to leave a group</summary>
        /// <remarks>Subscribe to <code>OnGroupLeft</code> event to receive confirmation</remarks>
        /// <param name="groupID">The group to leave</param>
        public void LeaveGroup(UUID groupID)
        {
            LeaveGroupRequestPacket p = new LeaveGroupRequestPacket();
            p.AgentData.AgentID = Client.self.getAgentID();
            p.AgentData.SessionID = Client.self.getSessionID();
            p.GroupData.GroupID = groupID;

            Client.network.SendPacket(p);
        }
//endregion

        //region Packet Handlers
        
        protected void AgentGroupDataUpdateMessageHandler(String capsKey, IMessage message, Simulator simulator)
        {      
            if (onCurrentGroups != null)
            {
                AgentGroupDataUpdateMessage msg = (AgentGroupDataUpdateMessage)message;

                Map<UUID, Group> currentGroups = new HashMap<UUID, Group>();
                for (int i = 0; i < msg.GroupDataBlock.length; i++)
                {
                    Group group = new Group();
                    group.ID = msg.GroupDataBlock[i].GroupID;
                    group.InsigniaID = msg.GroupDataBlock[i].GroupInsigniaID;
                    group.Name = msg.GroupDataBlock[i].GroupName;
                    group.Contribution = msg.GroupDataBlock[i].Contribution;
                    group.AcceptNotices = msg.GroupDataBlock[i].AcceptNotices;
                    group.Powers = msg.GroupDataBlock[i].GroupPowers;
                    group.ListInProfile = msg.NewGroupDataBlock[i].ListInProfile;

                    currentGroups.put(group.ID, group);

                    synchronized (GroupName2KeyCache.getDictionary())
                    {
                        if (!GroupName2KeyCache.containsKey(group.ID))
                            GroupName2KeyCache.add(group.ID, group.Name);
                    }
                }
                onCurrentGroups.raiseEvent(new CurrentGroupsEventArgs(currentGroups));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void AgentDropGroupHandler(Object sender, PacketReceivedEventArgs e)
        {
            if (onGroupDropped != null)
            {
                Packet packet = e.getPacket();
                onGroupDropped.raiseEvent(new GroupDroppedEventArgs(((AgentDropGroupPacket)packet).AgentData.GroupID));
            }
        }

        protected void AgentDropGroupMessageHandler(String capsKey, IMessage message, Simulator simulator)
        {
            
            if (onGroupDropped != null)
            {
                AgentDropGroupMessage msg = (AgentDropGroupMessage)message;
                for (int i = 0; i < msg.AgentDataBlock.length; i++)
                {
                    onGroupDropped.raiseEvent(new GroupDroppedEventArgs(msg.AgentDataBlock[i].GroupID));
                }
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupProfileReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {            
            if (onGroupProfile != null)
            {
                Packet packet = e.getPacket();
                GroupProfileReplyPacket profile = (GroupProfileReplyPacket)packet;
                Group group = new Group();

                group.ID = profile.GroupData.GroupID;
                group.AllowPublish = profile.GroupData.AllowPublish;
                group.Charter = Utils.bytesWithTrailingNullByteToString(profile.GroupData.Charter);
                group.FounderID = profile.GroupData.FounderID;
                group.GroupMembershipCount = profile.GroupData.GroupMembershipCount;
                group.GroupRolesCount = profile.GroupData.GroupRolesCount;
                group.InsigniaID = profile.GroupData.InsigniaID;
                group.MaturePublish = profile.GroupData.MaturePublish;
                group.MembershipFee = profile.GroupData.MembershipFee;
                group.MemberTitle = Utils.bytesWithTrailingNullByteToString(profile.GroupData.MemberTitle);
                group.Money = profile.GroupData.Money;
                group.Name = Utils.bytesWithTrailingNullByteToString(profile.GroupData.Name);
                group.OpenEnrollment = profile.GroupData.OpenEnrollment;
                group.OwnerRole = profile.GroupData.OwnerRole;
                group.Powers = GroupPowers.get(profile.GroupData.PowersMask.longValue());
                group.ShowInList = profile.GroupData.ShowInList;

                onGroupProfile.raiseEvent(new GroupProfileEventArgs(group));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupNoticesListReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onGroupNoticesListReply != null)
            {
                Packet packet = e.getPacket();
                GroupNoticesListReplyPacket reply = (GroupNoticesListReplyPacket)packet;

                List<GroupNoticesListEntry> notices = new ArrayList<GroupNoticesListEntry>();

                for(GroupNoticesListReplyPacket.DataBlock entry : reply.Data)
                {
                    GroupNoticesListEntry notice = new GroupNoticesListEntry();
                    notice.FromName = Utils.bytesWithTrailingNullByteToString(entry.FromName);
                    notice.Subject = Utils.bytesWithTrailingNullByteToString(entry.Subject);
                    notice.NoticeID = entry.NoticeID;
                    notice.Timestamp = entry.Timestamp;
                    notice.HasAttachment = entry.HasAttachment;
                    notice.AssetType = AssetType.get(entry.AssetType);

                    notices.add(notice);
                }

               onGroupNoticesListReply.raiseEvent(new GroupNoticesListReplyEventArgs(reply.AgentData.GroupID, notices));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupTitlesReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onGroupTitlesReply != null)
            {
                Packet packet = e.getPacket();
                GroupTitlesReplyPacket titles = (GroupTitlesReplyPacket)packet;
                Map<UUID, GroupTitle> groupTitleCache = new HashMap<UUID, GroupTitle>();

                for(GroupTitlesReplyPacket.GroupDataBlock block : titles.GroupData)
                {
                    GroupTitle groupTitle = new GroupTitle();

                    groupTitle.GroupID = titles.AgentData.GroupID;
                    groupTitle.RoleID = block.RoleID;
                    groupTitle.Title = Utils.bytesWithTrailingNullByteToString(block.Title);
                    groupTitle.Selected = block.Selected;

                    groupTitleCache.put(block.RoleID, groupTitle);
                }
                onGroupTitlesReply.raiseEvent(new GroupTitlesReplyEventArgs(titles.AgentData.RequestID, titles.AgentData.GroupID, groupTitleCache));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupMembersHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            Packet packet = e.getPacket();
            GroupMembersReplyPacket members = (GroupMembersReplyPacket)packet;
            Map<UUID, GroupMember> groupMemberCache = null;

            synchronized (GroupMembersRequests)
            {
                // If nothing is registered to receive this RequestID drop the data
                if (GroupMembersRequests.contains(members.GroupData.RequestID))
                {
                    synchronized (TempGroupMembers.getDictionary())
                    {
                        if ((groupMemberCache = TempGroupMembers.get(members.GroupData.RequestID)) == null)
                        {
                            groupMemberCache = new HashMap<UUID, GroupMember>();
                            TempGroupMembers.add(members.GroupData.RequestID, groupMemberCache);
                        }

                        for(GroupMembersReplyPacket.MemberDataBlock block : members.MemberData)
                        {
                            GroupMember groupMember = new GroupMember();

                            groupMember.ID = block.AgentID;
                            groupMember.Contribution = block.Contribution;
                            groupMember.IsOwner = block.IsOwner;
                            groupMember.OnlineStatus = Utils.bytesWithTrailingNullByteToString(block.OnlineStatus);
                            groupMember.Powers = GroupPowers.get(block.AgentPowers.longValue());
                            groupMember.Title = Utils.bytesWithTrailingNullByteToString(block.Title);

                            groupMemberCache.put(block.AgentID, groupMember);
                        }

                        if (groupMemberCache.size() >= members.GroupData.MemberCount)
                        {
                            GroupMembersRequests.remove(members.GroupData.RequestID);
                            TempGroupMembers.remove(members.GroupData.RequestID);
                        }
                    }
                }
            }

            if (onGroupMembersReply != null && groupMemberCache != null && groupMemberCache.size() >= members.GroupData.MemberCount)
            {
                onGroupMembersReply.raiseEvent(new GroupMembersReplyEventArgs(members.GroupData.RequestID, members.GroupData.GroupID, groupMemberCache));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupRoleDataReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            Packet packet = e.getPacket();
            GroupRoleDataReplyPacket roles = (GroupRoleDataReplyPacket)packet;
            Map<UUID, GroupRole> groupRoleCache = null;

            synchronized (GroupRolesRequests)
            {
                // If nothing is registered to receive this RequestID drop the data
                if (GroupRolesRequests.contains(roles.GroupData.RequestID))
                {
                    GroupRolesRequests.remove(roles.GroupData.RequestID);

                    synchronized (TempGroupRoles.getDictionary())
                    {
                        if ((groupRoleCache = TempGroupRoles.get(roles.GroupData.RequestID))==null)
                        {
                            groupRoleCache = new HashMap<UUID, GroupRole>();
                            TempGroupRoles.add(roles.GroupData.RequestID, groupRoleCache);
                        }

                        for(GroupRoleDataReplyPacket.RoleDataBlock block : roles.RoleData)
                        {
                            GroupRole groupRole = new GroupRole();

                            groupRole.GroupID = roles.GroupData.GroupID;
                            groupRole.ID = block.RoleID;
                            groupRole.Description = Utils.bytesWithTrailingNullByteToString(block.Description);
                            groupRole.Name = Utils.bytesWithTrailingNullByteToString(block.Name);
                            groupRole.Powers = GroupPowers.get(block.Powers.longValue());
                            groupRole.Title = Utils.bytesWithTrailingNullByteToString(block.Title);

                            groupRoleCache.put(block.RoleID, groupRole);
                        }

                        if (groupRoleCache.size() >= roles.GroupData.RoleCount)
                        {
                            GroupRolesRequests.remove(roles.GroupData.RequestID);
                            TempGroupRoles.remove(roles.GroupData.RequestID);
                        }
                    }
                }
            }

            if (onGroupRoleDataReply != null && groupRoleCache != null && groupRoleCache.size() >= roles.GroupData.RoleCount)
            {
                onGroupRoleDataReply.raiseEvent(new GroupRolesDataReplyEventArgs(roles.GroupData.RequestID, roles.GroupData.GroupID, groupRoleCache));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupRoleMembersReplyHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();
            GroupRoleMembersReplyPacket members = (GroupRoleMembersReplyPacket)packet;
            List<KeyValuePair<UUID, UUID>> groupRoleMemberCache = null;

            try
            {
                synchronized (GroupRolesMembersRequests)
                {
                    // If nothing is registered to receive this RequestID drop the data
                    if (GroupRolesMembersRequests.contains(members.AgentData.RequestID))
                    {
                        synchronized (TempGroupRolesMembers.getDictionary())
                        {
                            if ((groupRoleMemberCache = TempGroupRolesMembers.get(members.AgentData.RequestID))==null)
                            {
                                groupRoleMemberCache = new ArrayList<KeyValuePair<UUID, UUID>>();
                                TempGroupRolesMembers.add(members.AgentData.RequestID, groupRoleMemberCache);
                            }

                            for(GroupRoleMembersReplyPacket.MemberDataBlock block : members.MemberData)
                            {
                                KeyValuePair<UUID, UUID> rolemember =
                                    new KeyValuePair<UUID, UUID>(block.RoleID, block.MemberID);

                                groupRoleMemberCache.add(rolemember);
                            }

                            if (groupRoleMemberCache.size() >= members.AgentData.TotalPairs)
                            {
                                GroupRolesMembersRequests.remove(members.AgentData.RequestID);
                                TempGroupRolesMembers.remove(members.AgentData.RequestID);
                            }
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                JLogger.error(Utils.getExceptionStackTraceAsString(ex));
            }

            if (onGroupRoleMembersReply != null && groupRoleMemberCache != null && groupRoleMemberCache.size() >= members.AgentData.TotalPairs)
            {
            	onGroupRoleMembersReply.raiseEvent(new GroupRolesMembersReplyEventArgs(members.AgentData.RequestID, members.AgentData.GroupID, groupRoleMemberCache));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupActiveProposalItemHandler(Object sender, PacketReceivedEventArgs e)
        {
            //GroupActiveProposalItemReplyPacket proposal = (GroupActiveProposalItemReplyPacket)packet;

            // TODO: Create a proposal struct to represent the fields in a proposal item
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupVoteHistoryItemHandler(Object sender, PacketReceivedEventArgs e)
        {
            //GroupVoteHistoryItemReplyPacket history = (GroupVoteHistoryItemReplyPacket)packet;

            // TODO: This was broken in the official viewer when I was last trying to work  on it
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void GroupAccountSummaryReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onGroupAccountSummaryReply != null)
            {
                Packet packet = e.getPacket();
                GroupAccountSummaryReplyPacket summary = (GroupAccountSummaryReplyPacket)packet;
                GroupAccountSummary account = new GroupAccountSummary();

                account.Balance = summary.MoneyData.Balance;
                account.CurrentInterval = summary.MoneyData.CurrentInterval;
                account.GroupTaxCurrent = summary.MoneyData.GroupTaxCurrent;
                account.GroupTaxEstimate = summary.MoneyData.GroupTaxEstimate;
                account.IntervalDays = summary.MoneyData.IntervalDays;
                account.LandTaxCurrent = summary.MoneyData.LandTaxCurrent;
                account.LandTaxEstimate = summary.MoneyData.LandTaxEstimate;
                account.LastTaxDate = Utils.bytesWithTrailingNullByteToString(summary.MoneyData.LastTaxDate);
                account.LightTaxCurrent = summary.MoneyData.LightTaxCurrent;
                account.LightTaxEstimate = summary.MoneyData.LightTaxEstimate;
                account.NonExemptMembers = summary.MoneyData.NonExemptMembers;
                account.ObjectTaxCurrent = summary.MoneyData.ObjectTaxCurrent;
                account.ObjectTaxEstimate = summary.MoneyData.ObjectTaxEstimate;
                account.ParcelDirFeeCurrent = summary.MoneyData.ParcelDirFeeCurrent;
                account.ParcelDirFeeEstimate = summary.MoneyData.ParcelDirFeeEstimate;
                account.StartDate = Utils.bytesWithTrailingNullByteToString(summary.MoneyData.StartDate);
                account.TaxDate = Utils.bytesWithTrailingNullByteToString(summary.MoneyData.TaxDate);
                account.TotalCredits = summary.MoneyData.TotalCredits;
                account.TotalDebits = summary.MoneyData.TotalDebits;

                onGroupAccountSummaryReply.raiseEvent(new GroupAccountSummaryReplyEventArgs(summary.AgentData.GroupID, account));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void CreateGroupReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            if (onGroupCreatedReply != null)
            {
                Packet packet = e.getPacket();
                CreateGroupReplyPacket reply = (CreateGroupReplyPacket)packet;

                String message = Utils.bytesWithTrailingNullByteToString(reply.ReplyData.Message);

                onGroupCreatedReply.raiseEvent(new GroupCreatedReplyEventArgs(reply.ReplyData.GroupID, reply.ReplyData.Success, message));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void JoinGroupReplyHandler(Object sender, PacketReceivedEventArgs e)
        {
            if (onGroupJoinedReply != null)
            {
                Packet packet = e.getPacket();
                JoinGroupReplyPacket reply = (JoinGroupReplyPacket)packet;

                onGroupJoinedReply.raiseEvent(new GroupOperationEventArgs(reply.GroupData.GroupID, reply.GroupData.Success));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void LeaveGroupReplyHandler(Object sender, PacketReceivedEventArgs e)
        {
            if (onGroupLeaveReply != null)
            {
                Packet packet = e.getPacket();
                LeaveGroupReplyPacket reply = (LeaveGroupReplyPacket)packet;

                onGroupLeaveReply.raiseEvent(new GroupOperationEventArgs(reply.GroupData.GroupID, reply.GroupData.Success));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        private void UUIDGroupNameReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
        {
            Packet packet = e.getPacket();
            UUIDGroupNameReplyPacket reply = (UUIDGroupNameReplyPacket)packet;
            UUIDGroupNameReplyPacket.UUIDNameBlockBlock[] blocks = reply.UUIDNameBlock;

            Map<UUID, String> groupNames = new HashMap<UUID, String>();

            for(UUIDGroupNameReplyPacket.UUIDNameBlockBlock block : blocks)
            {
                groupNames.put(block.ID, Utils.bytesWithTrailingNullByteToString(block.GroupName));
                if (!GroupName2KeyCache.containsKey(block.ID))
                    GroupName2KeyCache.add(block.ID, Utils.bytesWithTrailingNullByteToString(block.GroupName));
            }

            if (onGroupNamesReply != null)
            {
                onGroupNamesReply.raiseEvent(new GroupNamesEventArgs(groupNames));
            }
        }

        /// <summary>Process an incoming packet and raise the appropriate events</summary>
        /// <param name="sender">The sender</param>
        /// <param name="e">The EventArgs object containing the packet data</param>
        protected void EjectGroupMemberReplyHandler(Object sender, PacketReceivedEventArgs e)
        {
            Packet packet = e.getPacket();
            EjectGroupMemberReplyPacket reply = (EjectGroupMemberReplyPacket)packet;

            // TODO: On Success remove the member from the cache(s)

            if (onGroupMemberEjected != null)
            {
                onGroupMemberEjected.raiseEvent(new GroupOperationEventArgs(reply.GroupData.GroupID, reply.EjectData.Success));
            }
        }        

        //endregion Packet Handlers
}
