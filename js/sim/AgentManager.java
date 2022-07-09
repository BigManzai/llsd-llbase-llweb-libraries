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
import java.net.InetSocketAddress;
import java.net.URI;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.Date;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.exception.NotImplementedException;
import com.ngt.jopenmetaverse.shared.protocol.ActivateGesturesPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentAnimationPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentDataUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentHeightWidthPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentMovementCompletePacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentRequestSitPacket;
import com.ngt.jopenmetaverse.shared.protocol.AgentSitPacket;
import com.ngt.jopenmetaverse.shared.protocol.AlertMessagePacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarAnimationPacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarInterestsUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarPropertiesUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.AvatarSitResponsePacket;
import com.ngt.jopenmetaverse.shared.protocol.CameraConstraintPacket;
import com.ngt.jopenmetaverse.shared.protocol.ChatFromSimulatorPacket;
import com.ngt.jopenmetaverse.shared.protocol.ChatFromViewerPacket;
import com.ngt.jopenmetaverse.shared.protocol.ClassifiedDeletePacket;
import com.ngt.jopenmetaverse.shared.protocol.ClassifiedInfoUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.CompleteAgentMovementPacket;
import com.ngt.jopenmetaverse.shared.protocol.CrossedRegionPacket;
import com.ngt.jopenmetaverse.shared.protocol.DeactivateGesturesPacket;
import com.ngt.jopenmetaverse.shared.protocol.GenericMessagePacket;
import com.ngt.jopenmetaverse.shared.protocol.HealthMessagePacket;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.protocol.ImprovedInstantMessagePacket;
import com.ngt.jopenmetaverse.shared.protocol.LoadURLPacket;
import com.ngt.jopenmetaverse.shared.protocol.MeanCollisionAlertPacket;
import com.ngt.jopenmetaverse.shared.protocol.MoneyBalanceReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.MoneyBalanceRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.MoneyTransferRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.MuteListRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.MuteListUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectDeGrabPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectGrabPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectGrabUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.PickDeletePacket;
import com.ngt.jopenmetaverse.shared.protocol.PickInfoUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.RemoveMuteListEntryPacket;
import com.ngt.jopenmetaverse.shared.protocol.RetrieveInstantMessagesPacket;
import com.ngt.jopenmetaverse.shared.protocol.ScriptAnswerYesPacket;
import com.ngt.jopenmetaverse.shared.protocol.ScriptControlChangePacket;
import com.ngt.jopenmetaverse.shared.protocol.ScriptDialogPacket;
import com.ngt.jopenmetaverse.shared.protocol.ScriptDialogReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.ScriptQuestionPacket;
import com.ngt.jopenmetaverse.shared.protocol.ScriptSensorReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.ScriptSensorRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.SetStartLocationRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.StartLurePacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportFailedPacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportFinishPacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportLandmarkRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportLocalPacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportLocationRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportLureRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportProgressPacket;
import com.ngt.jopenmetaverse.shared.protocol.TeleportStartPacket;
import com.ngt.jopenmetaverse.shared.protocol.UpdateMuteListEntryPacket;
import com.ngt.jopenmetaverse.shared.protocol.ViewerEffectPacket;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.sim.GridManager.GridLayerType;
import com.ngt.jopenmetaverse.shared.sim.GridManager.GridRegion;
import com.ngt.jopenmetaverse.shared.sim.GroupManager.ChatSessionMember;
import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupPowers;
import com.ngt.jopenmetaverse.shared.sim.agent.AgentMovement;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetGesture;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetGesture.GestureStep;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetGesture.GestureStepAnimation;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetGesture.GestureStepChat;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetGesture.GestureStepSound;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetGesture.GestureStepWait;
import com.ngt.jopenmetaverse.shared.sim.events.AutoResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.ManualResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventObservableArg;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.am.AgentDataReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.AlertMessageEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.AnimationsChangedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.AttachmentResourcesCallbackArg;
import com.ngt.jopenmetaverse.shared.sim.events.am.AvatarSitResponseEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.BalanceEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.CameraConstraintEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.ChatEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.ChatSessionMemberAddedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.ChatSessionMemberLeftEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.GroupChatJoinedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.InstantMessageEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.LoadUrlEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.MeanCollisionEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.MoneyBalanceReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.RegionCrossedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.ScriptControlEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.ScriptDialogEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.ScriptQuestionEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.ScriptSensorReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.SetDisplayNameReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.am.TeleportEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.AssetReceivedCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.XferReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.DisconnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.nm.EventQueueRunningEventArgs;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseCallbackArg;
import com.ngt.jopenmetaverse.shared.sim.login.LoginResponseData;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.AttachmentResourcesMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ChatSessionAcceptInvitation;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ChatSessionRequestMuteUpdate;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ChatSessionRequestStartConference;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ChatterBoxInvitationMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ChatterBoxSessionAgentListUpdatesMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ChatterBoxSessionStartReplyMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ChatterboxSessionEventReplyMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.CrossedRegionMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.EstablishAgentCommunicationMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.SetDisplayNameMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.SetDisplayNameReplyMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.TeleportFailedMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.TeleportFinishMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.UpdateAgentLanguageMessage;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.types.Vector4;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.PlatformUtils;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// Manager class for our own avatar
/// </summary>
public class AgentManager {

	/*Patch up code*/

	//region AgentUpdate Constants

	private final static int CONTROL_AT_POS_INDEX = 0;
	private final static int CONTROL_AT_NEG_INDEX = 1;
	private final static int CONTROL_LEFT_POS_INDEX = 2;
	private final static int CONTROL_LEFT_NEG_INDEX = 3;
	private final static int CONTROL_UP_POS_INDEX = 4;
	private final static int CONTROL_UP_NEG_INDEX = 5;
	private final static int CONTROL_PITCH_POS_INDEX = 6;
	private final static int CONTROL_PITCH_NEG_INDEX = 7;
	private final static int CONTROL_YAW_POS_INDEX = 8;
	private final static int CONTROL_YAW_NEG_INDEX = 9;
	private final static int CONTROL_FAST_AT_INDEX = 10;
	private final static int CONTROL_FAST_LEFT_INDEX = 11;
	private final static int CONTROL_FAST_UP_INDEX = 12;
	private final static int CONTROL_FLY_INDEX = 13;
	private final static int CONTROL_STOP_INDEX = 14;
	private final static int CONTROL_FINISH_ANIM_INDEX = 15;
	private final static int CONTROL_STAND_UP_INDEX = 16;
	private final static int CONTROL_SIT_ON_GROUND_INDEX = 17;
	private final static int CONTROL_MOUSELOOK_INDEX = 18;
	private final static int CONTROL_NUDGE_AT_POS_INDEX = 19;
	private final static int CONTROL_NUDGE_AT_NEG_INDEX = 20;
	private final static int CONTROL_NUDGE_LEFT_POS_INDEX = 21;
	private final static int CONTROL_NUDGE_LEFT_NEG_INDEX = 22;
	private final static int CONTROL_NUDGE_UP_POS_INDEX = 23;
	private final static int CONTROL_NUDGE_UP_NEG_INDEX = 24;
	private final static int CONTROL_TURN_LEFT_INDEX = 25;
	private final static int CONTROL_TURN_RIGHT_INDEX = 26;
	private final static int CONTROL_AWAY_INDEX = 27;
	private final static int CONTROL_LBUTTON_DOWN_INDEX = 28;
	private final static int CONTROL_LBUTTON_UP_INDEX = 29;
	private final static int CONTROL_ML_LBUTTON_DOWN_INDEX = 30;
	private final static int CONTROL_ML_LBUTTON_UP_INDEX = 31;
	private final static int TOTAL_CONTROLS = 32;

	//endregion AgentUpdate Constants


	//region Enums
	/// <summary>
	/// Used to specify movement actions for your agent
	/// </summary>
	//[Flags]
	public enum ControlFlags
	{
		//uint
		/// <summary>Empty flag</summary>
		NONE((long)0),
		/// <summary>Move Forward (SL Keybinding: W/Up Arrow)</summary>
		AGENT_CONTROL_AT_POS((long)0x1 << CONTROL_AT_POS_INDEX),
		/// <summary>Move Backward (SL Keybinding: S/Down Arrow)</summary>
		AGENT_CONTROL_AT_NEG((long)0x1 << CONTROL_AT_NEG_INDEX),
		/// <summary>Move Left (SL Keybinding: Shift-(A/Left Arrow))</summary>
		AGENT_CONTROL_LEFT_POS((long)0x1 << CONTROL_LEFT_POS_INDEX),
		/// <summary>Move Right (SL Keybinding: Shift-(D/Right Arrow))</summary>
		AGENT_CONTROL_LEFT_NEG((long)0x1 << CONTROL_LEFT_NEG_INDEX),
		/// <summary>Not Flying: Jump/Flying: Move Up (SL Keybinding: E)</summary>
		AGENT_CONTROL_UP_POS((long)0x1 << CONTROL_UP_POS_INDEX),
		/// <summary>Not Flying: Croutch/Flying: Move Down (SL Keybinding: C)</summary>
		AGENT_CONTROL_UP_NEG((long)0x1 << CONTROL_UP_NEG_INDEX),
		/// <summary>Unused</summary>
		AGENT_CONTROL_PITCH_POS((long)0x1 << CONTROL_PITCH_POS_INDEX),
		/// <summary>Unused</summary>
		AGENT_CONTROL_PITCH_NEG((long)0x1 << CONTROL_PITCH_NEG_INDEX),
		/// <summary>Unused</summary>
		AGENT_CONTROL_YAW_POS((long)0x1 << CONTROL_YAW_POS_INDEX),
		/// <summary>Unused</summary>
		AGENT_CONTROL_YAW_NEG((long)0x1 << CONTROL_YAW_NEG_INDEX),
		/// <summary>ORed with AGENT_CONTROL_AT_* if the keyboard is being used</summary>
		AGENT_CONTROL_FAST_AT((long)0x1 << CONTROL_FAST_AT_INDEX),
		/// <summary>ORed with AGENT_CONTROL_LEFT_* if the keyboard is being used</summary>
		AGENT_CONTROL_FAST_LEFT((long)0x1 << CONTROL_FAST_LEFT_INDEX),
		/// <summary>ORed with AGENT_CONTROL_UP_* if the keyboard is being used</summary>
		AGENT_CONTROL_FAST_UP((long)0x1 << CONTROL_FAST_UP_INDEX),
		/// <summary>Fly</summary>
		AGENT_CONTROL_FLY((long)0x1 << CONTROL_FLY_INDEX),
		/// <summary></summary>
		AGENT_CONTROL_STOP((long)0x1 << CONTROL_STOP_INDEX),
		/// <summary>Finish our current animation</summary>
		AGENT_CONTROL_FINISH_ANIM((long) 0x1 << CONTROL_FINISH_ANIM_INDEX),
		/// <summary>Stand up from the ground or a prim seat</summary>
		AGENT_CONTROL_STAND_UP((long)0x1 << CONTROL_STAND_UP_INDEX),
		/// <summary>Sit on the ground at our current location</summary>
		AGENT_CONTROL_SIT_ON_GROUND((long)0x1 << CONTROL_SIT_ON_GROUND_INDEX),
		/// <summary>Whether mouselook is currently enabled</summary>
		AGENT_CONTROL_MOUSELOOK((long)0x1 << CONTROL_MOUSELOOK_INDEX),
		/// <summary>Legacy, used if a key was pressed for less than a certain amount of time</summary>
		AGENT_CONTROL_NUDGE_AT_POS((long) 0x1 << CONTROL_NUDGE_AT_POS_INDEX),
		/// <summary>Legacy, used if a key was pressed for less than a certain amount of time</summary>
		AGENT_CONTROL_NUDGE_AT_NEG((long) 0x1 << CONTROL_NUDGE_AT_NEG_INDEX),
		/// <summary>Legacy, used if a key was pressed for less than a certain amount of time</summary>
		AGENT_CONTROL_NUDGE_LEFT_POS((long) 0x1 << CONTROL_NUDGE_LEFT_POS_INDEX),
		/// <summary>Legacy, used if a key was pressed for less than a certain amount of time</summary>
		AGENT_CONTROL_NUDGE_LEFT_NEG((long) 0x1 << CONTROL_NUDGE_LEFT_NEG_INDEX),
		/// <summary>Legacy, used if a key was pressed for less than a certain amount of time</summary>
		AGENT_CONTROL_NUDGE_UP_POS((long) 0x1 << CONTROL_NUDGE_UP_POS_INDEX),
		/// <summary>Legacy, used if a key was pressed for less than a certain amount of time</summary>
		AGENT_CONTROL_NUDGE_UP_NEG((long) 0x1 << CONTROL_NUDGE_UP_NEG_INDEX),
		/// <summary></summary>
		AGENT_CONTROL_TURN_LEFT((long)0x1 << CONTROL_TURN_LEFT_INDEX),
		/// <summary></summary>
		AGENT_CONTROL_TURN_RIGHT((long) 0x1 << CONTROL_TURN_RIGHT_INDEX),
		/// <summary>Set when the avatar is idled or set to away. Note that the away animation is 
		/// activated separately from setting this flag</summary>
		AGENT_CONTROL_AWAY((long)0x1 << CONTROL_AWAY_INDEX),
		/// <summary></summary>
		AGENT_CONTROL_LBUTTON_DOWN((long) 0x1 << CONTROL_LBUTTON_DOWN_INDEX),
		/// <summary></summary>
		AGENT_CONTROL_LBUTTON_UP((long) 0x1 << CONTROL_LBUTTON_UP_INDEX),
		/// <summary></summary>
		AGENT_CONTROL_ML_LBUTTON_DOWN((long) 0x1 << CONTROL_ML_LBUTTON_DOWN_INDEX),
		/// <summary></summary>
		AGENT_CONTROL_ML_LBUTTON_UP((long) 0x1 << CONTROL_ML_LBUTTON_UP_INDEX);

		private long index;
		ControlFlags(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,ControlFlags> lookup  = new HashMap<Long,ControlFlags>();

		static {
			for(ControlFlags s : EnumSet.allOf(ControlFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<ControlFlags> get(Long index)
		{
			EnumSet<ControlFlags> enumsSet = EnumSet.allOf(ControlFlags.class);
			for(Entry<Long,ControlFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().longValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static long getIndex(EnumSet<ControlFlags> enumSet)
		{
			long ret = 0;
			for(ControlFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}


	}

	//endregion Enums


	/*Patch up code*/


	/// <summary>
	/// Permission request flags, asked when a script wants to control an Avatar
	/// </summary>
	//    [Flags]
	public enum ScriptPermission 
	{
		/// <summary>Placeholder for empty values, shouldn't ever see this</summary>
		None(0),
		/// <summary>Script wants ability to take money from you</summary>
		Debit(1 << 1),
		/// <summary>Script wants to take camera controls for you</summary>
		TakeControls (1 << 2),
		/// <summary>Script wants to remap avatars controls</summary>
		RemapControls (1 << 3),
		/// <summary>Script wants to trigger avatar animations</summary>
		/// <remarks>This function is not implemented on the grid</remarks>
		TriggerAnimation (1 << 4),
		/// <summary>Script wants to attach or detach the prim or primset to your avatar</summary>
		Attach (1 << 5),
		/// <summary>Script wants permission to release ownership</summary>
		/// <remarks>This function is not implemented on the grid
		/// The concept of "public" objects does not exist anymore.</remarks>
		ReleaseOwnership (1 << 6),
		/// <summary>Script wants ability to link/delink with other prims</summary>
		ChangeLinks (1 << 7),
		/// <summary>Script wants permission to change joints</summary>
		/// <remarks>This function is not implemented on the grid</remarks>
		ChangeJoints (1 << 8),
		/// <summary>Script wants permissions to change permissions</summary>
		/// <remarks>This function is not implemented on the grid</remarks>
		ChangePermissions (1 << 9),
		/// <summary>Script wants to track avatars camera position and rotation </summary>
		TrackCamera (1 << 10),
		/// <summary>Script wants to control your camera</summary>
		ControlCamera (1 << 11);
		private int index;
		ScriptPermission(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,ScriptPermission> lookup  
		= new HashMap<Integer,ScriptPermission>();

		static {
			for(ScriptPermission s : EnumSet.allOf(ScriptPermission.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<ScriptPermission> get(Integer index)
		{
			EnumSet<ScriptPermission> enumsSet = EnumSet.allOf(ScriptPermission.class);
			for(Entry<Integer,ScriptPermission> entry: lookup.entrySet())
			{
				if((entry.getKey().intValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static int getIndex(EnumSet<ScriptPermission> enumSet)
		{
			int ret = 0;
			for(ScriptPermission s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}


	/// <summary>
	/// Special commands used in Instant Messages
	/// </summary>
	public enum InstantMessageDialog
	{
		//ubyte
		/// <summary>Indicates a regular IM from another agent</summary>
		MessageFromAgent((byte)0),
		/// <summary>Simple notification box with an OK button</summary>
		MessageBox((byte)1),
		// <summary>Used to show a countdown notification with an OK
		// button, deprecated now</summary>
		//[Obsolete]
		//MessageBoxCountdown = 2,
		/// <summary>You've been invited to join a group.</summary>
		GroupInvitation((byte)3),
		/// <summary>Inventory offer</summary>
		InventoryOffered((byte)4),
		/// <summary>Accepted inventory offer</summary>
		InventoryAccepted((byte)5),
		/// <summary>Declined inventory offer</summary>
		InventoryDeclined((byte)6),
		/// <summary>Group vote</summary>
		GroupVote((byte)7),
		// <summary>A message to everyone in the agent's group, no longer
		// used</summary>
		//[Obsolete]
		//DeprecatedGroupMessage = 8,
		/// <summary>An object is offering its inventory</summary>
		TaskInventoryOffered((byte)9),
		/// <summary>Accept an inventory offer from an object</summary>
		TaskInventoryAccepted((byte)10),
		/// <summary>Decline an inventory offer from an object</summary>
		TaskInventoryDeclined((byte)11),
		/// <summary>Unknown</summary>
		NewUserDefault((byte)12),
		/// <summary>Start a session, or add users to a session</summary>
		SessionAdd((byte)13),
		/// <summary>Start a session, but don't prune offline users</summary>
		SessionOfflineAdd((byte)14),
		/// <summary>Start a session with your group</summary>
		SessionGroupStart((byte)15),
		/// <summary>Start a session without a calling card (finder or objects)</summary>
		SessionCardlessStart((byte)16),
		/// <summary>Send a message to a session</summary>
		SessionSend((byte)17),
		/// <summary>Leave a session</summary>
		SessionDrop((byte)18),
		/// <summary>Indicates that the IM is from an object</summary>
		MessageFromObject((byte)19),
		/// <summary>Sent an IM to a busy user, this is the auto response</summary>
		BusyAutoResponse((byte)20),
		/// <summary>Shows the message in the console and chat history</summary>
		ConsoleAndChatHistory((byte)21),
		/// <summary>Send a teleport lure</summary>
		RequestTeleport((byte)22),
		/// <summary>Response sent to the agent which inititiated a teleport invitation</summary>
		AcceptTeleport((byte)23),
		/// <summary>Response sent to the agent which inititiated a teleport invitation</summary>
		DenyTeleport((byte)24),
		/// <summary>Only useful if you have Linden permissions</summary>
		GodLikeRequestTeleport((byte)25),
		/// <summary>A placeholder type for future expansion, currently not
		/// used</summary>
		CurrentlyUnused((byte)26),
		// <summary>Notification of a new group election, this is 
		// deprecated</summary>
		//[Obsolete]
		//DeprecatedGroupElection = 27,
		/// <summary>IM to tell the user to go to an URL</summary>
		GotoUrl((byte)28),
		/// <summary>IM for help</summary>
		Session911Start((byte)29),
		/// <summary>IM sent automatically on call for help, sends a lure 
		/// to each Helper reached</summary>
		Lure911((byte)30),
		/// <summary>Like an IM but won't go to email</summary>
		FromTaskAsAlert((byte)31),
		/// <summary>IM from a group officer to all group members</summary>
		GroupNotice((byte)32),
		/// <summary>Unknown</summary>
		GroupNoticeInventoryAccepted((byte)33),
		/// <summary>Unknown</summary>
		GroupNoticeInventoryDeclined((byte)34),
		/// <summary>Accept a group invitation</summary>
		GroupInvitationAccept((byte)35),
		/// <summary>Decline a group invitation</summary>
		GroupInvitationDecline((byte)36),
		/// <summary>Unknown</summary>
		GroupNoticeRequested((byte)37),
		/// <summary>An avatar is offering you friendship</summary>
		FriendshipOffered((byte)38),
		/// <summary>An avatar has accepted your friendship offer</summary>
		FriendshipAccepted((byte)39),
		/// <summary>An avatar has declined your friendship offer</summary>
		FriendshipDeclined((byte)40),
		/// <summary>Indicates that a user has started typing</summary>
		StartTyping((byte)41),
		/// <summary>Indicates that a user has stopped typing</summary>
		StopTyping((byte)42);
		private byte index;
		InstantMessageDialog(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,InstantMessageDialog> lookup  = new HashMap<Byte,InstantMessageDialog>();

		static {
			for(InstantMessageDialog s : EnumSet.allOf(InstantMessageDialog.class))
				lookup.put(s.getIndex(), s);
		}

		public static InstantMessageDialog get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Flag in Instant Messages, whether the IM should be delivered to
	/// offline avatars as well
	/// </summary>
	public enum InstantMessageOnline
	{
		/// <summary>Only deliver to online avatars</summary>
		Online (0),
		/// <summary>If the avatar is offline the message will be held until
		/// they login next, and possibly forwarded to their e-mail account</summary>
		Offline (1);
		private int index;
		InstantMessageOnline(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,InstantMessageOnline> lookup  = new HashMap<Integer,InstantMessageOnline>();

		static {
			for(InstantMessageOnline s : EnumSet.allOf(InstantMessageOnline.class))
				lookup.put(s.getIndex(), s);
		}

		public static InstantMessageOnline get(Integer index)
		{
			return lookup.get(index);
		}
	}


	/// <summary>
	/// Conversion type to denote Chat Packet types in an easier-to-understand format
	/// </summary>
	public enum ChatType
	{
		//ubyte
		/// <summary>Whisper (5m radius)</summary>
		Whisper((byte)0),
		/// <summary>Normal chat (10/20m radius), what the official viewer typically sends</summary>
		Normal((byte)1),
		/// <summary>Shouting! (100m radius)</summary>
		Shout((byte)2),
		// <summary>Say chat (10/20m radius) - The official viewer will 
		// print "[4:15] You say, hey" instead of "[4:15] You: hey"</summary>
		//[Obsolete]
		//Say = 3,
		/// <summary>Event message when an Avatar has begun to type</summary>
		StartTyping((byte)4),
		/// <summary>Event message when an Avatar has stopped typing</summary>
		StopTyping((byte)5),
		/// <summary>Send the message to the debug channel</summary>
		Debug((byte)6),
		/// <summary>Event message when an object uses llOwnerSay</summary>
		OwnerSay((byte)8),
		/// <summary>Special value to support llRegionSay, never sent to the client</summary>
		RegionSay((byte)Byte.MAX_VALUE);
		private byte index;
		ChatType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,ChatType> lookup  = new HashMap<Byte,ChatType>();

		static {
			for(ChatType s : EnumSet.allOf(ChatType.class))
				lookup.put(s.getIndex(), s);
		}

		public static ChatType get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Identifies the source of a chat message
	/// </summary>
	public enum ChatSourceType
	{
		//ubyte
		/// <summary>Chat from the grid or simulator</summary>
		System((byte)0),
		/// <summary>Chat from another avatar</summary>
		Agent((byte)1),
		/// <summary>Chat from an object</summary>
		Object((byte)2);
		private byte index;
		ChatSourceType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,ChatSourceType> lookup  = new HashMap<Byte,ChatSourceType>();

		static {
			for(ChatSourceType s : EnumSet.allOf(ChatSourceType.class))
				lookup.put(s.getIndex(), s);
		}

		public static ChatSourceType get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// 
	/// </summary>
	public enum ChatAudibleLevel
	{
		/// <summary></summary>
		Not((byte)-1),
		/// <summary></summary>
		Barely((byte)0),
		/// <summary></summary>
		Fully((byte)1);
		private byte index;
		ChatAudibleLevel(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,ChatAudibleLevel> lookup  = new HashMap<Byte,ChatAudibleLevel>();

		static {
			for(ChatAudibleLevel s : EnumSet.allOf(ChatAudibleLevel.class))
				lookup.put(s.getIndex(), s);
		}

		public static ChatAudibleLevel get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Effect type used in ViewerEffect packets
	/// </summary>
	public enum EffectType
	{
		//ubyte
		/// <summary></summary>
		Text((byte)0),
		/// <summary></summary>
		Icon((byte)1),
		/// <summary></summary>
		Connector((byte)2),
		/// <summary></summary>
		FlexibleObject((byte)3),
		/// <summary></summary>
		AnimalControls((byte)4),
		/// <summary></summary>
		AnimationObject((byte)5),
		/// <summary></summary>
		Cloth((byte)6),
		/// <summary>Project a beam from a source to a destination, such as
		/// the one used when editing an object</summary>
		Beam((byte)7),
		/// <summary></summary>
		Glow((byte)8),
		/// <summary></summary>
		Point((byte)9),
		/// <summary></summary>
		Trail((byte)10),
		/// <summary>Create a swirl of particles around an object</summary>
		Sphere((byte)11),
		/// <summary></summary>
		Spiral((byte)12),
		/// <summary></summary>
		Edit((byte)13),
		/// <summary>Cause an avatar to look at an object</summary>
		LookAt((byte)14),
		/// <summary>Cause an avatar to point at an object</summary>
		PointAt((byte)15);
		private byte index;
		EffectType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,EffectType> lookup  = new HashMap<Byte,EffectType>();

		static {
			for(EffectType s : EnumSet.allOf(EffectType.class))
				lookup.put(s.getIndex(), s);
		}

		public static EffectType get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// The action an avatar is doing when looking at something, used in 
	/// ViewerEffect packets for the LookAt effect
	/// </summary>
	public enum LookAtType
	{
		/// <summary></summary>
		None((byte)0),
		/// <summary></summary>
		Idle((byte)1),
		/// <summary></summary>
		AutoListen((byte)2),
		/// <summary></summary>
		FreeLook((byte)3),
		/// <summary></summary>
		Respond((byte)4),
		/// <summary></summary>
		Hover((byte)5),
		/// <summary>Deprecated</summary>
		//	        [Obsolete]
		Conversation((byte)6),
		/// <summary></summary>
		Select((byte)7),
		/// <summary></summary>
		Focus((byte)8),
		/// <summary></summary>
		Mouselook((byte)9),
		/// <summary></summary>
		Clear((byte)10);
		private byte index;
		LookAtType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,LookAtType> lookup  = new HashMap<Byte,LookAtType>();

		static {
			for(LookAtType s : EnumSet.allOf(LookAtType.class))
				lookup.put(s.getIndex(), s);
		}

		public static LookAtType get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// The action an avatar is doing when pointing at something, used in
	/// ViewerEffect packets for the PointAt effect
	/// </summary>
	public enum PointAtType
	{
		/// <summary></summary>
		None((byte)0),
		/// <summary></summary>
		Select((byte)1),
		/// <summary></summary>
		Grab((byte)2),
		/// <summary></summary>
		Clear((byte)3);
		private byte index;
		PointAtType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,PointAtType> lookup  = new HashMap<Byte,PointAtType>();

		static {
			for(PointAtType s : EnumSet.allOf(PointAtType.class))
				lookup.put(s.getIndex(), s);
		}

		public static PointAtType get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Money transaction types
	/// </summary>
	public enum MoneyTransactionType
	{
		/// <summary></summary>
		None(0),
		/// <summary></summary>
		FailSimulatorTimeout(1),
		/// <summary></summary>
		FailDataserverTimeout(2),
		/// <summary></summary>
		ObjectClaim(1000),
		/// <summary></summary>
		LandClaim(1001),
		/// <summary></summary>
		GroupCreate(1002),
		/// <summary></summary>
		ObjectPublicClaim(1003),
		/// <summary></summary>
		GroupJoin(1004),
		/// <summary></summary>
		TeleportCharge(1100),
		/// <summary></summary>
		UploadCharge(1101),
		/// <summary></summary>
		LandAuction(1102),
		/// <summary></summary>
		ClassifiedCharge(1103),
		/// <summary></summary>
		ObjectTax(2000),
		/// <summary></summary>
		LandTax(2001),
		/// <summary></summary>
		LightTax(2002),
		/// <summary></summary>
		ParcelDirFee(2003),
		/// <summary></summary>
		GroupTax(2004),
		/// <summary></summary>
		ClassifiedRenew(2005),
		/// <summary></summary>
		GiveInventory(3000),
		/// <summary></summary>
		ObjectSale(5000),
		/// <summary></summary>
		Gift(5001),
		/// <summary></summary>
		LandSale(5002),
		/// <summary></summary>
		ReferBonus(5003),
		/// <summary></summary>
		InventorySale(5004),
		/// <summary></summary>
		RefundPurchase(5005),
		/// <summary></summary>
		LandPassSale(5006),
		/// <summary></summary>
		DwellBonus(5007),
		/// <summary></summary>
		PayObject(5008),
		/// <summary></summary>
		ObjectPays(5009),
		/// <summary></summary>
		GroupLandDeed(6001),
		/// <summary></summary>
		GroupObjectDeed(6002),
		/// <summary></summary>
		GroupLiability(6003),
		/// <summary></summary>
		GroupDividend(6004),
		/// <summary></summary>
		GroupMembershipDues(6005),
		/// <summary></summary>
		ObjectRelease(8000),
		/// <summary></summary>
		LandRelease(8001),
		/// <summary></summary>
		ObjectDelete(8002),
		/// <summary></summary>
		ObjectPublicDecay(8003),
		/// <summary></summary>
		ObjectPublicDelete(8004),
		/// <summary></summary>
		LindenAdjustment(9000),
		/// <summary></summary>
		LindenGrant(9001),
		/// <summary></summary>
		LindenPenalty(9002),
		/// <summary></summary>
		EventFee(9003),
		/// <summary></summary>
		EventPrize(9004),
		/// <summary></summary>
		StipendBasic(10000),
		/// <summary></summary>
		StipendDeveloper(10001),
		/// <summary></summary>
		StipendAlways(10002),
		/// <summary></summary>
		StipendDaily(10003),
		/// <summary></summary>
		StipendRating(10004),
		/// <summary></summary>
		StipendDelta(10005);

		private int index;
		MoneyTransactionType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,MoneyTransactionType> lookup  
		= new HashMap<Integer,MoneyTransactionType>();

		static {
			for(MoneyTransactionType s : EnumSet.allOf(MoneyTransactionType.class))
				lookup.put(s.getIndex(), s);
		}

		public static MoneyTransactionType get(Integer index)
		{
			return lookup.get(index);
		}
	}
	/// <summary>
	/// 
	/// </summary>
	//	    [Flags]
	public enum TransactionFlags
	{
		/// <summary></summary>
		None((byte)0),
		/// <summary></summary>
		SourceGroup((byte)1),
		/// <summary></summary>
		DestGroup((byte)2),
		/// <summary></summary>
		OwnerGroup((byte)4),
		/// <summary></summary>
		SimultaneousContribution((byte)8),
		/// <summary></summary>
		ContributionRemoval((byte)16);
		private byte index;
		TransactionFlags(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,TransactionFlags> lookup  = new HashMap<Byte,TransactionFlags>();

		static {
			for(TransactionFlags s : EnumSet.allOf(TransactionFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<TransactionFlags> get(Byte index)
		{
			EnumSet<TransactionFlags> enumsSet = EnumSet.allOf(TransactionFlags.class);
			for(Entry<Byte,TransactionFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().byteValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static byte getIndex(EnumSet<TransactionFlags> enumSet)
		{
			byte ret = 0;
			for(TransactionFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}
	/// <summary>
	/// 
	/// </summary>
	public enum MeanCollisionType
	{
		/// <summary></summary>
		None((byte)0),
		/// <summary></summary>
		Bump((byte)1),
		/// <summary></summary>
		LLPushObject((byte)2),
		/// <summary></summary>
		SelectedObjectCollide((byte)3),
		/// <summary></summary>
		ScriptedObjectCollide((byte)4),
		/// <summary></summary>
		PhysicalObjectCollide((byte)5);
		private byte index;
		MeanCollisionType(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,MeanCollisionType> lookup  = new HashMap<Byte,MeanCollisionType>();

		static {
			for(MeanCollisionType s : EnumSet.allOf(MeanCollisionType.class))
				lookup.put(s.getIndex(), s);
		}

		public static MeanCollisionType get(Byte index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Flags sent when a script takes or releases a control
	/// </summary>
	/// <remarks>NOTE: (need to verify) These might be a subset of the ControlFlags enum in Movement,</remarks>
	//	    [Flags]
	public enum ScriptControlChange
	{
		//uint
		/// <summary>No Flags set</summary>
		None((long)0),
		/// <summary>Forward (W or up Arrow)</summary>
		Forward((long)1),
		/// <summary>Back (S or down arrow)</summary>
		Back((long)2),
		/// <summary>Move left (shift+A or left arrow)</summary>
		Left((long)4),
		/// <summary>Move right (shift+D or right arrow)</summary>
		Right((long)8),
		/// <summary>Up (E or PgUp)</summary>
		Up((long)16),
		/// <summary>Down (C or PgDown)</summary>
		Down((long)32),
		/// <summary>Rotate left (A or left arrow)</summary>
		RotateLeft((long)256),
		/// <summary>Rotate right (D or right arrow)</summary>
		RotateRight((long)512),
		/// <summary>Left Mouse Button</summary>
		LeftButton((long)268435456),
		/// <summary>Left Mouse button in MouseLook</summary>
		MouseLookLeftButton((long)1073741824);
		private long index;
		ScriptControlChange(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,ScriptControlChange> lookup  = new HashMap<Long,ScriptControlChange>();

		static {
			for(ScriptControlChange s : EnumSet.allOf(ScriptControlChange.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<ScriptControlChange> get(Long index)
		{
			EnumSet<ScriptControlChange> enumsSet = EnumSet.allOf(ScriptControlChange.class);
			for(Entry<Long,ScriptControlChange> entry: lookup.entrySet())
			{
				if((entry.getKey().longValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static long getIndex(EnumSet<ScriptControlChange> enumSet)
		{
			long ret = 0;
			for(ScriptControlChange s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}


	}

	/// <summary>
	/// Currently only used to hide your group title
	/// </summary>
	public enum AgentFlags
	{
		/// <summary>No flags set</summary>
		None((byte)0),
		/// <summary>Hide your group title</summary>
		HideTitle((byte)0x01);
		private byte index;
		AgentFlags(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		} 

		private static final Map<Byte,AgentFlags> lookup  = new HashMap<Byte,AgentFlags>();

		static {
			for(AgentFlags s : EnumSet.allOf(AgentFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<AgentFlags> get(Byte index)
		{
			EnumSet<AgentFlags> enumsSet = EnumSet.allOf(AgentFlags.class);
			for(Entry<Byte,AgentFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().byteValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static byte getIndex(EnumSet<AgentFlags> enumSet)
		{
			byte ret = 0;
			for(AgentFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}

	/// <summary>
	/// Action state of the avatar, which can currently be typing and
	/// editing
	/// </summary>
	//	    [Flags]
	public enum AgentState 
	{
		/// <summary></summary>
		None((byte)0x00),
		/// <summary></summary>
		Typing((byte)0x04),
		/// <summary></summary>
		Editing((byte)0x10);

		private byte index;
		AgentState(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}  

		private static final Map<Byte,AgentState> lookup  = new HashMap<Byte,AgentState>();

		static {
			for(AgentState s : EnumSet.allOf(AgentState.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<AgentState> get(Byte index)
		{
			EnumSet<AgentState> enumsSet = EnumSet.allOf(AgentState.class);
			for(Entry<Byte,AgentState> entry: lookup.entrySet())
			{
				if((entry.getKey().byteValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static byte getIndex(EnumSet<AgentState> enumSet)
		{
			byte ret = 0;
			for(AgentState s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}

	/// <summary>
	/// Current teleport status
	/// </summary>
	public enum TeleportStatus
	{
		/// <summary>Unknown status</summary>
		None,
		/// <summary>Teleport initialized</summary>
		Start,
		/// <summary>Teleport in progress</summary>
		Progress,
		/// <summary>Teleport failed</summary>
		Failed,
		/// <summary>Teleport completed</summary>
		Finished,
		/// <summary>Teleport cancelled</summary>
		Cancelled
	}

	/// <summary>
	/// 
	/// </summary>
	//	    [Flags]
	public enum TeleportFlags
	{
		//uint
		/// <summary>No flags set, or teleport failed</summary>
		Default((long)0),
		/// <summary>Set when newbie leaves help island for first time</summary>
		SetHomeToTarget((long)1 << 0),
		/// <summary></summary>
		SetLastToTarget((long)1 << 1),
		/// <summary>Via Lure</summary>
		ViaLure((long)1 << 2),
		/// <summary>Via Landmark</summary>
		ViaLandmark((long)1 << 3),
		/// <summary>Via Location</summary>
		ViaLocation((long)1 << 4),
		/// <summary>Via Home</summary>
		ViaHome((long)1 << 5),
		/// <summary>Via Telehub</summary>
		ViaTelehub((long)1 << 6),
		/// <summary>Via Login</summary>
		ViaLogin((long)1 << 7),
		/// <summary>Linden Summoned</summary>
		ViaGodlikeLure((long)1 << 8),
		/// <summary>Linden Forced me</summary>
		Godlike((long)1 << 9),
		/// <summary></summary>
		NineOneOne((long)1 << 10),
		/// <summary>Agent Teleported Home via Script</summary>
		DisableCancel((long)1 << 11),
		/// <summary></summary>
		ViaRegionID((long)1 << 12),
		/// <summary></summary>
		IsFlying((long)1 << 13),
		/// <summary></summary>
		ResetHome((long)1 << 14),
		/// <summary>forced to new location for example when avatar is banned or ejected</summary>
		ForceRedirect((long)1 << 15),
		/// <summary>Teleport Finished via a Lure</summary>
		FinishedViaLure((long)1 << 26),
		/// <summary>Finished, Sim Changed</summary>
		FinishedViaNewSim((long)1 << 28),
		/// <summary>Finished, Same Sim</summary>
		FinishedViaSameSim((long)1 << 29);
		private long index;
		TeleportFlags(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,TeleportFlags> lookup  = new HashMap<Long,TeleportFlags>();

		static {
			for(TeleportFlags s : EnumSet.allOf(TeleportFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<TeleportFlags> get(Long index)
		{
			EnumSet<TeleportFlags> enumsSet = EnumSet.allOf(TeleportFlags.class);
			for(Entry<Long,TeleportFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().longValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static long getIndex(EnumSet<TeleportFlags> enumSet)
		{
			long ret = 0;
			for(TeleportFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}

	/// <summary>
	/// 
	/// </summary>
	//	    [Flags]
	public enum TeleportLureFlags
	{
		/// <summary></summary>
		NormalLure(0),
		/// <summary></summary>
		GodlikeLure(1),
		/// <summary></summary>
		GodlikePursuit(2);
		private int index;
		TeleportLureFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,TeleportLureFlags> lookup  
		= new HashMap<Integer,TeleportLureFlags>();

		static {
			for(TeleportLureFlags s : EnumSet.allOf(TeleportLureFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<TeleportLureFlags> get(Integer index)
		{
			EnumSet<TeleportLureFlags> enumsSet = EnumSet.allOf(TeleportLureFlags.class);
			for(Entry<Integer,TeleportLureFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().intValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static int getIndex(EnumSet<TeleportLureFlags> enumSet)
		{
			int ret = 0;
			for(TeleportLureFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}

	/// <summary>
	/// 
	/// </summary>
	//	    [Flags]
	public enum ScriptSensorTypeFlags
	{
		/// <summary></summary>
		Agent(1),
		/// <summary></summary>
		Active(2),
		/// <summary></summary>
		Passive(4),
		/// <summary></summary>
		Scripted(8);
		private int index;
		ScriptSensorTypeFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,ScriptSensorTypeFlags> lookup  
		= new HashMap<Integer,ScriptSensorTypeFlags>();

		static {
			for(ScriptSensorTypeFlags s : EnumSet.allOf(ScriptSensorTypeFlags.class))
				lookup.put(s.getIndex(), s);
		}


		public static EnumSet<ScriptSensorTypeFlags> get(Integer index)
		{
			EnumSet<ScriptSensorTypeFlags> enumsSet = EnumSet.allOf(ScriptSensorTypeFlags.class);
			for(Entry<Integer,ScriptSensorTypeFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().intValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static int getIndex(EnumSet<ScriptSensorTypeFlags> enumSet)
		{
			int ret = 0;
			for(ScriptSensorTypeFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}

	/// <summary>
	/// Type of mute entry
	/// </summary>
	public enum MuteType
	{
		/// <summary>Object muted by name</summary>
		ByName(0),
		/// <summary>Muted residet</summary>
		Resident(1),
		/// <summary>Object muted by UUID</summary>
		Object(2),
		/// <summary>Muted group</summary>
		Group(3),
		/// <summary>Muted external entry</summary>
		External(4);
		private int index;
		MuteType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,MuteType> lookup  
		= new HashMap<Integer,MuteType>();

		static {
			for(MuteType s : EnumSet.allOf(MuteType.class))
				lookup.put(s.getIndex(), s);
		}

		public static MuteType get(Integer index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Flags of mute entry
	/// </summary>
	//	    [Flags]
	public enum MuteFlags
	{
		/// <summary>No exceptions</summary>
		Default(0x0),
		/// <summary>Don't mute text chat</summary>
		TextChat(0x1),
		/// <summary>Don't mute voice chat</summary>
		VoiceChat (0x2),
		/// <summary>Don't mute particles</summary>
		Particles (0x4),
		/// <summary>Don't mute sounds</summary>
		ObjectSounds (0x8),
		/// <summary>Don't mute</summary>
		All (0xf);
		private int index;
		MuteFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,MuteFlags> lookup  
		= new HashMap<Integer,MuteFlags>();

		static {
			for(MuteFlags s : EnumSet.allOf(MuteFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EnumSet<MuteFlags> get(Integer index)
		{
			EnumSet<MuteFlags> enumsSet = EnumSet.allOf(MuteFlags.class);
			for(Entry<Integer,MuteFlags> entry: lookup.entrySet())
			{
				if((entry.getKey().intValue() | index) != index)
				{
					enumsSet.remove(entry.getValue());
				}
			}
			return enumsSet;
		}

		public static int getIndex(EnumSet<MuteFlags> enumSet)
		{
			int ret = 0;
			for(MuteFlags s: enumSet)
			{
				ret |= s.getIndex();
			}
			return ret;
		}

	}
	//endregion Enums

	//region Structs

	/// <summary>
	/// Instant Message
	/// </summary>
	//struct
	public  class InstantMessage
	{
		/// <summary>Key of sender</summary>
		public UUID FromAgentID;
		/// <summary>Name of sender</summary>
		public String FromAgentName;
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
		public String Message;
		/// <summary>Whether this message is held for offline avatars</summary>
		public InstantMessageOnline Offline;
		/// <summary>Context specific packed data</summary>
		public byte[] BinaryBucket;

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A String containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			try {
				return Helpers.StructToString(this);
			} catch (Exception e) {
				JLogger.warn("Error " + Utils.getExceptionStackTraceAsString(e));
			}
			return super.toString();
		}
	}

	/// <summary>Represents muted object or resident</summary>
	public class MuteEntry
	{
		/// <summary>Type of the mute entry</summary>
		public MuteType Type;
		/// <summary>UUID of the mute etnry</summary>
		public UUID ID;
		/// <summary>Mute entry name</summary>
		public String Name;
		/// <summary>Mute flags</summary>
		public EnumSet<MuteFlags> Flags;
	}

	/// <summary>Transaction detail sent with MoneyBalanceReply message</summary>
	public class TransactionInfo
	{
		/// <summary>Type of the transaction</summary>
		public int TransactionType; // FIXME: this should be an enum
		/// <summary>UUID of the transaction source</summary>
		public UUID SourceID;
		/// <summary>Is the transaction source a group</summary>
		public boolean IsSourceGroup;
		/// <summary>UUID of the transaction destination</summary>
		public UUID DestID;
		/// <summary>Is transaction destination a group</summary>
		public boolean IsDestGroup;
		/// <summary>Transaction amount</summary>
		public int Amount;
		/// <summary>Transaction description</summary>
		public String ItemDescription;
	}
	//endregion Structs

	private EventObservable<ChatEventArgs> onChatFromSimulator = new EventObservable<ChatEventArgs>();
	private EventObservable<ScriptDialogEventArgs> onScriptDialog = new EventObservable<ScriptDialogEventArgs>();
	private EventObservable<ScriptQuestionEventArgs> onScriptQuestion = new EventObservable<ScriptQuestionEventArgs>();


	public void registerChatFromSimulator(EventObserver<ChatEventArgs> o)
	{
		onChatFromSimulator.addObserver(o);
	}
	public void unregisterChatFromSimulator(EventObserver<ChatEventArgs> o)
	{
		onChatFromSimulator.deleteObserver(o);
	}
	public void registerScriptDialog(EventObserver<ScriptDialogEventArgs> o)
	{
		onScriptDialog.addObserver(o);
	}

	public void unregisterScriptDialog(EventObserver<ScriptDialogEventArgs> o)
	{
		onScriptDialog.deleteObserver(o);
	}

	public void registerScriptQuestion(EventObserver<ScriptQuestionEventArgs> o)
	{
		onScriptQuestion.addObserver(o);
	}
	public void unregisterScriptQuestion(EventObserver<ScriptQuestionEventArgs> o)
	{
		onScriptQuestion.deleteObserver(o);
	}

	private EventObservable<LoadUrlEventArgs> onLoadURL = new EventObservable<LoadUrlEventArgs>();
	public void registerLoadURL(EventObserver<LoadUrlEventArgs> o)
	{
		onLoadURL.addObserver(o);
	}
	public void unregisterLoadURL(EventObserver<LoadUrlEventArgs> o)
	{
		onLoadURL.deleteObserver(o);
	}	

	private EventObservable<BalanceEventArgs> onMoneyBalance = new EventObservable<BalanceEventArgs>();
	public void registerMoneyBalance(EventObserver<BalanceEventArgs> o)
	{
		onMoneyBalance.addObserver(o);
	}
	public void unregisterMoneyBalance(EventObserver<BalanceEventArgs> o)
	{
		onMoneyBalance.deleteObserver(o);
	}

	private EventObservable<MoneyBalanceReplyEventArgs> onMoneyBalanceReply = new EventObservable<MoneyBalanceReplyEventArgs>();
	public void registerMoneyBalanceReply(EventObserver<MoneyBalanceReplyEventArgs> o)
	{
		onMoneyBalanceReply.addObserver(o);
	}
	public void unregisterMoneyBalanceReply(EventObserver<MoneyBalanceReplyEventArgs> o)
	{
		onMoneyBalanceReply.deleteObserver(o);
	}	

	private EventObservable<InstantMessageEventArgs> onIM = new EventObservable<InstantMessageEventArgs>();
	public void registerIM(EventObserver<InstantMessageEventArgs> o)
	{
		onIM.addObserver(o);
	}
	public void unregisterIM(EventObserver<InstantMessageEventArgs> o)
	{
		onIM.deleteObserver(o);
	}	

	private EventObservable<TeleportEventArgs> onTeleportProgress = new EventObservable<TeleportEventArgs>();
	public void registerTeleportProgress(EventObserver<TeleportEventArgs> o)
	{
		onTeleportProgress.addObserver(o);
	}
	public void unregisterTeleportProgress(EventObserver<TeleportEventArgs> o)
	{
		onTeleportProgress.deleteObserver(o);
	}

	private EventObservable<AgentDataReplyEventArgs> onAgentDataReply = new EventObservable<AgentDataReplyEventArgs>();
	public void registerAgentDataReply(EventObserver<AgentDataReplyEventArgs> o)
	{
		onAgentDataReply.addObserver(o);
	}
	public void unregisterAgentDataReply(EventObserver<AgentDataReplyEventArgs> o)
	{
		onAgentDataReply.deleteObserver(o);
	}	

	private EventObservable<AnimationsChangedEventArgs> onAnimationsChanged = new EventObservable<AnimationsChangedEventArgs>();
	public void registerAnimationsChanged(EventObserver<AnimationsChangedEventArgs> o)
	{
		onAnimationsChanged.addObserver(o);
	}
	public void unregisterAnimationsChanged(EventObserver<AnimationsChangedEventArgs> o)
	{
		onAnimationsChanged.deleteObserver(o);
	}

	private EventObservable<MeanCollisionEventArgs> onMeanCollision = new EventObservable<MeanCollisionEventArgs>();
	public void registerMeanCollision(EventObserver<MeanCollisionEventArgs> o)
	{
		onMeanCollision.addObserver(o);
	}
	public void unregisterMeanCollision(EventObserver<MeanCollisionEventArgs> o)
	{
		onMeanCollision.deleteObserver(o);
	}

	private EventObservable<RegionCrossedEventArgs> onRegionCrossed = new EventObservable<RegionCrossedEventArgs>();
	public void registerRegionCrossed(EventObserver<RegionCrossedEventArgs> o)
	{
		onRegionCrossed.addObserver(o);
	}
	public void unregisterRegionCrossed(EventObserver<RegionCrossedEventArgs> o)
	{
		onRegionCrossed.deleteObserver(o);
	}

	private EventObservable<GroupChatJoinedEventArgs> onGroupChatJoined = new EventObservable<GroupChatJoinedEventArgs>();
	public void registerGroupChatJoined(EventObserver<GroupChatJoinedEventArgs> o)
	{
		onGroupChatJoined.addObserver(o);
	}
	public void unregisterGroupChatJoined(EventObserver<GroupChatJoinedEventArgs> o)
	{
		onGroupChatJoined.deleteObserver(o);
	}

	private EventObservable<AlertMessageEventArgs> onAlertMessage = new EventObservable<AlertMessageEventArgs>();
	public void registerAlertMessage(EventObserver<AlertMessageEventArgs> o)
	{
		onAlertMessage.addObserver(o);
	}
	public void unregisterAlertMessage(EventObserver<AlertMessageEventArgs> o)
	{
		onAlertMessage.deleteObserver(o);
	}

	private EventObservable<ScriptControlEventArgs> onOnScriptControlChange = new EventObservable<ScriptControlEventArgs>();
	public void registerOnScriptControlChange(EventObserver<ScriptControlEventArgs> o)
	{
		onOnScriptControlChange.addObserver(o);
	}
	public void unregisterOnScriptControlChange(EventObserver<ScriptControlEventArgs> o)
	{
		onOnScriptControlChange.deleteObserver(o);
	}	

	private EventObservable<CameraConstraintEventArgs> onCameraConstraint = new EventObservable<CameraConstraintEventArgs>();
	public void registerCameraConstraint(EventObserver<CameraConstraintEventArgs> o)
	{
		onCameraConstraint.addObserver(o);
	}
	public void unregisterCameraConstraint(EventObserver<CameraConstraintEventArgs> o)
	{
		onCameraConstraint.deleteObserver(o);
	}	
	private EventObservable<AvatarSitResponseEventArgs> onAvatarSitResponse = new EventObservable<AvatarSitResponseEventArgs>();
	public void registerAvatarSitResponse(EventObserver<AvatarSitResponseEventArgs> o)
	{
		onAvatarSitResponse.addObserver(o);
	}
	public void unregisterAvatarSitResponse(EventObserver<AvatarSitResponseEventArgs> o)
	{
		onAvatarSitResponse.deleteObserver(o);
	}

	private EventObservable<ScriptSensorReplyEventArgs> onScriptSensorReply = new EventObservable<ScriptSensorReplyEventArgs>();
	public void registerScriptSensorReply(EventObserver<ScriptSensorReplyEventArgs> o)
	{
		onScriptSensorReply.addObserver(o);
	}
	public void unregisterScriptSensorReply(EventObserver<ScriptSensorReplyEventArgs> o)
	{
		onScriptSensorReply.deleteObserver(o);
	}

	private EventObservable<ChatSessionMemberAddedEventArgs> onChatSessionMemberAdded = new EventObservable<ChatSessionMemberAddedEventArgs>();
	public void registerChatSessionMemberAdded(EventObserver<ChatSessionMemberAddedEventArgs> o)
	{
		onChatSessionMemberAdded.addObserver(o);
	}
	public void unregisterChatSessionMemberAdded(EventObserver<ChatSessionMemberAddedEventArgs> o)
	{
		onChatSessionMemberAdded.deleteObserver(o);
	}	

	private EventObservable<ChatSessionMemberLeftEventArgs> onChatSessionMemberLeft = new EventObservable<ChatSessionMemberLeftEventArgs>();
	public void registerChatSessionMemberLeft(EventObserver<ChatSessionMemberLeftEventArgs> o)
	{
		onChatSessionMemberLeft.addObserver(o);
	}
	public void unregisterChatSessionMemberLeft(EventObserver<ChatSessionMemberLeftEventArgs> o)
	{
		onChatSessionMemberLeft.addObserver(o);
	}

	private EventObservable<SetDisplayNameReplyEventArgs> onSetDisplayNameReply = new EventObservable<SetDisplayNameReplyEventArgs>();
	public void registerSetDisplayNameReply(EventObserver<SetDisplayNameReplyEventArgs> o)
	{
		onSetDisplayNameReply.addObserver(o);
	}
	public void unregisterSetDisplayNameReply(EventObserver<SetDisplayNameReplyEventArgs> o)
	{
		onSetDisplayNameReply.deleteObserver(o);
	}

	private EventObservable<EventArgs> onMuteListUpdated = new EventObservable<EventArgs>();
	public void registerMuteListUpdated(EventObserver<EventArgs> o)
	{
		onMuteListUpdated.addObserver(o);
	}
	public void unregisterMuteListUpdated(EventObserver<EventArgs> o)
	{
		onMuteListUpdated.deleteObserver(o);
	}

	//	        //region Delegates
	//	        /// <summary>
	//	        /// Called once attachment resource usage information has been collected
	//	        /// </summary>
	//	        /// <param name="success">Indicates if operation was successfull</param>
	//	        /// <param name="info">Attachment resource usage information</param>
	//	        public delegate void AttachmentResourcesCallback(boolean success, AttachmentResourcesMessage info);
	//	        //endregion Delegates
	//	
	//	        //region Event Delegates
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<ChatEventArgs> m_Chat;
	//	
	//	        /// <summary>Raises the ChatFromSimulator event</summary>
	//	        /// <param name="e">A ChatEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnChat(ChatEventArgs e)
	//	        {
	//	            EventHandler<ChatEventArgs> handler = m_Chat;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ChatLock = new object();
	//	
	//	        /// <summary>Raised when a scripted object or agent within range sends a public message</summary>
	//	        public event EventHandler<ChatEventArgs> ChatFromSimulator 	
	//	        {
	//	            add { synchronized (m_ChatLock) { m_Chat += value; } }
	//	            remove { synchronized (m_ChatLock) { m_Chat -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<ScriptDialogEventArgs> m_ScriptDialog;
	//	
	//	        /// <summary>Raises the ScriptDialog event</summary>
	//	        /// <param name="e">A SctriptDialogEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnScriptDialog(ScriptDialogEventArgs e)
	//	        {
	//	            EventHandler<ScriptDialogEventArgs> handler = m_ScriptDialog;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ScriptDialogLock = new object();
	//	        /// <summary>Raised when a scripted object sends a dialog box containing possible
	//	        /// options an agent can respond to</summary>
	//	        public event EventHandler<ScriptDialogEventArgs> ScriptDialog 

	//	        {
	//	            add { synchronized (m_ScriptDialogLock) { m_ScriptDialog += value; } }
	//	            remove { synchronized (m_ScriptDialogLock) { m_ScriptDialog -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<ScriptQuestionEventArgs> m_ScriptQuestion;
	//	
	//	        /// <summary>Raises the ScriptQuestion event</summary>
	//	        /// <param name="e">A ScriptQuestionEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnScriptQuestion(ScriptQuestionEventArgs e)
	//	        {
	//	            EventHandler<ScriptQuestionEventArgs> handler = m_ScriptQuestion;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ScriptQuestionLock = new object();
	//	        /// <summary>Raised when an object requests a change in the permissions an agent has permitted</summary>
	//	        public event EventHandler<ScriptQuestionEventArgs> ScriptQuestion 

	//	        {
	//	            add { synchronized (m_ScriptQuestionLock) { m_ScriptQuestion += value; } }
	//	            remove { synchronized (m_ScriptQuestionLock) { m_ScriptQuestion -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<LoadUrlEventArgs> m_LoadURL;
	//	
	//	        /// <summary>Raises the LoadURL event</summary>
	//	        /// <param name="e">A LoadUrlEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnLoadURL(LoadUrlEventArgs e)
	//	        {
	//	            EventHandler<LoadUrlEventArgs> handler = m_LoadURL;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_LoadUrlLock = new object();
	//	        /// <summary>Raised when a script requests an agent open the specified URL</summary>
	//	        public event EventHandler<LoadUrlEventArgs> LoadURL 

	//	        {
	//	            add { synchronized (m_LoadUrlLock) { m_LoadURL += value; } }
	//	            remove { synchronized (m_LoadUrlLock) { m_LoadURL -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<BalanceEventArgs> m_Balance;
	//	
	//	        /// <summary>Raises the MoneyBalance event</summary>
	//	        /// <param name="e">A BalanceEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnBalance(BalanceEventArgs e)
	//	        {
	//	            EventHandler<BalanceEventArgs> handler = m_Balance;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_BalanceLock = new object();
	//	
	//	        /// <summary>Raised when an agents currency balance is updated</summary>
	//	        public event EventHandler<BalanceEventArgs> MoneyBalance 

	//	        {
	//	            add { synchronized (m_BalanceLock) { m_Balance += value; } }
	//	            remove { synchronized (m_BalanceLock) { m_Balance -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<MoneyBalanceReplyEventArgs> m_MoneyBalance;
	//	
	//	        /// <summary>Raises the MoneyBalanceReply event</summary>
	//	        /// <param name="e">A MoneyBalanceReplyEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnMoneyBalanceReply(MoneyBalanceReplyEventArgs e)
	//	        {
	//	            EventHandler<MoneyBalanceReplyEventArgs> handler = m_MoneyBalance;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_MoneyBalanceReplyLock = new object();
	//	
	//	        /// <summary>Raised when a transaction occurs involving currency such as a land purchase</summary>
	//	        public event EventHandler<MoneyBalanceReplyEventArgs> MoneyBalanceReply 

	//	        {
	//	            add { synchronized (m_MoneyBalanceReplyLock) { m_MoneyBalance += value; } }
	//	            remove { synchronized (m_MoneyBalanceReplyLock) { m_MoneyBalance -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<InstantMessageEventArgs> m_InstantMessage;
	//	
	//	        /// <summary>Raises the IM event</summary>
	//	        /// <param name="e">A InstantMessageEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnInstantMessage(InstantMessageEventArgs e)
	//	        {
	//	            EventHandler<InstantMessageEventArgs> handler = m_InstantMessage;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_InstantMessageLock = new object();
	//	        /// <summary>Raised when an ImprovedInstantMessage packet is recieved from the simulator, this is used for everything from
	//	        /// private messaging to friendship offers. The Dialog field defines what type of message has arrived</summary>
	//	        public event EventHandler<InstantMessageEventArgs> IM 

	//	        {
	//	            add { synchronized (m_InstantMessageLock) { m_InstantMessage += value; } }
	//	            remove { synchronized (m_InstantMessageLock) { m_InstantMessage -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<TeleportEventArgs> m_Teleport;
	//	
	//	        /// <summary>Raises the TeleportProgress event</summary>
	//	        /// <param name="e">A TeleportEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnTeleport(TeleportEventArgs e)
	//	        {
	//	            EventHandler<TeleportEventArgs> handler = m_Teleport;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_TeleportLock = new object();
	//	        /// <summary>Raised when an agent has requested a teleport to another location, or when responding to a lure. Raised multiple times
	//	        /// for each teleport indicating the progress of the request</summary>
	//	        public event EventHandler<TeleportEventArgs> TeleportProgress 

	//	        {
	//	            add { synchronized (m_TeleportLock) { m_Teleport += value; } }
	//	            remove { synchronized (m_TeleportLock) { m_Teleport += value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<AgentDataReplyEventArgs> m_AgentData;
	//	
	//	        /// <summary>Raises the AgentDataReply event</summary>
	//	        /// <param name="e">A AgentDataReplyEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnAgentData(AgentDataReplyEventArgs e)
	//	        {
	//	            EventHandler<AgentDataReplyEventArgs> handler = m_AgentData;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_AgentDataLock = new object();
	//	
	//	        /// <summary>Raised when a simulator sends agent specific information for our avatar.</summary>
	//	        public event EventHandler<AgentDataReplyEventArgs> AgentDataReply 

	//	        {
	//	            add { synchronized (m_AgentDataLock) { m_AgentData += value; } }
	//	            remove { synchronized (m_AgentDataLock) { m_AgentData -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<AnimationsChangedEventArgs> m_AnimationsChanged;
	//	
	//	        /// <summary>Raises the AnimationsChanged event</summary>
	//	        /// <param name="e">A AnimationsChangedEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnAnimationsChanged(AnimationsChangedEventArgs e)
	//	        {
	//	            EventHandler<AnimationsChangedEventArgs> handler = m_AnimationsChanged;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_AnimationsChangedLock = new object();
	//	
	//	        /// <summary>Raised when our agents animation playlist changes</summary>
	//	        public event EventHandler<AnimationsChangedEventArgs> AnimationsChanged 

	//	        {
	//	            add { synchronized (m_AnimationsChangedLock) { m_AnimationsChanged += value; } }
	//	            remove { synchronized (m_AnimationsChangedLock) { m_AnimationsChanged -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<MeanCollisionEventArgs> m_MeanCollision;
	//	
	//	        /// <summary>Raises the MeanCollision event</summary>
	//	        /// <param name="e">A MeanCollisionEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnMeanCollision(MeanCollisionEventArgs e)
	//	        {
	//	            EventHandler<MeanCollisionEventArgs> handler = m_MeanCollision;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_MeanCollisionLock = new object();
	//	
	//	        /// <summary>Raised when an object or avatar forcefully collides with our agent</summary>
	//	        public event EventHandler<MeanCollisionEventArgs> MeanCollision 

	//	        {
	//	            add { synchronized (m_MeanCollisionLock) { m_MeanCollision += value; } }
	//	            remove { synchronized (m_MeanCollisionLock) { m_MeanCollision -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<RegionCrossedEventArgs> m_RegionCrossed;
	//	
	//	        /// <summary>Raises the RegionCrossed event</summary>
	//	        /// <param name="e">A RegionCrossedEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnRegionCrossed(RegionCrossedEventArgs e)
	//	        {
	//	            EventHandler<RegionCrossedEventArgs> handler = m_RegionCrossed;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_RegionCrossedLock = new object();
	//	
	//	        /// <summary>Raised when our agent crosses a region border into another region</summary>
	//	        public event EventHandler<RegionCrossedEventArgs> RegionCrossed 

	//	        {
	//	            add { synchronized (m_RegionCrossedLock) { m_RegionCrossed += value; } }
	//	            remove { synchronized (m_RegionCrossedLock) { m_RegionCrossed -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<GroupChatJoinedEventArgs> m_GroupChatJoined;
	//	
	//	        /// <summary>Raises the GroupChatJoined event</summary>
	//	        /// <param name="e">A GroupChatJoinedEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnGroupChatJoined(GroupChatJoinedEventArgs e)
	//	        {
	//	            EventHandler<GroupChatJoinedEventArgs> handler = m_GroupChatJoined;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_GroupChatJoinedLock = new object();
	//	
	//	        /// <summary>Raised when our agent succeeds or fails to join a group chat session</summary>
	//	        public event EventHandler<GroupChatJoinedEventArgs> GroupChatJoined 

	//	        {
	//	            add { synchronized (m_GroupChatJoinedLock) { m_GroupChatJoined += value; } }
	//	            remove { synchronized (m_GroupChatJoinedLock) { m_GroupChatJoined -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<AlertMessageEventArgs> m_AlertMessage;
	//	
	//	        /// <summary>Raises the AlertMessage event</summary>
	//	        /// <param name="e">A AlertMessageEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnAlertMessage(AlertMessageEventArgs e)
	//	        {
	//	            EventHandler<AlertMessageEventArgs> handler = m_AlertMessage;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_AlertMessageLock = new object();
	//	
	//	        /// <summary>Raised when a simulator sends an urgent message usually indication the recent failure of
	//	        /// another action we have attempted to take such as an attempt to enter a parcel where we are denied access</summary>
	//	        public event EventHandler<AlertMessageEventArgs> AlertMessage 

	//	        {
	//	            add { synchronized (m_AlertMessageLock) { m_AlertMessage += value; } }
	//	            remove { synchronized (m_AlertMessageLock) { m_AlertMessage -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<ScriptControlEventArgs> m_ScriptControl;
	//	
	//	        /// <summary>Raises the ScriptControlChange event</summary>
	//	        /// <param name="e">A ScriptControlEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnScriptControlChange(ScriptControlEventArgs e)
	//	        {
	//	            EventHandler<ScriptControlEventArgs> handler = m_ScriptControl;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ScriptControlLock = new object();
	//	
	//	        /// <summary>Raised when a script attempts to take or release specified controls for our agent</summary>
	//	        public event EventHandler<ScriptControlEventArgs> ScriptControlChange 

	//	        {
	//	            add { synchronized (m_ScriptControlLock) { m_ScriptControl += value; } }
	//	            remove { synchronized (m_ScriptControlLock) { m_ScriptControl -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<CameraConstraintEventArgs> m_CameraConstraint;
	//	
	//	        /// <summary>Raises the CameraConstraint event</summary>
	//	        /// <param name="e">A CameraConstraintEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnCameraConstraint(CameraConstraintEventArgs e)
	//	        {
	//	            EventHandler<CameraConstraintEventArgs> handler = m_CameraConstraint;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_CameraConstraintLock = new object();
	//	
	//	        /// <summary>Raised when the simulator detects our agent is trying to view something
	//	        /// beyond its limits</summary>
	//	        public event EventHandler<CameraConstraintEventArgs> CameraConstraint 

	//	        {
	//	            add { synchronized (m_CameraConstraintLock) { m_CameraConstraint += value; } }
	//	            remove { synchronized (m_CameraConstraintLock) { m_CameraConstraint -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<ScriptSensorReplyEventArgs> m_ScriptSensorReply;
	//	
	//	        /// <summary>Raises the ScriptSensorReply event</summary>
	//	        /// <param name="e">A ScriptSensorReplyEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnScriptSensorReply(ScriptSensorReplyEventArgs e)
	//	        {
	//	            EventHandler<ScriptSensorReplyEventArgs> handler = m_ScriptSensorReply;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ScriptSensorReplyLock = new object();
	//	
	//	        /// <summary>Raised when a script sensor reply is received from a simulator</summary>
	//	        public event EventHandler<ScriptSensorReplyEventArgs> ScriptSensorReply 

	//	        {
	//	            add { synchronized (m_ScriptSensorReplyLock) { m_ScriptSensorReply += value; } }
	//	            remove { synchronized (m_ScriptSensorReplyLock) { m_ScriptSensorReply -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<AvatarSitResponseEventArgs> m_AvatarSitResponse;
	//	
	//	        /// <summary>Raises the AvatarSitResponse event</summary>
	//	        /// <param name="e">A AvatarSitResponseEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnAvatarSitResponse(AvatarSitResponseEventArgs e)
	//	        {
	//	            EventHandler<AvatarSitResponseEventArgs> handler = m_AvatarSitResponse;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_AvatarSitResponseLock = new object();
	//	
	//	        /// <summary>Raised in response to a <see cref="RequestSit"/> request</summary>
	//	        public event EventHandler<AvatarSitResponseEventArgs> AvatarSitResponse 
	//	        {
	//	            add { synchronized (m_AvatarSitResponseLock) { m_AvatarSitResponse += value; } }
	//	            remove { synchronized (m_AvatarSitResponseLock) { m_AvatarSitResponse -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<ChatSessionMemberAddedEventArgs> m_ChatSessionMemberAdded;
	//	
	//	        /// <summary>Raises the ChatSessionMemberAdded event</summary>
	//	        /// <param name="e">A ChatSessionMemberAddedEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnChatSessionMemberAdded(ChatSessionMemberAddedEventArgs e)
	//	        {
	//	            EventHandler<ChatSessionMemberAddedEventArgs> handler = m_ChatSessionMemberAdded;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ChatSessionMemberAddedLock = new object();
	//	
	//	        /// <summary>Raised when an avatar enters a group chat session we are participating in</summary>
	//	        public event EventHandler<ChatSessionMemberAddedEventArgs> ChatSessionMemberAdded 
	//	        {
	//	            add { synchronized (m_ChatSessionMemberAddedLock) { m_ChatSessionMemberAdded += value; } }
	//	            remove { synchronized (m_ChatSessionMemberAddedLock) { m_ChatSessionMemberAdded -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<ChatSessionMemberLeftEventArgs> m_ChatSessionMemberLeft;
	//	
	//	        /// <summary>Raises the ChatSessionMemberLeft event</summary>
	//	        /// <param name="e">A ChatSessionMemberLeftEventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnChatSessionMemberLeft(ChatSessionMemberLeftEventArgs e)
	//	        {
	//	            EventHandler<ChatSessionMemberLeftEventArgs> handler = m_ChatSessionMemberLeft;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ChatSessionMemberLeftLock = new object();
	//	
	//	        /// <summary>Raised when an agent exits a group chat session we are participating in</summary>
	//	        public event EventHandler<ChatSessionMemberLeftEventArgs> ChatSessionMemberLeft 
	//	        {
	//	            add { synchronized (m_ChatSessionMemberLeftLock) { m_ChatSessionMemberLeft += value; } }
	//	            remove { synchronized (m_ChatSessionMemberLeftLock) { m_ChatSessionMemberLeft -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<SetDisplayNameReplyEventArgs> m_SetDisplayNameReply;
	//	
	//	        ///<summary>Raises the SetDisplayNameReply Event</summary>
	//	        /// <param name="e">A SetDisplayNameReplyEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnSetDisplayNameReply(SetDisplayNameReplyEventArgs e)
	//	        {
	//	            EventHandler<SetDisplayNameReplyEventArgs> handler = m_SetDisplayNameReply;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_SetDisplayNameReplyLock = new object();
	//	
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// the details of display name change</summary>
	//	        public event EventHandler<SetDisplayNameReplyEventArgs> SetDisplayNameReply 
	//	        {
	//	            add { synchronized (m_SetDisplayNameReplyLock) { m_SetDisplayNameReply += value; } }
	//	            remove { synchronized (m_SetDisplayNameReplyLock) { m_SetDisplayNameReply -= value; } }
	//	        }
	//	
	//	        /// <summary>The event subscribers. null if no subcribers</summary>
	//	        private EventHandler<EventArgs> m_MuteListUpdated;
	//	
	//	        /// <summary>Raises the MuteListUpdated event</summary>
	//	        /// <param name="e">A EventArgs object containing the
	//	        /// data returned from the data server</param>
	//	        protected virtual void OnMuteListUpdated(EventArgs e)
	//	        {
	//	            EventHandler<EventArgs> handler = m_MuteListUpdated;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//	
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_MuteListUpdatedLock = new object();
	//	
	//	        /// <summary>Raised when a scripted object or agent within range sends a public message</summary>
	//	        public event EventHandler<EventArgs> MuteListUpdated 
	//	        {
	//	            add { synchronized (m_MuteListUpdatedLock) { m_MuteListUpdated += value; } }
	//	            remove { synchronized (m_MuteListUpdatedLock) { m_MuteListUpdated -= value; } }
	//	        }
	//	        //endregion Callbacks


	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();

	/// <summary>Reference to the GridClient instance</summary>
	private GridClient Client;
	/// <summary>Used for movement and camera tracking</summary>
	//readonly
	public AgentMovement Movement;
	/// <summary>Currently playing animations for the agent. Can be used to
	/// check the current movement status such as walking, hovering, aiming,
	/// etc. by checking against system animations found in the Animations class</summary>
	public InternalDictionary<UUID, Integer> SignaledAnimations = new InternalDictionary<UUID, Integer>();
	/// <summary>Dictionary containing current Group Chat sessions and members</summary>
	public InternalDictionary<UUID, List<ChatSessionMember>> GroupChatSessions = new InternalDictionary<UUID, List<ChatSessionMember>>();
	/// <summary>Dictionary containing mute list keyead on mute name and key</summary>
	public InternalDictionary<String, MuteEntry> MuteList = new InternalDictionary<String, MuteEntry>();

	//region Properties

	/// <summary>Your (client) avatars <see cref="UUID"/></summary>
	/// <remarks>"client", "agent", and "avatar" all represent the same thing</remarks>
	public UUID getAgentID() {return id;}
	/// <summary>Temporary <seealso cref="UUID"/> assigned to this session, used for 
	/// verifying our identity in packets</summary>
	public UUID getSessionID() {return sessionID;}
	/// <summary>Shared secret <seealso cref="UUID"/> that is never sent over the wire</summary>
	public UUID getSecureSessionID() {return secureSessionID;}
	/// <summary>Your (client) avatar ID, local to the current region/sim</summary>
	public long getLocalID() {return localID;}
	public void setLocalID(long value) {localID = value;}
	/// <summary>Where the avatar started at login. Can be "last", "home" 
	/// or a login <seealso cref="T:OpenMetaverse.URI"/></summary>
	public String getStartLocation() {return startLocation;}
	/// <summary>The access level of this agent, usually M or PG</summary>
	public String getAgentAccess() {return agentAccess;}
	/// <summary>The CollisionPlane of Agent</summary>
	public Vector4 getCollisionPlane() {return collisionPlane;}
	public void setCollisionPlane(Vector4 value) {collisionPlane = value;}

	/// <summary>An <seealso cref="Vector3"/> representing the velocity of our agent</summary>
	public Vector3 getVelocity() {return velocity;}
	public void setVelocity(Vector3 value) {velocity = value;}
	/// <summary>An <seealso cref="Vector3"/> representing the acceleration of our agent</summary>
	public Vector3 getAcceleration() {return acceleration;}
	public void setAcceleration(Vector3 value) {acceleration = value;}

	/// <summary>A <seealso cref="Vector3"/> which specifies the angular speed, and axis about which an Avatar is rotating.</summary>
	public Vector3 getAngularVelocity() {return angularVelocity;}
	public void setAngularVelocity(Vector3 value) {angularVelocity = value;}
	/// <summary>Position avatar client will goto when login to 'home' or during
	/// teleport request to 'home' region.</summary>
	public Vector3 getHomePosition() {return homePosition;}
	/// <summary>LookAt point saved/restored with HomePosition</summary>
	public Vector3 getHomeLookAt() {return homeLookAt;}
	/// <summary>Avatar First Name (i.e. Philip)</summary>
	public String getFirstName() {return firstName;}
	/// <summary>Avatar Last Name (i.e. Linden)</summary>
	public String getLastName() {return lastName;}
	/// <summary>Avatar Full Name (i.e. Philip Linden)</summary>
	public String getName()
	{
		// This is a fairly common request, so assume the name doesn't
		// change mid-session and cache the result
		if (fullName == null || fullName.length() < 2)
			fullName = String.format("%s %s", firstName, lastName);
		return fullName;
	}
	/// <summary>Gets the health of the agent</summary>
	public float getHealth() {return health;}
	/// <summary>Gets the current balance of the agent</summary>
	public int getBalance() {return balance;}
	/// <summary>Gets the local ID of the prim the agent is sitting on,
	/// zero if the avatar is not currently sitting</summary>
	public long getSittingOn() {return sittingOn;}
	public void setSittingOn(long value) {sittingOn = value;}
	/// <summary>Gets the <seealso cref="UUID"/> of the agents active group.</summary>
	public UUID getActiveGroup() {return activeGroup;}
	/// <summary>Gets the Agents powers in the currently active group</summary>
	public EnumSet<GroupPowers> getActiveGroupPowers() {return activeGroupPowers;}
	/// <summary>Current status message for teleporting</summary>
	public String getTeleportMessage() {return teleportMessage;}
	/// <summary>Current position of the agent as a relative offset from
	/// the simulator, or the parent object if we are sitting on something</summary>
	public Vector3 getRelativePosition() {return relativePosition; } 

	public void setRelativePosition(Vector3 value) 
	{ relativePosition = value;}
	/// <summary>Current rotation of the agent as a relative rotation from
	/// the simulator, or the parent object if we are sitting on something</summary>
	public Quaternion getRelativeRotation() {return relativeRotation; }
	public void setRelativeRotation(Quaternion value) {relativeRotation = value; } 

	public void getRelativeRotation(Quaternion value)	        
	{ relativeRotation = value;}
	/// <summary>Current position of the agent in the simulator</summary>
	public Vector3 getSimPosition()
	{
		// simple case, agent not seated
		if (sittingOn == 0)
		{
			return relativePosition;
		}

		// a bit more complicatated, agent sitting on a prim
		Primitive p = null;
		Vector3 fullPosition = relativePosition;

		if ((( p = Client.network.getCurrentSim().ObjectsPrimitives.get(sittingOn)) != null))
		{
			fullPosition = Vector3.add(p.Position,  Vector3.multiply(relativePosition,  p.Rotation));
		}

		// go up the hiearchy trying to find the root prim
		while (p != null && p.ParentID != 0)
		{
			Avatar av;
			if (((av = Client.network.getCurrentSim().ObjectsAvatars.get(p.ParentID)) != null))
			{
				p = av;
				fullPosition = Vector3.add(fullPosition, p.Position);
			}
			else
			{
				if (((p = Client.network.getCurrentSim().ObjectsPrimitives.get(p.ParentID)) !=null))
				{
					fullPosition = Vector3.add(fullPosition, p.Position);
				}
			}
		}

		if (p != null) // we found the root prim
		{
			return fullPosition;
		}

		// Didn't find the seat's root prim, try returning coarse loaction
		if (((fullPosition = Client.network.getCurrentSim().avatarPositions.get(getAgentID()))!=null))
		{
			return fullPosition;
		}

		JLogger.warn("Failed to determine agents sim position");
		return relativePosition;
	}

	/// <summary>
	/// A <seealso cref="Quaternion"/> representing the agents current rotation
	/// </summary>
	public Quaternion getSimRotation()
	{
		if (sittingOn != 0)
		{
			Primitive parent;
			if (Client.network.getCurrentSim() != null && ((parent = Client.network.getCurrentSim().ObjectsPrimitives.get(sittingOn))!=null))
			{
				return Quaternion.multiply(relativeRotation,  parent.Rotation);
			}
			else
			{
				JLogger.warn("Currently sitting on object " + sittingOn + " which is not tracked, SimRotation will be inaccurate");
				return relativeRotation;
			}
		}
		else
		{
			return relativeRotation;
		}
	}

	/// <summary>Returns the global grid position of the avatar</summary>
	public Vector3d getGlobalPosition()
	{
		if (Client.network.getCurrentSim() != null)
		{
			//uint
			long globalX, globalY;
			long[] xy = new long[2]; 
			Utils.longToUInts(Client.network.getCurrentSim().Handle.longValue(), xy);
			globalX = xy[0];
			globalY = xy[1];
			//	                    Utils.LongToUInts(Client.network.getCurrentSim().Handle, out globalX, out globalY);
			Vector3 pos = getSimPosition();

			return new Vector3d(
					(double)globalX + (double)pos.X,
					(double)globalY + (double)pos.Y,
					(double)pos.Z);
		}
		else
			return Vector3d.Zero;
	}

	//endregion Properties

	//uint
	private long localID;
	private Vector3 relativePosition;
	private Quaternion relativeRotation = Quaternion.Identity;
	private Vector4 collisionPlane;
	private Vector3 velocity;
	private Vector3 acceleration;
	private Vector3 angularVelocity;
	//uint
	private long sittingOn;
	public long lastInterpolation;
	//region Private Members

	private UUID id;
	private UUID sessionID;
	private UUID secureSessionID;
	private String startLocation = "";
	private String agentAccess = "";
	private Vector3 homePosition;
	private Vector3 homeLookAt;
	private String firstName = "";
	private String lastName = "";
	private String fullName;
	private String teleportMessage = "";
	private TeleportStatus teleportStat = TeleportStatus.None;
	private ManualResetEvent teleportEvent = new ManualResetEvent(false);
	//uint
	private long heightWidthGenCounter;
	private float health;
	private int balance;
	private UUID activeGroup;
	private EnumSet<GroupPowers> activeGroupPowers;
	private Map<UUID, AssetGesture> gestureCache = new HashMap<UUID, AssetGesture>();
	//endregion Private Members

	/// <summary>
	/// Constructor, setup callbacks for packets related to our avatar
	/// </summary>
	/// <param name="client">A reference to the <seealso cref="T:OpenMetaverse.GridClient"/> Class</param>
	public AgentManager(GridClient client)
	{
		Client = client;
		Movement = new AgentMovement(Client);

		//		            Client.network.Disconnected += Network_OnDisconnected;
		Client.network.RegisterOnDisconnectedCallback(new EventObserver<DisconnectedEventArgs>(){
			@Override
			public void handleEvent(Observable o, DisconnectedEventArgs arg) {
				Network_OnDisconnected(o, arg);
			}
		});

		//		            // Teleport callbacks            
		//		            // Client.network.RegisterCallback(PacketType.TeleportStart, TeleportHandler);

		Client.network.RegisterCallback(PacketType.TeleportStart, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ TeleportHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterCallback(PacketType.TeleportProgress, TeleportHandler);

		Client.network.RegisterCallback(PacketType.TeleportProgress, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ TeleportHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterCallback(PacketType.TeleportFailed, TeleportHandler);

		Client.network.RegisterCallback(PacketType.TeleportFailed, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ TeleportHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterCallback(PacketType.TeleportCancel, TeleportHandler);

		Client.network.RegisterCallback(PacketType.TeleportCancel, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ TeleportHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterCallback(PacketType.TeleportLocal, TeleportHandler);

		Client.network.RegisterCallback(PacketType.TeleportLocal, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ TeleportHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // these come in via the EventQueue
		//		            // Client.network.RegisterEventCallback("TeleportFailed", new Caps.EventQueueCallback(TeleportFailedEventHandler);

		Client.network.RegisterEventCallback("TeleportFailed", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ TeleportFailedEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterEventCallback("TeleportFinish", new Caps.EventQueueCallback(TeleportFinishEventHandler);

		Client.network.RegisterEventCallback("TeleportFinish", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ TeleportFinishEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		
		//		            // Instant message callback
		//		            // Client.network.RegisterCallback(PacketType.ImprovedInstantMessage, InstantMessageHandler);

		Client.network.RegisterCallback(PacketType.ImprovedInstantMessage, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ InstantMessageHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Chat callback
		//		            // Client.network.RegisterCallback(PacketType.ChatFromSimulator, ChatHandler);

		Client.network.RegisterCallback(PacketType.ChatFromSimulator, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ChatHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Script dialog callback
		//		            // Client.network.RegisterCallback(PacketType.ScriptDialog, ScriptDialogHandler);

		Client.network.RegisterCallback(PacketType.ScriptDialog, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ScriptDialogHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Script question callback
		//		            // Client.network.RegisterCallback(PacketType.ScriptQuestion, ScriptQuestionHandler);

		Client.network.RegisterCallback(PacketType.ScriptQuestion, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ScriptQuestionHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Script URL callback
		//		            // Client.network.RegisterCallback(PacketType.LoadURL, LoadURLHandler);

		Client.network.RegisterCallback(PacketType.LoadURL, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ LoadURLHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Movement complete callback
		//		            // Client.network.RegisterCallback(PacketType.AgentMovementComplete, MovementCompleteHandler);

		Client.network.RegisterCallback(PacketType.AgentMovementComplete, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ MovementCompleteHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Health callback
		//		            // Client.network.RegisterCallback(PacketType.HealthMessage, HealthHandler);

		Client.network.RegisterCallback(PacketType.HealthMessage, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ HealthHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Money callback
		//		            // Client.network.RegisterCallback(PacketType.MoneyBalanceReply, MoneyBalanceReplyHandler);

		Client.network.RegisterCallback(PacketType.MoneyBalanceReply, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ MoneyBalanceReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            //Agent update callback
		//		            // Client.network.RegisterCallback(PacketType.AgentDataUpdate, AgentDataUpdateHandler);

		Client.network.RegisterCallback(PacketType.AgentDataUpdate, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ AgentDataUpdateHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Animation callback
		//		            // Client.network.RegisterCallback(PacketType.AvatarAnimation, AvatarAnimationHandler, false);

		Client.network.RegisterCallback(PacketType.AvatarAnimation, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ AvatarAnimationHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
		, false);
		//		            // Object colliding into our agent callback
		//		            // Client.network.RegisterCallback(PacketType.MeanCollisionAlert, MeanCollisionAlertHandler);

		Client.network.RegisterCallback(PacketType.MeanCollisionAlert, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ MeanCollisionAlertHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Region Crossing
		//		            // Client.network.RegisterCallback(PacketType.CrossedRegion, CrossedRegionHandler);

		Client.network.RegisterCallback(PacketType.CrossedRegion, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ CrossedRegionHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterEventCallback("CrossedRegion", new Caps.EventQueueCallback(CrossedRegionEventHandler);

		Client.network.RegisterEventCallback("CrossedRegion", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ CrossedRegionEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // CAPS callbacks
		//		            // Client.network.RegisterEventCallback("EstablishAgentCommunication", new Caps.EventQueueCallback(EstablishAgentCommunicationEventHandler);

		Client.network.RegisterEventCallback("EstablishAgentCommunication", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ EstablishAgentCommunicationEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterEventCallback("SetDisplayNameReply", new Caps.EventQueueCallback(SetDisplayNameReplyEventHandler);

		Client.network.RegisterEventCallback("SetDisplayNameReply", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ SetDisplayNameReplyEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Incoming Group Chat
		//		            // Client.network.RegisterEventCallback("ChatterBoxInvitation", new Caps.EventQueueCallback(ChatterBoxInvitationEventHandler);

		Client.network.RegisterEventCallback("ChatterBoxInvitation", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ ChatterBoxInvitationEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Outgoing Group Chat Reply
		//		            // Client.network.RegisterEventCallback("ChatterBoxSessionEventReply", new Caps.EventQueueCallback(ChatterBoxSessionEventReplyEventHandler);

		Client.network.RegisterEventCallback("ChatterBoxSessionEventReply", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ ChatterBoxSessionEventReplyEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterEventCallback("ChatterBoxSessionStartReply", new Caps.EventQueueCallback(ChatterBoxSessionStartReplyEventHandler);

		Client.network.RegisterEventCallback("ChatterBoxSessionStartReply", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ ChatterBoxSessionStartReplyEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//		            // Client.network.RegisterEventCallback("ChatterBoxSessionAgentListUpdates", new Caps.EventQueueCallback(ChatterBoxSessionAgentListUpdatesEventHandler);


		Client.network.RegisterEventCallback("ChatterBoxSessionAgentListUpdates", new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,CapsEventObservableArg arg) {
				try{ ChatterBoxSessionAgentListUpdatesEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		// Login
		Client.network.RegisterLoginResponseCallback(new EventObserver<LoginResponseCallbackArg>()
				{
			public void handleEvent(Observable arg0, LoginResponseCallbackArg arg1) {
				LoginResponseCallbackArg obj = (LoginResponseCallbackArg)arg1;
				Network_OnLoginResponse(obj.isLoginSuccess(), obj.isRedirect(), 
						obj.getMessage(), obj.getReason(), obj.getReplyData());
			}	
				});

		//            // Alert Messages
		//            // Client.network.RegisterCallback(PacketType.AlertMessage, AlertMessageHandler);

		Client.network.RegisterCallback(PacketType.AlertMessage, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ AlertMessageHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//            // script control change messages, ie: when an in-world LSL script wants to take control of your agent.
		//            // Client.network.RegisterCallback(PacketType.ScriptControlChange, ScriptControlChangeHandler);

		Client.network.RegisterCallback(PacketType.ScriptControlChange, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ScriptControlChangeHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//            // Camera Constraint (probably needs to move to AgentManagerCamera TODO:
		//            // Client.network.RegisterCallback(PacketType.CameraConstraint, CameraConstraintHandler);

		Client.network.RegisterCallback(PacketType.CameraConstraint, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ CameraConstraintHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//            // Client.network.RegisterCallback(PacketType.ScriptSensorReply, ScriptSensorReplyHandler);

		Client.network.RegisterCallback(PacketType.ScriptSensorReply, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ScriptSensorReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//            // Client.network.RegisterCallback(PacketType.AvatarSitResponse, AvatarSitResponseHandler);

		Client.network.RegisterCallback(PacketType.AvatarSitResponse, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ AvatarSitResponseHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//            // Process mute list update message
		//            // Client.network.RegisterCallback(PacketType.MuteListUpdate, MuteListUpdateHander);

		Client.network.RegisterCallback(PacketType.MuteListUpdate, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ MuteListUpdateHander(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
	}

	//region Chat and instant messages

	/// <summary>
	/// Send a text message from the Agent to the Simulator
	/// </summary>
	/// <param name="message">A <see cref="string"/> containing the message</param>
	/// <param name="channel">The channel to send the message on, 0 is the public channel. Channels above 0
	/// can be used however only scripts listening on the specified channel will see the message</param>
	/// <param name="type">Denotes the type of message being sent, shout, whisper, etc.</param>
	public void Chat(String message, int channel, ChatType type)
	{
		ChatFromViewerPacket chat = new ChatFromViewerPacket();
		chat.AgentData.AgentID = this.id;
		chat.AgentData.SessionID = Client.self.getSessionID();
		chat.ChatData.Channel = channel;
		chat.ChatData.Message = Utils.stringToBytesWithTrailingNullByte(message);
		chat.ChatData.Type = (byte)type.getIndex();

		Client.network.SendPacket(chat);
	}

	/// <summary>
	/// Request any instant messages sent while the client was offline to be resent.
	/// </summary>
	public void RetrieveInstantMessages()
	{
		RetrieveInstantMessagesPacket p = new RetrieveInstantMessagesPacket();
		p.AgentData.AgentID = Client.self.getAgentID();
		p.AgentData.SessionID = Client.self.getSessionID();
		Client.network.SendPacket(p);
	}

	/// <summary>
	/// Send an Instant Message to another Avatar
	/// </summary>
	/// <param name="target">The recipients <see cref="UUID"/></param>
	/// <param name="message">A <see cref="string"/> containing the message to send</param>
	public void InstantMessage(UUID target, String message)
	{
		InstantMessage(getName(), target, message, getAgentID().equals(target) ? getAgentID() : UUID.xor(target, getAgentID()),
				InstantMessageDialog.MessageFromAgent, InstantMessageOnline.Offline, this.getSimPosition(),
				UUID.Zero, Utils.EmptyBytes);
	}

	/// <summary>
	/// Send an Instant Message to an existing group chat or conference chat
	/// </summary>
	/// <param name="target">The recipients <see cref="UUID"/></param>
	/// <param name="message">A <see cref="string"/> containing the message to send</param>
	/// <param name="imSessionID">IM session ID (to differentiate between IM windows)</param>
	public void InstantMessage(UUID target, String message, UUID imSessionID)
	{
		InstantMessage(getName(), target, message, imSessionID,
				InstantMessageDialog.MessageFromAgent, InstantMessageOnline.Offline, this.getSimPosition(),
				UUID.Zero, Utils.EmptyBytes);
	}

	/// <summary>
	/// Send an Instant Message
	/// </summary>
	/// <param name="fromName">The name this IM will show up as being from</param>
	/// <param name="target">Key of Avatar</param>
	/// <param name="message">Text message being sent</param>
	/// <param name="imSessionID">IM session ID (to differentiate between IM windows)</param>
	/// <param name="conferenceIDs">IDs of sessions for a conference</param>
	public void InstantMessage(String fromName, UUID target, String message, UUID imSessionID,
			UUID[] conferenceIDs)
	{
		byte[] binaryBucket;

		if (conferenceIDs != null && conferenceIDs.length > 0)
		{
			binaryBucket = new byte[16 * conferenceIDs.length];
			for (int i = 0; i < conferenceIDs.length; ++i)
				Utils.arraycopy(conferenceIDs[i].GetBytes(), 0, binaryBucket, i * 16, 16);
		}
		else
		{
			binaryBucket = Utils.EmptyBytes;
		}

		InstantMessage(fromName, target, message, imSessionID, InstantMessageDialog.MessageFromAgent,
				InstantMessageOnline.Offline, Vector3.Zero, UUID.Zero, binaryBucket);
	}

	/// <summary>
	/// Send an Instant Message
	/// </summary>
	/// <param name="fromName">The name this IM will show up as being from</param>
	/// <param name="target">Key of Avatar</param>
	/// <param name="message">Text message being sent</param>
	/// <param name="imSessionID">IM session ID (to differentiate between IM windows)</param>
	/// <param name="dialog">Type of instant message to send</param>
	/// <param name="offline">Whether to IM offline avatars as well</param>
	/// <param name="position">Senders Position</param>
	/// <param name="regionID">RegionID Sender is In</param>
	/// <param name="binaryBucket">Packed binary data that is specific to
	/// the dialog type</param>
	public void InstantMessage(String fromName, UUID target, String message, UUID imSessionID,
			InstantMessageDialog dialog, InstantMessageOnline offline, Vector3 position, UUID regionID,
			byte[] binaryBucket)
	{
		if (target != UUID.Zero)
		{
			ImprovedInstantMessagePacket im = new ImprovedInstantMessagePacket();

			if (imSessionID.equals(UUID.Zero) || imSessionID.equals(getAgentID()))
				imSessionID = getAgentID().equals(target) ? getAgentID() : UUID.xor(target , getAgentID());

				im.AgentData.AgentID = Client.self.getAgentID();
				im.AgentData.SessionID = Client.self.getSessionID();

				im.MessageBlock.Dialog = (byte)dialog.getIndex();
				im.MessageBlock.FromAgentName = Utils.stringToBytesWithTrailingNullByte(fromName);
				im.MessageBlock.FromGroup = false;
				im.MessageBlock.ID = imSessionID;
				im.MessageBlock.Message = Utils.stringToBytesWithTrailingNullByte(message);
				im.MessageBlock.Offline = (byte)offline.getIndex();
				im.MessageBlock.ToAgentID = target;

				if (binaryBucket != null)
					im.MessageBlock.BinaryBucket = binaryBucket;
				else
					im.MessageBlock.BinaryBucket = Utils.EmptyBytes;

				// These fields are mandatory, even if we don't have valid values for them
				im.MessageBlock.Position = Vector3.Zero;
				//TODO: Allow region id to be correctly set by caller or fetched from Client.*
				im.MessageBlock.RegionID = regionID;

				// Send the message
				Client.network.SendPacket(im);
		}
		else
		{
			JLogger.error(String.format("Suppressing instant message \"%s\" to UUID.Zero", message));
		}
	}

	/// <summary>
	/// Send an Instant Message to a group
	/// </summary>
	/// <param name="groupID"><seealso cref="UUID"/> of the group to send message to</param>
	/// <param name="message">Text Message being sent.</param>
	public void InstantMessageGroup(UUID groupID, String message)
	{
		InstantMessageGroup(getName(), groupID, message);
	}

	/// <summary>
	/// Send an Instant Message to a group the agent is a member of
	/// </summary>
	/// <param name="fromName">The name this IM will show up as being from</param>
	/// <param name="groupID"><seealso cref="UUID"/> of the group to send message to</param>
	/// <param name="message">Text message being sent</param>
	public void InstantMessageGroup(String fromName, UUID groupID, String message)
	{
		synchronized (GroupChatSessions.getDictionary())
		{
			if (GroupChatSessions.containsKey(groupID))
			{
				ImprovedInstantMessagePacket im = new ImprovedInstantMessagePacket();

				im.AgentData.AgentID = Client.self.getAgentID();
				im.AgentData.SessionID = Client.self.getSessionID();
				im.MessageBlock.Dialog = (byte)InstantMessageDialog.SessionSend.getIndex();
				im.MessageBlock.FromAgentName = Utils.stringToBytesWithTrailingNullByte(fromName);
				im.MessageBlock.FromGroup = false;
				im.MessageBlock.Message = Utils.stringToBytesWithTrailingNullByte(message);
				im.MessageBlock.Offline = 0;
				im.MessageBlock.ID = groupID;
				im.MessageBlock.ToAgentID = groupID;
				im.MessageBlock.Position = Vector3.Zero;
				im.MessageBlock.RegionID = UUID.Zero;
				im.MessageBlock.BinaryBucket = Utils.stringToBytesWithTrailingNullByte("\0");

				Client.network.SendPacket(im);
			}
			else
			{
				JLogger.error("No Active group chat session appears to exist, use RequestJoinGroupChat() to join one");
			}
		}
	}

	/// <summary>
	/// Send a request to join a group chat session
	/// </summary>
	/// <param name="groupID"><seealso cref="UUID"/> of Group to leave</param>
	public void RequestJoinGroupChat(UUID groupID)
	{
		ImprovedInstantMessagePacket im = new ImprovedInstantMessagePacket();

		im.AgentData.AgentID = Client.self.getAgentID();
		im.AgentData.SessionID = Client.self.getSessionID();
		im.MessageBlock.Dialog = (byte)InstantMessageDialog.SessionGroupStart.getIndex();
		im.MessageBlock.FromAgentName = Utils.stringToBytesWithTrailingNullByte(Client.self.getName());
		im.MessageBlock.FromGroup = false;
		im.MessageBlock.Message = Utils.EmptyBytes;
		im.MessageBlock.ParentEstateID = 0;
		im.MessageBlock.Offline = 0;
		im.MessageBlock.ID = groupID;
		im.MessageBlock.ToAgentID = groupID;
		im.MessageBlock.BinaryBucket = Utils.EmptyBytes;
		im.MessageBlock.Position = Client.self.getSimPosition();
		im.MessageBlock.RegionID = UUID.Zero;

		Client.network.SendPacket(im);
	}

	/// <summary>
	/// Exit a group chat session. This will stop further Group chat messages
	/// from being sent until session is rejoined.
	/// </summary>
	/// <param name="groupID"><seealso cref="UUID"/> of Group chat session to leave</param>
	public void RequestLeaveGroupChat(UUID groupID)
	{
		ImprovedInstantMessagePacket im = new ImprovedInstantMessagePacket();

		im.AgentData.AgentID = Client.self.getAgentID();
		im.AgentData.SessionID = Client.self.getSessionID();
		im.MessageBlock.Dialog = (byte)InstantMessageDialog.SessionDrop.getIndex();
		im.MessageBlock.FromAgentName = Utils.stringToBytesWithTrailingNullByte(Client.self.getName());
		im.MessageBlock.FromGroup = false;
		im.MessageBlock.Message = Utils.EmptyBytes;
		im.MessageBlock.Offline = 0;
		im.MessageBlock.ID = groupID;
		im.MessageBlock.ToAgentID = groupID;
		im.MessageBlock.BinaryBucket = Utils.EmptyBytes;
		im.MessageBlock.Position = Vector3.Zero;
		im.MessageBlock.RegionID = UUID.Zero;

		Client.network.SendPacket(im);

		synchronized (GroupChatSessions.getDictionary())
		{
			if (GroupChatSessions.containsKey(groupID))
				GroupChatSessions.remove(groupID);
		}
	}

	/// <summary>
	/// Reply to script dialog questions. 
	/// </summary>
	/// <param name="channel">Channel initial request came on</param>
	/// <param name="buttonIndex">Index of button you're "clicking"</param>
	/// <param name="buttonlabel">Label of button you're "clicking"</param>
	/// <param name="objectID"><seealso cref="UUID"/> of Object that sent the dialog request</param>
	/// <seealso cref="OnScriptDialog"/>
	public void ReplyToScriptDialog(int channel, int buttonIndex, String buttonlabel, UUID objectID)
	{
		ScriptDialogReplyPacket reply = new ScriptDialogReplyPacket();

		reply.AgentData.AgentID = Client.self.getAgentID();
		reply.AgentData.SessionID = Client.self.getSessionID();

		reply.Data.ButtonIndex = buttonIndex;
		reply.Data.ButtonLabel = Utils.stringToBytesWithTrailingNullByte(buttonlabel);
		reply.Data.ChatChannel = channel;
		reply.Data.ObjectID = objectID;

		Client.network.SendPacket(reply);
	}

	/// <summary>
	/// Accept invite for to a chatterbox session
	/// </summary>
	/// <param name="session_id"><seealso cref="UUID"/> of session to accept invite to</param>
	public void ChatterBoxAcceptInvite(UUID session_id) throws Exception
	{
		if (Client.network.getCurrentSim() == null || Client.network.getCurrentSim().Caps == null)
			throw new Exception("ChatSessionRequest capability is not currently available");

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("ChatSessionRequest");

		if (url != null)
		{
			ChatSessionAcceptInvitation acceptInvite = new ChatSessionAcceptInvitation();
			acceptInvite.SessionID = session_id;

			CapsHttpClient request = new CapsHttpClient(url);
			request.BeginGetResponse(acceptInvite.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);

			synchronized (GroupChatSessions.getDictionary())
			{
				if (!GroupChatSessions.containsKey(session_id))
					GroupChatSessions.add(session_id, new ArrayList<ChatSessionMember>());
			}
		}
		else
		{
			throw new Exception("ChatSessionRequest capability is not currently available");
		}

	}

	/// <summary>
	/// Start a friends conference
	/// </summary>
	/// <param name="participants"><seealso cref="UUID"/> List of UUIDs to start a conference with</param>
	/// <param name="tmp_session_id">the temportary session ID returned in the <see cref="OnJoinedGroupChat"/> callback></param>
	public void StartIMConference(List<UUID> participants, UUID tmp_session_id) throws Exception
	{
		if (Client.network.getCurrentSim() == null || Client.network.getCurrentSim().Caps == null)
			throw new Exception("ChatSessionRequest capability is not currently available");

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("ChatSessionRequest");

		if (url != null)
		{
			ChatSessionRequestStartConference startConference = new ChatSessionRequestStartConference();

			startConference.AgentsBlock = new UUID[participants.size()];
			for (int i = 0; i < participants.size(); i++)
				startConference.AgentsBlock[i] = participants.get(i);

			startConference.SessionID = tmp_session_id;

			CapsHttpClient request = new CapsHttpClient(url);
			request.BeginGetResponse(startConference.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			throw new Exception("ChatSessionRequest capability is not currently available");
		}
	}

	//endregion Chat and instant messages

	//region Viewer Effects

	/// <summary>
	/// Start a particle stream between an agent and an object
	/// </summary>
	/// <param name="sourceAvatar"><seealso cref="UUID"/> Key of the source agent</param>
	/// <param name="targetObject"><seealso cref="UUID"/> Key of the target object</param>
	/// <param name="globalOffset"></param>
	/// <param name="type">The type from the <seealso cref="T:PointAtType"/> enum</param>
	/// <param name="effectID">A unique <seealso cref="UUID"/> for this effect</param>
	public void PointAtEffect(UUID sourceAvatar, UUID targetObject, Vector3d globalOffset, PointAtType type,
			UUID effectID)
	{
		ViewerEffectPacket effect = new ViewerEffectPacket();

		effect.AgentData.AgentID = Client.self.getAgentID();
		effect.AgentData.SessionID = Client.self.getSessionID();

		effect.Effect = new ViewerEffectPacket.EffectBlock[1];
		effect.Effect[0] = new ViewerEffectPacket.EffectBlock();
		effect.Effect[0].AgentID = Client.self.getAgentID();
		effect.Effect[0].Color = new byte[4];
		//TODO Verify following
		//	            effect.Effect[0].Duration = (type == PointAtType.Clear) ? 0.0f : Single.MaxValue / 4.0f;
		effect.Effect[0].Duration = (type == PointAtType.Clear) ? 0.0f : Float.MAX_VALUE / 4.0f;

		effect.Effect[0].ID = effectID;
		effect.Effect[0].Type = (byte)EffectType.PointAt.getIndex();

		byte[] typeData = new byte[57];
		if (sourceAvatar != UUID.Zero)
			Utils.arraycopy(sourceAvatar.GetBytes(), 0, typeData, 0, 16);
		if (targetObject != UUID.Zero)
			Utils.arraycopy(targetObject.GetBytes(), 0, typeData, 16, 16);
		Utils.arraycopy(globalOffset.getBytesLit(), 0, typeData, 32, 24);
		typeData[56] = (byte)type.getIndex();

		effect.Effect[0].TypeData = typeData;

		Client.network.SendPacket(effect);
	}

	/// <summary>
	/// Start a particle stream between an agent and an object
	/// </summary>
	/// <param name="sourceAvatar"><seealso cref="UUID"/> Key of the source agent</param>
	/// <param name="targetObject"><seealso cref="UUID"/> Key of the target object</param>
	/// <param name="globalOffset">A <seealso cref="Vector3d"/> representing the beams offset from the source</param>
	/// <param name="type">A <seealso cref="T:PointAtType"/> which sets the avatars lookat animation</param>
	/// <param name="effectID"><seealso cref="UUID"/> of the Effect</param>
	public void LookAtEffect(UUID sourceAvatar, UUID targetObject, Vector3d globalOffset, LookAtType type,
			UUID effectID)
	{
		ViewerEffectPacket effect = new ViewerEffectPacket();

		effect.AgentData.AgentID = Client.self.getAgentID();
		effect.AgentData.SessionID = Client.self.getSessionID();

		float duration;

		switch (type)
		{
		case Clear:
			duration = 2.0f;
			break;
		case Hover:
			duration = 1.0f;
			break;
		case FreeLook:
			duration = 2.0f;
			break;
		case Idle:
			duration = 3.0f;
			break;
		case AutoListen:
		case Respond:
			duration = 4.0f;
			break;
		case None:
		case Select:
		case Focus:
		case Mouselook:
			//TODO verify following
			//	                    duration = Single.MaxValue / 2.0f;
			duration = Float.MAX_VALUE / 2.0f;

			break;
		default:
			duration = 0.0f;
			break;
		}

		effect.Effect = new ViewerEffectPacket.EffectBlock[1];
		effect.Effect[0] = new ViewerEffectPacket.EffectBlock();
		effect.Effect[0].AgentID = Client.self.getAgentID();
		effect.Effect[0].Color = new byte[4];
		effect.Effect[0].Duration = duration;
		effect.Effect[0].ID = effectID;
		effect.Effect[0].Type = (byte)EffectType.LookAt.getIndex();

		byte[] typeData = new byte[57];
		Utils.arraycopy(sourceAvatar.GetBytes(), 0, typeData, 0, 16);
		Utils.arraycopy(targetObject.GetBytes(), 0, typeData, 16, 16);
		Utils.arraycopy(globalOffset.getBytesLit(), 0, typeData, 32, 24);
		typeData[56] = (byte)type.getIndex();

		effect.Effect[0].TypeData = typeData;

		Client.network.SendPacket(effect);
	}

	/// <summary>
	/// Create a particle beam between an avatar and an primitive 
	/// </summary>
	/// <param name="sourceAvatar">The ID of source avatar</param>
	/// <param name="targetObject">The ID of the target primitive</param>
	/// <param name="globalOffset">global offset</param>
	/// <param name="color">A <see cref="Color4"/> object containing the combined red, green, blue and alpha 
	/// color values of particle beam</param>
	/// <param name="duration">a float representing the duration the parcicle beam will last</param>
	/// <param name="effectID">A Unique ID for the beam</param>
	/// <seealso cref="ViewerEffectPacket"/>
	public void BeamEffect(UUID sourceAvatar, UUID targetObject, Vector3d globalOffset, Color4 color,
			float duration, UUID effectID)
	{
		ViewerEffectPacket effect = new ViewerEffectPacket();

		effect.AgentData.AgentID = Client.self.getAgentID();
		effect.AgentData.SessionID = Client.self.getSessionID();

		effect.Effect = new ViewerEffectPacket.EffectBlock[1];
		effect.Effect[0] = new ViewerEffectPacket.EffectBlock();
		effect.Effect[0].AgentID = Client.self.getAgentID();
		effect.Effect[0].Color = color.getBytes();
		effect.Effect[0].Duration = duration;
		effect.Effect[0].ID = effectID;
		effect.Effect[0].Type = (byte)EffectType.Beam.getIndex();

		byte[] typeData = new byte[56];
		Utils.arraycopy(sourceAvatar.GetBytes(), 0, typeData, 0, 16);
		Utils.arraycopy(targetObject.GetBytes(), 0, typeData, 16, 16);
		Utils.arraycopy(globalOffset.getBytesLit(), 0, typeData, 32, 24);

		effect.Effect[0].TypeData = typeData;

		Client.network.SendPacket(effect);
	}

	/// <summary>
	/// Create a particle swirl around a target position using a <seealso cref="ViewerEffectPacket"/> packet
	/// </summary>
	/// <param name="globalOffset">global offset</param>
	/// <param name="color">A <see cref="Color4"/> object containing the combined red, green, blue and alpha 
	/// color values of particle beam</param>
	/// <param name="duration">a float representing the duration the parcicle beam will last</param>
	/// <param name="effectID">A Unique ID for the beam</param>
	public void SphereEffect(Vector3d globalOffset, Color4 color, float duration, UUID effectID)
	{
		ViewerEffectPacket effect = new ViewerEffectPacket();

		effect.AgentData.AgentID = Client.self.getAgentID();
		effect.AgentData.SessionID = Client.self.getSessionID();

		effect.Effect = new ViewerEffectPacket.EffectBlock[1];
		effect.Effect[0] = new ViewerEffectPacket.EffectBlock();
		effect.Effect[0].AgentID = Client.self.getAgentID();
		effect.Effect[0].Color = color.getBytes();
		effect.Effect[0].Duration = duration;
		effect.Effect[0].ID = effectID;
		effect.Effect[0].Type = (byte)EffectType.Sphere.getIndex();

		byte[] typeData = new byte[56];
		Utils.arraycopy(UUID.Zero.GetBytes(), 0, typeData, 0, 16);
		Utils.arraycopy(UUID.Zero.GetBytes(), 0, typeData, 16, 16);
		Utils.arraycopy(globalOffset.getBytesLit(), 0, typeData, 32, 24);

		effect.Effect[0].TypeData = typeData;

		Client.network.SendPacket(effect);
	}


	//endregion Viewer Effects

	//region Movement Actions

	/// <summary>
	/// Sends a request to sit on the specified object
	/// </summary>
	/// <param name="targetID"><seealso cref="UUID"/> of the object to sit on</param>
	/// <param name="offset">Sit at offset</param>
	public void RequestSit(UUID targetID, Vector3 offset)
	{
		AgentRequestSitPacket requestSit = new AgentRequestSitPacket();
		requestSit.AgentData.AgentID = Client.self.getAgentID();
		requestSit.AgentData.SessionID = Client.self.getSessionID();
		requestSit.TargetObject.TargetID = targetID;
		requestSit.TargetObject.Offset = offset;
		Client.network.SendPacket(requestSit);
	}

	/// <summary>
	/// Follows a call to <seealso cref="RequestSit"/> to actually sit on the object
	/// </summary>
	public void Sit()
	{
		AgentSitPacket sit = new AgentSitPacket();
		sit.AgentData.AgentID = Client.self.getAgentID();
		sit.AgentData.SessionID = Client.self.getSessionID();
		Client.network.SendPacket(sit);
	}

	/// <summary>Stands up from sitting on a prim or the ground</summary>
	/// <returns>true of AgentUpdate was sent</returns>
	public boolean Stand()
	{
		if (Client.settings.SEND_AGENT_UPDATES)
		{
			Movement.setSitOnGround(false);
			Movement.setStandUp(true);
			Movement.SendUpdate();
			Movement.setStandUp(false);
			Movement.SendUpdate();
			return true;
		}
		else
		{
			JLogger.warn("Attempted to Stand() but agent updates are disabled");
			return false;
		}
	}

	/// <summary>
	/// Does a "ground sit" at the avatar's current position
	/// </summary>
	public void SitOnGround()
	{
		Movement.setSitOnGround(true);
		Movement.SendUpdate(true);
	}

	/// <summary>
	/// Starts or stops flying
	/// </summary>
	/// <param name="start">True to start flying, false to stop flying</param>
	public void Fly(boolean start)
	{
		if (start)
			Movement.setFly(true);
		else
			Movement.setFly(false);

		Movement.SendUpdate(true);
	}

	/// <summary>
	/// Starts or stops crouching
	/// </summary>
	/// <param name="crouching">True to start crouching, false to stop crouching</param>
	public void Crouch(boolean crouching)
	{
		Movement.setUpNeg(crouching);
		Movement.SendUpdate(true);
	}

	/// <summary>
	/// Starts a jump (begin holding the jump key)
	/// </summary>
	public void Jump(boolean jumping)
	{
		Movement.setUpPos(jumping);
		Movement.setFastUp(jumping);
		Movement.SendUpdate(true);
	}

	/// <summary>
	/// Use the autopilot sim function to move the avatar to a new
	/// position. Uses double precision to get precise movements
	/// </summary>
	/// <remarks>The z value is currently not handled properly by the simulator</remarks>
	/// <param name="globalX">Global X coordinate to move to</param>
	/// <param name="globalY">Global Y coordinate to move to</param>
	/// <param name="z">Z coordinate to move to</param>
	public void AutoPilot(double globalX, double globalY, double z)
	{
		GenericMessagePacket autopilot = createGenericMessagePacket(globalX, globalY, z);

		Client.network.SendPacket(autopilot);		
	}

	public GenericMessagePacket createGenericMessagePacket(double globalX, double globalY, double z)
	{
		GenericMessagePacket autopilot = new GenericMessagePacket();

		autopilot.AgentData.AgentID = Client.self.getAgentID();
		autopilot.AgentData.SessionID = Client.self.getSessionID();
		autopilot.AgentData.TransactionID = UUID.Zero;
		autopilot.MethodData.Invoice = UUID.Zero;
		autopilot.MethodData.Method = Utils.stringToBytesWithTrailingNullByte("autopilot");
		autopilot.ParamList = new GenericMessagePacket.ParamListBlock[3];
		autopilot.ParamList[0] = new GenericMessagePacket.ParamListBlock();
		autopilot.ParamList[0].Parameter = Utils.stringToBytesWithTrailingNullByte(Double.toString(globalX));
		autopilot.ParamList[1] = new GenericMessagePacket.ParamListBlock();
		autopilot.ParamList[1].Parameter = Utils.stringToBytesWithTrailingNullByte(Double.toString(globalY));
		autopilot.ParamList[2] = new GenericMessagePacket.ParamListBlock();
		autopilot.ParamList[2].Parameter = Utils.stringToBytesWithTrailingNullByte(Double.toString(z));

		return autopilot;		
	}	


	/// <summary>
	/// Use the autopilot sim function to move the avatar to a new position
	/// </summary>
	/// <remarks>The z value is currently not handled properly by the simulator</remarks>
	/// <param name="globalX">Integer value for the global X coordinate to move to</param>
	/// <param name="globalY">Integer value for the global Y coordinate to move to</param>
	/// <param name="z">Floating-point value for the Z coordinate to move to</param>
	//	        public void AutoPilot(ulong globalX, ulong globalY, float z)
	public void AutoPilot(BigInteger globalX, BigInteger globalY, float z)
	{
		GenericMessagePacket autopilot = new GenericMessagePacket();

		autopilot.AgentData.AgentID = Client.self.getAgentID();
		autopilot.AgentData.SessionID = Client.self.getSessionID();
		autopilot.AgentData.TransactionID = UUID.Zero;
		autopilot.MethodData.Invoice = UUID.Zero;
		autopilot.MethodData.Method = Utils.stringToBytesWithTrailingNullByte("autopilot");
		autopilot.ParamList = new GenericMessagePacket.ParamListBlock[3];
		autopilot.ParamList[0] = new GenericMessagePacket.ParamListBlock();
		autopilot.ParamList[0].Parameter = Utils.stringToBytesWithTrailingNullByte(globalX.toString());
		autopilot.ParamList[1] = new GenericMessagePacket.ParamListBlock();
		autopilot.ParamList[1].Parameter = Utils.stringToBytesWithTrailingNullByte(globalY.toString());
		autopilot.ParamList[2] = new GenericMessagePacket.ParamListBlock();
		autopilot.ParamList[2].Parameter = Utils.stringToBytesWithTrailingNullByte(Float.toString(z));

		Client.network.SendPacket(autopilot);
	}

	/// <summary>
	/// Use the autopilot sim function to move the avatar to a new position
	/// </summary>
	/// <remarks>The z value is currently not handled properly by the simulator</remarks>
	/// <param name="localX">Integer value for the local X coordinate to move to</param>
	/// <param name="localY">Integer value for the local Y coordinate to move to</param>
	/// <param name="z">Floating-point value for the Z coordinate to move to</param>
	public void AutoPilotLocal(int localX, int localY, float z)
	{
		//uint
		long x, y;
		long[] a = new long[2];

		Utils.longToUInts(Client.network.getCurrentSim().Handle.longValue(), a);
		x = a[0];
		y = a[1];
		AutoPilot(new BigInteger(Utils.int64ToBytesLit(x + localX)), new BigInteger(Utils.int64ToBytesLit(y + localY)), z);
	}

	/// <summary>Macro to cancel autopilot sim function</summary>
	/// <remarks>Not certain if this is how it is really done</remarks>
	/// <returns>true if control flags were set and AgentUpdate was sent to the simulator</returns>
	public boolean AutoPilotCancel()
	{
		if (Client.settings.SEND_AGENT_UPDATES)
		{
			Movement.setAtPos(true);
			Movement.SendUpdate();
			Movement.setAtPos(false);
			Movement.SendUpdate();
			return true;
		}
		else
		{
			JLogger.warn("Attempted to AutoPilotCancel() but agent updates are disabled");
			return false;
		}
	}

	//endregion Movement actions

	//region Touch and grab

	/// <summary>
	/// Grabs an object
	/// </summary>
	/// <param name="objectLocalID">an unsigned integer of the objects ID within the simulator</param>
	/// <seealso cref="Simulator.ObjectsPrimitives"/>
	public void Grab(long objectLocalID)
	{
		Grab(objectLocalID, Vector3.Zero, Vector3.Zero, Vector3.Zero, 0, Vector3.Zero, Vector3.Zero, Vector3.Zero);
	}

	/// <summary>
	/// Overload: Grab a simulated object
	/// </summary>
	/// <param name="objectLocalID">an unsigned integer of the objects ID within the simulator</param>
	/// <param name="grabOffset"></param>
	/// <param name="uvCoord">The texture coordinates to grab</param>
	/// <param name="stCoord">The surface coordinates to grab</param>
	/// <param name="faceIndex">The face of the position to grab</param>
	/// <param name="position">The region coordinates of the position to grab</param>
	/// <param name="normal">The surface normal of the position to grab (A normal is a vector perpindicular to the surface)</param>
	/// <param name="binormal">The surface binormal of the position to grab (A binormal is a vector tangen to the surface
	/// pointing along the U direction of the tangent space</param>
	public void Grab(long objectLocalID, Vector3 grabOffset, Vector3 uvCoord, Vector3 stCoord, int faceIndex, Vector3 position,
			Vector3 normal, Vector3 binormal)
	{
		ObjectGrabPacket grab = new ObjectGrabPacket();

		grab.AgentData.AgentID = Client.self.getAgentID();
		grab.AgentData.SessionID = Client.self.getSessionID();

		grab.ObjectData.LocalID = objectLocalID;
		grab.ObjectData.GrabOffset = grabOffset;

		grab.SurfaceInfo = new ObjectGrabPacket.SurfaceInfoBlock[1];
		grab.SurfaceInfo[0] = new ObjectGrabPacket.SurfaceInfoBlock();
		grab.SurfaceInfo[0].UVCoord = uvCoord;
		grab.SurfaceInfo[0].STCoord = stCoord;
		grab.SurfaceInfo[0].FaceIndex = faceIndex;
		grab.SurfaceInfo[0].Position = position;
		grab.SurfaceInfo[0].Normal = normal;
		grab.SurfaceInfo[0].Binormal = binormal;

		Client.network.SendPacket(grab);
	}

	/// <summary>
	/// Drag an object
	/// </summary>
	/// <param name="objectID"><seealso cref="UUID"/> of the object to drag</param>
	/// <param name="grabPosition">Drag target in region coordinates</param>
	public void GrabUpdate(UUID objectID, Vector3 grabPosition)
	{
		GrabUpdate(objectID, grabPosition, Vector3.Zero, Vector3.Zero, Vector3.Zero, 0, Vector3.Zero, Vector3.Zero, Vector3.Zero);
	}

	/// <summary>
	/// Overload: Drag an object
	/// </summary>
	/// <param name="objectID"><seealso cref="UUID"/> of the object to drag</param>
	/// <param name="grabPosition">Drag target in region coordinates</param>
	/// <param name="grabOffset"></param>
	/// <param name="uvCoord">The texture coordinates to grab</param>
	/// <param name="stCoord">The surface coordinates to grab</param>
	/// <param name="faceIndex">The face of the position to grab</param>
	/// <param name="position">The region coordinates of the position to grab</param>
	/// <param name="normal">The surface normal of the position to grab (A normal is a vector perpindicular to the surface)</param>
	/// <param name="binormal">The surface binormal of the position to grab (A binormal is a vector tangen to the surface
	/// pointing along the U direction of the tangent space</param>
	public void GrabUpdate(UUID objectID, Vector3 grabPosition, Vector3 grabOffset, Vector3 uvCoord, Vector3 stCoord, int faceIndex, Vector3 position,
			Vector3 normal, Vector3 binormal)
	{
		ObjectGrabUpdatePacket grab = new ObjectGrabUpdatePacket();
		grab.AgentData.AgentID = Client.self.getAgentID();
		grab.AgentData.SessionID = Client.self.getSessionID();

		grab.ObjectData.ObjectID = objectID;
		grab.ObjectData.GrabOffsetInitial = grabOffset;
		grab.ObjectData.GrabPosition = grabPosition;
		grab.ObjectData.TimeSinceLast = 0;

		grab.SurfaceInfo = new ObjectGrabUpdatePacket.SurfaceInfoBlock[1];
		grab.SurfaceInfo[0] = new ObjectGrabUpdatePacket.SurfaceInfoBlock();
		grab.SurfaceInfo[0].UVCoord = uvCoord;
		grab.SurfaceInfo[0].STCoord = stCoord;
		grab.SurfaceInfo[0].FaceIndex = faceIndex;
		grab.SurfaceInfo[0].Position = position;
		grab.SurfaceInfo[0].Normal = normal;
		grab.SurfaceInfo[0].Binormal = binormal;

		Client.network.SendPacket(grab);
	}

	/// <summary>
	/// Release a grabbed object
	/// </summary>
	/// <param name="objectLocalID">The Objects Simulator Local ID</param>
	/// <seealso cref="Simulator.ObjectsPrimitives"/>
	/// <seealso cref="Grab"/>
	/// <seealso cref="GrabUpdate"/>
	public void DeGrab(long objectLocalID)
	{
		DeGrab(objectLocalID, Vector3.Zero, Vector3.Zero, 0, Vector3.Zero, Vector3.Zero, Vector3.Zero);
	}

	/// <summary>
	/// Release a grabbed object
	/// </summary>
	/// <param name="objectLocalID">The Objects Simulator Local ID</param>
	/// <param name="uvCoord">The texture coordinates to grab</param>
	/// <param name="stCoord">The surface coordinates to grab</param>
	/// <param name="faceIndex">The face of the position to grab</param>
	/// <param name="position">The region coordinates of the position to grab</param>
	/// <param name="normal">The surface normal of the position to grab (A normal is a vector perpindicular to the surface)</param>
	/// <param name="binormal">The surface binormal of the position to grab (A binormal is a vector tangen to the surface
	/// pointing along the U direction of the tangent space</param>
	public void DeGrab(long objectLocalID, Vector3 uvCoord, Vector3 stCoord, int faceIndex, Vector3 position,
			Vector3 normal, Vector3 binormal)
	{
		ObjectDeGrabPacket degrab = new ObjectDeGrabPacket();
		degrab.AgentData.AgentID = Client.self.getAgentID();
		degrab.AgentData.SessionID = Client.self.getSessionID();

		degrab.ObjectData.LocalID = objectLocalID;

		degrab.SurfaceInfo = new ObjectDeGrabPacket.SurfaceInfoBlock[1];
		degrab.SurfaceInfo[0] = new ObjectDeGrabPacket.SurfaceInfoBlock();
		degrab.SurfaceInfo[0].UVCoord = uvCoord;
		degrab.SurfaceInfo[0].STCoord = stCoord;
		degrab.SurfaceInfo[0].FaceIndex = faceIndex;
		degrab.SurfaceInfo[0].Position = position;
		degrab.SurfaceInfo[0].Normal = normal;
		degrab.SurfaceInfo[0].Binormal = binormal;

		Client.network.SendPacket(degrab);
	}

	/// <summary>
	/// Touches an object
	/// </summary>
	/// <param name="objectLocalID">an unsigned integer of the objects ID within the simulator</param>
	/// <seealso cref="Simulator.ObjectsPrimitives"/>
	public void Touch(long objectLocalID)
	{
		Client.self.Grab(objectLocalID);
		Client.self.DeGrab(objectLocalID);
	}

	//endregion Touch and grab

	//region Money

	/// <summary>
	/// Request the current L$ balance
	/// </summary>
	public void RequestBalance()
	{
		MoneyBalanceRequestPacket money = new MoneyBalanceRequestPacket();
		money.AgentData.AgentID = Client.self.getAgentID();
		money.AgentData.SessionID = Client.self.getSessionID();
		money.MoneyData.TransactionID = UUID.Zero;

		Client.network.SendPacket(money);
	}

	/// <summary>
	/// Give Money to destination Avatar
	/// </summary>
	/// <param name="target">UUID of the Target Avatar</param>
	/// <param name="amount">Amount in L$</param>
	public void GiveAvatarMoney(UUID target, int amount)
	{
		GiveMoney(target, amount, "", MoneyTransactionType.Gift, TransactionFlags.None);
	}

	/// <summary>
	/// Give Money to destination Avatar
	/// </summary>
	/// <param name="target">UUID of the Target Avatar</param>
	/// <param name="amount">Amount in L$</param>
	/// <param name="description">Description that will show up in the
	/// recipients transaction history</param>
	public void GiveAvatarMoney(UUID target, int amount, String description)
	{
		GiveMoney(target, amount, description, MoneyTransactionType.Gift, TransactionFlags.None);
	}

	/// <summary>
	/// Give L$ to an object
	/// </summary>
	/// <param name="target">object <seealso cref="UUID"/> to give money to</param>
	/// <param name="amount">amount of L$ to give</param>
	/// <param name="objectName">name of object</param>
	public void GiveObjectMoney(UUID target, int amount, String objectName)
	{
		GiveMoney(target, amount, objectName, MoneyTransactionType.PayObject, TransactionFlags.None);
	}

	/// <summary>
	/// Give L$ to a group
	/// </summary>
	/// <param name="target">group <seealso cref="UUID"/> to give money to</param>
	/// <param name="amount">amount of L$ to give</param>
	public void GiveGroupMoney(UUID target, int amount)
	{
		GiveMoney(target, amount, "", MoneyTransactionType.Gift, TransactionFlags.DestGroup);
	}

	/// <summary>
	/// Give L$ to a group
	/// </summary>
	/// <param name="target">group <seealso cref="UUID"/> to give money to</param>
	/// <param name="amount">amount of L$ to give</param>
	/// <param name="description">description of transaction</param>
	public void GiveGroupMoney(UUID target, int amount, String description)
	{
		GiveMoney(target, amount, description, MoneyTransactionType.Gift, TransactionFlags.DestGroup);
	}

	/// <summary>
	/// Pay texture/animation upload fee
	/// </summary>
	public void PayUploadFee()
	{
		GiveMoney(UUID.Zero, Client.settings.UPLOAD_COST(), "", MoneyTransactionType.UploadCharge,
				TransactionFlags.None);
	}

	/// <summary>
	/// Pay texture/animation upload fee
	/// </summary>
	/// <param name="description">description of the transaction</param>
	public void PayUploadFee(String description)
	{
		GiveMoney(UUID.Zero, Client.settings.UPLOAD_COST(), description, MoneyTransactionType.UploadCharge,
				TransactionFlags.None);
	}

	/// <summary>
	/// Give Money to destination Object or Avatar
	/// </summary>
	/// <param name="target">UUID of the Target Object/Avatar</param>
	/// <param name="amount">Amount in L$</param>
	/// <param name="description">Reason (Optional normally)</param>
	/// <param name="type">The type of transaction</param>
	/// <param name="flags">Transaction flags, mostly for identifying group
	/// transactions</param>
	public void GiveMoney(UUID target, int amount, String description, MoneyTransactionType type, TransactionFlags flags)
	{
		MoneyTransferRequestPacket money = new MoneyTransferRequestPacket();
		money.AgentData.AgentID = this.id;
		money.AgentData.SessionID = Client.self.getSessionID();
		money.MoneyData.Description = Utils.stringToBytesWithTrailingNullByte(description);
		money.MoneyData.DestID = target;
		money.MoneyData.SourceID = this.id;
		money.MoneyData.TransactionType = (int)type.getIndex();
		money.MoneyData.AggregatePermInventory = 0; // This is weird, apparently always set to zero though
		money.MoneyData.AggregatePermNextOwner = 0; // This is weird, apparently always set to zero though
		money.MoneyData.Flags = (byte)flags.getIndex();
		money.MoneyData.Amount = amount;

		Client.network.SendPacket(money);
	}

	//endregion Money

	//region Gestures
	/// <summary>
	/// Plays a gesture
	/// </summary>
	/// <param name="gestureID">Asset <seealso cref="UUID"/> of the gesture</param>
	public void PlayGesture(final UUID gestureID)
	{
		threadPool.execute(new Runnable(){
			public void run()
			{
				final AssetGesture[] gesture = new AssetGesture[1];

				if (gestureCache.containsKey(gestureID))
				{
					gesture[0] = gestureCache.get(gestureID);
				}
				else
				{
					final AutoResetEvent gotAsset = new AutoResetEvent(false);

					MethodDelegate<Void, AssetReceivedCallbackArgs> assetReceivedCallback = new MethodDelegate<Void, AssetReceivedCallbackArgs>()
							{
						public Void execute(
								AssetReceivedCallbackArgs e) {
							if (e.getTransfer().Success)
							{
								gesture[0] = (AssetGesture)e.getAsset();
							}

							gotAsset.set();
							return null;
						}

							};
							Client.assets.RequestAsset(gestureID, AssetType.Gesture, true, 
									assetReceivedCallback);

							try {
								gotAsset.waitOne(30 * 1000);
							} catch (InterruptedException e1) {
								JLogger.error("Error while playing a gesture: " + Utils.getExceptionStackTraceAsString(e1));
							}

							if (gesture[0] != null && gesture[0].Decode())
							{
								synchronized (gestureCache)
								{
									if (!gestureCache.containsKey(gestureID))
									{
										gestureCache.put(gestureID, gesture[0]);
									}
								}
							}
				}

				// We got it, now we play it
				if (gesture[0] != null)
				{
					for (int i = 0; i < gesture[0].Sequence.size(); i++)
					{
						GestureStep step = gesture[0].Sequence.get(i);

						switch (step.getGestureStepType())
						{
						case Chat:
							Chat(((GestureStepChat)step).Text, 0, ChatType.Normal);
							break;

						case Animation:
							GestureStepAnimation anim = (GestureStepAnimation)step;

							if (anim.AnimationStart)
							{
								if (SignaledAnimations.containsKey(anim.ID))
								{
									AnimationStop(anim.ID, true);
								}
								AnimationStart(anim.ID, true);
							}
							else
							{
								AnimationStop(anim.ID, true);
							}
							break;

						case Sound:
							Client.sound.PlaySound(((GestureStepSound)step).ID);
							break;

						case Wait:
							GestureStepWait wait = (GestureStepWait)step;
							if (wait.WaitForTime)
							{
								PlatformUtils.sleep((int)(1000f * wait.WaitTime));
							}
							if (wait.WaitForAnimation)
							{
								// TODO: implement waiting for all animations to end that were triggered
								// during playing of this guesture sequence
							}
							break;
						}
					}
				}
			}
		});
	}

	/// <summary>
	/// Mark gesture active
	/// </summary>
	/// <param name="invID">Inventory <seealso cref="UUID"/> of the gesture</param>
	/// <param name="assetID">Asset <seealso cref="UUID"/> of the gesture</param>
	public void ActivateGesture(UUID invID, UUID assetID)
	{
		ActivateGesturesPacket p = new ActivateGesturesPacket();

		p.AgentData.AgentID = getAgentID();
		p.AgentData.SessionID = getSessionID();
		p.AgentData.Flags = 0x00;

		ActivateGesturesPacket.DataBlock b = new ActivateGesturesPacket.DataBlock();
		b.ItemID = invID;
		b.AssetID = assetID;
		b.GestureFlags = 0x00;

		p.Data = new ActivateGesturesPacket.DataBlock[1];
		p.Data[0] = b;

		Client.network.SendPacket(p);

	}

	/// <summary>
	/// Mark gesture inactive
	/// </summary>
	/// <param name="invID">Inventory <seealso cref="UUID"/> of the gesture</param>
	public void DeactivateGesture(UUID invID)
	{
		DeactivateGesturesPacket p = new DeactivateGesturesPacket();

		p.AgentData.AgentID = getAgentID();
		p.AgentData.SessionID = getSessionID();
		p.AgentData.Flags = 0x00;

		DeactivateGesturesPacket.DataBlock b = new DeactivateGesturesPacket.DataBlock();
		b.ItemID = invID;
		b.GestureFlags = 0x00;

		p.Data = new DeactivateGesturesPacket.DataBlock[1];
		p.Data[0] = b;

		Client.network.SendPacket(p);
	}
	//endregion

	//region Animations

	/// <summary>
	/// Send an AgentAnimation packet that toggles a single animation on
	/// </summary>
	/// <param name="animation">The <seealso cref="UUID"/> of the animation to start playing</param>
	/// <param name="reliable">Whether to ensure delivery of this packet or not</param>
	public void AnimationStart(UUID animation, boolean reliable)
	{
		Map<UUID, Boolean> animations = new HashMap<UUID, Boolean>();
		animations.put(animation, true);

		Animate(animations, reliable);
	}

	/// <summary>
	/// Send an AgentAnimation packet that toggles a single animation off
	/// </summary>
	/// <param name="animation">The <seealso cref="UUID"/> of a 
	/// currently playing animation to stop playing</param>
	/// <param name="reliable">Whether to ensure delivery of this packet or not</param>
	public void AnimationStop(UUID animation, boolean reliable)
	{
		Map<UUID, Boolean> animations = new HashMap<UUID, Boolean>();
		animations.put(animation, false);

		Animate(animations, reliable);
	}

	/// <summary>
	/// Send an AgentAnimation packet that will toggle animations on or off
	/// </summary>
	/// <param name="animations">A list of animation <seealso cref="UUID"/>s, and whether to
	/// turn that animation on or off</param>
	/// <param name="reliable">Whether to ensure delivery of this packet or not</param>
	public void Animate(Map<UUID, Boolean> animations, boolean reliable)
	{
		AgentAnimationPacket animate = new AgentAnimationPacket();
		animate.header.Reliable = reliable;

		animate.AgentData.AgentID = Client.self.getAgentID();
		animate.AgentData.SessionID = Client.self.getSessionID();
		animate.AnimationList = new AgentAnimationPacket.AnimationListBlock[animations.size()];
		int i = 0;

		for (Entry<UUID, Boolean> animation : animations.entrySet())
		{
			animate.AnimationList[i] = new AgentAnimationPacket.AnimationListBlock();
			animate.AnimationList[i].AnimID = animation.getKey();
			animate.AnimationList[i].StartAnim = animation.getValue();

			i++;
		}

		// TODO: Implement support for this
		animate.PhysicalAvatarEventList = new AgentAnimationPacket.PhysicalAvatarEventListBlock[0];

		Client.network.SendPacket(animate);
	}

	//endregion Animations

	//region Teleporting

	/// <summary>
	/// Teleports agent to their stored home location
	/// </summary>
	/// <returns>true on successful teleport to home location</returns>
	public boolean GoHome() throws InterruptedException
	{
		return Teleport(UUID.Zero);
	}

	/// <summary>
	/// Teleport agent to a landmark
	/// </summary>
	/// <param name="landmark"><seealso cref="UUID"/> of the landmark to teleport agent to</param>
	/// <returns>true on success, false on failure</returns>
	public boolean Teleport(UUID landmark) throws InterruptedException
	{
		teleportStat = TeleportStatus.None;
		teleportEvent.reset();
		TeleportLandmarkRequestPacket p = new TeleportLandmarkRequestPacket();
		p.Info = new TeleportLandmarkRequestPacket.InfoBlock();
		p.Info.AgentID = Client.self.getAgentID();
		p.Info.SessionID = Client.self.getSessionID();
		p.Info.LandmarkID = landmark;
		Client.network.SendPacket(p);

		teleportEvent.waitOne(Client.settings.TELEPORT_TIMEOUT);

		if (teleportStat == TeleportStatus.None ||
				teleportStat == TeleportStatus.Start ||
				teleportStat == TeleportStatus.Progress)
		{
			teleportMessage = "Teleport timed out.";
			teleportStat = TeleportStatus.Failed;
		}

		return (teleportStat == TeleportStatus.Finished);
	}

	/// <summary>
	/// Attempt to look up a simulator name and teleport to the discovered
	/// destination
	/// </summary>
	/// <param name="simName">Region name to look up</param>
	/// <param name="position">Position to teleport to</param>
	/// <returns>True if the lookup and teleport were successful, otherwise
	/// false</returns>
	public boolean Teleport(String simName, Vector3 position) throws InterruptedException
	{
		return Teleport(simName, position, new Vector3(0, 1.0f, 0));
	}

	/// <summary>
	/// Attempt to look up a simulator name and teleport to the discovered
	/// destination
	/// </summary>
	/// <param name="simName">Region name to look up</param>
	/// <param name="position">Position to teleport to</param>
	/// <param name="lookAt">Target to look at</param>
	/// <returns>True if the lookup and teleport were successful, otherwise
	/// false</returns>
	public boolean Teleport(String simName, Vector3 position, Vector3 lookAt) throws InterruptedException
	{
		if (Client.network.getCurrentSim() == null)
			return false;

		teleportStat = TeleportStatus.None;

		if (simName != Client.network.getCurrentSim().Name)
		{
			// Teleporting to a foreign sim
			GridRegion[] region = new GridRegion[1];

			if (Client.grid.GetGridRegion(simName, GridLayerType.Objects, region))
			{
				return Teleport(region[0].RegionHandle, position, lookAt);
			}
			else
			{
				teleportMessage = "Unable to resolve name: " + simName;
				teleportStat = TeleportStatus.Failed;
				return false;
			}
		}
		else
		{
			// Teleporting to the sim we're already in
			return Teleport(Client.network.getCurrentSim().Handle, position, lookAt);
		}
	}

	/// <summary>
	/// Teleport agent to another region
	/// </summary>
	/// <param name="regionHandle">handle of region to teleport agent to</param>
	/// <param name="position"><seealso cref="Vector3"/> position in destination sim to teleport to</param>
	/// <returns>true on success, false on failure</returns>
	/// <remarks>This call is blocking</remarks>
	public boolean Teleport(BigInteger regionHandle, Vector3 position) throws InterruptedException
	{
		return Teleport(regionHandle, position, new Vector3(0.0f, 1.0f, 0.0f));
	}

	/// <summary>
	/// Teleport agent to another region
	/// </summary>
	/// <param name="regionHandle">handle of region to teleport agent to</param>
	/// <param name="position"><seealso cref="Vector3"/> position in destination sim to teleport to</param>
	/// <param name="lookAt"><seealso cref="Vector3"/> direction in destination sim agent will look at</param>
	/// <returns>true on success, false on failure</returns>
	/// <remarks>This call is blocking</remarks>
	public boolean Teleport(BigInteger regionHandle, Vector3 position, Vector3 lookAt) throws InterruptedException
	{
		if (Client.network.getCurrentSim() == null ||
				Client.network.getCurrentSim().Caps == null ||
				!Client.network.getCurrentSim().Caps.isEventQueueRunning())
		{
			// Wait a bit to see if the event queue comes online
			final AutoResetEvent queueEvent = new AutoResetEvent(false);
			//		                EventHandler<EventQueueRunningEventArgs> queueCallback =
			//		                    delegate(Object sender, EventQueueRunningEventArgs e)
			//		                    {
			//		                        if (e.getSimulator() == Client.network.getCurrentSim())
			//		                            queueEvent.Set();
			//		                    };
			//		
			//		                Client.network.EventQueueRunning += queueCallback;
			//		                queueEvent.WaitOne(10 * 1000, false);
			//		                Client.network.EventQueueRunning -= queueCallback;

			EventObserver<EventQueueRunningEventArgs> queueCallback = new EventObserver<EventQueueRunningEventArgs>()
					{ @Override
				public void handleEvent(Observable o, EventQueueRunningEventArgs e) 
					{
						if (e.getSimulator().equals(Client.network.getCurrentSim()))
							queueEvent.set();
					}
					};
					Client.network.RegisterOnEventQueueRunningCallback(queueCallback);
					queueEvent.waitOne(10 * 1000);
					Client.network.UnregisterOnEventQueueRunningCallback(queueCallback);		                
		}

		teleportStat = TeleportStatus.None;
		teleportEvent.reset();

		RequestTeleport(regionHandle, position, lookAt);

		teleportEvent.waitOne(Client.settings.TELEPORT_TIMEOUT);

		if (teleportStat == TeleportStatus.None ||
				teleportStat == TeleportStatus.Start ||
				teleportStat == TeleportStatus.Progress)
		{
			teleportMessage = "Teleport timed out.";
			teleportStat = TeleportStatus.Failed;
		}

		return (teleportStat == TeleportStatus.Finished);
	}

	/// <summary>
	/// Request teleport to a another simulator
	/// </summary>
	/// <param name="regionHandle">handle of region to teleport agent to</param>
	/// <param name="position"><seealso cref="Vector3"/> position in destination sim to teleport to</param>
	public void RequestTeleport(BigInteger regionHandle, Vector3 position)
	{
		RequestTeleport(regionHandle, position, new Vector3(0.0f, 1.0f, 0.0f));
	}

	/// <summary>
	/// Request teleport to a another simulator
	/// </summary>
	/// <param name="regionHandle">handle of region to teleport agent to</param>
	/// <param name="position"><seealso cref="Vector3"/> position in destination sim to teleport to</param>
	/// <param name="lookAt"><seealso cref="Vector3"/> direction in destination sim agent will look at</param>
	public void RequestTeleport(BigInteger regionHandle, Vector3 position, Vector3 lookAt)
	{
		if (Client.network.getCurrentSim() != null &&
				Client.network.getCurrentSim().Caps != null &&
				Client.network.getCurrentSim().Caps.isEventQueueRunning())
		{
			TeleportLocationRequestPacket teleport = new TeleportLocationRequestPacket();
			teleport.AgentData.AgentID = Client.self.getAgentID();
			teleport.AgentData.SessionID = Client.self.getSessionID();
			teleport.Info.LookAt = lookAt;
			teleport.Info.Position = position;
			teleport.Info.RegionHandle = regionHandle;

			JLogger.info("Requesting teleport to region handle " + regionHandle.toString());

			Client.network.SendPacket(teleport);
		}
		else
		{
			teleportMessage = "CAPS event queue is not running";
			teleportEvent.set();
			teleportStat = TeleportStatus.Failed;
		}
	}

	/// <summary>
	/// Teleport agent to a landmark
	/// </summary>
	/// <param name="landmark"><seealso cref="UUID"/> of the landmark to teleport agent to</param>
	public void RequestTeleport(UUID landmark)
	{
		TeleportLandmarkRequestPacket p = new TeleportLandmarkRequestPacket();
		p.Info = new TeleportLandmarkRequestPacket.InfoBlock();
		p.Info.AgentID = Client.self.getAgentID();
		p.Info.SessionID = Client.self.getSessionID();
		p.Info.LandmarkID = landmark;
		Client.network.SendPacket(p);
	}

	/// <summary>
	/// Send a teleport lure to another avatar with default "Join me in ..." invitation message
	/// </summary>
	/// <param name="targetID">target avatars <seealso cref="UUID"/> to lure</param>
	public void SendTeleportLure(UUID targetID)
	{
		SendTeleportLure(targetID, "Join me in " + Client.network.getCurrentSim().Name + "!");
	}

	/// <summary>
	/// Send a teleport lure to another avatar with custom invitation message
	/// </summary>
	/// <param name="targetID">target avatars <seealso cref="UUID"/> to lure</param>
	/// <param name="message">custom message to send with invitation</param>
	public void SendTeleportLure(UUID targetID, String message)
	{
		StartLurePacket p = new StartLurePacket();
		p.AgentData.AgentID = Client.self.id;
		p.AgentData.SessionID = Client.self.getSessionID();
		p.Info.LureType = 0;
		p.Info.Message = Utils.stringToBytesWithTrailingNullByte(message);
		p.TargetData = new StartLurePacket.TargetDataBlock[] { new StartLurePacket.TargetDataBlock() };
		p.TargetData[0].TargetID = targetID;
		Client.network.SendPacket(p);
	}

	/// <summary>
	/// Respond to a teleport lure by either accepting it and initiating 
	/// the teleport, or denying it
	/// </summary>
	/// <param name="requesterID"><seealso cref="UUID"/> of the avatar sending the lure</param>
	/// <param name="sessionID">IM session <seealso cref="UUID"/> of the incoming lure request</param>
	/// <param name="accept">true to accept the lure, false to decline it</param>
	public void TeleportLureRespond(UUID requesterID, UUID sessionID, boolean accept)
	{
		if (accept)
		{
			TeleportLureRequestPacket lure = new TeleportLureRequestPacket();

			lure.Info.AgentID = Client.self.getAgentID();
			lure.Info.SessionID = Client.self.getSessionID();
			lure.Info.LureID = sessionID;
			lure.Info.TeleportFlags = TeleportFlags.ViaLure.getIndex();

			Client.network.SendPacket(lure);
		}
		else
		{
			InstantMessage(getName(), requesterID, "", sessionID,
					accept ? InstantMessageDialog.AcceptTeleport : InstantMessageDialog.DenyTeleport,
							InstantMessageOnline.Offline, this.getSimPosition(), UUID.Zero, Utils.EmptyBytes);
		}
	}

	//endregion Teleporting

	//region Misc

	/// <summary>
	/// Update agent profile
	/// </summary>
	/// <param name="profile"><seealso cref="OpenMetaverse.Avatar.AvatarProperties"/> struct containing updated 
	/// profile information</param>
	public void UpdateProfile(Avatar.AvatarProperties profile)
	{
		AvatarPropertiesUpdatePacket apup = new AvatarPropertiesUpdatePacket();
		apup.AgentData.AgentID = id;
		apup.AgentData.SessionID = sessionID;
		apup.PropertiesData.AboutText = Utils.stringToBytesWithTrailingNullByte(profile.AboutText);
		apup.PropertiesData.AllowPublish = profile.isAllowPublish();
		apup.PropertiesData.FLAboutText = Utils.stringToBytesWithTrailingNullByte(profile.FirstLifeText);
		apup.PropertiesData.FLImageID = profile.FirstLifeImage;
		apup.PropertiesData.ImageID = profile.ProfileImage;
		apup.PropertiesData.MaturePublish = profile.getMaturePublish();
		apup.PropertiesData.ProfileURL = Utils.stringToBytesWithTrailingNullByte(profile.ProfileURL);

		Client.network.SendPacket(apup);
	}

	/// <summary>
	/// Update agents profile interests
	/// </summary>
	/// <param name="interests">selection of interests from <seealso cref="T:OpenMetaverse.Avatar.Interests"/> struct</param>
	public void UpdateInterests(Avatar.Interests interests)
	{
		AvatarInterestsUpdatePacket aiup = new AvatarInterestsUpdatePacket();
		aiup.AgentData.AgentID = id;
		aiup.AgentData.SessionID = sessionID;
		aiup.PropertiesData.LanguagesText = Utils.stringToBytesWithTrailingNullByte(interests.LanguagesText);
		aiup.PropertiesData.SkillsMask = interests.SkillsMask;
		aiup.PropertiesData.SkillsText = Utils.stringToBytesWithTrailingNullByte(interests.SkillsText);
		aiup.PropertiesData.WantToMask = interests.WantToMask;
		aiup.PropertiesData.WantToText = Utils.stringToBytesWithTrailingNullByte(interests.WantToText);

		Client.network.SendPacket(aiup);
	}

	/// <summary>
	/// Set the height and the width of the client window. This is used
	/// by the server to build a virtual camera frustum for our avatar
	/// </summary>
	/// <param name="height">New height of the viewer window</param>
	/// <param name="width">New width of the viewer window</param>
	public void SetHeightWidth(int height, int width)
	{
		AgentHeightWidthPacket heightwidth = new AgentHeightWidthPacket();
		heightwidth.AgentData.AgentID = Client.self.getAgentID();
		heightwidth.AgentData.SessionID = Client.self.getSessionID();
		heightwidth.AgentData.CircuitCode = Client.network.getCircuitCode();
		heightwidth.HeightWidthBlock.Height = height;
		heightwidth.HeightWidthBlock.Width = width;
		heightwidth.HeightWidthBlock.GenCounter = heightWidthGenCounter++;

		Client.network.SendPacket(heightwidth);
	}

	/// <summary>
	/// Request the list of muted objects and avatars for this agent
	/// </summary>
	public void RequestMuteList()
	{
		MuteListRequestPacket mute = new MuteListRequestPacket();
		mute.AgentData.AgentID = Client.self.getAgentID();
		mute.AgentData.SessionID = Client.self.getSessionID();
		mute.MuteData.MuteCRC = 0;

		Client.network.SendPacket(mute);
	}

	/// <summary>
	/// Mute an object, resident, etc.
	/// </summary>
	/// <param name="type">Mute type</param>
	/// <param name="id">Mute UUID</param>
	/// <param name="name">Mute name</param>
	public void UpdateMuteListEntry(MuteType type, UUID id, String name)
	{
		UpdateMuteListEntry(type, id, name, MuteFlags.get(MuteFlags.Default.getIndex()));
	}

	/// <summary>
	/// Mute an object, resident, etc.
	/// </summary>
	/// <param name="type">Mute type</param>
	/// <param name="id">Mute UUID</param>
	/// <param name="name">Mute name</param>
	/// <param name="flags">Mute flags</param>
	public void UpdateMuteListEntry(MuteType type, UUID id, String name, EnumSet<MuteFlags> flags)
	{
		UpdateMuteListEntryPacket p = new UpdateMuteListEntryPacket();
		p.AgentData.AgentID = Client.self.getAgentID();
		p.AgentData.SessionID = Client.self.getSessionID();

		p.MuteData.MuteType = (int)type.getIndex();
		p.MuteData.MuteID = id;
		p.MuteData.MuteName = Utils.stringToBytesWithTrailingNullByte(name);
		p.MuteData.MuteFlags = (long)MuteFlags.getIndex(flags);

		Client.network.SendPacket(p);

		MuteEntry me = new MuteEntry();
		me.Type = type;
		me.ID = id;
		me.Name = name;
		me.Flags = flags;
		synchronized (MuteList.getDictionary())
		{
			MuteList.add(String.format("{0}|{1}", me.ID, me.Name), me);
		}
		onMuteListUpdated.raiseEvent(new EventArgs());

	}

	/// <summary>
	/// Unmute an object, resident, etc.
	/// </summary>
	/// <param name="id">Mute UUID</param>
	/// <param name="name">Mute name</param>
	public void RemoveMuteListEntry(UUID id, String name)
	{
		RemoveMuteListEntryPacket p = new RemoveMuteListEntryPacket();
		p.AgentData.AgentID = Client.self.getAgentID();
		p.AgentData.SessionID = Client.self.getSessionID();

		p.MuteData.MuteID = id;
		p.MuteData.MuteName = Utils.stringToBytesWithTrailingNullByte(name);

		Client.network.SendPacket(p);

		String listKey = String.format("%s|%s", id.toString(), name);
		if (MuteList.containsKey(listKey))
		{
			synchronized (MuteList.getDictionary())
			{
				MuteList.remove(listKey);
			}
			onMuteListUpdated.raiseEvent(new EventArgs());
		}
	}

	/// <summary>
	/// Sets home location to agents current position
	/// </summary>
	/// <remarks>will fire an AlertMessage (<seealso cref="E:OpenMetaverse.AgentManager.OnAlertMessage"/>) with 
	/// success or failure message</remarks>
	public void SetHome()
	{
		SetStartLocationRequestPacket s = new SetStartLocationRequestPacket();
		s.AgentData = new SetStartLocationRequestPacket.AgentDataBlock();
		s.AgentData.AgentID = Client.self.getAgentID();
		s.AgentData.SessionID = Client.self.getSessionID();
		s.StartLocationData = new SetStartLocationRequestPacket.StartLocationDataBlock();
		s.StartLocationData.LocationPos = Client.self.getSimPosition();
		s.StartLocationData.LocationID = 1;
		s.StartLocationData.SimName = Utils.stringToBytesWithTrailingNullByte("");
		s.StartLocationData.LocationLookAt = Movement.Camera.getAtAxis();
		Client.network.SendPacket(s);
	}

	/// <summary>
	/// Move an agent in to a simulator. This packet is the last packet
	/// needed to complete the transition in to a new simulator
	/// </summary>
	/// <param name="simulator"><seealso cref="T:OpenMetaverse.getSimulator()"/> Object</param>
	public void CompleteAgentMovement(Simulator simulator)
	{
		CompleteAgentMovementPacket move = new CompleteAgentMovementPacket();

		move.AgentData.AgentID = Client.self.getAgentID();
		move.AgentData.SessionID = Client.self.getSessionID();
		move.AgentData.CircuitCode = Client.network.getCircuitCode();

		Client.network.SendPacket(move, simulator);
	}

	/// <summary>
	/// Reply to script permissions request
	/// </summary>
	/// <param name="simulator"><seealso cref="T:OpenMetaverse.getSimulator()"/> Object</param>
	/// <param name="itemID"><seealso cref="UUID"/> of the itemID requesting permissions</param>
	/// <param name="taskID"><seealso cref="UUID"/> of the taskID requesting permissions</param>
	/// <param name="permissions"><seealso cref="OpenMetaverse.ScriptPermission"/> list of permissions to allow</param>
	public void ScriptQuestionReply(Simulator simulator, UUID itemID, UUID taskID, ScriptPermission permissions)
	{
		ScriptAnswerYesPacket yes = new ScriptAnswerYesPacket();
		yes.AgentData.AgentID = Client.self.getAgentID();
		yes.AgentData.SessionID = Client.self.getSessionID();
		yes.Data.ItemID = itemID;
		yes.Data.TaskID = taskID;
		yes.Data.Questions = (int)permissions.getIndex();

		Client.network.SendPacket(yes, simulator);
	}

	/// <summary>
	/// Respond to a group invitation by either accepting or denying it
	/// </summary>
	/// <param name="groupID">UUID of the group (sent in the AgentID field of the invite message)</param>
	/// <param name="imSessionID">IM Session ID from the group invitation message</param>
	/// <param name="accept">Accept the group invitation or deny it</param>
	public void GroupInviteRespond(UUID groupID, UUID imSessionID, boolean accept)
	{
		InstantMessage(getName(), groupID, "", imSessionID,
				accept ? InstantMessageDialog.GroupInvitationAccept : InstantMessageDialog.GroupInvitationDecline,
						InstantMessageOnline.Offline, Vector3.Zero, UUID.Zero, Utils.EmptyBytes);
	}

	/// <summary>
	/// Requests script detection of objects and avatars
	/// </summary>
	/// <param name="name">name of the object/avatar to search for</param>
	/// <param name="searchID">UUID of the object or avatar to search for</param>
	/// <param name="type">Type of search from ScriptSensorTypeFlags</param>
	/// <param name="range">range of scan (96 max?)</param>
	/// <param name="arc">the arc in radians to search within</param>
	/// <param name="requestID">an user generated ID to correlate replies with</param>
	/// <param name="sim">Simulator to perform search in</param>
	public void RequestScriptSensor(String name, UUID searchID, ScriptSensorTypeFlags type, float range, float arc, UUID requestID, Simulator sim)
	{
		ScriptSensorRequestPacket request = new ScriptSensorRequestPacket();
		request.Requester.Arc = arc;
		request.Requester.Range = range;
		request.Requester.RegionHandle = sim.Handle;
		request.Requester.RequestID = requestID;
		request.Requester.SearchDir = Quaternion.Identity; // TODO: this needs to be tested
		request.Requester.SearchID = searchID;
		request.Requester.SearchName = Utils.stringToBytesWithTrailingNullByte(name);
		request.Requester.SearchPos = Vector3.Zero;
		request.Requester.SearchRegions = 0; // TODO: ?
		request.Requester.SourceID = Client.self.getAgentID();
		request.Requester.Type = (int)type.getIndex();

		Client.network.SendPacket(request, sim);
	}

	/// <summary>
	/// Create or update profile pick
	/// </summary>
	/// <param name="pickID">UUID of the pick to update, or random UUID to create a new pick</param>
	/// <param name="topPick">Is this a top pick? (typically false)</param>
	/// <param name="parcelID">UUID of the parcel (UUID.Zero for the current parcel)</param>
	/// <param name="name">Name of the pick</param>
	/// <param name="globalPosition">Global position of the pick landmark</param>
	/// <param name="textureID">UUID of the image displayed with the pick</param>
	/// <param name="description">Long description of the pick</param>
	public void PickInfoUpdate(UUID pickID, boolean topPick, UUID parcelID, String name, Vector3d globalPosition, UUID textureID, String description)
	{
		PickInfoUpdatePacket pick = new PickInfoUpdatePacket();
		pick.AgentData.AgentID = Client.self.getAgentID();
		pick.AgentData.SessionID = Client.self.getSessionID();
		pick.Data.PickID = pickID;
		pick.Data.Desc = Utils.stringToBytesWithTrailingNullByte(description);
		pick.Data.CreatorID = Client.self.getAgentID();
		pick.Data.TopPick = topPick;
		pick.Data.ParcelID = parcelID;
		pick.Data.Name = Utils.stringToBytesWithTrailingNullByte(name);
		pick.Data.SnapshotID = textureID;
		pick.Data.PosGlobal = globalPosition;
		pick.Data.SortOrder = 0;
		pick.Data.Enabled = false;

		Client.network.SendPacket(pick);
	}

	/// <summary>
	/// Delete profile pick
	/// </summary>
	/// <param name="pickID">UUID of the pick to delete</param>
	public void PickDelete(UUID pickID)
	{
		PickDeletePacket delete = new PickDeletePacket();
		delete.AgentData.AgentID = Client.self.getAgentID();
		delete.AgentData.SessionID = Client.self.sessionID;
		delete.Data.PickID = pickID;

		Client.network.SendPacket(delete);
	}

	/// <summary>
	/// Create or update profile Classified
	/// </summary>
	/// <param name="classifiedID">UUID of the classified to update, or random UUID to create a new classified</param>
	/// <param name="category">Defines what catagory the classified is in</param>
	/// <param name="snapshotID">UUID of the image displayed with the classified</param>
	/// <param name="price">Price that the classified will cost to place for a week</param>
	/// <param name="position">Global position of the classified landmark</param>
	/// <param name="name">Name of the classified</param>
	/// <param name="desc">Long description of the classified</param>
	/// <param name="autoRenew">if true, auto renew classified after expiration</param>
	public void UpdateClassifiedInfo(UUID classifiedID, DirectoryManager.ClassifiedCategories category,
			UUID snapshotID, int price, Vector3d position, String name, String desc, boolean autoRenew)
	{
		ClassifiedInfoUpdatePacket classified = new ClassifiedInfoUpdatePacket();
		classified.AgentData.AgentID = Client.self.getAgentID();
		classified.AgentData.SessionID = Client.self.getSessionID();

		classified.Data.ClassifiedID = classifiedID;
		classified.Data.Category = (long)category.getIndex();

		classified.Data.ParcelID = UUID.Zero;
		// TODO: verify/fix ^
		classified.Data.ParentEstate = 0;
		// TODO: verify/fix ^

		classified.Data.SnapshotID = snapshotID;
		classified.Data.PosGlobal = position;

		classified.Data.ClassifiedFlags = autoRenew ? (byte)32 : (byte)0;
		// TODO: verify/fix ^

		classified.Data.PriceForListing = price;
		classified.Data.Name = Utils.stringToBytesWithTrailingNullByte(name);
		classified.Data.Desc = Utils.stringToBytesWithTrailingNullByte(desc);
		Client.network.SendPacket(classified);
	}

	/// <summary>
	/// Create or update profile Classified
	/// </summary>
	/// <param name="classifiedID">UUID of the classified to update, or random UUID to create a new classified</param>
	/// <param name="category">Defines what catagory the classified is in</param>
	/// <param name="snapshotID">UUID of the image displayed with the classified</param>
	/// <param name="price">Price that the classified will cost to place for a week</param>
	/// <param name="name">Name of the classified</param>
	/// <param name="desc">Long description of the classified</param>
	/// <param name="autoRenew">if true, auto renew classified after expiration</param>
	public void UpdateClassifiedInfo(UUID classifiedID, DirectoryManager.ClassifiedCategories category, UUID snapshotID, int price, String name, String desc, boolean autoRenew)
	{
		UpdateClassifiedInfo(classifiedID, category, snapshotID, price, Client.self.getGlobalPosition(), name, desc, autoRenew);
	}

	/// <summary>
	/// Delete a classified ad
	/// </summary>
	/// <param name="classifiedID">The classified ads ID</param>
	public void DeleteClassfied(UUID classifiedID)
	{
		ClassifiedDeletePacket classified = new ClassifiedDeletePacket();
		classified.AgentData.AgentID = Client.self.getAgentID();
		classified.AgentData.SessionID = Client.self.getSessionID();

		classified.Data.ClassifiedID = classifiedID;
		Client.network.SendPacket(classified);
	}

	/// <summary>
	/// Fetches resource usage by agents attachmetns
	/// </summary>
	/// <param name="callback">Called when the requested information is collected</param>
	public void GetAttachmentResources(final EventObserver<AttachmentResourcesCallbackArg> callback)
	{
		try
		{
			URI url = Client.network.getCurrentSim().Caps.CapabilityURI("AttachmentResources");
			CapsHttpClient request = new CapsHttpClient(url);
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
					{
				public void handleEvent(Observable arg0, CapsHttpRequestCompletedArg arg1) {
					//			System.out.println("RequestCompletedObserver called ...");
					CapsHttpRequestCompletedArg rcha = (CapsHttpRequestCompletedArg) arg1;
					try
					{
						if (rcha.getResult() == null || rcha.getError() != null)
						{
							callback.handleEvent(null, new AttachmentResourcesCallbackArg(false, null));
						}
						AttachmentResourcesMessage info = AttachmentResourcesMessage.FromOSD(rcha.getResult());
						callback.handleEvent(null, new AttachmentResourcesCallbackArg(true, info));

					}
					catch (Exception ex)
					{
						JLogger.error("Failed fetching AttachmentResources" + Utils.getExceptionStackTraceAsString(ex));
						callback.handleEvent(null, new AttachmentResourcesCallbackArg(false, null));
					}
				}	
					}
					);
			//			request.OnComplete += delegate(CapsHttpClient client, OSD result, Exception error)
			//					{
			//				try
			//				{
			//					if (result == null || error != null)
			//					{
			//						callback(false, null);
			//					}
			//					AttachmentResourcesMessage info = AttachmentResourcesMessage.FromOSD(result);
			//					callback(true, info);
			//
			//				}
			//				catch (Exception ex)
			//				{
			//					JLogger.error("Failed fetching AttachmentResources" + Utils.getExceptionStackTraceAsString(ex));
			//					callback(false, null);
			//				}
			//					};

			request.BeginGetResponse(Client.settings.CAPS_TIMEOUT);
		}
		catch (Exception ex)
		{
			JLogger.error("Failed fetching AttachmentResources" + Utils.getExceptionStackTraceAsString(ex));
			callback.handleEvent(null, new AttachmentResourcesCallbackArg(false, null));
		}
	}

	/// <summary>
	/// Initates request to set a new display name
	/// </summary>
	/// <param name="oldName">Previous display name</param>
	/// <param name="newName">Desired new display name</param>
	public void SetDisplayName(String oldName, String newName) throws Exception
	{
		URI uri;

		if (Client.network.getCurrentSim() == null ||
				Client.network.getCurrentSim().Caps == null ||
				(uri = Client.network.getCurrentSim().Caps.CapabilityURI("SetDisplayName")) == null)
		{
			JLogger.warn("Unable to invoke SetDisplyName capability at this time");
			return;
		}

		SetDisplayNameMessage msg = new SetDisplayNameMessage();
		msg.OldDisplayName = oldName;
		msg.NewDisplayName = newName;

		CapsHttpClient cap = new CapsHttpClient(uri);
		cap.BeginGetResponse(msg.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
	}

	/// <summary>
	/// Tells the sim what UI language is used, and if it's ok to share that with scripts
	/// </summary>
	/// <param name="language">Two letter language code</param>
	/// <param name="isPublic">Share language info with scripts</param>
	public void UpdateAgentLanguage(String language, boolean isPublic)
	{
		try
		{
			UpdateAgentLanguageMessage msg = new UpdateAgentLanguageMessage();
			msg.Language = language;
			msg.LanguagePublic = isPublic;

			URI url = Client.network.getCurrentSim().Caps.CapabilityURI("UpdateAgentLanguage");
			CapsHttpClient request = new CapsHttpClient(url);
			request.BeginGetResponse(msg.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		catch (Exception ex)
		{
			JLogger.error("Failes to update agent language" + Utils.getExceptionStackTraceAsString(ex));
		}
	}
	//endregion Misc

	//region Packet Handlers

	/// <summary>
	/// Take an incoming ImprovedInstantMessage packet, auto-parse, and if
	/// OnInstantMessage is defined call that with the appropriate arguments
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void InstantMessageHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		if (packet.Type == PacketType.ImprovedInstantMessage)
		{
			ImprovedInstantMessagePacket im = (ImprovedInstantMessagePacket)packet;

			if (onIM != null)
			{
				InstantMessage message = new InstantMessage();
				message.FromAgentID = im.AgentData.AgentID;
				message.FromAgentName = Utils.bytesWithTrailingNullByteToString(im.MessageBlock.FromAgentName);
				message.ToAgentID = im.MessageBlock.ToAgentID;
				message.ParentEstateID = im.MessageBlock.ParentEstateID;
				message.RegionID = im.MessageBlock.RegionID;
				message.Position = im.MessageBlock.Position;
				message.Dialog = InstantMessageDialog.get(im.MessageBlock.Dialog);
				message.GroupIM = im.MessageBlock.FromGroup;
				message.IMSessionID = im.MessageBlock.ID;
				message.Timestamp = new Date(im.MessageBlock.Timestamp);
				message.Message = Utils.bytesWithTrailingNullByteToString(im.MessageBlock.Message);
				message.Offline = InstantMessageOnline.get((int)im.MessageBlock.Offline);
				message.BinaryBucket = im.MessageBlock.BinaryBucket;

				onIM.raiseEvent((new InstantMessageEventArgs(message, simulator)));
			}
		}
	}

	/// <summary>
	/// Take an incoming Chat packet, auto-parse, and if OnChat is defined call 
	///   that with the appropriate arguments.
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ChatHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (onChatFromSimulator != null)
		{
			Packet packet = e.getPacket();

			ChatFromSimulatorPacket chat = (ChatFromSimulatorPacket)packet;

			onChatFromSimulator.raiseEvent(new ChatEventArgs(e.getSimulator(), Utils.bytesWithTrailingNullByteToString(chat.ChatData.Message),
					ChatAudibleLevel.get(chat.ChatData.Audible),
					ChatType.get(chat.ChatData.ChatType),
					ChatSourceType.get(chat.ChatData.SourceType),
					Utils.bytesWithTrailingNullByteToString(chat.ChatData.FromName),
					chat.ChatData.SourceID,
					chat.ChatData.OwnerID,
					chat.ChatData.Position));
		}
	}

	/// <summary>
	/// Used for parsing llDialogs
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ScriptDialogHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (onScriptDialog != null)
		{
			Packet packet = e.getPacket();

			ScriptDialogPacket dialog = (ScriptDialogPacket)packet;
			List<String> buttons = new ArrayList<String>();

			for (ScriptDialogPacket.ButtonsBlock button : dialog.Buttons)
			{
				buttons.add(Utils.bytesWithTrailingNullByteToString(button.ButtonLabel));
			}

			UUID ownerID = UUID.Zero;

			if (dialog.OwnerData != null && dialog.OwnerData.length > 0)
			{
				ownerID = dialog.OwnerData[0].OwnerID;
			}

			onScriptDialog.raiseEvent(new ScriptDialogEventArgs(Utils.bytesWithTrailingNullByteToString(dialog.Data.Message),
					Utils.bytesWithTrailingNullByteToString(dialog.Data.ObjectName),
					dialog.Data.ImageID,
					dialog.Data.ObjectID,
					Utils.bytesWithTrailingNullByteToString(dialog.Data.FirstName),
					Utils.bytesWithTrailingNullByteToString(dialog.Data.LastName),
					dialog.Data.ChatChannel,
					buttons,
					ownerID));
		}
	}

	/// <summary>
	/// Used for parsing llRequestPermissions dialogs
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ScriptQuestionHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (onScriptQuestion != null)
		{
			Packet packet = e.getPacket();
			Simulator simulator = e.getSimulator();

			ScriptQuestionPacket question = (ScriptQuestionPacket)packet;

			onScriptQuestion.raiseEvent(new ScriptQuestionEventArgs(simulator,
					question.Data.TaskID,
					question.Data.ItemID,
					Utils.bytesWithTrailingNullByteToString(question.Data.ObjectName),
					Utils.bytesWithTrailingNullByteToString(question.Data.ObjectOwner),
					ScriptPermission.get(question.Data.Questions)));
		}
	}

	/// <summary>
	/// Handles Script Control changes when Script with permissions releases or takes a control
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	private void ScriptControlChangeHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (onOnScriptControlChange != null)
		{
			Packet packet = e.getPacket();

			ScriptControlChangePacket change = (ScriptControlChangePacket)packet;
			for (int i = 0; i < change.Data.length; i++)
			{
				onOnScriptControlChange.raiseEvent(new ScriptControlEventArgs(ScriptControlChange.get(change.Data[i].Controls),
						change.Data[i].PassToAgent,
						change.Data[i].TakeControls));
			}
		}
	}

	/// <summary>
	/// Used for parsing llLoadURL Dialogs
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void LoadURLHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{

		if (onLoadURL != null)
		{
			Packet packet = e.getPacket();

			LoadURLPacket loadURL = (LoadURLPacket)packet;

			onLoadURL.raiseEvent(new LoadUrlEventArgs(
					Utils.bytesWithTrailingNullByteToString(loadURL.Data.ObjectName),
					loadURL.Data.ObjectID,
					loadURL.Data.OwnerID,
					loadURL.Data.OwnerIsGroup,
					Utils.bytesWithTrailingNullByteToString(loadURL.Data.Message),
					Utils.bytesWithTrailingNullByteToString(loadURL.Data.URL)
					));
		}
	}

	/// <summary>
	/// Update client's Position, LookAt and region handle from incoming packet
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	/// <remarks>This occurs when after an avatar moves into a new sim</remarks>
	private void MovementCompleteHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		AgentMovementCompletePacket movement = (AgentMovementCompletePacket)packet;

		relativePosition = movement.Data.Position;
		Movement.Camera.LookDirection(movement.Data.LookAt);
		simulator.Handle = movement.Data.RegionHandle;
		simulator.SimVersion = Utils.bytesWithTrailingNullByteToString(movement.SimData.ChannelVersion);
		simulator.AgentMovementComplete = true;
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void HealthHandler(Object sender, PacketReceivedEventArgs e)
	{
		Packet packet = e.getPacket();
		health = ((HealthMessagePacket)packet).HealthData.Health;
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void AgentDataUpdateHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		AgentDataUpdatePacket p = (AgentDataUpdatePacket)packet;

		if (p.AgentData.AgentID.equals(simulator.Client.self.getAgentID()))
		{
			firstName = Utils.bytesWithTrailingNullByteToString(p.AgentData.FirstName);
			lastName = Utils.bytesWithTrailingNullByteToString(p.AgentData.LastName);
			activeGroup = p.AgentData.ActiveGroupID;
			activeGroupPowers = GroupPowers.get(p.AgentData.GroupPowers.longValue());

			if (onAgentDataReply != null)
			{
				String groupTitle = Utils.bytesWithTrailingNullByteToString(p.AgentData.GroupTitle);
				String groupName = Utils.bytesWithTrailingNullByteToString(p.AgentData.GroupName);

				onAgentDataReply.raiseEvent(new AgentDataReplyEventArgs(firstName, lastName, activeGroup, groupTitle, activeGroupPowers, groupName));
			}
		}
		else
		{
			JLogger.error("Got an AgentDataUpdate packet for avatar " + p.AgentData.AgentID.toString() +
					" instead of " + Client.self.getAgentID().toString() + ", this shouldn't happen");
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void MoneyBalanceReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();

		if (packet.Type == PacketType.MoneyBalanceReply)
		{
			MoneyBalanceReplyPacket reply = (MoneyBalanceReplyPacket)packet;
			this.balance = reply.MoneyData.MoneyBalance;

			if (onMoneyBalanceReply != null)
			{
				TransactionInfo transactionInfo = new TransactionInfo();
				transactionInfo.TransactionType = reply.TransactionInfo.TransactionType;
				transactionInfo.SourceID = reply.TransactionInfo.SourceID;
				transactionInfo.IsSourceGroup = reply.TransactionInfo.IsSourceGroup;
				transactionInfo.DestID = reply.TransactionInfo.DestID;
				transactionInfo.IsDestGroup = reply.TransactionInfo.IsDestGroup;
				transactionInfo.Amount = reply.TransactionInfo.Amount;
				transactionInfo.ItemDescription =  Utils.bytesWithTrailingNullByteToString(reply.TransactionInfo.ItemDescription);

				onMoneyBalanceReply.raiseEvent(new MoneyBalanceReplyEventArgs(reply.MoneyData.TransactionID,
						reply.MoneyData.TransactionSuccess,
						reply.MoneyData.MoneyBalance,
						reply.MoneyData.SquareMetersCredit,
						reply.MoneyData.SquareMetersCommitted,
						Utils.bytesWithTrailingNullByteToString(reply.MoneyData.Description),
						transactionInfo));
			}
		}

		if (onMoneyBalance != null)
		{
			onMoneyBalance.raiseEvent(new BalanceEventArgs(balance));
		}
	}

	/// <summary>
	/// EQ Message fired with the result of SetDisplayName request
	/// </summary>
	/// <param name="capsKey">The message key</param>
	/// <param name="message">the IMessage object containing the deserialized data sent from the simulator</param>
	/// <param name="simulator">The <see cref="Simulator"/> which originated the packet</param>
	protected void SetDisplayNameReplyEventHandler(String capsKey, IMessage message, Simulator simulator)
	{
		if (onSetDisplayNameReply != null)
		{
			SetDisplayNameReplyMessage msg = (SetDisplayNameReplyMessage)message;
			onSetDisplayNameReply.raiseEvent(new SetDisplayNameReplyEventArgs(msg.Status, msg.Reason, msg.DisplayName));
		}
	}

	protected void EstablishAgentCommunicationEventHandler(String capsKey, IMessage message, Simulator simulator) throws Exception
	{
		EstablishAgentCommunicationMessage msg = (EstablishAgentCommunicationMessage)message;

		if (Client.settings.MULTIPLE_SIMS)
		{

			InetSocketAddress endPoint = new InetSocketAddress(msg.Address, msg.Port);
			Simulator sim = Client.network.FindSimulator(endPoint);

			if (sim == null)
			{
				JLogger.error("Got EstablishAgentCommunication for unknown sim " + msg.Address + ":" + msg.Port);

				// FIXME: Should we use this opportunity to connect to the simulator?
			}
			else
			{
				JLogger.info("Got EstablishAgentCommunication for " + sim.toString());

				sim.SetSeedCaps(msg.SeedCapability.toString());
			}
		}
	}

	/// <summary>
	/// Process TeleportFailed message sent via EventQueue, informs agent its last teleport has failed and why.
	/// </summary>
	/// <param name="messageKey">The Message Key</param>
	/// <param name="message">An IMessage object Deserialized from the recieved message event</param>
	/// <param name="simulator">The simulator originating the event message</param>
	public void TeleportFailedEventHandler(String messageKey, IMessage message, Simulator simulator) throws UnknownHostException, Exception
	{
		TeleportFailedMessage msg = (TeleportFailedMessage)message;

		TeleportFailedPacket failedPacket = new TeleportFailedPacket();
		failedPacket.Info.AgentID = msg.AgentID;
		failedPacket.Info.Reason = Utils.stringToBytesWithTrailingNullByte(msg.Reason);

		TeleportHandler(this, new PacketReceivedEventArgs(failedPacket, simulator));
	}

	/// <summary>
	/// Process TeleportFinish from Event Queue and pass it onto our TeleportHandler
	/// </summary>
	/// <param name="capsKey">The message system key for this event</param>
	/// <param name="message">IMessage object containing decoded data from OSD</param>
	/// <param name="simulator">The simulator originating the event message</param>
	private void TeleportFinishEventHandler(String capsKey, IMessage message, Simulator simulator) throws UnknownHostException, Exception
	{
		TeleportFinishMessage msg = (TeleportFinishMessage)message;

		TeleportFinishPacket p = new TeleportFinishPacket();
		p.Info.AgentID = msg.AgentID;
		p.Info.LocationID = msg.LocationID;
		p.Info.RegionHandle = msg.RegionHandle;
		p.Info.SeedCapability = Utils.stringToBytesWithTrailingNullByte(msg.SeedCapability.toString()); // FIXME: Check This
		p.Info.SimAccess = (byte)msg.SimAccess.getIndex();
		p.Info.SimIP = Utils.IPToUInt(msg.IP);
		p.Info.SimPort = msg.Port;
		p.Info.TeleportFlags = TeleportFlags.getIndex(msg.Flags);

		// pass the packet onto the teleport handler
		TeleportHandler(this, new PacketReceivedEventArgs(p, simulator));
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void TeleportHandler(Object sender, PacketReceivedEventArgs e) throws UnknownHostException, Exception
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		boolean finished = false;
		EnumSet<TeleportFlags> flags = TeleportFlags.get(TeleportFlags.Default.getIndex());

		if (packet.Type == PacketType.TeleportStart)
		{
			TeleportStartPacket start = (TeleportStartPacket)packet;

			teleportMessage = "Teleport started";
			flags = TeleportFlags.get(start.Info.TeleportFlags);
			teleportStat = TeleportStatus.Start;

			JLogger.debug("TeleportStart received, Flags: " + flags.toString());
		}
		else if (packet.Type == PacketType.TeleportProgress)
		{
			TeleportProgressPacket progress = (TeleportProgressPacket)packet;

			teleportMessage = Utils.bytesWithTrailingNullByteToString(progress.Info.Message);
			flags = TeleportFlags.get(progress.Info.TeleportFlags);
			teleportStat = TeleportStatus.Progress;

			JLogger.debug("TeleportProgress received, Message: " + teleportMessage + ", Flags: " + flags.toString());
		}
		else if (packet.Type == PacketType.TeleportFailed)
		{
			TeleportFailedPacket failed = (TeleportFailedPacket)packet;

			teleportMessage = Utils.bytesWithTrailingNullByteToString(failed.Info.Reason);
			teleportStat = TeleportStatus.Failed;
			finished = true;

			JLogger.debug("TeleportFailed received, Reason: " + teleportMessage);
		}
		else if (packet.Type == PacketType.TeleportFinish)
		{
			TeleportFinishPacket finish = (TeleportFinishPacket)packet;

			flags = TeleportFlags.get(finish.Info.TeleportFlags);
			String seedcaps = Utils.bytesWithTrailingNullByteToString(finish.Info.SeedCapability);
			finished = true;

			JLogger.debug("TeleportFinish received, Flags: " + flags.toString());

			// Connect to the new sim
			Client.network.getCurrentSim().AgentMovementComplete = false; // we're not there anymore
			Simulator newSimulator = Client.network.Connect(Utils.UIntToIP(finish.Info.SimIP),
					finish.Info.SimPort, finish.Info.RegionHandle, true, seedcaps);

			if (newSimulator != null)
			{
				teleportMessage = "Teleport finished";
				teleportStat = TeleportStatus.Finished;

				JLogger.info("Moved to new sim " + newSimulator.toString());
			}
			else
			{
				teleportMessage = "Failed to connect to the new sim after a teleport";
				teleportStat = TeleportStatus.Failed;

				// We're going to get disconnected now
				JLogger.error(teleportMessage);
			}
		}
		else if (packet.Type == PacketType.TeleportCancel)
		{
			//TeleportCancelPacket cancel = (TeleportCancelPacket)packet;

			teleportMessage = "Cancelled";
			teleportStat = TeleportStatus.Cancelled;
			finished = true;

			JLogger.debug("TeleportCancel received from " + simulator.toString());
		}
		else if (packet.Type == PacketType.TeleportLocal)
		{
			TeleportLocalPacket local = (TeleportLocalPacket)packet;

			teleportMessage = "Teleport finished";
			flags = TeleportFlags.get(local.Info.TeleportFlags);
			teleportStat = TeleportStatus.Finished;
			relativePosition = local.Info.Position;
			Movement.Camera.LookDirection(local.Info.LookAt);
			// This field is apparently not used for anything
			//local.Info.LocationID;
			finished = true;

			JLogger.debug("TeleportLocal received, Flags: " + flags.toString());
		}

		if (onTeleportProgress != null)
		{
			onTeleportProgress.raiseEvent(new TeleportEventArgs(teleportMessage, teleportStat, flags));
		}

		if (finished) teleportEvent.set();
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void AvatarAnimationHandler(Object sender, PacketReceivedEventArgs e)
	{
		Packet packet = e.getPacket();
		AvatarAnimationPacket animation = (AvatarAnimationPacket)packet;

		if (animation.Sender.ID.equals(Client.self.getAgentID()))
		{
			synchronized (SignaledAnimations.getDictionary())
			{
				// Reset the signaled animation list
				SignaledAnimations.getDictionary().clear();

				for (int i = 0; i < animation.AnimationList.length; i++)
				{
					UUID animID = animation.AnimationList[i].AnimID;
					int sequenceID = animation.AnimationList[i].AnimSequenceID;

					// Add this animation to the list of currently signaled animations
					SignaledAnimations.add(animID,  sequenceID);

					if (i < animation.AnimationSourceList.length)
					{
						// FIXME: The server tells us which objects triggered our animations,
						// we should store this info

						//animation.AnimationSourceList[i].ObjectID
					}

					if (i < animation.PhysicalAvatarEventList.length)
					{
						// FIXME: What is this?
					}

					if (Client.settings.SEND_AGENT_UPDATES)
					{
						// We have to manually tell the server to stop playing some animations
						if (animID == Animations.STANDUP ||
								animID == Animations.PRE_JUMP ||
								animID == Animations.LAND ||
								animID == Animations.MEDIUM_LAND)
						{
							Movement.setFinishAnim(true);
							Movement.SendUpdate(true);
							Movement.setFinishAnim(false);
						}
					}
				}
			}
		}

		if (onAnimationsChanged != null)
		{
			//			ThreadPool.QueueUserWorkItem(delegate(object o)
			//					{ AnimationsChanged.raiseEvent(new AnimationsChangedEventArgs(this.SignaledAnimations)); });

			threadPool.execute(new Runnable(){
				public void run()
				{
					onAnimationsChanged.raiseEvent(new AnimationsChangedEventArgs(SignaledAnimations));
				}
			});
		}

	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void MeanCollisionAlertHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (onMeanCollision != null)
		{
			Packet packet = e.getPacket();
			MeanCollisionAlertPacket collision = (MeanCollisionAlertPacket)packet;

			for (int i = 0; i < collision.MeanCollision.length; i++)
			{
				MeanCollisionAlertPacket.MeanCollisionBlock block = collision.MeanCollision[i];

				Date time = Utils.unixTimeToDate(block.Time);
				MeanCollisionType type = MeanCollisionType.get(block.Type);

				onMeanCollision.raiseEvent(new MeanCollisionEventArgs(type, block.Perp, block.Victim, block.Mag, time));
			}
		}
	}

	private void Network_OnLoginResponse(boolean loginSuccess, boolean redirect, String message, String reason,
			LoginResponseData reply)
	{
		JLogger.debug("AgentManager: Network_OnLoginResponse: " + reply.AgentID + " : " + reply.SessionID);
		id = reply.AgentID;
		sessionID = reply.SessionID;
		secureSessionID = reply.SecureSessionID;
		firstName = reply.FirstName;
		lastName = reply.LastName;
		startLocation = reply.StartLocation;
		agentAccess = reply.AgentAccess;
		Movement.Camera.LookDirection(reply.LookAt);
		homePosition = reply.HomePosition;
		homeLookAt = reply.HomeLookAt;
	}

	private void Network_OnDisconnected(Object sender, DisconnectedEventArgs e)
	{
		// Null out the cached fullName since it can change after logging
		// in again (with a different account name or different login
		// server but using the same GridClient object
		fullName = null;
	}

	/// <summary>
	/// Crossed region handler for message that comes across the EventQueue. Sent to an agent
	/// when the agent crosses a sim border into a new region.
	/// </summary>
	/// <param name="capsKey">The message key</param>
	/// <param name="message">the IMessage object containing the deserialized data sent from the simulator</param>
	/// <param name="simulator">The <see cref="Simulator"/> which originated the packet</param>
	private void CrossedRegionEventHandler(String capsKey, IMessage message, Simulator simulator) throws Exception
	{
		CrossedRegionMessage crossed = (CrossedRegionMessage)message;

		InetSocketAddress endPoint = new InetSocketAddress(crossed.IP, crossed.Port);

		JLogger.debug("Crossed in to new region area, attempting to connect to " + endPoint.toString());

		Simulator oldSim = Client.network.getCurrentSim();
		Simulator newSim = Client.network.Connect(endPoint, crossed.RegionHandle, true, crossed.SeedCapability.toString());

		if (newSim != null)
		{
			JLogger.info("Finished crossing over in to region " + newSim.toString());
			oldSim.AgentMovementComplete = false; // We're no longer there
			if (onRegionCrossed != null)
			{
				onRegionCrossed.raiseEvent(new RegionCrossedEventArgs(oldSim, newSim));
			}
		}
		else
		{
			// The old simulator will (poorly) handle our movement still, so the connection isn't
			// completely shot yet
			JLogger.warn("Failed to connect to new region " + endPoint.toString() + " after crossing over");
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	/// <remarks>This packet is now being sent via the EventQueue</remarks>
	protected void CrossedRegionHandler(Object sender, PacketReceivedEventArgs e) throws Exception
	{
		Packet packet = e.getPacket();
		CrossedRegionPacket crossing = (CrossedRegionPacket)packet;
		String seedCap = Utils.bytesWithTrailingNullByteToString(crossing.RegionData.SeedCapability);
		InetSocketAddress endPoint = new InetSocketAddress(Utils.UIntToIP(crossing.RegionData.SimIP), crossing.RegionData.SimPort);

		JLogger.debug("Crossed in to new region area, attempting to connect to " + endPoint.toString());

		Simulator oldSim = Client.network.getCurrentSim();
		Simulator newSim = Client.network.Connect(endPoint, crossing.RegionData.RegionHandle, true, seedCap);

		if (newSim != null)
		{
			JLogger.info("Finished crossing over in to region " + newSim.toString());

			if (onRegionCrossed != null)
			{
				onRegionCrossed.raiseEvent(new RegionCrossedEventArgs(oldSim, newSim));
			}
		}
		else
		{
			// The old simulator will (poorly) handle our movement still, so the connection isn't
			// completely shot yet
			JLogger.warn("Failed to connect to new region " + endPoint.toString() + " after crossing over");
		}
	}

	/// <summary>
	/// Group Chat event handler
	/// </summary>
	/// <param name="capsKey">The capability Key</param>
	/// <param name="message">IMessage object containing decoded data from OSD</param>
	/// <param name="simulator"></param>
	protected void ChatterBoxSessionEventReplyEventHandler(String capsKey, IMessage message, Simulator simulator)
	{
		ChatterboxSessionEventReplyMessage msg = (ChatterboxSessionEventReplyMessage)message;

		if (!msg.Success)
		{
			RequestJoinGroupChat(msg.SessionID);
			JLogger.info("Attempt to send group chat to non-existant session for group " + msg.SessionID);
		}
	}

	/// <summary>
	/// Response from request to join a group chat
	/// </summary>
	/// <param name="capsKey"></param>
	/// <param name="message">IMessage object containing decoded data from OSD</param>
	/// <param name="simulator"></param>
	protected void ChatterBoxSessionStartReplyEventHandler(String capsKey, IMessage message, Simulator simulator)
	{
		ChatterBoxSessionStartReplyMessage msg = (ChatterBoxSessionStartReplyMessage)message;

		if (msg.Success)
		{
			synchronized (GroupChatSessions.getDictionary())
			{
				if (!GroupChatSessions.containsKey(msg.SessionID))
					GroupChatSessions.add(msg.SessionID, new ArrayList<ChatSessionMember>());
			}
		}

		onGroupChatJoined.raiseEvent(new GroupChatJoinedEventArgs(msg.SessionID, msg.SessionName, msg.TempSessionID, msg.Success));
	}

	private ChatSessionMember findChatSessionMember(List<ChatSessionMember> list, UUID agentID)
	{
		for(ChatSessionMember c : list)
		{
			if(c.AvatarKey.equals(agentID))
				return c;
		}
		return null;
	}

	private int findChatSessionMemberIndex(List<ChatSessionMember> list, UUID agentID)
	{
		for(int i =0; i < list.size(); i++)
		{
			ChatSessionMember c = list.get(i);
			if(c.AvatarKey.equals(agentID))
				return i;
		}
		return -1;
	}
	
	/// <summary>
	/// Someone joined or left group chat
	/// </summary>
	/// <param name="capsKey"></param>
	/// <param name="message">IMessage object containing decoded data from OSD</param>
	/// <param name="simulator"></param>
	private void ChatterBoxSessionAgentListUpdatesEventHandler(String capsKey, IMessage message, Simulator simulator) throws NotImplementedException
	{
				ChatterBoxSessionAgentListUpdatesMessage msg = (ChatterBoxSessionAgentListUpdatesMessage)message;
		
				synchronized (GroupChatSessions.getDictionary())
				{
				if (!GroupChatSessions.containsKey(msg.SessionID))
					GroupChatSessions.add(msg.SessionID, new ArrayList<ChatSessionMember>());
				}
		
				for (int i = 0; i < msg.Updates.length; i++)
				{
					ChatSessionMember fndMbr;
					synchronized (GroupChatSessions.getDictionary())
					{
						fndMbr = findChatSessionMember(GroupChatSessions.get(msg.SessionID), msg.Updates[i].AgentID);
					}
		
					if (msg.Updates[i].Transition != null)
					{
						if (msg.Updates[i].Transition.equals("ENTER"))
						{
							if (fndMbr.AvatarKey == UUID.Zero)
							{
								fndMbr = new ChatSessionMember();
								fndMbr.AvatarKey = msg.Updates[i].AgentID;
		
								synchronized (GroupChatSessions.getDictionary())
								{
								GroupChatSessions.get(msg.SessionID).add(fndMbr);
								}
		
								if (onChatSessionMemberAdded != null)
								{
									onChatSessionMemberAdded.raiseEvent(new ChatSessionMemberAddedEventArgs(msg.SessionID, fndMbr.AvatarKey));
								}
							}
						}
						else if (msg.Updates[i].Transition.equals("LEAVE"))
						{
							if (fndMbr.AvatarKey != UUID.Zero)
								synchronized (GroupChatSessions.getDictionary())
								{
								GroupChatSessions.get(msg.SessionID).remove(fndMbr);
								}
		
							if (onChatSessionMemberLeft != null)
							{
								onChatSessionMemberLeft.raiseEvent(new ChatSessionMemberLeftEventArgs(msg.SessionID, msg.Updates[i].AgentID));
							}
						}
					}
		
					// handle updates
					ChatSessionMember update_member = findChatSessionMember(GroupChatSessions.getDictionary().get(msg.SessionID),  msg.Updates[i].AgentID);
						
		
					update_member.MuteText = msg.Updates[i].MuteText;
					update_member.MuteVoice = msg.Updates[i].MuteVoice;
		
					update_member.CanVoiceChat = msg.Updates[i].CanVoiceChat;
					update_member.IsModerator = msg.Updates[i].IsModerator;
		
					// replace existing member record
					synchronized (GroupChatSessions.getDictionary())
					{
						int found = findChatSessionMemberIndex(GroupChatSessions.getDictionary().get(msg.SessionID),  msg.Updates[i].AgentID);
		
						if (found >= 0)
							GroupChatSessions.getDictionary().get(msg.SessionID).add(found, update_member);
					}
				}
	}

	/// <summary>
	/// Handle a group chat Invitation
	/// </summary>
	/// <param name="capsKey">Caps Key</param>
	/// <param name="message">IMessage object containing decoded data from OSD</param>
	/// <param name="simulator">Originating Simulator</param>
	private void ChatterBoxInvitationEventHandler(String capsKey, IMessage message, Simulator simulator)
	{
		if (onIM != null)
		{
			ChatterBoxInvitationMessage msg = (ChatterBoxInvitationMessage)message;

			//TODO: do something about invitations to voice group chat/friends conference
			//Skip for now
			if (msg.Voice) return;

			InstantMessage im = new InstantMessage();

			im.FromAgentID = msg.FromAgentID;
			im.FromAgentName = msg.FromAgentName;
			im.ToAgentID = msg.ToAgentID;
			im.ParentEstateID = msg.ParentEstateID;
			im.RegionID = msg.RegionID;
			im.Position = msg.Position;
			im.Dialog = msg.Dialog;
			im.GroupIM = msg.GroupIM;
			im.IMSessionID = msg.IMSessionID;
			im.Timestamp = msg.Timestamp;
			im.Message = msg.Message;
			im.Offline = msg.Offline;
			im.BinaryBucket = msg.BinaryBucket;
			try
			{
				ChatterBoxAcceptInvite(msg.IMSessionID);
			}
			catch (Exception ex)
			{
				JLogger.warn("Failed joining IM:" +  Utils.getExceptionStackTraceAsString(ex));
			}
			onIM.raiseEvent(new InstantMessageEventArgs(im, simulator));
		}
	}


	/// <summary>
	/// Moderate a chat session
	/// </summary>
	/// <param name="sessionID">the <see cref="UUID"/> of the session to moderate, for group chats this will be the groups UUID</param>
	/// <param name="memberID">the <see cref="UUID"/> of the avatar to moderate</param>
	/// <param name="key">Either "voice" to moderate users voice, or "text" to moderate users text session</param>
	/// <param name="moderate">true to moderate (silence user), false to allow avatar to speak</param>
	public void ModerateChatSessions(UUID sessionID, UUID memberID, String key, boolean moderate) throws Exception
	{
		if (Client.network.getCurrentSim() == null || Client.network.getCurrentSim().Caps == null)
			throw new Exception("ChatSessionRequest capability is not currently available");

		URI url = Client.network.getCurrentSim().Caps.CapabilityURI("ChatSessionRequest");

		if (url != null)
		{
			ChatSessionRequestMuteUpdate req = new ChatSessionRequestMuteUpdate();

			req.RequestKey = key;
			req.RequestValue = moderate;
			req.SessionID = sessionID;
			req.AgentID = memberID;

			CapsHttpClient request = new CapsHttpClient(url);
			request.BeginGetResponse(req.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			throw new Exception("ChatSessionRequest capability is not currently available");
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void AlertMessageHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (onAlertMessage != null)
		{
			Packet packet = e.getPacket();

			AlertMessagePacket alert = (AlertMessagePacket)packet;

			onAlertMessage.raiseEvent(new AlertMessageEventArgs(Utils.bytesWithTrailingNullByteToString(alert.AlertData.Message)));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void CameraConstraintHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (onCameraConstraint != null)
		{
			Packet packet = e.getPacket();

			CameraConstraintPacket camera = (CameraConstraintPacket)packet;
			onCameraConstraint.raiseEvent(new CameraConstraintEventArgs(camera.CameraCollidePlane.Plane));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ScriptSensorReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (onScriptSensorReply != null)
		{
			Packet packet = e.getPacket();

			ScriptSensorReplyPacket reply = (ScriptSensorReplyPacket)packet;

			for (int i = 0; i < reply.SensedData.length; i++)
			{
				ScriptSensorReplyPacket.SensedDataBlock block = reply.SensedData[i];
				ScriptSensorReplyPacket.RequesterBlock requestor = reply.Requester;

				onScriptSensorReply.raiseEvent(new ScriptSensorReplyEventArgs(requestor.SourceID, block.GroupID, Utils.bytesWithTrailingNullByteToString(block.Name),
						block.ObjectID, block.OwnerID, block.Position, block.Range, block.Rotation, ScriptSensorTypeFlags.get(block.Type), block.Velocity));
			}
		}

	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void AvatarSitResponseHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (onAvatarSitResponse != null)
		{
			Packet packet = e.getPacket();

			AvatarSitResponsePacket sit = (AvatarSitResponsePacket)packet;

			onAvatarSitResponse.raiseEvent(new AvatarSitResponseEventArgs(sit.SitObject.ID, sit.SitTransform.AutoPilot, sit.SitTransform.CameraAtOffset,
					sit.SitTransform.CameraEyeOffset, sit.SitTransform.ForceMouselook, sit.SitTransform.SitPosition,
					sit.SitTransform.SitRotation));
		}
	}

	protected void MuteListUpdateHander(Object sender, PacketReceivedEventArgs e) 
	{
				final MuteListUpdatePacket packet = (MuteListUpdatePacket)e.getPacket();
				if (packet.MuteData.AgentID != Client.self.getAgentID())
				{
					return;
				}
				threadPool.execute(new Runnable(){
					public void run()
					{
						try
						{
						final AutoResetEvent gotMuteList = new AutoResetEvent(false);
						String fileName = Utils.bytesWithTrailingNullByteToString(packet.MuteData.Filename);
						String muteList = "";
						final BigInteger[] xferID = new BigInteger[]{new BigInteger("0")};
						final byte[][] assetData = new byte[1][];
		
						EventObserver<XferReceivedEventArgs> xferCallback = new EventObserver<XferReceivedEventArgs>()
								{
									@Override
									public void handleEvent(Observable sender,
											XferReceivedEventArgs xe) {
										if (xe.getXfer().XferID.equals(xferID[0]))
											{
												assetData[0] = xe.getXfer().AssetData;
												gotMuteList.set();
											}
									}	
								};
						
					Client.assets.registerOnXferReceived(xferCallback);
						
					xferID[0] = Client.assets.RequestAssetXfer(fileName, true, false, UUID.Zero, AssetType.Unknown, true);
		
						if (gotMuteList.waitOne(60 * 1000))
						{
							muteList = Utils.bytesWithTrailingNullByteToString(assetData[0]);
		
							synchronized (MuteList.getDictionary())
							{
								MuteList.getDictionary().clear();
								for (String line : muteList.split("\n"))
								{
									if (Utils.isNullOrEmpty(line.trim())) continue;
		
									try
									{
										Pattern pattern = Pattern.compile("(?<MyteType>\\d+)\\s+(?<Key>[a-zA-Z0-9-]+)\\s+(?<Name>[^|]+)|(?<Flags>.+)");
										Matcher m;
										if ((m = pattern.matcher(line)).find())
										{
											MuteEntry me = new MuteEntry();
											me.Type = MuteType.get(Integer.parseInt(m.group(0)));
											me.ID = new UUID(m.group(1));
											me.Name = m.group(2);
											int[] flags = new int[1];
											Utils.tryParseInt(m.group(3), flags);
//											Integer.parseInt(m.Groups["Flags"].Value;
											me.Flags = MuteFlags.get(flags[0]);
											MuteList.add(String.format("%s|%s", me.ID, me.Name), me);
										}
										else
										{
											throw new IllegalArgumentException("Invalid mutelist entry line");
										}
									}
									catch (Exception ex)
									{
										JLogger.error("Failed to parse the mute list line: " + line, Utils.getExceptionStackTraceAsString(ex));
									}
								}
							}
		
							onMuteListUpdated.raiseEvent(new EventArgs());
						}
						else
						{
							JLogger.warn("Timed out waiting for mute list download");
						}
		
						Client.assets.unregisterOnXferReceived(xferCallback);
					}
					
					catch(Exception e)
					{
						JLogger.error(e);
					}
				}});
	}

	//endregion Packet Handlers
}

