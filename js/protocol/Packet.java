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
package com.ngt.jopenmetaverse.shared.protocol;

import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.logging.Logger;

import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.BinaryLLSDOSDParser;
import com.ngt.jopenmetaverse.shared.structureddata.llsd.XmlLLSDOSDParser;

public abstract class Packet
{
	protected  static Logger logger = Logger.getLogger("Packet"); 
	public final static int MTU = 1200;

	public Header header;
	public boolean HasVariableBlocks;
	public PacketType Type;
	public abstract int getLength();
	public abstract void FromBytes(byte[] bytes, int i[], int packetEnd[], byte[] zeroBuffer) throws MalformedDataException;
	public abstract void FromBytes(Header header, byte[] bytes, int i[], int packetEnd[]) throws MalformedDataException;
	public abstract byte[] ToBytes();
	public abstract byte[][] ToBytesMultiple();

	public static PacketType GetType(int id, PacketFrequency frequency)
	{
		switch (frequency)
		{
		case Low:
			switch (id)
			{
			case 1: return PacketType.TestMessage;
			case 3: return PacketType.UseCircuitCode;
			case 10: return PacketType.TelehubInfo;
			case 24: return PacketType.EconomyDataRequest;
			case 25: return PacketType.EconomyData;
			case 26: return PacketType.AvatarPickerRequest;
			case 28: return PacketType.AvatarPickerReply;
			case 29: return PacketType.PlacesQuery;
			case 30: return PacketType.PlacesReply;
			case 31: return PacketType.DirFindQuery;
			case 33: return PacketType.DirPlacesQuery;
			case 35: return PacketType.DirPlacesReply;
			case 36: return PacketType.DirPeopleReply;
			case 37: return PacketType.DirEventsReply;
			case 38: return PacketType.DirGroupsReply;
			case 39: return PacketType.DirClassifiedQuery;
			case 41: return PacketType.DirClassifiedReply;
			case 42: return PacketType.AvatarClassifiedReply;
			case 43: return PacketType.ClassifiedInfoRequest;
			case 44: return PacketType.ClassifiedInfoReply;
			case 45: return PacketType.ClassifiedInfoUpdate;
			case 46: return PacketType.ClassifiedDelete;
			case 47: return PacketType.ClassifiedGodDelete;
			case 48: return PacketType.DirLandQuery;
			case 50: return PacketType.DirLandReply;
			case 51: return PacketType.DirPopularQuery;
			case 53: return PacketType.DirPopularReply;
			case 54: return PacketType.ParcelInfoRequest;
			case 55: return PacketType.ParcelInfoReply;
			case 56: return PacketType.ParcelObjectOwnersRequest;
			case 57: return PacketType.ParcelObjectOwnersReply;
			case 58: return PacketType.GroupNoticesListRequest;
			case 59: return PacketType.GroupNoticesListReply;
			case 60: return PacketType.GroupNoticeRequest;
			case 62: return PacketType.TeleportRequest;
			case 63: return PacketType.TeleportLocationRequest;
			case 64: return PacketType.TeleportLocal;
			case 65: return PacketType.TeleportLandmarkRequest;
			case 66: return PacketType.TeleportProgress;
			case 69: return PacketType.TeleportFinish;
			case 70: return PacketType.StartLure;
			case 71: return PacketType.TeleportLureRequest;
			case 72: return PacketType.TeleportCancel;
			case 73: return PacketType.TeleportStart;
			case 74: return PacketType.TeleportFailed;
			case 75: return PacketType.Undo;
			case 76: return PacketType.Redo;
			case 77: return PacketType.UndoLand;
			case 78: return PacketType.AgentPause;
			case 79: return PacketType.AgentResume;
			case 80: return PacketType.ChatFromViewer;
			case 81: return PacketType.AgentThrottle;
			case 82: return PacketType.AgentFOV;
			case 83: return PacketType.AgentHeightWidth;
			case 84: return PacketType.AgentSetAppearance;
			case 85: return PacketType.AgentQuitCopy;
			case 86: return PacketType.ImageNotInDatabase;
			case 87: return PacketType.RebakeAvatarTextures;
			case 88: return PacketType.SetAlwaysRun;
			case 89: return PacketType.ObjectDelete;
			case 90: return PacketType.ObjectDuplicate;
			case 91: return PacketType.ObjectDuplicateOnRay;
			case 92: return PacketType.ObjectScale;
			case 93: return PacketType.ObjectRotation;
			case 94: return PacketType.ObjectFlagUpdate;
			case 95: return PacketType.ObjectClickAction;
			case 96: return PacketType.ObjectImage;
			case 97: return PacketType.ObjectMaterial;
			case 98: return PacketType.ObjectShape;
			case 99: return PacketType.ObjectExtraParams;
			case 100: return PacketType.ObjectOwner;
			case 101: return PacketType.ObjectGroup;
			case 102: return PacketType.ObjectBuy;
			case 103: return PacketType.BuyObjectInventory;
			case 104: return PacketType.DerezContainer;
			case 105: return PacketType.ObjectPermissions;
			case 106: return PacketType.ObjectSaleInfo;
			case 107: return PacketType.ObjectName;
			case 108: return PacketType.ObjectDescription;
			case 109: return PacketType.ObjectCategory;
			case 110: return PacketType.ObjectSelect;
			case 111: return PacketType.ObjectDeselect;
			case 112: return PacketType.ObjectAttach;
			case 113: return PacketType.ObjectDetach;
			case 114: return PacketType.ObjectDrop;
			case 115: return PacketType.ObjectLink;
			case 116: return PacketType.ObjectDelink;
			case 117: return PacketType.ObjectGrab;
			case 118: return PacketType.ObjectGrabUpdate;
			case 119: return PacketType.ObjectDeGrab;
			case 120: return PacketType.ObjectSpinStart;
			case 121: return PacketType.ObjectSpinUpdate;
			case 122: return PacketType.ObjectSpinStop;
			case 123: return PacketType.ObjectExportSelected;
			case 124: return PacketType.ModifyLand;
			case 125: return PacketType.VelocityInterpolateOn;
			case 126: return PacketType.VelocityInterpolateOff;
			case 127: return PacketType.StateSave;
			case 128: return PacketType.ReportAutosaveCrash;
			case 129: return PacketType.SimWideDeletes;
			case 130: return PacketType.TrackAgent;
			case 131: return PacketType.ViewerStats;
			case 132: return PacketType.ScriptAnswerYes;
			case 133: return PacketType.UserReport;
			case 134: return PacketType.AlertMessage;
			case 135: return PacketType.AgentAlertMessage;
			case 136: return PacketType.MeanCollisionAlert;
			case 137: return PacketType.ViewerFrozenMessage;
			case 138: return PacketType.HealthMessage;
			case 139: return PacketType.ChatFromSimulator;
			case 140: return PacketType.SimStats;
			case 141: return PacketType.RequestRegionInfo;
			case 142: return PacketType.RegionInfo;
			case 143: return PacketType.GodUpdateRegionInfo;
			case 148: return PacketType.RegionHandshake;
			case 149: return PacketType.RegionHandshakeReply;
			case 150: return PacketType.SimulatorViewerTimeMessage;
			case 151: return PacketType.EnableSimulator;
			case 152: return PacketType.DisableSimulator;
			case 153: return PacketType.TransferRequest;
			case 154: return PacketType.TransferInfo;
			case 155: return PacketType.TransferAbort;
			case 156: return PacketType.RequestXfer;
			case 157: return PacketType.AbortXfer;
			case 158: return PacketType.AvatarAppearance;
			case 159: return PacketType.SetFollowCamProperties;
			case 160: return PacketType.ClearFollowCamProperties;
			case 161: return PacketType.RequestPayPrice;
			case 162: return PacketType.PayPriceReply;
			case 163: return PacketType.KickUser;
			case 165: return PacketType.GodKickUser;
			case 167: return PacketType.EjectUser;
			case 168: return PacketType.FreezeUser;
			case 169: return PacketType.AvatarPropertiesRequest;
			case 171: return PacketType.AvatarPropertiesReply;
			case 172: return PacketType.AvatarInterestsReply;
			case 173: return PacketType.AvatarGroupsReply;
			case 174: return PacketType.AvatarPropertiesUpdate;
			case 175: return PacketType.AvatarInterestsUpdate;
			case 176: return PacketType.AvatarNotesReply;
			case 177: return PacketType.AvatarNotesUpdate;
			case 178: return PacketType.AvatarPicksReply;
			case 179: return PacketType.EventInfoRequest;
			case 180: return PacketType.EventInfoReply;
			case 181: return PacketType.EventNotificationAddRequest;
			case 182: return PacketType.EventNotificationRemoveRequest;
			case 183: return PacketType.EventGodDelete;
			case 184: return PacketType.PickInfoReply;
			case 185: return PacketType.PickInfoUpdate;
			case 186: return PacketType.PickDelete;
			case 187: return PacketType.PickGodDelete;
			case 188: return PacketType.ScriptQuestion;
			case 189: return PacketType.ScriptControlChange;
			case 190: return PacketType.ScriptDialog;
			case 191: return PacketType.ScriptDialogReply;
			case 192: return PacketType.ForceScriptControlRelease;
			case 193: return PacketType.RevokePermissions;
			case 194: return PacketType.LoadURL;
			case 195: return PacketType.ScriptTeleportRequest;
			case 196: return PacketType.ParcelOverlay;
			case 197: return PacketType.ParcelPropertiesRequestByID;
			case 198: return PacketType.ParcelPropertiesUpdate;
			case 199: return PacketType.ParcelReturnObjects;
			case 200: return PacketType.ParcelSetOtherCleanTime;
			case 201: return PacketType.ParcelDisableObjects;
			case 202: return PacketType.ParcelSelectObjects;
			case 203: return PacketType.EstateCovenantRequest;
			case 204: return PacketType.EstateCovenantReply;
			case 205: return PacketType.ForceObjectSelect;
			case 206: return PacketType.ParcelBuyPass;
			case 207: return PacketType.ParcelDeedToGroup;
			case 208: return PacketType.ParcelReclaim;
			case 209: return PacketType.ParcelClaim;
			case 210: return PacketType.ParcelJoin;
			case 211: return PacketType.ParcelDivide;
			case 212: return PacketType.ParcelRelease;
			case 213: return PacketType.ParcelBuy;
			case 214: return PacketType.ParcelGodForceOwner;
			case 215: return PacketType.ParcelAccessListRequest;
			case 216: return PacketType.ParcelAccessListReply;
			case 217: return PacketType.ParcelAccessListUpdate;
			case 218: return PacketType.ParcelDwellRequest;
			case 219: return PacketType.ParcelDwellReply;
			case 227: return PacketType.ParcelGodMarkAsContent;
			case 228: return PacketType.ViewerStartAuction;
			case 235: return PacketType.UUIDNameRequest;
			case 236: return PacketType.UUIDNameReply;
			case 237: return PacketType.UUIDGroupNameRequest;
			case 238: return PacketType.UUIDGroupNameReply;
			case 240: return PacketType.ChildAgentDying;
			case 241: return PacketType.ChildAgentUnknown;
			case 243: return PacketType.GetScriptRunning;
			case 244: return PacketType.ScriptRunningReply;
			case 245: return PacketType.SetScriptRunning;
			case 246: return PacketType.ScriptReset;
			case 247: return PacketType.ScriptSensorRequest;
			case 248: return PacketType.ScriptSensorReply;
			case 249: return PacketType.CompleteAgentMovement;
			case 250: return PacketType.AgentMovementComplete;
			case 252: return PacketType.LogoutRequest;
			case 253: return PacketType.LogoutReply;
			case 254: return PacketType.ImprovedInstantMessage;
			case 255: return PacketType.RetrieveInstantMessages;
			case 256: return PacketType.FindAgent;
			case 257: return PacketType.RequestGodlikePowers;
			case 258: return PacketType.GrantGodlikePowers;
			case 259: return PacketType.GodlikeMessage;
			case 260: return PacketType.EstateOwnerMessage;
			case 261: return PacketType.GenericMessage;
			case 262: return PacketType.MuteListRequest;
			case 263: return PacketType.UpdateMuteListEntry;
			case 264: return PacketType.RemoveMuteListEntry;
			case 265: return PacketType.CopyInventoryFromNotecard;
			case 266: return PacketType.UpdateInventoryItem;
			case 267: return PacketType.UpdateCreateInventoryItem;
			case 268: return PacketType.MoveInventoryItem;
			case 269: return PacketType.CopyInventoryItem;
			case 270: return PacketType.RemoveInventoryItem;
			case 271: return PacketType.ChangeInventoryItemFlags;
			case 272: return PacketType.SaveAssetIntoInventory;
			case 273: return PacketType.CreateInventoryFolder;
			case 274: return PacketType.UpdateInventoryFolder;
			case 275: return PacketType.MoveInventoryFolder;
			case 276: return PacketType.RemoveInventoryFolder;
			case 277: return PacketType.FetchInventoryDescendents;
			case 278: return PacketType.InventoryDescendents;
			case 279: return PacketType.FetchInventory;
			case 280: return PacketType.FetchInventoryReply;
			case 281: return PacketType.BulkUpdateInventory;
			case 284: return PacketType.RemoveInventoryObjects;
			case 285: return PacketType.PurgeInventoryDescendents;
			case 286: return PacketType.UpdateTaskInventory;
			case 287: return PacketType.RemoveTaskInventory;
			case 288: return PacketType.MoveTaskInventory;
			case 289: return PacketType.RequestTaskInventory;
			case 290: return PacketType.ReplyTaskInventory;
			case 291: return PacketType.DeRezObject;
			case 292: return PacketType.DeRezAck;
			case 293: return PacketType.RezObject;
			case 294: return PacketType.RezObjectFromNotecard;
			case 297: return PacketType.AcceptFriendship;
			case 298: return PacketType.DeclineFriendship;
			case 300: return PacketType.TerminateFriendship;
			case 301: return PacketType.OfferCallingCard;
			case 302: return PacketType.AcceptCallingCard;
			case 303: return PacketType.DeclineCallingCard;
			case 304: return PacketType.RezScript;
			case 305: return PacketType.CreateInventoryItem;
			case 306: return PacketType.CreateLandmarkForEvent;
			case 309: return PacketType.RegionHandleRequest;
			case 310: return PacketType.RegionIDAndHandleReply;
			case 311: return PacketType.MoneyTransferRequest;
			case 313: return PacketType.MoneyBalanceRequest;
			case 314: return PacketType.MoneyBalanceReply;
			case 315: return PacketType.RoutedMoneyBalanceReply;
			case 316: return PacketType.ActivateGestures;
			case 317: return PacketType.DeactivateGestures;
			case 318: return PacketType.MuteListUpdate;
			case 319: return PacketType.UseCachedMuteList;
			case 320: return PacketType.GrantUserRights;
			case 321: return PacketType.ChangeUserRights;
			case 322: return PacketType.OnlineNotification;
			case 323: return PacketType.OfflineNotification;
			case 324: return PacketType.SetStartLocationRequest;
			case 333: return PacketType.AssetUploadRequest;
			case 334: return PacketType.AssetUploadComplete;
			case 339: return PacketType.CreateGroupRequest;
			case 340: return PacketType.CreateGroupReply;
			case 341: return PacketType.UpdateGroupInfo;
			case 342: return PacketType.GroupRoleChanges;
			case 343: return PacketType.JoinGroupRequest;
			case 344: return PacketType.JoinGroupReply;
			case 345: return PacketType.EjectGroupMemberRequest;
			case 346: return PacketType.EjectGroupMemberReply;
			case 347: return PacketType.LeaveGroupRequest;
			case 348: return PacketType.LeaveGroupReply;
			case 349: return PacketType.InviteGroupRequest;
			case 351: return PacketType.GroupProfileRequest;
			case 352: return PacketType.GroupProfileReply;
			case 353: return PacketType.GroupAccountSummaryRequest;
			case 354: return PacketType.GroupAccountSummaryReply;
			case 355: return PacketType.GroupAccountDetailsRequest;
			case 356: return PacketType.GroupAccountDetailsReply;
			case 357: return PacketType.GroupAccountTransactionsRequest;
			case 358: return PacketType.GroupAccountTransactionsReply;
			case 359: return PacketType.GroupActiveProposalsRequest;
			case 360: return PacketType.GroupActiveProposalItemReply;
			case 361: return PacketType.GroupVoteHistoryRequest;
			case 362: return PacketType.GroupVoteHistoryItemReply;
			case 363: return PacketType.StartGroupProposal;
			case 364: return PacketType.GroupProposalBallot;
			case 366: return PacketType.GroupMembersRequest;
			case 367: return PacketType.GroupMembersReply;
			case 368: return PacketType.ActivateGroup;
			case 369: return PacketType.SetGroupContribution;
			case 370: return PacketType.SetGroupAcceptNotices;
			case 371: return PacketType.GroupRoleDataRequest;
			case 372: return PacketType.GroupRoleDataReply;
			case 373: return PacketType.GroupRoleMembersRequest;
			case 374: return PacketType.GroupRoleMembersReply;
			case 375: return PacketType.GroupTitlesRequest;
			case 376: return PacketType.GroupTitlesReply;
			case 377: return PacketType.GroupTitleUpdate;
			case 378: return PacketType.GroupRoleUpdate;
			case 379: return PacketType.LiveHelpGroupRequest;
			case 380: return PacketType.LiveHelpGroupReply;
			case 381: return PacketType.AgentWearablesRequest;
			case 382: return PacketType.AgentWearablesUpdate;
			case 383: return PacketType.AgentIsNowWearing;
			case 384: return PacketType.AgentCachedTexture;
			case 385: return PacketType.AgentCachedTextureResponse;
			case 386: return PacketType.AgentDataUpdateRequest;
			case 387: return PacketType.AgentDataUpdate;
			case 388: return PacketType.GroupDataUpdate;
			case 389: return PacketType.AgentGroupDataUpdate;
			case 390: return PacketType.AgentDropGroup;
			case 395: return PacketType.RezSingleAttachmentFromInv;
			case 396: return PacketType.RezMultipleAttachmentsFromInv;
			case 397: return PacketType.DetachAttachmentIntoInv;
			case 398: return PacketType.CreateNewOutfitAttachments;
			case 399: return PacketType.UserInfoRequest;
			case 400: return PacketType.UserInfoReply;
			case 401: return PacketType.UpdateUserInfo;
			case 403: return PacketType.InitiateDownload;
			case 405: return PacketType.MapLayerRequest;
			case 406: return PacketType.MapLayerReply;
			case 407: return PacketType.MapBlockRequest;
			case 408: return PacketType.MapNameRequest;
			case 409: return PacketType.MapBlockReply;
			case 410: return PacketType.MapItemRequest;
			case 411: return PacketType.MapItemReply;
			case 412: return PacketType.SendPostcard;
			case 419: return PacketType.ParcelMediaCommandMessage;
			case 420: return PacketType.ParcelMediaUpdate;
			case 421: return PacketType.LandStatRequest;
			case 422: return PacketType.LandStatReply;
			case 423: return PacketType.Error;
			case 424: return PacketType.ObjectIncludeInSearch;
			case 425: return PacketType.RezRestoreToWorld;
			case 426: return PacketType.LinkInventoryItem;
			case 65531: return PacketType.PacketAck;
			case 65532: return PacketType.OpenCircuit;
			case 65533: return PacketType.CloseCircuit;
			}
			break;
		case Medium:
			switch (id)
			{
			case 1: return PacketType.ObjectAdd;
			case 2: return PacketType.MultipleObjectUpdate;
			case 3: return PacketType.RequestMultipleObjects;
			case 4: return PacketType.ObjectPosition;
			case 5: return PacketType.RequestObjectPropertiesFamily;
			case 6: return PacketType.CoarseLocationUpdate;
			case 7: return PacketType.CrossedRegion;
			case 8: return PacketType.ConfirmEnableSimulator;
			case 9: return PacketType.ObjectProperties;
			case 10: return PacketType.ObjectPropertiesFamily;
			case 11: return PacketType.ParcelPropertiesRequest;
			case 13: return PacketType.AttachedSound;
			case 14: return PacketType.AttachedSoundGainChange;
			case 15: return PacketType.PreloadSound;
			case 17: return PacketType.ViewerEffect;
			}
			break;
		case High:
			switch (id)
			{
			case 1: return PacketType.StartPingCheck;
			case 2: return PacketType.CompletePingCheck;
			case 4: return PacketType.AgentUpdate;
			case 5: return PacketType.AgentAnimation;
			case 6: return PacketType.AgentRequestSit;
			case 7: return PacketType.AgentSit;
			case 8: return PacketType.RequestImage;
			case 9: return PacketType.ImageData;
			case 10: return PacketType.ImagePacket;
			case 11: return PacketType.LayerData;
			case 12: return PacketType.ObjectUpdate;
			case 13: return PacketType.ObjectUpdateCompressed;
			case 14: return PacketType.ObjectUpdateCached;
			case 15: return PacketType.ImprovedTerseObjectUpdate;
			case 16: return PacketType.KillObject;
			case 17: return PacketType.TransferPacket;
			case 18: return PacketType.SendXferPacket;
			case 19: return PacketType.ConfirmXferPacket;
			case 20: return PacketType.AvatarAnimation;
			case 21: return PacketType.AvatarSitResponse;
			case 22: return PacketType.CameraConstraint;
			case 23: return PacketType.ParcelProperties;
			case 25: return PacketType.ChildAgentUpdate;
			case 26: return PacketType.ChildAgentAlive;
			case 27: return PacketType.ChildAgentPositionUpdate;
			case 29: return PacketType.SoundTrigger;
			}
			break;
		}

		return PacketType.Default;
	}

	public static Packet BuildPacket(PacketType type)
	{
		if(type == PacketType.StartPingCheck) return new StartPingCheckPacket();
		if(type == PacketType.CompletePingCheck) return new CompletePingCheckPacket();
		if(type == PacketType.AgentUpdate) return new AgentUpdatePacket();
		if(type == PacketType.AgentAnimation) return new AgentAnimationPacket();
		if(type == PacketType.AgentRequestSit) return new AgentRequestSitPacket();
		if(type == PacketType.AgentSit) return new AgentSitPacket();
		if(type == PacketType.RequestImage) return new RequestImagePacket();
		if(type == PacketType.ImageData) return new ImageDataPacket();
		if(type == PacketType.ImagePacket) return new ImagePacketPacket();
		if(type == PacketType.LayerData) return new LayerDataPacket();
		if(type == PacketType.ObjectUpdate) return new ObjectUpdatePacket();
		if(type == PacketType.ObjectUpdateCompressed) return new ObjectUpdateCompressedPacket();
		if(type == PacketType.ObjectUpdateCached) return new ObjectUpdateCachedPacket();
		if(type == PacketType.ImprovedTerseObjectUpdate) return new ImprovedTerseObjectUpdatePacket();
		if(type == PacketType.KillObject) return new KillObjectPacket();
		if(type == PacketType.TransferPacket) return new TransferPacketPacket();
		if(type == PacketType.SendXferPacket) return new SendXferPacketPacket();
		if(type == PacketType.ConfirmXferPacket) return new ConfirmXferPacketPacket();
		if(type == PacketType.AvatarAnimation) return new AvatarAnimationPacket();
		if(type == PacketType.AvatarSitResponse) return new AvatarSitResponsePacket();
		if(type == PacketType.CameraConstraint) return new CameraConstraintPacket();
		if(type == PacketType.ParcelProperties) return new ParcelPropertiesPacket();
		if(type == PacketType.ChildAgentUpdate) return new ChildAgentUpdatePacket();
		if(type == PacketType.ChildAgentAlive) return new ChildAgentAlivePacket();
		if(type == PacketType.ChildAgentPositionUpdate) return new ChildAgentPositionUpdatePacket();
		if(type == PacketType.SoundTrigger) return new SoundTriggerPacket();
		if(type == PacketType.ObjectAdd) return new ObjectAddPacket();
		if(type == PacketType.MultipleObjectUpdate) return new MultipleObjectUpdatePacket();
		if(type == PacketType.RequestMultipleObjects) return new RequestMultipleObjectsPacket();
		if(type == PacketType.ObjectPosition) return new ObjectPositionPacket();
		if(type == PacketType.RequestObjectPropertiesFamily) return new RequestObjectPropertiesFamilyPacket();
		if(type == PacketType.CoarseLocationUpdate) return new CoarseLocationUpdatePacket();
		if(type == PacketType.CrossedRegion) return new CrossedRegionPacket();
		if(type == PacketType.ConfirmEnableSimulator) return new ConfirmEnableSimulatorPacket();
		if(type == PacketType.ObjectProperties) return new ObjectPropertiesPacket();
		if(type == PacketType.ObjectPropertiesFamily) return new ObjectPropertiesFamilyPacket();
		if(type == PacketType.ParcelPropertiesRequest) return new ParcelPropertiesRequestPacket();
		if(type == PacketType.AttachedSound) return new AttachedSoundPacket();
		if(type == PacketType.AttachedSoundGainChange) return new AttachedSoundGainChangePacket();
		if(type == PacketType.PreloadSound) return new PreloadSoundPacket();
		if(type == PacketType.ViewerEffect) return new ViewerEffectPacket();
		if(type == PacketType.TestMessage) return new TestMessagePacket();
		if(type == PacketType.UseCircuitCode) return new UseCircuitCodePacket();
		if(type == PacketType.TelehubInfo) return new TelehubInfoPacket();
		if(type == PacketType.EconomyDataRequest) return new EconomyDataRequestPacket();
		if(type == PacketType.EconomyData) return new EconomyDataPacket();
		if(type == PacketType.AvatarPickerRequest) return new AvatarPickerRequestPacket();
		if(type == PacketType.AvatarPickerReply) return new AvatarPickerReplyPacket();
		if(type == PacketType.PlacesQuery) return new PlacesQueryPacket();
		if(type == PacketType.PlacesReply) return new PlacesReplyPacket();
		if(type == PacketType.DirFindQuery) return new DirFindQueryPacket();
		if(type == PacketType.DirPlacesQuery) return new DirPlacesQueryPacket();
		if(type == PacketType.DirPlacesReply) return new DirPlacesReplyPacket();
		if(type == PacketType.DirPeopleReply) return new DirPeopleReplyPacket();
		if(type == PacketType.DirEventsReply) return new DirEventsReplyPacket();
		if(type == PacketType.DirGroupsReply) return new DirGroupsReplyPacket();
		if(type == PacketType.DirClassifiedQuery) return new DirClassifiedQueryPacket();
		if(type == PacketType.DirClassifiedReply) return new DirClassifiedReplyPacket();
		if(type == PacketType.AvatarClassifiedReply) return new AvatarClassifiedReplyPacket();
		if(type == PacketType.ClassifiedInfoRequest) return new ClassifiedInfoRequestPacket();
		if(type == PacketType.ClassifiedInfoReply) return new ClassifiedInfoReplyPacket();
		if(type == PacketType.ClassifiedInfoUpdate) return new ClassifiedInfoUpdatePacket();
		if(type == PacketType.ClassifiedDelete) return new ClassifiedDeletePacket();
		if(type == PacketType.ClassifiedGodDelete) return new ClassifiedGodDeletePacket();
		if(type == PacketType.DirLandQuery) return new DirLandQueryPacket();
		if(type == PacketType.DirLandReply) return new DirLandReplyPacket();
		if(type == PacketType.DirPopularQuery) return new DirPopularQueryPacket();
		if(type == PacketType.DirPopularReply) return new DirPopularReplyPacket();
		if(type == PacketType.ParcelInfoRequest) return new ParcelInfoRequestPacket();
		if(type == PacketType.ParcelInfoReply) return new ParcelInfoReplyPacket();
		if(type == PacketType.ParcelObjectOwnersRequest) return new ParcelObjectOwnersRequestPacket();
		if(type == PacketType.ParcelObjectOwnersReply) return new ParcelObjectOwnersReplyPacket();
		if(type == PacketType.GroupNoticesListRequest) return new GroupNoticesListRequestPacket();
		if(type == PacketType.GroupNoticesListReply) return new GroupNoticesListReplyPacket();
		if(type == PacketType.GroupNoticeRequest) return new GroupNoticeRequestPacket();
		if(type == PacketType.TeleportRequest) return new TeleportRequestPacket();
		if(type == PacketType.TeleportLocationRequest) return new TeleportLocationRequestPacket();
		if(type == PacketType.TeleportLocal) return new TeleportLocalPacket();
		if(type == PacketType.TeleportLandmarkRequest) return new TeleportLandmarkRequestPacket();
		if(type == PacketType.TeleportProgress) return new TeleportProgressPacket();
		if(type == PacketType.TeleportFinish) return new TeleportFinishPacket();
		if(type == PacketType.StartLure) return new StartLurePacket();
		if(type == PacketType.TeleportLureRequest) return new TeleportLureRequestPacket();
		if(type == PacketType.TeleportCancel) return new TeleportCancelPacket();
		if(type == PacketType.TeleportStart) return new TeleportStartPacket();
		if(type == PacketType.TeleportFailed) return new TeleportFailedPacket();
		if(type == PacketType.Undo) return new UndoPacket();
		if(type == PacketType.Redo) return new RedoPacket();
		if(type == PacketType.UndoLand) return new UndoLandPacket();
		if(type == PacketType.AgentPause) return new AgentPausePacket();
		if(type == PacketType.AgentResume) return new AgentResumePacket();
		if(type == PacketType.ChatFromViewer) return new ChatFromViewerPacket();
		if(type == PacketType.AgentThrottle) return new AgentThrottlePacket();
		if(type == PacketType.AgentFOV) return new AgentFOVPacket();
		if(type == PacketType.AgentHeightWidth) return new AgentHeightWidthPacket();
		if(type == PacketType.AgentSetAppearance) return new AgentSetAppearancePacket();
		if(type == PacketType.AgentQuitCopy) return new AgentQuitCopyPacket();
		if(type == PacketType.ImageNotInDatabase) return new ImageNotInDatabasePacket();
		if(type == PacketType.RebakeAvatarTextures) return new RebakeAvatarTexturesPacket();
		if(type == PacketType.SetAlwaysRun) return new SetAlwaysRunPacket();
		if(type == PacketType.ObjectDelete) return new ObjectDeletePacket();
		if(type == PacketType.ObjectDuplicate) return new ObjectDuplicatePacket();
		if(type == PacketType.ObjectDuplicateOnRay) return new ObjectDuplicateOnRayPacket();
		if(type == PacketType.ObjectScale) return new ObjectScalePacket();
		if(type == PacketType.ObjectRotation) return new ObjectRotationPacket();
		if(type == PacketType.ObjectFlagUpdate) return new ObjectFlagUpdatePacket();
		if(type == PacketType.ObjectClickAction) return new ObjectClickActionPacket();
		if(type == PacketType.ObjectImage) return new ObjectImagePacket();
		if(type == PacketType.ObjectMaterial) return new ObjectMaterialPacket();
		if(type == PacketType.ObjectShape) return new ObjectShapePacket();
		if(type == PacketType.ObjectExtraParams) return new ObjectExtraParamsPacket();
		if(type == PacketType.ObjectOwner) return new ObjectOwnerPacket();
		if(type == PacketType.ObjectGroup) return new ObjectGroupPacket();
		if(type == PacketType.ObjectBuy) return new ObjectBuyPacket();
		if(type == PacketType.BuyObjectInventory) return new BuyObjectInventoryPacket();
		if(type == PacketType.DerezContainer) return new DerezContainerPacket();
		if(type == PacketType.ObjectPermissions) return new ObjectPermissionsPacket();
		if(type == PacketType.ObjectSaleInfo) return new ObjectSaleInfoPacket();
		if(type == PacketType.ObjectName) return new ObjectNamePacket();
		if(type == PacketType.ObjectDescription) return new ObjectDescriptionPacket();
		if(type == PacketType.ObjectCategory) return new ObjectCategoryPacket();
		if(type == PacketType.ObjectSelect) return new ObjectSelectPacket();
		if(type == PacketType.ObjectDeselect) return new ObjectDeselectPacket();
		if(type == PacketType.ObjectAttach) return new ObjectAttachPacket();
		if(type == PacketType.ObjectDetach) return new ObjectDetachPacket();
		if(type == PacketType.ObjectDrop) return new ObjectDropPacket();
		if(type == PacketType.ObjectLink) return new ObjectLinkPacket();
		if(type == PacketType.ObjectDelink) return new ObjectDelinkPacket();
		if(type == PacketType.ObjectGrab) return new ObjectGrabPacket();
		if(type == PacketType.ObjectGrabUpdate) return new ObjectGrabUpdatePacket();
		if(type == PacketType.ObjectDeGrab) return new ObjectDeGrabPacket();
		if(type == PacketType.ObjectSpinStart) return new ObjectSpinStartPacket();
		if(type == PacketType.ObjectSpinUpdate) return new ObjectSpinUpdatePacket();
		if(type == PacketType.ObjectSpinStop) return new ObjectSpinStopPacket();
		if(type == PacketType.ObjectExportSelected) return new ObjectExportSelectedPacket();
		if(type == PacketType.ModifyLand) return new ModifyLandPacket();
		if(type == PacketType.VelocityInterpolateOn) return new VelocityInterpolateOnPacket();
		if(type == PacketType.VelocityInterpolateOff) return new VelocityInterpolateOffPacket();
		if(type == PacketType.StateSave) return new StateSavePacket();
		if(type == PacketType.ReportAutosaveCrash) return new ReportAutosaveCrashPacket();
		if(type == PacketType.SimWideDeletes) return new SimWideDeletesPacket();
		if(type == PacketType.TrackAgent) return new TrackAgentPacket();
		if(type == PacketType.ViewerStats) return new ViewerStatsPacket();
		if(type == PacketType.ScriptAnswerYes) return new ScriptAnswerYesPacket();
		if(type == PacketType.UserReport) return new UserReportPacket();
		if(type == PacketType.AlertMessage) return new AlertMessagePacket();
		if(type == PacketType.AgentAlertMessage) return new AgentAlertMessagePacket();
		if(type == PacketType.MeanCollisionAlert) return new MeanCollisionAlertPacket();
		if(type == PacketType.ViewerFrozenMessage) return new ViewerFrozenMessagePacket();
		if(type == PacketType.HealthMessage) return new HealthMessagePacket();
		if(type == PacketType.ChatFromSimulator) return new ChatFromSimulatorPacket();
		if(type == PacketType.SimStats) return new SimStatsPacket();
		if(type == PacketType.RequestRegionInfo) return new RequestRegionInfoPacket();
		if(type == PacketType.RegionInfo) return new RegionInfoPacket();
		if(type == PacketType.GodUpdateRegionInfo) return new GodUpdateRegionInfoPacket();
		if(type == PacketType.RegionHandshake) return new RegionHandshakePacket();
		if(type == PacketType.RegionHandshakeReply) return new RegionHandshakeReplyPacket();
		if(type == PacketType.SimulatorViewerTimeMessage) return new SimulatorViewerTimeMessagePacket();
		if(type == PacketType.EnableSimulator) return new EnableSimulatorPacket();
		if(type == PacketType.DisableSimulator) return new DisableSimulatorPacket();
		if(type == PacketType.TransferRequest) return new TransferRequestPacket();
		if(type == PacketType.TransferInfo) return new TransferInfoPacket();
		if(type == PacketType.TransferAbort) return new TransferAbortPacket();
		if(type == PacketType.RequestXfer) return new RequestXferPacket();
		if(type == PacketType.AbortXfer) return new AbortXferPacket();
		if(type == PacketType.AvatarAppearance) return new AvatarAppearancePacket();
		if(type == PacketType.SetFollowCamProperties) return new SetFollowCamPropertiesPacket();
		if(type == PacketType.ClearFollowCamProperties) return new ClearFollowCamPropertiesPacket();
		if(type == PacketType.RequestPayPrice) return new RequestPayPricePacket();
		if(type == PacketType.PayPriceReply) return new PayPriceReplyPacket();
		if(type == PacketType.KickUser) return new KickUserPacket();
		if(type == PacketType.GodKickUser) return new GodKickUserPacket();
		if(type == PacketType.EjectUser) return new EjectUserPacket();
		if(type == PacketType.FreezeUser) return new FreezeUserPacket();
		if(type == PacketType.AvatarPropertiesRequest) return new AvatarPropertiesRequestPacket();
		if(type == PacketType.AvatarPropertiesReply) return new AvatarPropertiesReplyPacket();
		if(type == PacketType.AvatarInterestsReply) return new AvatarInterestsReplyPacket();
		if(type == PacketType.AvatarGroupsReply) return new AvatarGroupsReplyPacket();
		if(type == PacketType.AvatarPropertiesUpdate) return new AvatarPropertiesUpdatePacket();
		if(type == PacketType.AvatarInterestsUpdate) return new AvatarInterestsUpdatePacket();
		if(type == PacketType.AvatarNotesReply) return new AvatarNotesReplyPacket();
		if(type == PacketType.AvatarNotesUpdate) return new AvatarNotesUpdatePacket();
		if(type == PacketType.AvatarPicksReply) return new AvatarPicksReplyPacket();
		if(type == PacketType.EventInfoRequest) return new EventInfoRequestPacket();
		if(type == PacketType.EventInfoReply) return new EventInfoReplyPacket();
		if(type == PacketType.EventNotificationAddRequest) return new EventNotificationAddRequestPacket();
		if(type == PacketType.EventNotificationRemoveRequest) return new EventNotificationRemoveRequestPacket();
		if(type == PacketType.EventGodDelete) return new EventGodDeletePacket();
		if(type == PacketType.PickInfoReply) return new PickInfoReplyPacket();
		if(type == PacketType.PickInfoUpdate) return new PickInfoUpdatePacket();
		if(type == PacketType.PickDelete) return new PickDeletePacket();
		if(type == PacketType.PickGodDelete) return new PickGodDeletePacket();
		if(type == PacketType.ScriptQuestion) return new ScriptQuestionPacket();
		if(type == PacketType.ScriptControlChange) return new ScriptControlChangePacket();
		if(type == PacketType.ScriptDialog) return new ScriptDialogPacket();
		if(type == PacketType.ScriptDialogReply) return new ScriptDialogReplyPacket();
		if(type == PacketType.ForceScriptControlRelease) return new ForceScriptControlReleasePacket();
		if(type == PacketType.RevokePermissions) return new RevokePermissionsPacket();
		if(type == PacketType.LoadURL) return new LoadURLPacket();
		if(type == PacketType.ScriptTeleportRequest) return new ScriptTeleportRequestPacket();
		if(type == PacketType.ParcelOverlay) return new ParcelOverlayPacket();
		if(type == PacketType.ParcelPropertiesRequestByID) return new ParcelPropertiesRequestByIDPacket();
		if(type == PacketType.ParcelPropertiesUpdate) return new ParcelPropertiesUpdatePacket();
		if(type == PacketType.ParcelReturnObjects) return new ParcelReturnObjectsPacket();
		if(type == PacketType.ParcelSetOtherCleanTime) return new ParcelSetOtherCleanTimePacket();
		if(type == PacketType.ParcelDisableObjects) return new ParcelDisableObjectsPacket();
		if(type == PacketType.ParcelSelectObjects) return new ParcelSelectObjectsPacket();
		if(type == PacketType.EstateCovenantRequest) return new EstateCovenantRequestPacket();
		if(type == PacketType.EstateCovenantReply) return new EstateCovenantReplyPacket();
		if(type == PacketType.ForceObjectSelect) return new ForceObjectSelectPacket();
		if(type == PacketType.ParcelBuyPass) return new ParcelBuyPassPacket();
		if(type == PacketType.ParcelDeedToGroup) return new ParcelDeedToGroupPacket();
		if(type == PacketType.ParcelReclaim) return new ParcelReclaimPacket();
		if(type == PacketType.ParcelClaim) return new ParcelClaimPacket();
		if(type == PacketType.ParcelJoin) return new ParcelJoinPacket();
		if(type == PacketType.ParcelDivide) return new ParcelDividePacket();
		if(type == PacketType.ParcelRelease) return new ParcelReleasePacket();
		if(type == PacketType.ParcelBuy) return new ParcelBuyPacket();
		if(type == PacketType.ParcelGodForceOwner) return new ParcelGodForceOwnerPacket();
		if(type == PacketType.ParcelAccessListRequest) return new ParcelAccessListRequestPacket();
		if(type == PacketType.ParcelAccessListReply) return new ParcelAccessListReplyPacket();
		if(type == PacketType.ParcelAccessListUpdate) return new ParcelAccessListUpdatePacket();
		if(type == PacketType.ParcelDwellRequest) return new ParcelDwellRequestPacket();
		if(type == PacketType.ParcelDwellReply) return new ParcelDwellReplyPacket();
		if(type == PacketType.ParcelGodMarkAsContent) return new ParcelGodMarkAsContentPacket();
		if(type == PacketType.ViewerStartAuction) return new ViewerStartAuctionPacket();
		if(type == PacketType.UUIDNameRequest) return new UUIDNameRequestPacket();
		if(type == PacketType.UUIDNameReply) return new UUIDNameReplyPacket();
		if(type == PacketType.UUIDGroupNameRequest) return new UUIDGroupNameRequestPacket();
		if(type == PacketType.UUIDGroupNameReply) return new UUIDGroupNameReplyPacket();
		if(type == PacketType.ChildAgentDying) return new ChildAgentDyingPacket();
		if(type == PacketType.ChildAgentUnknown) return new ChildAgentUnknownPacket();
		if(type == PacketType.GetScriptRunning) return new GetScriptRunningPacket();
		if(type == PacketType.ScriptRunningReply) return new ScriptRunningReplyPacket();
		if(type == PacketType.SetScriptRunning) return new SetScriptRunningPacket();
		if(type == PacketType.ScriptReset) return new ScriptResetPacket();
		if(type == PacketType.ScriptSensorRequest) return new ScriptSensorRequestPacket();
		if(type == PacketType.ScriptSensorReply) return new ScriptSensorReplyPacket();
		if(type == PacketType.CompleteAgentMovement) return new CompleteAgentMovementPacket();
		if(type == PacketType.AgentMovementComplete) return new AgentMovementCompletePacket();
		if(type == PacketType.LogoutRequest) return new LogoutRequestPacket();
		if(type == PacketType.LogoutReply) return new LogoutReplyPacket();
		if(type == PacketType.ImprovedInstantMessage) return new ImprovedInstantMessagePacket();
		if(type == PacketType.RetrieveInstantMessages) return new RetrieveInstantMessagesPacket();
		if(type == PacketType.FindAgent) return new FindAgentPacket();
		if(type == PacketType.RequestGodlikePowers) return new RequestGodlikePowersPacket();
		if(type == PacketType.GrantGodlikePowers) return new GrantGodlikePowersPacket();
		if(type == PacketType.GodlikeMessage) return new GodlikeMessagePacket();
		if(type == PacketType.EstateOwnerMessage) return new EstateOwnerMessagePacket();
		if(type == PacketType.GenericMessage) return new GenericMessagePacket();
		if(type == PacketType.MuteListRequest) return new MuteListRequestPacket();
		if(type == PacketType.UpdateMuteListEntry) return new UpdateMuteListEntryPacket();
		if(type == PacketType.RemoveMuteListEntry) return new RemoveMuteListEntryPacket();
		if(type == PacketType.CopyInventoryFromNotecard) return new CopyInventoryFromNotecardPacket();
		if(type == PacketType.UpdateInventoryItem) return new UpdateInventoryItemPacket();
		if(type == PacketType.UpdateCreateInventoryItem) return new UpdateCreateInventoryItemPacket();
		if(type == PacketType.MoveInventoryItem) return new MoveInventoryItemPacket();
		if(type == PacketType.CopyInventoryItem) return new CopyInventoryItemPacket();
		if(type == PacketType.RemoveInventoryItem) return new RemoveInventoryItemPacket();
		if(type == PacketType.ChangeInventoryItemFlags) return new ChangeInventoryItemFlagsPacket();
		if(type == PacketType.SaveAssetIntoInventory) return new SaveAssetIntoInventoryPacket();
		if(type == PacketType.CreateInventoryFolder) return new CreateInventoryFolderPacket();
		if(type == PacketType.UpdateInventoryFolder) return new UpdateInventoryFolderPacket();
		if(type == PacketType.MoveInventoryFolder) return new MoveInventoryFolderPacket();
		if(type == PacketType.RemoveInventoryFolder) return new RemoveInventoryFolderPacket();
		if(type == PacketType.FetchInventoryDescendents) return new FetchInventoryDescendentsPacket();
		if(type == PacketType.InventoryDescendents) return new InventoryDescendentsPacket();
		if(type == PacketType.FetchInventory) return new FetchInventoryPacket();
		if(type == PacketType.FetchInventoryReply) return new FetchInventoryReplyPacket();
		if(type == PacketType.BulkUpdateInventory) return new BulkUpdateInventoryPacket();
		if(type == PacketType.RemoveInventoryObjects) return new RemoveInventoryObjectsPacket();
		if(type == PacketType.PurgeInventoryDescendents) return new PurgeInventoryDescendentsPacket();
		if(type == PacketType.UpdateTaskInventory) return new UpdateTaskInventoryPacket();
		if(type == PacketType.RemoveTaskInventory) return new RemoveTaskInventoryPacket();
		if(type == PacketType.MoveTaskInventory) return new MoveTaskInventoryPacket();
		if(type == PacketType.RequestTaskInventory) return new RequestTaskInventoryPacket();
		if(type == PacketType.ReplyTaskInventory) return new ReplyTaskInventoryPacket();
		if(type == PacketType.DeRezObject) return new DeRezObjectPacket();
		if(type == PacketType.DeRezAck) return new DeRezAckPacket();
		if(type == PacketType.RezObject) return new RezObjectPacket();
		if(type == PacketType.RezObjectFromNotecard) return new RezObjectFromNotecardPacket();
		if(type == PacketType.AcceptFriendship) return new AcceptFriendshipPacket();
		if(type == PacketType.DeclineFriendship) return new DeclineFriendshipPacket();
		if(type == PacketType.TerminateFriendship) return new TerminateFriendshipPacket();
		if(type == PacketType.OfferCallingCard) return new OfferCallingCardPacket();
		if(type == PacketType.AcceptCallingCard) return new AcceptCallingCardPacket();
		if(type == PacketType.DeclineCallingCard) return new DeclineCallingCardPacket();
		if(type == PacketType.RezScript) return new RezScriptPacket();
		if(type == PacketType.CreateInventoryItem) return new CreateInventoryItemPacket();
		if(type == PacketType.CreateLandmarkForEvent) return new CreateLandmarkForEventPacket();
		if(type == PacketType.RegionHandleRequest) return new RegionHandleRequestPacket();
		if(type == PacketType.RegionIDAndHandleReply) return new RegionIDAndHandleReplyPacket();
		if(type == PacketType.MoneyTransferRequest) return new MoneyTransferRequestPacket();
		if(type == PacketType.MoneyBalanceRequest) return new MoneyBalanceRequestPacket();
		if(type == PacketType.MoneyBalanceReply) return new MoneyBalanceReplyPacket();
		if(type == PacketType.RoutedMoneyBalanceReply) return new RoutedMoneyBalanceReplyPacket();
		if(type == PacketType.ActivateGestures) return new ActivateGesturesPacket();
		if(type == PacketType.DeactivateGestures) return new DeactivateGesturesPacket();
		if(type == PacketType.MuteListUpdate) return new MuteListUpdatePacket();
		if(type == PacketType.UseCachedMuteList) return new UseCachedMuteListPacket();
		if(type == PacketType.GrantUserRights) return new GrantUserRightsPacket();
		if(type == PacketType.ChangeUserRights) return new ChangeUserRightsPacket();
		if(type == PacketType.OnlineNotification) return new OnlineNotificationPacket();
		if(type == PacketType.OfflineNotification) return new OfflineNotificationPacket();
		if(type == PacketType.SetStartLocationRequest) return new SetStartLocationRequestPacket();
		if(type == PacketType.AssetUploadRequest) return new AssetUploadRequestPacket();
		if(type == PacketType.AssetUploadComplete) return new AssetUploadCompletePacket();
		if(type == PacketType.CreateGroupRequest) return new CreateGroupRequestPacket();
		if(type == PacketType.CreateGroupReply) return new CreateGroupReplyPacket();
		if(type == PacketType.UpdateGroupInfo) return new UpdateGroupInfoPacket();
		if(type == PacketType.GroupRoleChanges) return new GroupRoleChangesPacket();
		if(type == PacketType.JoinGroupRequest) return new JoinGroupRequestPacket();
		if(type == PacketType.JoinGroupReply) return new JoinGroupReplyPacket();
		if(type == PacketType.EjectGroupMemberRequest) return new EjectGroupMemberRequestPacket();
		if(type == PacketType.EjectGroupMemberReply) return new EjectGroupMemberReplyPacket();
		if(type == PacketType.LeaveGroupRequest) return new LeaveGroupRequestPacket();
		if(type == PacketType.LeaveGroupReply) return new LeaveGroupReplyPacket();
		if(type == PacketType.InviteGroupRequest) return new InviteGroupRequestPacket();
		if(type == PacketType.GroupProfileRequest) return new GroupProfileRequestPacket();
		if(type == PacketType.GroupProfileReply) return new GroupProfileReplyPacket();
		if(type == PacketType.GroupAccountSummaryRequest) return new GroupAccountSummaryRequestPacket();
		if(type == PacketType.GroupAccountSummaryReply) return new GroupAccountSummaryReplyPacket();
		if(type == PacketType.GroupAccountDetailsRequest) return new GroupAccountDetailsRequestPacket();
		if(type == PacketType.GroupAccountDetailsReply) return new GroupAccountDetailsReplyPacket();
		if(type == PacketType.GroupAccountTransactionsRequest) return new GroupAccountTransactionsRequestPacket();
		if(type == PacketType.GroupAccountTransactionsReply) return new GroupAccountTransactionsReplyPacket();
		if(type == PacketType.GroupActiveProposalsRequest) return new GroupActiveProposalsRequestPacket();
		if(type == PacketType.GroupActiveProposalItemReply) return new GroupActiveProposalItemReplyPacket();
		if(type == PacketType.GroupVoteHistoryRequest) return new GroupVoteHistoryRequestPacket();
		if(type == PacketType.GroupVoteHistoryItemReply) return new GroupVoteHistoryItemReplyPacket();
		if(type == PacketType.StartGroupProposal) return new StartGroupProposalPacket();
		if(type == PacketType.GroupProposalBallot) return new GroupProposalBallotPacket();
		if(type == PacketType.GroupMembersRequest) return new GroupMembersRequestPacket();
		if(type == PacketType.GroupMembersReply) return new GroupMembersReplyPacket();
		if(type == PacketType.ActivateGroup) return new ActivateGroupPacket();
		if(type == PacketType.SetGroupContribution) return new SetGroupContributionPacket();
		if(type == PacketType.SetGroupAcceptNotices) return new SetGroupAcceptNoticesPacket();
		if(type == PacketType.GroupRoleDataRequest) return new GroupRoleDataRequestPacket();
		if(type == PacketType.GroupRoleDataReply) return new GroupRoleDataReplyPacket();
		if(type == PacketType.GroupRoleMembersRequest) return new GroupRoleMembersRequestPacket();
		if(type == PacketType.GroupRoleMembersReply) return new GroupRoleMembersReplyPacket();
		if(type == PacketType.GroupTitlesRequest) return new GroupTitlesRequestPacket();
		if(type == PacketType.GroupTitlesReply) return new GroupTitlesReplyPacket();
		if(type == PacketType.GroupTitleUpdate) return new GroupTitleUpdatePacket();
		if(type == PacketType.GroupRoleUpdate) return new GroupRoleUpdatePacket();
		if(type == PacketType.LiveHelpGroupRequest) return new LiveHelpGroupRequestPacket();
		if(type == PacketType.LiveHelpGroupReply) return new LiveHelpGroupReplyPacket();
		if(type == PacketType.AgentWearablesRequest) return new AgentWearablesRequestPacket();
		if(type == PacketType.AgentWearablesUpdate) return new AgentWearablesUpdatePacket();
		if(type == PacketType.AgentIsNowWearing) return new AgentIsNowWearingPacket();
		if(type == PacketType.AgentCachedTexture) return new AgentCachedTexturePacket();
		if(type == PacketType.AgentCachedTextureResponse) return new AgentCachedTextureResponsePacket();
		if(type == PacketType.AgentDataUpdateRequest) return new AgentDataUpdateRequestPacket();
		if(type == PacketType.AgentDataUpdate) return new AgentDataUpdatePacket();
		if(type == PacketType.GroupDataUpdate) return new GroupDataUpdatePacket();
		if(type == PacketType.AgentGroupDataUpdate) return new AgentGroupDataUpdatePacket();
		if(type == PacketType.AgentDropGroup) return new AgentDropGroupPacket();
		if(type == PacketType.RezSingleAttachmentFromInv) return new RezSingleAttachmentFromInvPacket();
		if(type == PacketType.RezMultipleAttachmentsFromInv) return new RezMultipleAttachmentsFromInvPacket();
		if(type == PacketType.DetachAttachmentIntoInv) return new DetachAttachmentIntoInvPacket();
		if(type == PacketType.CreateNewOutfitAttachments) return new CreateNewOutfitAttachmentsPacket();
		if(type == PacketType.UserInfoRequest) return new UserInfoRequestPacket();
		if(type == PacketType.UserInfoReply) return new UserInfoReplyPacket();
		if(type == PacketType.UpdateUserInfo) return new UpdateUserInfoPacket();
		if(type == PacketType.InitiateDownload) return new InitiateDownloadPacket();
		if(type == PacketType.MapLayerRequest) return new MapLayerRequestPacket();
		if(type == PacketType.MapLayerReply) return new MapLayerReplyPacket();
		if(type == PacketType.MapBlockRequest) return new MapBlockRequestPacket();
		if(type == PacketType.MapNameRequest) return new MapNameRequestPacket();
		if(type == PacketType.MapBlockReply) return new MapBlockReplyPacket();
		if(type == PacketType.MapItemRequest) return new MapItemRequestPacket();
		if(type == PacketType.MapItemReply) return new MapItemReplyPacket();
		if(type == PacketType.SendPostcard) return new SendPostcardPacket();
		if(type == PacketType.ParcelMediaCommandMessage) return new ParcelMediaCommandMessagePacket();
		if(type == PacketType.ParcelMediaUpdate) return new ParcelMediaUpdatePacket();
		if(type == PacketType.LandStatRequest) return new LandStatRequestPacket();
		if(type == PacketType.LandStatReply) return new LandStatReplyPacket();
		if(type == PacketType.Error) return new ErrorPacket();
		if(type == PacketType.ObjectIncludeInSearch) return new ObjectIncludeInSearchPacket();
		if(type == PacketType.RezRestoreToWorld) return new RezRestoreToWorldPacket();
		if(type == PacketType.LinkInventoryItem) return new LinkInventoryItemPacket();
		if(type == PacketType.PacketAck) return new PacketAckPacket();
		if(type == PacketType.OpenCircuit) return new OpenCircuitPacket();
		if(type == PacketType.CloseCircuit) return new CloseCircuitPacket();
		return null;

	}

	public static Packet BuildPacket(byte[] packetBuffer, int packetEnd[], byte[] zeroBuffer) throws MalformedDataException
	{
		byte[] bytes;
		int[] i = new int[]{0};
		Header header = Header.BuildHeader(packetBuffer, i, packetEnd);
		if (header.Zerocoded)
		{
			packetEnd[0] = Helpers.ZeroDecode(packetBuffer, packetEnd[0] + 1, zeroBuffer) - 1;
			bytes = zeroBuffer;
		}
		else
		{
			bytes = packetBuffer;
		}
		Arrays.fill(bytes, packetEnd[0] + 1, bytes.length, (byte)0x00);

		switch (header.Frequency)
		{
		case Low:
			switch (header.ID)
			{
			case 1: return new TestMessagePacket(header, bytes, i);
			case 3: return new UseCircuitCodePacket(header, bytes, i);
			case 10: return new TelehubInfoPacket(header, bytes, i);
			case 24: return new EconomyDataRequestPacket(header, bytes, i);
			case 25: return new EconomyDataPacket(header, bytes, i);
			case 26: return new AvatarPickerRequestPacket(header, bytes, i);
			case 28: return new AvatarPickerReplyPacket(header, bytes, i);
			case 29: return new PlacesQueryPacket(header, bytes, i);
			case 30: return new PlacesReplyPacket(header, bytes, i);
			case 31: return new DirFindQueryPacket(header, bytes, i);
			case 33: return new DirPlacesQueryPacket(header, bytes, i);
			case 35: return new DirPlacesReplyPacket(header, bytes, i);
			case 36: return new DirPeopleReplyPacket(header, bytes, i);
			case 37: return new DirEventsReplyPacket(header, bytes, i);
			case 38: return new DirGroupsReplyPacket(header, bytes, i);
			case 39: return new DirClassifiedQueryPacket(header, bytes, i);
			case 41: return new DirClassifiedReplyPacket(header, bytes, i);
			case 42: return new AvatarClassifiedReplyPacket(header, bytes, i);
			case 43: return new ClassifiedInfoRequestPacket(header, bytes, i);
			case 44: return new ClassifiedInfoReplyPacket(header, bytes, i);
			case 45: return new ClassifiedInfoUpdatePacket(header, bytes, i);
			case 46: return new ClassifiedDeletePacket(header, bytes, i);
			case 47: return new ClassifiedGodDeletePacket(header, bytes, i);
			case 48: return new DirLandQueryPacket(header, bytes, i);
			case 50: return new DirLandReplyPacket(header, bytes, i);
			case 51: return new DirPopularQueryPacket(header, bytes, i);
			case 53: return new DirPopularReplyPacket(header, bytes, i);
			case 54: return new ParcelInfoRequestPacket(header, bytes, i);
			case 55: return new ParcelInfoReplyPacket(header, bytes, i);
			case 56: return new ParcelObjectOwnersRequestPacket(header, bytes, i);
			case 57: return new ParcelObjectOwnersReplyPacket(header, bytes, i);
			case 58: return new GroupNoticesListRequestPacket(header, bytes, i);
			case 59: return new GroupNoticesListReplyPacket(header, bytes, i);
			case 60: return new GroupNoticeRequestPacket(header, bytes, i);
			case 62: return new TeleportRequestPacket(header, bytes, i);
			case 63: return new TeleportLocationRequestPacket(header, bytes, i);
			case 64: return new TeleportLocalPacket(header, bytes, i);
			case 65: return new TeleportLandmarkRequestPacket(header, bytes, i);
			case 66: return new TeleportProgressPacket(header, bytes, i);
			case 69: return new TeleportFinishPacket(header, bytes, i);
			case 70: return new StartLurePacket(header, bytes, i);
			case 71: return new TeleportLureRequestPacket(header, bytes, i);
			case 72: return new TeleportCancelPacket(header, bytes, i);
			case 73: return new TeleportStartPacket(header, bytes, i);
			case 74: return new TeleportFailedPacket(header, bytes, i);
			case 75: return new UndoPacket(header, bytes, i);
			case 76: return new RedoPacket(header, bytes, i);
			case 77: return new UndoLandPacket(header, bytes, i);
			case 78: return new AgentPausePacket(header, bytes, i);
			case 79: return new AgentResumePacket(header, bytes, i);
			case 80: return new ChatFromViewerPacket(header, bytes, i);
			case 81: return new AgentThrottlePacket(header, bytes, i);
			case 82: return new AgentFOVPacket(header, bytes, i);
			case 83: return new AgentHeightWidthPacket(header, bytes, i);
			case 84: return new AgentSetAppearancePacket(header, bytes, i);
			case 85: return new AgentQuitCopyPacket(header, bytes, i);
			case 86: return new ImageNotInDatabasePacket(header, bytes, i);
			case 87: return new RebakeAvatarTexturesPacket(header, bytes, i);
			case 88: return new SetAlwaysRunPacket(header, bytes, i);
			case 89: return new ObjectDeletePacket(header, bytes, i);
			case 90: return new ObjectDuplicatePacket(header, bytes, i);
			case 91: return new ObjectDuplicateOnRayPacket(header, bytes, i);
			case 92: return new ObjectScalePacket(header, bytes, i);
			case 93: return new ObjectRotationPacket(header, bytes, i);
			case 94: return new ObjectFlagUpdatePacket(header, bytes, i);
			case 95: return new ObjectClickActionPacket(header, bytes, i);
			case 96: return new ObjectImagePacket(header, bytes, i);
			case 97: return new ObjectMaterialPacket(header, bytes, i);
			case 98: return new ObjectShapePacket(header, bytes, i);
			case 99: return new ObjectExtraParamsPacket(header, bytes, i);
			case 100: return new ObjectOwnerPacket(header, bytes, i);
			case 101: return new ObjectGroupPacket(header, bytes, i);
			case 102: return new ObjectBuyPacket(header, bytes, i);
			case 103: return new BuyObjectInventoryPacket(header, bytes, i);
			case 104: return new DerezContainerPacket(header, bytes, i);
			case 105: return new ObjectPermissionsPacket(header, bytes, i);
			case 106: return new ObjectSaleInfoPacket(header, bytes, i);
			case 107: return new ObjectNamePacket(header, bytes, i);
			case 108: return new ObjectDescriptionPacket(header, bytes, i);
			case 109: return new ObjectCategoryPacket(header, bytes, i);
			case 110: return new ObjectSelectPacket(header, bytes, i);
			case 111: return new ObjectDeselectPacket(header, bytes, i);
			case 112: return new ObjectAttachPacket(header, bytes, i);
			case 113: return new ObjectDetachPacket(header, bytes, i);
			case 114: return new ObjectDropPacket(header, bytes, i);
			case 115: return new ObjectLinkPacket(header, bytes, i);
			case 116: return new ObjectDelinkPacket(header, bytes, i);
			case 117: return new ObjectGrabPacket(header, bytes, i);
			case 118: return new ObjectGrabUpdatePacket(header, bytes, i);
			case 119: return new ObjectDeGrabPacket(header, bytes, i);
			case 120: return new ObjectSpinStartPacket(header, bytes, i);
			case 121: return new ObjectSpinUpdatePacket(header, bytes, i);
			case 122: return new ObjectSpinStopPacket(header, bytes, i);
			case 123: return new ObjectExportSelectedPacket(header, bytes, i);
			case 124: return new ModifyLandPacket(header, bytes, i);
			case 125: return new VelocityInterpolateOnPacket(header, bytes, i);
			case 126: return new VelocityInterpolateOffPacket(header, bytes, i);
			case 127: return new StateSavePacket(header, bytes, i);
			case 128: return new ReportAutosaveCrashPacket(header, bytes, i);
			case 129: return new SimWideDeletesPacket(header, bytes, i);
			case 130: return new TrackAgentPacket(header, bytes, i);
			case 131: return new ViewerStatsPacket(header, bytes, i);
			case 132: return new ScriptAnswerYesPacket(header, bytes, i);
			case 133: return new UserReportPacket(header, bytes, i);
			case 134: return new AlertMessagePacket(header, bytes, i);
			case 135: return new AgentAlertMessagePacket(header, bytes, i);
			case 136: return new MeanCollisionAlertPacket(header, bytes, i);
			case 137: return new ViewerFrozenMessagePacket(header, bytes, i);
			case 138: return new HealthMessagePacket(header, bytes, i);
			case 139: return new ChatFromSimulatorPacket(header, bytes, i);
			case 140: return new SimStatsPacket(header, bytes, i);
			case 141: return new RequestRegionInfoPacket(header, bytes, i);
			case 142: return new RegionInfoPacket(header, bytes, i);
			case 143: return new GodUpdateRegionInfoPacket(header, bytes, i);
			case 148: return new RegionHandshakePacket(header, bytes, i);
			case 149: return new RegionHandshakeReplyPacket(header, bytes, i);
			case 150: return new SimulatorViewerTimeMessagePacket(header, bytes, i);
			case 151: return new EnableSimulatorPacket(header, bytes, i);
			case 152: return new DisableSimulatorPacket(header, bytes, i);
			case 153: return new TransferRequestPacket(header, bytes, i);
			case 154: return new TransferInfoPacket(header, bytes, i);
			case 155: return new TransferAbortPacket(header, bytes, i);
			case 156: return new RequestXferPacket(header, bytes, i);
			case 157: return new AbortXferPacket(header, bytes, i);
			case 158: return new AvatarAppearancePacket(header, bytes, i);
			case 159: return new SetFollowCamPropertiesPacket(header, bytes, i);
			case 160: return new ClearFollowCamPropertiesPacket(header, bytes, i);
			case 161: return new RequestPayPricePacket(header, bytes, i);
			case 162: return new PayPriceReplyPacket(header, bytes, i);
			case 163: return new KickUserPacket(header, bytes, i);
			case 165: return new GodKickUserPacket(header, bytes, i);
			case 167: return new EjectUserPacket(header, bytes, i);
			case 168: return new FreezeUserPacket(header, bytes, i);
			case 169: return new AvatarPropertiesRequestPacket(header, bytes, i);
			case 171: return new AvatarPropertiesReplyPacket(header, bytes, i);
			case 172: return new AvatarInterestsReplyPacket(header, bytes, i);
			case 173: return new AvatarGroupsReplyPacket(header, bytes, i);
			case 174: return new AvatarPropertiesUpdatePacket(header, bytes, i);
			case 175: return new AvatarInterestsUpdatePacket(header, bytes, i);
			case 176: return new AvatarNotesReplyPacket(header, bytes, i);
			case 177: return new AvatarNotesUpdatePacket(header, bytes, i);
			case 178: return new AvatarPicksReplyPacket(header, bytes, i);
			case 179: return new EventInfoRequestPacket(header, bytes, i);
			case 180: return new EventInfoReplyPacket(header, bytes, i);
			case 181: return new EventNotificationAddRequestPacket(header, bytes, i);
			case 182: return new EventNotificationRemoveRequestPacket(header, bytes, i);
			case 183: return new EventGodDeletePacket(header, bytes, i);
			case 184: return new PickInfoReplyPacket(header, bytes, i);
			case 185: return new PickInfoUpdatePacket(header, bytes, i);
			case 186: return new PickDeletePacket(header, bytes, i);
			case 187: return new PickGodDeletePacket(header, bytes, i);
			case 188: return new ScriptQuestionPacket(header, bytes, i);
			case 189: return new ScriptControlChangePacket(header, bytes, i);
			case 190: return new ScriptDialogPacket(header, bytes, i);
			case 191: return new ScriptDialogReplyPacket(header, bytes, i);
			case 192: return new ForceScriptControlReleasePacket(header, bytes, i);
			case 193: return new RevokePermissionsPacket(header, bytes, i);
			case 194: return new LoadURLPacket(header, bytes, i);
			case 195: return new ScriptTeleportRequestPacket(header, bytes, i);
			case 196: return new ParcelOverlayPacket(header, bytes, i);
			case 197: return new ParcelPropertiesRequestByIDPacket(header, bytes, i);
			case 198: return new ParcelPropertiesUpdatePacket(header, bytes, i);
			case 199: return new ParcelReturnObjectsPacket(header, bytes, i);
			case 200: return new ParcelSetOtherCleanTimePacket(header, bytes, i);
			case 201: return new ParcelDisableObjectsPacket(header, bytes, i);
			case 202: return new ParcelSelectObjectsPacket(header, bytes, i);
			case 203: return new EstateCovenantRequestPacket(header, bytes, i);
			case 204: return new EstateCovenantReplyPacket(header, bytes, i);
			case 205: return new ForceObjectSelectPacket(header, bytes, i);
			case 206: return new ParcelBuyPassPacket(header, bytes, i);
			case 207: return new ParcelDeedToGroupPacket(header, bytes, i);
			case 208: return new ParcelReclaimPacket(header, bytes, i);
			case 209: return new ParcelClaimPacket(header, bytes, i);
			case 210: return new ParcelJoinPacket(header, bytes, i);
			case 211: return new ParcelDividePacket(header, bytes, i);
			case 212: return new ParcelReleasePacket(header, bytes, i);
			case 213: return new ParcelBuyPacket(header, bytes, i);
			case 214: return new ParcelGodForceOwnerPacket(header, bytes, i);
			case 215: return new ParcelAccessListRequestPacket(header, bytes, i);
			case 216: return new ParcelAccessListReplyPacket(header, bytes, i);
			case 217: return new ParcelAccessListUpdatePacket(header, bytes, i);
			case 218: return new ParcelDwellRequestPacket(header, bytes, i);
			case 219: return new ParcelDwellReplyPacket(header, bytes, i);
			case 227: return new ParcelGodMarkAsContentPacket(header, bytes, i);
			case 228: return new ViewerStartAuctionPacket(header, bytes, i);
			case 235: return new UUIDNameRequestPacket(header, bytes, i);
			case 236: return new UUIDNameReplyPacket(header, bytes, i);
			case 237: return new UUIDGroupNameRequestPacket(header, bytes, i);
			case 238: return new UUIDGroupNameReplyPacket(header, bytes, i);
			case 240: return new ChildAgentDyingPacket(header, bytes, i);
			case 241: return new ChildAgentUnknownPacket(header, bytes, i);
			case 243: return new GetScriptRunningPacket(header, bytes, i);
			case 244: return new ScriptRunningReplyPacket(header, bytes, i);
			case 245: return new SetScriptRunningPacket(header, bytes, i);
			case 246: return new ScriptResetPacket(header, bytes, i);
			case 247: return new ScriptSensorRequestPacket(header, bytes, i);
			case 248: return new ScriptSensorReplyPacket(header, bytes, i);
			case 249: return new CompleteAgentMovementPacket(header, bytes, i);
			case 250: return new AgentMovementCompletePacket(header, bytes, i);
			case 252: return new LogoutRequestPacket(header, bytes, i);
			case 253: return new LogoutReplyPacket(header, bytes, i);
			case 254: return new ImprovedInstantMessagePacket(header, bytes, i);
			case 255: return new RetrieveInstantMessagesPacket(header, bytes, i);
			case 256: return new FindAgentPacket(header, bytes, i);
			case 257: return new RequestGodlikePowersPacket(header, bytes, i);
			case 258: return new GrantGodlikePowersPacket(header, bytes, i);
			case 259: return new GodlikeMessagePacket(header, bytes, i);
			case 260: return new EstateOwnerMessagePacket(header, bytes, i);
			case 261: return new GenericMessagePacket(header, bytes, i);
			case 262: return new MuteListRequestPacket(header, bytes, i);
			case 263: return new UpdateMuteListEntryPacket(header, bytes, i);
			case 264: return new RemoveMuteListEntryPacket(header, bytes, i);
			case 265: return new CopyInventoryFromNotecardPacket(header, bytes, i);
			case 266: return new UpdateInventoryItemPacket(header, bytes, i);
			case 267: return new UpdateCreateInventoryItemPacket(header, bytes, i);
			case 268: return new MoveInventoryItemPacket(header, bytes, i);
			case 269: return new CopyInventoryItemPacket(header, bytes, i);
			case 270: return new RemoveInventoryItemPacket(header, bytes, i);
			case 271: return new ChangeInventoryItemFlagsPacket(header, bytes, i);
			case 272: return new SaveAssetIntoInventoryPacket(header, bytes, i);
			case 273: return new CreateInventoryFolderPacket(header, bytes, i);
			case 274: return new UpdateInventoryFolderPacket(header, bytes, i);
			case 275: return new MoveInventoryFolderPacket(header, bytes, i);
			case 276: return new RemoveInventoryFolderPacket(header, bytes, i);
			case 277: return new FetchInventoryDescendentsPacket(header, bytes, i);
			case 278: return new InventoryDescendentsPacket(header, bytes, i);
			case 279: return new FetchInventoryPacket(header, bytes, i);
			case 280: return new FetchInventoryReplyPacket(header, bytes, i);
			case 281: return new BulkUpdateInventoryPacket(header, bytes, i);
			case 284: return new RemoveInventoryObjectsPacket(header, bytes, i);
			case 285: return new PurgeInventoryDescendentsPacket(header, bytes, i);
			case 286: return new UpdateTaskInventoryPacket(header, bytes, i);
			case 287: return new RemoveTaskInventoryPacket(header, bytes, i);
			case 288: return new MoveTaskInventoryPacket(header, bytes, i);
			case 289: return new RequestTaskInventoryPacket(header, bytes, i);
			case 290: return new ReplyTaskInventoryPacket(header, bytes, i);
			case 291: return new DeRezObjectPacket(header, bytes, i);
			case 292: return new DeRezAckPacket(header, bytes, i);
			case 293: return new RezObjectPacket(header, bytes, i);
			case 294: return new RezObjectFromNotecardPacket(header, bytes, i);
			case 297: return new AcceptFriendshipPacket(header, bytes, i);
			case 298: return new DeclineFriendshipPacket(header, bytes, i);
			case 300: return new TerminateFriendshipPacket(header, bytes, i);
			case 301: return new OfferCallingCardPacket(header, bytes, i);
			case 302: return new AcceptCallingCardPacket(header, bytes, i);
			case 303: return new DeclineCallingCardPacket(header, bytes, i);
			case 304: return new RezScriptPacket(header, bytes, i);
			case 305: return new CreateInventoryItemPacket(header, bytes, i);
			case 306: return new CreateLandmarkForEventPacket(header, bytes, i);
			case 309: return new RegionHandleRequestPacket(header, bytes, i);
			case 310: return new RegionIDAndHandleReplyPacket(header, bytes, i);
			case 311: return new MoneyTransferRequestPacket(header, bytes, i);
			case 313: return new MoneyBalanceRequestPacket(header, bytes, i);
			case 314: return new MoneyBalanceReplyPacket(header, bytes, i);
			case 315: return new RoutedMoneyBalanceReplyPacket(header, bytes, i);
			case 316: return new ActivateGesturesPacket(header, bytes, i);
			case 317: return new DeactivateGesturesPacket(header, bytes, i);
			case 318: return new MuteListUpdatePacket(header, bytes, i);
			case 319: return new UseCachedMuteListPacket(header, bytes, i);
			case 320: return new GrantUserRightsPacket(header, bytes, i);
			case 321: return new ChangeUserRightsPacket(header, bytes, i);
			case 322: return new OnlineNotificationPacket(header, bytes, i);
			case 323: return new OfflineNotificationPacket(header, bytes, i);
			case 324: return new SetStartLocationRequestPacket(header, bytes, i);
			case 333: return new AssetUploadRequestPacket(header, bytes, i);
			case 334: return new AssetUploadCompletePacket(header, bytes, i);
			case 339: return new CreateGroupRequestPacket(header, bytes, i);
			case 340: return new CreateGroupReplyPacket(header, bytes, i);
			case 341: return new UpdateGroupInfoPacket(header, bytes, i);
			case 342: return new GroupRoleChangesPacket(header, bytes, i);
			case 343: return new JoinGroupRequestPacket(header, bytes, i);
			case 344: return new JoinGroupReplyPacket(header, bytes, i);
			case 345: return new EjectGroupMemberRequestPacket(header, bytes, i);
			case 346: return new EjectGroupMemberReplyPacket(header, bytes, i);
			case 347: return new LeaveGroupRequestPacket(header, bytes, i);
			case 348: return new LeaveGroupReplyPacket(header, bytes, i);
			case 349: return new InviteGroupRequestPacket(header, bytes, i);
			case 351: return new GroupProfileRequestPacket(header, bytes, i);
			case 352: return new GroupProfileReplyPacket(header, bytes, i);
			case 353: return new GroupAccountSummaryRequestPacket(header, bytes, i);
			case 354: return new GroupAccountSummaryReplyPacket(header, bytes, i);
			case 355: return new GroupAccountDetailsRequestPacket(header, bytes, i);
			case 356: return new GroupAccountDetailsReplyPacket(header, bytes, i);
			case 357: return new GroupAccountTransactionsRequestPacket(header, bytes, i);
			case 358: return new GroupAccountTransactionsReplyPacket(header, bytes, i);
			case 359: return new GroupActiveProposalsRequestPacket(header, bytes, i);
			case 360: return new GroupActiveProposalItemReplyPacket(header, bytes, i);
			case 361: return new GroupVoteHistoryRequestPacket(header, bytes, i);
			case 362: return new GroupVoteHistoryItemReplyPacket(header, bytes, i);
			case 363: return new StartGroupProposalPacket(header, bytes, i);
			case 364: return new GroupProposalBallotPacket(header, bytes, i);
			case 366: return new GroupMembersRequestPacket(header, bytes, i);
			case 367: return new GroupMembersReplyPacket(header, bytes, i);
			case 368: return new ActivateGroupPacket(header, bytes, i);
			case 369: return new SetGroupContributionPacket(header, bytes, i);
			case 370: return new SetGroupAcceptNoticesPacket(header, bytes, i);
			case 371: return new GroupRoleDataRequestPacket(header, bytes, i);
			case 372: return new GroupRoleDataReplyPacket(header, bytes, i);
			case 373: return new GroupRoleMembersRequestPacket(header, bytes, i);
			case 374: return new GroupRoleMembersReplyPacket(header, bytes, i);
			case 375: return new GroupTitlesRequestPacket(header, bytes, i);
			case 376: return new GroupTitlesReplyPacket(header, bytes, i);
			case 377: return new GroupTitleUpdatePacket(header, bytes, i);
			case 378: return new GroupRoleUpdatePacket(header, bytes, i);
			case 379: return new LiveHelpGroupRequestPacket(header, bytes, i);
			case 380: return new LiveHelpGroupReplyPacket(header, bytes, i);
			case 381: return new AgentWearablesRequestPacket(header, bytes, i);
			case 382: return new AgentWearablesUpdatePacket(header, bytes, i);
			case 383: return new AgentIsNowWearingPacket(header, bytes, i);
			case 384: return new AgentCachedTexturePacket(header, bytes, i);
			case 385: return new AgentCachedTextureResponsePacket(header, bytes, i);
			case 386: return new AgentDataUpdateRequestPacket(header, bytes, i);
			case 387: return new AgentDataUpdatePacket(header, bytes, i);
			case 388: return new GroupDataUpdatePacket(header, bytes, i);
			case 389: return new AgentGroupDataUpdatePacket(header, bytes, i);
			case 390: return new AgentDropGroupPacket(header, bytes, i);
			case 395: return new RezSingleAttachmentFromInvPacket(header, bytes, i);
			case 396: return new RezMultipleAttachmentsFromInvPacket(header, bytes, i);
			case 397: return new DetachAttachmentIntoInvPacket(header, bytes, i);
			case 398: return new CreateNewOutfitAttachmentsPacket(header, bytes, i);
			case 399: return new UserInfoRequestPacket(header, bytes, i);
			case 400: return new UserInfoReplyPacket(header, bytes, i);
			case 401: return new UpdateUserInfoPacket(header, bytes, i);
			case 403: return new InitiateDownloadPacket(header, bytes, i);
			case 405: return new MapLayerRequestPacket(header, bytes, i);
			case 406: return new MapLayerReplyPacket(header, bytes, i);
			case 407: return new MapBlockRequestPacket(header, bytes, i);
			case 408: return new MapNameRequestPacket(header, bytes, i);
			case 409: return new MapBlockReplyPacket(header, bytes, i);
			case 410: return new MapItemRequestPacket(header, bytes, i);
			case 411: return new MapItemReplyPacket(header, bytes, i);
			case 412: return new SendPostcardPacket(header, bytes, i);
			case 419: return new ParcelMediaCommandMessagePacket(header, bytes, i);
			case 420: return new ParcelMediaUpdatePacket(header, bytes, i);
			case 421: return new LandStatRequestPacket(header, bytes, i);
			case 422: return new LandStatReplyPacket(header, bytes, i);
			case 423: return new ErrorPacket(header, bytes, i);
			case 424: return new ObjectIncludeInSearchPacket(header, bytes, i);
			case 425: return new RezRestoreToWorldPacket(header, bytes, i);
			case 426: return new LinkInventoryItemPacket(header, bytes, i);
			case 65531: return new PacketAckPacket(header, bytes, i);
			case 65532: return new OpenCircuitPacket(header, bytes, i);
			case 65533: return new CloseCircuitPacket(header, bytes, i);

			}
			break;
		case Medium:
			switch (header.ID)
			{
			case 1: return new ObjectAddPacket(header, bytes, i);
			case 2: return new MultipleObjectUpdatePacket(header, bytes, i);
			case 3: return new RequestMultipleObjectsPacket(header, bytes, i);
			case 4: return new ObjectPositionPacket(header, bytes, i);
			case 5: return new RequestObjectPropertiesFamilyPacket(header, bytes, i);
			case 6: return new CoarseLocationUpdatePacket(header, bytes, i);
			case 7: return new CrossedRegionPacket(header, bytes, i);
			case 8: return new ConfirmEnableSimulatorPacket(header, bytes, i);
			case 9: return new ObjectPropertiesPacket(header, bytes, i);
			case 10: return new ObjectPropertiesFamilyPacket(header, bytes, i);
			case 11: return new ParcelPropertiesRequestPacket(header, bytes, i);
			case 13: return new AttachedSoundPacket(header, bytes, i);
			case 14: return new AttachedSoundGainChangePacket(header, bytes, i);
			case 15: return new PreloadSoundPacket(header, bytes, i);
			case 17: return new ViewerEffectPacket(header, bytes, i);

			}
			break;
		case High:
			switch (header.ID)
			{
			case 1: return new StartPingCheckPacket(header, bytes, i);
			case 2: return new CompletePingCheckPacket(header, bytes, i);
			case 4: return new AgentUpdatePacket(header, bytes, i);
			case 5: return new AgentAnimationPacket(header, bytes, i);
			case 6: return new AgentRequestSitPacket(header, bytes, i);
			case 7: return new AgentSitPacket(header, bytes, i);
			case 8: return new RequestImagePacket(header, bytes, i);
			case 9: return new ImageDataPacket(header, bytes, i);
			case 10: return new ImagePacketPacket(header, bytes, i);
			case 11: return new LayerDataPacket(header, bytes, i);
			case 12: return new ObjectUpdatePacket(header, bytes, i);
			case 13: return new ObjectUpdateCompressedPacket(header, bytes, i);
			case 14: return new ObjectUpdateCachedPacket(header, bytes, i);
			case 15: return new ImprovedTerseObjectUpdatePacket(header, bytes, i);
			case 16: return new KillObjectPacket(header, bytes, i);
			case 17: return new TransferPacketPacket(header, bytes, i);
			case 18: return new SendXferPacketPacket(header, bytes, i);
			case 19: return new ConfirmXferPacketPacket(header, bytes, i);
			case 20: return new AvatarAnimationPacket(header, bytes, i);
			case 21: return new AvatarSitResponsePacket(header, bytes, i);
			case 22: return new CameraConstraintPacket(header, bytes, i);
			case 23: return new ParcelPropertiesPacket(header, bytes, i);
			case 25: return new ChildAgentUpdatePacket(header, bytes, i);
			case 26: return new ChildAgentAlivePacket(header, bytes, i);
			case 27: return new ChildAgentPositionUpdatePacket(header, bytes, i);
			case 29: return new SoundTriggerPacket(header, bytes, i);

			}
			break;
		}

		throw new MalformedDataException("Unknown packet ID " + header.Frequency + " " + header.ID);
	}

	//region Serialization/Deserialization

	public static String ToXmlString(Packet packet) throws Exception
	{
		return XmlLLSDOSDParser.SerializeLLSDXmlString(GetLLSD(packet));
	}

	public static OSD GetLLSD(Packet packet) throws IllegalArgumentException, IllegalAccessException
	{
		//TODO Need to implement
		
		return null;
		
//		Field[] fields = packet.getClass().getFields();
//        OSDMap map = new OSDMap(fields.length);      
//        for (int i = 0; i < fields.length; i++)
//        {
//            Field field = fields[i];
//        	logger.info(field.getName());
//            if (field.get(packet) instanceof Serializable)
//            {
//                if (field.get(packet) instanceof Object[])
//                {
//                	Object[] oarray = (Object[])field.get(packet);
//            		OSDArray osdArray = new OSDArray(oarray.length);
//            		for(int j=0; j< oarray.length; j++)
//            		{
//            			OSD osd = BuildLLSDBlock(oarray[j]); 
//                    	osdArray.add(osd);
//            		}
//                	map.put(field.getName(), osdArray);
//                }
//                else
//                {
//                	OSD osd = BuildLLSDBlock(field.get(packet)); 
//                	map.put(field.getName(), osd);
//                }
//            }
//            else
//            	logger.info(field.getName() + "is not serializable");
//        }
//        return map;
        
		//		OSDMap body = new OSDMap();
		//		Type type = packet.GetType();
		//
		//		foreach (FieldInfo field in type.GetFields())
		//		{
		//			if (field.IsPublic)
		//			{
		//				Type blockType = field.FieldType;
		//
		//				if (blockType.IsArray)
		//				{
		//					object blockArray = field.GetValue(packet);
		//					Array array = (Array)blockArray;
		//					OSDArray blockList = new OSDArray(array.Length);
		//					IEnumerator ie = array.GetEnumerator();
		//
		//					while (ie.MoveNext())
		//					{
		//						object block = ie.Current;
		//						blockList.Add(BuildLLSDBlock(block));
		//					}
		//
		//					body[field.Name] = blockList;
		//				}
		//				else
		//				{
		//					object block = field.GetValue(packet);
		//					body[field.Name] = BuildLLSDBlock(block);
		//				}
		//			}
		//		}
	}

	public static byte[] ToBinary(Packet packet) throws Exception
	{
		return BinaryLLSDOSDParser.SerializeLLSDBinary(GetLLSD(packet));
	}

	public static Packet FromXmlString(String xml) throws Exception
	{
		//            System.Xml.XmlTextReader reader =
		//                new System.Xml.XmlTextReader(new System.IO.MemoryStream(Utils.StringToBytes(xml)));

		return FromLLSD(XmlLLSDOSDParser.DeserializeLLSDXml(xml));
	}

	public static Packet FromLLSD(OSD osd) throws Exception
	{
		throw new Exception("Not Implemented");
//		if(osd.getType().equals(OSDType.Map))
//		{
//			OSDMap map = (OSDMap)osd;
//			if(map.containsKey("Type"))
//			{
//				//TODO Need to implement
//				try
//				{
//					int packetType = Integer.parseInt(map.get("Type").asString());
//					//Packet packet = BuildPacket(PacketType.valueOf(packetType));
//				}
//				catch(NumberFormatException e)
//				{
//
//				}
//			}
//			else
//			{
//				
//			}
//		}
//		else
//			throw new IllegalArgumentException("osd parameter should of type OSDMap");
//		return null;
	}

	//endregion Serialization/Deserialization

	/// <summary>
	/// Attempts to convert an LLSD structure to a known Packet type
	/// </summary>
	/// <param name="capsEventName">Event name, this must match an actual
	/// packet name for a Packet to be successfully built</param>
	/// <param name="body">LLSD to convert to a Packet</param>
	/// <returns>A Packet on success, otherwise null</returns>
	public static Packet BuildPacket(String capsEventName, OSDMap body) throws Exception
	{
		throw new Exception("Not Implemented");

//		Packet packet = BuildPacket(PacketType.valueOf(capsEventName));
//
//		OSD.DeserializeMembers(packet, body);
//		return packet;


		//		Assembly assembly = Assembly.GetExecutingAssembly();
		//
		//		// Check if we have a subclass of packet with the same name as this event
		//		Type type = assembly.GetType("OpenMetaverse.Packets." + capsEventName + "Packet", false);
		//		if (type == null)
		//			return null;
		//
		//		Packet packet = null;
		//
		//		try
		//		{
		//			// Create an instance of the object
		//			packet = (Packet)Activator.CreateInstance(type);
		//
		//			// Iterate over all of the fields in the packet class, looking for matches in the LLSD
		//			foreach (FieldInfo field in type.GetFields())
		//			{
		//				if (body.ContainsKey(field.Name))
		//				{
		//					Type blockType = field.FieldType;
		//
		//					if (blockType.IsArray)
		//					{
		//						OSDArray array = (OSDArray)body[field.Name];
		//						Type elementType = blockType.GetElementType();
		//						object[] blockArray = (object[])Array.CreateInstance(elementType, array.Count);
		//
		//						for (int i = 0; i < array.Count; i++)
		//						{
		//							OSDMap map = (OSDMap)array[i];
		//							blockArray[i] = ParseLLSDBlock(map, elementType);
		//						}
		//
		//						field.SetValue(packet, blockArray);
		//					}
		//					else
		//					{
		//						OSDMap map = (OSDMap)((OSDArray)body[field.Name])[0];
		//						field.SetValue(packet, ParseLLSDBlock(map, blockType));
		//					}
		//				}
		//			}
		//		}
		//		catch (Exception e)
		//		{
		//			//FIXME Logger.Log(e.Message, Helpers.LogLevel.Error, e);
		//		}
		//
		//		return packet;
	}

	//	private static Object ParseLLSDBlock(OSDMap blockData, Type blockType)
	//	{
	//		object block = Activator.CreateInstance(blockType);
	//
	//		// Iterate over each field and set the value if a match was found in the LLSD
	//		foreach (FieldInfo field in blockType.GetFields())
	//		{
	//			if (blockData.ContainsKey(field.Name))
	//			{
	//				Type fieldType = field.FieldType;
	//
	//				if (fieldType == typeof(ulong))
	//				{
	//					// ulongs come in as a byte array, convert it manually here
	//					byte[] bytes = blockData[field.Name].AsBinary();
	//					ulong value = Utils.BytesToUInt64(bytes);
	//					field.SetValue(block, value);
	//				}
	//				else if (fieldType == typeof(uint))
	//				{
	//					// uints come in as a byte array, convert it manually here
	//					byte[] bytes = blockData[field.Name].AsBinary();
	//					uint value = Utils.BytesToUInt(bytes);
	//					field.SetValue(block, value);
	//				}
	//				else if (fieldType == typeof(ushort))
	//				{
	//					// Just need a bit of manual typecasting love here
	//					field.SetValue(block, (ushort)blockData[field.Name].AsInteger());
	//				}
	//				else if (fieldType == typeof(byte))
	//				{
	//					// Just need a bit of manual typecasting love here
	//					field.SetValue(block, (byte)blockData[field.Name].AsInteger());
	//				}
	//				else if (fieldType == typeof(short))
	//				{
	//					field.SetValue(block, (short)blockData[field.Name].AsInteger());
	//				}
	//				else if (fieldType == typeof(string))
	//				{
	//					field.SetValue(block, blockData[field.Name].AsString());
	//				}
	//				else if (fieldType == typeof(bool))
	//				{
	//					field.SetValue(block, blockData[field.Name].AsBoolean());
	//				}
	//				else if (fieldType == typeof(float))
	//				{
	//					field.SetValue(block, (float)blockData[field.Name].AsReal());
	//				}
	//				else if (fieldType == typeof(double))
	//				{
	//					field.SetValue(block, blockData[field.Name].AsReal());
	//				}
	//				else if (fieldType == typeof(int))
	//				{
	//					field.SetValue(block, blockData[field.Name].AsInteger());
	//				}
	//				else if (fieldType == typeof(UUID))
	//				{
	//					field.SetValue(block, blockData[field.Name].AsUUID());
	//				}
	//				else if (fieldType == typeof(Vector3))
	//				{
	//					Vector3 vec = ((OSDArray)blockData[field.Name]).AsVector3();
	//					field.SetValue(block, vec);
	//				}
	//				else if (fieldType == typeof(Vector4))
	//				{
	//					Vector4 vec = ((OSDArray)blockData[field.Name]).AsVector4();
	//					field.SetValue(block, vec);
	//				}
	//				else if (fieldType == typeof(Quaternion))
	//				{
	//					Quaternion quat = ((OSDArray)blockData[field.Name]).AsQuaternion();
	//					field.SetValue(block, quat);
	//				}
	//			}
	//		}
	//
	//		// Additional fields come as properties, Handle those as well.
	//		foreach (PropertyInfo property in blockType.GetProperties())
	//		{
	//			if (blockData.ContainsKey(property.Name))
	//			{
	//				OSDType proptype = blockData[property.Name].Type;
	//				MethodInfo set = property.GetSetMethod();
	//
	//				if (proptype.Equals(OSDType.Binary))
	//				{
	//					set.Invoke(block, new object[] { blockData[property.Name].AsBinary() });
	//				}
	//				else
	//					set.Invoke(block, new object[] { Utils.StringToBytes(blockData[property.Name].AsString()) });
	//			}
	//		}
	//
	//		return block;
	//	}

		private static OSD BuildLLSDBlock(Object packet) throws IllegalArgumentException, IllegalAccessException
		{	
			Field[] fields = packet.getClass().getFields();

	        OSDMap map = new OSDMap(fields.length);
	        
	        for (int i = 0; i < fields.length; i++)
	        {
	        	Field field = fields[i];
	        	OSD osd = OSD.SerializeMembers(field.get(packet)); 
            	map.put(field.getName(), osd);
	        }
	        return map;
			
	//		OSDMap map = new OSDMap();
	//		Type blockType = block.GetType();
	//
	//		foreach (FieldInfo field in blockType.GetFields())
	//		{
	//			if (field.IsPublic)
	//				map[field.Name] = OSD.FromObject(field.GetValue(block));
	//		}
	//
	//		foreach (PropertyInfo property in blockType.GetProperties())
	//		{
	//			if (property.Name != "Length")
	//			{
	//				map[property.Name] = OSD.FromObject(property.GetValue(block, null));
	//			}
	//		}
	//		return map;
	//		return OSD.SerializeMembers(block);
		}
}