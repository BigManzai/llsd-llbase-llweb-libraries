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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Observable;
import java.util.Timer;
import java.util.TimerTask;

import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpClient;
import com.ngt.jopenmetaverse.shared.cap.http.CapsHttpRequestCompletedArg;
import com.ngt.jopenmetaverse.shared.exception.NotSupportedException;
import com.ngt.jopenmetaverse.shared.protocol.ImprovedTerseObjectUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.KillObjectPacket;
import com.ngt.jopenmetaverse.shared.protocol.MultipleObjectUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.NameValue;
import com.ngt.jopenmetaverse.shared.protocol.ObjectAddPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectAttachPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectBuyPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectDeGrabPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectDelinkPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectDescriptionPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectDeselectPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectDetachPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectDropPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectExtraParamsPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectFlagUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectGrabPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectGroupPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectImagePacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectLinkPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectMaterialPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectNamePacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectOwnerPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectPermissionsPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectPropertiesFamilyPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectPropertiesPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectRotationPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectSaleInfoPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectSelectPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectShapePacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectUpdateCachedPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectUpdateCompressedPacket;
import com.ngt.jopenmetaverse.shared.protocol.ObjectUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.PayPriceReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.RequestMultipleObjectsPacket;
import com.ngt.jopenmetaverse.shared.protocol.RequestObjectPropertiesFamilyPacket;
import com.ngt.jopenmetaverse.shared.protocol.RequestPayPricePacket;
import com.ngt.jopenmetaverse.shared.protocol.primitives.ConstructionData;
import com.ngt.jopenmetaverse.shared.protocol.primitives.FlexibleData;
import com.ngt.jopenmetaverse.shared.protocol.primitives.LightData;
import com.ngt.jopenmetaverse.shared.protocol.primitives.MediaEntry;
import com.ngt.jopenmetaverse.shared.protocol.primitives.ObjectProperties;
import com.ngt.jopenmetaverse.shared.protocol.primitives.ParticleSystem;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions.PermissionMask;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions.PermissionWho;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.protocol.primitives.SculptData;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureAnimation;
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntry;
import com.ngt.jopenmetaverse.shared.sim.Avatar;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventObservableArg;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.om.AvatarSitChangedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.AvatarUpdateEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.KillObjectEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.ObjectDataBlockUpdateEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.ObjectMediaCallbackArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.ObjectPropertiesEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.ObjectPropertiesFamilyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.ObjectPropertiesUpdatedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.PayPriceReplyEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.PhysicsPropertiesEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.PrimEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.om.TerseObjectUpdateEventArgs;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ObjectMediaNavigateMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.ObjectPhysicsPropertiesMessage;
import com.ngt.jopenmetaverse.shared.structureddata.OSDFormat;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.Action;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.Enums.SaleType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.AttachmentPoint;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ClickAction;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ExtraParamType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.Grass;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.JointType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.Material;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ObjectCategory;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PCode;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PathCurve;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PhysicsShapeType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PrimFlags;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.PrimType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.ProfileCurve;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.Tree;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Vector4;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// Handles all network traffic related to prims and avatar positions and 
/// movement.
/// </summary>
public class ObjectManager {
	//region Enums

	/// <summary>
	/// 
	/// </summary>
	public enum ReportType 
	{
		/// <summary>No report</summary>
		None ((long)0),
		/// <summary>Unknown report type</summary>
		Unknown ((long)1),
		/// <summary>Bug report</summary>
		Bug ((long)2),
		/// <summary>Complaint report</summary>
		Complaint ((long)3),
		/// <summary>Customer service report</summary>
		CustomerServiceRequest ((long)4);
		private long index;
		ReportType(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,ReportType> lookup  = new HashMap<Long,ReportType>();

		static {
			for(ReportType s : EnumSet.allOf(ReportType.class))
				lookup.put(s.getIndex(), s);
		}

		public static ReportType get(Long index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Bitflag field for ObjectUpdateCompressed data blocks, describing 
	/// which options are present for each object
	/// </summary>

	public enum CompressedFlags 
	{
		None ((long)0x00),
		/// <summary>Unknown</summary>
		ScratchPad ((long)0x01),
		/// <summary>Whether the object has a TreeSpecies</summary>
		Tree ((long)0x02),
		/// <summary>Whether the object has floating text ala llSetText</summary>
		HasText ((long)0x04),
		/// <summary>Whether the object has an active particle system</summary>
		HasParticles ((long)0x08),
		/// <summary>Whether the object has sound attached to it</summary>
		HasSound ((long)0x10),
		/// <summary>Whether the object is attached to a root object or not</summary>
		HasParent ((long)0x20),
		/// <summary>Whether the object has texture animation settings</summary>
		TextureAnimation ((long)0x40),
		/// <summary>Whether the object has an angular velocity</summary>
		HasAngularVelocity ((long)0x80),
		/// <summary>Whether the object has a name value pairs String</summary>
		HasNameValues ((long)0x100),
		/// <summary>Whether the object has a Media URL set</summary>
		MediaURL((long)0x200);
		private long index;
		CompressedFlags(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,CompressedFlags> lookup  = new HashMap<Long,CompressedFlags>();

		static {
			for(CompressedFlags s : EnumSet.allOf(CompressedFlags.class))
				lookup.put(s.getIndex(), s);
		}

        public static EnumSet<CompressedFlags> get(Long index)
        {
                EnumSet<CompressedFlags> enumsSet = EnumSet.allOf(CompressedFlags.class);
                for(Entry<Long,CompressedFlags> entry: lookup.entrySet())
                {
                        if((entry.getKey().longValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }

        public static long getIndex(EnumSet<CompressedFlags> enumSet)
        {
                long ret = 0;
                for(CompressedFlags s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }
	}

	/// <summary>
	/// Specific Flags for MultipleObjectUpdate requests
	/// </summary>

	public enum UpdateType
	{
		/// <summary>None</summary>
		None ((long)0x00),
		/// <summary>Change position of prims</summary>
		Position ((long)0x01),
		/// <summary>Change rotation of prims</summary>
		Rotation ((long)0x02),
		/// <summary>Change size of prims</summary>
		Scale ((long)0x04),
		/// <summary>Perform operation on link set</summary>
		Linked ((long)0x08),
		/// <summary>Scale prims uniformly, same as selecing ctrl+shift in the
		/// viewer. Used in conjunction with Scale</summary>
		Uniform ((long)0x10);
		private long index;
		UpdateType(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,UpdateType> lookup  = new HashMap<Long,UpdateType>();

		static {
			for(UpdateType s : EnumSet.allOf(UpdateType.class))
				lookup.put(s.getIndex(), s);
		}

        public static EnumSet<UpdateType> get(Long index)
        {
                EnumSet<UpdateType> enumsSet = EnumSet.allOf(UpdateType.class);
                for(Entry<Long,UpdateType> entry: lookup.entrySet())
                {
                        if((entry.getKey().longValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }

        public static long getIndex(EnumSet<UpdateType> enumSet)
        {
                long ret = 0;
                for(UpdateType s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }

	}

	/// <summary>
	/// Special values in PayPriceReply. If the price is not one of these
	/// literal value of the price should be use
	/// </summary>
	public enum PayPriceType
	{
		/// <summary>
		/// Indicates that this pay option should be hidden
		/// </summary>
		Hide(-1),

		/// <summary>
		/// Indicates that this pay option should have the default value
		/// </summary>
		Default(-2);
		private int index;
		PayPriceType(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,PayPriceType> lookup  
		= new HashMap<Integer,PayPriceType>();

		static {
			for(PayPriceType s : EnumSet.allOf(PayPriceType.class))
				lookup.put(s.getIndex(), s);
		}

		public static PayPriceType get(Integer index)
		{
			return lookup.get(index);
		}
	}

	//endregion Enums

	//region Structs

	/// <summary>
	/// Contains the variables sent in an object update packet for objects. 
	/// Used to track position and movement of prims and avatars
	/// </summary>
	public class ObjectMovementUpdate
	{
		/// <summary></summary>
		public boolean Avatar;
		/// <summary></summary>
		public Vector4 CollisionPlane;
		/// <summary></summary>
		public byte State;
		/// <summary></summary>
		//uint
		public long LocalID;
		/// <summary></summary>
		public Vector3 Position;
		/// <summary></summary>
		public Vector3 Velocity;
		/// <summary></summary>
		public Vector3 Acceleration;
		/// <summary></summary>
		public Quaternion Rotation;
		/// <summary></summary>
		public Vector3 AngularVelocity;
		/// <summary></summary>
		public TextureEntry Textures;
	}

	//endregion Structs

	public final float HAVOK_TIMESTEP = 1.0f / 45.0f;

	//region Delegates

	private EventObservable<PrimEventArgs> onObjectUpdate = new EventObservable<PrimEventArgs>();
	public void registerOnObjectUpdate(EventObserver<PrimEventArgs> o)
	{
		onObjectUpdate.addObserver(o);
	}
	public void unregisterOnObjectUpdate(EventObserver<PrimEventArgs> o) 
	{
		onObjectUpdate.addObserver(o);
	}
	private EventObservable<ObjectPropertiesEventArgs> onObjectProperties = new EventObservable<ObjectPropertiesEventArgs>();
	public void registerOnObjectProperties(EventObserver<ObjectPropertiesEventArgs> o)
	{
		onObjectProperties.addObserver(o);
	}
	public void unregisterOnObjectProperties(EventObserver<ObjectPropertiesEventArgs> o) 
	{
		onObjectProperties.addObserver(o);
	}
	private EventObservable<ObjectPropertiesUpdatedEventArgs> onObjectPropertiesUpdated = new EventObservable<ObjectPropertiesUpdatedEventArgs>();
	public void registerOnObjectPropertiesUpdated(EventObserver<ObjectPropertiesUpdatedEventArgs> o)
	{
		onObjectPropertiesUpdated.addObserver(o);
	}
	public void unregisterOnObjectPropertiesUpdated(EventObserver<ObjectPropertiesUpdatedEventArgs> o) 
	{
		onObjectPropertiesUpdated.addObserver(o);
	}
	private EventObservable<ObjectPropertiesFamilyEventArgs> onObjectPropertiesFamily = new EventObservable<ObjectPropertiesFamilyEventArgs>();
	public void registerOnObjectPropertiesFamily(EventObserver<ObjectPropertiesFamilyEventArgs> o)
	{
		onObjectPropertiesFamily.addObserver(o);
	}
	public void unregisterOnObjectPropertiesFamily(EventObserver<ObjectPropertiesFamilyEventArgs> o) 
	{
		onObjectPropertiesFamily.addObserver(o);
	}
	private EventObservable<AvatarUpdateEventArgs> onAvatarUpdate = new EventObservable<AvatarUpdateEventArgs>();
	public void registerOnAvatarUpdate(EventObserver<AvatarUpdateEventArgs> o)
	{
		onAvatarUpdate.addObserver(o);
	}
	public void unregisterOnAvatarUpdate(EventObserver<AvatarUpdateEventArgs> o) 
	{
		onAvatarUpdate.addObserver(o);
	}
	private EventObservable<TerseObjectUpdateEventArgs> onTerseObjectUpdate = new EventObservable<TerseObjectUpdateEventArgs>();
	public void registerOnTerseObjectUpdate(EventObserver<TerseObjectUpdateEventArgs> o)
	{
		onTerseObjectUpdate.addObserver(o);
	}
	public void unregisterOnTerseObjectUpdate(EventObserver<TerseObjectUpdateEventArgs> o) 
	{
		onTerseObjectUpdate.addObserver(o);
	}
	private EventObservable<ObjectDataBlockUpdateEventArgs> onObjectDataBlockUpdate = new EventObservable<ObjectDataBlockUpdateEventArgs>();
	public void registerOnObjectDataBlockUpdate(EventObserver<ObjectDataBlockUpdateEventArgs> o)
	{
		onObjectDataBlockUpdate.addObserver(o);
	}
	public void unregisterOnObjectDataBlockUpdate(EventObserver<ObjectDataBlockUpdateEventArgs> o) 
	{
		onObjectDataBlockUpdate.addObserver(o);
	}
	private EventObservable<KillObjectEventArgs> onKillObject = new EventObservable<KillObjectEventArgs>();
	public void registerOnKillObject(EventObserver<KillObjectEventArgs> o)
	{
		onKillObject.addObserver(o);
	}
	public void unregisterOnKillObject(EventObserver<KillObjectEventArgs> o) 
	{
		onKillObject.addObserver(o);
	}
	private EventObservable<AvatarSitChangedEventArgs> onAvatarSitChanged = new EventObservable<AvatarSitChangedEventArgs>();
	public void registerOnAvatarSitChanged(EventObserver<AvatarSitChangedEventArgs> o)
	{
		onAvatarSitChanged.addObserver(o);
	}
	public void unregisterOnAvatarSitChanged(EventObserver<AvatarSitChangedEventArgs> o) 
	{
		onAvatarSitChanged.addObserver(o);
	}
	private EventObservable<PayPriceReplyEventArgs> onPayPriceReply = new EventObservable<PayPriceReplyEventArgs>();
	public void registerOnPayPriceReply(EventObserver<PayPriceReplyEventArgs> o)
	{
		onPayPriceReply.addObserver(o);
	}
	public void unregisterOnPayPriceReply(EventObserver<PayPriceReplyEventArgs> o) 
	{
		onPayPriceReply.addObserver(o);
	}
	private EventObservable<PhysicsPropertiesEventArgs> onPhysicsProperties = new EventObservable<PhysicsPropertiesEventArgs>();
	public void registerOnPhysicsProperties(EventObserver<PhysicsPropertiesEventArgs> o)
	{
		onPhysicsProperties.addObserver(o);
	}
	public void unregisterOnPhysicsProperties(EventObserver<PhysicsPropertiesEventArgs> o) 
	{
		onPhysicsProperties.addObserver(o);
	}



	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<PrimEventArgs> m_ObjectUpdate;
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ObjectUpdateLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// A <see cref="Primitive"/>, Foliage or Attachment</summary>
	//	        /// <seealso cref="RequestObject"/>
	//	        /// <seealso cref="RequestObjects"/>
	//	        public event EventHandler<PrimEventArgs> ObjectUpdate 
	//	        {
	//	            add { lock (m_ObjectUpdateLock) { m_ObjectUpdate += value; } }
	//	            remove { lock (m_ObjectUpdateLock) { m_ObjectUpdate -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<ObjectPropertiesEventArgs> m_ObjectProperties;
	//
	//	        ///<summary>Raises the ObjectProperties Event</summary>
	//	        /// <param name="e">A ObjectPropertiesEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnObjectProperties(ObjectPropertiesEventArgs e)
	//	        {
	//	            EventHandler<ObjectPropertiesEventArgs> handler = m_ObjectProperties;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ObjectPropertiesLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// additional <seea cref="Primitive"/> information</summary>
	//	        /// <seealso cref="SelectObject"/>
	//	        /// <seealso cref="SelectObjects"/>
	//	        public event EventHandler<ObjectPropertiesEventArgs> ObjectProperties 
	//	        {
	//	            add { lock (m_ObjectPropertiesLock) { m_ObjectProperties += value; } }
	//	            remove { lock (m_ObjectPropertiesLock) { m_ObjectProperties -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<ObjectPropertiesUpdatedEventArgs> m_ObjectPropertiesUpdated;
	//
	//	        ///<summary>Raises the ObjectPropertiesUpdated Event</summary>
	//	        /// <param name="e">A ObjectPropertiesUpdatedEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnObjectPropertiesUpdated(ObjectPropertiesUpdatedEventArgs e)
	//	        {
	//	            EventHandler<ObjectPropertiesUpdatedEventArgs> handler = m_ObjectPropertiesUpdated;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ObjectPropertiesUpdatedLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// Primitive.ObjectProperties for an object we are currently tracking</summary>
	//	        public event EventHandler<ObjectPropertiesUpdatedEventArgs> ObjectPropertiesUpdated 
	//	        {
	//	            add { lock (m_ObjectPropertiesUpdatedLock) { m_ObjectPropertiesUpdated += value; } }
	//	            remove { lock (m_ObjectPropertiesUpdatedLock) { m_ObjectPropertiesUpdated -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<ObjectPropertiesFamilyEventArgs> m_ObjectPropertiesFamily;
	//
	//	        ///<summary>Raises the ObjectPropertiesFamily Event</summary>
	//	        /// <param name="e">A ObjectPropertiesFamilyEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnObjectPropertiesFamily(ObjectPropertiesFamilyEventArgs e)
	//	        {
	//	            EventHandler<ObjectPropertiesFamilyEventArgs> handler = m_ObjectPropertiesFamily;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ObjectPropertiesFamilyLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// additional <seea cref="Primitive"/> and <see cref="Avatar"/> details</summary>
	//	        /// <seealso cref="RequestObjectPropertiesFamily"/>
	//	        public event EventHandler<ObjectPropertiesFamilyEventArgs> ObjectPropertiesFamily 
	//	        {
	//	            add { lock (m_ObjectPropertiesFamilyLock) { m_ObjectPropertiesFamily += value; } }
	//	            remove { lock (m_ObjectPropertiesFamilyLock) { m_ObjectPropertiesFamily -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<AvatarUpdateEventArgs> m_AvatarUpdate;
	//
	//	        ///<summary>Raises the AvatarUpdate Event</summary>
	//	        /// <param name="e">A AvatarUpdateEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnAvatarUpdate(AvatarUpdateEventArgs e)
	//	        {
	//	            EventHandler<AvatarUpdateEventArgs> handler = m_AvatarUpdate;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_AvatarUpdateLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// updated information for an <see cref="Avatar"/></summary>
	//	        public event EventHandler<AvatarUpdateEventArgs> AvatarUpdate 
	//	        {
	//	            add { lock (m_AvatarUpdateLock) { m_AvatarUpdate += value; } }
	//	            remove { lock (m_AvatarUpdateLock) { m_AvatarUpdate -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<TerseObjectUpdateEventArgs> m_TerseObjectUpdate;
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_TerseObjectUpdateLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// <see cref="Primitive"/> and <see cref="Avatar"/> movement changes</summary>
	//	        public event EventHandler<TerseObjectUpdateEventArgs> TerseObjectUpdate 
	//	        {
	//	            add { lock (m_TerseObjectUpdateLock) { m_TerseObjectUpdate += value; } }
	//	            remove { lock (m_TerseObjectUpdateLock) { m_TerseObjectUpdate -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<ObjectDataBlockUpdateEventArgs> m_ObjectDataBlockUpdate;
	//
	//	        ///<summary>Raises the ObjectDataBlockUpdate Event</summary>
	//	        /// <param name="e">A ObjectDataBlockUpdateEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnObjectDataBlockUpdate(ObjectDataBlockUpdateEventArgs e)
	//	        {
	//	            EventHandler<ObjectDataBlockUpdateEventArgs> handler = m_ObjectDataBlockUpdate;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_ObjectDataBlockUpdateLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// updates to an Objects DataBlock</summary>
	//	        public event EventHandler<ObjectDataBlockUpdateEventArgs> ObjectDataBlockUpdate 
	//	        {
	//	            add { lock (m_ObjectDataBlockUpdateLock) { m_ObjectDataBlockUpdate += value; } }
	//	            remove { lock (m_ObjectDataBlockUpdateLock) { m_ObjectDataBlockUpdate -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<KillObjectEventArgs> m_KillObject;
	//
	//	        ///<summary>Raises the KillObject Event</summary>
	//	        /// <param name="e">A KillObjectEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnKillObject(KillObjectEventArgs e)
	//	        {
	//	            EventHandler<KillObjectEventArgs> handler = m_KillObject;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_KillObjectLock = new object();
	//
	//	        /// <summary>Raised when the simulator informs us an <see cref="Primitive"/>
	//	        /// or <see cref="Avatar"/> is no longer within view</summary>
	//	        public event EventHandler<KillObjectEventArgs> KillObject 
	//	        {
	//	            add { lock (m_KillObjectLock) { m_KillObject += value; } }
	//	            remove { lock (m_KillObjectLock) { m_KillObject -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<AvatarSitChangedEventArgs> m_AvatarSitChanged;
	//
	//	        ///<summary>Raises the AvatarSitChanged Event</summary>
	//	        /// <param name="e">A AvatarSitChangedEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnAvatarSitChanged(AvatarSitChangedEventArgs e)
	//	        {
	//	            EventHandler<AvatarSitChangedEventArgs> handler = m_AvatarSitChanged;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_AvatarSitChangedLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// updated sit information for our <see cref="Avatar"/></summary>
	//	        public event EventHandler<AvatarSitChangedEventArgs> AvatarSitChanged 
	//	        {
	//	            add { lock (m_AvatarSitChangedLock) { m_AvatarSitChanged += value; } }
	//	            remove { lock (m_AvatarSitChangedLock) { m_AvatarSitChanged -= value; } }
	//	        }
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<PayPriceReplyEventArgs> m_PayPriceReply;
	//
	//	        ///<summary>Raises the PayPriceReply Event</summary>
	//	        /// <param name="e">A PayPriceReplyEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnPayPriceReply(PayPriceReplyEventArgs e)
	//	        {
	//	            EventHandler<PayPriceReplyEventArgs> handler = m_PayPriceReply;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_PayPriceReplyLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// purchase price information for a <see cref="Primitive"/></summary>
	//	        public event EventHandler<PayPriceReplyEventArgs> PayPriceReply 
	//	        {
	//	            add { lock (m_PayPriceReplyLock) { m_PayPriceReply += value; } }
	//	            remove { lock (m_PayPriceReplyLock) { m_PayPriceReply -= value; } }
	//	        }
	//
	//	        /// <summary>
	//	        /// Callback for getting object media data via CAP
	//	        /// </summary>
	//	        /// <param name="success">Indicates if the operation was succesfull</param>
	//	        /// <param name="version">Object media version String</param>
	//	        /// <param name="faceMedia">Array indexed on prim face of media entry data</param>
	//	        public delegate void ObjectMediaCallback(bool success, String version, MediaEntry[] faceMedia);
	//
	//	        /// <summary>The event subscribers, null of no subscribers</summary>
	//	        private EventHandler<PhysicsPropertiesEventArgs> m_PhysicsProperties;
	//
	//	        ///<summary>Raises the PhysicsProperties Event</summary>
	//	        /// <param name="e">A PhysicsPropertiesEventArgs object containing
	//	        /// the data sent from the simulator</param>
	//	        protected virtual void OnPhysicsProperties(PhysicsPropertiesEventArgs e)
	//	        {
	//	            EventHandler<PhysicsPropertiesEventArgs> handler = m_PhysicsProperties;
	//	            if (handler != null)
	//	                handler(this, e);
	//	        }
	//
	//	        /// <summary>Thread sync lock object</summary>
	//	        private readonly object m_PhysicsPropertiesLock = new object();
	//
	//	        /// <summary>Raised when the simulator sends us data containing
	//	        /// additional <seea cref="Primitive"/> information</summary>
	//	        /// <seealso cref="SelectObject"/>
	//	        /// <seealso cref="SelectObjects"/>
	//	        public event EventHandler<PhysicsPropertiesEventArgs> PhysicsProperties 
	//	        {
	//	            add { lock (m_PhysicsPropertiesLock) { m_PhysicsProperties += value; } }
	//	            remove { lock (m_PhysicsPropertiesLock) { m_PhysicsProperties -= value; } }
	//	        }

	//endregion Delegates

	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();

	/// <summary>Reference to the GridClient object</summary>
	protected GridClient Client;
	/// <summary>Does periodic dead reckoning calculation to convert
	/// velocity and acceleration to new positions for objects</summary>
	private Timer InterpolationTimer;

	/// <summary>
	/// Construct a new instance of the ObjectManager class
	/// </summary>
	/// <param name="client">A reference to the <see cref="GridClient"/> instance</param>
	public ObjectManager(GridClient client)
	{
		Client = client;

		//			            Client.network.RegisterCallback(PacketType.ObjectUpdate, ObjectUpdateHandler, false);
		Client.network.RegisterCallback(PacketType.ObjectUpdate, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ObjectUpdateHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}, false
				);
		//	            Client.network.RegisterCallback(PacketType.ImprovedTerseObjectUpdate, ImprovedTerseObjectUpdateHandler, false);
		Client.network.RegisterCallback(PacketType.ImprovedTerseObjectUpdate, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ImprovedTerseObjectUpdateHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}, false
				);
		//	            Client.network.RegisterCallback(PacketType.ObjectUpdateCompressed, ObjectUpdateCompressedHandler);
		Client.network.RegisterCallback(PacketType.ObjectUpdateCompressed, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ObjectUpdateCompressedHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//	            Client.network.RegisterCallback(PacketType.ObjectUpdateCached, ObjectUpdateCachedHandler);
		Client.network.RegisterCallback(PacketType.ObjectUpdateCached, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ObjectUpdateCachedHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//	            Client.network.RegisterCallback(PacketType.KillObject, KillObjectHandler);
		Client.network.RegisterCallback(PacketType.KillObject, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ KillObjectHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//	            Client.network.RegisterCallback(PacketType.ObjectPropertiesFamily, ObjectPropertiesFamilyHandler);
		Client.network.RegisterCallback(PacketType.ObjectPropertiesFamily, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ObjectPropertiesFamilyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//	            Client.network.RegisterCallback(PacketType.ObjectProperties, ObjectPropertiesHandler);
		Client.network.RegisterCallback(PacketType.ObjectProperties, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ ObjectPropertiesHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//	            Client.network.RegisterCallback(PacketType.PayPriceReply, PayPriceReplyHandler);
		Client.network.RegisterCallback(PacketType.PayPriceReply, new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
				try{ PayPriceReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//	            Client.network.RegisterEventCallback("ObjectPhysicsProperties", ObjectPhysicsPropertiesHandler);
		Client.network.RegisterEventCallback("ObjectPhysicsProperties", 
				new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,
					CapsEventObservableArg arg) {
				try{ObjectPhysicsPropertiesHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});
	}

	//region Internal event handlers

	private void Network_OnDisconnected(NetworkManager.DisconnectType reason, String message)
	{
		if (InterpolationTimer != null)
		{
			InterpolationTimer.cancel();
			InterpolationTimer = null;
		}
	}

	private void Network_OnConnected(Object sender)
	{
		if (Client.settings.USE_INTERPOLATION_TIMER)
		{
			//	                InterpolationTimer = new Timer(InterpolationTimer_Elapsed, null, Settings.INTERPOLATION_INTERVAL, Timeout.Infinite);
			//			InterpolationTimer = new Timer();
			//			InterpolationTimer.schedule(new TimerTask(){
			//				@Override
			//				public void run() {
			//					InterpolationTimer_Elapsed(null);
			//				}				
			//			}, Settings.SIMULATOR_TIMEOUT);
			createInterpolationTimer(Client.settings.SIMULATOR_TIMEOUT);
		}
	}

	private void createInterpolationTimer(long delay)
	{
		InterpolationTimer = new Timer();
		InterpolationTimer.schedule(new TimerTask(){
			@Override
			public void run() {
				InterpolationTimer_Elapsed(null);
			}				
		}, delay);
	}

	private void cancelInterpolationTimer()
	{
		InterpolationTimer.cancel();
		InterpolationTimer = null;
	}


	private void updateInterpolationTimer(long delay)
	{
		cancelInterpolationTimer();
		createInterpolationTimer(delay);
	}


	//endregion Internal event handlers

	//region Public Methods

	/// <summary>
	/// Request information for a single object from a <see cref="Simulator"/> 
	/// you are currently connected to
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>
	/// <param name="localID">The Local ID of the object</param>
	public void RequestObject(Simulator simulator, long localID)
	{
		RequestMultipleObjectsPacket request = new RequestMultipleObjectsPacket();
		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();
		request.ObjectData = new RequestMultipleObjectsPacket.ObjectDataBlock[1];
		request.ObjectData[0] = new RequestMultipleObjectsPacket.ObjectDataBlock();
		request.ObjectData[0].ID = localID;
		request.ObjectData[0].CacheMissType = 0;

		Client.network.SendPacket(request, simulator);
	}

	/// <summary>
	/// Request information for multiple objects contained in
	/// the same simulator
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the objects are located</param>
	/// <param name="localIDs">An array containing the Local IDs of the objects</param>
	public void RequestObjects(Simulator simulator, List<Long> localIDs)
	{
		RequestMultipleObjectsPacket request = new RequestMultipleObjectsPacket();
		request.AgentData.AgentID = Client.self.getAgentID();
		request.AgentData.SessionID = Client.self.getSessionID();
		request.ObjectData = new RequestMultipleObjectsPacket.ObjectDataBlock[localIDs.size()];

		for (int i = 0; i < localIDs.size(); i++)
		{
			request.ObjectData[i] = new RequestMultipleObjectsPacket.ObjectDataBlock();
			request.ObjectData[i].ID = localIDs.get(i);
			request.ObjectData[i].CacheMissType = 0;
		}

		Client.network.SendPacket(request, simulator);
	}

	/// <summary>
	/// Attempt to purchase an original object, a copy, or the contents of
	/// an object
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>
	/// <param name="saleType">Whether the original, a copy, or the object
	/// contents are on sale. This is used for verification, if the this
	/// sale type is not valid for the object the purchase will fail</param>
	/// <param name="price">Price of the object. This is used for 
	/// verification, if it does not match the actual price the purchase
	/// will fail</param>
	/// <param name="groupID">Group ID that will be associated with the new
	/// purchase</param>
	/// <param name="categoryID">Inventory folder UUID where the object or objects 
	/// purchased should be placed</param>
	/// <example>
	/// <code>
	///     BuyObject(Client.network.CurrentSim, 500, SaleType.Copy, 
	///         100, UUID.Zero, Client.Self.InventoryRootFolderUUID);
	/// </code> 
	///</example>
	public void BuyObject(Simulator simulator, long localID, SaleType saleType, int price, UUID groupID,
			UUID categoryID)
	{
		ObjectBuyPacket buy = new ObjectBuyPacket();

		buy.AgentData.AgentID = Client.self.getAgentID();
		buy.AgentData.SessionID = Client.self.getSessionID();
		buy.AgentData.GroupID = groupID;
		buy.AgentData.CategoryID = categoryID;

		buy.ObjectData = new ObjectBuyPacket.ObjectDataBlock[1];
		buy.ObjectData[0] = new ObjectBuyPacket.ObjectDataBlock();
		buy.ObjectData[0].ObjectLocalID = localID;
		buy.ObjectData[0].SaleType = saleType.getIndex();
		buy.ObjectData[0].SalePrice = price;

		Client.network.SendPacket(buy, simulator);
	}

	/// <summary>
	/// Request prices that should be displayed in pay dialog. This will triggger the simulator
	/// to send us back a PayPriceReply which can be handled by OnPayPriceReply event
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>
	/// <param name="objectID">The ID of the object</param>
	/// <remarks>The result is raised in the <see cref="PayPriceReply"/> event</remarks>
	public void RequestPayPrice(Simulator simulator, UUID objectID)
	{
		RequestPayPricePacket payPriceRequest = new RequestPayPricePacket();

		payPriceRequest.ObjectData = new RequestPayPricePacket.ObjectDataBlock();
		payPriceRequest.ObjectData.ObjectID = objectID;

		Client.network.SendPacket(payPriceRequest, simulator);
	}

	/// <summary>
	/// Select a single object. This will cause the <see cref="Simulator"/> to send us 
	/// an <see cref="ObjectPropertiesPacket"/> which will raise the <see cref="ObjectProperties"/> event
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>        
	/// <seealso cref="ObjectPropertiesFamilyEventArgs"/>
	public void SelectObject(Simulator simulator, long localID)
	{
		SelectObject(simulator, localID, true);
	}

	/// <summary>
	/// Select a single object. This will cause the <see cref="Simulator"/> to send us 
	/// an <see cref="ObjectPropertiesPacket"/> which will raise the <see cref="ObjectProperties"/> event
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>
	/// <param name="localID">The Local ID of the object</param>
	/// <param name="automaticDeselect">if true, a call to <see cref="DeselectObject"/> is
	/// made immediately following the request</param>
	/// <seealso cref="ObjectPropertiesFamilyEventArgs"/>
	public void SelectObject(Simulator simulator, long localID, boolean automaticDeselect)
	{
		ObjectSelectPacket select = new ObjectSelectPacket();

		select.AgentData.AgentID = Client.self.getAgentID();
		select.AgentData.SessionID = Client.self.getSessionID();

		select.ObjectData = new ObjectSelectPacket.ObjectDataBlock[1];
		select.ObjectData[0] = new ObjectSelectPacket.ObjectDataBlock();
		select.ObjectData[0].ObjectLocalID = localID;

		Client.network.SendPacket(select, simulator);

		if (automaticDeselect)
		{
			DeselectObject(simulator, localID);
		}
	}

	/// <summary>
	/// Select multiple objects. This will cause the <see cref="Simulator"/> to send us 
	/// an <see cref="ObjectPropertiesPacket"/> which will raise the <see cref="ObjectProperties"/> event
	/// </summary>        
	/// <param name="simulator">The <see cref="Simulator"/> the objects are located</param> 
	/// <param name="localIDs">An array containing the Local IDs of the objects</param>
	/// <param name="automaticDeselect">Should objects be deselected immediately after selection</param>
	/// <seealso cref="ObjectPropertiesFamilyEventArgs"/>
	public void SelectObjects(Simulator simulator, long[] localIDs, boolean automaticDeselect)
	{
		ObjectSelectPacket select = new ObjectSelectPacket();

		select.AgentData.AgentID = Client.self.getAgentID();
		select.AgentData.SessionID = Client.self.getSessionID();

		select.ObjectData = new ObjectSelectPacket.ObjectDataBlock[localIDs.length];

		for (int i = 0; i < localIDs.length; i++)
		{
			select.ObjectData[i] = new ObjectSelectPacket.ObjectDataBlock();
			select.ObjectData[i].ObjectLocalID = localIDs[i];
		}

		Client.network.SendPacket(select, simulator);

		if (automaticDeselect)
		{
			DeselectObjects(simulator, localIDs);
		}
	}

	/// <summary>
	/// Select multiple objects. This will cause the <see cref="Simulator"/> to send us 
	/// an <see cref="ObjectPropertiesPacket"/> which will raise the <see cref="ObjectProperties"/> event
	/// </summary>        
	/// <param name="simulator">The <see cref="Simulator"/> the objects are located</param> 
	/// <param name="localIDs">An array containing the Local IDs of the objects</param>
	/// <seealso cref="ObjectPropertiesFamilyEventArgs"/>
	public void SelectObjects(Simulator simulator, long[] localIDs)
	{
		SelectObjects(simulator, localIDs, true);
	}

	/// <summary>
	/// Update the properties of an object
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>        
	/// <param name="physical">true to turn the objects physical property on</param>
	/// <param name="temporary">true to turn the objects temporary property on</param>
	/// <param name="phantom">true to turn the objects phantom property on</param>
	/// <param name="castsShadow">true to turn the objects cast shadows property on</param>
	public void SetFlags(Simulator simulator, long localID, boolean physical, boolean temporary, boolean phantom, boolean castsShadow)
	{
		SetFlags(simulator, localID, physical, temporary, phantom, castsShadow, PhysicsShapeType.Prim, 1000f, 0.6f, 0.5f, 1f);
	}

	/// <summary>
	/// Update the properties of an object
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>        
	/// <param name="physical">true to turn the objects physical property on</param>
	/// <param name="temporary">true to turn the objects temporary property on</param>
	/// <param name="phantom">true to turn the objects phantom property on</param>
	/// <param name="castsShadow">true to turn the objects cast shadows property on</param>
	/// <param name="physicsType">Type of the represetnation prim will have in the physics engine</param>
	/// <param name="density">Density - normal value 1000</param>
	/// <param name="friction">Friction - normal value 0.6</param>
	/// <param name="restitution">Restitution - standard value 0.5</param>
	/// <param name="gravityMultiplier">Gravity multiplier - standar value 1.0</param>
	public void SetFlags(Simulator simulator, long localID, boolean physical, boolean temporary, boolean phantom, boolean castsShadow,
			PhysicsShapeType physicsType, float density, float friction, float restitution, float gravityMultiplier)
	{
		ObjectFlagUpdatePacket flags = new ObjectFlagUpdatePacket();
		flags.AgentData.AgentID = Client.self.getAgentID();
		flags.AgentData.SessionID = Client.self.getSessionID();
		flags.AgentData.ObjectLocalID = localID;
		flags.AgentData.UsePhysics = physical;
		flags.AgentData.IsTemporary = temporary;
		flags.AgentData.IsPhantom = phantom;
		flags.AgentData.CastsShadows = castsShadow;

		flags.ExtraPhysics = new ObjectFlagUpdatePacket.ExtraPhysicsBlock[1];
		flags.ExtraPhysics[0] = new ObjectFlagUpdatePacket.ExtraPhysicsBlock();
		flags.ExtraPhysics[0].PhysicsShapeType = (byte)physicsType.getIndex();
		flags.ExtraPhysics[0].Density = density;
		flags.ExtraPhysics[0].Friction = friction;
		flags.ExtraPhysics[0].Restitution = restitution;
		flags.ExtraPhysics[0].GravityMultiplier = gravityMultiplier;

		Client.network.SendPacket(flags, simulator);
	}

	/// <summary>
	/// Sets the sale properties of a single object
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>        
	/// <param name="saleType">One of the options from the <see cref="SaleType"/> enum</param>
	/// <param name="price">The price of the object</param>
	public void SetSaleInfo(Simulator simulator, long localID, SaleType saleType, int price)
	{
		ObjectSaleInfoPacket sale = new ObjectSaleInfoPacket();
		sale.AgentData.AgentID = Client.self.getAgentID();
		sale.AgentData.SessionID = Client.self.getSessionID();
		sale.ObjectData = new ObjectSaleInfoPacket.ObjectDataBlock[1];
		sale.ObjectData[0] = new ObjectSaleInfoPacket.ObjectDataBlock();
		sale.ObjectData[0].LocalID = localID;
		sale.ObjectData[0].SalePrice = price;
		sale.ObjectData[0].SaleType = saleType.getIndex();

		Client.network.SendPacket(sale, simulator);
	}

	/// <summary>
	/// Sets the sale properties of multiple objects
	/// </summary>        
	/// <param name="simulator">The <see cref="Simulator"/> the objects are located</param> 
	/// <param name="localIDs">An array containing the Local IDs of the objects</param>
	/// <param name="saleType">One of the options from the <see cref="SaleType"/> enum</param>
	/// <param name="price">The price of the object</param>
	public void SetSaleInfo(Simulator simulator, List<Long> localIDs, SaleType saleType, int price)
	{
		ObjectSaleInfoPacket sale = new ObjectSaleInfoPacket();
		sale.AgentData.AgentID = Client.self.getAgentID();
		sale.AgentData.SessionID = Client.self.getSessionID();
		sale.ObjectData = new ObjectSaleInfoPacket.ObjectDataBlock[localIDs.size()];

		for (int i = 0; i < localIDs.size(); i++)
		{
			sale.ObjectData[i] = new ObjectSaleInfoPacket.ObjectDataBlock();
			sale.ObjectData[i].LocalID = localIDs.get(i);
			sale.ObjectData[i].SalePrice = price;
			sale.ObjectData[i].SaleType = saleType.getIndex();
		}

		Client.network.SendPacket(sale, simulator);
	}

	/// <summary>
	/// Deselect a single object
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>
	public void DeselectObject(Simulator simulator, long localID)
	{
		ObjectDeselectPacket deselect = new ObjectDeselectPacket();

		deselect.AgentData.AgentID = Client.self.getAgentID();
		deselect.AgentData.SessionID = Client.self.getSessionID();

		deselect.ObjectData = new ObjectDeselectPacket.ObjectDataBlock[1];
		deselect.ObjectData[0] = new ObjectDeselectPacket.ObjectDataBlock();
		deselect.ObjectData[0].ObjectLocalID = localID;

		Client.network.SendPacket(deselect, simulator);
	}

	/// <summary>
	/// Deselect multiple objects.
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the objects are located</param> 
	/// <param name="localIDs">An array containing the Local IDs of the objects</param>
	public void DeselectObjects(Simulator simulator, long[] localIDs)
	{
		ObjectDeselectPacket deselect = new ObjectDeselectPacket();

		deselect.AgentData.AgentID = Client.self.getAgentID();
		deselect.AgentData.SessionID = Client.self.getSessionID();

		deselect.ObjectData = new ObjectDeselectPacket.ObjectDataBlock[localIDs.length];

		for (int i = 0; i < localIDs.length; i++)
		{
			deselect.ObjectData[i] = new ObjectDeselectPacket.ObjectDataBlock();
			deselect.ObjectData[i].ObjectLocalID = localIDs[i];
		}

		Client.network.SendPacket(deselect, simulator);
	}

	/// <summary>
	/// Perform a click action on an object
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>
	public void ClickObject(Simulator simulator, long localID) throws InterruptedException
	{
		ClickObject(simulator, localID, Vector3.Zero, Vector3.Zero, 0, Vector3.Zero, Vector3.Zero, Vector3.Zero);
	}

	/// <summary>
	/// Perform a click action (Grab) on a single object
	/// </summary>
	/// <param name="simulator">The <see cref="Simulator"/> the object is located</param>        
	/// <param name="localID">The Local ID of the object</param>
	/// <param name="uvCoord">The texture coordinates to touch</param>
	/// <param name="stCoord">The surface coordinates to touch</param>
	/// <param name="faceIndex">The face of the position to touch</param>
	/// <param name="position">The region coordinates of the position to touch</param>
	/// <param name="normal">The surface normal of the position to touch (A normal is a vector perpindicular to the surface)</param>
	/// <param name="binormal">The surface binormal of the position to touch (A binormal is a vector tangen to the surface
	/// pointing along the U direction of the tangent space</param>
	public void ClickObject(Simulator simulator, long localID, Vector3 uvCoord, Vector3 stCoord, int faceIndex, Vector3 position,
			Vector3 normal, Vector3 binormal) throws InterruptedException
			{
		ObjectGrabPacket grab = new ObjectGrabPacket();
		grab.AgentData.AgentID = Client.self.getAgentID();
		grab.AgentData.SessionID = Client.self.getSessionID();
		grab.ObjectData.GrabOffset = Vector3.Zero;
		grab.ObjectData.LocalID = localID;
		grab.SurfaceInfo = new ObjectGrabPacket.SurfaceInfoBlock[1];
		grab.SurfaceInfo[0] = new ObjectGrabPacket.SurfaceInfoBlock();
		grab.SurfaceInfo[0].UVCoord = uvCoord;
		grab.SurfaceInfo[0].STCoord = stCoord;
		grab.SurfaceInfo[0].FaceIndex = faceIndex;
		grab.SurfaceInfo[0].Position = position;
		grab.SurfaceInfo[0].Normal = normal;
		grab.SurfaceInfo[0].Binormal = binormal;

		Client.network.SendPacket(grab, simulator);

		// TODO: If these hit the server out of order the click will fail 
		// and we'll be grabbing the object
		Thread.sleep(50);

		ObjectDeGrabPacket degrab = new ObjectDeGrabPacket();
		degrab.AgentData.AgentID = Client.self.getAgentID();
		degrab.AgentData.SessionID = Client.self.getSessionID();
		degrab.ObjectData.LocalID = localID;
		degrab.SurfaceInfo = new ObjectDeGrabPacket.SurfaceInfoBlock[1];
		degrab.SurfaceInfo[0] = new ObjectDeGrabPacket.SurfaceInfoBlock();
		degrab.SurfaceInfo[0].UVCoord = uvCoord;
		degrab.SurfaceInfo[0].STCoord = stCoord;
		degrab.SurfaceInfo[0].FaceIndex = faceIndex;
		degrab.SurfaceInfo[0].Position = position;
		degrab.SurfaceInfo[0].Normal = normal;
		degrab.SurfaceInfo[0].Binormal = binormal;

		Client.network.SendPacket(degrab, simulator);
			}

	/// <summary>
	/// Create (rez) a new prim object in a simulator
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object to place the object in</param>
	/// <param name="prim">Data describing the prim object to rez</param>
	/// <param name="groupID">Group ID that this prim will be set to, or UUID.Zero if you
	/// do not want the object to be associated with a specific group</param>
	/// <param name="position">An approximation of the position at which to rez the prim</param>
	/// <param name="scale">Scale vector to size this prim</param>
	/// <param name="rotation">Rotation quaternion to rotate this prim</param>
	/// <remarks>Due to the way client prim rezzing is done on the server,
	/// the requested position for an object is only close to where the prim
	/// actually ends up. If you desire exact placement you'll need to 
	/// follow up by moving the object after it has been created. This
	/// function will not set textures, light and flexible data, or other 
	/// extended primitive properties</remarks>
	public void AddPrim(Simulator simulator, ConstructionData prim, UUID groupID, Vector3 position,
			Vector3 scale, Quaternion rotation)
	{
		AddPrim(simulator, prim, groupID, position, scale, rotation, PrimFlags.CreateSelected);
	}

	/// <summary>
	/// Create (rez) a new prim object in a simulator
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="Simulator"/> object to place the object in</param>
	/// <param name="prim">Data describing the prim object to rez</param>
	/// <param name="groupID">Group ID that this prim will be set to, or UUID.Zero if you
	/// do not want the object to be associated with a specific group</param>
	/// <param name="position">An approximation of the position at which to rez the prim</param>
	/// <param name="scale">Scale vector to size this prim</param>
	/// <param name="rotation">Rotation quaternion to rotate this prim</param>
	/// <param name="createFlags">Specify the <seealso cref="PrimFlags"/></param>
	/// <remarks>Due to the way client prim rezzing is done on the server,
	/// the requested position for an object is only close to where the prim
	/// actually ends up. If you desire exact placement you'll need to 
	/// follow up by moving the object after it has been created. This
	/// function will not set textures, light and flexible data, or other 
	/// extended primitive properties</remarks>
	public void AddPrim(Simulator simulator, ConstructionData prim, UUID groupID, Vector3 position,
			Vector3 scale, Quaternion rotation, PrimFlags createFlags)
	{
		ObjectAddPacket packet = new ObjectAddPacket();

		packet.AgentData.AgentID = Client.self.getAgentID();
		packet.AgentData.SessionID = Client.self.getSessionID();
		packet.AgentData.GroupID = groupID;

		packet.ObjectData.State = prim.State;
		packet.ObjectData.AddFlags = (long)createFlags.getIndex();
		packet.ObjectData.PCode = (byte)PCode.Prim.getIndex();

		packet.ObjectData.Material = (byte)prim.Material.getIndex();
		packet.ObjectData.Scale = scale;
		packet.ObjectData.Rotation = rotation;

		packet.ObjectData.PathCurve = (byte)prim.PathCurve.getIndex();
		packet.ObjectData.PathBegin = Primitive.PackBeginCut(prim.PathBegin);
		packet.ObjectData.PathEnd = Primitive.PackEndCut(prim.PathEnd);
		packet.ObjectData.PathRadiusOffset = Primitive.PackPathTwist(prim.PathRadiusOffset);
		packet.ObjectData.PathRevolutions = Primitive.PackPathRevolutions(prim.PathRevolutions);
		packet.ObjectData.PathScaleX = Primitive.PackPathScale(prim.PathScaleX);
		packet.ObjectData.PathScaleY = Primitive.PackPathScale(prim.PathScaleY);
		packet.ObjectData.PathShearX = (byte)Primitive.PackPathShear(prim.PathShearX);
		packet.ObjectData.PathShearY = (byte)Primitive.PackPathShear(prim.PathShearY);
		packet.ObjectData.PathSkew = Primitive.PackPathTwist(prim.PathSkew);
		packet.ObjectData.PathTaperX = Primitive.PackPathTaper(prim.PathTaperX);
		packet.ObjectData.PathTaperY = Primitive.PackPathTaper(prim.PathTaperY);
		packet.ObjectData.PathTwist = Primitive.PackPathTwist(prim.PathTwist);
		packet.ObjectData.PathTwistBegin = Primitive.PackPathTwist(prim.PathTwistBegin);

		packet.ObjectData.ProfileCurve = prim.profileCurve;
		packet.ObjectData.ProfileBegin = Primitive.PackBeginCut(prim.ProfileBegin);
		packet.ObjectData.ProfileEnd = Primitive.PackEndCut(prim.ProfileEnd);
		packet.ObjectData.ProfileHollow = Primitive.PackProfileHollow(prim.ProfileHollow);

		packet.ObjectData.RayStart = position;
		packet.ObjectData.RayEnd = position;
		packet.ObjectData.RayEndIsIntersection = 0;
		packet.ObjectData.RayTargetID = UUID.Zero;
		packet.ObjectData.BypassRaycast = 1;

		Client.network.SendPacket(packet, simulator);
	}

	/// <summary>
	/// Rez a Linden tree
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="scale">The size of the tree</param>
	/// <param name="rotation">The rotation of the tree</param>
	/// <param name="position">The position of the tree</param>
	/// <param name="treeType">The Type of tree</param>
	/// <param name="groupOwner">The <seealso cref="UUID"/> of the group to set the tree to, 
	/// or UUID.Zero if no group is to be set</param>
	/// <param name="newTree">true to use the "new" Linden trees, false to use the old</param>
	public void AddTree(Simulator simulator, Vector3 scale, Quaternion rotation, Vector3 position,
			Tree treeType, UUID groupOwner, boolean newTree)
	{
		ObjectAddPacket add = new ObjectAddPacket();

		add.AgentData.AgentID = Client.self.getAgentID();
		add.AgentData.SessionID = Client.self.getSessionID();
		add.AgentData.GroupID = groupOwner;
		add.ObjectData.BypassRaycast = 1;
		add.ObjectData.Material = 3;
		add.ObjectData.PathCurve = 16;
		add.ObjectData.PCode = newTree ? (byte)PCode.NewTree.getIndex() : (byte)PCode.Tree.getIndex();
		add.ObjectData.RayEnd = position;
		add.ObjectData.RayStart = position;
		add.ObjectData.RayTargetID = UUID.Zero;
		add.ObjectData.Rotation = rotation;
		add.ObjectData.Scale = scale;
		add.ObjectData.State = (byte)treeType.getIndex();

		Client.network.SendPacket(add, simulator);
	}

	/// <summary>
	/// Rez grass and ground cover
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="scale">The size of the grass</param>
	/// <param name="rotation">The rotation of the grass</param>
	/// <param name="position">The position of the grass</param>
	/// <param name="grassType">The type of grass from the <seealso cref="Grass"/> enum</param>
	/// <param name="groupOwner">The <seealso cref="UUID"/> of the group to set the tree to, 
	/// or UUID.Zero if no group is to be set</param>
	public void AddGrass(Simulator simulator, Vector3 scale, Quaternion rotation, Vector3 position,
			Grass grassType, UUID groupOwner)
	{
		ObjectAddPacket add = new ObjectAddPacket();

		add.AgentData.AgentID = Client.self.getAgentID();
		add.AgentData.SessionID = Client.self.getSessionID();
		add.AgentData.GroupID = groupOwner;
		add.ObjectData.BypassRaycast = 1;
		add.ObjectData.Material = 3;
		add.ObjectData.PathCurve = 16;
		add.ObjectData.PCode = (byte)PCode.Grass.getIndex();
		add.ObjectData.RayEnd = position;
		add.ObjectData.RayStart = position;
		add.ObjectData.RayTargetID = UUID.Zero;
		add.ObjectData.Rotation = rotation;
		add.ObjectData.Scale = scale;
		add.ObjectData.State = (byte)grassType.getIndex();

		Client.network.SendPacket(add, simulator);
	}

	/// <summary>
	/// Set the textures to apply to the faces of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="textures">The texture data to apply</param>
	public void SetTextures(Simulator simulator, long localID, TextureEntry textures) throws IOException
	{
		SetTextures(simulator, localID, textures, "");
	}

	/// <summary>
	/// Set the textures to apply to the faces of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="textures">The texture data to apply</param>
	/// <param name="mediaUrl">A media URL (not used)</param>
	public void SetTextures(Simulator simulator, long localID, TextureEntry textures, String mediaUrl) throws IOException
	{
		ObjectImagePacket image = new ObjectImagePacket();

		image.AgentData.AgentID = Client.self.getAgentID();
		image.AgentData.SessionID = Client.self.getSessionID();
		image.ObjectData = new ObjectImagePacket.ObjectDataBlock[1];
		image.ObjectData[0] = new ObjectImagePacket.ObjectDataBlock();
		image.ObjectData[0].ObjectLocalID = localID;
		image.ObjectData[0].TextureEntry = textures.GetBytes();
		image.ObjectData[0].MediaURL = Utils.stringToBytesWithTrailingNullByte(mediaUrl);

		Client.network.SendPacket(image, simulator);
	}

	/// <summary>
	/// Set the Light data on an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="light">A <seealso cref="LightData"/> object containing the data to set</param>
	public void SetLight(Simulator simulator, long localID, LightData light)
	{
		ObjectExtraParamsPacket extra = new ObjectExtraParamsPacket();

		extra.AgentData.AgentID = Client.self.getAgentID();
		extra.AgentData.SessionID = Client.self.getSessionID();
		extra.ObjectData = new ObjectExtraParamsPacket.ObjectDataBlock[1];
		extra.ObjectData[0] = new ObjectExtraParamsPacket.ObjectDataBlock();
		extra.ObjectData[0].ObjectLocalID = localID;
		extra.ObjectData[0].ParamType = ExtraParamType.Light.getIndex();
		if (light.Intensity == 0.0f)
		{
			// Disables the light if intensity is 0
			extra.ObjectData[0].ParamInUse = false;
		}
		else
		{
			extra.ObjectData[0].ParamInUse = true;
		}
		extra.ObjectData[0].ParamData = light.GetBytes();
		extra.ObjectData[0].ParamSize = (long)extra.ObjectData[0].ParamData.length;

		Client.network.SendPacket(extra, simulator);
	}

	/// <summary>
	/// Set the flexible data on an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="flexible">A <seealso cref="Primitive.FlexibleData"/> object containing the data to set</param>
	public void SetFlexible(Simulator simulator, long localID,FlexibleData flexible)
	{
		ObjectExtraParamsPacket extra = new ObjectExtraParamsPacket();

		extra.AgentData.AgentID = Client.self.getAgentID();
		extra.AgentData.SessionID = Client.self.getSessionID();
		extra.ObjectData = new ObjectExtraParamsPacket.ObjectDataBlock[1];
		extra.ObjectData[0] = new ObjectExtraParamsPacket.ObjectDataBlock();
		extra.ObjectData[0].ObjectLocalID = localID;
		extra.ObjectData[0].ParamType = ExtraParamType.Flexible.getIndex();
		extra.ObjectData[0].ParamInUse = true;
		extra.ObjectData[0].ParamData = flexible.GetBytes();
		extra.ObjectData[0].ParamSize = (long)extra.ObjectData[0].ParamData.length;

		Client.network.SendPacket(extra, simulator);
	}

	/// <summary>
	/// Set the sculptie texture and data on an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="sculpt">A <seealso cref="Primitive.SculptData"/> object containing the data to set</param>
	public void SetSculpt(Simulator simulator, long localID,SculptData sculpt)
	{
		ObjectExtraParamsPacket extra = new ObjectExtraParamsPacket();

		extra.AgentData.AgentID = Client.self.getAgentID();
		extra.AgentData.SessionID = Client.self.getSessionID();

		extra.ObjectData = new ObjectExtraParamsPacket.ObjectDataBlock[1];
		extra.ObjectData[0] = new ObjectExtraParamsPacket.ObjectDataBlock();
		extra.ObjectData[0].ObjectLocalID = localID;
		extra.ObjectData[0].ParamType = ExtraParamType.Sculpt.getIndex();
		extra.ObjectData[0].ParamInUse = true;
		extra.ObjectData[0].ParamData = sculpt.GetBytes();
		extra.ObjectData[0].ParamSize = (long)extra.ObjectData[0].ParamData.length;

		Client.network.SendPacket(extra, simulator);

		// Not sure why, but if you don't send this the sculpted prim disappears
		ObjectShapePacket shape = new ObjectShapePacket();

		shape.AgentData.AgentID = Client.self.getAgentID();
		shape.AgentData.SessionID = Client.self.getSessionID();

		shape.ObjectData = new ObjectShapePacket.ObjectDataBlock[1];
		shape.ObjectData[0] = new ObjectShapePacket.ObjectDataBlock();
		shape.ObjectData[0].ObjectLocalID = localID;
		shape.ObjectData[0].PathScaleX = 100;
		shape.ObjectData[0].PathScaleY = (byte)150;
		shape.ObjectData[0].PathCurve = 32;

		Client.network.SendPacket(shape, simulator);
	}

	/// <summary>
	/// Unset additional primitive parameters on an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="type">The extra parameters to set</param>
	public void SetExtraParamOff(Simulator simulator, long localID, EnumSet<ExtraParamType> type)
	{
		ObjectExtraParamsPacket extra = new ObjectExtraParamsPacket();

		extra.AgentData.AgentID = Client.self.getAgentID();
		extra.AgentData.SessionID = Client.self.getSessionID();
		extra.ObjectData = new ObjectExtraParamsPacket.ObjectDataBlock[1];
		extra.ObjectData[0] = new ObjectExtraParamsPacket.ObjectDataBlock();
		extra.ObjectData[0].ObjectLocalID = localID;
		extra.ObjectData[0].ParamType = ExtraParamType.getIndex(type);
		extra.ObjectData[0].ParamInUse = false;
		extra.ObjectData[0].ParamData = Utils.EmptyBytes;
		extra.ObjectData[0].ParamSize = 0;

		Client.network.SendPacket(extra, simulator);
	}

	/// <summary>
	/// Link multiple prims into a linkset
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the objects reside</param>
	/// <param name="localIDs">An array which contains the IDs of the objects to link</param>
	/// <remarks>The last object in the array will be the root object of the linkset TODO: Is this true?</remarks>
	public void LinkPrims(Simulator simulator, List<Long> localIDs)
	{
		ObjectLinkPacket packet = new ObjectLinkPacket();

		packet.AgentData.AgentID = Client.self.getAgentID();
		packet.AgentData.SessionID = Client.self.getSessionID();

		packet.ObjectData = new ObjectLinkPacket.ObjectDataBlock[localIDs.size()];

		for (int i = 0; i < localIDs.size(); i++)
		{
			packet.ObjectData[i] = new ObjectLinkPacket.ObjectDataBlock();
			packet.ObjectData[i].ObjectLocalID = localIDs.get(i);
		}

		Client.network.SendPacket(packet, simulator);
	}

	/// <summary>
	/// Delink/Unlink multiple prims from a linkset
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the objects reside</param>
	/// <param name="localIDs">An array which contains the IDs of the objects to delink</param>
	public void DelinkPrims(Simulator simulator, List<Long> localIDs)
	{
		ObjectDelinkPacket packet = new ObjectDelinkPacket();

		packet.AgentData.AgentID = Client.self.getAgentID();
		packet.AgentData.SessionID = Client.self.getSessionID();

		packet.ObjectData = new ObjectDelinkPacket.ObjectDataBlock[localIDs.size()];

		int i = 0;
		for (long localID : localIDs)
		{
			packet.ObjectData[i] = new ObjectDelinkPacket.ObjectDataBlock();
			packet.ObjectData[i].ObjectLocalID = localID;

			i++;
		}

		Client.network.SendPacket(packet, simulator);
	}

	/// <summary>
	/// Change the rotation of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="rotation">The new rotation of the object</param>
	public void SetRotation(Simulator simulator, long localID, Quaternion rotation)
	{
		ObjectRotationPacket objRotPacket = new ObjectRotationPacket();
		objRotPacket.AgentData.AgentID = Client.self.getAgentID();
		objRotPacket.AgentData.SessionID = Client.self.getSessionID();

		objRotPacket.ObjectData = new ObjectRotationPacket.ObjectDataBlock[1];

		objRotPacket.ObjectData[0] = new ObjectRotationPacket.ObjectDataBlock();
		objRotPacket.ObjectData[0].ObjectLocalID = localID;
		objRotPacket.ObjectData[0].Rotation = rotation;
		Client.network.SendPacket(objRotPacket, simulator);
	}

	/// <summary>
	/// Set the name of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="name">A String containing the new name of the object</param>
	public void SetName(Simulator simulator, long localID, String name)
	{
		SetNames(simulator, new long[] { localID }, new String[] { name });
	}

	/// <summary>
	/// Set the name of multiple objects
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the objects reside</param>
	/// <param name="localIDs">An array which contains the IDs of the objects to change the name of</param>
	/// <param name="names">An array which contains the new names of the objects</param>
	public void SetNames(Simulator simulator, long[] localIDs, String[] names)
	{
		ObjectNamePacket namePacket = new ObjectNamePacket();
		namePacket.AgentData.AgentID = Client.self.getAgentID();
		namePacket.AgentData.SessionID = Client.self.getSessionID();

		namePacket.ObjectData = new ObjectNamePacket.ObjectDataBlock[localIDs.length];

		for (int i = 0; i < localIDs.length; ++i)
		{
			namePacket.ObjectData[i] = new ObjectNamePacket.ObjectDataBlock();
			namePacket.ObjectData[i].LocalID = localIDs[i];
			namePacket.ObjectData[i].Name = Utils.stringToBytesWithTrailingNullByte(names[i]);
		}

		Client.network.SendPacket(namePacket, simulator);
	}

	/// <summary>
	/// Set the description of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="description">A String containing the new description of the object</param>
	public void SetDescription(Simulator simulator, long localID, String description)
	{
		SetDescriptions(simulator, new long[] { localID }, new String[] { description });
	}

	/// <summary>
	/// Set the descriptions of multiple objects
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the objects reside</param>
	/// <param name="localIDs">An array which contains the IDs of the objects to change the description of</param>
	/// <param name="descriptions">An array which contains the new descriptions of the objects</param>
	public void SetDescriptions(Simulator simulator, long[] localIDs, String[] descriptions)
	{
		ObjectDescriptionPacket descPacket = new ObjectDescriptionPacket();
		descPacket.AgentData.AgentID = Client.self.getAgentID();
		descPacket.AgentData.SessionID = Client.self.getSessionID();

		descPacket.ObjectData = new ObjectDescriptionPacket.ObjectDataBlock[localIDs.length];

		for (int i = 0; i < localIDs.length; ++i)
		{
			descPacket.ObjectData[i] = new ObjectDescriptionPacket.ObjectDataBlock();
			descPacket.ObjectData[i].LocalID = localIDs[i];
			descPacket.ObjectData[i].Description = Utils.stringToBytesWithTrailingNullByte(descriptions[i]);
		}

		Client.network.SendPacket(descPacket, simulator);
	}

	/// <summary>
	/// Attach an object to this avatar
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="attachPoint">The point on the avatar the object will be attached</param>
	/// <param name="rotation">The rotation of the attached object</param>
	public void AttachObject(Simulator simulator, long localID, AttachmentPoint attachPoint, Quaternion rotation)
	{
		ObjectAttachPacket attach = new ObjectAttachPacket();
		attach.AgentData.AgentID = Client.self.getAgentID();
		attach.AgentData.SessionID = Client.self.getSessionID();
		attach.AgentData.AttachmentPoint = attachPoint.getIndex();

		attach.ObjectData = new ObjectAttachPacket.ObjectDataBlock[1];
		attach.ObjectData[0] = new ObjectAttachPacket.ObjectDataBlock();
		attach.ObjectData[0].ObjectLocalID = localID;
		attach.ObjectData[0].Rotation = rotation;

		Client.network.SendPacket(attach, simulator);
	}

	/// <summary>
	/// Drop an attached object from this avatar
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/>
	/// object where the objects reside. This will always be the simulator the avatar is currently in
	/// </param>
	/// <param name="localID">The object's ID which is local to the simulator the object is in</param>
	public void DropObject(Simulator simulator, long localID)
	{
		ObjectDropPacket dropit = new ObjectDropPacket();
		dropit.AgentData.AgentID = Client.self.getAgentID();
		dropit.AgentData.SessionID = Client.self.getSessionID();
		dropit.ObjectData = new ObjectDropPacket.ObjectDataBlock[1];
		dropit.ObjectData[0] = new ObjectDropPacket.ObjectDataBlock();
		dropit.ObjectData[0].ObjectLocalID = localID;

		Client.network.SendPacket(dropit, simulator);
	}

	/// <summary>
	/// Detach an object from yourself
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> 
	/// object where the objects reside
	/// 
	/// This will always be the simulator the avatar is currently in
	/// </param>
	/// <param name="localIDs">An array which contains the IDs of the objects to detach</param>
	public void DetachObjects(Simulator simulator, List<Long> localIDs)
	{
		ObjectDetachPacket detach = new ObjectDetachPacket();
		detach.AgentData.AgentID = Client.self.getAgentID();
		detach.AgentData.SessionID = Client.self.getSessionID();
		detach.ObjectData = new ObjectDetachPacket.ObjectDataBlock[localIDs.size()];

		for (int i = 0; i < localIDs.size(); i++)
		{
			detach.ObjectData[i] = new ObjectDetachPacket.ObjectDataBlock();
			detach.ObjectData[i].ObjectLocalID = localIDs.get(i);
		}

		Client.network.SendPacket(detach, simulator);
	}

	/// <summary>
	/// Change the position of an object, Will change position of entire linkset
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="position">The new position of the object</param>
	public void SetPosition(Simulator simulator, long localID, Vector3 position)
	{
		UpdateObject(simulator, localID, position, UpdateType.get(UpdateType.Position.getIndex() | UpdateType.Linked.getIndex()));
	}

	/// <summary>
	/// Change the position of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="position">The new position of the object</param>
	/// <param name="childOnly">if true, will change position of (this) child prim only, not entire linkset</param>
	public void SetPosition(Simulator simulator, long localID, Vector3 position, boolean childOnly)
	{
		EnumSet<UpdateType> type = UpdateType.get(UpdateType.Position.getIndex());

		if (!childOnly)
			type = UpdateType.get(UpdateType.getIndex(type) | UpdateType.Linked.getIndex());

		UpdateObject(simulator, localID, position, type);
	}

	/// <summary>
	/// Change the Scale (size) of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="scale">The new scale of the object</param>
	/// <param name="childOnly">If true, will change scale of this prim only, not entire linkset</param>
	/// <param name="uniform">True to resize prims uniformly</param>
	public void SetScale(Simulator simulator, long localID, Vector3 scale, boolean childOnly, boolean uniform)
	{
		EnumSet<UpdateType> type = UpdateType.get(UpdateType.Scale.getIndex());

		if (!childOnly)
			type = UpdateType.get(UpdateType.getIndex(type) | UpdateType.Linked.getIndex());

		if (uniform)
			type = UpdateType.get(UpdateType.getIndex(type) | UpdateType.Uniform.getIndex());

		UpdateObject(simulator, localID, scale, type);
	}

	/// <summary>
	/// Change the Rotation of an object that is either a child or a whole linkset
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="quat">The new scale of the object</param>
	/// <param name="childOnly">If true, will change rotation of this prim only, not entire linkset</param>
	public void SetRotation(Simulator simulator, long localID, Quaternion quat, boolean childOnly)
	{
		EnumSet<UpdateType> type = UpdateType.get(UpdateType.Rotation.getIndex());

		if (!childOnly)
			type = UpdateType.get(UpdateType.getIndex(type) | UpdateType.Linked.getIndex());

		MultipleObjectUpdatePacket multiObjectUpdate = new MultipleObjectUpdatePacket();
		multiObjectUpdate.AgentData.AgentID = Client.self.getAgentID();
		multiObjectUpdate.AgentData.SessionID = Client.self.getSessionID();

		multiObjectUpdate.ObjectData = new MultipleObjectUpdatePacket.ObjectDataBlock[1];

		multiObjectUpdate.ObjectData[0] = new MultipleObjectUpdatePacket.ObjectDataBlock();
		multiObjectUpdate.ObjectData[0].Type = (byte)UpdateType.getIndex(type);
		multiObjectUpdate.ObjectData[0].ObjectLocalID = localID;
		multiObjectUpdate.ObjectData[0].Data = quat.getBytes();

		Client.network.SendPacket(multiObjectUpdate, simulator);
	}

	/// <summary>
	/// Send a Multiple Object Update packet to change the size, scale or rotation of a primitive
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="data">The new rotation, size, or position of the target object</param>
	/// <param name="type">The flags from the <seealso cref="UpdateType"/> Enum</param>
	public void UpdateObject(Simulator simulator, long localID, Vector3 data, EnumSet<UpdateType> type)
	{
		MultipleObjectUpdatePacket multiObjectUpdate = new MultipleObjectUpdatePacket();
		multiObjectUpdate.AgentData.AgentID = Client.self.getAgentID();
		multiObjectUpdate.AgentData.SessionID = Client.self.getSessionID();

		multiObjectUpdate.ObjectData = new MultipleObjectUpdatePacket.ObjectDataBlock[1];

		multiObjectUpdate.ObjectData[0] = new MultipleObjectUpdatePacket.ObjectDataBlock();
		multiObjectUpdate.ObjectData[0].Type = (byte)UpdateType.getIndex(type);
		multiObjectUpdate.ObjectData[0].ObjectLocalID = localID;
		multiObjectUpdate.ObjectData[0].Data = data.getBytes();

		Client.network.SendPacket(multiObjectUpdate, simulator);
	}

	/// <summary>
	/// Deed an object (prim) to a group, Object must be shared with group which
	/// can be accomplished with SetPermissions()
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="groupOwner">The <seealso cref="UUID"/> of the group to deed the object to</param>
	public void DeedObject(Simulator simulator, long localID, UUID groupOwner)
	{
		ObjectOwnerPacket objDeedPacket = new ObjectOwnerPacket();
		objDeedPacket.AgentData.AgentID = Client.self.getAgentID();
		objDeedPacket.AgentData.SessionID = Client.self.getSessionID();

		// Can only be use in God mode
		objDeedPacket.HeaderData.Override = false;
		objDeedPacket.HeaderData.OwnerID = UUID.Zero;
		objDeedPacket.HeaderData.GroupID = groupOwner;

		objDeedPacket.ObjectData = new ObjectOwnerPacket.ObjectDataBlock[1];
		objDeedPacket.ObjectData[0] = new ObjectOwnerPacket.ObjectDataBlock();

		objDeedPacket.ObjectData[0].ObjectLocalID = localID;

		Client.network.SendPacket(objDeedPacket, simulator);
	}

	/// <summary>
	/// Deed multiple objects (prims) to a group, Objects must be shared with group which
	/// can be accomplished with SetPermissions()
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localIDs">An array which contains the IDs of the objects to deed</param>
	/// <param name="groupOwner">The <seealso cref="UUID"/> of the group to deed the object to</param>
	public void DeedObjects(Simulator simulator, List<Long> localIDs, UUID groupOwner)
	{
		ObjectOwnerPacket packet = new ObjectOwnerPacket();
		packet.AgentData.AgentID = Client.self.getAgentID();
		packet.AgentData.SessionID = Client.self.getSessionID();

		// Can only be use in God mode
		packet.HeaderData.Override = false;
		packet.HeaderData.OwnerID = UUID.Zero;
		packet.HeaderData.GroupID = groupOwner;

		packet.ObjectData = new ObjectOwnerPacket.ObjectDataBlock[localIDs.size()];

		for (int i = 0; i < localIDs.size(); i++)
		{
			packet.ObjectData[i] = new ObjectOwnerPacket.ObjectDataBlock();
			packet.ObjectData[i].ObjectLocalID = localIDs.get(i);
		}
		Client.network.SendPacket(packet, simulator);
	}

	/// <summary>
	/// Set the permissions on multiple objects
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the objects reside</param>
	/// <param name="localIDs">An array which contains the IDs of the objects to set the permissions on</param>
	/// <param name="who">The new Who mask to set</param>
	/// <param name="permissions">Which permission to modify</param>
	/// <param name="set">The new state of permission</param>
	public void SetPermissions(Simulator simulator, List<Long> localIDs, PermissionWho who,
			PermissionMask permissions, boolean set)
	{
		ObjectPermissionsPacket packet = new ObjectPermissionsPacket();

		packet.AgentData.AgentID = Client.self.getAgentID();
		packet.AgentData.SessionID = Client.self.getSessionID();

		// Override can only be used by gods
		packet.HeaderData.Override = false;

		packet.ObjectData = new ObjectPermissionsPacket.ObjectDataBlock[localIDs.size()];

		for (int i = 0; i < localIDs.size(); i++)
		{
			packet.ObjectData[i] = new ObjectPermissionsPacket.ObjectDataBlock();

			packet.ObjectData[i].ObjectLocalID = localIDs.get(i);
			packet.ObjectData[i].Field = (byte)who.getIndex();
			packet.ObjectData[i].Mask = (long)permissions.getIndex();
			packet.ObjectData[i].Set = Utils.booleanToBytes(set);
		}

		Client.network.SendPacket(packet, simulator);
	}

	/// <summary>
	/// Request additional properties for an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="objectID"></param>
	public void RequestObjectPropertiesFamily(Simulator simulator, UUID objectID)
	{
		RequestObjectPropertiesFamily(simulator, objectID, true);
	}

	/// <summary>
	/// Request additional properties for an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="objectID">Absolute UUID of the object</param>
	/// <param name="reliable">Whether to require server acknowledgement of this request</param>
	public void RequestObjectPropertiesFamily(Simulator simulator, UUID objectID, boolean reliable)
	{
		RequestObjectPropertiesFamilyPacket properties = new RequestObjectPropertiesFamilyPacket();
		properties.AgentData.AgentID = Client.self.getAgentID();
		properties.AgentData.SessionID = Client.self.getSessionID();
		properties.ObjectData.ObjectID = objectID;
		// TODO: RequestFlags is typically only for bug report submissions, but we might be able to
		// use it to pass an arbitrary uint back to the callback
		properties.ObjectData.RequestFlags = 0;

		properties.header.Reliable = reliable;

		Client.network.SendPacket(properties, simulator);
	}

	/// <summary>
	/// Set the ownership of a list of objects to the specified group
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the objects reside</param>
	/// <param name="localIds">An array which contains the IDs of the objects to set the group id on</param>
	/// <param name="groupID">The Groups ID</param>
	public void SetObjectsGroup(Simulator simulator, List<Long> localIds, UUID groupID)
	{
		ObjectGroupPacket packet = new ObjectGroupPacket();
		packet.AgentData.AgentID = Client.self.getAgentID();
		packet.AgentData.GroupID = groupID;
		packet.AgentData.SessionID = Client.self.getSessionID();

		packet.ObjectData = new ObjectGroupPacket.ObjectDataBlock[localIds.size()];
		for (int i = 0; i < localIds.size(); i++)
		{
			packet.ObjectData[i] = new ObjectGroupPacket.ObjectDataBlock();
			packet.ObjectData[i].ObjectLocalID = localIds.get(i);
		}

		Client.network.SendPacket(packet, simulator);
	}

	/// <summary>
	/// Update current URL of the previously set prim media
	/// </summary>
	/// <param name="primID">UUID of the prim</param>
	/// <param name="newURL">Set current URL to this</param>
	/// <param name="face">Prim face number</param>
	/// <param name="sim">Simulator in which prim is located</param>
	public void NavigateObjectMedia(UUID primID, int face, String newURL, Simulator sim) throws Exception
	{
		URI url;
		if (sim.Caps != null && null != (url = sim.Caps.CapabilityURI("ObjectMediaNavigate")))
		{
			ObjectMediaNavigateMessage req = new ObjectMediaNavigateMessage();
			req.PrimID = primID;
			req.URL = newURL;
			req.Face = face;

			CapsHttpClient request = new CapsHttpClient(url);

			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
			{
				public void handleEvent(Observable arg0, CapsHttpRequestCompletedArg arg1) {
					//			System.out.println("RequestCompletedObserver called ...");
					CapsHttpRequestCompletedArg rcha = (CapsHttpRequestCompletedArg) arg1;
					if (rcha.getError() != null)
					{
						JLogger.error("ObjectMediaNavigate: " + Utils.getExceptionStackTraceAsString(rcha.getError()));
					}
				}
			});

			request.BeginGetResponse(req.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			JLogger.error("ObjectMediaNavigate capability not available");
		}
	}

	/// <summary>
	/// Set object media
	/// </summary>
	/// <param name="primID">UUID of the prim</param>
	/// <param name="faceMedia">Array the length of prims number of faces. Null on face indexes where there is
	/// no media, <seealso cref="MediaEntry"/> on faces which contain the media</param>
	/// <param name="sim">Simulatior in which prim is located</param>
	public void UpdateObjectMedia(UUID primID, MediaEntry[] faceMedia, Simulator sim) throws Exception
	{
		URI url;
		if (sim.Caps != null && null != (url = sim.Caps.CapabilityURI("ObjectMedia")))
		{
			LindenMessages.ObjectMediaUpdate req = new LindenMessages.ObjectMediaUpdate();
			req.PrimID = primID;
			req.FaceMedia = faceMedia;
			req.Verb = "UPDATE";

			CapsHttpClient request = new CapsHttpClient(url);
			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
			{
				public void handleEvent(Observable arg0, CapsHttpRequestCompletedArg arg1) {
					//			System.out.println("RequestCompletedObserver called ...");
					CapsHttpRequestCompletedArg rcha = (CapsHttpRequestCompletedArg) arg1;
					if (rcha.getError() != null)
					{
						JLogger.error("ObjectMediaUpdate: " + Utils.getExceptionStackTraceAsString(rcha.getError()));
					}
				}
			});

			request.BeginGetResponse(req.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			JLogger.error("ObjectMedia capability not available");
		}
	}

	/// <summary>
	/// Retrieve information about object media
	/// </summary>
	/// <param name="primID">UUID of the primitive</param>
	/// <param name="sim">Simulator where prim is located</param>
	/// <param name="callback">Call this callback when done</param>
	public void RequestObjectMedia(final UUID primID, final Simulator sim, final Action<ObjectMediaCallbackArgs> callback) throws Exception
	{
		URI url;
		if (sim.Caps != null && null != (url = sim.Caps.CapabilityURI("ObjectMedia")))
		{
			LindenMessages.ObjectMediaRequest req = new LindenMessages.ObjectMediaRequest();
			req.PrimID = primID;
			req.Verb = "GET";

			CapsHttpClient request = new CapsHttpClient(url);

			request.addRequestCompleteObserver(new EventObserver<CapsHttpRequestCompletedArg>()
			{
				public void handleEvent(Observable arg0, CapsHttpRequestCompletedArg arg1) {
					//			System.out.println("RequestCompletedObserver called ...");
					CapsHttpRequestCompletedArg rcha = (CapsHttpRequestCompletedArg) arg1;
					if (rcha.getResult() == null)
					{
						JLogger.error("Failed retrieving ObjectMedia data");
						try { callback.execute( new ObjectMediaCallbackArgs(false, "", null)); }
						catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
						return;
					}

					LindenMessages.ObjectMediaMessage msg = new LindenMessages.ObjectMediaMessage();
					msg.Deserialize((OSDMap)rcha.getResult());

					if (msg.Request instanceof LindenMessages.ObjectMediaResponse)
					{
						LindenMessages.ObjectMediaResponse response = (LindenMessages.ObjectMediaResponse)msg.Request;

						if (Client.settings.OBJECT_TRACKING)
						{
//							final Primitive[] primarray = new Primitive[]{null}; 
//							Primitive prim = null;
//							sim.ObjectsPrimitives.foreach(new Action<Entry<Long, Primitive>>()
//									{
//								public void execute(
//										Entry<Long, Primitive> t) {
//									if(t.getValue().ID.equals(primID))
//									{
//										primarray[0] = t.getValue();
//									}
//								}
//									});
//							prim = primarray[0];
							
							Primitive prim = sim.ObjectsPrimitives.get(primID);

							//		                                Find((Primitive p) => { return p.ID == primID; });
							if (prim != null)
							{
								prim.MediaVersion = response.Version;
								prim.FaceMedia = response.FaceMedia;
							}
						}

						try { callback.execute( new ObjectMediaCallbackArgs(true, response.Version, response.FaceMedia)); }
						catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
					}
					else
					{
						try { callback.execute( new ObjectMediaCallbackArgs(false, "", null)); }
						catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
					}
				}
			});

			request.BeginGetResponse(req.Serialize(), OSDFormat.Xml, Client.settings.CAPS_TIMEOUT);
		}
		else
		{
			JLogger.error("ObjectMedia capability not available");
			try { callback.execute( new ObjectMediaCallbackArgs(false, "", null)); }
			catch (Exception ex) { JLogger.error(Utils.getExceptionStackTraceAsString(ex)); }
		}
	}
	//endregion


	//region Packet Handlers

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ObjectUpdateHandler(Object sender, PacketReceivedEventArgs e) throws Exception
	{
		JLogger.debug("Starting ObjectUpdateHandler ...");
		Packet packet = e.getPacket();
		final Simulator simulator = e.getSimulator();

		final ObjectUpdatePacket update = (ObjectUpdatePacket)packet;
		UpdateDilation(e.getSimulator(), update.RegionData.TimeDilation);

		for (int b = 0; b < update.ObjectData.length; b++)
		{
			//JLogger.debug("Decoding ObjectData index: " + b);
			ObjectUpdatePacket.ObjectDataBlock block = update.ObjectData[b];

			ObjectMovementUpdate objectupdate = new ObjectMovementUpdate();
			//Vector4 collisionPlane = Vector4.Zero;
			//Vector3 position;
			//Vector3 velocity;
			//Vector3 acceleration;
			//Quaternion rotation;
			//Vector3 angularVelocity;
			NameValue[] nameValues;
			boolean attachment = false;
			PCode pcode = PCode.get(block.PCode);

			//region Relevance check

			// Check if we are interested in this object
			if (!Client.settings.ALWAYS_DECODE_OBJECTS)
			{
				switch (pcode)
				{
				case Grass:
				case Tree:
				case NewTree:
				case Prim:
					JLogger.debug("Got a Grass, Tree, NewTree or Prim....");
					if (onObjectUpdate == null) continue;
					break;
				case Avatar:
					JLogger.debug("Got an Avatar...");
					// Make an exception for updates about our own agent
					if (!block.FullID.equals(Client.self.getAgentID()) && onAvatarUpdate == null) continue;
					break;
				case ParticleSystem:
					JLogger.debug("Got an Partical System ... Going to Next Block");
					continue; // TODO: Do something with these
				}
			}

			//endregion Relevance check

			//region NameValue parsing

			String nameValue = Utils.bytesWithTrailingNullByteToString(block.NameValue);
			if (nameValue.length() > 0)
			{
				String[] lines = nameValue.split("\n");
				nameValues = new NameValue[lines.length];

				for (int i = 0; i < lines.length; i++)
				{
					if (!Utils.isNullOrEmpty(lines[i]))
					{
						NameValue nv = new NameValue(lines[i]);
						if (nv.Name.equals("AttachItemID")) attachment = true;
						nameValues[i] = nv;
					}
				}
			}
			else
			{
				nameValues = new NameValue[0];
			}

			//JLogger.debug("Got NameValue String: " + nameValue + " $Parsed Values$ " + NameValue.NameValuesToString(nameValues));
			
			//endregion NameValue parsing

			//region Decode Object (primitive) parameters
			ConstructionData data = new ConstructionData();
			data.State = block.State;
			data.Material = Material.get(block.Material);
			data.PathCurve = PathCurve.get(block.PathCurve);
			data.profileCurve = block.ProfileCurve;
			data.PathBegin = Primitive.UnpackBeginCut(block.PathBegin);
			data.PathEnd = Primitive.UnpackEndCut(block.PathEnd);
			data.PathScaleX = Primitive.UnpackPathScale(block.PathScaleX);
			data.PathScaleY = Primitive.UnpackPathScale(block.PathScaleY);
			data.PathShearX = Primitive.UnpackPathShear((byte)block.PathShearX);
			data.PathShearY = Primitive.UnpackPathShear((byte)block.PathShearY);
			data.PathTwist = Primitive.UnpackPathTwist(block.PathTwist);
			data.PathTwistBegin = Primitive.UnpackPathTwist(block.PathTwistBegin);
			data.PathRadiusOffset = Primitive.UnpackPathTwist(block.PathRadiusOffset);
			data.PathTaperX = Primitive.UnpackPathTaper(block.PathTaperX);
			data.PathTaperY = Primitive.UnpackPathTaper(block.PathTaperY);
			data.PathRevolutions = Primitive.UnpackPathRevolutions(block.PathRevolutions);
			data.PathSkew = Primitive.UnpackPathTwist(block.PathSkew);
			data.ProfileBegin = Primitive.UnpackBeginCut(block.ProfileBegin);
			data.ProfileEnd = Primitive.UnpackEndCut(block.ProfileEnd);
			data.ProfileHollow = Primitive.UnpackProfileHollow(block.ProfileHollow);
			data.PCode = pcode;
			//endregion

			//region Decode Additional packed parameters in ObjectData
			int pos = 0;
			int i = block.ObjectData.length;
			
			//TODO following comment is only for debugging
			JLogger.debug("ObjectUpdateHandler Got Object Length : " + i);
			boolean search = true;
			while(search)
			{
				search = false;
				switch (i)
				{
				case 76:
					// Collision normal for avatar
					objectupdate.CollisionPlane = new Vector4(block.ObjectData, pos);
					pos += 16;
					
					//Keep on decoding 
					i = 60;
					search = true;
					break;
				case 60:
					// Position
					objectupdate.Position = new Vector3(block.ObjectData, pos);
					pos += 12;
					// Velocity
					objectupdate.Velocity = new Vector3(block.ObjectData, pos);
					pos += 12;
					// Acceleration
					objectupdate.Acceleration = new Vector3(block.ObjectData, pos);
					pos += 12;
					// Rotation (theta)
					objectupdate.Rotation = new Quaternion(block.ObjectData, pos, true);
					pos += 12;
					// Angular velocity (omega)
					objectupdate.AngularVelocity = new Vector3(block.ObjectData, pos);
					pos += 12;

					break;
				case 48:
					// Collision normal for avatar
					objectupdate.CollisionPlane = new Vector4(block.ObjectData, pos);
					pos += 16;
					
					//Keep on decoding
					i = 32;
					search = true;
					break;
				case 32:
					// The data is an array of unsigned shorts
					//TODO need to handle Little Endian Data
					// Position
					objectupdate.Position = new Vector3(
							Utils.UInt16ToFloat(block.ObjectData, pos, -0.5f * 256.0f, 1.5f * 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 2, -0.5f * 256.0f, 1.5f * 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 4, -256.0f, 3.0f * 256.0f));
					pos += 6;
					// Velocity
					objectupdate.Velocity = new Vector3(
							Utils.UInt16ToFloat(block.ObjectData, pos, -256.0f, 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 2, -256.0f, 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 4, -256.0f, 256.0f));
					pos += 6;
					// Acceleration
					objectupdate.Acceleration = new Vector3(
							Utils.UInt16ToFloat(block.ObjectData, pos, -256.0f, 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 2, -256.0f, 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 4, -256.0f, 256.0f));
					pos += 6;
					// Rotation (theta)
					objectupdate.Rotation = new Quaternion(
							Utils.UInt16ToFloat(block.ObjectData, pos, -1.0f, 1.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 2, -1.0f, 1.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 4, -1.0f, 1.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 6, -1.0f, 1.0f));
					pos += 8;
					// Angular velocity (omega)
					objectupdate.AngularVelocity = new Vector3(
							Utils.UInt16ToFloat(block.ObjectData, pos, -256.0f, 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 2, -256.0f, 256.0f),
							Utils.UInt16ToFloat(block.ObjectData, pos + 4, -256.0f, 256.0f));
					pos += 6;

					break;
				case 16:
					// The data is an array of single bytes (8-bit numbers)
					//TODO need to handle Little Endian Data
					// Position
					objectupdate.Position = new Vector3(
							Utils.byteToFloat(block.ObjectData, pos, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 1, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 2, -256.0f, 256.0f));
					pos += 3;
					// Velocity
					objectupdate.Velocity = new Vector3(
							Utils.byteToFloat(block.ObjectData, pos, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 1, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 2, -256.0f, 256.0f));
					pos += 3;
					// Accleration
					objectupdate.Acceleration = new Vector3(
							Utils.byteToFloat(block.ObjectData, pos, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 1, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 2, -256.0f, 256.0f));
					pos += 3;
					// Rotation
					objectupdate.Rotation = new Quaternion(
							Utils.byteToFloat(block.ObjectData, pos, -1.0f, 1.0f),
							Utils.byteToFloat(block.ObjectData, pos + 1, -1.0f, 1.0f),
							Utils.byteToFloat(block.ObjectData, pos + 2, -1.0f, 1.0f),
							Utils.byteToFloat(block.ObjectData, pos + 3, -1.0f, 1.0f));
					pos += 4;
					// Angular Velocity
					objectupdate.AngularVelocity = new Vector3(
							Utils.byteToFloat(block.ObjectData, pos, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 1, -256.0f, 256.0f),
							Utils.byteToFloat(block.ObjectData, pos + 2, -256.0f, 256.0f));
					pos += 3;

					break;
				default:
					JLogger.warn("Unhandled: Got an ObjectUpdate block with ObjectUpdate field length of " +
							block.ObjectData.length);
					continue;
				} //switch
			}
			//endregion

			if(objectupdate.Position == null)
				JLogger.warn("ObjectUpdateHandler: Object Position found to be null after update");
			
			// Determine the object type and create the appropriate class
			switch (pcode)
			{
			//region Prim and Foliage
			case Grass:
			case Tree:
			case NewTree:
			case Prim:

				boolean isNewObject;
				//	                        lock (simulator.ObjectsPrimitives.Dictionary)
				isNewObject = !simulator.ObjectsPrimitives.containsKey(block.ID);

				final Primitive prim = GetPrimitive(simulator, block.ID, block.FullID);

				// Textures
				objectupdate.Textures = new TextureEntry(block.TextureEntry, 0,
						block.TextureEntry.length);

				onObjectDataBlockUpdate.raiseEvent(new ObjectDataBlockUpdateEventArgs(simulator, prim, data, block, objectupdate, nameValues));

				//region Update Prim Info with decoded data
				prim.Flags = block.UpdateFlags;
				//JLogger.debug("Block UpdateFlags: " + block.UpdateFlags + Utils.bytesToHexDebugString(Utils.int64ToBytes(block.UpdateFlags), ""));
				
				if ((prim.Flags & PrimFlags.ZlibCompressed.getIndex()) != 0)
				{
					JLogger.warn("Got a ZlibCompressed ObjectUpdate, implement me!");
					continue;
				}

				// Automatically request ObjectProperties for prim if it was rezzed selected.
				if (( prim.Flags & PrimFlags.CreateSelected.getIndex()) != 0)
				{
					SelectObject(simulator, prim.LocalID);
				}

				prim.NameValues = nameValues;
				prim.LocalID = block.ID;
				prim.ID = block.FullID;
				prim.ParentID = block.ParentID;
				prim.RegionHandle = update.RegionData.RegionHandle;
				prim.Scale = block.Scale;
				prim.ClickAction = ClickAction.get(block.ClickAction);
				prim.OwnerID = block.OwnerID;
				prim.MediaURL = Utils.bytesWithTrailingNullByteToString(block.MediaURL);
				prim.Text = Utils.bytesWithTrailingNullByteToString(block.Text);
				prim.TextColor = new Color4(block.TextColor, 0, false, true);
				prim.IsAttachment = attachment;

				// Sound information
				prim.Sound = block.Sound;
				prim.SoundFlags = block.Flags;
				prim.SoundGain = block.Gain;
				prim.SoundRadius = block.Radius;

				// Joint information
				prim.Joint = JointType.get(block.JointType);
				prim.JointPivot = block.JointPivot;
				prim.JointAxisOrAnchor = block.JointAxisOrAnchor;

				// Object parameters
				prim.PrimData = data;

				// Textures, texture animations, particle system, and extra params
				prim.Textures = objectupdate.Textures;

				prim.TextureAnim = new TextureAnimation(block.TextureAnim, 0);
				
				//FIXME need to fix the following exception
				try{
					prim.ParticleSys = new ParticleSystem(block.PSBlock, 0);
				}
				catch(Exception ex)
				{
					JLogger.warn("Error while decoding ParticleSystem from packet: " 
							+ Utils.getExceptionStackTraceAsString(ex));
				}
				
				prim.SetExtraParamsFromBytes(block.ExtraParams, 0);

				// PCode-specific data
				switch (pcode)
				{
				case Grass:
				case Tree:
				case NewTree:
					if (block.Data.length == 1)
						prim.TreeSpecies = Tree.get(block.Data[0]);
					else
						JLogger.warn("Got a foliage update with an invalid TreeSpecies field");
					//    prim.ScratchPad = Utils.EmptyBytes;
					//    break;
					//default:
					//    prim.ScratchPad = new byte[block.Data.Length];
					//    if (block.Data.Length > 0)
					//        Buffer.BlockCopy(block.Data, 0, prim.ScratchPad, 0, prim.ScratchPad.Length);
					break;
				}
				prim.ScratchPad = Utils.EmptyBytes;

				// Packed parameters
				prim.CollisionPlane = objectupdate.CollisionPlane;
				prim.Position = objectupdate.Position;
				prim.Velocity = objectupdate.Velocity;
				prim.Acceleration = objectupdate.Acceleration;
				prim.Rotation = objectupdate.Rotation;
				prim.AngularVelocity = objectupdate.AngularVelocity;
				
				JLogger.debug("Primitive " + prim.LocalID + " Position " + prim.Position.toString());
				
				//endregion

				final boolean isNewObject2 = isNewObject;

				//				EventHandler<PrimEventArgs> handler = m_ObjectUpdate;
				if (onObjectUpdate != null)
				{
					final boolean attachment2 = attachment;
					threadPool.execute(new Runnable(){
						public void run()
						{
							onObjectUpdate.raiseEvent(new PrimEventArgs(simulator, prim, update.RegionData.TimeDilation, isNewObject2, attachment2));
						}
					});

					//					ThreadPool.QueueUserWorkItem(delegate(object o)
					//							{ handler(this, new PrimEventArgs(simulator, prim, update.RegionData.TimeDilation, isNewObject, attachment)); });
				}

				break;
				//endregion Prim and Foliage
				//region Avatar
			case Avatar:

				boolean isNewAvatar;
				//				lock (simulator.ObjectsAvatars.Dictionary)
				isNewAvatar = !simulator.ObjectsAvatars.containsKey(block.ID);

				if(block.FullID.equals(UUID.Zero))
					JLogger.warn(String.format("Received Avatar with Zero ID PCcode %d LocalID %d", block.PCode, block.ID));
				
				// Update some internals if this is our avatar
				if (block.FullID.equals(Client.self.getAgentID()) && simulator.equals(Client.network.getCurrentSim()))
				{
					//region Update Client.Self

					// We need the local ID to recognize terse updates for our agent
					Client.self.setLocalID(block.ID);

					// Packed parameters
					Client.self.setCollisionPlane(objectupdate.CollisionPlane);
					Client.self.setRelativePosition(objectupdate.Position);
					Client.self.setVelocity(objectupdate.Velocity);
					Client.self.setAcceleration(objectupdate.Acceleration);
					Client.self.setRelativeRotation(objectupdate.Rotation);
					Client.self.setAngularVelocity(objectupdate.AngularVelocity);

					//endregion
				}

				//region Create an Avatar from the decoded data

				Avatar avatar = GetAvatar(simulator, block.ID, block.FullID);

				objectupdate.Avatar = true;
				// Textures
				objectupdate.Textures = new TextureEntry(block.TextureEntry, 0,
						block.TextureEntry.length);

				onObjectDataBlockUpdate.raiseEvent(new ObjectDataBlockUpdateEventArgs(simulator, avatar, data, block, objectupdate, nameValues));

				long oldSeatID = avatar.ParentID;

				avatar.ID = block.FullID;
				avatar.LocalID = block.ID;
				avatar.Scale = block.Scale;
				avatar.CollisionPlane = objectupdate.CollisionPlane;
				avatar.Position = objectupdate.Position;
				avatar.Velocity = objectupdate.Velocity;
				avatar.Acceleration = objectupdate.Acceleration;
				avatar.Rotation = objectupdate.Rotation;
				avatar.AngularVelocity = objectupdate.AngularVelocity;
				avatar.NameValues = nameValues;
				avatar.PrimData = data;
				if (block.Data.length > 0)
				{
					JLogger.warn("Unexpected Data field for an avatar update, length " + block.Data.length);
				}
				avatar.ParentID = block.ParentID;
				avatar.RegionHandle = update.RegionData.RegionHandle;
				JLogger.debug("Set an avatar: " + avatar.getName() 
						+ "\n with name values:\n" + NameValue.NameValuesToString(avatar.NameValues));
				
				SetAvatarSittingOn(simulator, avatar, block.ParentID, oldSeatID);

				// Textures
				avatar.Textures = objectupdate.Textures;

				//endregion Create an Avatar from the decoded data

				onAvatarUpdate.raiseEvent(new AvatarUpdateEventArgs(simulator, avatar, update.RegionData.TimeDilation, isNewAvatar));

				break;
				//endregion Avatar
			case ParticleSystem:
				DecodeParticleUpdate(block);
				// TODO: Create a callback for particle updates
				break;
			default:
				JLogger.debug("Got an ObjectUpdate block with an unrecognized PCode " + pcode.toString());
				break;
			}
		}
	}

	protected void DecodeParticleUpdate(ObjectUpdatePacket.ObjectDataBlock block)
	{
		// TODO: Handle ParticleSystem ObjectUpdate blocks

		// float bounce_b
		// Vector4 scale_range
		// Vector4 alpha_range
		// Vector3 vel_offset
		// float dist_begin_fadeout
		// float dist_end_fadeout
		// UUID image_uuid
		// long flags
		// byte createme
		// Vector3 diff_eq_alpha
		// Vector3 diff_eq_scale
		// byte max_particles
		// byte initial_particles
		// float kill_plane_z
		// Vector3 kill_plane_normal
		// float bounce_plane_z
		// Vector3 bounce_plane_normal
		// float spawn_range
		// float spawn_frequency
		// float spawn_frequency_range
		// Vector3 spawn_direction
		// float spawn_direction_range
		// float spawn_velocity
		// float spawn_velocity_range
		// float speed_limit
		// float wind_weight
		// Vector3 current_gravity
		// float gravity_weight
		// float global_lifetime
		// float individual_lifetime
		// float individual_lifetime_range
		// float alpha_decay
		// float scale_decay
		// float distance_death
		// float damp_motion_factor
		// Vector3 wind_diffusion_factor
	}

	/// <summary>
	/// A terse object update, used when a transformation matrix or
	/// velocity/acceleration for an object changes but nothing else
	/// (scale/position/rotation/acceleration/velocity)
	/// </summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ImprovedTerseObjectUpdateHandler(Object sender, PacketReceivedEventArgs e)
	{
		Packet packet = e.getPacket();
		final Simulator simulator = e.getSimulator();

		final ImprovedTerseObjectUpdatePacket terse = (ImprovedTerseObjectUpdatePacket)packet;
		UpdateDilation(simulator, terse.RegionData.TimeDilation);

		for (int i = 0; i < terse.ObjectData.length; i++)
		{
			ImprovedTerseObjectUpdatePacket.ObjectDataBlock block = terse.ObjectData[i];

			try
			{
				int pos = 4;
				long localid = Utils.bytesToUIntLit(block.Data, 0);

				// Check if we are interested in this update
				if (!Client.settings.ALWAYS_DECODE_OBJECTS
						&& localid != Client.self.getLocalID()
						&& onTerseObjectUpdate == null)
				{
					continue;
				}

				//region Decode update data

				final ObjectMovementUpdate update = new ObjectMovementUpdate();

				// LocalID
				update.LocalID = localid;
				// State
				update.State = block.Data[pos++];
				// Avatar boolean
				update.Avatar = (block.Data[pos++] != 0);
				// Collision normal for avatar
				if (update.Avatar)
				{
					update.CollisionPlane = new Vector4(block.Data, pos);
					pos += 16;
				}
				// Position
				update.Position = new Vector3(block.Data, pos);
				pos += 12;
				// Velocity
				update.Velocity = new Vector3(
						Utils.UInt16ToFloat(block.Data, pos, -128.0f, 128.0f),
						Utils.UInt16ToFloat(block.Data, pos + 2, -128.0f, 128.0f),
						Utils.UInt16ToFloat(block.Data, pos + 4, -128.0f, 128.0f));
				pos += 6;
				// Acceleration
				update.Acceleration = new Vector3(
						Utils.UInt16ToFloat(block.Data, pos, -64.0f, 64.0f),
						Utils.UInt16ToFloat(block.Data, pos + 2, -64.0f, 64.0f),
						Utils.UInt16ToFloat(block.Data, pos + 4, -64.0f, 64.0f));
				pos += 6;
				// Rotation (theta)
				update.Rotation = new Quaternion(
						Utils.UInt16ToFloat(block.Data, pos, -1.0f, 1.0f),
						Utils.UInt16ToFloat(block.Data, pos + 2, -1.0f, 1.0f),
						Utils.UInt16ToFloat(block.Data, pos + 4, -1.0f, 1.0f),
						Utils.UInt16ToFloat(block.Data, pos + 6, -1.0f, 1.0f));
				pos += 8;
				// Angular velocity (omega)
				update.AngularVelocity = new Vector3(
						Utils.UInt16ToFloat(block.Data, pos, -64.0f, 64.0f),
						Utils.UInt16ToFloat(block.Data, pos + 2, -64.0f, 64.0f),
						Utils.UInt16ToFloat(block.Data, pos + 4, -64.0f, 64.0f));
				pos += 6;

				// Textures
				// FIXME: Why are we ignoring the first four bytes here?
				if (block.TextureEntry.length != 0)
					update.Textures = new TextureEntry(block.TextureEntry, 4, block.TextureEntry.length - 4);

				//endregion Decode update data
				
				final Primitive obj = !Client.settings.OBJECT_TRACKING ? null : (update.Avatar) ?
						(Primitive)GetAvatar(simulator, update.LocalID, UUID.Zero) :
							(Primitive)GetPrimitive(simulator, update.LocalID, UUID.Zero);

						// Fire the pre-emptive notice (before we stomp the object)
						//		                    EventHandler<TerseObjectUpdateEventArgs> handler = m_TerseObjectUpdate;
						//		                    if (handler != null)
						//		                    {
						//		                        ThreadPool.QueueUserWorkItem(delegate(object o)
						//		                        { handler(this, new TerseObjectUpdateEventArgs(simulator, obj, update, terse.RegionData.TimeDilation)); });
						//		                    }

						threadPool.execute(new Runnable(){
							public void run()
							{
								onTerseObjectUpdate.raiseEvent(new TerseObjectUpdateEventArgs(simulator, obj, update, terse.RegionData.TimeDilation));
							}
						});

						//region Update Client.Self
						if (update.LocalID == Client.self.getLocalID())
						{
							Client.self.setCollisionPlane(update.CollisionPlane);
							Client.self.setRelativePosition(update.Position);
							Client.self.setVelocity(update.Velocity);
							Client.self.setAcceleration(update.Acceleration);
							Client.self.setRelativeRotation(update.Rotation);
							Client.self.setAngularVelocity(update.AngularVelocity);
						}
						//endregion Update Client.Self
						if (Client.settings.OBJECT_TRACKING && obj != null)
						{
							obj.Position = update.Position;
							obj.Rotation = update.Rotation;
							obj.Velocity = update.Velocity;
							obj.CollisionPlane = update.CollisionPlane;
							obj.Acceleration = update.Acceleration;
							obj.AngularVelocity = update.AngularVelocity;
							obj.PrimData.State = update.State;
							if (update.Textures != null)
								obj.Textures = update.Textures;
						}

			}
			catch (Exception ex)
			{
				JLogger.warn(Utils.getExceptionStackTraceAsString(ex));
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ObjectUpdateCompressedHandler(Object sender, PacketReceivedEventArgs e) throws Exception
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		ObjectUpdateCompressedPacket update = (ObjectUpdateCompressedPacket)packet;

		for (int b = 0; b < update.ObjectData.length; b++)
		{
			ObjectUpdateCompressedPacket.ObjectDataBlock block = update.ObjectData[b];
			int i = 0;

			try
			{
				// UUID
				UUID FullID = new UUID(block.Data, 0);
				i += 16;
				// Local ID
				//uint
				long LocalID = Utils.bytesToUIntLit(block.Data, i); i += 4;
				//		                    		(uint)(block.Data[i++] + (block.Data[i++] << 8) +
				//		                        (block.Data[i++] << 16) + (block.Data[i++] << 24));

				// PCode
				PCode pcode = PCode.get(block.Data[i++]);

				//region Relevance check

				if (!Client.settings.ALWAYS_DECODE_OBJECTS)
				{
					switch (pcode)
					{
					case Grass:
					case Tree:
					case NewTree:
					case Prim:
						if (onObjectUpdate == null) continue;
						break;
					}
				}

				//endregion Relevance check

				boolean isNew;
				//		                    lock (simulator.ObjectsPrimitives.Dictionary)
				isNew = simulator.ObjectsPrimitives.containsKey(LocalID);

				Primitive prim = GetPrimitive(simulator, LocalID, FullID);

				prim.LocalID = LocalID;
				prim.ID = FullID;
				prim.Flags = block.UpdateFlags;
				prim.PrimData.PCode = pcode;

				//region Decode block and update Prim

				// State
				prim.PrimData.State = block.Data[i++];
				// CRC
				i += 4;
				// Material
				prim.PrimData.Material = Material.get(block.Data[i++]);
				// Click action
				prim.ClickAction = ClickAction.get(block.Data[i++]);
				// Scale
				prim.Scale = new Vector3(block.Data, i);
				i += 12;
				// Position
				prim.Position = new Vector3(block.Data, i);
				i += 12;
				// Rotation
				prim.Rotation = new Quaternion(block.Data, i, true);
				i += 12;
				// Compressed flags
				//EnumSet<CompressedFlags>
				long flags = Utils.bytesToUIntLit(block.Data, i);
				i += 4;

				prim.OwnerID = new UUID(block.Data, i);
				i += 16;

				// Angular velocity
				if ((flags & CompressedFlags.HasAngularVelocity.getIndex()) != 0)
				{
					prim.AngularVelocity = new Vector3(block.Data, i);
					i += 12;
				}

				// Parent ID
				if ((flags & CompressedFlags.HasParent.getIndex()) != 0)
				{
					//		                        prim.ParentID = (uint)(block.Data[i++] + (block.Data[i++] << 8) +
					//		                        (block.Data[i++] << 16) + (block.Data[i++] << 24));
					prim.ParentID = Utils.bytesToUIntLit(block.Data, i); i += 4;
				}
				else
				{
					prim.ParentID = 0;
				}

				// Tree data
				if ((flags & CompressedFlags.Tree.getIndex()) != 0)
				{
					prim.TreeSpecies = Tree.get(block.Data[i++]);
					//prim.ScratchPad = Utils.EmptyBytes;
				}
				// Scratch pad
				else if ((flags & CompressedFlags.ScratchPad.getIndex()) != 0)
				{
					prim.TreeSpecies = Tree.get((byte)0);

					int size = block.Data[i++];
					//prim.ScratchPad = new byte[size];
					//Buffer.BlockCopy(block.Data, i, prim.ScratchPad, 0, size);
					i += size;
				}
				prim.ScratchPad = Utils.EmptyBytes;

				// Floating text
				if ((flags & CompressedFlags.HasText.getIndex())  != 0)
				{
					String text = "";
					while (block.Data[i] != 0)
					{
						text += (char)block.Data[i];
						i++;
					}
					i++;

					// Floating text
					prim.Text = text;

					// Text color
					prim.TextColor = new Color4(block.Data, i, false);
					i += 4;
				}
				else
				{
					prim.Text = "";
				}

				// Media URL
				if ((flags & CompressedFlags.MediaURL.getIndex())  != 0)
				{
					String text = "";
					while (block.Data[i] != 0)
					{
						text += (char)block.Data[i];
						i++;
					}
					i++;

					prim.MediaURL = text;
				}

				// Particle system
				if ((flags & CompressedFlags.HasParticles.getIndex())  != 0)
				{
					prim.ParticleSys = new ParticleSystem(block.Data, i);
					i += 86;
				}

				// Extra parameters
				i += prim.SetExtraParamsFromBytes(block.Data, i);

				//Sound data
				if ((flags & CompressedFlags.HasSound.getIndex())  != 0)
				{
					prim.Sound = new UUID(block.Data, i);
					i += 16;

					prim.SoundGain = Utils.bytesToFloatLit(block.Data, i);
					i += 4;
					prim.SoundFlags = block.Data[i++];
					prim.SoundRadius = Utils.bytesToFloatLit(block.Data, i);
					i += 4;
				}

				// Name values
				if ((flags & CompressedFlags.HasNameValues.getIndex())  != 0)
				{
					String text = "";
					while (block.Data[i] != 0)
					{
						text += (char)block.Data[i];
						i++;
					}
					i++;

					// Parse the name values
					if (text.length() > 0)
					{
						String[] lines = text.split("\n");
						prim.NameValues = new NameValue[lines.length];

						for (int j = 0; j < lines.length; j++)
						{
							if (!Utils.isNullOrEmpty(lines[j]))
							{
								NameValue nv = new NameValue(lines[j]);
								prim.NameValues[j] = nv;
							}
						}
					}
				}

				prim.PrimData.PathCurve = PathCurve.get(block.Data[i++]);
				int pathBegin = Utils.bytesToUInt16Lit(block.Data, i); i += 2;
				prim.PrimData.PathBegin = Primitive.UnpackBeginCut(pathBegin);
				int pathEnd = Utils.bytesToUInt16Lit(block.Data, i); i += 2;
				prim.PrimData.PathEnd = Primitive.UnpackEndCut(pathEnd);
				prim.PrimData.PathScaleX = Primitive.UnpackPathScale(block.Data[i++]);
				prim.PrimData.PathScaleY = Primitive.UnpackPathScale(block.Data[i++]);
				prim.PrimData.PathShearX = Primitive.UnpackPathShear((byte)block.Data[i++]);
				prim.PrimData.PathShearY = Primitive.UnpackPathShear((byte)block.Data[i++]);
				prim.PrimData.PathTwist = Primitive.UnpackPathTwist((byte)block.Data[i++]);
				prim.PrimData.PathTwistBegin = Primitive.UnpackPathTwist((byte)block.Data[i++]);
				prim.PrimData.PathRadiusOffset = Primitive.UnpackPathTwist((byte)block.Data[i++]);
				prim.PrimData.PathTaperX = Primitive.UnpackPathTaper((byte)block.Data[i++]);
				prim.PrimData.PathTaperY = Primitive.UnpackPathTaper((byte)block.Data[i++]);
				prim.PrimData.PathRevolutions = Primitive.UnpackPathRevolutions(block.Data[i++]);
				prim.PrimData.PathSkew = Primitive.UnpackPathTwist((byte)block.Data[i++]);

				prim.PrimData.profileCurve = block.Data[i++];
				int profileBegin = Utils.bytesToUInt16Lit(block.Data, i); i += 2;
				prim.PrimData.ProfileBegin = Primitive.UnpackBeginCut(profileBegin);
				int profileEnd = Utils.bytesToUInt16Lit(block.Data, i); i += 2;
				prim.PrimData.ProfileEnd = Primitive.UnpackEndCut(profileEnd);
				int profileHollow = Utils.bytesToUInt16Lit(block.Data, i); i += 2;
				prim.PrimData.ProfileHollow = Primitive.UnpackProfileHollow(profileHollow);

				// TextureEntry
				int textureEntryLength = (int)Utils.bytesToUInt(block.Data, i);
				i += 4;
				prim.Textures = new TextureEntry(block.Data, i, textureEntryLength);
				i += textureEntryLength;

				// Texture animation
				if ((flags & CompressedFlags.TextureAnimation.getIndex())  != 0)
				{
					//int textureAnimLength = (int)Utils.BytesToUIntBig(block.Data, i);
					i += 4;
					prim.TextureAnim = new TextureAnimation(block.Data, i);
				}

				//endregion

				prim.IsAttachment = (flags & CompressedFlags.HasNameValues.getIndex())  != 0 && prim.ParentID != 0;

				//region Raise Events

				//		                    EventHandler<PrimEventArgs> handler = m_ObjectUpdate;
				if (onObjectUpdate != null)
					onObjectUpdate.raiseEvent( new PrimEventArgs(simulator, prim, update.RegionData.TimeDilation, isNew, prim.IsAttachment));

				//endregion
			}
			catch (IndexOutOfBoundsException ex)
			{
				JLogger.warn("Error decoding an ObjectUpdateCompressed packet\n" + ex.getMessage());
				JLogger.warn(block.toString());
			}
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ObjectUpdateCachedHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (Client.settings.ALWAYS_REQUEST_OBJECTS)
		{
			Packet packet = e.getPacket();
			Simulator simulator = e.getSimulator();

			ObjectUpdateCachedPacket update = (ObjectUpdateCachedPacket)packet;
			List<Long> ids = new ArrayList<Long>(update.ObjectData.length);

			// No object caching implemented yet, so request updates for all of these objects
			for (int i = 0; i < update.ObjectData.length; i++)
			{
				ids.add(update.ObjectData[i].ID);
			}

			RequestObjects(simulator, ids);
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void KillObjectHandler(Object sender, PacketReceivedEventArgs e)
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		KillObjectPacket kill = (KillObjectPacket)packet;

		// Notify first, so that handler has a chance to get a
		// reference from the ObjectTracker to the object being killed
		for (int i = 0; i < kill.ObjectData.length; i++)
		{
			onKillObject.raiseEvent(new KillObjectEventArgs(simulator, kill.ObjectData[i].ID));
		}


		synchronized (simulator.ObjectsPrimitives.getDictionary())
		{
			List<Long> removeAvatars = new ArrayList<Long>();
			List<Long> removePrims = new ArrayList<Long>();

			if (Client.settings.OBJECT_TRACKING)
			{
				long localID;
				for (int i = 0; i < kill.ObjectData.length; i++)
				{
					localID = kill.ObjectData[i].ID;

					if (simulator.ObjectsPrimitives.getDictionary().containsKey(localID))
						removePrims.add(localID);

					for (Entry<Long, Primitive> prim : simulator.ObjectsPrimitives.getDictionary().entrySet())
					{
						if (prim.getValue().ParentID == localID)
						{
							onKillObject.raiseEvent(new KillObjectEventArgs(simulator, prim.getKey()));
							removePrims.add(prim.getKey());
						}
					}
				}
			}

			if (Client.settings.AVATAR_TRACKING)
			{
				synchronized (simulator.ObjectsAvatars.getDictionary())
				{
					long localID;
					for (int i = 0; i < kill.ObjectData.length; i++)
					{
						localID = kill.ObjectData[i].ID;

						if (simulator.ObjectsAvatars.getDictionary().containsKey(localID))
							removeAvatars.add(localID);

						List<Long> rootPrims = new ArrayList<Long>();

						for (Entry<Long, Primitive> prim : simulator.ObjectsPrimitives.getDictionary().entrySet())
						{
							if (prim.getValue().ParentID == localID)
							{
								onKillObject.raiseEvent(new KillObjectEventArgs(simulator, prim.getKey()));
								removePrims.add(prim.getKey());
								rootPrims.add(prim.getKey());
							}
						}

						for (Entry<Long, Primitive> prim : simulator.ObjectsPrimitives.getDictionary().entrySet())
						{
							if (rootPrims.contains(prim.getValue().ParentID))
							{
								onKillObject.raiseEvent(new KillObjectEventArgs(simulator, prim.getKey()));
								removePrims.add(prim.getKey());
							}
						}
					}

					//Do the actual removing outside of the loops but still inside the lock.
					//This safely prevents the collection from being modified during a loop.
					for (long removeID : removeAvatars)
						simulator.ObjectsAvatars.getDictionary().remove(removeID);
				}
			}

			for (long removeID : removePrims)
				simulator.ObjectsPrimitives.getDictionary().remove(removeID);
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ObjectPropertiesHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		ObjectPropertiesPacket op = (ObjectPropertiesPacket)packet;
		ObjectPropertiesPacket.ObjectDataBlock[] datablocks = op.ObjectData;

		for (int i = 0; i < datablocks.length; ++i)
		{
			ObjectPropertiesPacket.ObjectDataBlock objectData = datablocks[i];
			final ObjectProperties props = new ObjectProperties();

			props.ObjectID = objectData.ObjectID;
			props.AggregatePerms = objectData.AggregatePerms;
			props.AggregatePermTextures = objectData.AggregatePermTextures;
			props.AggregatePermTexturesOwner = objectData.AggregatePermTexturesOwner;
			props.Permissions = new Permissions(objectData.BaseMask, objectData.EveryoneMask, objectData.GroupMask,
					objectData.NextOwnerMask, objectData.OwnerMask);
			props.Category = ObjectCategory.get((int)objectData.Category);
			props.CreationDate = Utils.unixTimeToDate(objectData.CreationDate.longValue());
			props.CreatorID = objectData.CreatorID;
			props.Description = Utils.bytesWithTrailingNullByteToString(objectData.Description);
			props.FolderID = objectData.FolderID;
			props.FromTaskID = objectData.FromTaskID;
			props.GroupID = objectData.GroupID;
			props.InventorySerial = objectData.InventorySerial;
			props.ItemID = objectData.ItemID;
			props.LastOwnerID = objectData.LastOwnerID;
			props.Name = Utils.bytesWithTrailingNullByteToString(objectData.Name);
			props.OwnerID = objectData.OwnerID;
			props.OwnershipCost = objectData.OwnershipCost;
			props.SalePrice = objectData.SalePrice;
			props.SaleType = SaleType.get(objectData.SaleType);
			props.SitName = Utils.bytesWithTrailingNullByteToString(objectData.SitName);
			props.TouchName = Utils.bytesWithTrailingNullByteToString(objectData.TouchName);

			int numTextures = objectData.TextureID.length / 16;
			props.TextureIDs = new UUID[numTextures];
			for (int j = 0; j < numTextures; ++j)
				props.TextureIDs[j] = new UUID(objectData.TextureID, j * 16);

			if (Client.settings.OBJECT_TRACKING)
			{
				//		                    Primitive findPrim = simulator.ObjectsPrimitives.Find(
				//		                        delegate(Primitive prim) { return prim.ID == props.ObjectID; });

//				final Primitive[] primarray = new Primitive[]{null}; 
//				Primitive findPrim = null;
//				simulator.ObjectsPrimitives.foreach(new Action<Entry<Long, Primitive>>()
//						{
//					public void execute(
//							Entry<Long, Primitive> t) {
//						if(t.getValue().ID.equals(props.ObjectID))
//						{
//							primarray[0] = t.getValue();
//						}
//					}
//						});
//				findPrim = primarray[0];

				Primitive findPrim = simulator.ObjectsPrimitives.get(props.ObjectID);
				
				if (findPrim != null)
				{
					onObjectPropertiesUpdated.raiseEvent(new ObjectPropertiesUpdatedEventArgs(simulator, findPrim, props));

					synchronized (simulator.ObjectsPrimitives.getDictionary())
					{
						if (simulator.ObjectsPrimitives.getDictionary().containsKey(findPrim.LocalID))
							simulator.ObjectsPrimitives.getDictionary().get(findPrim.LocalID).Properties = props;
					}
				}
			}

			onObjectProperties.raiseEvent(new ObjectPropertiesEventArgs(simulator, props));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void ObjectPropertiesFamilyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		Packet packet = e.getPacket();
		Simulator simulator = e.getSimulator();

		final ObjectPropertiesFamilyPacket op = (ObjectPropertiesFamilyPacket)packet;
		ObjectProperties props = new ObjectProperties();

		ReportType requestType = ReportType.get(op.ObjectData.RequestFlags);

		props.ObjectID = op.ObjectData.ObjectID;
//		ObjectCategory a;
		props.Category = ObjectCategory.get((int)op.ObjectData.Category);
		props.Description = Utils.bytesWithTrailingNullByteToString(op.ObjectData.Description);
		props.GroupID = op.ObjectData.GroupID;
		props.LastOwnerID = op.ObjectData.LastOwnerID;
		props.Name = Utils.bytesWithTrailingNullByteToString(op.ObjectData.Name);
		props.OwnerID = op.ObjectData.OwnerID;
		props.OwnershipCost = op.ObjectData.OwnershipCost;
		props.SalePrice = op.ObjectData.SalePrice;
		props.SaleType = SaleType.get(op.ObjectData.SaleType);
		props.Permissions.BaseMask = PermissionMask.get(op.ObjectData.BaseMask);
		props.Permissions.EveryoneMask = PermissionMask.get(op.ObjectData.EveryoneMask);
		props.Permissions.GroupMask = PermissionMask.get(op.ObjectData.GroupMask);
		props.Permissions.NextOwnerMask = PermissionMask.get(op.ObjectData.NextOwnerMask);
		props.Permissions.OwnerMask = PermissionMask.get(op.ObjectData.OwnerMask);

		if (Client.settings.OBJECT_TRACKING)
		{
			//		                Primitive findPrim = simulator.ObjectsPrimitives.Find(
			//		                        delegate(Primitive prim) { return prim.ID == op.ObjectData.ObjectID; });

//			final Primitive[] primarray = new Primitive[]{null}; 
//			Primitive findPrim = null;
//			simulator.ObjectsPrimitives.foreach(new Action<Entry<Long, Primitive>>()
//					{
//				public void execute(
//						Entry<Long, Primitive> t) {
//					if(t.getValue().ID.equals(op.ObjectData.ObjectID))
//					{
//						primarray[0] = t.getValue();
//					}
//				}
//					});
//			findPrim = primarray[0];

			Primitive findPrim = simulator.ObjectsPrimitives.get(op.ObjectData.ObjectID);
			
			if (findPrim != null)
			{
				synchronized (simulator.ObjectsPrimitives.getDictionary())
				{
					if (simulator.ObjectsPrimitives.getDictionary().containsKey(findPrim.LocalID))
					{
						if (simulator.ObjectsPrimitives.getDictionary().get(findPrim.LocalID).Properties == null)
							simulator.ObjectsPrimitives.getDictionary().get(findPrim.LocalID).Properties = new ObjectProperties();
						simulator.ObjectsPrimitives.getDictionary().get(findPrim.LocalID).Properties.SetFamilyProperties(props);
					}
				}
			}
		}

		onObjectPropertiesFamily.raiseEvent(new ObjectPropertiesFamilyEventArgs(simulator, props, requestType));
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void PayPriceReplyHandler(Object sender, PacketReceivedEventArgs e)
	{
		if (onPayPriceReply != null)
		{
			Packet packet = e.getPacket();
			Simulator simulator = e.getSimulator();

			PayPriceReplyPacket p = (PayPriceReplyPacket)packet;
			UUID objectID = p.ObjectData.ObjectID;
			int defaultPrice = p.ObjectData.DefaultPayPrice;
			int[] buttonPrices = new int[p.ButtonData.length];

			for (int i = 0; i < p.ButtonData.length; i++)
			{
				buttonPrices[i] = p.ButtonData[i].PayButton;
			}

			onPayPriceReply.raiseEvent(new PayPriceReplyEventArgs(simulator, objectID, defaultPrice, buttonPrices));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="capsKey"></param>
	/// <param name="message"></param>
	/// <param name="simulator"></param>
	protected void ObjectPhysicsPropertiesHandler(String capsKey, IMessage message, Simulator simulator)
	{
		ObjectPhysicsPropertiesMessage msg = (ObjectPhysicsPropertiesMessage)message;

		if (Client.settings.OBJECT_TRACKING)
		{
			for (int i = 0; i < msg.ObjectPhysicsProperties.length; i++)
			{
				synchronized (simulator.ObjectsPrimitives.getDictionary())
				{
					if (simulator.ObjectsPrimitives.getDictionary().containsKey(msg.ObjectPhysicsProperties[i].LocalID))
					{
						simulator.ObjectsPrimitives.getDictionary().get(msg.ObjectPhysicsProperties[i].LocalID).PhysicsProps = msg.ObjectPhysicsProperties[i];
					}
				}
			}
		}

		if (onPhysicsProperties != null)
		{
			for (int i = 0; i < msg.ObjectPhysicsProperties.length; i++)
			{
				onPhysicsProperties.raiseEvent(
						new PhysicsPropertiesEventArgs(simulator, msg.ObjectPhysicsProperties[i]));
			}
		}
	}

	//endregion Packet Handlers

	//region Utility Functions

	/// <summary>
	/// Setup construction data for a basic primitive shape
	/// </summary>
	/// <param name="type">Primitive shape to construct</param>
	/// <returns>Construction data that can be plugged into a <seealso cref="Primitive"/></returns>
	public static ConstructionData BuildBasicShape(PrimType type) throws NotSupportedException
	{
		ConstructionData prim = new ConstructionData();
		prim.PCode = PCode.Prim;
		prim.Material = Material.Wood;

		switch (type)
		{
		case Box:
			prim.profileCurve = ProfileCurve.Square.getIndex();
			prim.PathCurve = PathCurve.Line;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 1f;
			prim.PathScaleY = 1f;
			prim.PathRevolutions = 1f;
			break;
		case Cylinder:
			prim.profileCurve = ProfileCurve.Circle.getIndex();
			prim.PathCurve = PathCurve.Line;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 1f;
			prim.PathScaleY = 1f;
			prim.PathRevolutions = 1f;
			break;
		case Prism:
			prim.profileCurve = ProfileCurve.Square.getIndex();
			prim.PathCurve = PathCurve.Line;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 0f;
			prim.PathScaleY = 0f;
			prim.PathRevolutions = 1f;
			break;
		case Ring:
			prim.profileCurve = ProfileCurve.EqualTriangle.getIndex();
			prim.PathCurve = PathCurve.Circle;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 1f;
			prim.PathScaleY = 0.25f;
			prim.PathRevolutions = 1f;
			break;
		case Sphere:
			prim.profileCurve = ProfileCurve.HalfCircle.getIndex();
			prim.PathCurve = PathCurve.Circle;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 1f;
			prim.PathScaleY = 1f;
			prim.PathRevolutions = 1f;
			break;
		case Torus:
			prim.profileCurve = ProfileCurve.Circle.getIndex();
			prim.PathCurve = PathCurve.Circle;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 1f;
			prim.PathScaleY = 0.25f;
			prim.PathRevolutions = 1f;
			break;
		case Tube:
			prim.profileCurve = ProfileCurve.Square.getIndex();
			prim.PathCurve = PathCurve.Circle;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 1f;
			prim.PathScaleY = 0.25f;
			prim.PathRevolutions = 1f;
			break;
		case Sculpt:
			prim.profileCurve = ProfileCurve.Circle.getIndex();
			prim.PathCurve = PathCurve.Circle;
			prim.ProfileEnd = 1f;
			prim.PathEnd = 1f;
			prim.PathScaleX = 1f;
			prim.PathScaleY = 0.5f;
			prim.PathRevolutions = 1f;
			break;
		default:
			throw new NotSupportedException("Unsupported shape: " + type.toString());
		}

		return prim;
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="sim"></param>
	/// <param name="av"></param>
	/// <param name="localid"></param>
	/// <param name="oldSeatID"></param>
	protected void SetAvatarSittingOn(Simulator sim, Avatar av, long localid, long oldSeatID)
	{
		if (Client.network.getCurrentSim().equals(sim) && av.LocalID == Client.self.getLocalID())
		{
			Client.self.setSittingOn(localid);
		}

		av.ParentID = localid;


		if (onAvatarSitChanged != null && oldSeatID != localid)
		{
			onAvatarSitChanged.raiseEvent(new AvatarSitChangedEventArgs(sim, av, localid, oldSeatID));
		}
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="s"></param>
	/// <param name="dilation"></param>
	protected void UpdateDilation(Simulator s, long dilation)
	{
		s.Stats.Dilation = (float)dilation / 65535.0f;
	}


	/// <summary>
	/// Set the Shape data of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="prim">Data describing the prim shape</param>
	public void SetShape(Simulator simulator, long localID, ConstructionData prim)
	{
		ObjectShapePacket shape = new ObjectShapePacket();

		shape.AgentData.AgentID = Client.self.getAgentID();
		shape.AgentData.SessionID = Client.self.getSessionID();

		shape.ObjectData = new ObjectShapePacket.ObjectDataBlock[1];
		shape.ObjectData[0] = new ObjectShapePacket.ObjectDataBlock();

		shape.ObjectData[0].ObjectLocalID = localID;

		shape.ObjectData[0].PathCurve = (byte)prim.PathCurve.getIndex();
		shape.ObjectData[0].PathBegin = Primitive.PackBeginCut(prim.PathBegin);
		shape.ObjectData[0].PathEnd = Primitive.PackEndCut(prim.PathEnd);
		shape.ObjectData[0].PathScaleX = Primitive.PackPathScale(prim.PathScaleX);
		shape.ObjectData[0].PathScaleY = Primitive.PackPathScale(prim.PathScaleY);
		shape.ObjectData[0].PathShearX = (byte)Primitive.PackPathShear(prim.PathShearX);
		shape.ObjectData[0].PathShearY = (byte)Primitive.PackPathShear(prim.PathShearY);
		shape.ObjectData[0].PathTwist = Primitive.PackPathTwist(prim.PathTwist);
		shape.ObjectData[0].PathTwistBegin = Primitive.PackPathTwist(prim.PathTwistBegin);
		shape.ObjectData[0].PathRadiusOffset = Primitive.PackPathTwist(prim.PathRadiusOffset);
		shape.ObjectData[0].PathTaperX = Primitive.PackPathTaper(prim.PathTaperX);
		shape.ObjectData[0].PathTaperY = Primitive.PackPathTaper(prim.PathTaperY);
		shape.ObjectData[0].PathRevolutions = Primitive.PackPathRevolutions(prim.PathRevolutions);
		shape.ObjectData[0].PathSkew = Primitive.PackPathTwist(prim.PathSkew);

		shape.ObjectData[0].ProfileCurve = prim.profileCurve;
		shape.ObjectData[0].ProfileBegin = Primitive.PackBeginCut(prim.ProfileBegin);
		shape.ObjectData[0].ProfileEnd = Primitive.PackEndCut(prim.ProfileEnd);
		shape.ObjectData[0].ProfileHollow = Primitive.PackProfileHollow(prim.ProfileHollow);

		Client.network.SendPacket(shape, simulator);
	}

	/// <summary>
	/// Set the Material data of an object
	/// </summary>
	/// <param name="simulator">A reference to the <seealso cref="OpenMetaverse.Simulator"/> object where the object resides</param>
	/// <param name="localID">The objects ID which is local to the simulator the object is in</param>
	/// <param name="material">The new material of the object</param>
	public void SetMaterial(Simulator simulator, long localID, Material material)
	{
		ObjectMaterialPacket matPacket = new ObjectMaterialPacket();

		matPacket.AgentData.AgentID = Client.self.getAgentID();
		matPacket.AgentData.SessionID = Client.self.getSessionID();

		matPacket.ObjectData = new ObjectMaterialPacket.ObjectDataBlock[1];
		matPacket.ObjectData[0] = new ObjectMaterialPacket.ObjectDataBlock();

		matPacket.ObjectData[0].ObjectLocalID = localID;
		matPacket.ObjectData[0].Material = (byte)material.getIndex();

		Client.network.SendPacket(matPacket, simulator);
	}


	//endregion Utility Functions

	//region Object Tracking Link

	/// <summary>
	/// 
	/// </summary>
	/// <param name="simulator"></param>
	/// <param name="localID"></param>
	/// <param name="fullID"></param>
	/// <returns></returns>
	protected Primitive GetPrimitive(Simulator simulator, long localID, UUID fullID)
	{
		//TODO only for debugging
		JLogger.debug("Creating or Fetching Primitive with local ID" + localID);
		if (Client.settings.OBJECT_TRACKING)
		{
			synchronized (simulator.ObjectsPrimitives.getDictionary())
			{

				Primitive prim;

				if ((prim = simulator.ObjectsPrimitives.get(localID))!=null)
				{
					return prim;
				}
				else
				{
					prim = new Primitive();
					prim.LocalID = localID;
					prim.ID = fullID;
					prim.RegionHandle = simulator.Handle;

					simulator.ObjectsPrimitives.add(localID, prim);

					return prim;
				}
			}
		}
		else
		{
			return new Primitive();
		}
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="simulator"></param>
	/// <param name="localID"></param>
	/// <param name="fullID"></param>
	/// <returns></returns>
	protected Avatar GetAvatar(Simulator simulator, long localID, UUID fullID)
	{
		if (Client.settings.AVATAR_TRACKING)
		{
			synchronized (simulator.ObjectsPrimitives.getDictionary())
			{
				Avatar avatar;

				if ((avatar = simulator.ObjectsAvatars.get(localID))!=null)
				{
					JLogger.debug("Found Avatar: " + avatar.LocalID);
					return avatar;
				}
				else
				{
					avatar = new Avatar();
					avatar.LocalID = localID;
					avatar.ID = fullID;
					avatar.RegionHandle = simulator.Handle;

					simulator.ObjectsAvatars.add(localID, avatar);
					JLogger.debug(String.format("Added Avatar: ID %s LocalID %d", fullID, avatar.LocalID));
					return avatar;
				}
			}
		}
		else
		{
			return new Avatar();
		}
	}

	//endregion Object Tracking Link

	protected void InterpolationTimer_Elapsed(Object obj)
	{
		long elapsed = 0;

		if (Client.network.getConnected())
		{
			//			int start = Environment.TickCount;
			long start = Utils.getUnixTime();

			long interval = Utils.getUnixTime() - Client.self.lastInterpolation;
			float seconds = (float)interval / 1000f;

			// Iterate through all of the simulators
			Simulator[] sims = Client.network.Simulators.toArray(new Simulator[0]);
			for (int i = 0; i < sims.length; i++)
			{
				Simulator sim = sims[i];

				final float adjSeconds = seconds * sim.Stats.Dilation;

				// Iterate through all of this sims avatars
				sim.ObjectsAvatars.foreach(new Action<Entry<Long, Avatar>>()
						{
					public void execute(Entry<Long, Avatar> e) {
						Avatar avatar = e.getValue();
						//region Linear Motion
						// Only do movement interpolation (extrapolation) when there is a non-zero velocity but 
						// no acceleration
						if (avatar.Acceleration.equals(Vector3.Zero) && avatar.Velocity.equals(Vector3.Zero))
						{
							avatar.Position = Vector3.add(avatar.Position, 
									Vector3.multiply((Vector3.add(avatar.Velocity, Vector3.multiply(avatar.Acceleration,
											(0.5f * (adjSeconds - HAVOK_TIMESTEP))))), adjSeconds));
							avatar.Velocity = Vector3.add(avatar.Velocity,  Vector3.multiply(avatar.Acceleration, adjSeconds));
						}
						//endregion Linear Motion	
					}
						}
						);

				// Iterate through all of this sims primitives
				sim.ObjectsPrimitives.foreach(new Action<Entry<Long, Primitive>>()
						{
					public void execute(Entry<Long, Primitive> e) 
					{
						Primitive prim = e.getValue();
						if (prim.Joint.equals(JointType.Invalid))
						{
							//region Angular Velocity
							Vector3 angVel = prim.AngularVelocity;
							float omega = angVel.lengthSquared();

							if (omega > 0.00001f)
							{
								omega = (float)Math.sqrt(omega);
								float angle = omega * adjSeconds;
								angVel = Vector3.multiply(angVel, 1.0f / omega);
								Quaternion dQ = Quaternion.createFromAxisAngle(angVel, angle);

								prim.Rotation = Quaternion.multiply(prim.Rotation, dQ);
							}
							//endregion Angular Velocity

							//region Linear Motion
							// Only do movement interpolation (extrapolation) when there is a non-zero velocity but 
							// no acceleration
							if (Vector3.notEquals(prim.Acceleration, Vector3.Zero) && Vector3.equals(prim.Velocity, Vector3.Zero))
							{
								prim.Position = Vector3.add(prim.Position, Vector3.multiply(Vector3.add(prim.Velocity, Vector3.multiply(prim.Acceleration, 
										(0.5f * (adjSeconds - HAVOK_TIMESTEP)))), adjSeconds));
								prim.Velocity = Vector3.add(prim.Velocity, Vector3.multiply(prim.Acceleration , adjSeconds));
							}
							//endregion Linear Motion
						}
						else if (prim.Joint == JointType.Hinge)
						{
							//FIXME: Hinge movement extrapolation
						}
						else if (prim.Joint == JointType.Point)
						{
							//FIXME: Point movement extrapolation
						}
						else
						{
							JLogger.warn("Unhandled joint type " + prim.Joint);
						}
					}});
			}

			// Make sure the last interpolated time is always updated
			Client.self.lastInterpolation = Utils.getUnixTime();

			elapsed = Client.self.lastInterpolation - start;
		}

		// Start the timer again. Use a minimum of a 50ms pause in between calculations
		long delay = Math.max(50, Settings.INTERPOLATION_INTERVAL - elapsed);
		if (InterpolationTimer != null)
		{
			//			InterpolationTimer.Change(delay, Timeout.Infinite);
			updateInterpolationTimer(delay);
		}

	}
}

