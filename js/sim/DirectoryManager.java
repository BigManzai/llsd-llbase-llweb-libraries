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
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Date;
import java.util.Map.Entry;
import java.util.Observable;

import com.ngt.jopenmetaverse.shared.protocol.DirClassifiedQueryPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirClassifiedReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirEventsReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirFindQueryPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirGroupsReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirLandQueryPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirLandReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirPeopleReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirPlacesQueryPacket;
import com.ngt.jopenmetaverse.shared.protocol.DirPlacesReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.EventInfoReplyPacket;
import com.ngt.jopenmetaverse.shared.protocol.EventInfoRequestPacket;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.protocol.Packet;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.PlacesQueryPacket;
import com.ngt.jopenmetaverse.shared.protocol.PlacesReplyPacket;
import com.ngt.jopenmetaverse.shared.sim.ParcelManager.ParcelCategory;
import com.ngt.jopenmetaverse.shared.sim.events.CapsEventObservableArg;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.dm.*;
import com.ngt.jopenmetaverse.shared.sim.interfaces.IMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.DirLandReplyMessage;
import com.ngt.jopenmetaverse.shared.sim.message.LindenMessages.PlacesReplyMessage;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class DirectoryManager {

	//region Enums
	/// <summary>Classified Ad categories</summary>
	public enum ClassifiedCategories
	{
		/// <summary>Classified is listed in the Any category</summary>
		Any (0),
		/// <summary>Classified is shopping related</summary>
		Shopping (1),
		/// <summary>Classified is </summary>
		LandRental (2),
		/// <summary></summary>
		PropertyRental (3),
		/// <summary></summary>
		SpecialAttraction (4),
		/// <summary></summary>
		NewProducts (5),
		/// <summary></summary>
		Employment (6),
		/// <summary></summary>
		Wanted (7),
		/// <summary></summary>
		Service (8),
		/// <summary></summary>
		Personal (9);
		private int index;
		ClassifiedCategories(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,ClassifiedCategories> lookup  
		= new HashMap<Integer,ClassifiedCategories>();

		static {
			for(ClassifiedCategories s : EnumSet.allOf(ClassifiedCategories.class))
				lookup.put(s.getIndex(), s);
		}

		public static ClassifiedCategories get(Integer index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>Event Categories</summary>
	public enum EventCategories
	{
		/// <summary></summary>
		All(0),
		/// <summary></summary>
		Discussion(18),
		/// <summary></summary>
		Sports(19),
		/// <summary></summary>
		LiveMusic(20),
		/// <summary></summary>
		Commercial(22),
		/// <summary></summary>
		Nightlife(23),
		/// <summary></summary>
		Games(24),
		/// <summary></summary>
		Pageants(25),
		/// <summary></summary>
		Education(26),
		/// <summary></summary>
		Arts(27),
		/// <summary></summary>
		Charity(28),
		/// <summary></summary>
		Miscellaneous(29);
		private int index;
		EventCategories(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,EventCategories> lookup  
		= new HashMap<Integer,EventCategories>();

		static {
			for(EventCategories s : EnumSet.allOf(EventCategories.class))
				lookup.put(s.getIndex(), s);
		}

		public static EventCategories get(Integer index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Query Flags used in many of the DirectoryManager methods to specify which query to execute and how to return the results.
	/// 
	/// Flags can be combined using the | (pipe) character, not all flags are available in all queries
	/// </summary>
	//    [Flags]
	public enum DirFindFlags
	{
		/// <summary>Query the People database</summary>
		People(1 << 0),
		/// <summary></summary>
		Online(1 << 1),
		// <summary></summary>
		//[Obsolete]
		//Places = 1 << 2,
		/// <summary></summary>
		Events(1 << 3),
		/// <summary>Query the Groups database</summary>
		Groups(1 << 4),
		/// <summary>Query the Events database</summary>
		DateEvents(1 << 5),
		/// <summary>Query the land holdings database for land owned by the currently connected agent</summary>
		AgentOwned(1 << 6),
		/// <summary></summary>
		ForSale(1 << 7),
		/// <summary>Query the land holdings database for land which is owned by a Group</summary>
		GroupOwned(1 << 8),
		// <summary></summary>
		//[Obsolete]
		//Auction = 1 << 9,
		/// <summary>Specifies the query should pre sort the results based upon traffic
		/// when searching the Places database</summary>
		DwellSort(1 << 10),
		/// <summary></summary>
		PgSimsOnly(1 << 11),
		/// <summary></summary>
		PicturesOnly(1 << 12),
		/// <summary></summary>
		PgEventsOnly(1 << 13),
		/// <summary></summary>
		MatureSimsOnly(1 << 14),
		/// <summary>Specifies the query should pre sort the results in an ascending order when searching the land sales database. 
		/// This flag is only used when searching the land sales database</summary>
		SortAsc(1 << 15),
		/// <summary>Specifies the query should pre sort the results using the SalePrice field when searching the land sales database. 
		/// This flag is only used when searching the land sales database</summary>
		PricesSort(1 << 16),
		/// <summary>Specifies the query should pre sort the results by calculating the average price/sq.m (SalePrice / Area) when searching the land sales database. 
		/// This flag is only used when searching the land sales database</summary>
		PerMeterSort(1 << 17),
		/// <summary>Specifies the query should pre sort the results using the ParcelSize field when searching the land sales database. 
		/// This flag is only used when searching the land sales database</summary>
		AreaSort( 1 << 18),
		/// <summary>Specifies the query should pre sort the results using the Name field when searching the land sales database. 
		/// This flag is only used when searching the land sales database</summary>
		NameSort(1 << 19),
		/// <summary>When set, only parcels less than the specified Price will be included when searching the land sales database.
		/// This flag is only used when searching the land sales database</summary>
		LimitByPrice(1 << 20),
		/// <summary>When set, only parcels greater than the specified Size will be included when searching the land sales database.
		/// This flag is only used when searching the land sales database</summary>
		LimitByArea(1 << 21),
		/// <summary></summary>
		FilterMature(1 << 22),
		/// <summary></summary>
		PGOnly(1 << 23),
		/// <summary>Include PG land in results. This flag is used when searching both the Groups, Events and Land sales databases</summary>
		IncludePG(1 << 24),
		/// <summary>Include Mature land in results. This flag is used when searching both the Groups, Events and Land sales databases</summary>
		IncludeMature(1 << 25),
		/// <summary>Include Adult land in results. This flag is used when searching both the Groups, Events and Land sales databases</summary>
		IncludeAdult(1 << 26),
		/// <summary></summary>
		AdultOnly(1 << 27);
		private int index;
		DirFindFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,DirFindFlags> lookup  
		= new HashMap<Integer,DirFindFlags>();

		static {
			for(DirFindFlags s : EnumSet.allOf(DirFindFlags.class))
				lookup.put(s.getIndex(), s);
		}

		   public static EnumSet<DirFindFlags> get(Integer index)
           {
                   EnumSet<DirFindFlags> enumsSet = EnumSet.allOf(DirFindFlags.class);
                   for(Entry<Integer,DirFindFlags> entry: lookup.entrySet())
                   {
                           if((entry.getKey().longValue() | index) != index)
                           {
                                   enumsSet.remove(entry.getValue());
                           }
                   }
                   return enumsSet;
           }
           
           public static int getIndex(EnumSet<DirFindFlags> enumSet)
           {
                   int ret = 0;
                   for(DirFindFlags s: enumSet)
                   {
                           ret |= s.getIndex();
                   }
                   return ret;
           }

	}

	/// <summary>
	/// Land types to search dataserver for
	/// </summary>
	//    [Flags]
	public enum SearchTypeFlags
	{
		/// <summary>Search Auction, Mainland and Estate</summary>
		Any(-1),
		/// <summary>Land which is currently up for auction</summary>
		Auction(1 << 1),
		// <summary>Land available to new landowners (formerly the FirstLand program)</summary>
		//[Obsolete]
		//Newbie = 1 << 2,
		/// <summary>Parcels which are on the mainland (Linden owned) continents</summary>
		Mainland(1 << 3),
		/// <summary>Parcels which are on privately owned simulators</summary>
		Estate(1 << 4);
		private int index;
		SearchTypeFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,SearchTypeFlags> lookup  
		= new HashMap<Integer,SearchTypeFlags>();

		static {
			for(SearchTypeFlags s : EnumSet.allOf(SearchTypeFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static SearchTypeFlags get(Integer index)
		{
			return lookup.get(index);
		}
		
		 public static EnumSet<SearchTypeFlags> get(Long index)
         {
                 EnumSet<SearchTypeFlags> enumsSet = EnumSet.allOf(SearchTypeFlags.class);
                 for(Entry<Integer,SearchTypeFlags> entry: lookup.entrySet())
                 {
                         if((entry.getKey().longValue() | index) != index)
                         {
                                 enumsSet.remove(entry.getValue());
                         }
                 }
                 return enumsSet;
         }
         
         public static int getIndex(EnumSet<SearchTypeFlags> enumSet)
         {
                 int ret = 0;
                 for(SearchTypeFlags s: enumSet)
                 {
                         ret |= s.getIndex();
                 }
                 return ret;
         }

	}

	/// <summary>
	/// The content rating of the event
	/// </summary>
	public enum EventFlags
	{
		/// <summary>Event is PG</summary>
		PG(0),
		/// <summary>Event is Mature</summary>
		Mature(1),
		/// <summary>Event is Adult</summary>
		Adult(2);
		private int index;
		EventFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,EventFlags> lookup  
		= new HashMap<Integer,EventFlags>();

		static {
			for(EventFlags s : EnumSet.allOf(EventFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static EventFlags get(Integer index)
		{
			return lookup.get(index);
		}
	}

	/// <summary>
	/// Classified Ad Options
	/// </summary>
	/// <remarks>There appear to be two formats the flags are packed in.
	/// This set of flags is for the newer style</remarks>
	//    [Flags]
	public enum ClassifiedFlags 
	{
		/// <summary></summary>
		None ((byte)(1 << 0)),
		/// <summary></summary>
		Mature ((byte)(1 << 1)),
		/// <summary></summary>
		Enabled ((byte)(1 << 2)),
		// HasPrice = 1 << 3, // Deprecated
		/// <summary></summary>
		UpdateTime ((byte)(1 << 4)),
		/// <summary></summary>
		AutoRenew ((byte)(1 << 5));
		private byte index;
		ClassifiedFlags(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,ClassifiedFlags> lookup  = new HashMap<Byte,ClassifiedFlags>();

		static {
			for(ClassifiedFlags s : EnumSet.allOf(ClassifiedFlags.class))
				lookup.put(s.getIndex(), s);
		}

        public static EnumSet<ClassifiedFlags> get(Byte index)
        {
                EnumSet<ClassifiedFlags> enumsSet = EnumSet.allOf(ClassifiedFlags.class);
                for(Entry<Byte,ClassifiedFlags> entry: lookup.entrySet())
                {
                        if((entry.getKey().byteValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }
        
        public static byte getIndex(EnumSet<ClassifiedFlags> enumSet)
        {
                byte ret = 0;
                for(ClassifiedFlags s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }

	}

	/// <summary>
	/// Classified ad query options
	/// </summary>
	//    [Flags]
	public enum ClassifiedQueryFlags
	{
		/// <summary>Include all ads in results</summary>
		All((1<<2) | (1 << 3) | (1 << 6)),
		/// <summary>Include PG ads in results</summary>
		PG(1 << 2),
		/// <summary>Include Mature ads in results</summary>
		Mature(1 << 3),
		/// <summary>Include Adult ads in results</summary>
		Adult(1 << 6);
		private int index;

		ClassifiedQueryFlags(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		private static final Map<Integer,ClassifiedQueryFlags> lookup  
		= new HashMap<Integer,ClassifiedQueryFlags>();

		static {
			for(ClassifiedQueryFlags s : EnumSet.allOf(ClassifiedQueryFlags.class))
				lookup.put(s.getIndex(), s);
		}

		 public static EnumSet<ClassifiedQueryFlags> get(Long index)
         {
                 EnumSet<ClassifiedQueryFlags> enumsSet = EnumSet.allOf(ClassifiedQueryFlags.class);
                 for(Entry<Integer,ClassifiedQueryFlags> entry: lookup.entrySet())
                 {
                         if((entry.getKey().intValue() | index) != index)
                         {
                                 enumsSet.remove(entry.getValue());
                         }
                 }
                 return enumsSet;
         }

         public static int getIndex(EnumSet<ClassifiedQueryFlags> enumSet)
         {
                 int ret = 0;
                 for(ClassifiedQueryFlags s: enumSet)
                 {
                         ret |= s.getIndex();
                 }
                 return ret;
         }

	}

	/// <summary>
	/// The For Sale flag in PlacesReplyData
	/// </summary>
	public enum PlacesFlags 
	{
		/// <summary>Parcel is not listed for sale</summary>
		NotForSale((byte)0),
		/// <summary>Parcel is For Sale</summary>
		ForSale((byte)128);
		private byte index;
		PlacesFlags(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}

		private static final Map<Byte,PlacesFlags> lookup  = new HashMap<Byte,PlacesFlags>();

		static {
			for(PlacesFlags s : EnumSet.allOf(PlacesFlags.class))
				lookup.put(s.getIndex(), s);
		}

		public static PlacesFlags get(Byte index)
		{
			return lookup.get(index);
		}
	}

	//endregion
	//region Structs
	/// <summary>
	/// A classified ad on the grid
	/// </summary>
	public class Classified
	{
		/// <summary>UUID for this ad, useful for looking up detailed
		/// information about it</summary>
		public UUID ID;
		/// <summary>The title of this classified ad</summary>
		public String Name;
		/// <summary>Flags that show certain options applied to the classified</summary>
		public EnumSet<ClassifiedFlags> Flags;
		/// <summary>Creation date of the ad</summary>
		public Date CreationDate;
		/// <summary>Expiration date of the ad</summary>
		public Date ExpirationDate;
		/// <summary>Price that was paid for this ad</summary>
		public int Price;

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A string containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			return Helpers.StructToStringWithOutException(this);
		}
	}

	/// <summary>
	/// A parcel retrieved from the dataserver such as results from the 
	/// "For-Sale" listings or "Places" Search
	/// </summary>
	public class DirectoryParcel
	{
		/// <summary>The unique dataserver parcel ID</summary>
		/// <remarks>This id is used to obtain additional information from the entry
		/// by using the <see cref="ParcelManager.InfoRequest"/> method</remarks>
		public UUID ID;
		/// <summary>A string containing the name of the parcel</summary>
		public String Name;
		/// <summary>The size of the parcel</summary>
		/// <remarks>This field is not returned for Places searches</remarks>
		public int ActualArea;
		/// <summary>The price of the parcel</summary>
		/// <remarks>This field is not returned for Places searches</remarks>
		public int SalePrice;
		/// <summary>If True, this parcel is flagged to be auctioned</summary>
		public boolean Auction;
		/// <summary>If true, this parcel is currently set for sale</summary>
		public boolean ForSale;
		/// <summary>Parcel traffic</summary>
		public float Dwell;

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A string containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			return Helpers.StructToStringWithOutException(this);
		}
	}

	/// <summary>
	/// An Avatar returned from the dataserver
	/// </summary>
	public class AgentSearchData
	{
		/// <summary>Online status of agent</summary>
		/// <remarks>This field appears to be obsolete and always returns false</remarks>
		public boolean Online;
		/// <summary>The agents first name</summary>
		public String FirstName;
		/// <summary>The agents last name</summary>
		public String LastName;
		/// <summary>The agents <see cref="UUID"/></summary>
		public UUID AgentID;

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A string containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			return Helpers.StructToStringWithOutException(this);
		}
	}

	/// <summary>
	///  Response to a "Groups" Search
	/// </summary>
	public class GroupSearchData
	{
		/// <summary>The Group ID</summary>
		public UUID GroupID;
		/// <summary>The name of the group</summary>
		public String GroupName;
		/// <summary>The current number of members</summary>
		public int Members;

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A string containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			return Helpers.StructToStringWithOutException(this);
		}
	}

	/// <summary>
	/// Parcel information returned from a <see cref="StartPlacesSearch"/> request
	/// <para>
	/// Represents one of the following:
	/// A parcel of land on the grid that has its Show In Search flag set
	/// A parcel of land owned by the agent making the request
	/// A parcel of land owned by a group the agent making the request is a member of
	/// </para>
	/// <para>
	/// In a request for Group Land, the First record will contain an empty record
	/// </para>
	/// Note: This is not the same as searching the land for sale data source
	/// </summary>
	public class PlacesSearchData
	{
		/// <summary>The ID of the Agent of Group that owns the parcel</summary>
		public UUID OwnerID;
		/// <summary>The name</summary>
		public String Name;
		/// <summary>The description</summary>
		public String Desc;
		/// <summary>The Size of the parcel</summary>
		public int ActualArea;
		/// <summary>The billable Size of the parcel, for mainland
		/// parcels this will match the ActualArea field. For Group owned land this will be 10 percent smaller
		/// than the ActualArea. For Estate land this will always be 0</summary>
		public int BillableArea;
		/// <summary>Indicates the ForSale status of the parcel</summary>
		public PlacesFlags Flags;
		/// <summary>The Gridwide X position</summary>
		public float GlobalX;
		/// <summary>The Gridwide Y position</summary>
		public float GlobalY;
		/// <summary>The Z position of the parcel, or 0 if no landing point set</summary>
		public float GlobalZ;
		/// <summary>The name of the Region the parcel is located in</summary>
		public String SimName;
		/// <summary>The Asset ID of the parcels Snapshot texture</summary>
		public UUID SnapshotID;
		/// <summary>The calculated visitor traffic</summary>
		public float Dwell;
		/// <summary>The billing product SKU</summary>
		/// <remarks>Known values are:
		/// <list type="table">
		/// <item><term>023</term><description>Mainland / Full Region</description></item>
		/// <item><term>024</term><description>Estate / Full Region</description></item>
		/// <item><term>027</term><description>Estate / Openspace</description></item>
		/// <item><term>029</term><description>Estate / Homestead</description></item>
		/// <item><term>129</term><description>Mainland / Homestead (Linden Owned)</description></item>
		/// </list>
		/// </remarks>
		public String SKU;
		/// <summary>No longer used, will always be 0</summary>
		public int Price;

		/// <summary>Get a SL URL for the parcel</summary>
		/// <returns>A string, containing a standard SLURL</returns>
		public String toSLurl()
		{
			float[] xy = new float[2];

			Helpers.GlobalPosToRegionHandle(this.GlobalX, this.GlobalY, xy);
			return "secondlife://" + this.SimName + "/" + xy[0] + "/" + xy[1] + "/" + this.GlobalZ;
		}

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A string containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			return Helpers.StructToStringWithOutException(this);
		}
	}

	/// <summary>
	/// An "Event" Listing summary
	/// </summary>
	public class EventsSearchData
	{
		/// <summary>The ID of the event creator</summary>
		public UUID Owner;
		/// <summary>The name of the event</summary>
		public String Name;
		/// <summary>The events ID</summary>
		//uint
		public long ID;
		/// <summary>A string containing the short date/time the event will begin</summary>
		public String Date;
		/// <summary>The event start time in Unixtime (seconds since epoch)</summary>
		//uint
		public long Time;
		/// <summary>The events maturity rating</summary>
		public EventFlags Flags;

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A string containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			return Helpers.StructToStringWithOutException(this);
		}
	}

	/// <summary>
	/// The details of an "Event"
	/// </summary>
	public class EventInfo
	{
		/// <summary>The events ID</summary>
		//uint
		public long ID;
		/// <summary>The ID of the event creator</summary>
		public UUID Creator;
		/// <summary>The name of the event</summary>
		public String Name;
		/// <summary>The category</summary>
		public EventCategories Category;
		/// <summary>The events description</summary>
		public String Desc;
		/// <summary>The short date/time the event will begin</summary>
		public String Date;
		/// <summary>The event start time in Unixtime (seconds since epoch) UTC adjusted</summary>
		//uint
		public long DateUTC;
		/// <summary>The length of the event in minutes</summary>
		//uint
		public long Duration;
		/// <summary>0 if no cover charge applies</summary>
		//uint
		public long Cover;
		/// <summary>The cover charge amount in L$ if applicable</summary>
		//uint
		public long Amount;
		/// <summary>The name of the region where the event is being held</summary>
		public String SimName;
		/// <summary>The gridwide location of the event</summary>
		public Vector3d GlobalPos;
		/// <summary>The maturity rating</summary>
		public EventFlags Flags;

		/// <summary>Get a SL URL for the parcel where the event is hosted</summary>
		/// <returns>A string, containing a standard SLURL</returns>
		public String toSLurl()
		{
			float[] xy = new float[2];

			Helpers.GlobalPosToRegionHandle((float)this.GlobalPos.X, (float)this.GlobalPos.Y, xy);
			return "secondlife://" + this.SimName + "/" + xy[0] + "/" + xy[1] + "/" + this.GlobalPos.Z;
		}

		/// <summary>Print the struct data as a string</summary>
		/// <returns>A string containing the field name, and field value</returns>
		@Override
		public String toString()
		{
			return Helpers.StructToStringWithOutException(this);
		}
	}

	//endregion Structs


    //region Event delegates, Raise Events
    /// <summary>Raised when the data server responds to a <see cref="EventInfoRequest"/> request.</summary>
	private EventObservable<EventInfoReplyEventArgs> OnEventInfo = new EventObservable<EventInfoReplyEventArgs>();
    /// <summary>Raised when the data server responds to a <see cref="StartEventsSearch"/> request.</summary>
	private EventObservable<DirEventsReplyEventArgs> OnDirEvents = new EventObservable<DirEventsReplyEventArgs>();
    /// <summary>Raised when the data server responds to a <see cref="StartPlacesSearch"/> request.</summary>
	private EventObservable<PlacesReplyEventArgs> OnPlaces = new EventObservable<PlacesReplyEventArgs>();
    /// <summary>Raised when the data server responds to a <see cref="StartDirPlacesSearch"/> request.</summary>
	private EventObservable<DirPlacesReplyEventArgs> OnDirPlaces = new EventObservable<DirPlacesReplyEventArgs>();

    /// <summary>Raised when the data server responds to a <see cref="StartClassifiedSearch"/> request.</summary>
	private EventObservable<DirClassifiedsReplyEventArgs> OnDirClassifieds = new EventObservable<DirClassifiedsReplyEventArgs>();
    /// <summary>Raised when the data server responds to a <see cref="StartGroupSearch"/> request.</summary>
	private EventObservable<DirGroupsReplyEventArgs> OnDirGroups = new EventObservable<DirGroupsReplyEventArgs>();
    /// <summary>Raised when the data server responds to a <see cref="StartPeopleSearch"/> request.</summary>
	private EventObservable<DirPeopleReplyEventArgs> OnDirPeople = new EventObservable<DirPeopleReplyEventArgs>();
    /// <summary>Raised when the data server responds to a <see cref="StartLandSearch"/> request.</summary>
	private EventObservable<DirLandReplyEventArgs> OnDirLand = new EventObservable<DirLandReplyEventArgs>();
	
	public void registerOnEventInfoReply(EventObserver<EventInfoReplyEventArgs> o)
	{
		OnEventInfo.addObserver(o);
	}

	public void unregisterOnEventInfoReply(EventObserver<EventInfoReplyEventArgs> o)
	{
		OnEventInfo.deleteObserver(o);
	}

	public void registerOnDirEventsReply(EventObserver<DirEventsReplyEventArgs> o)
	{
		OnDirEvents.addObserver(o);
	}

	public void unregisterOnDirEventsReply(EventObserver<DirEventsReplyEventArgs> o)
	{
		OnDirEvents.deleteObserver(o);
	}
	
	public void registerOnPlacesReply(EventObserver<PlacesReplyEventArgs> o)
	{
		OnPlaces.addObserver(o);
	}

	public void unregisterOnPlacesReply(EventObserver<PlacesReplyEventArgs> o)
	{
		OnPlaces.deleteObserver(o);
	}
	
	public void registerOnDirPlacesReply(EventObserver<DirPlacesReplyEventArgs> o)
	{
		OnDirPlaces.addObserver(o);
	}

	public void unregisterOnDirPlacesReply(EventObserver<DirPlacesReplyEventArgs> o)
	{
		OnDirPlaces.deleteObserver(o);
	}
	
	public void registerOnDirClassifiedsReply(EventObserver<DirClassifiedsReplyEventArgs> o)
	{
		OnDirClassifieds.addObserver(o);
	}

	public void unregisterOnDirClassifiedsReply(EventObserver<DirClassifiedsReplyEventArgs> o)
	{
		OnDirClassifieds.deleteObserver(o);
	}
	
	public void registerOnDirGroupsReply(EventObserver<DirGroupsReplyEventArgs> o)
	{
		OnDirGroups.addObserver(o);
	}

	public void unregisterOnDirGroupsReply(EventObserver<DirGroupsReplyEventArgs> o)
	{
		OnDirGroups.deleteObserver(o);
	}
	
	public void registerOnDirPeopleReply(EventObserver<DirPeopleReplyEventArgs> o)
	{
		OnDirPeople.addObserver(o);
	}

	public void unregisterOnDirPeopleReply(EventObserver<DirPeopleReplyEventArgs> o)
	{
		OnDirPeople.deleteObserver(o);
	}
	
	public void registerOnDirLandReply(EventObserver<DirLandReplyEventArgs> o)
	{
		OnDirLand.addObserver(o);
	}

	public void unregisterOnDirLandReply(EventObserver<DirLandReplyEventArgs> o)
	{
		OnDirLand.deleteObserver(o);
	}
	
	//End Region
		    

	//region Private Members
	private GridClient Client;
	//endregion

	//region Constructors
	/// <summary>
	/// Constructs a new instance of the DirectoryManager class
	/// </summary>
	/// <param name="client">An instance of GridClient</param>
	public DirectoryManager(GridClient client)
	{
		Client = client;

		//        Client.network.RegisterCallback(PacketType.DirClassifiedReply, DirClassifiedReplyHandler);        
		Client.network.RegisterCallback(PacketType.DirClassifiedReply,
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{ DirClassifiedReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}}
				);
		//        // Deprecated, replies come in over capabilities
		//        Client.network.RegisterCallback(PacketType.DirLandReply, DirLandReplyHandler);
		Client.network.RegisterCallback(PacketType.DirLandReply, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{DirLandReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}}
				);

		//        Client.network.RegisterEventCallback("DirLandReply", DirLandReplyEventHandler);
		Client.network.RegisterEventCallback("DirLandReply",  new EventObserver<CapsEventObservableArg>()
				{ 
			@Override
			public void handleEvent(Observable o,
					CapsEventObservableArg arg) {
				try{DirLandReplyEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});

		//        Client.network.RegisterCallback(PacketType.DirPeopleReply, DirPeopleReplyHandler);
		Client.network.RegisterCallback(PacketType.DirPeopleReply,
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{DirPeopleReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});

		//        Client.network.RegisterCallback(PacketType.DirGroupsReply, DirGroupsReplyHandler);
		Client.network.RegisterCallback(PacketType.DirGroupsReply, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{DirGroupsReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});

		//        // Deprecated as of viewer 1.2.3
		//		        Client.network.RegisterCallback(PacketType.PlacesReply, PlacesReplyHandler);
		Client.network.RegisterCallback(PacketType.PlacesReply,
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{PlacesReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
			}});

		//        Client.network.RegisterEventCallback("PlacesReply", PlacesReplyEventHandler);
		        Client.network.RegisterEventCallback("PlacesReply", 
		        		new EventObserver<CapsEventObservableArg>()
						{ 
					@Override
					public void handleEvent(Observable o,
							CapsEventObservableArg arg) {
						try{PlacesReplyEventHandler(arg.getCapsKey(), arg.getMessage(), arg.getSimulator());}
						catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

					}});
		
		
		//        Client.network.RegisterCallback(PacketType.DirEventsReply, EventsReplyHandler);
		Client.network.RegisterCallback(PacketType.DirEventsReply, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{EventsReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});

		//        Client.network.RegisterCallback(PacketType.EventInfoReply, EventInfoReplyHandler);
		Client.network.RegisterCallback(PacketType.EventInfoReply, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{EventInfoReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});

		//        Client.network.RegisterCallback(PacketType.DirPlacesReply, DirPlacesReplyHandler);
		Client.network.RegisterCallback(PacketType.DirPlacesReply, 
				new EventObserver<PacketReceivedEventArgs>()
				{ 
			@Override
			public void handleEvent(Observable o,
					PacketReceivedEventArgs arg) {
				try{DirPlacesReplyHandler(o, arg);}
				catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}

			}});

	}

	//endregion

	//region Public Methods
	/**
	 * @deprecated
	 * Obsoleted due to new Adult search option
	 */
	@Deprecated		
	public UUID StartClassifiedSearch(String searchText, ClassifiedCategories category, boolean mature)
	{
		return UUID.Zero;
	}

	/// <summary>
	/// Query the data server for a list of classified ads containing the specified string.
	/// Defaults to searching for classified placed in any category, and includes PG, Adult and Mature 
	/// results.
	/// 
	/// Responses are sent 16 per response packet, there is no way to know how many results a query reply will contain however assuming
	/// the reply packets arrived ordered, a response with less than 16 entries would indicate all results have been received
	/// 
	/// The <see cref="OnClassifiedReply"/> event is raised when a response is received from the simulator
	/// </summary>
	/// <param name="searchText">A string containing a list of keywords to search for</param>
	/// <returns>A UUID to correlate the results when the <see cref="OnClassifiedReply"/> event is raised</returns>
	public UUID StartClassifiedSearch(String searchText)
	{
		return StartClassifiedSearch(searchText, ClassifiedCategories.Any, ClassifiedQueryFlags.All);
	}

	/// <summary>
	/// Query the data server for a list of classified ads which contain specified keywords (Overload)
	/// 
	/// The <see cref="OnClassifiedReply"/> event is raised when a response is received from the simulator
	/// </summary>
	/// <param name="searchText">A string containing a list of keywords to search for</param>
	/// <param name="category">The category to search</param>
	/// <param name="queryFlags">A set of flags which can be ORed to modify query options 
	/// such as classified maturity rating.</param>
	/// <returns>A UUID to correlate the results when the <see cref="OnClassifiedReply"/> event is raised</returns>
	/// <example>
	/// Search classified ads containing the key words "foo" and "bar" in the "Any" category that are either PG or Mature
	/// <code>
	/// UUID searchID = StartClassifiedSearch("foo bar", ClassifiedCategories.Any, ClassifiedQueryFlags.PG | ClassifiedQueryFlags.Mature);
	/// </code>
	/// </example>
	/// <remarks>        
	/// Responses are sent 16 at a time, there is no way to know how many results a query reply will contain however assuming
	/// the reply packets arrived ordered, a response with less than 16 entries would indicate all results have been received
	/// </remarks>
	public UUID StartClassifiedSearch(String searchText, ClassifiedCategories category, ClassifiedQueryFlags queryFlags)
	{
		DirClassifiedQueryPacket query = new DirClassifiedQueryPacket();
		UUID queryID = UUID.Random();

		query.AgentData.AgentID = Client.self.getAgentID();
		query.AgentData.SessionID = Client.self.getSessionID();

		query.QueryData.Category = (long)category.getIndex();
		query.QueryData.QueryFlags = (long)queryFlags.getIndex();
		query.QueryData.QueryID = queryID;
		query.QueryData.QueryText = Utils.stringToBytesWithTrailingNullByte(searchText);

		Client.network.SendPacket(query);

		return queryID;
	}

	/// <summary>
	/// Starts search for places (Overloaded)
	/// 
	/// The <see cref="OnDirPlacesReply"/> event is raised when a response is received from the simulator
	/// </summary>
	/// <param name="searchText">Search text</param>
	/// <param name="queryStart">Each request is limited to 100 places
	/// being returned. To get the first 100 result entries of a request use 0,
	/// from 100-199 use 1, 200-299 use 2, etc.</param>        
	/// <returns>A UUID to correlate the results when the <see cref="OnDirPlacesReply"/> event is raised</returns>
	public UUID StartDirPlacesSearch(String searchText, int queryStart)
	{
		return StartDirPlacesSearch(searchText, DirFindFlags.get(DirFindFlags.DwellSort.getIndex() 
				| DirFindFlags.IncludePG.getIndex() | DirFindFlags.IncludeMature.getIndex()
				| DirFindFlags.IncludeAdult.getIndex()), ParcelCategory.Any, queryStart);
	}

	/// <summary>
	/// Queries the dataserver for parcels of land which are flagged to be shown in search
	/// 
	/// The <see cref="OnDirPlacesReply"/> event is raised when a response is received from the simulator
	/// </summary>
	/// <param name="searchText">A string containing a list of keywords to search for separated by a space character</param>
	/// <param name="queryFlags">A set of flags which can be ORed to modify query options 
	/// such as classified maturity rating.</param>
	/// <param name="category">The category to search</param>        
	/// <param name="queryStart">Each request is limited to 100 places
	/// being returned. To get the first 100 result entries of a request use 0,
	/// from 100-199 use 1, 200-299 use 2, etc.</param>        
	/// <returns>A UUID to correlate the results when the <see cref="OnDirPlacesReply"/> event is raised</returns>
	/// <example>
	/// Search places containing the key words "foo" and "bar" in the "Any" category that are either PG or Adult
	/// <code>
	/// UUID searchID = StartDirPlacesSearch("foo bar", DirFindFlags.DwellSort | DirFindFlags.IncludePG | DirFindFlags.IncludeAdult, ParcelCategory.Any, 0);
	/// </code>
	/// </example>
	/// <remarks>        
	/// Additional information on the results can be obtained by using the ParcelManager.InfoRequest method
	/// </remarks>
	public UUID StartDirPlacesSearch(String searchText, EnumSet<DirFindFlags> queryFlags, ParcelCategory category, int queryStart)
	{
		DirPlacesQueryPacket query = new DirPlacesQueryPacket();

		UUID queryID = UUID.Random();

		query.AgentData.AgentID = Client.self.getAgentID();
		query.AgentData.SessionID = Client.self.getSessionID();

		query.QueryData.Category = (byte)category.getIndex();
		query.QueryData.QueryFlags = (long)DirFindFlags.getIndex(queryFlags);

		query.QueryData.QueryID = queryID;
		query.QueryData.QueryText = Utils.stringToBytesWithTrailingNullByte(searchText);
		query.QueryData.QueryStart = queryStart;
		query.QueryData.SimName = Utils.stringToBytesWithTrailingNullByte("");

		Client.network.SendPacket(query);

		return queryID;

	}

	/// <summary>
	/// Starts a search for land sales using the directory
	/// 
	/// The <see cref="OnDirLandReply"/> event is raised when a response is received from the simulator
	/// </summary>
	/// <param name="typeFlags">What type of land to search for. Auction, 
	/// estate, mainland, "first land", etc</param>        
	/// <remarks>The OnDirLandReply event handler must be registered before
	/// calling this function. There is no way to determine how many 
	/// results will be returned, or how many times the callback will be 
	/// fired other than you won't get more than 100 total parcels from 
	/// each query.</remarks>
	public void StartLandSearch(EnumSet<SearchTypeFlags> typeFlags)
	{
		StartLandSearch(DirFindFlags.get(DirFindFlags.SortAsc.getIndex() | DirFindFlags.PerMeterSort.getIndex()), typeFlags, 0, 0, 0);
	}

	/// <summary>
	/// Starts a search for land sales using the directory
	/// 
	/// The <seealso cref="OnDirLandReply"/> event is raised when a response is received from the simulator
	/// </summary>
	/// <param name="typeFlags">What type of land to search for. Auction, 
	/// estate, mainland, "first land", etc</param>
	/// <param name="priceLimit">Maximum price to search for</param>
	/// <param name="areaLimit">Maximum area to search for</param>
	/// <param name="queryStart">Each request is limited to 100 parcels
	/// being returned. To get the first 100 parcels of a request use 0,
	/// from 100-199 use 1, 200-299 use 2, etc.</param>        
	/// <remarks>The OnDirLandReply event handler must be registered before
	/// calling this function. There is no way to determine how many 
	/// results will be returned, or how many times the callback will be 
	/// fired other than you won't get more than 100 total parcels from 
	/// each query.</remarks>
	public void StartLandSearch(EnumSet<SearchTypeFlags> typeFlags, int priceLimit, int areaLimit, int queryStart)
	{
		StartLandSearch(DirFindFlags.get(DirFindFlags.SortAsc.getIndex() | DirFindFlags.PerMeterSort.getIndex() | DirFindFlags.LimitByPrice.getIndex() |
				DirFindFlags.LimitByArea.getIndex()), typeFlags, priceLimit, areaLimit, queryStart);
	}

	/// <summary>
	/// Send a request to the data server for land sales listings
	/// </summary>
	/// 
	/// <param name="findFlags">Flags sent to specify query options
	/// 
	/// Available flags:
	/// Specify the parcel rating with one or more of the following:
	///     IncludePG IncludeMature IncludeAdult
	/// 
	/// Specify the field to pre sort the results with ONLY ONE of the following:
	///     PerMeterSort NameSort AreaSort PricesSort
	///     
	/// Specify the order the results are returned in, if not specified the results are pre sorted in a Descending Order
	///     SortAsc
	///     
	/// Specify additional filters to limit the results with one or both of the following:
	///     LimitByPrice LimitByArea
	///     
	/// Flags can be combined by separating them with the | (pipe) character
	/// 
	/// Additional details can be found in <see cref="DirFindFlags"/>
	/// </param>
	/// <param name="typeFlags">What type of land to search for. Auction, 
	/// Estate or Mainland</param>
	/// <param name="priceLimit">Maximum price to search for when the 
	/// DirFindFlags.LimitByPrice flag is specified in findFlags</param>
	/// <param name="areaLimit">Maximum area to search for when the
	/// DirFindFlags.LimitByArea flag is specified in findFlags</param>
	/// <param name="queryStart">Each request is limited to 100 parcels
	/// being returned. To get the first 100 parcels of a request use 0,
	/// from 100-199 use 100, 200-299 use 200, etc.</param>
	/// <remarks><para>The <seealso cref="OnDirLandReply"/> event will be raised with the response from the simulator 
	/// 
	/// There is no way to determine how many results will be returned, or how many times the callback will be 
	/// fired other than you won't get more than 100 total parcels from 
	/// each reply.</para>
	/// 
	/// <para>Any land set for sale to either anybody or specific to the connected agent will be included in the
	/// results if the land is included in the query</para></remarks>
	/// <example>
	/// <code>
	/// // request all mainland, any maturity rating that is larger than 512 sq.m
	/// StartLandSearch(DirFindFlags.SortAsc | DirFindFlags.PerMeterSort | DirFindFlags.LimitByArea | DirFindFlags.IncludePG | DirFindFlags.IncludeMature | DirFindFlags.IncludeAdult, SearchTypeFlags.Mainland, 0, 512, 0);
	/// </code></example>
	public void StartLandSearch(EnumSet<DirFindFlags> findFlags, EnumSet<SearchTypeFlags> typeFlags, int priceLimit,
			int areaLimit, int queryStart)
	{
		DirLandQueryPacket query = new DirLandQueryPacket();
		query.AgentData.AgentID = Client.self.getAgentID();
		query.AgentData.SessionID = Client.self.getSessionID();
		query.QueryData.Area = areaLimit;
		query.QueryData.Price = priceLimit;
		query.QueryData.QueryStart = queryStart;
		query.QueryData.SearchType = (int)SearchTypeFlags.getIndex(typeFlags);
		query.QueryData.QueryFlags = (int)DirFindFlags.getIndex(findFlags);
		query.QueryData.QueryID = UUID.Random();

		Client.network.SendPacket(query);            
	}

	/// <summary>
	/// Search for Groups
	/// </summary>
	/// <param name="searchText">The name or portion of the name of the group you wish to search for</param>
	/// <param name="queryStart">Start from the match number</param>
	/// <returns></returns>
	public UUID StartGroupSearch(String searchText, int queryStart)
	{
		return StartGroupSearch(searchText, queryStart, DirFindFlags.get(DirFindFlags.Groups.getIndex() | DirFindFlags.IncludePG.getIndex() 
				| DirFindFlags.IncludeMature.getIndex() | DirFindFlags.IncludeAdult.getIndex()));
	}

	/// <summary>
	/// Search for Groups
	/// </summary>
	/// <param name="searchText">The name or portion of the name of the group you wish to search for</param>
	/// <param name="queryStart">Start from the match number</param>
	/// <param name="flags">Search flags</param>
	/// <returns></returns>
	public UUID StartGroupSearch(String searchText, int queryStart, EnumSet<DirFindFlags> flags)
	{
		DirFindQueryPacket find = new DirFindQueryPacket();
		find.AgentData.AgentID = Client.self.getAgentID();
		find.AgentData.SessionID = Client.self.getSessionID();
		find.QueryData.QueryFlags = DirFindFlags.getIndex(flags);
		find.QueryData.QueryText = Utils.stringToBytesWithTrailingNullByte(searchText);
		find.QueryData.QueryID = UUID.Random();
		find.QueryData.QueryStart = queryStart;

		Client.network.SendPacket(find);

		return find.QueryData.QueryID;
	}

	/// <summary>
	/// Search the People directory for other avatars
	/// </summary>
	/// <param name="searchText">The name or portion of the name of the avatar you wish to search for</param>
	/// <param name="queryStart"></param>
	/// <returns></returns>
	public UUID StartPeopleSearch(String searchText, int queryStart)
	{
		DirFindQueryPacket find = new DirFindQueryPacket();
		find.AgentData.AgentID = Client.self.getAgentID();
		find.AgentData.SessionID = Client.self.getSessionID();
		find.QueryData.QueryFlags = (long)DirFindFlags.People.getIndex();
		find.QueryData.QueryText = Utils.stringToBytesWithTrailingNullByte(searchText);
		find.QueryData.QueryID = UUID.Random();
		find.QueryData.QueryStart = queryStart;

		Client.network.SendPacket(find);

		return find.QueryData.QueryID;
	}

	/// <summary>
	/// Search Places for parcels of land you personally own
	/// </summary>
	public UUID StartPlacesSearch()
	{
		return StartPlacesSearch(DirFindFlags.get(DirFindFlags.AgentOwned.getIndex()), ParcelCategory.Any, "", "",
				UUID.Zero, UUID.Random());
	}

	/// <summary>
	/// Searches Places for land owned by the specified group
	/// </summary>
	/// <param name="groupID">ID of the group you want to recieve land list for (You must be a member of the group)</param>
	/// <returns>Transaction (Query) ID which can be associated with results from your request.</returns>
	public UUID StartPlacesSearch(UUID groupID)
	{
		return StartPlacesSearch(DirFindFlags.get(DirFindFlags.GroupOwned.getIndex()), ParcelCategory.Any, "", "", 
				groupID, UUID.Random());
	}

	/// <summary>
	/// Search the Places directory for parcels that are listed in search and contain the specified keywords
	/// </summary>
	/// <param name="searchText">A string containing the keywords to search for</param>
	/// <returns>Transaction (Query) ID which can be associated with results from your request.</returns>
	public UUID StartPlacesSearch(String searchText)
	{
		return StartPlacesSearch(DirFindFlags.get(DirFindFlags.DwellSort.getIndex() 
				| DirFindFlags.IncludePG.getIndex() 
				| DirFindFlags.IncludeMature.getIndex() 
				| DirFindFlags.IncludeAdult.getIndex()), 
				ParcelCategory.Any, searchText, "", UUID.Zero, UUID.Random());
	}

	/// <summary>
	/// Search Places - All Options
	/// </summary>
	/// <param name="findFlags">One of the Values from the DirFindFlags struct, ie: AgentOwned, GroupOwned, etc.</param>
	/// <param name="searchCategory">One of the values from the SearchCategory Struct, ie: Any, Linden, Newcomer</param>
	/// <param name="searchText">A string containing a list of keywords to search for separated by a space character</param>
	/// <param name="simulatorName">String Simulator Name to search in</param>
	/// <param name="groupID">LLUID of group you want to recieve results for</param>
	/// <param name="transactionID">Transaction (Query) ID which can be associated with results from your request.</param>
	/// <returns>Transaction (Query) ID which can be associated with results from your request.</returns>
	public UUID StartPlacesSearch(EnumSet<DirFindFlags> findFlags, ParcelCategory searchCategory, String searchText, String simulatorName, 
			UUID groupID, UUID transactionID)
	{
		PlacesQueryPacket find = new PlacesQueryPacket();
		find.AgentData.AgentID = Client.self.getAgentID();
		find.AgentData.SessionID = Client.self.getSessionID();
		find.AgentData.QueryID = groupID;

		find.TransactionData.TransactionID = transactionID;

		find.QueryData.QueryText = Utils.stringToBytesWithTrailingNullByte(searchText);
		find.QueryData.QueryFlags = DirFindFlags.getIndex(findFlags);
		find.QueryData.Category = searchCategory.getIndex();
		find.QueryData.SimName = Utils.stringToBytesWithTrailingNullByte(simulatorName);

		Client.network.SendPacket(find);
		return transactionID;
	}

	/// <summary>
	/// Search All Events with specifid searchText in all categories, includes PG, Mature and Adult
	/// </summary>
	/// <param name="searchText">A string containing a list of keywords to search for separated by a space character</param>
	/// <param name="queryStart">Each request is limited to 100 entries
	/// being returned. To get the first group of entries of a request use 0,
	/// from 100-199 use 100, 200-299 use 200, etc.</param>
	/// <returns>UUID of query to correlate results in callback.</returns>
	public UUID StartEventsSearch(String searchText, long queryStart)
	{
		return StartEventsSearch(searchText, DirFindFlags.get(DirFindFlags.DateEvents.getIndex() 
				| DirFindFlags.IncludePG.getIndex() 
				| DirFindFlags.IncludeMature.getIndex() 
				| DirFindFlags.IncludeAdult.getIndex()), 
				"u", queryStart, EventCategories.All);
	}

	/// <summary>
	/// Search Events
	/// </summary>
	/// <param name="searchText">A string containing a list of keywords to search for separated by a space character</param>
	/// <param name="queryFlags">One or more of the following flags: DateEvents, IncludePG, IncludeMature, IncludeAdult
	/// from the <see cref="DirFindFlags"/> Enum
	/// 
	/// Multiple flags can be combined by separating the flags with the | (pipe) character</param>
	/// <param name="eventDay">"u" for in-progress and upcoming events, -or- number of days since/until event is scheduled
	/// For example "0" = Today, "1" = tomorrow, "2" = following day, "-1" = yesterday, etc.</param>
	/// <param name="queryStart">Each request is limited to 100 entries
	/// being returned. To get the first group of entries of a request use 0,
	/// from 100-199 use 100, 200-299 use 200, etc.</param>
	/// <param name="category">EventCategory event is listed under.</param>
	/// <returns>UUID of query to correlate results in callback.</returns>
	public UUID StartEventsSearch(String searchText, EnumSet<DirFindFlags> queryFlags, String eventDay, long queryStart, EventCategories category)
	{
		DirFindQueryPacket find = new DirFindQueryPacket();
		find.AgentData.AgentID = Client.self.getAgentID();
		find.AgentData.SessionID = Client.self.getSessionID();

		UUID queryID = UUID.Random();

		find.QueryData.QueryID = queryID;
		find.QueryData.QueryText = Utils.stringToBytesWithTrailingNullByte(eventDay + "|" + (int)category.getIndex() + "|" + searchText);
		find.QueryData.QueryFlags = (long)DirFindFlags.getIndex(queryFlags);
		find.QueryData.QueryStart = (int)queryStart;

		Client.network.SendPacket(find);
		return queryID;
	}

	/// <summary>Requests Event Details</summary>
	/// <param name="eventID">ID of Event returned from the <see cref="StartEventsSearch"/> method</param>
	public void EventInfoRequest(long eventID)
	{
		EventInfoRequestPacket find = new EventInfoRequestPacket();
		find.AgentData.AgentID = Client.self.getAgentID();
		find.AgentData.SessionID = Client.self.getSessionID();

		find.EventData.EventID = eventID;

		Client.network.SendPacket(find);
	}
	//endregion

	//region Blocking Functions

	//    @Deprecated("Use the async StartPeoplSearch method instead")
	//    public boolean PeopleSearch(DirFindFlags findFlags, string searchText, int queryStart,
	//        int timeoutMS, List<AgentSearchData> results)
	//    {
	//        AutoResetEvent searchEvent = new AutoResetEvent(false);
	//        UUID id = UUID.Zero;
	//        List<AgentSearchData> people = null;
	//
	//        EventHandler<DirPeopleReplyEventArgs> callback =
	//            delegate(Object sender, DirPeopleReplyEventArgs e)
	//            {
	//                if (id == e.QueryID)
	//                {
	//                    people = e.MatchedPeople;
	//                    searchEvent.Set();
	//                }
	//            };
	//
	//        DirPeopleReply += callback;
	//        
	//        id = StartPeopleSearch(searchText, queryStart);
	//        searchEvent.WaitOne(timeoutMS, false);
	//        DirPeopleReply -= callback;
	//
	//        results = people;
	//        return (results != null);
	//    }

	//endregion Blocking Functions

	//region Packet Handlers

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void DirClassifiedReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnDirClassifieds != null)
		{
			DirClassifiedReplyPacket reply = (DirClassifiedReplyPacket)e.getPacket();
			List<Classified> classifieds = new ArrayList<Classified>();

			for(DirClassifiedReplyPacket.QueryRepliesBlock block : reply.QueryReplies)
			{
				Classified classified = new Classified();

				classified.CreationDate = Utils.unixTimeToDate(block.CreationDate);
				classified.ExpirationDate = Utils.unixTimeToDate(block.ExpirationDate);
				classified.Flags = ClassifiedFlags.get(block.ClassifiedFlags);
				classified.ID = block.ClassifiedID;
				classified.Name = Utils.bytesWithTrailingNullByteToString(block.Name);
				classified.Price = block.PriceForListing;

				classifieds.add(classified);
			}

			OnDirClassifieds.raiseEvent(new DirClassifiedsReplyEventArgs(classifieds));                
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void DirLandReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnDirLand != null)
		{
			List<DirectoryParcel> parcelsForSale = new ArrayList<DirectoryParcel>();
			DirLandReplyPacket reply = (DirLandReplyPacket)e.getPacket();

			for (DirLandReplyPacket.QueryRepliesBlock block : reply.QueryReplies)
			{
				DirectoryParcel dirParcel = new DirectoryParcel();

				dirParcel.ActualArea = block.ActualArea;
				dirParcel.ID = block.ParcelID;
				dirParcel.Name = Utils.bytesWithTrailingNullByteToString(block.Name);
				dirParcel.SalePrice = block.SalePrice;
				dirParcel.Auction = block.Auction;
				dirParcel.ForSale = block.ForSale;

				parcelsForSale.add(dirParcel);
			}
			OnDirLand.raiseEvent(new DirLandReplyEventArgs(parcelsForSale));                
		}
	}

	/// <summary>Process an incoming <see cref="DirLandReplyMessage"/> event message</summary>
	/// <param name="capsKey">The Unique Capabilities Key</param>
	/// <param name="message">The <see cref="DirLandReplyMessage"/> event message containing the data</param>
	/// <param name="simulator">The simulator the message originated from</param>
	protected void DirLandReplyEventHandler(String capsKey, IMessage message, Simulator simulator)
	{
		if (OnDirLand != null)
		{
			List<DirectoryParcel> parcelsForSale = new ArrayList<DirectoryParcel>();

			DirLandReplyMessage reply = (DirLandReplyMessage)message;

			for (DirLandReplyMessage.QueryReply block : reply.QueryReplies)
			{
				DirectoryParcel dirParcel = new DirectoryParcel();

				dirParcel.ActualArea = block.ActualArea;
				dirParcel.ID = block.ParcelID;
				dirParcel.Name = block.Name;
				dirParcel.SalePrice = block.SalePrice;
				dirParcel.Auction = block.Auction;
				dirParcel.ForSale = block.ForSale;

				parcelsForSale.add(dirParcel);
			}

			OnDirLand.raiseEvent((new DirLandReplyEventArgs(parcelsForSale)));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void DirPeopleReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnDirPeople != null)
		{
			DirPeopleReplyPacket peopleReply = (DirPeopleReplyPacket)e.getPacket();
			List<AgentSearchData> matches = new ArrayList<AgentSearchData>(peopleReply.QueryReplies.length);
			for (DirPeopleReplyPacket.QueryRepliesBlock reply : peopleReply.QueryReplies)
			{
				AgentSearchData searchData = new AgentSearchData();
				searchData.Online = reply.Online;
				searchData.FirstName = Utils.bytesWithTrailingNullByteToString(reply.FirstName);
				searchData.LastName = Utils.bytesWithTrailingNullByteToString(reply.LastName);
				searchData.AgentID = reply.AgentID;
				matches.add(searchData);
			}

			OnDirPeople.raiseEvent(new DirPeopleReplyEventArgs(peopleReply.QueryData.QueryID, matches));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void DirGroupsReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnDirGroups != null)
		{
			Packet packet = e.getPacket();
			DirGroupsReplyPacket groupsReply = (DirGroupsReplyPacket)packet;
			List<GroupSearchData> matches = new ArrayList<GroupSearchData>(groupsReply.QueryReplies.length);
			for (DirGroupsReplyPacket.QueryRepliesBlock reply : groupsReply.QueryReplies)
			{
				GroupSearchData groupsData = new GroupSearchData();
				groupsData.GroupID = reply.GroupID;
				groupsData.GroupName = Utils.bytesWithTrailingNullByteToString(reply.GroupName);
				groupsData.Members = reply.Members;
				matches.add(groupsData);
			}

			OnDirGroups.raiseEvent(new DirGroupsReplyEventArgs(groupsReply.QueryData.QueryID, matches));
		}
	}

	/// <summary>Process an incoming <see cref="PlacesReplyMessage"/> event message</summary>
	/// <param name="capsKey">The Unique Capabilities Key</param>
	/// <param name="message">The <see cref="PlacesReplyMessage"/> event message containing the data</param>
	/// <param name="simulator">The simulator the message originated from</param>
	protected void PlacesReplyEventHandler(String capsKey, IMessage message, Simulator simulator)
	{
		if (OnPlaces != null)
		{
			PlacesReplyMessage replyMessage = (PlacesReplyMessage)message;
			List<PlacesSearchData> places = new ArrayList<PlacesSearchData>();

			for (int i = 0; i < replyMessage.QueryDataBlocks.length; i++)
			{
				PlacesSearchData place = new PlacesSearchData();
				place.ActualArea = replyMessage.QueryDataBlocks[i].ActualArea;
				place.BillableArea = replyMessage.QueryDataBlocks[i].BillableArea;
				place.Desc = replyMessage.QueryDataBlocks[i].Description;
				place.Dwell = replyMessage.QueryDataBlocks[i].Dwell;
				place.Flags = DirectoryManager.PlacesFlags.get((byte)replyMessage.QueryDataBlocks[i].Flags);
				place.GlobalX = replyMessage.QueryDataBlocks[i].GlobalX;
				place.GlobalY = replyMessage.QueryDataBlocks[i].GlobalY;
				place.GlobalZ = replyMessage.QueryDataBlocks[i].GlobalZ;
				place.Name = replyMessage.QueryDataBlocks[i].Name;
				place.OwnerID = replyMessage.QueryDataBlocks[i].OwnerID;
				place.Price = replyMessage.QueryDataBlocks[i].Price;
				place.SimName = replyMessage.QueryDataBlocks[i].SimName;
				place.SnapshotID = replyMessage.QueryDataBlocks[i].SnapShotID;
				place.SKU = replyMessage.QueryDataBlocks[i].ProductSku;
				places.add(place);
			}

			OnPlaces.raiseEvent(new PlacesReplyEventArgs(replyMessage.QueryID, places));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void PlacesReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnPlaces != null)
		{
			Packet packet = e.getPacket();
			PlacesReplyPacket placesReply = (PlacesReplyPacket)packet;
			List<PlacesSearchData> places = new ArrayList<PlacesSearchData>();

			for (PlacesReplyPacket.QueryDataBlock block : placesReply.QueryData)
			{
				PlacesSearchData place = new PlacesSearchData();
				place.OwnerID = block.OwnerID;
				place.Name = Utils.bytesWithTrailingNullByteToString(block.Name);
				place.Desc = Utils.bytesWithTrailingNullByteToString(block.Desc);
				place.ActualArea = block.ActualArea;
				place.BillableArea = block.BillableArea;
				place.Flags = PlacesFlags.get(block.Flags);
				place.GlobalX = block.GlobalX;
				place.GlobalY = block.GlobalY;
				place.GlobalZ = block.GlobalZ;
				place.SimName = Utils.bytesWithTrailingNullByteToString(block.SimName);
				place.SnapshotID = block.SnapshotID;
				place.Dwell = block.Dwell;
				place.Price = block.Price;

				places.add(place);
			}

			OnPlaces.raiseEvent(new PlacesReplyEventArgs(placesReply.TransactionData.TransactionID, places));                
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void EventsReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnDirEvents != null)
		{
			Packet packet = e.getPacket();
			DirEventsReplyPacket eventsReply = (DirEventsReplyPacket)packet;
			List<EventsSearchData> matches = new ArrayList<EventsSearchData>(eventsReply.QueryReplies.length);

			for (DirEventsReplyPacket.QueryRepliesBlock reply : eventsReply.QueryReplies)
			{
				EventsSearchData eventsData = new EventsSearchData();
				eventsData.Owner = reply.OwnerID;
				eventsData.Name = Utils.bytesWithTrailingNullByteToString(reply.Name);
				eventsData.ID = reply.EventID;
				eventsData.Date = Utils.bytesWithTrailingNullByteToString(reply.Date);
				eventsData.Time = reply.UnixTime;
				eventsData.Flags = EventFlags.get((int)reply.EventFlags);
				matches.add(eventsData);
			}

			OnDirEvents.raiseEvent(new DirEventsReplyEventArgs(eventsReply.QueryData.QueryID, matches));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void EventInfoReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnEventInfo != null)
		{
			Packet packet = e.getPacket();
			EventInfoReplyPacket eventReply = (EventInfoReplyPacket)packet;
			EventInfo evinfo = new EventInfo();
			evinfo.ID = eventReply.EventData.EventID;
			evinfo.Name = Utils.bytesWithTrailingNullByteToString(eventReply.EventData.Name);
			evinfo.Desc = Utils.bytesWithTrailingNullByteToString(eventReply.EventData.Desc);
			evinfo.Amount = eventReply.EventData.Amount;
			evinfo.Category = EventCategories.get((int)Utils.bytesToUInt(eventReply.EventData.Category));
			evinfo.Cover = eventReply.EventData.Cover;
			evinfo.Creator = new UUID(Utils.bytesWithTrailingNullByteToString(eventReply.EventData.Creator));
			evinfo.Date = Utils.bytesWithTrailingNullByteToString(eventReply.EventData.Date);
			evinfo.DateUTC = eventReply.EventData.DateUTC;
			evinfo.Duration = eventReply.EventData.Duration;
			evinfo.Flags = EventFlags.get((int)eventReply.EventData.EventFlags);
			evinfo.SimName = Utils.bytesWithTrailingNullByteToString(eventReply.EventData.SimName);
			evinfo.GlobalPos = eventReply.EventData.GlobalPos;

			OnEventInfo.raiseEvent(new EventInfoReplyEventArgs(evinfo));
		}
	}

	/// <summary>Process an incoming packet and raise the appropriate events</summary>
	/// <param name="sender">The sender</param>
	/// <param name="e">The EventArgs object containing the packet data</param>
	protected void DirPlacesReplyHandler(Object sender, PacketReceivedEventArgs e) throws UnsupportedEncodingException
	{
		if (OnDirPlaces != null)
		{
			Packet packet = e.getPacket();
			DirPlacesReplyPacket reply = (DirPlacesReplyPacket)packet;
			List<DirectoryParcel> result = new ArrayList<DirectoryParcel>();

			for (int i = 0; i < reply.QueryReplies.length; i++)
			{
				DirectoryParcel p = new DirectoryParcel();

				p.ID = reply.QueryReplies[i].ParcelID;
				p.Name = Utils.bytesWithTrailingNullByteToString(reply.QueryReplies[i].Name);
				p.Dwell = reply.QueryReplies[i].Dwell;
				p.Auction = reply.QueryReplies[i].Auction;
				p.ForSale = reply.QueryReplies[i].ForSale;

				result.add(p);
			}

			OnDirPlaces.raiseEvent(new DirPlacesReplyEventArgs(reply.QueryData[0].QueryID, result));                
		}
	}

	//endregion Packet Handlers
}
