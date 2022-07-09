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

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.protocol.NameValue;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// Represents an avatar (other than your own)
/// </summary>
public class Avatar extends Primitive 
{
	//region Enums

	/// <summary>
	/// Avatar profile flags
	/// </summary>
	//[Flags]
	public enum ProfileFlags 
	{
		//uint
		AllowPublish((long)1),
		MaturePublish((long)2),
		Identified((long)4),
		Transacted((long)8),
		Online((long)16);
		private long index;
		ProfileFlags(long index)
		{
			this.index = index;
		}     

		public long getIndex()
		{
			return index;
		}  

		private static final Map<Long,ProfileFlags> lookup  = new HashMap<Long,ProfileFlags>();

		static {
			for(ProfileFlags s : EnumSet.allOf(ProfileFlags.class))
				lookup.put(s.getIndex(), s);
		}

	      public static EnumSet<ProfileFlags> get(Long index)
          {
                  EnumSet<ProfileFlags> enumsSet = EnumSet.allOf(ProfileFlags.class);
                  for(Entry<Long,ProfileFlags> entry: lookup.entrySet())
                  {
                          if((entry.getKey().longValue() | index) != index)
                          {
                                  enumsSet.remove(entry.getValue());
                          }
                  }
                  return enumsSet;
          }

          public static long getIndex(EnumSet<ProfileFlags> enumSet)
          {
                  long ret = 0;
                  for(ProfileFlags s: enumSet)
                  {
                          ret |= s.getIndex();
                  }
                  return ret;
          }

	}

	//endregion Enums


	//region Subclasses

	/// <summary>
	/// Positive and negative ratings
	/// </summary>
	public static class Statistics
	{
		/// <summary>Positive ratings for Behavior</summary>
		public int BehaviorPositive;
		/// <summary>Negative ratings for Behavior</summary>
		public int BehaviorNegative;
		/// <summary>Positive ratings for Appearance</summary>
		public int AppearancePositive;
		/// <summary>Negative ratings for Appearance</summary>
		public int AppearanceNegative;
		/// <summary>Positive ratings for Building</summary>
		public int BuildingPositive;
		/// <summary>Negative ratings for Building</summary>
		public int BuildingNegative;
		/// <summary>Positive ratings given by this avatar</summary>
		public int GivenPositive;
		/// <summary>Negative ratings given by this avatar</summary>
		public int GivenNegative;

		public OSD GetOSD()
		{
			OSDMap tex = new OSDMap(8);
			tex.put("behavior_positive", OSD.FromInteger(BehaviorPositive));
			tex.put("behavior_negative", OSD.FromInteger(BehaviorNegative));
			tex.put("appearance_positive",  OSD.FromInteger(AppearancePositive));
			tex.put("appearance_negative", OSD.FromInteger(AppearanceNegative));
			tex.put("buildings_positive", OSD.FromInteger(BuildingPositive));
			tex.put("buildings_negative", OSD.FromInteger(BuildingNegative));
			tex.put("given_positive", OSD.FromInteger(GivenPositive));
			tex.put("given_negative", OSD.FromInteger(GivenNegative));
			return tex;
		}

		public static Statistics FromOSD(OSD osd)
		{
			Statistics S = new Statistics();
			OSDMap tex = (OSDMap)osd;

			S.BehaviorPositive = tex.get("behavior_positive").asInteger();
			S.BuildingNegative = tex.get("behavior_negative").asInteger();
			S.AppearancePositive = tex.get("appearance_positive").asInteger();
			S.AppearanceNegative = tex.get("appearance_negative").asInteger();
			S.BuildingPositive = tex.get("buildings_positive").asInteger();
			S.BuildingNegative = tex.get("buildings_negative").asInteger();
			S.GivenPositive = tex.get("given_positive").asInteger();
			S.GivenNegative = tex.get("given_negative").asInteger();


			return S;

		}
	}

	/// <summary>
	/// Avatar properties including about text, profile URL, image IDs and 
	/// publishing settings
	/// </summary>
	public static class AvatarProperties
	{
		/// <summary>First Life about text</summary>
		public String FirstLifeText;
		/// <summary>First Life image ID</summary>
		public UUID FirstLifeImage;
		/// <summary></summary>
		public UUID Partner;
		/// <summary></summary>
		public String AboutText;
		/// <summary></summary>
		public String BornOn;
		/// <summary></summary>
		public String CharterMember;
		/// <summary>Profile image ID</summary>
		public UUID ProfileImage;
		/// <summary>Flags of the profile</summary>
		public EnumSet<ProfileFlags> Flags;
		/// <summary>Web URL for this profile</summary>
		public String ProfileURL;

		//region Properties

		/// <summary>Should this profile be published on the web</summary>
		public boolean isAllowPublish()
		{
			return ((ProfileFlags.getIndex(Flags) & ProfileFlags.AllowPublish.getIndex()) != 0);
		}

		public void setAllowPublish(boolean value)
		{
			if (value == true)
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) | ProfileFlags.AllowPublish.getIndex());
			else
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) & ~ProfileFlags.AllowPublish.getIndex());
		}

		/// <summary>Avatar Online Status</summary>
		public boolean getOnline()
		{
			return ((ProfileFlags.getIndex(Flags) & ProfileFlags.Online.getIndex()) != 0);
		}

		public void setOnline(boolean value)
		{
			if (value == true)
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) | ProfileFlags.Online.getIndex());
			else
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) & ~ProfileFlags.Online.getIndex());
		}

		/// <summary>Is this a mature profile</summary>
		public boolean getMaturePublish()
		{
			return ((ProfileFlags.getIndex(Flags) & ProfileFlags.MaturePublish.getIndex()) != 0); 
		}

		public void setMaturePublish(boolean value)
		{	       
			if (value == true)
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) | ProfileFlags.MaturePublish.getIndex());
			else
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) & ~ProfileFlags.MaturePublish.getIndex());
		}
		/// <summary></summary>
		public boolean getIdentified()
		{
			return ((ProfileFlags.getIndex(Flags) & ProfileFlags.Identified.getIndex()) != 0);
		}

		public void setIdentified(boolean value)
		{
			if (value == true)
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) | ProfileFlags.Identified.getIndex());
			else
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) & ~ProfileFlags.Identified.getIndex());
		}
		/// <summary></summary>
		public boolean getTransacted(boolean value)
		{
			return ((ProfileFlags.getIndex(Flags) & ProfileFlags.Transacted.getIndex()) != 0);
		}

		public void setTransacted(boolean value)
		{
			if (value == true)
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) | ProfileFlags.Transacted.getIndex());
			else
				Flags  = ProfileFlags.get(ProfileFlags.getIndex(Flags) & ~ProfileFlags.Transacted.getIndex());
		}

		public OSD GetOSD()
		{
			OSDMap tex = new OSDMap(9);
			tex.put("first_life_text", OSD.FromString(FirstLifeText));
			tex.put("first_life_image",  OSD.FromUUID(FirstLifeImage));
			tex.put("partner", OSD.FromUUID(Partner));
			tex.put("about_text", OSD.FromString(AboutText));
			tex.put("born_on", OSD.FromString(BornOn));
			tex.put("charter_member", OSD.FromString(CharterMember));
			tex.put("profile_image", OSD.FromUUID(ProfileImage));
			tex.put("flags", OSD.FromInteger((short)ProfileFlags.getIndex(Flags)));
			tex.put("profile_url",  OSD.FromString(ProfileURL));
			return tex;
		}

		public static AvatarProperties FromOSD(OSD O)
		{
			AvatarProperties A = new AvatarProperties();
			OSDMap tex = (OSDMap)O;

			A.FirstLifeText = tex.get("first_life_text").asString();
			A.FirstLifeImage = tex.get("first_life_image").asUUID();
			A.Partner = tex.get("partner").asUUID();
			A.AboutText = tex.get("about_text").asString();
			A.BornOn = tex.get("born_on").asString();
			A.CharterMember = tex.get("chart_member").asString();
			A.ProfileImage = tex.get("profile_image").asUUID();
			A.Flags = ProfileFlags.get((long)tex.get("flags").asInteger());
			A.ProfileURL = tex.get("profile_url").asString();

			return A;

		}

		//endregion Properties
	}

	/// <summary>
	/// Avatar interests including spoken languages, skills, and "want to"
	/// choices
	/// </summary>
	public static  class Interests
	{
		/// <summary>Languages profile field</summary>
		public String LanguagesText;
		/// <summary></summary>
		// FIXME:
		//uint
		public long SkillsMask;
		/// <summary></summary>
		public String SkillsText;
		/// <summary></summary>
		// FIXME:
		//uint
		public long WantToMask;
		/// <summary></summary>
		public String WantToText;

		public OSD GetOSD()
		{
			OSDMap InterestsOSD = new OSDMap(5);
			InterestsOSD.put("languages_text", OSD.FromString(LanguagesText));
			InterestsOSD.put("skills_mask", OSD.FromUInteger(SkillsMask));
			InterestsOSD.put("skills_text", OSD.FromString(SkillsText));
			InterestsOSD.put("want_to_mask", OSD.FromUInteger(WantToMask));
			InterestsOSD.put("want_to_text", OSD.FromString(WantToText));
			return InterestsOSD;
		}

		public static Interests FromOSD(OSD O)
		{
			Interests I = new Interests();
			OSDMap tex = (OSDMap)O;

			I.LanguagesText = tex.get("languages_text").asString();
			I.SkillsMask = tex.get("skills_mask").asUInteger();
			I.SkillsText = tex.get("skills_text").asString();
			I.WantToMask = tex.get("want_to_mask").asUInteger();
			I.WantToText = tex.get("want_to_text").asString();

			return I;

		}
	}

	//endregion Subclasses

	//region Public Members

	/// <summary>Groups that this avatar is a member of</summary>
	public List<UUID> Groups = new ArrayList<UUID>();
	/// <summary>Positive and negative ratings</summary>
	public Statistics ProfileStatistics;
	/// <summary>Avatar properties including about text, profile URL, image IDs and 
	/// publishing settings</summary>
	public AvatarProperties ProfileProperties;
	/// <summary>Avatar interests including spoken languages, skills, and "want to"
	/// choices</summary>
	public Interests ProfileInterests;
	/// <summary>Movement control flags for avatars. Typically not set or used by
	/// clients. To move your avatar, use Client.Self.Movement instead</summary>
	public EnumSet<com.ngt.jopenmetaverse.shared.sim.AgentManager.ControlFlags> ControlFlags;

	/// <summary>
	/// Contains the visual parameters describing the deformation of the avatar
	/// </summary>
	public byte[] VisualParameters = null;

	//endregion Public Members

	protected String name;
	protected String groupName;

	//region Properties

	/// <summary>First name</summary>
	public String getFirstName()
	{
		if (NameValues != null && NameValues.length > 0)
		for (int i = 0; i < NameValues.length; i++)
		{
			if (NameValues[i].Name.equals("FirstName") && NameValues[i].Type == NameValue.ValueType.String)
				return (String)NameValues[i].Value;
		}

		return "";
	}

	/// <summary>Last name</summary>
	public String getLastName()
	{
		if (NameValues != null && NameValues.length > 0)
		for (int i = 0; i < NameValues.length; i++)
		{
			if (NameValues[i].Name.equals("LastName") && NameValues[i].Type == NameValue.ValueType.String)
				return (String)NameValues[i].Value;
		}

		return "";
	}

	/// <summary>Full name</summary>
	public String getName()
	{
		if (!Utils.isNullOrEmpty(name))
		{
			return name;
		}
		else if (NameValues != null && NameValues.length > 0)
		{
			synchronized(NameValues)
			{
				String firstName = "";
				String lastName = "";

				for (int i = 0; i < NameValues.length; i++)
				{
					if (NameValues[i].Name.equals("FirstName") && NameValues[i].Type == NameValue.ValueType.String)
						firstName = (String)NameValues[i].Value;
					else if (NameValues[i].Name.equals("LastName") && NameValues[i].Type == NameValue.ValueType.String)
						lastName = (String)NameValues[i].Value;
				}

				if (!Utils.isNullOrEmpty(firstName) && !Utils.isNullOrEmpty(lastName))
				{
					name = String.format("%s %s", firstName, lastName);
					return name;
				}
				else
				{
					return "";
				}
			}
		}
		else
		{
			return "";
		}
	}

	/// <summary>Active group</summary>
	public String getGroupName()
	{
		if (!Utils.isNullOrEmpty(groupName))
		{
			return groupName;
		}
		else
		{
			if (NameValues == null || NameValues.length == 0)
			{
				return "";
			}
			else
			{
				synchronized (NameValues)
				{
					for (int i = 0; i < NameValues.length; i++)
					{
						if (NameValues[i].Name.equals("Title") && NameValues[i].Type == NameValue.ValueType.String)
						{
							groupName = (String)NameValues[i].Value;
							return groupName;
						}
					}
				}
				return "";
			}
		}
	}

	@Override
	public OSD GetOSD()
	{
		OSDMap Avi = (OSDMap)super.GetOSD();

		OSDArray grp = new OSDArray();
		for(UUID u :Groups)
		{
			grp.add(OSD.FromUUID(u));
		}

		//	            Groups.ForEach(delegate(UUID u) { grp.Add(OSD.FromUUID(u)); });

		OSDArray vp = new OSDArray();

		for (int i = 0; i < VisualParameters.length; i++)
		{
			vp.add(OSD.FromInteger(VisualParameters[i]));
		}

		Avi.put("groups", grp);
		Avi.put("profile_statistics", ProfileStatistics.GetOSD());
		Avi.put("profile_properties", ProfileProperties.GetOSD());
		Avi.put("profile_interest", ProfileInterests.GetOSD());
		Avi.put("control_flags", OSD.FromInteger((byte)Flags));
		Avi.put("visual_parameters", vp);
		Avi.put("first_name", OSD.FromString(getFirstName()));
		Avi.put("last_name", OSD.FromString(getLastName()));
		Avi.put("group_name", OSD.FromString(getGroupName()));

		return Avi;

	}

	public static Avatar FromOSD(OSD O)
	{
		OSDMap tex = (OSDMap)O;
		Avatar A = new Avatar();
		Primitive P = Primitive.FromOSD(O);

		//FIXME need to uncomment following
//		Type Prim = typeof(Primitive);
//
//		FieldInfo[] Fields = Prim.GetFields();
//
//		for (int x = 0; x < Fields.Length; x++)
//		{
//			Logger.Log("Field Matched in FromOSD: "+Fields[x].Name, Helpers.LogLevel.Debug);
//			Fields[x].SetValue(A, Fields[x].GetValue(P));
//		}            

		A.Groups = new ArrayList<UUID>();

		for (OSD U : (OSDArray)tex.get("groups"))
		{
			A.Groups.add(U.asUUID());
		}

		A.ProfileStatistics = Statistics.FromOSD(tex.get("profile_statistics"));
		A.ProfileProperties = AvatarProperties.FromOSD(tex.get("profile_properties"));
		A.ProfileInterests = Interests.FromOSD(tex.get("profile_interest"));
		A.ControlFlags = AgentManager.ControlFlags.get(tex.get("control_flags").asLong());

		OSDArray vp = (OSDArray)tex.get("visual_parameters");
		A.VisualParameters = new byte[vp.count()];

		for (int i = 0; i < vp.count(); i++)
		{
			A.VisualParameters[i] = (byte)vp.get(i).asInteger();
		}

		// *********************From Code Above *******************************
		/*if (NameValues[i].Name == "FirstName" && NameValues[i].Type == NameValue.ValueType.String)
	                              firstName = (string)NameValues[i].Value;
	                          else if (NameValues[i].Name == "LastName" && NameValues[i].Type == NameValue.ValueType.String)
	                              lastName = (string)NameValues[i].Value;*/
		// ********************************************************************

		A.NameValues = new NameValue[3];

		NameValue First = new NameValue();
		First.Name = "FirstName";
		First.Type = NameValue.ValueType.String;
		First.Value = tex.get("first_name").asString();

		NameValue Last = new NameValue();
		Last.Name = "LastName";
		Last.Type = NameValue.ValueType.String;
		Last.Value = tex.get("last_name").asString();

		// ***************From Code Above***************
		// if (NameValues[i].Name == "Title" && NameValues[i].Type == NameValue.ValueType.String)
		// *********************************************

		NameValue Group = new NameValue();
		Group.Name = "Title";
		Group.Type = NameValue.ValueType.String;
		Group.Value = tex.get("group_name").asString();



		A.NameValues[0] = First;
		A.NameValues[1] = Last;
		A.NameValues[2] = Group;

		return A;


	}

	//endregion Properties

	//region Constructors

	/// <summary>
	/// Default constructor
	/// </summary>
	public Avatar()
	{
	}

	//endregion Constructors
}
