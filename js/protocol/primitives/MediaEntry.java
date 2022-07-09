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
package com.ngt.jopenmetaverse.shared.protocol.primitives;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.sim.GroupManager.GroupPowers;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;

/// <summary>
/// Class representing media data for a single face
/// </summary>
public class MediaEntry
{

	/// <summary>
	/// Permissions for control of object media
	/// </summary>
	public enum MediaPermission 
	{
		None ((byte)0),
		Owner ((byte)1),
		Group  ((byte)2),
		Anyone ((byte)4),
		All ((byte)(0 | 1 | 2 | 4));

		private byte index;
		MediaPermission(byte index)
		{
			this.index = index;
		}     

		public byte getIndex()
		{
			return index;
		}
		
		private static final Map<Byte,MediaPermission> lookup  = new HashMap<Byte,MediaPermission>();
		
//		public static MediaPermission create(byte index)
//		{
//			MediaPermission[] values = MediaPermission.values();
//			for(int i = 0; i < values.length; i ++)
//			{
//				if(values[i].getIndex() == index)
//					return values[i]; 
//			}
//			return null;
//		}
		
        public static EnumSet<MediaPermission> get(Byte index)
        {
                EnumSet<MediaPermission> enumsSet = EnumSet.allOf(MediaPermission.class);
                for(Entry<Byte,MediaPermission> entry: lookup.entrySet())
                {
                        if((entry.getKey().byteValue() | index) != index)
                        {
                                enumsSet.remove(entry.getValue());
                        }
                }
                return enumsSet;
        }
        
        public static long getIndex(EnumSet<MediaPermission> enumSet)
        {
                long ret = 0;
                for(MediaPermission s: enumSet)
                {
                        ret |= s.getIndex();
                }
                return ret;
        }

		
	}

	/// <summary>
	/// Style of cotrols that shold be displayed to the user
	/// </summary>
	public enum MediaControls
	{
		Standard (0),
		Mini (1);

		private int index;
		MediaControls(int index)
		{
			this.index = index;
		}     

		public int getIndex()
		{
			return index;
		}

		public static MediaControls create(byte index)
		{
			MediaControls[] values = MediaControls.values();
			for(int i = 0; i < values.length; i ++)
			{
				if(values[i].getIndex() == index)
					return values[i]; 
			}
			return null;
		}
	}

	/// <summary>Is display of the alternative image enabled</summary>
	public boolean EnableAlterntiveImage;

	/// <summary>Should media auto loop</summary>
	public boolean AutoLoop;

	/// <summary>Shoule media be auto played</summary>
	public boolean AutoPlay;

	/// <summary>Auto scale media to prim face</summary>
	public boolean AutoScale;

	/// <summary>Should viewer automatically zoom in on the face when clicked</summary>
	public boolean AutoZoom;

	/// <summary>Should viewer interpret first click as interaction with the media
	/// or when false should the first click be treated as zoom in commadn</summary>
	public boolean InteractOnFirstClick;

	/// <summary>Style of controls viewer should display when
	/// viewer media on this face</summary>
	public MediaControls Controls;

	/// <summary>Starting URL for the media</summary>
	public String HomeURL;

	/// <summary>Currently navigated URL</summary>
	public String CurrentURL;

	/// <summary>Media height in pixes</summary>
	public int Height;

	/// <summary>Media width in pixels</summary>
	public int Width;

	/// <summary>Who can controls the media</summary>
	public EnumSet<MediaPermission> ControlPermissions;

	/// <summary>Who can interact with the media</summary>
	public EnumSet<MediaPermission> InteractPermissions;

	/// <summary>Is URL whitelist enabled</summary>
	public boolean EnableWhiteList;

	/// <summary>Array of URLs that are whitelisted</summary>
	public String[] WhiteList;

	/// <summary>
	/// Serialize to OSD
	/// </summary>
	/// <returns>OSDMap with the serialized data</returns>
	public OSDMap GetOSD()
	{
		OSDMap map = new OSDMap();

		map.put("alt_image_enable", OSD.FromBoolean(EnableAlterntiveImage));
		map.put("auto_loop", OSD.FromBoolean(AutoLoop));
		map.put("auto_play", OSD.FromBoolean(AutoPlay));
		map.put("auto_scale", OSD.FromBoolean(AutoScale));
		map.put("auto_zoom", OSD.FromBoolean(AutoZoom));
		map.put("controls", OSD.FromInteger(Controls.getIndex()));
		map.put("current_url",  OSD.FromString(CurrentURL));
		map.put("first_click_interact", OSD.FromBoolean(InteractOnFirstClick));
		map.put("height_pixels", OSD.FromInteger(Height));
		map.put("home_url", OSD.FromString(HomeURL));
		map.put("perms_control", OSD.FromInteger((int)MediaPermission.getIndex(ControlPermissions)));
		map.put("perms_interact", OSD.FromInteger((int)MediaPermission.getIndex(InteractPermissions)));

		List<OSD> wl = new ArrayList<OSD>();
		if (WhiteList != null && WhiteList.length > 0)
		{
			for (int i = 0; i < WhiteList.length; i++)
				wl.add(OSD.FromString(WhiteList[i]));
		}

		map.put("whitelist", new OSDArray(wl));
		map.put("whitelist_enable", OSD.FromBoolean(EnableWhiteList));
		map.put("width_pixels", OSD.FromInteger(Width));

		return map;
	}

	/// <summary>
	/// Deserialize from OSD data
	/// </summary>
	/// <param name="osd">Serialized OSD data</param>
	/// <returns>Deserialized object</returns>
	public static MediaEntry FromOSD(OSD osd)
	{
		MediaEntry m = new MediaEntry();
		OSDMap map = (OSDMap)osd;

		m.EnableAlterntiveImage = map.get("alt_image_enable").asBoolean();
		m.AutoLoop = map.get("auto_loop").asBoolean();
		m.AutoPlay = map.get("auto_play").asBoolean();
		m.AutoScale = map.get("auto_scale").asBoolean();
		m.AutoZoom = map.get("auto_zoom").asBoolean();
		m.Controls = MediaControls.create((byte)map.get("controls").asInteger());
		m.CurrentURL = map.get("current_url").asString();
		m.InteractOnFirstClick = map.get("first_click_interact").asBoolean();
		m.Height = map.get("height_pixels").asInteger();
		m.HomeURL = map.get("home_url").asString();
		m.ControlPermissions = MediaPermission.get((byte)(map.get("perms_control").asInteger()));
		m.InteractPermissions = MediaPermission.get((byte)map.get("perms_interact").asInteger());

		if (map.get("whitelist").getType() == OSDType.Array)
		{
			OSDArray wl = (OSDArray)map.get("whitelist");
			if (wl.count() > 0)
			{
				m.WhiteList = new String[wl.count()];
				for (int i = 0; i < wl.count(); i++)
				{
					m.WhiteList[i] = wl.get(i).asString();
				}
			}
		}

		m.EnableWhiteList = map.get("whitelist_enable").asBoolean();
		m.Width = map.get("width_pixels").asInteger();

		return m;
	}
}