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
package com.ngt.jopenmetaverse.shared.sim.imaging;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public enum PixelFormat {

	Indexed(0)	,//The pixel data contains color-indexed values, which means the values are an index to colors in the system color table, as opposed to individual color values.
	Gdi(1)	,//The pixel data contains GDI colors.
	Alpha(2) , //	The pixel data contains alpha values that are not premultiplied.
	PAlpha(3),//	The pixel format contains premultiplied alpha values.
	Extended(4),//	Reserved.
	Canonical(5),//	The default pixel format of 32 bits per pixel. The format specifies 24-bit color depth and an 8-bit alpha channel.
	Undefined(6),//	The pixel format is undefined.
	DontCare(7),//	No pixel format is specified.
	Format1bppIndexed(8),//	Specifies that the pixel format is 1 bit per pixel and that it uses indexed color. The color table therefore has two colors in it.
	Format4bppIndexed(9),//	Specifies that the format is 4 bits per pixel, indexed.
	Format8bppIndexed(10),//	Specifies that the format is 8 bits per pixel, indexed. The color table therefore has 256 colors in it.
	Format16bppGrayScale(11),//	The pixel format is 16 bits per pixel. The color information specifies 65536 shades of gray.
	Format16bppRgb555(12),//	Specifies that the format is 16 bits per pixel; 5 bits each are used for the red, green, and blue components. The remaining bit is not used.
	Format16bppRgb565(13),//	Specifies that the format is 16 bits per pixel; 5 bits are used for the red component, 6 bits are used for the green component, and 5 bits are used for the blue component.
	Format16bppArgb1555(14),//	The pixel format is 16 bits per pixel. The color information specifies 32,768 shades of color, of which 5 bits are red, 5 bits are green, 5 bits are blue, and 1 bit is alpha.
	Format24bppRgb(15),//	Specifies that the format is 24 bits per pixel; 8 bits each are used for the red, green, and blue components.
	Format32bppRgb(16),//	Specifies that the format is 32 bits per pixel; 8 bits each are used for the red, green, and blue components. The remaining 8 bits are not used.
	Format32bppArgb(17),//	Specifies that the format is 32 bits per pixel; 8 bits each are used for the alpha, red, green, and blue components.
	Format32bppPArgb(18),//	Specifies that the format is 32 bits per pixel; 8 bits each are used for the alpha, red, green, and blue components. The red, green, and blue components are premultiplied, according to the alpha component.
	Format48bppRgb(19),//	Specifies that the format is 48 bits per pixel; 16 bits each are used for the red, green, and blue components.
	Format64bppArgb(20),//	Specifies that the format is 64 bits per pixel; 16 bits each are used for the alpha, red, green, and blue components.
	Format64bppPArgb(21),//	Specifies that the format is 64 bits per pixel; 16 bits each are used for the alpha, red, green, and blue components. The red, green, and blue components are premultiplied according to the alpha component.
	Format8bppGrayScale(22), //Specified 8 bits of gray 
	Custom(100),
	Max(Integer.MAX_VALUE);//	The maximum value for this enumeration.
	private int index;
	PixelFormat(int index)
	{
		this.index = index;
	}     

	public int getIndex()
	{
		return index;
	}

	private static final Map<Integer,PixelFormat> lookup  = new HashMap<Integer,PixelFormat>();

	static {
		for(PixelFormat s : EnumSet.allOf(PixelFormat.class))
			lookup.put(s.getIndex(), s);
	}

	public static PixelFormat get(Integer index)
	{
		return lookup.get(index);
	}
}
