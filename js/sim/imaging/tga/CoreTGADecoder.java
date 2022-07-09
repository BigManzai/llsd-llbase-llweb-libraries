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
package com.ngt.jopenmetaverse.shared.sim.imaging.tga;

import java.io.BufferedInputStream;
import java.io.DataInput;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;

import com.google.common.io.LittleEndianDataInputStream;
import com.ngt.jopenmetaverse.shared.sim.imaging.tga.CoreTGADecoder;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class CoreTGADecoder {

	public static class ImageSize
	{
		public int width;
		public int height;
		public ImageSize(int width, int height) {
			super();
			this.width = width;
			this.height = height;
		}
	}


	public static class TgaColorMap
	{
		//ushort
		public int FirstEntryIndex;
		//ushort
		public int Length;
		//unsigned byte
		public short EntrySize;

		public void Read(DataInput br) throws IOException
		{
			FirstEntryIndex = br.readUnsignedShort();
			Length = br.readUnsignedShort();
			EntrySize = (short)br.readUnsignedByte();
		}
	}

	public static class TgaImageSpec
	{
		//ushort
		public int XOrigin;
		public int YOrigin;
		public int Width;
		public int Height;
		//unsigned byte
		public short PixelDepth;
		public short Descriptor;

		public void Read(DataInput br) throws IOException
		{
			XOrigin = br.readUnsignedShort();
			YOrigin = br.readUnsignedShort();
			Width = br.readUnsignedShort();
			Height = br.readUnsignedShort();
			PixelDepth = (short)br.readUnsignedByte();
			Descriptor = (short)br.readUnsignedByte();
		}

		public short getAlphaBits()
		{
			return (short)(Descriptor & 0xF);
		}

		public void setAlphaBits(short value)
		{
			Descriptor = (short)((Descriptor & ~0xF) | (value & 0xF));
		}


		public boolean getBottomUp()
		{
			return (Descriptor & 0x20) == 0x20;
		}
		public void setBottomUp(boolean value)
		{
			Descriptor = (byte)((Descriptor & ~0x20) | (value ? 0x20 : 0));
		}

	}

	public static class TgaHeader
	{
		//unsigned byte
		public short IdLength;
		public short ColorMapType;
		public short ImageType;

		public TgaColorMap ColorMap;
		public TgaImageSpec ImageSpec;

		public void Read(DataInput br) throws IOException
		{
			this.IdLength = (short)br.readUnsignedByte();
			this.ColorMapType = (short)br.readUnsignedByte();
			this.ImageType = (short)br.readUnsignedByte();
			this.ColorMap = new TgaColorMap();
			this.ImageSpec = new TgaImageSpec();
			this.ColorMap.Read(br);
			this.ImageSpec.Read(br);
		}

		public boolean getRleEncoded()
		{
			return ImageType >= 9;
		}
	}

	public static class TgaCD
	{
		//uint
		public long RMask, GMask, BMask, AMask;
		public short RShift, GShift, BShift, AShift;
		//uint
		public long FinalOr;
		public boolean NeedNoConvert = false;
	}

	public static class DecodedImage
	{
		TgaHeader header;
		byte[] pixels;
		
		public DecodedImage(TgaHeader header, byte[] pixels) {
			super();
			this.header = header;
			this.pixels = pixels;
		}
		public DecodedImage() {
		}
		public TgaHeader getHeader() {
			return header;
		}
		public void setHeader(TgaHeader header) {
			this.header = header;
		}
		public byte[] getPixels() {
			return pixels;
		}
		public void setPixels(byte[] pixels) {
			this.pixels = pixels;
		}
	}
	
	public static DecodedImage decodeImage(InputStream source) throws Exception
	{
		DecodedImage decodedImage = new DecodedImage();
		DataInput br = null;
		try
		{
			// open a stream to the file
			BufferedInputStream bis = new BufferedInputStream(source, 8192);
			br = new LittleEndianDataInputStream(bis);
//			br.setByteOrder(ByteOrder.LITTLE_ENDIAN);

			TgaHeader header = new TgaHeader();
			header.Read(br);

			System.out.println(String.format("Width %d height %d pixeldepth %d", header.ImageSpec.Width, header.ImageSpec.Height, header.ImageSpec.PixelDepth));

			if (header.ImageSpec.PixelDepth != 8 &&
					header.ImageSpec.PixelDepth != 16 &&
					header.ImageSpec.PixelDepth != 24 &&
					header.ImageSpec.PixelDepth != 32)
				throw new Exception("Not a supported tga file.");

			if (header.ImageSpec.getAlphaBits() > 8)
				throw new Exception("Not a supported tga file.");

			if (header.ImageSpec.Width > 4096 ||
					header.ImageSpec.Height > 4096)
				throw new Exception("Image too large.");

			byte[] decoded = new byte[header.ImageSpec.Width*header.ImageSpec.Height*4];

			switch (header.ImageSpec.PixelDepth)
			{
			case 8:
				decodeStandard8(decoded, header.ImageSpec.Width,
						header.ImageSpec.Height, header, br, 4);
				break;
			case 16:
				if (header.ImageSpec.getAlphaBits() > 0)
					decodeSpecial16(decoded, header.ImageSpec.Width,
							header.ImageSpec.Height, header, br, 4);
				else
					decodeStandard16(decoded, header.ImageSpec.Width,
							header.ImageSpec.Height, header, br, 4);
				break;
			case 24:
				if (header.ImageSpec.getAlphaBits() > 0)
					decodeSpecial24(decoded, header.ImageSpec.Width,
							header.ImageSpec.Height, header, br, 4);
				else
					decodeStandard24(decoded, header.ImageSpec.Width,
							header.ImageSpec.Height, header, br, 4);
				break;
			case 32:
				decodeStandard32(decoded, header.ImageSpec.Width,
						header.ImageSpec.Height, header, br, 4);
				break;
			default:
				decoded = null;
				return null;
			}      
			
			decodedImage.setHeader(header);
			decodedImage.setPixels(decoded);
		}
		finally
		{
//			br.close();
		}
		
		return decodedImage;
	}


	static void decodeStandard8(
			byte[] b, int width, int height,
			TgaHeader hdr,
			DataInput br, int stride) throws IOException
			{
		TgaCD cd = new TgaCD();
		cd.RMask = 0x000000ff;
		cd.GMask = 0x000000ff;
		cd.BMask = 0x000000ff;
		cd.AMask = 0x000000ff;
		cd.RShift = 0;
		cd.GShift = 0;
		cd.BShift = 0;
		cd.AShift = 0;
		cd.FinalOr = 0x00000000;
		if (hdr.getRleEncoded())
			decodeRle(b, width, height, 1, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
		else
			decodePlain(b,width, height,  1, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
			}

	static void decodeSpecial16(
			byte[] b, int width, int height, TgaHeader hdr, DataInput br ,int stride) throws IOException
			{
		// i must convert the input stream to a sequence of uint values
		// which I then unpack.
		TgaCD cd = new TgaCD();
		cd.RMask = 0x00f00000;
		cd.GMask = 0x0000f000;
		cd.BMask = 0x000000f0;
		cd.AMask = 0xf0000000;
		cd.RShift = 12;
		cd.GShift = 8;
		cd.BShift = 4;
		cd.AShift = 16;
		cd.FinalOr = 0;

		if (hdr.getRleEncoded())
			decodeRle(b, width, height, 2, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
		else
			decodePlain(b, width, height, 2, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
			}

	static void decodeStandard16(
			byte[] b, int width, int height,
			TgaHeader hdr,
			DataInput br, int stride) throws IOException
			{
		// i must convert the input stream to a sequence of uint values
		// which I then unpack.
		TgaCD cd = new TgaCD();
		cd.RMask = 0x00f80000;	// from 0xF800
		cd.GMask = 0x0000fc00;	// from 0x07E0
		cd.BMask = 0x000000f8;  // from 0x001F
		cd.AMask = 0x00000000;
		cd.RShift = 8;
		cd.GShift = 5;
		cd.BShift = 3;
		cd.AShift = 0;
		cd.FinalOr = 0xff000000;

		if (hdr.getRleEncoded())
			decodeRle(b, width, height, 2, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
		else
			decodePlain(b, width, height, 2, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
			}


	static void decodeSpecial24(byte[] b, int width, int height,
			TgaHeader hdr, DataInput br, int stride) throws IOException
			{
		// i must convert the input stream to a sequence of uint values
		// which I then unpack.
		TgaCD cd = new TgaCD();
		cd.RMask = 0x00f80000;
		cd.GMask = 0x0000fc00;
		cd.BMask = 0x000000f8;
		cd.AMask = 0xff000000;
		cd.RShift = 8;
		cd.GShift = 5;
		cd.BShift = 3;
		cd.AShift = 8;
		cd.FinalOr = 0;

		if (hdr.getRleEncoded())
			decodeRle(b, width, height, 3, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
		else
			decodePlain(b, width, height, 3, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
			}

	static void decodeStandard24(byte[] b, int width, int height,
			TgaHeader hdr, DataInput br, int stride) throws IOException
			{
		// i must convert the input stream to a sequence of uint values
		// which I then unpack.
		TgaCD cd = new TgaCD();
		cd.RMask = 0x00ff0000;
		cd.GMask = 0x0000ff00;
		cd.BMask = 0x000000ff;
		cd.AMask = 0x00000000;
		cd.RShift = 0;
		cd.GShift = 0;
		cd.BShift = 0;
		cd.AShift = 0;
		cd.FinalOr = 0xff000000;

		if (hdr.getRleEncoded())
			decodeRle(b, width, height, 3, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
		else
			decodePlain(b, width, height, 3, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
			}

	static void decodeStandard32(byte[] b, int width, int height,
			TgaHeader hdr, DataInput br, int stride) throws IOException
			{
		// i must convert the input stream to a sequence of uint values
		// which I then unpack.
		TgaCD cd = new TgaCD();
		cd.RMask = 0x00ff0000;
		cd.GMask = 0x0000ff00;
		cd.BMask = 0x000000ff;
		cd.AMask = 0xff000000;
		cd.RShift = 0;
		cd.GShift = 0;
		cd.BShift = 0;
		cd.AShift = 0;
		cd.FinalOr = 0x00000000;
		cd.NeedNoConvert = true;

		if (hdr.getRleEncoded())
			decodeRle(b, width, height, 4, new TgaCD[]{cd}, br, stride, hdr.ImageSpec.getBottomUp());
		else
			decodePlain(b, width, height, 4, new TgaCD[]{cd}, br,stride,  hdr.ImageSpec.getBottomUp());
			}


	public static ImageSize GetTGASize(String filename) throws IOException
	{
		DataInput br = new LittleEndianDataInputStream(new FileInputStream(new File(filename)));

		TgaHeader header = new TgaHeader();
		header.Read(br);
//		br.close();
		return new ImageSize(header.ImageSpec.Width, header.ImageSpec.Height);

	}   
	
	static void decodeRle(
			byte[] b, int width, int height,
			int byp, TgaCD[] cd, DataInput br, int stride, boolean bottomUp) throws IOException
			{
		int w = width;
		// make buffer larger, so in case of emergency I can decode 
		// over line ends.
		byte[] linebuffer = new byte[(w + 128) * byp];
		int maxindex = w * byp;
		int index = 0;

		for (int j = 0; j < height; ++j)
		{
			while (index < maxindex)
			{
				short blocktype = (short)br.readUnsignedByte();

				int bytestoread;
				int bytestocopy;

				if (blocktype >= 0x80)
				{
					bytestoread = byp;
					bytestocopy = byp * (blocktype - 0x80);
				}
				else
				{
					bytestoread = byp * (blocktype + 1);
					bytestocopy = 0;
				}

				//if (index + bytestoread > maxindex)
					//	throw new System.Exception ("Corrupt TGA");

				br.readFully(linebuffer, index, bytestoread);
				index += bytestoread;

				for (int i = 0; i != bytestocopy; ++i)
				{
					linebuffer[index + i] = linebuffer[index + i - bytestoread];
				}
				index += bytestocopy;
			}
			if (!bottomUp)
				decodeLine(b, width, height, height - j - 1, byp, linebuffer, stride, cd);
			else
				decodeLine(b, width, height, j, byp, linebuffer, stride, cd);

			if (index > maxindex)
			{
				Utils.arraycopy(linebuffer, maxindex, linebuffer, 0, index - maxindex);
				index -= maxindex;
			}
			else
				index = 0;
		}
			}


	static void decodePlain(
			byte[] b, int width, int height,
			int byp, TgaCD[] cd, DataInput br,int stride, boolean bottomUp) throws IOException
			{
		int w = width;
		byte[] linebuffer = new byte[w * byp];

		for (int j = 0; j < height; ++j)
		{
			br.readFully(linebuffer, 0, w * byp);

			if (!bottomUp)
				decodeLine(b,width, height,  height - j - 1, byp, linebuffer, stride, cd);
			else
				decodeLine(b, width, height, j, byp, linebuffer, stride, cd);
		}
			}
	
	
	static long UnpackColor(
			long sourceColor, TgaCD[] cdarray)
	{
		if (cdarray[0].RMask == 0xFF && cdarray[0].GMask == 0xFF && cdarray[0].BMask == 0xFF)
		{
			// Special case to deal with 8-bit TGA files that we treat as alpha masks
			return sourceColor << 24;
		}
		else
		{
			long rpermute = (sourceColor << cdarray[0].RShift) | (sourceColor >> (32 - cdarray[0].RShift));
			long gpermute = (sourceColor << cdarray[0].GShift) | (sourceColor >> (32 - cdarray[0].GShift));
			long bpermute = (sourceColor << cdarray[0].BShift) | (sourceColor >> (32 - cdarray[0].BShift));
			long apermute = (sourceColor << cdarray[0].AShift) | (sourceColor >> (32 - cdarray[0].AShift));
			long result =
					(rpermute & cdarray[0].RMask) | (gpermute & cdarray[0].GMask)
					| (bpermute & cdarray[0].BMask) | (apermute & cdarray[0].AMask) | cdarray[0].FinalOr;

			return result;
		}
	}

	static void decodeLine(
			byte[] b, int width, int height,
			int line,
			int byp,
			byte[] data,
			int stride,
			TgaCD[] cd)
	{
		if(cd[0].NeedNoConvert)
		{
			int linep2 = line*width*4;

			for(int i =0 ;i < width; ++i)
			{
//				System.out.println(String.format("<X = %d,  Y = %d, %d %s>", i, line, linep2 + (i)*4,
//						Utils.bytesToHexDebugString(Arrays.copyOfRange(data, i*byp, i*byp + byp), "")));
				for(int k = 0; k < 4; k++)
				{
					if(k < byp)
						b[linep2 + (i)*4 + 3 - k] = data[(i)*byp + k];
					else
						b[linep2 + (i)*4 + 3 - k] = (byte)0x00;
					
				}
//				System.out.println(String.format("Stored <X = %d,  Y = %d, %d %s>", i, line, linep2 + (i)*4,
//						Utils.bytesToHexDebugString(Arrays.copyOfRange(b, linep2 + i*4, linep2 + i*4 + 4), "")));
			}
		}
		else
		{
			int linep2 = line*width*4;
			int rdi = 0;
			for (int i = 0; i < width; ++i)
			{
				long x = 0;
				for (int j = 0; j < byp; ++j)
				{
//					System.out.println(String.format("\tbyp %d rdi %d bits %s", byp, rdi, Utils.bytesToHexDebugString(Utils.int64ToBytes(( (((long)data[rdi] & 0x00ff) << (j << 3) ) )), "")));
					x |= ( (((long)(data[rdi] &  0x00ff)) << (j << 3) ) );
					++rdi;
				}
				
//				System.out.println(String.format("Before Unpacking<X = %d,  Y = %d, %d %s>", i, line, linep2 + (i)*4, Utils.bytesToHexDebugString(Utils.int64ToBytes(x), "")));
				
				long color = UnpackColor(x, cd);
				
//				if(color != 0)
//					System.out.println(String.format("After Unpacking<X = %d,  Y = %d, %d %s>", i, line, linep2 + (i)*4, Utils.bytesToHexDebugString(Utils.uintToBytes(color), "")));

				b[linep2 + (i)*4 ] = (byte) (color >> 24);
				b[linep2 + (i)*4 +1 ] = (byte) (color >> 16);
				b[linep2 + (i)*4 + 2] = (byte) (color >> 8);
				b[linep2 + (i)*4 + 3] = (byte)(color & 0xff);   
				
//				if(color != 0)
//				System.out.println(Utils.bytesToHexDebugString(new byte[]{b[linep2 + (i)*4 ], b[linep2 + (i)*4 +1],
//						b[linep2 + (i)*4 +2], b[linep2 + (i)*4 + 3]}, "Decoded ARGB: "));

			}
		}
	}
}