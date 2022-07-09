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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// Capability to load TGAs to Bitmap 
/// </summary>
public class LoadTGAClass {
//
//        public static class TGAColorMap
//        {
//        	//ushort
//            public int FirstEntryIndex;
//            //ushort
//            public int Length;
//            public short EntrySize;
//
//            public void Read(ObjectInputStream br)
//            {
//                FirstEntryIndex = Utils.uint16ToInt(br.readShort());
//                Length = Utils.uint16ToInt(br.readShort());
//                EntrySize = Utils.ubyteToShort(br.readByte());
//            }
//        }
//
//        public static class TGAImageSpec
//        {
//        	//ushort
//            public int XOrigin;
//          //ushort
//            public int YOrigin;
//          //ushort
//            public int Width;
//          //ushort
//            public int Height;
//            
//            //ubyte
//            public  short PixelDepth;
//            //ubyte
//            public short Descriptor;
//
//            public void Read(ObjectInputStream br)
//            {
//                XOrigin = Utils.uint16ToInt(br.readShort());
//                YOrigin = Utils.uint16ToInt(br.readShort());
//                Width = Utils.uint16ToInt(br.readShort());
//                Height = Utils.uint16ToInt(br.readShort());
//                PixelDepth = Utils.ubyteToShort(br.readByte());
//                Descriptor = Utils.ubyteToShort(br.readByte());
//            }
//
//            public short getAlphaBits()
//            {
//                    return (short)(Descriptor & 0xF);
//            }
//            
//            public void setAlphaBits(short value)
//            {
//                    Descriptor = (short)((Descriptor & ~0xF) | (value & 0xF));
//            }
//
//            public boolean getBottomUp()
//            {
//                    return (Descriptor & 0x20) == 0x20;
//            }
//            public void setBottomUp(boolean value)
//                {
//                    Descriptor = (short)((Descriptor & ~0x20) | (value ? 0x20 : 0));
//                }
//            }
//
//        public static class TGAHeader
//        {
//        	//ubyte
//            public short IdLength;
//            public short ColorMapType;
//            public short ImageType;
//
//            public TGAColorMap ColorMap;
//            public TGAImageSpec ImageSpec;
//
//            public void Read(ObjectInputStream br)
//            {
//                this.IdLength = (short)br.readUnsignedByte();
//                this.ColorMapType = (short)br.readUnsignedByte();
//                this.ImageType = (short)br.readUnsignedByte();
//                this.ColorMap = new TGAColorMap();
//                this.ImageSpec = new TGAImageSpec();
//                this.ColorMap.Read(br);
//                this.ImageSpec.Read(br);
//            }
//
//            public boolean getRleEncoded()
//            {
//                    return ImageType >= 9;
//            }
//        }
//
//        public class TGACd
//        {
//        	//uint
//            public long RMask, GMask, BMask, AMask;
//            public byte RShift, GShift, BShift, AShift;
//            //uint
//            public long FinalOr;
//            public boolean NeedNoConvert;
//        }
//
//        //return uint
//        static long UnpackColor(
//            long sourceColor, TGACd cd)
//        {
//            if (cd.RMask == 0xFF && cd.GMask == 0xFF && cd.BMask == 0xFF)
//            {
//                // Special case to deal with 8-bit TGA files that we treat as alpha masks
//                return sourceColor << 24;
//            }
//            else
//            {
//                long rpermute = (sourceColor << cd.RShift) | (sourceColor >> (32 - cd.RShift));
//                long gpermute = (sourceColor << cd.GShift) | (sourceColor >> (32 - cd.GShift));
//                long bpermute = (sourceColor << cd.BShift) | (sourceColor >> (32 - cd.BShift));
//                long apermute = (sourceColor << cd.AShift) | (sourceColor >> (32 - cd.AShift));
//                long result =
//                    (rpermute & cd.RMask) | (gpermute & cd.GMask)
//                    | (bpermute & cd.BMask) | (apermute & cd.AMask) | cd.FinalOr;
//
//                return result;
//            }
//        }
//        //unsafe ??
//        static  void decodeLine(
//            System.Drawing.Imaging.BitmapData b,
//            int line,
//            int byp,
//            byte[] data,
//            TGACd cd)
//        {
//            if (cd.NeedNoConvert)
//            {
//                // fast copy
//                uint* linep = (uint*)((byte*)b.Scan0.ToPointer() + line * b.Stride);
//                fixed (byte* ptr = data)
//                {
//                    uint* sptr = (uint*)ptr;
//                    for (int i = 0; i < b.Width; ++i)
//                    {
//                        linep[i] = sptr[i];
//                    }
//                }
//            }
//            else
//            {
//                byte* linep = (byte*)b.Scan0.ToPointer() + line * b.Stride;
//
//                uint* up = (uint*)linep;
//
//                int rdi = 0;
//
//                fixed (byte* ptr = data)
//                {
//                    for (int i = 0; i < b.Width; ++i)
//                    {
//                        uint x = 0;
//                        for (int j = 0; j < byp; ++j)
//                        {
//                            x |= ((uint)ptr[rdi]) << (j << 3);
//                            ++rdi;
//                        }
//                        up[i] = UnpackColor(x, ref cd);
//                    }
//                }
//            }
//        }
//
//        static void decodeRle(
//            System.Drawing.Imaging.BitmapData b,
//            int byp, TGACd cd, ObjectInputStream br, boolean bottomUp)
//        {
//            try
//            {
//                int w = b.Width;
//                // make buffer larger, so in case of emergency I can decode 
//                // over line ends.
//                byte[] linebuffer = new byte[(w + 128) * byp];
//                int maxindex = w * byp;
//                int index = 0;
//
//                for (int j = 0; j < b.Height; ++j)
//                {
//                    while (index < maxindex)
//                    {
//                        byte blocktype = br.ReadByte();
//
//                        int bytestoread;
//                        int bytestocopy;
//
//                        if (blocktype >= 0x80)
//                        {
//                            bytestoread = byp;
//                            bytestocopy = byp * (blocktype - 0x80);
//                        }
//                        else
//                        {
//                            bytestoread = byp * (blocktype + 1);
//                            bytestocopy = 0;
//                        }
//
//                        //if (index + bytestoread > maxindex)
//                        //	throw new System.ArgumentException ("Corrupt TGA");
//
//                        br.Read(linebuffer, index, bytestoread);
//                        index += bytestoread;
//
//                        for (int i = 0; i != bytestocopy; ++i)
//                        {
//                            linebuffer[index + i] = linebuffer[index + i - bytestoread];
//                        }
//                        index += bytestocopy;
//                    }
//                    if (!bottomUp)
//                        decodeLine(b, b.Height - j - 1, byp, linebuffer, ref cd);
//                    else
//                        decodeLine(b, j, byp, linebuffer, ref cd);
//
//                    if (index > maxindex)
//                    {
//                        Array.Copy(linebuffer, maxindex, linebuffer, 0, index - maxindex);
//                        index -= maxindex;
//                    }
//                    else
//                        index = 0;
//
//                }
//            }
//            catch (System.IO.EndOfStreamException)
//            {
//            }
//        }
//
//        static void decodePlain(
//            System.Drawing.Imaging.BitmapData b,
//            int byp, TGACd cd, ObjectInputStream br, boolean bottomUp)
//        {
//            int w = b.Width;
//            byte[] linebuffer = new byte[w * byp];
//
//            for (int j = 0; j < b.Height; ++j)
//            {
//                br.Read(linebuffer, 0, w * byp);
//
//                if (!bottomUp)
//                    decodeLine(b, b.Height - j - 1, byp, linebuffer, ref cd);
//                else
//                    decodeLine(b, j, byp, linebuffer, ref cd);
//            }
//        }
//
//        static void decodeStandard8(
//            System.Drawing.Imaging.BitmapData b,
//            TGAHeader hdr,
//            System.IO.BinaryReader br)
//        {
//            TGACd cd = new TGACd();
//            cd.RMask = 0x000000ff;
//            cd.GMask = 0x000000ff;
//            cd.BMask = 0x000000ff;
//            cd.AMask = 0x000000ff;
//            cd.RShift = 0;
//            cd.GShift = 0;
//            cd.BShift = 0;
//            cd.AShift = 0;
//            cd.FinalOr = 0x00000000;
//            if (hdr.RleEncoded)
//                decodeRle(b, 1, cd, br, hdr.ImageSpec.BottomUp);
//            else
//                decodePlain(b, 1, cd, br, hdr.ImageSpec.BottomUp);
//        }
//
//        static void decodeSpecial16(
//            System.Drawing.Imaging.BitmapData b, TGAHeader hdr, System.IO.BinaryReader br)
//        {
//            // i must convert the input stream to a sequence of uint values
//            // which I then unpack.
//            TGACd cd = new TGACd();
//            cd.RMask = 0x00f00000;
//            cd.GMask = 0x0000f000;
//            cd.BMask = 0x000000f0;
//            cd.AMask = 0xf0000000;
//            cd.RShift = 12;
//            cd.GShift = 8;
//            cd.BShift = 4;
//            cd.AShift = 16;
//            cd.FinalOr = 0;
//
//            if (hdr.RleEncoded)
//                decodeRle(b, 2, cd, br, hdr.ImageSpec.BottomUp);
//            else
//                decodePlain(b, 2, cd, br, hdr.ImageSpec.BottomUp);
//        }
//
//        static void decodeStandard16(
//            System.Drawing.Imaging.BitmapData b,
//            TGAHeader hdr,
//            System.IO.BinaryReader br)
//        {
//            // i must convert the input stream to a sequence of uint values
//            // which I then unpack.
//            TGACd cd = new TGACd();
//            cd.RMask = 0x00f80000;	// from 0xF800
//            cd.GMask = 0x0000fc00;	// from 0x07E0
//            cd.BMask = 0x000000f8;  // from 0x001F
//            cd.AMask = 0x00000000;
//            cd.RShift = 8;
//            cd.GShift = 5;
//            cd.BShift = 3;
//            cd.AShift = 0;
//            cd.FinalOr = 0xff000000;
//
//            if (hdr.RleEncoded)
//                decodeRle(b, 2, cd, br, hdr.ImageSpec.BottomUp);
//            else
//                decodePlain(b, 2, cd, br, hdr.ImageSpec.BottomUp);
//        }
//
//
//        static void decodeSpecial24(System.Drawing.Imaging.BitmapData b,
//            TGAHeader hdr, System.IO.BinaryReader br)
//        {
//            // i must convert the input stream to a sequence of uint values
//            // which I then unpack.
//            TGACd cd = new TGACd();
//            cd.RMask = 0x00f80000;
//            cd.GMask = 0x0000fc00;
//            cd.BMask = 0x000000f8;
//            cd.AMask = 0xff000000;
//            cd.RShift = 8;
//            cd.GShift = 5;
//            cd.BShift = 3;
//            cd.AShift = 8;
//            cd.FinalOr = 0;
//
//            if (hdr.RleEncoded)
//                decodeRle(b, 3, cd, br, hdr.ImageSpec.BottomUp);
//            else
//                decodePlain(b, 3, cd, br, hdr.ImageSpec.BottomUp);
//        }
//
//        static void decodeStandard24(System.Drawing.Imaging.BitmapData b,
//            TGAHeader hdr, System.IO.BinaryReader br)
//        {
//            // i must convert the input stream to a sequence of uint values
//            // which I then unpack.
//            TGACd cd = new TGACd();
//            cd.RMask = 0x00ff0000;
//            cd.GMask = 0x0000ff00;
//            cd.BMask = 0x000000ff;
//            cd.AMask = 0x00000000;
//            cd.RShift = 0;
//            cd.GShift = 0;
//            cd.BShift = 0;
//            cd.AShift = 0;
//            cd.FinalOr = 0xff000000;
//
//            if (hdr.RleEncoded)
//                decodeRle(b, 3, cd, br, hdr.ImageSpec.BottomUp);
//            else
//                decodePlain(b, 3, cd, br, hdr.ImageSpec.BottomUp);
//        }
//
//        static void decodeStandard32(System.Drawing.Imaging.BitmapData b,
//            TGAHeader hdr, System.IO.BinaryReader br)
//        {
//            // i must convert the input stream to a sequence of uint values
//            // which I then unpack.
//            TGACd cd = new TGACd();
//            cd.RMask = 0x00ff0000;
//            cd.GMask = 0x0000ff00;
//            cd.BMask = 0x000000ff;
//            cd.AMask = 0xff000000;
//            cd.RShift = 0;
//            cd.GShift = 0;
//            cd.BShift = 0;
//            cd.AShift = 0;
//            cd.FinalOr = 0x00000000;
//            cd.NeedNoConvert = true;
//
//            if (hdr.RleEncoded)
//                decodeRle(b, 4, cd, br, hdr.ImageSpec.BottomUp);
//            else
//                decodePlain(b, 4, cd, br, hdr.ImageSpec.BottomUp);
//        }


//        public static System.Drawing.Size GetTGASize(string filename)
//        {
//            System.IO.FileStream f = System.IO.File.OpenRead(filename);
//
//            System.IO.BinaryReader br = new System.IO.BinaryReader(f);
//
//            TGAHeader header = new TGAHeader();
//            header.Read(br);
//            br.Close();
//
//            return new System.Drawing.Size(header.ImageSpec.Width, header.ImageSpec.Height);
//
//        }
//
//        public static System.Drawing.Bitmap LoadTGA(System.IO.Stream source)
//        {
//            byte[] buffer = new byte[source.Length];
//            source.Read(buffer, 0, buffer.Length);
//
//            System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer);
//
//            using (System.IO.BinaryReader br = new System.IO.BinaryReader(ms))
//            {
//                TGAHeader header = new TGAHeader();
//                header.Read(br);
//
//                if (header.ImageSpec.PixelDepth != 8 &&
//                    header.ImageSpec.PixelDepth != 16 &&
//                    header.ImageSpec.PixelDepth != 24 &&
//                    header.ImageSpec.PixelDepth != 32)
//                    throw new ArgumentException("Not a supported tga file.");
//
//                if (header.ImageSpec.AlphaBits > 8)
//                    throw new ArgumentException("Not a supported tga file.");
//
//                if (header.ImageSpec.Width > 4096 ||
//                    header.ImageSpec.Height > 4096)
//                    throw new ArgumentException("Image too large.");
//
//				System.Drawing.Bitmap b;
//				System.Drawing.Imaging.BitmapData bd;
//
//				// Create a bitmap for the image.
//				// Only include an alpha layer when the image requires one.
//				if (header.ImageSpec.AlphaBits > 0 ||
//					header.ImageSpec.PixelDepth == 8 ||	// Assume  8 bit images are alpha only
//					header.ImageSpec.PixelDepth == 32)	// Assume 32 bit images are ARGB
//				{	// Image needs an alpha layer
//					b = new System.Drawing.Bitmap(
//						header.ImageSpec.Width,
//						header.ImageSpec.Height,
//						System.Drawing.Imaging.PixelFormat.Format32bppArgb);
//
//					bd = b.LockBits(new System.Drawing.Rectangle(0, 0, b.Width, b.Height),
//						System.Drawing.Imaging.ImageLockMode.WriteOnly,
//						System.Drawing.Imaging.PixelFormat.Format32bppPArgb);
//				}
//				else
//				{	// Image does not need an alpha layer, so do not include one.
//					b = new System.Drawing.Bitmap(
//						header.ImageSpec.Width,
//						header.ImageSpec.Height,
//						System.Drawing.Imaging.PixelFormat.Format32bppRgb);
//
//					bd = b.LockBits(new System.Drawing.Rectangle(0, 0, b.Width, b.Height),
//						System.Drawing.Imaging.ImageLockMode.WriteOnly,
//						System.Drawing.Imaging.PixelFormat.Format32bppRgb);
//				}
//
//                switch (header.ImageSpec.PixelDepth)
//                {
//                    case 8:
//                        decodeStandard8(bd, header, br);
//                        break;
//                    case 16:
//                        if (header.ImageSpec.AlphaBits > 0)
//                            decodeSpecial16(bd, header, br);
//                        else
//                            decodeStandard16(bd, header, br);
//                        break;
//                    case 24:
//                        if (header.ImageSpec.AlphaBits > 0)
//                            decodeSpecial24(bd, header, br);
//                        else
//                            decodeStandard24(bd, header, br);
//                        break;
//                    case 32:
//                        decodeStandard32(bd, header, br);
//                        break;
//                    default:
//                        b.UnlockBits(bd);
//                        b.Dispose();
//                        return null;
//                }
//
//                b.UnlockBits(bd);
//                return b;
//            }
//        }
//
//        //unsafe
//        public static ManagedImage LoadTGAImage(System.IO.Stream source)
//        {
//            return LoadTGAImage(source, false);
//        }
//        
//        public static unsafe ManagedImage LoadTGAImage(System.IO.Stream source, boolean mask)
//        {
//            byte[] buffer = new byte[source.Length];
//            source.Read(buffer, 0, buffer.Length);
//
//            System.IO.MemoryStream ms = new System.IO.MemoryStream(buffer);
//
//            using (System.IO.BinaryReader br = new System.IO.BinaryReader(ms))
//            {
//                TGAHeader header = new TGAHeader();
//                header.Read(br);
//
//                if (header.ImageSpec.PixelDepth != 8 &&
//                    header.ImageSpec.PixelDepth != 16 &&
//                    header.ImageSpec.PixelDepth != 24 &&
//                    header.ImageSpec.PixelDepth != 32)
//                    throw new ArgumentException("Not a supported tga file.");
//
//                if (header.ImageSpec.AlphaBits > 8)
//                    throw new ArgumentException("Not a supported tga file.");
//
//                if (header.ImageSpec.Width > 4096 ||
//                    header.ImageSpec.Height > 4096)
//                    throw new ArgumentException("Image too large.");
//
//                byte[] decoded = new byte[header.ImageSpec.Width * header.ImageSpec.Height * 4];
//                System.Drawing.Imaging.BitmapData bd = new System.Drawing.Imaging.BitmapData();
//
//                fixed (byte* pdecoded = &decoded[0])
//                {
//                    bd.Width = header.ImageSpec.Width;
//                    bd.Height = header.ImageSpec.Height;
//                    bd.PixelFormat = System.Drawing.Imaging.PixelFormat.Format32bppPArgb;
//                    bd.Stride = header.ImageSpec.Width * 4;
//                    bd.Scan0 = (IntPtr)pdecoded;
//
//                    switch (header.ImageSpec.PixelDepth)
//                    {
//                        case 8:
//                            decodeStandard8(bd, header, br);
//                            break;
//                        case 16:
//                            if (header.ImageSpec.AlphaBits > 0)
//                                decodeSpecial16(bd, header, br);
//                            else
//                                decodeStandard16(bd, header, br);
//                            break;
//                        case 24:
//                            if (header.ImageSpec.AlphaBits > 0)
//                                decodeSpecial24(bd, header, br);
//                            else
//                                decodeStandard24(bd, header, br);
//                            break;
//                        case 32:
//                            decodeStandard32(bd, header, br);
//                            break;
//                        default:
//                            return null;
//                    }
//                }
//
//                int n = header.ImageSpec.Width * header.ImageSpec.Height;
//                ManagedImage image;
//
//                if (mask && header.ImageSpec.AlphaBits == 0 && header.ImageSpec.PixelDepth == 8)
//                {
//                    image = new ManagedImage(header.ImageSpec.Width, header.ImageSpec.Height,
//                        ManagedImage.ImageChannels.Alpha);
//                    int p = 3;
//
//                    for (int i = 0; i < n; i++)
//                    {
//                        image.Alpha[i] = decoded[p];
//                        p += 4;
//                    }
//                }
//                else
//                {
//                    image = new ManagedImage(header.ImageSpec.Width, header.ImageSpec.Height,
//                        ManagedImage.ImageChannels.Color | ManagedImage.ImageChannels.Alpha);
//                    int p = 0;
//
//                    for (int i = 0; i < n; i++)
//                    {
//                        image.Blue[i] = decoded[p++];
//                        image.Green[i] = decoded[p++];
//                        image.Red[i] = decoded[p++];
//                        image.Alpha[i] = decoded[p++];
//                    }
//                }
//
//                br.Close();
//                return image;
//            }
//        }
//
        public static IBitmap LoadTGA(String filename) throws IOException
        {
        	FileInputStream inputStream = null;
				try {
					inputStream = new FileInputStream(new File(filename));
                    return LoadTGA(inputStream);                
				} catch (FileNotFoundException e) {
					JLogger.error(Utils.getExceptionStackTraceAsString(e));
				} 
				finally 
				{
					if(inputStream!=null)
					inputStream.close();
				}
				return null;
        }
        
        public static IBitmap LoadTGA(InputStream inputStream) 
        {
                    return TGAImageReaderFactory.getInstance().read(inputStream);                
        }
}
