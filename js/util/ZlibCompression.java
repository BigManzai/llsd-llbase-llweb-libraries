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

package com.ngt.jopenmetaverse.shared.util;
import java.util.zip.*;
import java.io.*;

/**
 *  Helper class to perform zlib compression and decompression
 * Inspired by http://stackoverflow.com/q/6173920/600500.
 */
public class ZlibCompression {

    /**
     * Compresses a file with zlib compression.
     */
    public static void compressFile(InputStream in, OutputStream compressed)
        throws IOException
    {
        OutputStream out =
            new DeflaterOutputStream(compressed);
        shovelInToOut(in, out);
        out.close();
    }

    /**
     * Decompresses a zlib compressed file.
     */
    public static void decompressFile(InputStream compressed, OutputStream out)
        throws IOException
    {
        InputStream in =
            new InflaterInputStream(compressed);
        shovelInToOut(in, out);
        in.close();
    }

    /**
     * Shovels all data from an input stream to an output stream.
     */
    private static void shovelInToOut(InputStream in, OutputStream out)
        throws IOException
    {
    	int max_read = 10000;
        byte[] buffer = new byte[max_read];
        int len;
        while((len = in.read(buffer)) > 0) {
            out.write(buffer, 0, len);
        }
    }
}