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
package com.ngt.jopenmetaverse.shared.sim.asset.archiving;

import java.io.IOException;
import java.io.ObjectOutputStream;
import java.io.OutputStream;

import com.ngt.jopenmetaverse.shared.util.Utils;
  /// <summary>
    /// Temporary code to produce a tar archive in tar v7 format
    /// </summary>
    public class TarArchiveWriter
    {
//        protected static ASCIIEncoding m_asciiEncoding = new ASCIIEncoding();

        /// <summary>
        /// Binary writer for the underlying stream
        /// </summary>
        protected ObjectOutputStream m_bw;

        public TarArchiveWriter(OutputStream s) throws IOException
        {
            m_bw = new ObjectOutputStream(s);
        }

        /// <summary>
        /// Write a directory entry to the tar archive.  We can only handle one path level right now!
        /// </summary>
        /// <param name="dirName"></param>
        public void WriteDir(String dirName) throws IOException
        {
            // Directories are signalled by a final /
            if (!dirName.endsWith("/"))
                dirName += "/";

            WriteFile(dirName, new byte[0]);
        }

        /// <summary>
        /// Write a file to the tar archive
        /// </summary>
        /// <param name="filePath"></param>
        /// <param name="data"></param>
        public void WriteFile(String filePath, String data) throws IOException
        {
            WriteFile(filePath, Utils.stringToBytes(data, "US-ASCII"));
        }

        /// <summary>
        /// Write a file to the tar archive
        /// </summary>
        /// <param name="filePath"></param>
        /// <param name="data"></param>
        public void WriteFile(String filePath, byte[] data) throws IOException
        {
            if (filePath.length() > 100)
                WriteEntry("././@LongLink", Utils.stringToBytes(filePath, "US-ASCII"), 'L');

            char fileType;

            if (filePath.endsWith("/"))
            {
                fileType = '5';
            }
            else
            {
                fileType = '0';
            }

            WriteEntry(filePath, data, fileType);
        }

        /// <summary>
        /// Finish writing the raw tar archive data to a stream.  The stream will be closed on completion.
        /// </summary>
        public void Close() throws IOException
        {
            //m_log.Debug("[TAR ARCHIVE WRITER]: Writing final consecutive 0 blocks");

            // Write two consecutive 0 blocks to end the archive
            byte[] finalZeroPadding = new byte[1024];
            m_bw.write(finalZeroPadding);

            m_bw.flush();
            m_bw.close();
        }

        public static byte[] ConvertDecimalToPaddedOctalBytes(int d, int padding)
        {
            String oString = "";

            while (d > 0)
            {
            	//TODO need to verify
                oString = Character.toString(Character.toChars((byte)'0' + d & 7)[0]) + oString;
                d >>= 3;
            }

            while (oString.length() < padding)
            {
                oString = "0" + oString;
            }

            byte[] oBytes = Utils.stringToBytes(oString, "US-ASCII");

            return oBytes;
        }

        /// <summary>
        /// Write a particular entry
        /// </summary>
        /// <param name="filePath"></param>
        /// <param name="data"></param>
        /// <param name="fileType"></param>
        protected void WriteEntry(String filePath, byte[] data, char fileType) throws IOException
        {
            byte[] header = new byte[512];

            // file path field (100)
            byte[] nameBytes = Utils.stringToBytes(filePath, "US-ASCII");
            int nameSize = (nameBytes.length >= 100) ? 100 : nameBytes.length;
            //TODO need to verify
            Utils.arraycopy(nameBytes, 0, header, 0, nameSize);

            // file mode (8)
            byte[] modeBytes = Utils.stringToBytes("0000777", "US-ASCII");
            Utils.arraycopy(modeBytes, 0, header, 100, 7);

            // owner user id (8)
            byte[] ownerIdBytes = Utils.stringToBytes("0000764", "US-ASCII");
            Utils.arraycopy(ownerIdBytes, 0, header, 108, 7);

            // group user id (8)
            byte[] groupIdBytes = Utils.stringToBytes("0000764", "US-ASCII");
            Utils.arraycopy(groupIdBytes, 0, header, 116, 7);

            // file size in bytes (12)
            int fileSize = data.length;
            //m_log.DebugFormat("[TAR ARCHIVE WRITER]: File size of {0} is {1}", filePath, fileSize);

            byte[] fileSizeBytes = ConvertDecimalToPaddedOctalBytes(fileSize, 11);

            Utils.arraycopy(fileSizeBytes, 0, header, 124, 11);

            // last modification time (12)
            byte[] lastModTimeBytes = Utils.stringToBytes("11017037332", "US-ASCII");
            Utils.arraycopy(lastModTimeBytes, 0, header, 136, 11);

            // entry type indicator (1)
            header[156] = Utils.stringToBytes(Character.toString(Character.toChars(fileType)[0]), "US-ASCII")[0];

            Utils.arraycopy(Utils.stringToBytes("0000000", "US-ASCII"), 0, header, 329, 7);
            Utils.arraycopy(Utils.stringToBytes("0000000", "US-ASCII"), 0, header, 337, 7);

            // check sum for header block (8) [calculated last]
            Utils.arraycopy(Utils.stringToBytes("        ", "US-ASCII"), 0, header, 148, 8);

            int checksum = 0;
            for (byte b : header)
            {
                checksum += b;
            }

            //m_log.DebugFormat("[TAR ARCHIVE WRITER]: Decimal header checksum is {0}", checksum);

            byte[] checkSumBytes = ConvertDecimalToPaddedOctalBytes(checksum, 6);

            Utils.arraycopy(checkSumBytes, 0, header, 148, 6);

            header[154] = 0;

            // Write out header
            m_bw.write(header);

            // Write out data
            m_bw.write(data);

            if (data.length % 512 != 0)
            {
                int paddingRequired = 512 - (data.length % 512);

                //m_log.DebugFormat("[TAR ARCHIVE WRITER]: Padding data with {0} bytes", paddingRequired);

                byte[] padding = new byte[paddingRequired];
                m_bw.write(padding);
            }
        }
    }