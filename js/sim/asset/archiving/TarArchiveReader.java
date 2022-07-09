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
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.UnsupportedEncodingException;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import com.ngt.jopenmetaverse.shared.util.Utils;


  /// <summary>
    /// Temporary code to do the bare minimum required to read a tar archive for our purposes
    /// </summary>
    public class TarArchiveReader
    {
        public enum TarEntryType
        {
            TYPE_UNKNOWN(0),
            TYPE_NORMAL_FILE (1),
            TYPE_HARD_LINK (2),
            TYPE_SYMBOLIC_LINK (3),
            TYPE_CHAR_SPECIAL (4),
            TYPE_BLOCK_SPECIAL (5),
            TYPE_DIRECTORY (6),
            TYPE_FIFO (7),
            TYPE_CONTIGUOUS_FILE (8);
    		private int index;
    		TarEntryType(int index)
    		{
    			this.index = index;
    		}     

    		public int getIndex()
    		{
    			return index;
    		}

    		private static final Map<Integer,TarEntryType> lookup  
    		= new HashMap<Integer,TarEntryType>();

    		static {
    			for(TarEntryType s : EnumSet.allOf(TarEntryType.class))
    				lookup.put(s.getIndex(), s);
    		}

    		public static TarEntryType get(Integer index)
    		{
    			return lookup.get(index);
    		}
        }

//        protected static ASCIIEncoding m_asciiEncoding = new ASCIIEncoding();

        /// <summary>
        /// Binary reader for the underlying stream
        /// </summary>
        protected ObjectInputStream m_br;

        /// <summary>
        /// Used to trim off null chars
        /// </summary>
        protected final static  char[] m_nullCharArray = new char[] { '\0' };

        /// <summary>
        /// Used to trim off space chars
        /// </summary>
        protected final static char[] m_spaceCharArray = new char[] { ' ' };

        /// <summary>
        /// Generate a tar reader which reads from the given stream.
        /// </summary>
        /// <param name="s"></param>
        public TarArchiveReader(InputStream s) throws IOException
        {
            m_br = new ObjectInputStream(s);
        }

        /// <summary>
        /// Read the next entry in the tar file.
        /// </summary>
        /// <param name="filePath"></param>
        /// <param name="entryType"></param>
        /// <returns>the data for the entry.  Returns null if there are no more entries</returns>
        public TarEntry ReadEntry() throws IOException
        {
        	TarEntry entry = new TarEntry();
            entry.setFilePath("");
            entry.setEntryType(TarEntryType.TYPE_UNKNOWN);
            TarHeader header = ReadHeader();
            entry.setData(null);
            
            if (null == header)
                return entry;

            entry.setEntryType(header.EntryType);
            entry.setFilePath(header.FilePath);
            entry.setData(ReadData(header.FileSize));
            return entry;
        }

        /// <summary>
        /// Read the next 512 byte chunk of data as a tar header.
        /// </summary>
        /// <returns>A tar header struct.  null if we have reached the end of the archive.</returns>
        protected TarHeader ReadHeader() throws IOException
        {
            byte[] header = new byte[512]; 
            m_br.read(header, 0, header.length);

            // If we've reached the end of the archive we'll be in null block territory, which means
            // the next byte will be 0
            if (header[0] == 0)
                return null;

            TarHeader tarHeader = new TarHeader();

            // If we're looking at a GNU tar long link then extract the long name and pull up the next header
            if (header[156] == (byte)'L')
            {
                int longNameLength = ConvertOctalBytesToDecimal(header, 124, 11);
                tarHeader.FilePath = Utils.bytesToString(ReadData(longNameLength), "US-ASCII");
                //m_log.DebugFormat("[TAR ARCHIVE READER]: Got long file name {0}", tarHeader.FilePath);
                m_br.read(header, 0, 512);
            }
            else
            {
                tarHeader.FilePath = Utils.bytesToString(header, 0, 100, "US-ASCII");
                tarHeader.FilePath = Utils.trim(tarHeader.FilePath, m_nullCharArray);
                //m_log.DebugFormat("[TAR ARCHIVE READER]: Got short file name {0}", tarHeader.FilePath);
            }

            tarHeader.FileSize = ConvertOctalBytesToDecimal(header, 124, 11);

            switch (header[156])
            {
                case 0:
                    tarHeader.EntryType = TarEntryType.TYPE_NORMAL_FILE;
                    break;
                case (byte)'0':
                    tarHeader.EntryType = TarEntryType.TYPE_NORMAL_FILE;
                    break;
                case (byte)'1':
                    tarHeader.EntryType = TarEntryType.TYPE_HARD_LINK;
                    break;
                case (byte)'2':
                    tarHeader.EntryType = TarEntryType.TYPE_SYMBOLIC_LINK;
                    break;
                case (byte)'3':
                    tarHeader.EntryType = TarEntryType.TYPE_CHAR_SPECIAL;
                    break;
                case (byte)'4':
                    tarHeader.EntryType = TarEntryType.TYPE_BLOCK_SPECIAL;
                    break;
                case (byte)'5':
                    tarHeader.EntryType = TarEntryType.TYPE_DIRECTORY;
                    break;
                case (byte)'6':
                    tarHeader.EntryType = TarEntryType.TYPE_FIFO;
                    break;
                case (byte)'7':
                    tarHeader.EntryType = TarEntryType.TYPE_CONTIGUOUS_FILE;
                    break;
            }

            return tarHeader;
        }

        /// <summary>
        /// Read data following a header
        /// </summary>
        /// <param name="fileSize"></param>
        /// <returns></returns>
        protected byte[] ReadData(int fileSize) throws IOException
        {
            byte[] data = new byte[fileSize];
            
            m_br.read(data, 0, fileSize);

            //m_log.DebugFormat("[TAR ARCHIVE READER]: fileSize {0}", fileSize);

            // Read the rest of the empty padding in the 512 byte block
            if (fileSize % 512 != 0)
            {
                int paddingLeft = 512 - (fileSize % 512);

                //m_log.DebugFormat("[TAR ARCHIVE READER]: Reading {0} padding bytes", paddingLeft);

                m_br.read(new byte[paddingLeft], 0, paddingLeft);
            }

            return data;
        }

        public void Close() throws IOException
        {
            m_br.close();
        }

        /// <summary>
        /// Convert octal bytes to a decimal representation
        /// </summary>
        /// <param name="bytes"></param>
        /// <param name="count"></param>
        /// <param name="startIndex"></param>
        /// <returns></returns>
        public static int ConvertOctalBytesToDecimal(byte[] bytes, int startIndex, int count) throws UnsupportedEncodingException
        {
            // Trim leading white space: ancient tars do that instead
            // of leading 0s :-( don't ask. really.
            String oString = Utils.trimStart(Utils.bytesToString(bytes, startIndex, count, "US-ASCII"), m_spaceCharArray);

            int d = 0;

            for (char c : oString.toCharArray())
            {
                d <<= 3;
                d |= c - '0';
            }

            return d;
        }
    }

    