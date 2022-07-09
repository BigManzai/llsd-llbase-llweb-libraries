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
package com.ngt.jopenmetaverse.shared.types;

import java.io.Serializable;
import java.math.BigInteger;
import java.security.NoSuchAlgorithmException;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class UUID implements Comparable<UUID>, Serializable
{
	/// <summary>The System.Guid object this struct wraps around</summary>
	public java.util.UUID Guid;

	//region Constructors

	public UUID()
	{
		Guid = new java.util.UUID(0x0000000000000000, 0x0000000000000000);
	}
	/// <summary>
	/// Constructor that takes a String UUID representation
	/// </summary>
	/// <param name="val">A String representation of a UUID, case 
	/// insensitive and can either be hyphenated or non-hyphenated</param>
	/// <example>UUID("11f8aa9c-b071-4242-836b-13b7abe0d489")</example>
	public UUID(String val)
	{
		if (Utils.isNullOrEmpty(val))
			Guid = new java.util.UUID(0x0000000000000000, 0x0000000000000000);
		else
			Guid = java.util.UUID.fromString(val);
	}

	/// <summary>
	/// Constructor that takes a System.Guid object
	/// </summary>
	/// <param name="val">A Guid object that contains the unique identifier
	/// to be represented by this UUID</param>
	public UUID(java.util.UUID val)
	{
		Guid = val;
	}

	/// <summary>
	/// Constructor that takes a byte array containing a UUID
	/// </summary>
	/// <param name="source">Byte array containing a 16 byte UUID</param>
	/// <param name="pos">Beginning offset in the array</param>
	public UUID(byte[] source, int pos)
	{
		Guid = UUID.Zero.Guid;
		FromBytes(source, pos);
	}

	/// <summary>
	/// Constructor that takes an unsigned 64-bit unsigned integer to 
	/// convert to a UUID
	/// </summary>
	/// <param name="val">64-bit unsigned integer to convert to a UUID</param>
	public UUID(long val)
	{
		Guid = new java.util.UUID(0x0000000000000000, val);
	}

	/// <summary>
	/// Copy constructor
	/// </summary>
	/// <param name="val">UUID to copy</param>
	public UUID(UUID val)
	{
		Guid = val.Guid;
	}

	//endregion Constructors

	//region Public Methods

	/// <summary>
	/// IComparable.CompareTo implementation
	/// </summary>
	public int compareTo(UUID id)
	{
		return Guid.compareTo(id.Guid);
	}

	/// <summary>
	/// Assigns this UUID from 16 bytes out of a byte array
	/// </summary>
	/// <param name="source">Byte array containing the UUID to assign this UUID to</param>
	/// <param name="pos">Starting position of the UUID in the byte array</param>
	public void FromBytes(byte[] source, int pos)
	{
		long msl = Utils.bytesToInt64(source, pos);
		long lsl = Utils.bytesToInt64(source, pos+8);

		Guid = new java.util.UUID(msl, lsl);
	}

	/// <summary>
	/// Returns a copy of the raw bytes for this UUID
	/// </summary>
	/// <returns>A 16 byte array containing this UUID</returns>
	public byte[] GetBytes()
	{
		byte[] output = new byte[16];
		ToBytes(output, 0);
		return output;
	}

	/// <summary>
	/// Writes the raw bytes for this UUID to a byte array
	/// </summary>
	/// <param name="dest">Destination byte array</param>
	/// <param name="pos">Position in the destination array to start
	/// writing. Must be at least 16 bytes before the end of the array</param>
	public void ToBytes(byte[] dest, int pos)
	{
		long msl = Guid.getMostSignificantBits();
		long lsl = Guid.getLeastSignificantBits();

		byte[] mslBytes = Utils.int64ToBytes(msl);
		byte[] lslBytes = Utils.int64ToBytes(lsl);

		System.arraycopy(mslBytes, 0, dest, pos, 8);
		System.arraycopy(lslBytes, 0, dest, pos+8, 8);

	}

	/// <summary>
	/// Calculate an LLCRC (cyclic redundancy check) for this UUID
	/// </summary>
	/// <returns>The CRC checksum for this UUID</returns>
	public long CRC()
	{
		long retval = 0;
		byte[] bytes = GetBytes();

		retval += (long)((bytes[3] << 24) + (bytes[2] << 16) + (bytes[1] << 8) + bytes[0]);
		retval += (long)((bytes[7] << 24) + (bytes[6] << 16) + (bytes[5] << 8) + bytes[4]);
		retval += (long)((bytes[11] << 24) + (bytes[10] << 16) + (bytes[9] << 8) + bytes[8]);
		retval += (long)((bytes[15] << 24) + (bytes[14] << 16) + (bytes[13] << 8) + bytes[12]);

		return retval;
	}

	/// <summary>
	/// Create a 64-bit integer representation from the second half of this UUID
	/// </summary>
	/// <returns>An integer created from the last eight bytes of this UUID</returns>
	public BigInteger GetULong()
	{
		return new BigInteger(Utils.int64ToBytes(Guid.getLeastSignificantBits()));
	}

	//endregion Public Methods

	//region Static Methods

	/// <summary>
	/// Generate a UUID from a String
	/// </summary>
	/// <param name="val">A String representation of a UUID, case 
	/// insensitive and can either be hyphenated or non-hyphenated</param>
	/// <example>UUID.Parse("11f8aa9c-b071-4242-836b-13b7abe0d489")</example>
	public static UUID Parse(String val)
	{
		return new UUID(val);
	}

	/// <summary>
	/// Generate a UUID from a String
	/// </summary>
	/// <param name="val">A String representation of a UUID, case 
	/// insensitive and can either be hyphenated or non-hyphenated</param>
	/// <param name="result">Will contain the parsed UUID if successful,
	/// otherwise null</param>
	/// <returns>True if the String was successfully parse, otherwise false</returns>
	/// <example>UUID.TryParse("11f8aa9c-b071-4242-836b-13b7abe0d489", result)</example>
	public static boolean TryParse(String val, UUID[] result)
	{
		if (Utils.isNullOrEmpty(val) ||
				(val.charAt(0) == '{' && val.length() != 38) ||
				(val.length() != 36 && val.length() != 32))
		{
			result[0] = UUID.Zero;
			return false;
		}

		try
		{
			result[0] = Parse(val);
			return true;
		}
		catch (Exception e)
		{
			result[0] = UUID.Zero;
			return false;
		}
	}

	/// <summary>
	/// Combine two UUIDs together by taking the MD5 hash of a byte array
	/// containing both UUIDs
	/// </summary>
	/// <param name="first">First UUID to combine</param>
	/// <param name="second">Second UUID to combine</param>
	/// <returns>The UUID product of the combination</returns>
	public static UUID Combine(UUID first, UUID second) throws NoSuchAlgorithmException
	{
		// Construct the buffer that MD5ed
		byte[] input = new byte[32];
		System.arraycopy(first.GetBytes(), 0, input, 0, 16);
		System.arraycopy(second.GetBytes(), 0, input, 16, 16);
		return new UUID(Utils.MD5(input), 0);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <returns></returns>
	public static UUID Random()
	{
		return new UUID(java.util.UUID.randomUUID());
	}

	//endregion Static Methods

	//region Overrides

	/// <summary>
	/// Return a hash code for this UUID, used by .NET for hash tables
	/// </summary>
	/// <returns>An integer composed of all the UUID bytes XORed together</returns>
	@Override
	public int hashCode()
	{
		return Guid.hashCode();
	}
	
	/// <summary>
	/// Comparison function
	/// </summary>
	/// <param name="o">An object to compare to this UUID</param>
	/// <returns>True if the object is a UUID and both UUIDs are equal</returns>
	public boolean equals(Object o)
	{
		if (!(o instanceof UUID)) return false;

		UUID uuid = (UUID)o;
		return Guid.equals(uuid.Guid);
	}

	/// <summary>
	/// Comparison function
	/// </summary>
	/// <param name="uuid">UUID to compare to</param>
	/// <returns>True if the UUIDs are equal, otherwise false</returns>
	public boolean equals(UUID uuid)
	{
		return Guid.equals(uuid.Guid);
	}

	/// <summary>
	/// Get a hyphenated String representation of this UUID
	/// </summary>
	/// <returns>A String representation of this UUID, lowercase and 
	/// with hyphens</returns>
	/// <example>11f8aa9c-b071-4242-836b-13b7abe0d489</example>
	public String toString()
	{
		return Guid.toString();
	}

	//endregion Overrides

	//region Operators

	//			        /// <summary>
	//			        /// Equals operator
	//			        /// </summary>
	//			        /// <param name="lhs">First UUID for comparison</param>
	//			        /// <param name="rhs">Second UUID for comparison</param>
	//			        /// <returns>True if the UUIDs are byte for byte equal, otherwise false</returns>
	//			        public static bool operator ==(UUID lhs, UUID rhs)
	//			        {
	//			            return lhs.Guid == rhs.Guid;
	//			        }
	//
	//			        /// <summary>
	//			        /// Not equals operator
	//			        /// </summary>
	//			        /// <param name="lhs">First UUID for comparison</param>
	//			        /// <param name="rhs">Second UUID for comparison</param>
	//			        /// <returns>True if the UUIDs are not equal, otherwise true</returns>
	//			        public static bool operator !=(UUID lhs, UUID rhs)
	//			        {
	//			            return !(lhs == rhs);
	//			        }

	/// <summary>
	/// XOR operator
	/// </summary>
	/// <param name="lhs">First UUID</param>
	/// <param name="rhs">Second UUID</param>
	/// <returns>A UUID that is a XOR combination of the two input UUIDs</returns>
	public static UUID xor(UUID lhs, UUID rhs)
	{
		byte[] lhsbytes = lhs.GetBytes();
		byte[] rhsbytes = rhs.GetBytes();
		byte[] output = new byte[16];

		for (int i = 0; i < 16; i++)
		{
			output[i] = (byte)(lhsbytes[i] ^ rhsbytes[i]);
		}

		return new UUID(output, 0);
	}

	//			        /// <summary>
	//			        /// String typecasting operator
	//			        /// </summary>
	//			        /// <param name="val">A UUID in String form. Case insensitive, 
	//			        /// hyphenated or non-hyphenated</param>
	//			        /// <returns>A UUID built from the String representation</returns>
	//			        public static explicit operator UUID(String val)
	//			        {
	//			            return new UUID(val);
	//			        }

	//endregion Operators

	/// <summary>An UUID with a value of all zeroes</summary>
	public static final UUID Zero = new UUID();

	/// <summary>A cache of UUID.Zero as a String to optimize a common path</summary>
	private static final String ZeroString = new java.util.UUID(0x0000000000000000, 0x0000000000000000).toString();
}