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
package com.ngt.jopenmetaverse.shared.structureddata;

import java.math.BigInteger;

import com.ngt.jopenmetaverse.shared.util.Utils;
import org.apache.commons.codec.binary.Base64;


/// <summary>
/// 
/// </summary>
public final class OSDBinary extends OSD
{
  private byte[] value;

   @Override
  public OSDType getType()
   {
  	return type;
  }
  
  public OSDBinary()
  {
	  super();
	  type =  OSDType.Binary;
  }
   
  public OSDBinary(byte[] value)
  {
	  this();
      if (value != null)
          this.value = value;
      else
          this.value = Utils.EmptyBytes;
  }

  public OSDBinary(int value)
  {
	  this();
      this.value = new byte[]
      {
          (byte)((value >> 24) % 256),
          (byte)((value >> 16) % 256),
          (byte)((value >> 8) % 256),
          (byte)(value % 256)
      };
  }
  
  public OSDBinary(long value)
  {
	  this();
      this.value = new byte[]
      {
          (byte)((value >> 56) % 256),
          (byte)((value >> 48) % 256),
          (byte)((value >> 40) % 256),
          (byte)((value >> 32) % 256),
          (byte)((value >> 24) % 256),
          (byte)((value >> 16) % 256),
          (byte)((value >> 8) % 256),
          (byte)(value % 256)
      };
  }

  public OSDBinary(BigInteger value)
  {
	  this();
      this.value = Utils.ulongToBytes(value);
  }
  
  public  String asString() { return Utils.encodeBase64String(value); }
  public  byte[] asBinary() { return value; }

  @Override
  public int asInteger()
  {
      return (int) (
          (value[0] << 24) +
          (value[1] << 16) +
          (value[2] << 8) +
          (value[3] << 0));
  }

  @Override
  public long asUInteger()
  {
      return (int) (
          (value[0] << 24) +
          (value[1] << 16) +
          (value[2] << 8) +
          (value[3] << 0));
  }
  
  @Override
  public  long asLong()
  {
      return (long)(
          ((long)value[0] << 56) +
          ((long)value[1] << 48) +
          ((long)value[2] << 40) +
          ((long)value[3] << 32) +
          ((long)value[4] << 24) +
          ((long)value[5] << 16) +
          ((long)value[6] << 8) +
          ((long)value[7] << 0));
  }

  @Override
  public BigInteger asULong()
  {
	  return new BigInteger(value);
  }
  
  @Override
  public  String toString()
  {
      return Utils.bytesToHexDebugString(value, null);
  }
}