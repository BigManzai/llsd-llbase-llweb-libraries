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


/// <summary>
/// 
/// </summary>
public final class OSDBoolean extends OSD
{
  private boolean value;

  private static byte[] trueBinary = { 0x31 };
  private static byte[] falseBinary = { 0x30 };

  public OSDBoolean()
  {
	  super();
	  type = getType();
  }
  
  public OSDBoolean(boolean value)
  {
	  this();
      this.value = value;
  }

  @Override
  public OSDType getType()
  {
	  return OSDType.Boolean;
  }
  
  @Override
  public  boolean asBoolean() { return value; }
  
  @Override
  public  int asInteger() { return value ? 1 : 0; }

  @Override
  public  double asReal() { return value ? 1d : 0d; }
  
  @Override
  public  String asString() { return value ? "1" : "0"; }
  
  @Override
  public  byte[] asBinary() { return value ? trueBinary : falseBinary; }

  @Override
  public  String toString() { return asString(); }
}