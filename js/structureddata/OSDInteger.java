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

import com.ngt.jopenmetaverse.shared.util.Utils;


/// <summary>
/// 
/// </summary>
public final class OSDInteger extends OSD
{
    private int value;

//    public  OSDType type = OSDType.Integer; 

    public OSDInteger()
    {
    	super();
    	type = OSDType.Integer; 
    }
    
    public OSDInteger(int value)
    {
    	this();
        this.value = value;
    }

    @Override
    public OSDType getType()
    { 
    	return OSDType.Integer;	
    }
    
    @Override
    public  boolean asBoolean() { return value != 0; }
    
    @Override
    public  int asInteger() { return value; }
//    public  uint asUInteger() { return (uint)value; }
    
    @Override
    public  long asLong() { return value; }
//    public  ulong asULong() { return (ulong)value; }
    
    @Override
    public  double asReal() { return (double)value; }
    
    @Override
    public  String asString() { return Integer.toString(value); }
    
    @Override
    public  byte[] asBinary() { return Utils.intToBytes(value); }
    
    @Override
    public  String toString() { return asString(); }
}