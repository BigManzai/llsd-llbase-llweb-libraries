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
public final class OSDReal extends OSD
{
    private double value;

//    public final OSDType type = OSDType.Real;

    public OSDReal()
    {
    	super();
    	type = OSDType.Real;
    }
    
    public OSDReal(double value)
    {
    	this();
        this.value = value;
    }

    public OSDType getType()
    {
    	return type;
    }
    
    @Override
    public  boolean asBoolean() { return (!Double.isNaN(value) && value != 0d); }
    
    @Override
    public  int asInteger()
    {
        if (Double.isNaN(value))
            return 0;
        if (value > (double)Integer.MAX_VALUE)
            return Integer.MAX_VALUE;
        if (value < (double)Integer.MIN_VALUE)
            return Integer.MIN_VALUE;
        return (int)Math.round(value);
    }

//    public  uint AsUInteger()
//    {
//        if (Double.IsNaN(value))
//            return 0;
//        if (value > (double)UInt32.MaxValue)
//            return UInt32.MaxValue;
//        if (value < (double)UInt32.MinValue)
//            return UInt32.MinValue;
//        return (uint)Math.Round(value);
//    }

    @Override
    public  long asLong()
    {
        if (Double.isNaN(value))
            return 0;
        if (value > (double)Long.MAX_VALUE)
            return Long.MAX_VALUE;
        if (value < (double)Long.MIN_VALUE)
            return Long.MIN_VALUE;
        return (long)Math.round(value);
    }

//    public  ulong AsULong()
//    {
//        if (Double.IsNaN(value))
//            return 0;
//        if (value > (double)UInt64.MaxValue)
//            return Int32.MaxValue;
//        if (value < (double)UInt64.MinValue)
//            return UInt64.MinValue;
//        return (ulong)Math.Round(value);
//    }

    @Override
    public  double asReal() { return value; }
    // "r" ensures the value will correctly round-trip back through Double.TryParse
    @Override
    public  String asString() { return Double.toString(value); }
    @Override
    public  byte[] asBinary() { return Utils.doubleToBytes(value); }
    @Override
    public  String toString() { return asString(); }
}