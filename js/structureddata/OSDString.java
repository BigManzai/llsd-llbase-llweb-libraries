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

import java.net.URI;
import java.util.Date;

import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// 
/// </summary>
public final class OSDString extends OSD
{
    private String value;

//    public final OSDType type =  OSDType.String;

    public OSDString()
    {
    	super();
    	type =  OSDType.String;
    }
    
    public OSDString(String value)
    {
    	this();
        // Refuse to hold null pointers
        if (value != null)
            this.value = value;
        else
            this.value = "";
    }

    @Override
    public OSDType getType()
    {
    	return type;
    }
    
    @Override
    public  boolean asBoolean()
    {
        if (Utils.isNullOrEmpty(value))
            return false;

        if (value.equals("0") || value.toLowerCase().equals("false"))
            return false;

        return true;
    }

    @Override
    public  int asInteger()
    {
        double[] dbl = new double[1];
        if(Utils.tryParseDouble(value, dbl))
        	return (int)Math.floor(dbl[0]);
        else
        	return 0;
    }
    
//
//    public  uint asUInteger()
//    {
//        double dbl;
//        if (Double.TryParse(value, out dbl))
//            return (uint)Math.Floor(dbl);
//        else
//            return 0;
//    }

    @Override
    public  long asLong()
    {
        double[] dbl = new double[1];
        if(Utils.tryParseDouble(value, dbl))
        	return (long)Math.floor(dbl[0]);
        else
        	return 0;
    }

//    public  ulong asULong()
//    {
//        double dbl;
//        if (Double.TryParse(value, out dbl))
//            return (ulong)Math.Floor(dbl);
//        else
//            return 0;
//    }

    @Override
    public  double asReal()
    {
        double[] dbl = new double[1];
        if(Utils.tryParseDouble(value, dbl))
        	return Math.floor(dbl[0]);
        else
        	return 0d;
    }

    @Override
    public  String asString() { return value; }
    
    @Override
    public  byte[] asBinary() { return Utils.stringToBytes(value); }
    
    @Override
    public  UUID asUUID()
    {
        UUID[] uuid = new UUID[1];
        if (UUID.TryParse(value, uuid))
            return uuid[0];
        else
            return UUID.Zero;
    }
    
    @Override
    public  Date asDate()
    {
        Date[] dt = new Date[1];
        if (Utils.tryParseDate(value, dt))
            return dt[0];
        else
            return Utils.Epoch;
    }
    
    @Override
    public  URI asUri()
    {
    	URI[] uri = new URI[1];
        if (Utils.tryParseUri(value, uri))
            return uri[0];
        else
            return null;
    }

    @Override
    public  String toString() { return asString(); }
}