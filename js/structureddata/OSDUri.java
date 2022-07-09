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
import com.ngt.jopenmetaverse.shared.util.Utils;


/// <summary>
/// 
/// </summary>
public final class OSDUri extends OSD
{
    private URI value;

     @Override
    public OSDType getType()
    {
    	return type;
    }

     public OSDUri()
     {
    	 super();
    	 type  = OSDType.URI;
     }
     
    public OSDUri(URI value)
    {
    	this();
        this.value = value;
    }

    public  String asString()
    {
        if (value != null)
        {
            return value.toString();

//        	try {
//				return URLDecoder.decode(value.toString(), "UTF8");
//			} catch (UnsupportedEncodingException e) {
//				return "";
//			}
            
//            if (value.isAbsolute())
//                return value.toString();
//            else
//                return value.toString();
        }
        return "";
    }

    @Override
    public  URI asUri() { return value; }
    @Override
    public  byte[] asBinary() { return Utils.stringToBytes(asString()); }
    @Override
    public  String toString() { return asString(); }
}