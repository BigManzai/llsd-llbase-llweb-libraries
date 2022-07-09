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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/// <summary>
/// 
/// </summary>
public final class OSDMap extends OSD
{
    private Map<String, OSD> value;

//     public final OSDType type =  OSDType.Map;

    public OSDType getType()
    {
    	return type;
    }
    
    public OSDMap()
    {
    	super();
    	type =  OSDType.Map;
        value = new HashMap<String, OSD>();
    }

    public OSDMap(int capacity)
    {
    	this();
        value = new HashMap<String, OSD>(capacity);
    }

    public OSDMap(Map<String, OSD> value)
    {
    	this();
        if (value != null)
            this.value = value;
        else
            this.value = new HashMap<String, OSD>();
    }

    public  boolean asBoolean() { return value.size() > 0; }

    public  String toString()
    {
    	//TODO Need to implement
        //return OSDParser.SerializeJsonString(this, true);
    	return "";
    }

    //region IDictionary Implementation

    public int count() { return value.size(); } 
    public boolean isReadOnly() { return false; } 
    public Set<String> keys() { return value.keySet(); } 
    public Collection<OSD> values() { return value.values(); }
    public OSD get(String key)
    {
            OSD llsd = this.value.get(key);
            if (llsd != null)
                return llsd;
            else
                return new OSD();
    }

    public boolean containsKey(String key)
    {
        return value.containsKey(key);
    }

    public void put(String key, OSD llsd)
    {
        value.put(key, llsd);
    }

    public void put(Map.Entry<String, OSD> kvp)
    {
        value.put(kvp.getKey(), kvp.getValue());
    }

    public OSD remove(String key)
    {
        return value.remove(key);
    }

    public void clear()
    {
        value.clear();
    }

//    public boolean Contains(Map.Entry<String, OSD> kvp)
//    {
//        // This is a bizarre function... we don't really implement it
//        // properly, hopefully no one wants to use it
//        return value.ContainsKey(kvp.Key);
//    }

//    public void CopyTo(Map.Entry<String, OSD>[] array, int index)
//    {
//        throw new NotImplementedException();
//    }

    public OSD remove(Map.Entry<String, OSD> kvp)
    {
        return this.value.remove(kvp.getKey());
    }

    public Set<Map.Entry<String, OSD>> entrySet()
    {
        return value.entrySet();
    }

//    IEnumerator<Map.Entry<String, OSD>> IEnumerable<Map.Entry<String, OSD>>.GetEnumerator()
//    {
//        return null;
//    }
//
//    IEnumerator IEnumerable.GetEnumerator()
//    {
//        return value.GetEnumerator();
//    }

    //endregion IDictionary Implementation
}