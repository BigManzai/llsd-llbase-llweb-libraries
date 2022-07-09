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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.Vector2;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.types.Vector4;

/// <summary>
/// 
/// </summary>
public final class OSDArray extends OSD implements Iterable<OSD>
{
    private List<OSD> value;

//     public  final OSDType type = OSDType.Array;

    public OSDType getType()
    {
    	return type;
    }
    
    public OSDArray()
    {
    	super();
    	type = OSDType.Array;
        value = new ArrayList<OSD>();
    }

    public OSDArray(int capacity)
    {
    	this();
        value = new ArrayList<OSD>(capacity);
    }

    public OSDArray(List<OSD> value)
    {
    	this();
        if (value != null)
            this.value = value;
        else
            this.value = new ArrayList<OSD>();
    }

    public  byte[] asBinary()
    {
        byte[] binary = new byte[value.size()];

        for (int i = 0; i < value.size(); i++)
            binary[i] = (byte)value.get(i).asInteger();

        return binary;
    }

    public  long asLong()
    {
        OSDBinary binary = new OSDBinary(asBinary());
        return binary.asLong();
    }

    public  int asInteger()
    {
        OSDBinary binary = new OSDBinary(asBinary());
        return binary.asInteger();
    }

    public  Vector2 asVector2()
    {
        Vector2 vector = Vector2.Zero;

        if (this.count()== 2)
        {
            vector.X = (float)(this.get(0).asReal());
            vector.Y = (float)this.get(1).asReal();
        }

        return vector;
    }

    public  Vector3 asVector3()
    {
        Vector3 vector = Vector3.Zero;

        if (this.count()== 3)
        {
            vector.X = (float)this.get(0).asReal();
            vector.Y = (float)this.get(1).asReal();
            vector.Z = (float)this.get(2).asReal();
        }

        return vector;
    }

    public  Vector3d asVector3d()
    {
        Vector3d vector = Vector3d.Zero;

        if (this.count()== 3)
        {
            vector.X = this.get(0).asReal();
            vector.Y = this.get(1).asReal();
            vector.Z = this.get(2).asReal();
        }

        return vector;
    }

    public  Vector4 asVector4()
    {
        Vector4 vector = Vector4.Zero;

        if (this.count()== 4)
        {
            vector.X = (float)this.get(0).asReal();
            vector.Y = (float)this.get(1).asReal();
            vector.Z = (float)this.get(2).asReal();
            vector.W = (float)this.get(3).asReal();
        }

        return vector;
    }

    public  Quaternion asQuaternion()
    {
        Quaternion quaternion = Quaternion.Identity;

        if (this.count()== 4)
        {
            quaternion.X = (float)this.get(0).asReal();
            quaternion.Y = (float)this.get(1).asReal();
            quaternion.Z = (float)this.get(2).asReal();
            quaternion.W = (float)this.get(3).asReal();
        }

        return quaternion;
    }

    public  Color4 asColor4()
    {
        Color4 color = Color4.Black;

        if (this.count()== 4)
        {
            color.setR((float)this.get(0).asReal());
            color.setG((float)this.get(1).asReal());
            color.setB((float)this.get(2).asReal());
            color.setA((float)this.get(3).asReal());
        }

        return color;
    }

    public  boolean asBoolean() { return value.size() > 0; }

    public  String toString()
    {
    	//TODO need to implement
        //return OSDParser.SerializeJsonString(this, true);
    	return "";
    }

    //region IList Implementation

    public int count() { return value.size();};
    public boolean isReadOnly (){ return false;};

    public OSD get(int index)
    {
    	return this.value.get(index);
    }    
    
    public void add(int index, OSD value)
    {
    	this.value.add(index, value);
    }
    
    public int indexOf(OSD llsd)
    {
        return value.indexOf(llsd);
    }

    public void insert(int index, OSD llsd)
    {
        value.add(index, llsd);
    }

    public void removeAt(int index)
    {
        value.remove(index);
    }

    public boolean add(OSD llsd)
    {
        return value.add(llsd);
    }

    public void clear()
    {
        value.clear();
    }

    public boolean contains(OSD llsd)
    {
        return value.contains(llsd);
    }

    public boolean contains(String element)
    {
        for (int i = 0; i < value.size(); i++)
        {
            if (value.get(i).getType() == OSDType.String && value.get(i).asString() == element)
                return true;
        }

        return false;
    }

    public void copyTo(OSD[] array, int index)
    {
        throw new UnsupportedOperationException();
    }

    public boolean remove(OSD llsd)
    {
        return value.remove(llsd);
    }

    public Iterator<OSD> iterator()
    {
        return value.iterator();
    }
    //endregion IList Implementation
}