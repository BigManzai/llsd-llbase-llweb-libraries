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

import java.io.Serializable;
import java.lang.reflect.Field;
import java.math.BigInteger;
import java.net.URI;
import java.util.Date;
import java.util.logging.Logger;

import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector2;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.types.Vector3d;
import com.ngt.jopenmetaverse.shared.types.Vector4;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// 
/// </summary>
public class OSD
{
	protected static Logger logger = Logger.getLogger("OSD"); 
    protected OSDType type;

    public boolean asBoolean() { return false; }
    public int asInteger() { return 0; }
    public long asUInteger() { return 0; }
    public long asLong() { return 0; }
    public BigInteger asULong() { return new BigInteger("0"); }
    public double asReal() { return 0d; }
    public String asString() { return ""; }
    public UUID asUUID() { return UUID.Zero; }
    public Date asDate() { return Utils.Epoch; }
    public URI asUri() { return null; }
    public byte[] asBinary() { return Utils.EmptyBytes; }
    public Vector2 asVector2() { return Vector2.Zero; }
    public Vector3 asVector3() { return Vector3.Zero; }
    public Vector3d asVector3d() { return Vector3d.Zero; }
    public Vector4 asVector4() { return Vector4.Zero; }
    public Quaternion asQuaternion() { return Quaternion.Identity; }
    public Color4 asColor4() { return Color4.Black; }

    public String toString() { return "undef"; }

    public static OSD FromBoolean(boolean value) { return new OSDBoolean(value); }
    public static OSD FromInteger(int value) { return new OSDInteger(value); }
    public static OSD FromInteger(short value) { return new OSDInteger((int)value); }
    public static OSD FromInteger(byte value) { return new OSDInteger((int)value); }
    public static OSD FromUInteger(long value) { return new OSDBinary(Utils.uintToBytes(value)); }
    public static OSD FromLong(long value) { return new OSDBinary(value); }
    public static OSD FromULong(BigInteger value) { return new OSDBinary((BigInteger)value); }
    public static OSD FromReal(double value) { return new OSDReal(value); }
    public static OSD FromReal(float value) { return new OSDReal((double)value); }
    public static OSD FromString(String value) { return new OSDString(value); }
    public static OSD FromUUID(UUID value) { return new OSDUUID(value); }
    public static OSD FromDate(Date value) { return new OSDDate(value); }
    public static OSD FromUri(URI value) { return new OSDUri(value); }
    public static OSD FromBinary(byte[] value) { return new OSDBinary(value); }

    public OSD()
    {
    	type =  OSDType.Unknown;
    }
    
    public OSDType getType()
    {
    	return type;
    }
    
    public static OSD FromVector2(Vector2 value)
    {
        OSDArray array = new OSDArray();
        array.add(OSD.FromReal(value.X));
        array.add(OSD.FromReal(value.Y));
        return array;
    }

    public static OSD FromVector3(Vector3 value)
    {
        OSDArray array = new OSDArray();
        array.add(OSD.FromReal(value.X));
        array.add(OSD.FromReal(value.Y));
        array.add(OSD.FromReal(value.Z));
        return array;
    }

    public static OSD FromVector3d(Vector3d value)
    {
        OSDArray array = new OSDArray();
        array.add(OSD.FromReal(value.X));
        array.add(OSD.FromReal(value.Y));
        array.add(OSD.FromReal(value.Z));
        return array;
    }

    public static OSD FromVector4(Vector4 value)
    {
        OSDArray array = new OSDArray();
        array.add(OSD.FromReal(value.X));
        array.add(OSD.FromReal(value.Y));
        array.add(OSD.FromReal(value.Z));
        array.add(OSD.FromReal(value.W));
        return array;
    }

    public static OSD FromQuaternion(Quaternion value)
    {
        OSDArray array = new OSDArray();
        array.add(OSD.FromReal(value.X));
        array.add(OSD.FromReal(value.Y));
        array.add(OSD.FromReal(value.Z));
        array.add(OSD.FromReal(value.W));
        return array;
    }

    public static OSD FromColor4(Color4 value)
    {
        OSDArray array = new OSDArray();
        array.add(OSD.FromReal(value.getR()));
        array.add(OSD.FromReal(value.getG()));
        array.add(OSD.FromReal(value.getB()));
        array.add(OSD.FromReal(value.getA()));
        return array;
    }

    public static OSD FromObject(Object value)
    {
        if (value == null) { return new OSD(); }
        else if (value instanceof Boolean) { return new OSDBoolean((Boolean)value); }
        else if (value instanceof Integer) { return new OSDInteger((Integer)value); }
        else if (value instanceof Short) { return new OSDInteger(new Integer(((Short)value).intValue())); }
        else if (value instanceof Byte) { return new OSDInteger(new Integer((Byte)value).intValue()); }
        else if (value instanceof Double) { return new OSDReal((Double)value); }
        else if (value instanceof Float) { return new OSDReal(new Double((Float) value).doubleValue()); }
        else if (value instanceof String) { return new OSDString((String)value); }
        else if (value instanceof UUID) { return new OSDUUID((UUID)value); }
        else if (value instanceof Date) { return new OSDDate((Date)value); }
        else if (value instanceof URI) { return new OSDUri((URI)value); }
        else if (value instanceof Byte[]) { return new OSDBinary((byte[])value); }
        else if (value instanceof Long) { return new OSDBinary((Long)value); }
        else if (value instanceof BigInteger) { return new OSDBinary((BigInteger)value); }        
        else if (value instanceof Vector2) { return FromVector2((Vector2)value); }
        else if (value instanceof Vector3) { return FromVector3((Vector3)value); }
        else if (value instanceof Vector3d) { return FromVector3d((Vector3d)value); }
        else if (value instanceof Vector4) { return FromVector4((Vector4)value); }
        else if (value instanceof Quaternion) { return FromQuaternion((Quaternion)value); }
        else if (value instanceof Color4) { return FromColor4((Color4)value); }
        else return new OSD();
    }

    public static Object toObject(Class<?> type, OSD value)
    {
        if (type == Long.TYPE)
        {
            if (value.getType() == OSDType.Binary)
            {
                byte[] bytes = value.asBinary();
                return Utils.bytesToInt64(bytes);
            }
            else
            {
                return (long)value.asInteger();
            }
        }
        else if (type.equals(BigInteger.class))
        {
            if (value.getType() == OSDType.Binary)
            {
                byte[] bytes = value.asBinary();
                return Utils.bytesToULong((bytes));
            }
            else
            {
                return (BigInteger)value.asULong();
            }
        }        
        else if (type == Integer.TYPE)
        {
            if (value.getType() == OSDType.Binary)
            {
                byte[] bytes = value.asBinary();
                return Utils.bytesToInt(bytes);
            }
            else
            {
                return (int)value.asInteger();
            }
        }
        else if (type == Byte.TYPE)
        {
            return (byte)value.asInteger();
        }
        else if (type == Short.TYPE)
        {
            return (short)value.asInteger();
        }
        else if (type == String.class)
        {
            return value.asString();
        }
        else if (type == Boolean.TYPE)
        {
            return value.asBoolean();
        }
        else if (type == Float.TYPE)
        {
            return (float)value.asReal();
        }
        else if (type == Double.TYPE)
        {
            return value.asReal();
        }
        else if (type == Integer.TYPE)
        {
            return value.asInteger();
        }
        else if (type == UUID.class)
        {
            return value.asUUID();
        }
        else if (type == Vector3.class)
        {
            if (value.getType() == OSDType.Array)
                return ((OSDArray)value).asVector3();
            else
                return Vector3.Zero;
        }
        else if (type == Vector4.class)
        {
            if (value.getType() == OSDType.Array)
                return ((OSDArray)value).asVector4();
            else
                return Vector4.Zero;
        }
        else if (type == Quaternion.class)
        {
            if (value.getType() == OSDType.Array)
                return ((OSDArray)value).asQuaternion();
            else
                return Quaternion.Identity;
        }
        else
        {
            return null;
        }
    }

    //region Implicit Conversions
    public static  OSDBoolean getInstance(boolean value) { return new OSDBoolean(value); }
    public static  OSDInteger getInstance(int value) { return new OSDInteger(value); }
    public static  OSDInteger getInstance(short value) { return new OSDInteger((int)value); }
    public static  OSDInteger getInstance(byte value) { return new OSDInteger((int)value); }
    public static  OSDBinary getInstance(long value) { return new OSDBinary(value); }
    public static  OSDBinary getInstance(BigInteger value) { return new OSDBinary(value); }
    public static  OSDReal getInstance(double value) { return new OSDReal(value); }
    public static  OSDReal getInstance(float value) { return new OSDReal(value); }
    public static  OSDString getInstance(String value) { return new OSDString(value); }
    public static  OSDUUID getInstance(UUID value) { return new OSDUUID(value); }
    public static  OSDDate getInstance(Date value) { return new OSDDate(value); }
    public static  OSDUri getInstance(URI value) { return new OSDUri(value); }
    public static  OSDBinary getInstance(byte[] value) { return new OSDBinary(value); }
    public static  OSD getInstance(Vector2 value) { return OSD.FromVector2(value); }
    public static  OSD getInstance(Vector3 value) { return OSD.FromVector3(value); }
    public static  OSD getInstance(Vector3d value) { return OSD.FromVector3d(value); }
    public static  OSD getInstance(Vector4 value) { return OSD.FromVector4(value); }
    public static  OSD getInstance(Quaternion value) { return OSD.FromQuaternion(value); }
    public static  OSD getInstance(Color4 value) { return OSD.FromColor4(value); }

//    public static  boolean(OSD value) { return value.asBoolean(); }
//    public static  int(OSD value) { return value.asInteger(); }
//    public static  uint(OSD value) { return value.asUInteger(); }
//    public static  long(OSD value) { return value.asLong(); }
//    public static  ulong(OSD value) { return value.asULong(); }
//    public static  double(OSD value) { return value.asReal(); }
//    public static  float(OSD value) { return (float)value.asReal(); }
//    public static  String(OSD value) { return value.asString(); }
//    public static  UUID(OSD value) { return value.asUUID(); }
//    public static  Date(OSD value) { return value.asDate(); }
//    public static  Uri(OSD value) { return value.asUri(); }
//    public static  byte[](OSD value) { return value.asBinary(); }
//    public static  Vector2(OSD value) { return value.asVector2(); }
//    public static  Vector3(OSD value) { return value.asVector3(); }
//    public static  Vector3d(OSD value) { return value.asVector3d(); }
//    public static  Vector4(OSD value) { return value.asVector4(); }
//    public static  Quaternion(OSD value) { return value.asQuaternion(); }
//    public static  Color4(OSD value) { return value.asColor4(); }

    //endregion Implicit Conversions

    /// <summary>
    /// Uses reflection to create an SDMap from all of the SD
    /// serializable types in an Object
    /// </summary>
    /// <param name="obj">Class or struct containing serializable types</param>
    /// <returns>An SDMap holding the serialized values from the
    /// container Object</returns>
    public static OSDMap SerializeMembers(Object obj) throws IllegalArgumentException, IllegalAccessException
    {
        Field[] fields = obj.getClass().getFields();

        OSDMap map = new OSDMap(fields.length);
        for (int i = 0; i < fields.length; i++)
        {
            Field field = fields[i];
        	logger.info(field.getName());
            if (field.get(obj) instanceof Serializable)
            {
                OSD serializedField = OSD.FromObject(field.get(obj));
          
                if (serializedField.getType() != OSDType.Unknown || (field.get(obj)  instanceof String) || (field.get(obj) instanceof byte[]))
                    map.put(field.getName(), serializedField);
            }
            else
            	logger.info(field.getName() + "is not serializable");
        }
        return map;
    }

    /// <summary>
    /// Uses reflection to deserialize member variables in an Object from
    /// an SDMap
    /// </summary>
    /// <param name="obj">Reference to an Object to fill with deserialized
    /// values</param>
    /// <param name="serialized">Serialized values to put in the target
    /// Object</param>
    public static void DeserializeMembers(Object obj, OSDMap serialized) throws IllegalArgumentException, IllegalAccessException
    {
        Field[] fields = obj.getClass().getFields();

        for (int i = 0; i < fields.length; i++)
        {
            Field field = fields[i];
            if (field.get(obj) instanceof Serializable)
            {
                OSD serializedField = serialized.get(field.getName());
                if (serializedField != null)
                    field.set(obj, toObject(field.getClass(), serializedField));
            }
        }
    }
}


