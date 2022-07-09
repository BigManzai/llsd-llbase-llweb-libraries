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

import com.ngt.jopenmetaverse.shared.util.Utils;


    public class Vector4 implements Comparable<Vector4>, Serializable
    {
        /// <summary>X value</summary>
        public float X;
        /// <summary>Y value</summary>
        public float Y;
        /// <summary>Z value</summary>
        public float Z;
        /// <summary>W value</summary>
        public float W;

        //region finalructors

        public Vector4()
        {
        	this(0f,0f,0f,0f);
        }
        
        public Vector4(float x, float y, float z, float w)
        {
            X = x;
            Y = y;
            Z = z;
            W = w;
        }

        public Vector4(Vector2 value, float z, float w)
        {
            X = value.X;
            Y = value.Y;
            Z = z;
            W = w;
        }

        public Vector4(Vector3 value, float w)
        {
            X = value.X;
            Y = value.Y;
            Z = value.Z;
            W = w;
        }

        public Vector4(float value)
        {
            X = value;
            Y = value;
            Z = value;
            W = value;
        }

        /// <summary>
        /// finalructor, builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing four four-byte floats</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public Vector4(byte[] byteArray, int pos)
        {
            X = Y = Z = W = 0f;
            fromBytesLit(byteArray, pos);
        }

        public Vector4(Vector4 value)
        {
            X = value.X;
            Y = value.Y;
            Z = value.Z;
            W = value.W;
        }

        //endregion finalructors

        //region Public Methods

        public float length()
        {
            return (float)Math.sqrt(distanceSquared(this, Zero));
        }

        public float lengthSquared()
        {
            return distanceSquared(this, Zero);
        }

        public void normalize()
        {
            normalize(this);
        }

        /// <summary>
        /// Test if this vector is equal to another vector, within a given
        /// tolerance range
        /// </summary>
        /// <param name="vec">Vector to test against</param>
        /// <param name="tolerance">The acceptable magnitude of difference
        /// between the two vectors</param>
        /// <returns>True if the magnitude of difference between the two vectors
        /// is less than the given tolerance, otherwise false</returns>
        public boolean approxEquals(Vector4 vec, float tolerance)
        {
            Vector4 diff = subtract(this, vec);
            return (diff.lengthSquared() <= tolerance * tolerance);
        }

        /// <summary>
        /// IComparable.CompareTo implementation
        /// </summary>
        public int compareTo(Vector4 vector)
        {
            return Float.compare(length(), vector.length());
        }

        /// <summary>
        /// Test if this vector is composed of all finite numbers
        /// </summary>
        public boolean isFinite()
        {
            return (Utils.isFinite(X) && Utils.isFinite(Y) && Utils.isFinite(Z) && Utils.isFinite(W));
        }

        /// <summary>
        /// Builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing a 16 byte vector</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytesLit(byte[] bytes, int pos)
        {            
            X = Utils.bytesToFloatLit(bytes, pos);
            Y = Utils.bytesToFloatLit(bytes, pos+4);
            Z = Utils.bytesToFloatLit(bytes, pos+8);
            W = Utils.bytesToFloatLit(bytes, pos+12);
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>A 16 byte array containing X, Y, Z, and W</returns>
        public byte[] getBytesLit()
        {
            byte[] byteArray = new byte[16];
            toBytesLit(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 16 bytes before the end of the array</param>
        public void toBytesLit(byte[] dest, int pos)
        {   
            byte[] xbytes = Utils.floatToBytesLit(X);
            byte[] ybytes = Utils.floatToBytesLit(Y);
            byte[] zbytes = Utils.floatToBytesLit(Z);
            byte[] wbytes = Utils.floatToBytesLit(W);            
            System.arraycopy(xbytes, 0, dest, pos, 4);
            System.arraycopy(ybytes, 0, dest, pos+4, 4);
            System.arraycopy(zbytes, 0, dest, pos+8, 4);
            System.arraycopy(wbytes, 0, dest, pos+12, 4);            
        }

        /// <summary>
        /// Builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing a 16 byte vector</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytes(byte[] bytes, int pos)
        {            
            X = Utils.bytesToFloat(bytes, pos);
            Y = Utils.bytesToFloat(bytes, pos+4);
            Z = Utils.bytesToFloat(bytes, pos+8);
            W = Utils.bytesToFloat(bytes, pos+12);
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>A 16 byte array containing X, Y, Z, and W</returns>
        public byte[] getBytes()
        {
            byte[] byteArray = new byte[16];
            toBytes(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 16 bytes before the end of the array</param>
        public void toBytes(byte[] dest, int pos)
        {   
            byte[] xbytes = Utils.floatToBytes(X);
            byte[] ybytes = Utils.floatToBytes(Y);
            byte[] zbytes = Utils.floatToBytes(Z);
            byte[] wbytes = Utils.floatToBytes(W);            
            System.arraycopy(xbytes, 0, dest, pos, 4);
            System.arraycopy(ybytes, 0, dest, pos+4, 4);
            System.arraycopy(zbytes, 0, dest, pos+8, 4);
            System.arraycopy(wbytes, 0, dest, pos+12, 4);            
        }
        
        //endregion Public Methods

        //region Static Methods


        public static Vector4 clamp(Vector4 value1, Vector4 min, Vector4 max)
        {
            return new Vector4(
                Utils.clamp(value1.X, min.X, max.X),
                Utils.clamp(value1.Y, min.Y, max.Y),
                Utils.clamp(value1.Z, min.Z, max.Z),
                Utils.clamp(value1.W, min.W, max.W));
        }

        public static float distance(Vector4 value1, Vector4 value2)
        {
            return (float)Math.sqrt(distanceSquared(value1, value2));
        }

        public static float distanceSquared(Vector4 value1, Vector4 value2)
        {
            return
                (value1.W - value2.W) * (value1.W - value2.W) +
                (value1.X - value2.X) * (value1.X - value2.X) +
                (value1.Y - value2.Y) * (value1.Y - value2.Y) +
                (value1.Z - value2.Z) * (value1.Z - value2.Z);
        }


        public static float dot(Vector4 vector1, Vector4 vector2)
        {
            return vector1.X * vector2.X + vector1.Y * vector2.Y + vector1.Z * vector2.Z + vector1.W * vector2.W;
        }

        public static Vector4 Lerp(Vector4 value1, Vector4 value2, float amount)
        {
            return new Vector4(
                Utils.lerp(value1.X, value2.X, amount),
                Utils.lerp(value1.Y, value2.Y, amount),
                Utils.lerp(value1.Z, value2.Z, amount),
                Utils.lerp(value1.W, value2.W, amount));
        }

        public static Vector4 max(Vector4 value1, Vector4 value2)
        {
            return new Vector4(
               Math.max(value1.X, value2.X),
               Math.max(value1.Y, value2.Y),
               Math.max(value1.Z, value2.Z),
               Math.max(value1.W, value2.W));
        }

        public static Vector4 min(Vector4 value1, Vector4 value2)
        {
            return new Vector4(
               Math.min(value1.X, value2.X),
               Math.min(value1.Y, value2.Y),
               Math.min(value1.Z, value2.Z),
               Math.min(value1.W, value2.W));
        }

        public static Vector4 negate(Vector4 value)
        {
        	Vector4 result = new Vector4();
        	result.X = -value.X;
        	result.Y = -value.Y;
        	result.Z = -value.Z;
        	result.W = -value.W;
            return result;
        }

        public static Vector4 normalize(Vector4 vector)
        {
        	Vector4 result = new Vector4();
            final float MAG_THRESHOLD = 0.0000001f;
            float factor = distanceSquared(vector, Zero);
            if (factor > MAG_THRESHOLD)
            {
                factor = 1f / (float)Math.sqrt(factor);
                result.X = vector.X * factor;
                result.Y = vector.Y * factor;
                result.Z = vector.Z * factor;
                result.W = vector.W *factor;
            }
            else
            {
            	result.X = 0f;
            	result.Y = 0f;
            	result.Z = 0f;
            	result.W = 0f;
            }
            return result;
        }

        public static Vector4 smoothStep(Vector4 value1, Vector4 value2, float amount)
        {
            return new Vector4(
                Utils.smoothStep(value1.X, value2.X, amount),
                Utils.smoothStep(value1.Y, value2.Y, amount),
                Utils.smoothStep(value1.Z, value2.Z, amount),
                Utils.smoothStep(value1.W, value2.W, amount));
        }

        public static Vector4 transform(Vector2 position, Matrix4 matrix)
        {
            return new Vector4(
                (position.X * matrix.M11) + (position.Y * matrix.M21) + matrix.M41,
                (position.X * matrix.M12) + (position.Y * matrix.M22) + matrix.M42,
                (position.X * matrix.M13) + (position.Y * matrix.M23) + matrix.M43,
                (position.X * matrix.M14) + (position.Y * matrix.M24) + matrix.M44);
        }

        public static Vector4 transform(Vector3 position, Matrix4 matrix)
        {
            return new Vector4(
                (position.X * matrix.M11) + (position.Y * matrix.M21) + (position.Z * matrix.M31) + matrix.M41,
                (position.X * matrix.M12) + (position.Y * matrix.M22) + (position.Z * matrix.M32) + matrix.M42,
                (position.X * matrix.M13) + (position.Y * matrix.M23) + (position.Z * matrix.M33) + matrix.M43,
                (position.X * matrix.M14) + (position.Y * matrix.M24) + (position.Z * matrix.M34) + matrix.M44);
        }

        public static Vector4 transform(Vector4 vector, Matrix4 matrix)
        {
            return new Vector4(
                (vector.X * matrix.M11) + (vector.Y * matrix.M21) + (vector.Z * matrix.M31) + (vector.W * matrix.M41),
                (vector.X * matrix.M12) + (vector.Y * matrix.M22) + (vector.Z * matrix.M32) + (vector.W * matrix.M42),
                (vector.X * matrix.M13) + (vector.Y * matrix.M23) + (vector.Z * matrix.M33) + (vector.W * matrix.M43),
                (vector.X * matrix.M14) + (vector.Y * matrix.M24) + (vector.Z * matrix.M34) + (vector.W * matrix.M44));
        }

        public static Vector4 parse(String val)
        {
            String splitChar = ",";
            String[] split = val.replace("<", "").replace(">", "").split(splitChar);
            return new Vector4(
                Float.parseFloat(split[0].trim()),
                Float.parseFloat(split[1].trim()),
                Float.parseFloat(split[2].trim()),
                Float.parseFloat(split[2].trim())
                );
            
            
        }

        public static boolean tryParse(String val, Vector4[] result)
        {
            try
            {
                result[0] = parse(val);
                return true;
            }
            catch (Exception e)
            {
                result[0] = new Vector4();
                return false;
            }
        }

        //endregion Static Methods

        //region Overrides

        public boolean equals(Object obj)
        {
            return (obj instanceof Vector4) ? equals((Vector4)obj) : false;
        }

        public boolean equals(Vector4 other)
        {
            return W == other.W
                && X == other.X
                && Y == other.Y
                && Z == other.Z;
        }

        public int hashCode()
        {
            return (new Float(X)).hashCode() ^ (new Float(Y)).hashCode() ^ (new Float(Z)).hashCode() ^ (new Float(W)).hashCode();
        }

        public String toString()
        {
            return "<"+ X + ", " + Y + ", " + Z + ", " + W + ">";
        }

        /// <summary>
        /// Get a String representation of the vector elements with up to three
        /// decimal digits and separated by spaces only
        /// </summary>
        /// <returns>Raw String representation of the vector</returns>
        public String toRawString()
        {
            return ""+ X + ", " + Y + ", " + Z + ", " + W;
        }

        //endregion Overrides

        //region Operators

        public static boolean equals(Vector4 value1, Vector4 value2)
        {
            return value1.W == value2.W
                && value1.X == value2.X
                && value1.Y == value2.Y
                && value1.Z == value2.Z;
        }

        public static boolean notEquals(Vector4 value1, Vector4 value2)
        {
            return !(value1 == value2);
        }

        public static Vector4 add(Vector4 value1, Vector4 value2)
        {
        	Vector4 result = new Vector4();
        	result.W = value1.W + value2.W;
        	result.X = value1.X + value2.X;
        	result.Y = value1.Y + value2.Y;
        	result.Z = value1.Z + value2.Z;
            return result;
        }

        public static Vector4 subtract(Vector4 value)
        {
            return new Vector4(-value.X, -value.Y, -value.Z, -value.W);
        }

        public static Vector4 subtract(Vector4 value1, Vector4 value2)
        {
        	Vector4 result = new Vector4();
        	result.W = value1.W - value2.W;
        	result.X = value1.X - value2.X;
        	result.Y = value1.Y - value2.Y;
        	result.Z = value1.Z - value2.Z;
            return result;
        }

        public static Vector4 multiply(Vector4 value1, Vector4 value2)
        {
        	Vector4 result = new Vector4();
        	result.W = value1.W * value2.W;
        	result.X = value1.X * value2.X;
        	result.Y = value1.Y * value2.Y;
        	result.Z = value1.Z * value2.Z;
            return result;
        }

        public static Vector4 multiply(Vector4 value1, float scaleFactor)
        {
        	Vector4 result = new Vector4();
        	result.W = value1.W * scaleFactor;
        	result.X = value1.X * scaleFactor;
        	result.Y = value1.Y * scaleFactor;
        	result.Z = value1.Z * scaleFactor;
            return result;
        }

        public static Vector4 divide(Vector4 value1, Vector4 value2)
        {
        	Vector4 result = new Vector4();
        	result.W = value1.W / value2.W;
        	result.X = value1.X / value2.X;
        	result.Y = value1.Y / value2.Y;
        	result.Z = value1.Z / value2.Z;
            return result;
        }

        public static Vector4 divide(Vector4 value1, float divider)
        {
        	Vector4 result = new Vector4();
            float factor = 1f / divider;
            result.W = value1.W * factor;
            result.X = value1.X * factor;
            result.Y = value1.Y * factor;
            result.Z = value1.Z * factor;
            return result;
        }

        //endregion Operators

        /// <summary>A vector with a value of 0,0,0,0</summary>
        public final static Vector4 Zero = new Vector4();
        /// <summary>A vector with a value of 1,1,1,1</summary>
        public final static Vector4 One = new Vector4(1f, 1f, 1f, 1f);
        /// <summary>A vector with a value of 1,0,0,0</summary>
        public final static Vector4 UnitX = new Vector4(1f, 0f, 0f, 0f);
        /// <summary>A vector with a value of 0,1,0,0</summary>
        public final static Vector4 UnitY = new Vector4(0f, 1f, 0f, 0f);
        /// <summary>A vector with a value of 0,0,1,0</summary>
        public final static Vector4 UnitZ = new Vector4(0f, 0f, 1f, 0f);
        /// <summary>A vector with a value of 0,0,0,1</summary>
        public final static Vector4 UnitW = new Vector4(0f, 0f, 0f, 1f);
    }
