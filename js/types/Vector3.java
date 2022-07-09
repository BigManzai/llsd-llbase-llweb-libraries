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

    /// <summary>
    /// A three-dimensional vector with floating-point values
    /// </summary>
    public class Vector3 implements Comparable<Vector3>, Serializable
    {
        /// <summary>X value</summary>
        public float X;
        /// <summary>Y value</summary>
        public float Y;
        /// <summary>Z value</summary>
        public float Z;

        //region Constructors

        public Vector3(float x, float y, float z)
        {
            X = x;
            Y = y;
            Z = z;
        }

        public Vector3(float value)
        {
            X = value;
            Y = value;
            Z = value;
        }

        public Vector3(Vector2 value, float z)
        {
            X = value.X;
            Y = value.Y;
            Z = z;
        }

        public Vector3(Vector3d vector)
        {
            X = (float)vector.X;
            Y = (float)vector.Y;
            Z = (float)vector.Z;
        }

        public Vector3()
        {
        	this(0f, 0f, 0f);
        }
        
        /// <summary>
        /// Constructor, builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing three four-byte floats</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public Vector3(byte[] byteArray, int pos)
        {
            X = Y = Z = 0f;
            fromBytesLit(byteArray, pos);
        }

        public Vector3(Vector3 vector)
        {
            X = vector.X;
            Y = vector.Y;
            Z = vector.Z;
        }

        //endregion Constructors

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
            final float MAG_THRESHOLD = 0.0000001f;
            float factor = distance(this, Zero);
            if (factor > MAG_THRESHOLD)
            {
                factor = 1f / factor;
                this.X = this.X * factor;
                this.Y = this.Y * factor;
                this.Z = this.Z * factor;
            }
            else
            {
            	this.X = 0f;
            	this.Y = 0f;
            	this.Z = 0f;
            }        	
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
        public boolean approxEquals(Vector3 vec, float tolerance)
        {
            Vector3 diff = subtract(this, vec);
            System.out.println(String.format("%f, %f", diff.lengthSquared(), tolerance * tolerance));
            return (diff.lengthSquared() <= tolerance * tolerance);
        }

        /// <summary>
        /// IComparable.CompareTo implementation
        /// </summary>
        public int compareTo(Vector3 vector)
        {
            return Float.compare(length(), vector.length());
        }
        
        /// <summary>
        /// Test if this vector is composed of all finite numbers
        /// </summary>
        public boolean isFinite()
        {
            return (Utils.isFinite(X) && Utils.isFinite(Y) && Utils.isFinite(Z));
        }

        /// <summary>
        /// Builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing a 12 byte vector</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytesLit(byte[] bytes, int pos)
        { 
                X = Utils.bytesToFloatLit(bytes, pos);
                Y = Utils.bytesToFloatLit(bytes, pos+4);
                Z = Utils.bytesToFloatLit(bytes, pos+8);
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>A 12 byte array containing X, Y, and Z</returns>
        public byte[] getBytesLit()
        {
            byte[] byteArray = new byte[12];
            toBytesLit(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 12 bytes before the end of the array</param>
        public void toBytesLit(byte[] dest, int pos)
        {            
            byte[] xbytes = Utils.floatToBytesLit(X);
            byte[] ybytes = Utils.floatToBytesLit(Y);
            byte[] zbytes = Utils.floatToBytesLit(Z);
            System.arraycopy(xbytes, 0, dest, pos, 4);
            System.arraycopy(ybytes, 0, dest, pos+4, 4);
            System.arraycopy(zbytes, 0, dest, pos+8, 4);
        }

        /// <summary>
        /// Builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing a 12 byte vector</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytes(byte[] bytes, int pos)
        { 
                X = Utils.bytesToFloat(bytes, pos);
                Y = Utils.bytesToFloat(bytes, pos+4);
                Z = Utils.bytesToFloat(bytes, pos+8);
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>A 12 byte array containing X, Y, and Z</returns>
        public byte[] getBytes()
        {
            byte[] byteArray = new byte[12];
            toBytes(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 12 bytes before the end of the array</param>
        public void toBytes(byte[] dest, int pos)
        {            
            byte[] xbytes = Utils.floatToBytes(X);
            byte[] ybytes = Utils.floatToBytes(Y);
            byte[] zbytes = Utils.floatToBytes(Z);
            System.arraycopy(xbytes, 0, dest, pos, 4);
            System.arraycopy(ybytes, 0, dest, pos+4, 4);
            System.arraycopy(zbytes, 0, dest, pos+8, 4);
        }

        
        //endregion Public Methods

        //region Static Methods


        public static Vector3 clamp(Vector3 value1, Vector3 min, Vector3 max)
        {
            return new Vector3(
                Utils.clamp(value1.X, min.X, max.X),
                Utils.clamp(value1.Y, min.Y, max.Y),
                Utils.clamp(value1.Z, min.Z, max.Z));
        }

        public static Vector3 cross(Vector3 value1, Vector3 value2)
        {
            return new Vector3(
                value1.Y * value2.Z - value2.Y * value1.Z,
                value1.Z * value2.X - value2.Z * value1.X,
                value1.X * value2.Y - value2.X * value1.Y);
        }

        public static float distance(Vector3 value1, Vector3 value2)
        {
            return (float)Math.sqrt(distanceSquared(value1, value2));
        }

        public static float distanceSquared(Vector3 value1, Vector3 value2)
        {
            return
                (value1.X - value2.X) * (value1.X - value2.X) +
                (value1.Y - value2.Y) * (value1.Y - value2.Y) +
                (value1.Z - value2.Z) * (value1.Z - value2.Z);
        }

        public static float dot(Vector3 value1, Vector3 value2)
        {
            return value1.X * value2.X + value1.Y * value2.Y + value1.Z * value2.Z;
        }

        public static Vector3 lerp(Vector3 value1, Vector3 value2, float amount)
        {
            return new Vector3(
                Utils.lerp(value1.X, value2.X, amount),
                Utils.lerp(value1.Y, value2.Y, amount),
                Utils.lerp(value1.Z, value2.Z, amount));
        }

        public static float mag(Vector3 value)
        {
            return (float)Math.sqrt((value.X * value.X) + (value.Y * value.Y) + (value.Z * value.Z));
        }

        public static Vector3 max(Vector3 value1, Vector3 value2)
        {
            return new Vector3(
                Math.max(value1.X, value2.X),
                Math.max(value1.Y, value2.Y),
                Math.max(value1.Z, value2.Z));
        }

        public static Vector3 min(Vector3 value1, Vector3 value2)
        {
            return new Vector3(
                Math.min(value1.X, value2.X),
                Math.min(value1.Y, value2.Y),
                Math.min(value1.Z, value2.Z));
        }

        public static Vector3 negate(Vector3 value)
        {
        	Vector3 result = new Vector3();
        	result.X = -value.X;
        	result.Y = -value.Y;
        	result.Z = -value.Z;
            return result;
        }

        public static Vector3 normalize(Vector3 value)
        {
        	Vector3 result = new Vector3(value);
           result.normalize();
            return result;
        }

        /// <summary>
        /// Parse a vector from a String
        /// </summary>
        /// <param name="val">A String representation of a 3D vector, enclosed 
        /// in arrow brackets and separated by commas</param>
        public static Vector3 parse(String val)
        {
            String splitChar = ",";
            String[] split = val.replace("<", "").replace(">", "").split(splitChar);
            return new Vector3(
                Float.parseFloat(split[0].trim()),
                Float.parseFloat(split[1].trim()),
                Float.parseFloat(split[2].trim()));
        }

        public static boolean TryParse(String val, Vector3[] result)
        {
            try
            {
                result[0] = parse(val);
                return true;
            }
            catch (Exception e)
            {
                result[0] = Vector3.Zero;
                return false;
            }
        }

        /// <summary>
        /// Calculate the rotation between two vectors
        /// </summary>
        /// <param name="a">Normalized directional vector (such as 1,0,0 for forward facing)</param>
        /// <param name="b">Normalized target vector</param>
        public static Quaternion rotationBetween(Vector3 a, Vector3 b)
        {
            float dotProduct = dot(a, b);
            Vector3 crossProduct = cross(a, b);
            float magProduct = a.length() * b.length();
            double angle = Math.acos(dotProduct / magProduct);
            Vector3 axis = normalize(crossProduct);
            float s = (float)Math.sin(angle / 2d);

            return new Quaternion(
                axis.X * s,
                axis.Y * s,
                axis.Z * s,
                (float)Math.cos(angle / 2d));
        }

        /// <summary>
        /// Interpolates between two vectors using a cubic equation
        /// </summary>
        public static Vector3 smoothStep(Vector3 value1, Vector3 value2, float amount)
        {
            return new Vector3(
                Utils.smoothStep(value1.X, value2.X, amount),
                Utils.smoothStep(value1.Y, value2.Y, amount),
                Utils.smoothStep(value1.Z, value2.Z, amount));
        }

        public static Vector3 transform(Vector3 position, Matrix4 matrix)
        {
            return new Vector3(
                (position.X * matrix.M11) + (position.Y * matrix.M21) + (position.Z * matrix.M31) + matrix.M41,
                (position.X * matrix.M12) + (position.Y * matrix.M22) + (position.Z * matrix.M32) + matrix.M42,
                (position.X * matrix.M13) + (position.Y * matrix.M23) + (position.Z * matrix.M33) + matrix.M43);
        }

        public static Vector3 transformNormal(Vector3 position, Matrix4 matrix)
        {
            return new Vector3(
                (position.X * matrix.M11) + (position.Y * matrix.M21) + (position.Z * matrix.M31),
                (position.X * matrix.M12) + (position.Y * matrix.M22) + (position.Z * matrix.M32),
                (position.X * matrix.M13) + (position.Y * matrix.M23) + (position.Z * matrix.M33));
        }

        //endregion Static Methods

        //region Overrides

        @Override
        public boolean equals(Object obj)
        {
            return (obj instanceof Vector3) ? equals((Vector3)obj) : false;
        }

        public boolean equals(Vector3 other)
        {
            return equals(this, other);
        }

        @Override
        public int hashCode()
        {
            return (new Float(X)).hashCode() ^ (new Float(Y)).hashCode() ^ (new Float(Z)).hashCode() ;
        }

        /// <summary>
        /// Get a formatted String representation of the vector
        /// </summary>
        /// <returns>A String representation of the vector</returns>
        public String toString()
        {
            return "<"+ X + ", " + Y + ", " + Z + ">";

        }

        /// <summary>
        /// Get a String representation of the vector elements with up to three
        /// decimal digits and separated by spaces only
        /// </summary>
        /// <returns>Raw String representation of the vector</returns>
        public String toRawString()
        {
            return ""+ X + ", " + Y + ", " + Z;
        }

        //endregion Overrides

        //region Operators

        public static boolean equals(Vector3 value1, Vector3 value2)
        {
            return value1.X == value2.X
                && value1.Y == value2.Y
                && value1.Z == value2.Z;
        }

        public static boolean notEquals(Vector3 value1, Vector3 value2)
        {
            return !(value1 == value2);
        }

        public Vector3 add(Vector3 value2)
        {
        	Vector3 result = this;
        	Vector3 value1 = this;
            result.X = value1.X + value2.X;
            result.Y = value1.Y + value2.Y;
            result.Z = value1.Z + value2.Z;
            return result;
        }
        
        public static Vector3 add(Vector3 value1, Vector3 value2)
        {
        	Vector3 result = new Vector3();
            result.X = value1.X + value2.X;
            result.Y = value1.Y + value2.Y;
            result.Z = value1.Z + value2.Z;
            return result;
        }

        public Vector3 subtract(Vector3 value2)
        {
        	Vector3 result = this;
        	Vector3 value1 = this;        	
        	result.X = value1.X - value2.X;
        	result.Y = value1.Y - value2.Y;
        	result.Z = value1.Z - value2.Z;
            return result;
        }
        
        public static Vector3 subtract(Vector3 value1, Vector3 value2)
        {
        	Vector3 result = new Vector3();
        	result.X = value1.X - value2.X;
        	result.Y = value1.Y - value2.Y;
        	result.Z = value1.Z - value2.Z;
            return result;
        }

        public static Vector3 multiply(Vector3 value1, Vector3 value2)
        {
        	Vector3 result = new Vector3();
        	result.X = value1.X * value2.X;
        	result.Y = value1.Y * value2.Y;
        	result.Z = value1.Z * value2.Z;
            return result;
        }

        public Vector3 multiply(float scaleFactor)
        {
        	Vector3 result = this;
        	Vector3 value = this;  
        	result.X = value.X * scaleFactor;
        	result.Y = value.Y * scaleFactor;
        	result.Z = value.Z * scaleFactor;
        	return result;
        }
        
        public static Vector3 multiply(Vector3 value, float scaleFactor)
        {
        	Vector3 result = new Vector3();
        	result.X = value.X * scaleFactor;
        	result.Y = value.Y * scaleFactor;
        	result.Z = value.Z * scaleFactor;
            return result;
        }

        public static Vector3 multiply(Vector3 vec, Quaternion rot)
        {
        	Vector3 result = new Vector3();
            float rw = -rot.X * vec.X - rot.Y * vec.Y - rot.Z * vec.Z;
            float rx = rot.W * vec.X + rot.Y * vec.Z - rot.Z * vec.Y;
            float ry = rot.W * vec.Y + rot.Z * vec.X - rot.X * vec.Z;
            float rz = rot.W * vec.Z + rot.X * vec.Y - rot.Y * vec.X;

            result.X = -rw * rot.X + rx * rot.W - ry * rot.Z + rz * rot.Y;
            result.Y = -rw * rot.Y + ry * rot.W - rz * rot.X + rx * rot.Z;
            result.Z = -rw * rot.Z + rz * rot.W - rx * rot.Y + ry * rot.X;

            return result;
        }

        public static Vector3 multiply(Vector3 vector, Matrix4 matrix)
        {
            return transform(vector, matrix);
        }

        public static Vector3 divide(Vector3 value1, Vector3 value2)
        {
        	Vector3 result = new Vector3();
            result.X = value1.X / value2.X;
            result.Y = value1.Y / value2.Y;
            result.Z = value1.Z / value2.Z;
            return result;
        }

        public static Vector3 divide(Vector3 value, float divider)
        {
        	Vector3 result = new Vector3();
            float factor = 1f / divider;
            result.X = value.X * factor;
            result.Y = value.Y * factor;
            result.Z = value.Z * factor;
            return result;
        }

        /// <summary>
        /// Cross product between two vectors
        /// </summary>
        public static Vector3 modulus(Vector3 value1, Vector3 value2)
        {
            return cross(value1, value2);
        }

        //endregion Operators

        /// <summary>A vector with a value of 0,0,0</summary>
        public final static Vector3 Zero = new Vector3();
        /// <summary>A vector with a value of 1,1,1</summary>
        public final static Vector3 One = new Vector3(1f, 1f, 1f);
        /// <summary>A unit vector facing forward (X axis), value 1,0,0</summary>
        public final static Vector3 UnitX = new Vector3(1f, 0f, 0f);
        /// <summary>A unit vector facing left (Y axis), value 0,1,0</summary>
        public final static Vector3 UnitY = new Vector3(0f, 1f, 0f);
        /// <summary>A unit vector facing up (Z axis), value 0,0,1</summary>
        public final static Vector3 UnitZ = new Vector3(0f, 0f, 1f);

    }
