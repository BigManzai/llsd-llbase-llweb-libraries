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


    public class Vector2 implements Comparable<Vector2>, Serializable
    {
        /// <summary>X value</summary>
        public float X;
        /// <summary>Y value</summary>
        public float Y;

        //region Constructors

        public Vector2(float x, float y)
        {
            X = x;
            Y = y;
        }

        public Vector2(float value)
        {
            X = value;
            Y = value;
        }

        public Vector2(Vector2 vector)
        {
            X = vector.X;
            Y = vector.Y;
        }

        //endregion Constructors

        //region Public Methods

        public Vector2() {
			this(0f, 0f);
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
        public boolean approxEquals(Vector2 vec, float tolerance)
        {
            Vector2 diff = subtract(this, vec);
            return (diff.lengthSquared() <= tolerance * tolerance);
        }

        /// <summary>
        /// Test if this vector is composed of all finite numbers
        /// </summary>
        public boolean isFinite()
        {
            return Utils.isFinite(X) && Utils.isFinite(Y);
        }

        /// <summary>
        /// IComparable.compareTo implementation
        /// </summary>
        public int compareTo(Vector2 vector)
        {
            return Float.compare(length(), vector.length());
        }

        /// <summary>
        /// Builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing two four-byte floats</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytesLit(byte[] bytes, int pos)
        {
//            if (!BitConverter.IsLittleEndian)
//            {
//                // Big endian architecture
//                byte[] conversionBuffer = new byte[8];
//
//                Buffer.BlockCopy(byteArray, pos, conversionBuffer, 0, 8);
//
//                Array.Reverse(conversionBuffer, 0, 4);
//                Array.Reverse(conversionBuffer, 4, 4);
//
//                X = BitConverter.ToSingle(conversionBuffer, 0);
//                Y = BitConverter.ToSingle(conversionBuffer, 4);
//            }
//            else
//            {
//                // Little endian architecture
//                X = BitConverter.ToSingle(byteArray, pos);
//                Y = BitConverter.ToSingle(byteArray, pos + 4);
//            }
            
            X = Utils.bytesToFloatLit(bytes, pos);
            Y = Utils.bytesToFloatLit(bytes, pos+4);
            
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>An eight-byte array containing X and Y</returns>
        public byte[] getBytesLit()
        {
            byte[] byteArray = new byte[8];
            toBytesLit(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 8 bytes before the end of the array</param>
        public void toBytesLit(byte[] dest, int pos)
        {
//            Buffer.BlockCopy(BitConverter.GetBytes(X), 0, dest, pos + 0, 4);
//            Buffer.BlockCopy(BitConverter.GetBytes(Y), 0, dest, pos + 4, 4);
//
//            if (!BitConverter.IsLittleEndian)
//            {
//                Array.Reverse(dest, pos + 0, 4);
//                Array.Reverse(dest, pos + 4, 4);
//            }
            byte[] xbytes = Utils.floatToBytesLit(X);
            byte[] ybytes = Utils.floatToBytesLit(Y);
            System.arraycopy(xbytes, 0, dest, pos, 4);
            System.arraycopy(ybytes, 0, dest, pos+4, 4);
            
        }

        /// <summary>
        /// Builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing two four-byte floats</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytes(byte[] bytes, int pos)
        {   
            X = Utils.bytesToFloat(bytes, pos);
            Y = Utils.bytesToFloat(bytes, pos+4);
            
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>An eight-byte array containing X and Y</returns>
        public byte[] getBytes()
        {
            byte[] byteArray = new byte[8];
            toBytes(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 8 bytes before the end of the array</param>
        public void toBytes(byte[] dest, int pos)
        {
            byte[] xbytes = Utils.floatToBytes(X);
            byte[] ybytes = Utils.floatToBytes(Y);
            System.arraycopy(xbytes, 0, dest, pos, 4);
            System.arraycopy(ybytes, 0, dest, pos+4, 4);
            
        }
        
        
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

        //endregion Public Methods

        //region Static Methods

        public static Vector2 clamp(Vector2 value1, Vector2 min, Vector2 max)
        {
            return new Vector2(
                Utils.clamp(value1.X, min.X, max.X),
                Utils.clamp(value1.Y, min.Y, max.Y));
        }

        public static float distance(Vector2 value1, Vector2 value2)
        {
            return (float)Math.sqrt(distanceSquared(value1, value2));
        }

        public static float distanceSquared(Vector2 value1, Vector2 value2)
        {
            return
                (value1.X - value2.X) * (value1.X - value2.X) +
                (value1.Y - value2.Y) * (value1.Y - value2.Y);
        }

        public static float dot(Vector2 value1, Vector2 value2)
        {
            return value1.X * value2.X + value1.Y * value2.Y;
        }

        public static Vector2 lerp(Vector2 value1, Vector2 value2, float amount)
        {
            return new Vector2(
                Utils.lerp(value1.X, value2.X, amount),
                Utils.lerp(value1.Y, value2.Y, amount));
        }

        public static Vector2 max(Vector2 value1, Vector2 value2)
        {
            return new Vector2(
                Math.max(value1.X, value2.X),
                Math.max(value1.Y, value2.Y));
        }

        public static Vector2 min(Vector2 value1, Vector2 value2)
        {
            return new Vector2(
                Math.min(value1.X, value2.X),
                Math.min(value1.Y, value2.Y));
        }

        public static Vector2 negate(Vector2 value)
        {
        	return new Vector2(-value.X, -value.Y);
//            value.X = -value.X;
//            value.Y = -value.Y;
//            return value;
        }

        public static Vector2 normalize(Vector2 value)
        {
        	Vector2 result = new Vector2(); 
            final float MAG_THRESHOLD = 0.0000001f;
            float factor = distanceSquared(value, Zero);
            if (factor > MAG_THRESHOLD)
            {
                factor = 1f / (float)Math.sqrt(factor);
                result.X = value.X*factor;
                result.Y = value.Y*factor;
            }
            else
            {
            	result.X = 0f;
            	result.Y = 0f;
            }
            return result;
        }

        /// <summary>
        /// Parse a vector from a String
        /// </summary>
        /// <param name="val">A String representation of a 2D vector, enclosed 
        /// in arrow brackets and separated by commas</param>
        public static Vector2 parse(String val)
        {
            String splitChar = ",";
            String[] split = val.replace("<", "").replace(">", "").split(splitChar);
            return new Vector2(
                Float.parseFloat(split[0].trim()),
                Float.parseFloat(split[1].trim()));
        }

        public static boolean tryParse(String val, Vector2[] result)
        {
            try
            {
                result[0] = parse(val);
                return true;
            }
            catch (Exception e)
            {
                result[0] = Vector2.Zero;
                return false;
            }
        }

        /// <summary>
        /// Interpolates between two vectors using a cubic equation
        /// </summary>
        public static Vector2 smoothStep(Vector2 value1, Vector2 value2, float amount)
        {
            return new Vector2(
                Utils.smoothStep(value1.X, value2.X, amount),
                Utils.smoothStep(value1.Y, value2.Y, amount));
        }


        public static Vector2 transform(Vector2 position, Matrix4 matrix)
        {
        	Vector2 result = new Vector2();
        	result.X = (position.X * matrix.M11) + (position.Y * matrix.M21) + matrix.M41;
        	result.Y = (position.X * matrix.M12) + (position.Y * matrix.M22) + matrix.M42;
            return result;
        }

        public static Vector2 transformNormal(Vector2 position, Matrix4 matrix)
        {
        	Vector2 result = new Vector2();
        	result.X = (position.X * matrix.M11) + (position.Y * matrix.M21);
        	result.Y = (position.X * matrix.M12) + (position.Y * matrix.M22);
            return result;
        }

        //endregion Static Methods

        //region Overrides

        @Override
        public boolean equals(Object obj)
        {
            return (obj instanceof Vector2) ? equals((Vector2)obj) : false;
        }

        public boolean equals(Vector2 other)
        {
            return equals(this, other);
        }

        @Override
        public int hashCode()
        {
            return (new Float(X)).hashCode() ^ (new Float(Y)).hashCode() ;
        }

        /// <summary>
        /// Get a formatted String representation of the vector
        /// </summary>
        /// <returns>A String representation of the vector</returns>
        public String toString()
        {
            return "<"+ X + ", " + Y + ">";
        }

        /// <summary>
        /// Get a String representation of the vector elements with up to three
        /// decimal digits and separated by spaces only
        /// </summary>
        /// <returns>Raw String representation of the vector</returns>
        public String toRawString()
        {
            return ""+ X + ", " + Y + "";
        }

        //endregion Overrides

        //region Operators

        public static boolean equals(Vector2 value1, Vector2 value2)
        {
            return value1.X == value2.X && value1.Y == value2.Y;
        }

        public static boolean notEquals(Vector2 value1, Vector2 value2)
        {
            return value1.X != value2.X || value1.Y != value2.Y;
        }

        public static Vector2 add(Vector2 value1, Vector2 value2)
        {
        	Vector2 result = new Vector2();
        	result.X = value1.X + value2.X;
        	result.Y = value1.Y + value2.Y;
            return result;
        }

//        public static Vector2 subtract(Vector2 value)
//        {
//            value.X = -value.X;
//            value.Y = -value.Y;
//            return value;
//        }

        public static Vector2 subtract(Vector2 value1, Vector2 value2)
        {
        	Vector2 result = new Vector2();
        	result.X = value1.X - value2.X;
        	result.Y = value1.Y - value2.Y;
            return result;
        }

        public static Vector2 multiply(Vector2 value1, Vector2 value2)
        {
        	Vector2 result = new Vector2();
        	result.X = value1.X * value2.X;
        	result.Y = value1.Y * value2.Y;
            return result;
        }

        public static Vector2 multiply(Vector2 value, float scaleFactor)
        {
        	Vector2 result = new Vector2();
        	result.X = value.X * scaleFactor;
        	result.Y = value.Y * scaleFactor;
            return result;
        }

        public static Vector2 divide(Vector2 value1, Vector2 value2)
        {
        	Vector2 result = new Vector2();
        	result.X = value1.X / value2.X;
        	result.Y = value1.Y /value2.Y;
            return result;
        }

        public static Vector2 divide(Vector2 value1, float divider)
        {
        	Vector2 result = new Vector2();        	
            float factor = 1 / divider;
            result.X = value1.X * factor;
            result.Y = value1.Y * factor;
            return result;
        }

        //endregion Operators

        /// <summary>A vector with a value of 0,0</summary>
        public final static Vector2 Zero = new Vector2();
        /// <summary>A vector with a value of 1,1</summary>
        public final static Vector2 One = new Vector2(1f, 1f);
        /// <summary>A vector with a value of 1,0</summary>
        public final static Vector2 UnitX = new Vector2(1f, 0f);
        /// <summary>A vector with a value of 0,1</summary>
        public final static Vector2 UnitY = new Vector2(0f, 1f);
    }
