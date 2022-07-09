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
    /// A three-dimensional vector with doubleing-point values
    /// </summary>
    public class Vector3d implements Comparable<Vector3d>, Serializable
    {
        /// <summary>X value</summary>
        public double X;
        /// <summary>Y value</summary>
        public double Y;
        /// <summary>Z value</summary>
        public double Z;

        //region Constructors

        public Vector3d()
        {
        	this(0f, 0f, 0f);
        }
        
        public Vector3d(double x, double y, double z)
        {
            X = x;
            Y = y;
            Z = z;
        }

        public Vector3d(double value)
        {
            X = value;
            Y = value;
            Z = value;
        }

        /// <summary>
        /// Constructor, builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing three eight-byte doubles</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public Vector3d(byte[] byteArray, int pos)
        {
            X = Y = Z = 0d;
            fromBytesLit(byteArray, pos);
        }

        public Vector3d(Vector3 vector)
        {
            X = vector.X;
            Y = vector.Y;
            Z = vector.Z;
        }

        public Vector3d(Vector3d vector)
        {
            X = vector.X;
            Y = vector.Y;
            Z = vector.Z;
        }

        //endregion Constructors

        //region Public Methods

        public double length()
        {
            return Math.sqrt(distanceSquared(this, Zero));
        }

        public double lengthSquared()
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
        public boolean approxEquals(Vector3d vec, double tolerance)
        {
            Vector3d diff = subtract(this, vec);
            return (diff.lengthSquared() <= tolerance * tolerance);
        }

        /// <summary>
        /// IComparable.CompareTo implementation
        /// </summary>
        public int compareTo(Vector3d vector)
        {
            return Double.compare(length(), vector.length());
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
        /// <param name="byteArray">Byte array containing a 24 byte vector</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytesLit(byte[] bytes, int pos)
        {
            X = Utils.bytesToDoubleLit(bytes, pos);
            Y = Utils.bytesToDoubleLit(bytes, pos+8);
            Z = Utils.bytesToDoubleLit(bytes, pos+16);
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>A 24 byte array containing X, Y, and Z</returns>
        public byte[] getBytesLit()
        {
            byte[] byteArray = new byte[24];
            toBytesLit(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 24 bytes before the end of the array</param>
        public void toBytesLit(byte[] dest, int pos)
        {   
            byte[] xbytes = Utils.doubleToBytesLit(X);
            byte[] ybytes = Utils.doubleToBytesLit(Y);
            byte[] zbytes = Utils.doubleToBytesLit(Z);
            System.arraycopy(xbytes, 0, dest, pos, 8);
            System.arraycopy(ybytes, 0, dest, pos+8, 8);
            System.arraycopy(zbytes, 0, dest, pos+16, 8);
            
        }

        
        /// <summary>
        /// Builds a vector from a byte array
        /// </summary>
        /// <param name="byteArray">Byte array containing a 24 byte vector</param>
        /// <param name="pos">Beginning position in the byte array</param>
        public void fromBytes(byte[] bytes, int pos)
        {
            X = Utils.bytesToDouble(bytes, pos);
            Y = Utils.bytesToDouble(bytes, pos+8);
            Z = Utils.bytesToDouble(bytes, pos+16);
        }

        /// <summary>
        /// Returns the raw bytes for this vector
        /// </summary>
        /// <returns>A 24 byte array containing X, Y, and Z</returns>
        public byte[] getBytes()
        {
            byte[] byteArray = new byte[24];
            toBytes(byteArray, 0);
            return byteArray;
        }

        /// <summary>
        /// Writes the raw bytes for this vector to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 24 bytes before the end of the array</param>
        public void toBytes(byte[] dest, int pos)
        {   
            byte[] xbytes = Utils.doubleToBytes(X);
            byte[] ybytes = Utils.doubleToBytes(Y);
            byte[] zbytes = Utils.doubleToBytes(Z);
            System.arraycopy(xbytes, 0, dest, pos, 8);
            System.arraycopy(ybytes, 0, dest, pos+8, 8);
            System.arraycopy(zbytes, 0, dest, pos+16, 8);
            
        }
        
        
        //endregion Public Methods

        //region Static Methods


        public static Vector3d clamp(Vector3d value1, Vector3d min, Vector3d max)
        {
            return new Vector3d(
                Utils.clamp(value1.X, min.X, max.X),
                Utils.clamp(value1.Y, min.Y, max.Y),
                Utils.clamp(value1.Z, min.Z, max.Z));
        }

        public static Vector3d cross(Vector3d value1, Vector3d value2)
        {
            return new Vector3d(
                value1.Y * value2.Z - value2.Y * value1.Z,
                value1.Z * value2.X - value2.Z * value1.X,
                value1.X * value2.Y - value2.X * value1.Y);
        }

        public static double distance(Vector3d value1, Vector3d value2)
        {
            return Math.sqrt(distanceSquared(value1, value2));
        }

        public static double distanceSquared(Vector3d value1, Vector3d value2)
        {
            return
                (value1.X - value2.X) * (value1.X - value2.X) +
                (value1.Y - value2.Y) * (value1.Y - value2.Y) +
                (value1.Z - value2.Z) * (value1.Z - value2.Z);
        }


        public static double dot(Vector3d value1, Vector3d value2)
        {
            return value1.X * value2.X + value1.Y * value2.Y + value1.Z * value2.Z;
        }

        public static Vector3d lerp(Vector3d value1, Vector3d value2, double amount)
        {
            return new Vector3d(
                Utils.lerp(value1.X, value2.X, amount),
                Utils.lerp(value1.Y, value2.Y, amount),
                Utils.lerp(value1.Z, value2.Z, amount));
        }

        public static Vector3d max(Vector3d value1, Vector3d value2)
        {
            return new Vector3d(
                Math.max(value1.X, value2.X),
                Math.max(value1.Y, value2.Y),
                Math.max(value1.Z, value2.Z));
        }

        public static Vector3d min(Vector3d value1, Vector3d value2)
        {
            return new Vector3d(
                Math.min(value1.X, value2.X),
                Math.min(value1.Y, value2.Y),
                Math.min(value1.Z, value2.Z));
        }


        public static Vector3d negate(Vector3d value)
        {
        	Vector3d result = new Vector3d();
        	result.X = -value.X;
        	result.Y = -value.Y;
        	result.Z = -value.Z;
            return result;
        }

        public static Vector3d normalize(Vector3d value)
        {
        	Vector3d result = new Vector3d();
            double factor = distance(value, Zero);
            if (factor > Double.MIN_EXPONENT)
            {
                factor = 1d / factor;
                result.X = value.X * factor;
                result.Y = value.Y * factor;
                result.Z = value.Z * factor;
            }
            else
            {
            	result.X = 0d;
            	result.Y = 0d;
            	result.Z = 0d;
            }
            return result;
        }

        /// <summary>
        /// Parse a vector from a String
        /// </summary>
        /// <param name="val">A String representation of a 3D vector, enclosed 
        /// in arrow brackets and separated by commas</param>
        public static Vector3d parse(String val)
        {
            String splitChar = ",";
            String[] split = val.replace("<", "").replace(">", "").split(splitChar);
            return new Vector3d(
                Double.parseDouble(split[0].trim()),
                Double.parseDouble(split[1].trim()),
                Double.parseDouble(split[2].trim()));
        }

        public static boolean tryParse(String val, Vector3d[] result)
        {
            try
            {
                result[0] = parse(val);
                return true;
            }
            catch (Exception e)
            {
                result[0] = Vector3d.Zero;
                return false;
            }
        }

        /// <summary>
        /// Interpolates between two vectors using a cubic equation
        /// </summary>
        public static Vector3d smoothStep(Vector3d value1, Vector3d value2, double amount)
        {
            return new Vector3d(
                Utils.smoothStep(value1.X, value2.X, amount),
                Utils.smoothStep(value1.Y, value2.Y, amount),
                Utils.smoothStep(value1.Z, value2.Z, amount));
        }

        public static Vector3d subtract(Vector3d value1, Vector3d value2)
        {
        	Vector3d result = new Vector3d();
        	result.X = value1.X - value2.X;
        	result.Y = value1.Y - value2.Y;
        	result.Z = value1.Z - value2.Z;
            return result;
        }

        //endregion Static Methods

        //region Overrides

        @Override
        public boolean equals(Object obj)
        {
            return (obj instanceof Vector3d) ? equals((Vector3d)obj) : false;
        }

        public boolean equals(Vector3d other)
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

        public static boolean equals(Vector3d value1, Vector3d value2)
        {
            return value1.X == value2.X
                && value1.Y == value2.Y
                && value1.Z == value2.Z;
        }

        public static boolean notEquals(Vector3d value1, Vector3d value2)
        {
            return !(value1 == value2);
        }

        public static Vector3d add(Vector3d value1, Vector3d value2)
        {
        	Vector3d result = new Vector3d();
        	result.X = value1.X + value2.X;
        	result.Y = value1.Y + value2.Y;
        	result.Z = value1.Z + value2.Z;
            return result;
        }

        public static Vector3d subtract(Vector3d value)
        {
        	Vector3d result = new Vector3d();
        	result.X = -value.X;
        	result.Y = -value.Y;
        	result.Z = -value.Z;
            return result;
        }

        public static Vector3d multiply(Vector3d value1, Vector3d value2)
        {
        	Vector3d result = new Vector3d();
        	result.X = value1.X * value2.X;
        	result.Y = value1.Y * value2.Y;
        	result.Z = value1.Z * value2.Z;
            return result;
        }

        public static Vector3d multiply(Vector3d value, double scaleFactor)
        {
        	Vector3d result = new Vector3d();
        	result.X = value.X * scaleFactor;
        	result.Y = value.Y * scaleFactor;
        	result.Z = value.Z * scaleFactor;
            return result;
        }

        public static Vector3d divide(Vector3d value1, Vector3d value2)
        {
        	Vector3d result = new Vector3d();
        	result.X = value1.X / value2.X;
        	result.Y = value1.Y / value2.Y;
        	result.Z = value1.Z / value2.Z;
            return result;
        }

        public static Vector3d divide(Vector3d value, double divider)
        {
        	Vector3d result = new Vector3d();
            double factor = 1d / divider;
            result.X = value.X * factor;
            result.Y = value.Y * factor;
            result.Z = value.Z * factor;
            return result;
        }

        /// <summary>
        /// Cross product between two vectors
        /// </summary>
        public static Vector3d modulus(Vector3d value1, Vector3d value2)
        {
            return cross(value1, value2);
        }

        //endregion Operators

        /// <summary>A vector with a value of 0,0,0</summary>
        public final static Vector3d Zero = new Vector3d();
        /// <summary>A vector with a value of 1,1,1</summary>
        public final static Vector3d One = new Vector3d();
        /// <summary>A unit vector facing forward (X axis), value of 1,0,0</summary>
        public final static Vector3d UnitX = new Vector3d(1d, 0d, 0d);
        /// <summary>A unit vector facing left (Y axis), value of 0,1,0</summary>
        public final static Vector3d UnitY = new Vector3d(0d, 1d, 0d);
        /// <summary>A unit vector facing up (Z axis), value of 0,0,1</summary>
        public final static Vector3d UnitZ = new Vector3d(0d, 0d, 1d);
    }
