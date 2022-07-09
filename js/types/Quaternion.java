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


    public class Quaternion implements Serializable
    {
        /**
		 * 
		 */
		private static final long serialVersionUID = -4078461694259895103L;
		/// <summary>X value</summary>
        public float X;
        /// <summary>Y value</summary>
        public float Y;
        /// <summary>Z value</summary>
        public float Z;
        /// <summary>W value</summary>
        public float W;

        //region Constructors

        public Quaternion()
        {
        	this(0f, 0f, 0f, 0f);
        }
        
        public Quaternion(float x, float y, float z, float w)
        {
            X = x;
            Y = y;
            Z = z;
            W = w;
        }

        public Quaternion(Vector3 vectorPart, float scalarPart)
        {
            X = vectorPart.X;
            Y = vectorPart.Y;
            Z = vectorPart.Z;
            W = scalarPart;
        }

        /// <summary>
        /// Build a quaternion from normalized float values
        /// </summary>
        /// <param name="x">X value from -1.0 to 1.0</param>
        /// <param name="y">Y value from -1.0 to 1.0</param>
        /// <param name="z">Z value from -1.0 to 1.0</param>
        public Quaternion(float x, float y, float z)
        {
            X = x;
            Y = y;
            Z = z;

            float xyzsum = 1 - X * X - Y * Y - Z * Z;
            W = (xyzsum > 0) ? (float)Math.sqrt(xyzsum) : 0;
        }

        /// <summary>
        /// Constructor, builds a quaternion Object from a byte array (Little Endian)
        /// </summary>
        /// <param name="byteArray">Byte array containing four four-byte floats</param>
        /// <param name="pos">Offset in the byte array to start reading at</param>
        /// <param name="normalized">Whether the source data is normalized or
        /// not. If this is true 12 bytes will be read, otherwise 16 bytes will
        /// be read.</param>
        public Quaternion(byte[] byteArray, int pos, boolean normalized)
        {
            X = Y = Z = W = 0;
            fromBytesLit(byteArray, pos, normalized);
        }

        public Quaternion(Quaternion q)
        {
            X = q.X;
            Y = q.Y;
            Z = q.Z;
            W = q.W;
        }

        //endregion Constructors

        //region Public Methods

        public boolean approxEquals(Quaternion quat, float tolerance)
        {
            Quaternion diff = subtract(this, quat);
            return (diff.lengthSquared() <= tolerance * tolerance);
        }

        public float length()
        {
            return (float)Math.sqrt(X * X + Y * Y + Z * Z + W * W);
        }

        public float lengthSquared()
        {
            return (X * X + Y * Y + Z * Z + W * W);
        }

        /// <summary>
        /// Normalizes the quaternion
        /// </summary>
        public void normalize()
        {
            normalize(this);
        }

        /// <summary>
        /// Builds a quaternion Object from a byte array
        /// </summary>
        /// <param name="byteArray">The source byte array</param>
        /// <param name="pos">Offset in the byte array to start reading at</param>
        /// <param name="normalized">Whether the source data is normalized or
        /// not. If this is true 12 bytes will be read, otherwise 16 bytes will
        /// be read.</param>
        public void fromBytesLit(byte[] bytes, int pos, boolean normalized)
        {
            X = Utils.bytesToFloatLit(bytes, pos);
            Y = Utils.bytesToFloatLit(bytes, pos+4);
            Z = Utils.bytesToFloatLit(bytes, pos+8);
        	
            if (!normalized)
            {
                    W = Utils.bytesToFloatLit(bytes, pos+12);
            }
            else
            {
                float xyzsum = 1f - X * X - Y * Y - Z * Z;
                W = (xyzsum > 0f) ? (float)Math.sqrt(xyzsum) : 0f;
            }
        }

        /// <summary>
        /// Normalize this quaternion and serialize it to a byte array
        /// </summary>
        /// <returns>A 12 byte array containing normalized X, Y, and Z floating
        /// point values in order using little endian byte ordering</returns>
        public byte[] getBytesLit()
        {
            byte[] bytes = new byte[12];
            toBytesLit(bytes, 0);
            return bytes;
        }

        /// <summary>
        /// Writes the raw bytes for this quaternion to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 12 bytes before the end of the array</param>
        public void toBytesLit(byte[] dest, int pos)
        {
            float norm = (float)Math.sqrt(X * X + Y * Y + Z * Z + W * W);

            if (norm != 0f)
            {
                norm = 1f / norm;

                float x, y, z;
                if (W >= 0f)
                {
                    x = X; y = Y; z = Z;
                }
                else
                {
                    x = -X; y = -Y; z = -Z;
                }
//                Buffer.BlockCopy(BitConverter.GetBytes(norm * x), 0, dest, pos + 0, 4);
//                Buffer.BlockCopy(BitConverter.GetBytes(norm * y), 0, dest, pos + 4, 4);
//                Buffer.BlockCopy(BitConverter.GetBytes(norm * z), 0, dest, pos + 8, 4);
//
//                if (!BitConverter.IsLittleEndian)
//                {
//                    Array.Reverse(dest, pos + 0, 4);
//                    Array.Reverse(dest, pos + 4, 4);
//                    Array.Reverse(dest, pos + 8, 4);
//                }
                byte[] xbytes = Utils.floatToBytesLit(x*norm);
                byte[] ybytes = Utils.floatToBytesLit(y*norm);
                byte[] zbytes = Utils.floatToBytesLit(z*norm);
                System.arraycopy(xbytes, 0, dest, pos, 4);
                System.arraycopy(ybytes, 0, dest, pos+4, 4);
                System.arraycopy(zbytes, 0, dest, pos+8, 4);
            }
            else
            {
                throw new IllegalStateException("Quaternion {0} normalized to zero");
            }
        }

        /// <summary>
        /// Builds a quaternion Object from a byte array
        /// </summary>
        /// <param name="byteArray">The source byte array</param>
        /// <param name="pos">Offset in the byte array to start reading at</param>
        /// <param name="normalized">Whether the source data is normalized or
        /// not. If this is true 12 bytes will be read, otherwise 16 bytes will
        /// be read.</param>
        public void fromBytes(byte[] bytes, int pos, boolean normalized)
        {
            X = Utils.bytesToFloat(bytes, pos);
            Y = Utils.bytesToFloat(bytes, pos+4);
            Z = Utils.bytesToFloat(bytes, pos+8);
        	
            if (!normalized)
            {
                    W = Utils.bytesToFloat(bytes, pos+12);
            }
            else
            {
                float xyzsum = 1f - X * X - Y * Y - Z * Z;
                W = (xyzsum > 0f) ? (float)Math.sqrt(xyzsum) : 0f;
            }
        }
        
        /// <summary>
        /// Normalize this quaternion and serialize it to a byte array
        /// </summary>
        /// <returns>A 12 byte array containing normalized X, Y, and Z floating
        /// point values in order using little endian byte ordering</returns>
        public byte[] getBytes()
        {
            byte[] bytes = new byte[12];
            toBytes(bytes, 0);
            return bytes;
        }

        /// <summary>
        /// Writes the raw bytes for this quaternion to a byte array
        /// </summary>
        /// <param name="dest">Destination byte array</param>
        /// <param name="pos">Position in the destination array to start
        /// writing. Must be at least 12 bytes before the end of the array</param>
        public void toBytes(byte[] dest, int pos)
        {
            float norm = (float)Math.sqrt(X * X + Y * Y + Z * Z + W * W);

            if (norm != 0f)
            {
                norm = 1f / norm;

                float x, y, z;
                if (W >= 0f)
                {
                    x = X; y = Y; z = Z;
                }
                else
                {
                    x = -X; y = -Y; z = -Z;
                }
                byte[] xbytes = Utils.floatToBytes(x*norm);
                byte[] ybytes = Utils.floatToBytes(y*norm);
                byte[] zbytes = Utils.floatToBytes(z*norm);
                System.arraycopy(xbytes, 0, dest, pos, 4);
                System.arraycopy(ybytes, 0, dest, pos+4, 4);
                System.arraycopy(zbytes, 0, dest, pos+8, 4);
            }
            else
            {
                throw new IllegalStateException("Quaternion {0} normalized to zero");
            }
        }
        
        
        /// <summary>
        /// Convert this quaternion to euler angles
        /// </summary>
        /// <param name="roll">X euler angle</param>
        /// <param name="pitch">Y euler angle</param>
        /// <param name="yaw">Z euler angle</param>
        public float[] getEulerAngles()
        {
            float roll = 0f;
            float pitch = 0f;
            float yaw = 0f;

            Quaternion t = new Quaternion(this.X * this.X, this.Y * this.Y, this.Z * this.Z, this.W * this.W);

            float m = (t.X + t.Y + t.Z + t.W);
            if (Math.abs(m) < 0.001d) return  new float[]{roll, pitch, yaw};
            float n = 2 * (this.Y * this.W + this.X * this.Z);
            float p = m * m - n * n;

            if (p > 0f)
            {
                roll = (float)Math.atan2(2.0f * (this.X * this.W - this.Y * this.Z), (-t.X - t.Y + t.Z + t.W));
                pitch = (float)Math.atan2(n, Math.sqrt(p));
                yaw = (float)Math.atan2(2.0f * (this.Z * this.W - this.X * this.Y), t.X - t.Y - t.Z + t.W);
            }
            else if (n > 0f)
            {
                roll = 0f;
                pitch = (float)(Math.PI / 2d);
                yaw = (float)Math.atan2((this.Z * this.W + this.X * this.Y), 0.5f - t.X - t.Y);
            }
            else
            {
                roll = 0f;
                pitch = -(float)(Math.PI / 2d);
                yaw = (float)Math.atan2((this.Z * this.W + this.X * this.Y), 0.5f - t.X - t.Z);
            }

            return new float[]{roll, pitch, yaw};
            
            //float sqx = X * X;
            //float sqy = Y * Y;
            //float sqz = Z * Z;
            //float sqw = W * W;

            //// Unit will be a correction factor if the quaternion is not normalized
            //float unit = sqx + sqy + sqz + sqw;
            //double test = X * Y + Z * W;

            //if (test > 0.499f * unit)
            //{
            //    // Singularity at north pole
            //    yaw = 2f * (float)Math.atan2(X, W);
            //    pitch = (float)Math.PI / 2f;
            //    roll = 0f;
            //}
            //else if (test < -0.499f * unit)
            //{
            //    // Singularity at south pole
            //    yaw = -2f * (float)Math.atan2(X, W);
            //    pitch = -(float)Math.PI / 2f;
            //    roll = 0f;
            //}
            //else
            //{
            //    yaw = (float)Math.atan2(2f * Y * W - 2f * X * Z, sqx - sqy - sqz + sqw);
            //    pitch = (float)Math.asin(2f * test / unit);
            //    roll = (float)Math.atan2(2f * X * W - 2f * Y * Z, -sqx + sqy - sqz + sqw);
            //}
        }

        public static class AxisAngle
        {
        	protected Vector3 axis;
        	protected float angle;
        	
        	public AxisAngle(Vector3 axis, float angle)
        	{
        		this.axis = axis;
        		this.angle = angle;
        	}

			public Vector3 getAxis() {
				return axis;
			}

			public void setAxis(Vector3 axis) {
				this.axis = axis;
			}

			public float getAngle() {
				return angle;
			}

			public void setAngle(float angle) {
				this.angle = angle;
			}
        }
        
        /// <summary>
        /// Convert this quaternion to an angle around an axis
        /// </summary>
        /// <param name="axis">Unit vector describing the axis</param>
        /// <param name="angle">Angle around the axis, in radians</param>
        public AxisAngle getAxisAngle()
        {
        	Vector3 axis;
        	float angle;
            axis = new Vector3();
            float scale = (float)Math.sqrt(X * X + Y * Y + Z * Z);

            //TODO is Float.EPSLON same as MIN_NORMAL
            if (scale < Float.MIN_NORMAL || W > 1.0f || W < -1.0f)
            {
                angle = 0.0f;
                axis.X = 0.0f;
                axis.Y = 1.0f;
                axis.Z = 0.0f;
            }
            else
            {
                angle = 2.0f * (float)Math.acos(W);
                float ooscale = 1f / scale;
                axis.X = X * ooscale;
                axis.Y = Y * ooscale;
                axis.Z = Z * ooscale;
            }
            
            return new AxisAngle(axis, angle);
        }

        //endregion Public Methods

        //region Static Methods

        /// <summary>
        /// Returns the conjugate (spatial inverse) of a quaternion
        /// </summary>
        public static Quaternion conjugate(Quaternion quaternion)
        {
            Quaternion result = new Quaternion();
            result.X = -quaternion.X;
            result.Y = -quaternion.Y;
            result.Z = -quaternion.Z;
            return result;
        }

        /// <summary>
        /// Build a quaternion from an axis and an angle of rotation around
        /// that axis
        /// </summary>
        public static Quaternion createFromAxisAngle(float axisX, float axisY, float axisZ, float angle)
        {
            Vector3 axis = new Vector3(axisX, axisY, axisZ);
            return createFromAxisAngle(axis, angle);
        }

        /// <summary>
        /// Build a quaternion from an axis and an angle of rotation around
        /// that axis
        /// </summary>
        /// <param name="axis">Axis of rotation</param>
        /// <param name="angle">Angle of rotation</param>
        public static Quaternion createFromAxisAngle(Vector3 axis, float angle)
        {
            Quaternion q = new Quaternion();
            axis = Vector3.normalize(axis);

            angle *= 0.5f;
            float c = (float)Math.cos(angle);
            float s = (float)Math.sin(angle);

            q.X = axis.X * s;
            q.Y = axis.Y * s;
            q.Z = axis.Z * s;
            q.W = c;

            return Quaternion.normalize(q);
        }

        /// <summary>
        /// Creates a quaternion from a vector containing roll, pitch, and yaw
        /// in radians
        /// </summary>
        /// <param name="eulers">Vector representation of the euler angles in
        /// radians</param>
        /// <returns>Quaternion representation of the euler angles</returns>
        public static Quaternion createFromEulers(Vector3 eulers)
        {
            return createFromEulers(eulers.X, eulers.Y, eulers.Z);
        }

        /// <summary>
        /// Creates a quaternion from roll, pitch, and yaw euler angles in
        /// radians
        /// </summary>
        /// <param name="roll">X angle in radians</param>
        /// <param name="pitch">Y angle in radians</param>
        /// <param name="yaw">Z angle in radians</param>
        /// <returns>Quaternion representation of the euler angles</returns>
        public static Quaternion createFromEulers(float roll, float pitch, float yaw)
        {
            if (roll > Utils.TWO_PI || pitch > Utils.TWO_PI || yaw > Utils.TWO_PI)
                throw new IllegalArgumentException("Euler angles must be in radians");

            double atCos = Math.cos(roll / 2f);
            double atSin = Math.sin(roll / 2f);
            double leftCos = Math.cos(pitch / 2f);
            double leftSin = Math.sin(pitch / 2f);
            double upCos = Math.cos(yaw / 2f);
            double upSin = Math.sin(yaw / 2f);
            double atLeftCos = atCos * leftCos;
            double atLeftSin = atSin * leftSin;
            return new Quaternion(
                (float)(atSin * leftCos * upCos + atCos * leftSin * upSin),
                (float)(atCos * leftSin * upCos - atSin * leftCos * upSin),
                (float)(atLeftCos * upSin + atLeftSin * upCos),
                (float)(atLeftCos * upCos - atLeftSin * upSin)
            );
        }

        public static Quaternion createFromRotationMatrix(Matrix4 m)
        {
            Quaternion quat = new Quaternion();

            float trace = m.trace();

            if (trace > Float.MIN_EXPONENT)
            {
                float s = (float)Math.sqrt(trace + 1f);
                quat.W = s * 0.5f;
                s = 0.5f / s;
                quat.X = (m.M23 - m.M32) * s;
                quat.Y = (m.M31 - m.M13) * s;
                quat.Z = (m.M12 - m.M21) * s;
            }
            else
            {
                if (m.M11 > m.M22 && m.M11 > m.M33)
                {
                    float s = (float)Math.sqrt(1f + m.M11 - m.M22 - m.M33);
                    quat.X = 0.5f * s;
                    s = 0.5f / s;
                    quat.Y = (m.M12 + m.M21) * s;
                    quat.Z = (m.M13 + m.M31) * s;
                    quat.W = (m.M23 - m.M32) * s;
                }
                else if (m.M22 > m.M33)
                {
                    float s = (float)Math.sqrt(1f + m.M22 - m.M11 - m.M33);
                    quat.Y = 0.5f * s;
                    s = 0.5f / s;
                    quat.X = (m.M21 + m.M12) * s;
                    quat.Z = (m.M32 + m.M23) * s;
                    quat.W = (m.M31 - m.M13) * s;
                }
                else
                {
                    float s = (float)Math.sqrt(1f + m.M33 - m.M11 - m.M22);
                    quat.Z = 0.5f * s;
                    s = 0.5f / s;
                    quat.X = (m.M31 + m.M13) * s;
                    quat.Y = (m.M32 + m.M23) * s;
                    quat.W = (m.M12 - m.M21) * s;
                }
            }

            return quat;
        }

        public static Quaternion divide(Quaternion quaternion1, Quaternion quaternion2)
        {
            float x = quaternion1.X;
            float y = quaternion1.Y;
            float z = quaternion1.Z;
            float w = quaternion1.W;

            float q2lensq = quaternion2.lengthSquared();
            float ooq2lensq = 1f / q2lensq;
            float x2 = -quaternion2.X * ooq2lensq;
            float y2 = -quaternion2.Y * ooq2lensq;
            float z2 = -quaternion2.Z * ooq2lensq;
            float w2 = quaternion2.W * ooq2lensq;

            return new Quaternion(
                ((x * w2) + (x2 * w)) + (y * z2) - (z * y2),
                ((y * w2) + (y2 * w)) + (z * x2) - (x * z2),
                ((z * w2) + (z2 * w)) + (x * y2) - (y * x2),
                (w * w2) - ((x * x2) + (y * y2)) + (z * z2));
        }

        public static float dot(Quaternion q1, Quaternion q2)
        {
            return (q1.X * q2.X) + (q1.Y * q2.Y) + (q1.Z * q2.Z) + (q1.W * q2.W);
        }

        /// <summary>
        /// Conjugates and renormalizes a vector
        /// </summary>
        public static Quaternion inverse(Quaternion quaternion)
        {
        	Quaternion result  = new Quaternion();
            float norm = quaternion.lengthSquared();

            if (norm == 0f)
            {
            	result.X = result.Y = result.Z = result.W = 0f;
            }
            else
            {
                float oonorm = 1f / norm;
                result = conjugate(quaternion);
                
                result.X = quaternion.X * oonorm;
                result.Y = quaternion.Y * oonorm;
                result.Z = quaternion.Z * oonorm;
                result.W = quaternion.W * oonorm;
            }

            return result;
        }

        /// <summary>
        /// Spherical linear interpolation between two quaternions
        /// </summary>
        public static Quaternion slerp(Quaternion q1, Quaternion q2, float amount)
        {
        	Quaternion inter = new Quaternion(q2.X, q2.Y, q2.Z, q2.W);
            float angle = dot(q1, q2);

            if (angle < 0f)
            {
                multiply(q1, -1f);
                angle *= -1f;
            }

            float scale;
            float invscale;

            
            
            if ((angle + 1f) > 0.05f)
            {
                if ((1f - angle) >= 0.05f)
                {
                    // slerp
                    float theta = (float)Math.acos(angle);
                    float invsintheta = 1f / (float)Math.sin(theta);
                    scale = (float)Math.sin(theta * (1f - amount)) * invsintheta;
                    invscale = (float)Math.sin(theta * amount) * invsintheta;
                }
                else
                {
                    // lerp
                    scale = 1f - amount;
                    invscale = amount;
                }
            }
            else
            {
            	inter.X = -q1.Y;
            	inter.Y = q1.X;
            	inter.Z = -q1.W;
            	inter.W = q1.Z;

                scale = (float)Math.sin(Utils.PI * (0.5f - amount));
                invscale = (float)Math.sin(Utils.PI * amount);
            }

            return add(multiply(q1,  scale),  multiply(inter, invscale));
        }

        public static Quaternion add(Quaternion quaternion1, Quaternion quaternion2)
        {
        	Quaternion result  = new Quaternion();
        	result.X = quaternion1.X + quaternion2.X;
        	result.Y = quaternion1.Y + quaternion2.Y;
        	result.Z = quaternion1.Z + quaternion2.Z;
        	result.W = quaternion1.W + quaternion2.W;
            return result;
        }
        
        
        public static Quaternion subtract(Quaternion quaternion1, Quaternion quaternion2)
        {
        	Quaternion result  = new Quaternion();
        	result.X = quaternion1.X - quaternion2.X;
        	result.Y = quaternion1.Y - quaternion2.Y;
        	result.Z = quaternion1.Z - quaternion2.Z;
        	result.W = quaternion1.W - quaternion2.W;
            return result;
        }

        public static Quaternion multiply(Quaternion a, Quaternion b)
        {
            return new Quaternion(
                a.W * b.X + a.X * b.W + a.Y * b.Z - a.Z * b.Y,
                a.W * b.Y + a.Y * b.W + a.Z * b.X - a.X * b.Z,
                a.W * b.Z + a.Z * b.W + a.X * b.Y - a.Y * b.X,
                a.W * b.W - a.X * b.X - a.Y * b.Y - a.Z * b.Z
            );
        }

        public static Quaternion multiply(Quaternion quaternion, float scaleFactor)
        {
        	Quaternion result  = new Quaternion();
        	result.X = quaternion.X * scaleFactor;
        	result.Y = quaternion.Y * scaleFactor;
        	result.Z = quaternion.Z * scaleFactor;
        	result.W = quaternion.W * scaleFactor;
            return result;
        }

        public static Quaternion negate(Quaternion quaternion)
        {
        	Quaternion result  = new Quaternion();
        	result.X = -quaternion.X;
        	result.Y = -quaternion.Y;
        	result.Z = -quaternion.Z;
        	result.W = -quaternion.W;
            return result;
        }

        public static Quaternion normalize(Quaternion q)
        {
        	Quaternion result  = new Quaternion();
            final float MAG_THRESHOLD = 0.0000001f;
            float mag = q.length();

            // Catch very small rounding errors when normalizing
            if (mag > MAG_THRESHOLD)
            {
                float oomag = 1f / mag;
                result.X = q.X * oomag;
                result.Y = q.Y * oomag;
                result.Z = q.Z * oomag;
                result.W = q.W * oomag;
            }
            else
            {
            	result.X = 0f;
            	result.Y = 0f;
            	result.Z = 0f;
            	result.W = 1f;
            }

            return result;
        }

        public static Quaternion parse(String val)
        {
            String splitChar = ",";
            String[] split = val.replace("<", "").replace(">", "").split(splitChar);
            if (split.length == 3)
            {
                return new Quaternion(
                    Float.parseFloat(split[0].trim()),
                    Float.parseFloat(split[1].trim()),
                    Float.parseFloat(split[2].trim()));
            }
            else
            {
                return new Quaternion(
                    Float.parseFloat(split[0].trim()),
                    Float.parseFloat(split[1].trim()),
                    Float.parseFloat(split[2].trim()),
                    Float.parseFloat(split[3].trim()));
            }
        }

        public static boolean tryParse(String val, Quaternion[] result)
        {
            try
            {
                result[0] = parse(val);
                return true;
            }
            catch (Exception e)
            {
                result[0] = new Quaternion();
                return false;
            }
        }

        //endregion Static Methods

        //region Overrides

        public boolean equals(Object obj)
        {
            return (obj instanceof Quaternion) ? equals((Quaternion)obj) : false;
        }

        public boolean equals(Quaternion other)
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
        /// Get a String representation of the quaternion elements with up to three
        /// decimal digits and separated by spaces only
        /// </summary>
        /// <returns>Raw String representation of the quaternion</returns>
        public String toRawString()
        {
            return ""+ X + ", " + Y + ", " + Z + ", " + W;

        }

        //endregion Overrides

        /// <summary>A quaternion with a value of 0,0,0,1</summary>
        public final static Quaternion Identity = new Quaternion(0f, 0f, 0f, 1f);
    }
