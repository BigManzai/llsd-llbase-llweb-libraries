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


    public class Matrix4 implements  Serializable
    {
        public float M11, M12, M13, M14;
        public float M21, M22, M23, M24;
        public float M31, M32, M33, M34;
        public float M41, M42, M43, M44;

        //region Properties

        public Vector3 getAtAxis()
        {
                return new Vector3(M11, M21, M31);
        }

        public void setAtAxis(Vector3 value)
        {
            M12 = value.X;
            M22 = value.Y;
            M32 = value.Z;
        }
        
        public Vector3 getLeftAxis()
        {
                return new Vector3(M12, M22, M32);
            }
        
        public void setLeftAxis(Vector3 value)
            {
                M12 = value.X;
                M22 = value.Y;
                M32 = value.Z;
            }

        public Vector3 getUpAxis()
        {
                return new Vector3(M13, M23, M33);
            }
        
            public void setUpAxis(Vector3 value)
            {
                M13 = value.X;
                M23 = value.Y;
                M33 = value.Z;
            }

        //endregion Properties

        //region Constructors

            public Matrix4()
            {
            	this(0.0f, 0.0f, 0.0f, 0.0f,
                		0.0f, 0.0f, 0.0f, 0.0f,
                		0.0f, 0.0f, 0.0f, 0.0f,
                		0.0f, 0.0f, 0.0f, 0.0f);
            }
            
        public Matrix4(
            float m11, float m12, float m13, float m14,
            float m21, float m22, float m23, float m24,
            float m31, float m32, float m33, float m34,
            float m41, float m42, float m43, float m44)
        {
            M11 = m11;
            M12 = m12;
            M13 = m13;
            M14 = m14;

            M21 = m21;
            M22 = m22;
            M23 = m23;
            M24 = m24;

            M31 = m31;
            M32 = m32;
            M33 = m33;
            M34 = m34;

            M41 = m41;
            M42 = m42;
            M43 = m43;
            M44 = m44;
        }

        public Matrix4(float roll, float pitch, float yaw)
        {
        	this(createFromEulers(roll, pitch, yaw));
        }

        public Matrix4(Matrix4 m)
        {
            M11 = m.M11;
            M12 = m.M12;
            M13 = m.M13;
            M14 = m.M14;

            M21 = m.M21;
            M22 = m.M22;
            M23 = m.M23;
            M24 = m.M24;

            M31 = m.M31;
            M32 = m.M32;
            M33 = m.M33;
            M34 = m.M34;

            M41 = m.M41;
            M42 = m.M42;
            M43 = m.M43;
            M44 = m.M44;
        }

        //endregion Constructors

        //region Public Methods

        public float determinant()
        {
            return
                M14 * M23 * M32 * M41 - M13 * M24 * M32 * M41 - M14 * M22 * M33 * M41 + M12 * M24 * M33 * M41 +
                M13 * M22 * M34 * M41 - M12 * M23 * M34 * M41 - M14 * M23 * M31 * M42 + M13 * M24 * M31 * M42 +
                M14 * M21 * M33 * M42 - M11 * M24 * M33 * M42 - M13 * M21 * M34 * M42 + M11 * M23 * M34 * M42 +
                M14 * M22 * M31 * M43 - M12 * M24 * M31 * M43 - M14 * M21 * M32 * M43 + M11 * M24 * M32 * M43 +
                M12 * M21 * M34 * M43 - M11 * M22 * M34 * M43 - M13 * M22 * M31 * M44 + M12 * M23 * M31 * M44 +
                M13 * M21 * M32 * M44 - M11 * M23 * M32 * M44 - M12 * M21 * M33 * M44 + M11 * M22 * M33 * M44;
        }

        public float determinant3x3()
        {
            float det = 0f;

            float diag1 = M11 * M22 * M33;
            float diag2 = M12 * M32 * M31;
            float diag3 = M13 * M21 * M32;
            float diag4 = M31 * M22 * M13;
            float diag5 = M32 * M23 * M11;
            float diag6 = M33 * M21 * M12;

            det = diag1 + diag2 + diag3 - (diag4 + diag5 + diag6);

            return det;
        }

        public float trace()
        {
            return M11 + M22 + M33 + M44;
        }

        /// <summary>
        /// Convert this matrix to euler rotations
        /// </summary>
        /// <param name="roll">X euler angle</param>
        /// <param name="pitch">Y euler angle</param>
        /// <param name="yaw">Z euler angle</param>
        public float[] getEulerAngles()
        {
            double angleX, angleY, angleZ;
            double cx, cy, cz; // cosines
            double sx, sz; // sines

            angleY = Math.asin(Utils.clamp(M13, -1f, 1f));
            cy = Math.cos(angleY);

            if (Math.abs(cy) > 0.005f)
            {
                // No gimbal lock
                cx = M33 / cy;
                sx = (-M23) / cy;

                angleX = (float)Math.atan2(sx, cx);

                cz = M11 / cy;
                sz = (-M12) / cy;

                angleZ = (float)Math.atan2(sz, cz);
            }
            else
            {
                // Gimbal lock
                angleX = 0;

                cz = M22;
                sz = M21;

                angleZ = Math.atan2(sz, cz);
            }

            // Return only positive angles in [0,360]
            if (angleX < 0) angleX += 360d;
            if (angleY < 0) angleY += 360d;
            if (angleZ < 0) angleZ += 360d;

            float roll = (float)angleX;
            float pitch = (float)angleY;
            float yaw = (float)angleZ;
            return new float[] {roll, pitch, yaw};
        }

        /// <summary>
        /// Convert this matrix to a quaternion rotation
        /// </summary>
        /// <returns>A quaternion representation of this rotation matrix</returns>
        public Quaternion getQuaternion()
        {
            Quaternion quat = new Quaternion();
            float trace = trace() + 1f;

            if (trace > Float.MIN_EXPONENT)
            {
                float s = 0.5f / (float)Math.sqrt(trace);

                quat.X = (M32 - M23) * s;
                quat.Y = (M13 - M31) * s;
                quat.Z = (M21 - M12) * s;
                quat.W = 0.25f / s;
            }
            else
            {
                if (M11 > M22 && M11 > M33)
                {
                    float s = 2.0f * (float)Math.sqrt(1.0f + M11 - M22 - M33);

                    quat.X = 0.25f * s;
                    quat.Y = (M12 + M21) / s;
                    quat.Z = (M13 + M31) / s;
                    quat.W = (M23 - M32) / s;
                }
                else if (M22 > M33)
                {
                    float s = 2.0f * (float)Math.sqrt(1.0f + M22 - M11 - M33);

                    quat.X = (M12 + M21) / s;
                    quat.Y = 0.25f * s;
                    quat.Z = (M23 + M32) / s;
                    quat.W = (M13 - M31) / s;
                }
                else
                {
                    float s = 2.0f * (float)Math.sqrt(1.0f + M33 - M11 - M22);

                    quat.X = (M13 + M31) / s;
                    quat.Y = (M23 + M32) / s;
                    quat.Z = 0.25f * s;
                    quat.W = (M12 - M21) / s;
                }
            }

            return quat;
        }

        //endregion Public Methods

        //region Static Methods

        public static Matrix4 add(Matrix4 matrix1, Matrix4 matrix2)
        {
            Matrix4 matrix = new Matrix4();
            matrix.M11 = matrix1.M11 + matrix2.M11;
            matrix.M12 = matrix1.M12 + matrix2.M12;
            matrix.M13 = matrix1.M13 + matrix2.M13;
            matrix.M14 = matrix1.M14 + matrix2.M14;

            matrix.M21 = matrix1.M21 + matrix2.M21;
            matrix.M22 = matrix1.M22 + matrix2.M22;
            matrix.M23 = matrix1.M23 + matrix2.M23;
            matrix.M24 = matrix1.M24 + matrix2.M24;

            matrix.M31 = matrix1.M31 + matrix2.M31;
            matrix.M32 = matrix1.M32 + matrix2.M32;
            matrix.M33 = matrix1.M33 + matrix2.M33;
            matrix.M34 = matrix1.M34 + matrix2.M34;

            matrix.M41 = matrix1.M41 + matrix2.M41;
            matrix.M42 = matrix1.M42 + matrix2.M42;
            matrix.M43 = matrix1.M43 + matrix2.M43;
            matrix.M44 = matrix1.M44 + matrix2.M44;
            return matrix;
        }

        public static Matrix4 createFromAxisAngle(Vector3 axis, float angle)
        {
            Matrix4 matrix = new Matrix4();

            float x = axis.X;
            float y = axis.Y;
            float z = axis.Z;
            float sin = (float)Math.sin(angle);
            float cos = (float)Math.cos(angle);
            float xx = x * x;
            float yy = y * y;
            float zz = z * z;
            float xy = x * y;
            float xz = x * z;
            float yz = y * z;

            matrix.M11 = xx + (cos * (1f - xx));
            matrix.M12 = (xy - (cos * xy)) + (sin * z);
            matrix.M13 = (xz - (cos * xz)) - (sin * y);
            //matrix.M14 = 0f;

            matrix.M21 = (xy - (cos * xy)) - (sin * z);
            matrix.M22 = yy + (cos * (1f - yy));
            matrix.M23 = (yz - (cos * yz)) + (sin * x);
            //matrix.M24 = 0f;

            matrix.M31 = (xz - (cos * xz)) + (sin * y);
            matrix.M32 = (yz - (cos * yz)) - (sin * x);
            matrix.M33 = zz + (cos * (1f - zz));
            //matrix.M34 = 0f;

            //matrix.M41 = matrix.M42 = matrix.M43 = 0f;
            matrix.M44 = 1f;

            return matrix;
        }

        /// <summary>
        /// Construct a matrix from euler rotation values in radians
        /// </summary>
        /// <param name="roll">X euler angle in radians</param>
        /// <param name="pitch">Y euler angle in radians</param>
        /// <param name="yaw">Z euler angle in radians</param>
        public static Matrix4 createFromEulers(float roll, float pitch, float yaw)
        {
            Matrix4 m = new Matrix4();

            float a, b, c, d, e, f;
            float ad, bd;

            a = (float)Math.cos(roll);
            b = (float)Math.sin(roll);
            c = (float)Math.cos(pitch);
            d = (float)Math.sin(pitch);
            e = (float)Math.cos(yaw);
            f = (float)Math.sin(yaw);

            ad = a * d;
            bd = b * d;

            m.M11 = c * e;
            m.M12 = -c * f;
            m.M13 = d;
            m.M14 = 0f;

            m.M21 = bd * e + a * f;
            m.M22 = -bd * f + a * e;
            m.M23 = -b * c;
            m.M24 = 0f;

            m.M31 = -ad * e + b * f;
            m.M32 = ad * f + b * e;
            m.M33 = a * c;
            m.M34 = 0f;

            m.M41 = m.M42 = m.M43 = 0f;
            m.M44 = 1f;

            return m;
        }

        public static Matrix4 createFromQuaternion(Quaternion quaternion)
        {
            Matrix4 matrix = new Matrix4();

            float xx = quaternion.X * quaternion.X;
            float yy = quaternion.Y * quaternion.Y;
            float zz = quaternion.Z * quaternion.Z;
            float xy = quaternion.X * quaternion.Y;
            float zw = quaternion.Z * quaternion.W;
            float zx = quaternion.Z * quaternion.X;
            float yw = quaternion.Y * quaternion.W;
            float yz = quaternion.Y * quaternion.Z;
            float xw = quaternion.X * quaternion.W;

            matrix.M11 = 1f - (2f * (yy + zz));
            matrix.M12 = 2f * (xy + zw);
            matrix.M13 = 2f * (zx - yw);
            matrix.M14 = 0f;

            matrix.M21 = 2f * (xy - zw);
            matrix.M22 = 1f - (2f * (zz + xx));
            matrix.M23 = 2f * (yz + xw);
            matrix.M24 = 0f;

            matrix.M31 = 2f * (zx + yw);
            matrix.M32 = 2f * (yz - xw);
            matrix.M33 = 1f - (2f * (yy + xx));
            matrix.M34 = 0f;

            matrix.M41 = matrix.M42 = matrix.M43 = 0f;
            matrix.M44 = 1f;

            return matrix;
        }

        public static Matrix4 createLookAt(Vector3 cameraPosition, Vector3 cameraTarget, Vector3 cameraUpVector)
        {
            Matrix4 matrix = new Matrix4();

            Vector3 z = Vector3.normalize(Vector3.subtract(cameraPosition, cameraTarget));
            Vector3 x = Vector3.normalize(Vector3.cross(cameraUpVector, z));
            Vector3 y = Vector3.cross(z, x);

            matrix.M11 = x.X;
            matrix.M12 = y.X;
            matrix.M13 = z.X;
            matrix.M14 = 0f;

            matrix.M21 = x.Y;
            matrix.M22 = y.Y;
            matrix.M23 = z.Y;
            matrix.M24 = 0f;

            matrix.M31 = x.Z;
            matrix.M32 = y.Z;
            matrix.M33 = z.Z;
            matrix.M34 = 0f;

            matrix.M41 = -Vector3.dot(x, cameraPosition);
            matrix.M42 = -Vector3.dot(y, cameraPosition);
            matrix.M43 = -Vector3.dot(z, cameraPosition);
            matrix.M44 = 1f;

            return matrix;
        }

        public static Matrix4 createRotationX(float radians)
        {
            Matrix4 matrix = new Matrix4();

            float cos = (float)Math.cos(radians);
            float sin = (float)Math.sin(radians);

            matrix.M11 = 1f;
            matrix.M12 = 0f;
            matrix.M13 = 0f;
            matrix.M14 = 0f;

            matrix.M21 = 0f;
            matrix.M22 = cos;
            matrix.M23 = sin;
            matrix.M24 = 0f;

            matrix.M31 = 0f;
            matrix.M32 = -sin;
            matrix.M33 = cos;
            matrix.M34 = 0f;

            matrix.M41 = 0f;
            matrix.M42 = 0f;
            matrix.M43 = 0f;
            matrix.M44 = 1f;

            return matrix;
        }

        public static Matrix4 createRotationY(float radians)
        {
            Matrix4 matrix = new Matrix4();

            float cos = (float)Math.cos(radians);
            float sin = (float)Math.sin(radians);

            matrix.M11 = cos;
            matrix.M12 = 0f;
            matrix.M13 = -sin;
            matrix.M14 = 0f;

            matrix.M21 = 0f;
            matrix.M22 = 1f;
            matrix.M23 = 0f;
            matrix.M24 = 0f;

            matrix.M31 = sin;
            matrix.M32 = 0f;
            matrix.M33 = cos;
            matrix.M34 = 0f;

            matrix.M41 = 0f;
            matrix.M42 = 0f;
            matrix.M43 = 0f;
            matrix.M44 = 1f;

            return matrix;
        }

        public static Matrix4 createRotationZ(float radians)
        {
            Matrix4 matrix = new Matrix4();

            float cos = (float)Math.cos(radians);
            float sin = (float)Math.sin(radians);

            matrix.M11 = cos;
            matrix.M12 = sin;
            matrix.M13 = 0f;
            matrix.M14 = 0f;

            matrix.M21 = -sin;
            matrix.M22 = cos;
            matrix.M23 = 0f;
            matrix.M24 = 0f;

            matrix.M31 = 0f;
            matrix.M32 = 0f;
            matrix.M33 = 1f;
            matrix.M34 = 0f;

            matrix.M41 = 0f;
            matrix.M42 = 0f;
            matrix.M43 = 0f;
            matrix.M44 = 1f;

            return matrix;
        }

        public static Matrix4 createScale(Vector3 scale)
        {
            Matrix4 matrix = new Matrix4();

            matrix.M11 = scale.X;
            matrix.M12 = 0f;
            matrix.M13 = 0f;
            matrix.M14 = 0f;

            matrix.M21 = 0f;
            matrix.M22 = scale.Y;
            matrix.M23 = 0f;
            matrix.M24 = 0f;

            matrix.M31 = 0f;
            matrix.M32 = 0f;
            matrix.M33 = scale.Z;
            matrix.M34 = 0f;

            matrix.M41 = 0f;
            matrix.M42 = 0f;
            matrix.M43 = 0f;
            matrix.M44 = 1f;

            return matrix;
        }

        public static Matrix4 createTranslation(Vector3 position)
        {
            Matrix4 matrix = new Matrix4();

            matrix.M11 = 1f;
            matrix.M12 = 0f;
            matrix.M13 = 0f;
            matrix.M14 = 0f;

            matrix.M21 = 0f;
            matrix.M22 = 1f;
            matrix.M23 = 0f;
            matrix.M24 = 0f;

            matrix.M31 = 0f;
            matrix.M32 = 0f;
            matrix.M33 = 1f;
            matrix.M34 = 0f;

            matrix.M41 = position.X;
            matrix.M42 = position.Y;
            matrix.M43 = position.Z;
            matrix.M44 = 1f;
            
            return matrix;
        }

        public static Matrix4 createWorld(Vector3 position, Vector3 forward, Vector3 up)
        {
            Matrix4 result = new Matrix4();
            
            // normalize forward vector
            forward.normalize();

            // Calculate right vector
            Vector3 right = Vector3.cross(forward, up);
            right.normalize();

            // Recalculate up vector
            up = Vector3.cross(right, forward);
            up.normalize();

            result.M11 = right.X;
            result.M12 = right.Y;
            result.M13 = right.Z;
            result.M14 = 0.0f;

            result.M21 = up.X;
            result.M22 = up.Y;
            result.M23 = up.Z;
            result.M24 = 0.0f;

            result.M31 = -forward.X;
            result.M32 = -forward.Y;
            result.M33 = -forward.Z;
            result.M34 = 0.0f;

            result.M41 = position.X;
            result.M42 = position.Y;
            result.M43 = position.Z;
            result.M44 = 1.0f;

            return result;
        }

        public static Matrix4 divide(Matrix4 matrix1, Matrix4 matrix2)
        {
            Matrix4 matrix = new Matrix4();

            matrix.M11 = matrix1.M11 / matrix2.M11;
            matrix.M12 = matrix1.M12 / matrix2.M12;
            matrix.M13 = matrix1.M13 / matrix2.M13;
            matrix.M14 = matrix1.M14 / matrix2.M14;

            matrix.M21 = matrix1.M21 / matrix2.M21;
            matrix.M22 = matrix1.M22 / matrix2.M22;
            matrix.M23 = matrix1.M23 / matrix2.M23;
            matrix.M24 = matrix1.M24 / matrix2.M24;

            matrix.M31 = matrix1.M31 / matrix2.M31;
            matrix.M32 = matrix1.M32 / matrix2.M32;
            matrix.M33 = matrix1.M33 / matrix2.M33;
            matrix.M34 = matrix1.M34 / matrix2.M34;

            matrix.M41 = matrix1.M41 / matrix2.M41;
            matrix.M42 = matrix1.M42 / matrix2.M42;
            matrix.M43 = matrix1.M43 / matrix2.M43;
            matrix.M44 = matrix1.M44 / matrix2.M44;

            return matrix;
        }

        public static Matrix4 divide(Matrix4 matrix1, float divider)
        {
            Matrix4 matrix = new Matrix4();

            float oodivider = 1f / divider;
            matrix.M11 = matrix1.M11 * oodivider;
            matrix.M12 = matrix1.M12 * oodivider;
            matrix.M13 = matrix1.M13 * oodivider;
            matrix.M14 = matrix1.M14 * oodivider;

            matrix.M21 = matrix1.M21 * oodivider;
            matrix.M22 = matrix1.M22 * oodivider;
            matrix.M23 = matrix1.M23 * oodivider;
            matrix.M24 = matrix1.M24 * oodivider;

            matrix.M31 = matrix1.M31 * oodivider;
            matrix.M32 = matrix1.M32 * oodivider;
            matrix.M33 = matrix1.M33 * oodivider;
            matrix.M34 = matrix1.M34 * oodivider;

            matrix.M41 = matrix1.M41 * oodivider;
            matrix.M42 = matrix1.M42 * oodivider;
            matrix.M43 = matrix1.M43 * oodivider;
            matrix.M44 = matrix1.M44 * oodivider;

            return matrix;
        }

        public static Matrix4 lerp(Matrix4 matrix1, Matrix4 matrix2, float amount)
        {
            Matrix4 matrix = new Matrix4();

            matrix.M11 = matrix1.M11 + ((matrix2.M11 - matrix1.M11) * amount);
            matrix.M12 = matrix1.M12 + ((matrix2.M12 - matrix1.M12) * amount);
            matrix.M13 = matrix1.M13 + ((matrix2.M13 - matrix1.M13) * amount);
            matrix.M14 = matrix1.M14 + ((matrix2.M14 - matrix1.M14) * amount);

            matrix.M21 = matrix1.M21 + ((matrix2.M21 - matrix1.M21) * amount);
            matrix.M22 = matrix1.M22 + ((matrix2.M22 - matrix1.M22) * amount);
            matrix.M23 = matrix1.M23 + ((matrix2.M23 - matrix1.M23) * amount);
            matrix.M24 = matrix1.M24 + ((matrix2.M24 - matrix1.M24) * amount);

            matrix.M31 = matrix1.M31 + ((matrix2.M31 - matrix1.M31) * amount);
            matrix.M32 = matrix1.M32 + ((matrix2.M32 - matrix1.M32) * amount);
            matrix.M33 = matrix1.M33 + ((matrix2.M33 - matrix1.M33) * amount);
            matrix.M34 = matrix1.M34 + ((matrix2.M34 - matrix1.M34) * amount);

            matrix.M41 = matrix1.M41 + ((matrix2.M41 - matrix1.M41) * amount);
            matrix.M42 = matrix1.M42 + ((matrix2.M42 - matrix1.M42) * amount);
            matrix.M43 = matrix1.M43 + ((matrix2.M43 - matrix1.M43) * amount);
            matrix.M44 = matrix1.M44 + ((matrix2.M44 - matrix1.M44) * amount);

            return matrix;
        }

        public static Matrix4 multiply(Matrix4 matrix1, Matrix4 matrix2)
        {
            return new Matrix4(
                matrix1.M11 * matrix2.M11 + matrix1.M12 * matrix2.M21 + matrix1.M13 * matrix2.M31 + matrix1.M14 * matrix2.M41,
                matrix1.M11 * matrix2.M12 + matrix1.M12 * matrix2.M22 + matrix1.M13 * matrix2.M32 + matrix1.M14 * matrix2.M42,
                matrix1.M11 * matrix2.M13 + matrix1.M12 * matrix2.M23 + matrix1.M13 * matrix2.M33 + matrix1.M14 * matrix2.M43,
                matrix1.M11 * matrix2.M14 + matrix1.M12 * matrix2.M24 + matrix1.M13 * matrix2.M34 + matrix1.M14 * matrix2.M44,

                matrix1.M21 * matrix2.M11 + matrix1.M22 * matrix2.M21 + matrix1.M23 * matrix2.M31 + matrix1.M24 * matrix2.M41,
                matrix1.M21 * matrix2.M12 + matrix1.M22 * matrix2.M22 + matrix1.M23 * matrix2.M32 + matrix1.M24 * matrix2.M42,
                matrix1.M21 * matrix2.M13 + matrix1.M22 * matrix2.M23 + matrix1.M23 * matrix2.M33 + matrix1.M24 * matrix2.M43,
                matrix1.M21 * matrix2.M14 + matrix1.M22 * matrix2.M24 + matrix1.M23 * matrix2.M34 + matrix1.M24 * matrix2.M44,

                matrix1.M31 * matrix2.M11 + matrix1.M32 * matrix2.M21 + matrix1.M33 * matrix2.M31 + matrix1.M34 * matrix2.M41,
                matrix1.M31 * matrix2.M12 + matrix1.M32 * matrix2.M22 + matrix1.M33 * matrix2.M32 + matrix1.M34 * matrix2.M42,
                matrix1.M31 * matrix2.M13 + matrix1.M32 * matrix2.M23 + matrix1.M33 * matrix2.M33 + matrix1.M34 * matrix2.M43,
                matrix1.M31 * matrix2.M14 + matrix1.M32 * matrix2.M24 + matrix1.M33 * matrix2.M34 + matrix1.M34 * matrix2.M44,

                matrix1.M41 * matrix2.M11 + matrix1.M42 * matrix2.M21 + matrix1.M43 * matrix2.M31 + matrix1.M44 * matrix2.M41,
                matrix1.M41 * matrix2.M12 + matrix1.M42 * matrix2.M22 + matrix1.M43 * matrix2.M32 + matrix1.M44 * matrix2.M42,
                matrix1.M41 * matrix2.M13 + matrix1.M42 * matrix2.M23 + matrix1.M43 * matrix2.M33 + matrix1.M44 * matrix2.M43,
                matrix1.M41 * matrix2.M14 + matrix1.M42 * matrix2.M24 + matrix1.M43 * matrix2.M34 + matrix1.M44 * matrix2.M44
            );
        }

        public static Matrix4 multiply(Matrix4 matrix1, float scaleFactor)
        {
            Matrix4 matrix = new Matrix4();
            matrix.M11 = matrix1.M11 * scaleFactor;
            matrix.M12 = matrix1.M12 * scaleFactor;
            matrix.M13 = matrix1.M13 * scaleFactor;
            matrix.M14 = matrix1.M14 * scaleFactor;

            matrix.M21 = matrix1.M21 * scaleFactor;
            matrix.M22 = matrix1.M22 * scaleFactor;
            matrix.M23 = matrix1.M23 * scaleFactor;
            matrix.M24 = matrix1.M24 * scaleFactor;

            matrix.M31 = matrix1.M31 * scaleFactor;
            matrix.M32 = matrix1.M32 * scaleFactor;
            matrix.M33 = matrix1.M33 * scaleFactor;
            matrix.M34 = matrix1.M34 * scaleFactor;

            matrix.M41 = matrix1.M41 * scaleFactor;
            matrix.M42 = matrix1.M42 * scaleFactor;
            matrix.M43 = matrix1.M43 * scaleFactor;
            matrix.M44 = matrix1.M44 * scaleFactor;
            return matrix;
        }

        public static Matrix4 negate(Matrix4 matrix)
        {
            Matrix4 result = new Matrix4();
            result.M11 = -matrix.M11;
            result.M12 = -matrix.M12;
            result.M13 = -matrix.M13;
            result.M14 = -matrix.M14;

            result.M21 = -matrix.M21;
            result.M22 = -matrix.M22;
            result.M23 = -matrix.M23;
            result.M24 = -matrix.M24;

            result.M31 = -matrix.M31;
            result.M32 = -matrix.M32;
            result.M33 = -matrix.M33;
            result.M34 = -matrix.M34;

            result.M41 = -matrix.M41;
            result.M42 = -matrix.M42;
            result.M43 = -matrix.M43;
            result.M44 = -matrix.M44;
            return result;
        }

        public static Matrix4 subtract(Matrix4 matrix1, Matrix4 matrix2)
        {
            Matrix4 matrix = new Matrix4();
            matrix.M11 = matrix1.M11 - matrix2.M11;
            matrix.M12 = matrix1.M12 - matrix2.M12;
            matrix.M13 = matrix1.M13 - matrix2.M13;
            matrix.M14 = matrix1.M14 - matrix2.M14;

            matrix.M21 = matrix1.M21 - matrix2.M21;
            matrix.M22 = matrix1.M22 - matrix2.M22;
            matrix.M23 = matrix1.M23 - matrix2.M23;
            matrix.M24 = matrix1.M24 - matrix2.M24;

            matrix.M31 = matrix1.M31 - matrix2.M31;
            matrix.M32 = matrix1.M32 - matrix2.M32;
            matrix.M33 = matrix1.M33 - matrix2.M33;
            matrix.M34 = matrix1.M34 - matrix2.M34;

            matrix.M41 = matrix1.M41 - matrix2.M41;
            matrix.M42 = matrix1.M42 - matrix2.M42;
            matrix.M43 = matrix1.M43 - matrix2.M43;
            matrix.M44 = matrix1.M44 - matrix2.M44;
            return matrix;
        }

        public static Matrix4 transform(Matrix4 value, Quaternion rotation)
        {
            Matrix4 matrix = new Matrix4();

            float x2 = rotation.X + rotation.X;
            float y2 = rotation.Y + rotation.Y;
            float z2 = rotation.Z + rotation.Z;

            float a = (1f - rotation.Y * y2) - rotation.Z * z2;
            float b = rotation.X * y2 - rotation.W * z2;
            float c = rotation.X * z2 + rotation.W * y2;
            float d = rotation.X * y2 + rotation.W * z2;
            float e = (1f - rotation.X * x2) - rotation.Z * z2;
            float f = rotation.Y * z2 - rotation.W * x2;
            float g = rotation.X * z2 - rotation.W * y2;
            float h = rotation.Y * z2 + rotation.W * x2;
            float i = (1f - rotation.X * x2) - rotation.Y * y2;

            matrix.M11 = ((value.M11 * a) + (value.M12 * b)) + (value.M13 * c);
            matrix.M12 = ((value.M11 * d) + (value.M12 * e)) + (value.M13 * f);
            matrix.M13 = ((value.M11 * g) + (value.M12 * h)) + (value.M13 * i);
            matrix.M14 = value.M14;

            matrix.M21 = ((value.M21 * a) + (value.M22 * b)) + (value.M23 * c);
            matrix.M22 = ((value.M21 * d) + (value.M22 * e)) + (value.M23 * f);
            matrix.M23 = ((value.M21 * g) + (value.M22 * h)) + (value.M23 * i);
            matrix.M24 = value.M24;

            matrix.M31 = ((value.M31 * a) + (value.M32 * b)) + (value.M33 * c);
            matrix.M32 = ((value.M31 * d) + (value.M32 * e)) + (value.M33 * f);
            matrix.M33 = ((value.M31 * g) + (value.M32 * h)) + (value.M33 * i);
            matrix.M34 = value.M34;

            matrix.M41 = ((value.M41 * a) + (value.M42 * b)) + (value.M43 * c);
            matrix.M42 = ((value.M41 * d) + (value.M42 * e)) + (value.M43 * f);
            matrix.M43 = ((value.M41 * g) + (value.M42 * h)) + (value.M43 * i);
            matrix.M44 = value.M44;

            return matrix;
        }

        public static Matrix4 transpose(Matrix4 matrix)
        {
            Matrix4 result = new Matrix4();

            result.M11 = matrix.M11;
            result.M12 = matrix.M21;
            result.M13 = matrix.M31;
            result.M14 = matrix.M41;

            result.M21 = matrix.M12;
            result.M22 = matrix.M22;
            result.M23 = matrix.M32;
            result.M24 = matrix.M42;

            result.M31 = matrix.M13;
            result.M32 = matrix.M23;
            result.M33 = matrix.M33;
            result.M34 = matrix.M43;

            result.M41 = matrix.M14;
            result.M42 = matrix.M24;
            result.M43 = matrix.M34;
            result.M44 = matrix.M44;

            return result;
        }

        public static Matrix4 inverse3x3(Matrix4 matrix)
        {
            if (matrix.determinant3x3() == 0f)
                throw new IllegalArgumentException("Singular matrix inverse not possible");

            return divide(adjoint3x3(matrix), matrix.determinant3x3());
        }

        public static Matrix4 adjoint3x3(Matrix4 matrix)
        {
            Matrix4 adjointMatrix = new Matrix4();
            for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 4; j++)
                    adjointMatrix.setValue(i,j, (float)(Math.pow(-1, i + j) * (minor(matrix, i, j).determinant3x3())));
            }

            adjointMatrix = transpose(adjointMatrix);
            return adjointMatrix;
        }

        public static Matrix4 inverse(Matrix4 matrix)
        {
            if (matrix.determinant() == 0f)
                throw new IllegalArgumentException("Singular matrix inverse not possible");

            return divide(adjoint(matrix),  matrix.determinant());
        }

        public static Matrix4 adjoint(Matrix4 matrix)
        {
            Matrix4 adjointMatrix = new Matrix4();
            for (int i = 0; i < 4; i++)
            {
                for (int j = 0; j < 4; j++)
                    adjointMatrix.setValue(i,j, (float)(Math.pow(-1, i + j) * ((minor(matrix, i, j)).determinant())));
            }

            adjointMatrix = transpose(adjointMatrix);
            return adjointMatrix;
        }

        public static Matrix4 minor(Matrix4 matrix, int row, int col)
        {
            Matrix4 minor = new Matrix4();
            int m = 0, n = 0;

            for (int i = 0; i < 4; i++)
            {
                if (i == row)
                    continue;
                n = 0;
                for (int j = 0; j < 4; j++)
                {
                    if (j == col)
                        continue;
                    minor.setValue(m,n, matrix.getValue(i,j));
                    n++;
                }
                m++;
            }

            return minor;
        }
        
        //endregion Static Methods

        //region Overrides

        public boolean equals(Object obj)
        {
            return (obj instanceof Matrix4) ? equals((Matrix4)obj) : false;
        }

        public boolean equals(Matrix4 other)
        {
            return equals(this, other);
        }

        public int hashCode()
        {
            return
                (new Float(M11)).hashCode() ^ (new Float(M12)).hashCode() ^ (new Float(M13)).hashCode() ^ (new Float(M14)).hashCode() 
                ^ (new Float(M21)).hashCode() ^ (new Float(M22)).hashCode() ^ (new Float(M23)).hashCode() ^ (new Float(M24)).hashCode() 
                ^ (new Float(M31)).hashCode() ^ (new Float(M32)).hashCode() ^ (new Float(M33)).hashCode() ^ (new Float(M34)).hashCode() 
                ^ (new Float(M41)).hashCode() ^ (new Float(M42)).hashCode() ^ (new Float(M43)).hashCode() ^ (new Float(M44)).hashCode();
        }

        /// <summary>
        /// Get a formatted String representation of the vector
        /// </summary>
        /// <returns>A String representation of the vector</returns>
        public String ToString()
        {
            return "|" + M11 + ", " + M12+ ", " + M13+ ", " + M14+ "|\n" +  M21+ ", " + M22+ ", " + M23+ ", " + M24+ "|\n" + M31+ ", " + M32+ ", " + M33 + ", " + M34 + "|\n" + M41 + ", " + M42 + ", " + M43 + ", " + M44 + "|";
        }

        //endregion Overrides

        //region Operators

        public static boolean equals(Matrix4 left, Matrix4 right)
        {
            return left.getRow(0).equals(right.getRow(0)) &&
            		left.getRow(1).equals(right.getRow(1)) &&
            		left.getRow(2).equals(right.getRow(2)) &&
            		left.getRow(3).equals(right.getRow(3));
        }

        public static boolean notEquals(Matrix4 left, Matrix4 right)
        {
            return !left.equals(right);
        }

//        public static Matrix4 subtract(Matrix4 matrix)
//        {
//            return Negate(matrix);
//        }
//
//        public static Matrix4 subtract(Matrix4 left, Matrix4 right)
//        {
//            return Subtract(left, right);
//        }
//
//        public static Matrix4 multiply(Matrix4 left, Matrix4 right)
//        {
//            return Multiply(left, right);
//        }
//
//        public static Matrix4 multiply(Matrix4 left, float scalar)
//        {
//            return Multiply(left, scalar);
//        }
//
//        public static Matrix4 divide(Matrix4 left, Matrix4 right)
//        {
//            return Divide(left, right);
//        }
//
//        public static Matrix4 divide(Matrix4 matrix, float divider)
//        {
//            return Divide(matrix, divider);
//        }

        public Vector4 getRow(int row)
        {
        	 switch (row)
             {
                 case 0:
                     return new Vector4(M11, M12, M13, M14);
                 case 1:
                     return new Vector4(M21, M22, M23, M24);
                 case 2:
                     return new Vector4(M31, M32, M33, M34);
                 case 3:
                     return new Vector4(M41, M42, M43, M44);
                 default:
                     throw new IndexOutOfBoundsException("Matrix4 row index must be from 0-3");
             }
        }
        
        public void setRow(int row, Vector4 value)
        {
        	switch (row)
            {
                case 0:
                    M11 = value.X;
                    M12 = value.Y;
                    M13 = value.Z;
                    M14 = value.W;
                    break;
                case 1:
                    M21 = value.X;
                    M22 = value.Y;
                    M23 = value.Z;
                    M24 = value.W;
                    break;
                case 2:
                    M31 = value.X;
                    M32 = value.Y;
                    M33 = value.Z;
                    M34 = value.W;
                    break;
                case 3:
                    M41 = value.X;
                    M42 = value.Y;
                    M43 = value.Z;
                    M44 = value.W;
                    break;
                default:
                    throw new IndexOutOfBoundsException("Matrix4 row index must be from 0-3");
            }
        }

        public void setValue(int row, int column, float value)
        {
        	switch (row)
            {
                case 0:
                    switch (column)
                    {
                        case 0:
                            M11 = value;
                        case 1:
                            M12 = value;
                        case 2:
                            M13 = value;
                        case 3:
                            M14 = value;
                        default:
                            throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                    }
                case 1:
                    switch (column)
                    {
                        case 0:
                            M21 = value;
                        case 1:
                            M22 = value;
                        case 2:
                            M23 = value;
                        case 3:
                            M24 = value;
                        default:
                            throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                    }
                case 2:
                    switch (column)
                    {
                        case 0:
                            M31 = value;
                        case 1:
                            M32 = value;
                        case 2:
                            M33 = value;
                        case 3:
                            M34 = value;
                        default:
                            throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                    }
                case 3:
                    switch (column)
                    {
                        case 0:
                            M41 = value;
                        case 1:
                            M42 = value;
                        case 2:
                            M43 = value;
                        case 3:
                            M44 = value;
                        default:
                            throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                    }
                default:
                    throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
            }
        }
        
        
        
        public float getValue(int row, int column)
        {
                switch (row)
                {
                    case 0:
                        switch (column)
                        {
                            case 0:
                                return M11;
                            case 1:
                                return M12;
                            case 2:
                                return M13;
                            case 3:
                                return M14;
                            default:
                                throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                        }
                    case 1:
                        switch (column)
                        {
                            case 0:
                                return M21;
                            case 1:
                                return M22;
                            case 2:
                                return M23;
                            case 3:
                                return M24;
                            default:
                                throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                        }
                    case 2:
                        switch (column)
                        {
                            case 0:
                                return M31;
                            case 1:
                                return M32;
                            case 2:
                                return M33;
                            case 3:
                                return M34;
                            default:
                                throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                        }
                    case 3:
                        switch (column)
                        {
                            case 0:
                                return M41;
                            case 1:
                                return M42;
                            case 2:
                                return M43;
                            case 3:
                                return M44;
                            default:
                                throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                        }
                    default:
                        throw new IndexOutOfBoundsException("Matrix4 row and column values must be from 0-3");
                }
            }
        //endregion Operators

        /// <summary>A 4x4 matrix containing all zeroes</summary>
        public final static Matrix4 Zero = new Matrix4();

        /// <summary>A 4x4 identity matrix</summary>
        public static final Matrix4 Identity = new Matrix4(
            1f, 0f, 0f, 0f,
            0f, 1f, 0f, 0f,
            0f, 0f, 1f, 0f,
            0f, 0f, 0f, 1f);
 }
