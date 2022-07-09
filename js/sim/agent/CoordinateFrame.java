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
package com.ngt.jopenmetaverse.shared.sim.agent;

import com.ngt.jopenmetaverse.shared.types.Matrix4;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.Vector3;

public class CoordinateFrame 
{
	   public static final Vector3 X_AXIS = new Vector3(1f, 0f, 0f);
       public static final Vector3 Y_AXIS = new Vector3(0f, 1f, 0f);
       public static final Vector3 Z_AXIS = new Vector3(0f, 0f, 1f);

       /// <summary>Origin position of this coordinate frame</summary>
       public Vector3 getOrigin()
       {
           return origin;
       }
       
       public void setOrigin(Vector3 value)
       {
               if (!value.isFinite())
                   throw new IllegalArgumentException("Non-finite in CoordinateFrame.Origin assignment");
               origin = value;
       }
       
       /// <summary>X axis of this coordinate frame, or Forward/At in grid terms</summary>
       public Vector3 getXAxis()
       {
           return xAxis;
       }
       
       public void setXAxis(Vector3 value)
       {
               if (!value.isFinite())
                   throw new IllegalArgumentException("Non-finite in CoordinateFrame.XAxis assignment");
               xAxis = value;
       }
       
       /// <summary>Y axis of this coordinate frame, or Left in grid terms</summary>
       public Vector3 getYAxis()
       {
           return yAxis;
       }
       
       public void setYAxis(Vector3 value)
       {
               if (!value.isFinite())
                   throw new IllegalArgumentException("Non-finite in CoordinateFrame.YAxis assignment");
               yAxis = value;
       }
       
       /// <summary>Z axis of this coordinate frame, or Up in grid terms</summary>
       public Vector3 getZAxis()
       {
           return zAxis; 
       }

       public void setZAxis(Vector3 value)
       {
               if (!value.isFinite())
                   throw new IllegalArgumentException("Non-finite in CoordinateFrame.ZAxis assignment");
               zAxis = value;
       }
       
       protected Vector3 origin;
       protected Vector3 xAxis;
       protected Vector3 yAxis;
       protected Vector3 zAxis;

       //region Constructors

       public CoordinateFrame(Vector3 origin)
       {
           this.origin = origin;
           xAxis = X_AXIS;
           yAxis = Y_AXIS;
           zAxis = Z_AXIS;

           if (!this.origin.isFinite())
               throw new IllegalArgumentException("Non-finite in CoordinateFrame constructor");
       }

       public CoordinateFrame(Vector3 origin, Vector3 direction)
       {
           this.origin = origin;
           LookDirection(direction);

           if (!IsFinite())
               throw new IllegalArgumentException("Non-finite in CoordinateFrame constructor");
       }

       public CoordinateFrame(Vector3 origin, Vector3 xAxis, Vector3 yAxis, Vector3 zAxis)
       {
           this.origin = origin;
           this.xAxis = xAxis;
           this.yAxis = yAxis;
           this.zAxis = zAxis;

           if (!IsFinite())
               throw new IllegalArgumentException("Non-finite in CoordinateFrame constructor");
       }

       public CoordinateFrame(Vector3 origin, Matrix4 rotation)
       {
           this.origin = origin;
           xAxis = rotation.getAtAxis();
           yAxis = rotation.getLeftAxis();
           zAxis = rotation.getUpAxis();

           if (!IsFinite())
               throw new IllegalArgumentException("Non-finite in CoordinateFrame constructor");
       }

       public CoordinateFrame(Vector3 origin, Quaternion rotation)
       {
           Matrix4 m = Matrix4.createFromQuaternion(rotation);

           this.origin = origin;
           xAxis = m.getAtAxis();
           yAxis = m.getLeftAxis();
           zAxis = m.getUpAxis();

           if (!IsFinite())
               throw new IllegalArgumentException("Non-finite in CoordinateFrame constructor");
       }

       //endregion Constructors

       //region Public Methods

       public void ResetAxes()
       {
           xAxis = X_AXIS;
           yAxis = Y_AXIS;
           zAxis = Z_AXIS;
       }

       public void Rotate(float angle, Vector3 rotationAxis) throws Exception
       {
           Quaternion q = Quaternion.createFromAxisAngle(rotationAxis, angle);
           Rotate(q);
       }

       public void Rotate(Quaternion q) throws Exception
       {
           Matrix4 m = Matrix4.createFromQuaternion(q);
           Rotate(m);
       }

       public void Rotate(Matrix4 m) throws Exception
       {
           xAxis = Vector3.transform(xAxis, m);
           yAxis = Vector3.transform(yAxis, m);

           Orthonormalize();

           if (!IsFinite())
               throw new Exception("Non-finite in CoordinateFrame.Rotate()");
       }

       public void Roll(float angle) throws Exception
       {
           Quaternion q = Quaternion.createFromAxisAngle(xAxis, angle);
           Matrix4 m = Matrix4.createFromQuaternion(q);
           Rotate(m);

           if (!yAxis.isFinite() || !zAxis.isFinite())
               throw new Exception("Non-finite in CoordinateFrame.Roll()");
       }

       public void Pitch(float angle) throws Exception
       {
           Quaternion q = Quaternion.createFromAxisAngle(yAxis, angle);
           Matrix4 m = Matrix4.createFromQuaternion(q);
           Rotate(m);

           if (!xAxis.isFinite() || !zAxis.isFinite())
               throw new Exception("Non-finite in CoordinateFrame.Pitch()");
       }

       public void Yaw(float angle) throws Exception
       {
           Quaternion q = Quaternion.createFromAxisAngle(zAxis, angle);
           Matrix4 m = Matrix4.createFromQuaternion(q);
           Rotate(m);

           if (!xAxis.isFinite() || !yAxis.isFinite())
               throw new Exception("Non-finite in CoordinateFrame.Yaw()");
       }

       public void LookDirection(Vector3 at)
       {
           LookDirection(at, Z_AXIS);
       }

       /// <summary>
       /// 
       /// </summary>
       /// <param name="at">Looking direction, must be a normalized vector</param>
       /// <param name="upDirection">Up direction, must be a normalized vector</param>
       public void LookDirection(Vector3 at, Vector3 upDirection)
       {
           // The two parameters cannot be parallel
           Vector3 left = Vector3.cross(upDirection, at);
           if (left == Vector3.Zero)
           {
               // Prevent left from being zero
               at.X += 0.01f;
               at.normalize();
               left = Vector3.cross(upDirection, at);
           }
           left.normalize();

           xAxis = at;
           yAxis = left;
           zAxis = Vector3.cross(at, left);
       }

       /// <summary>
       /// Align the coordinate frame X and Y axis with a given rotation
       /// around the Z axis in radians
       /// </summary>
       /// <param name="heading">Absolute rotation around the Z axis in
       /// radians</param>
       public void LookDirection(double heading)
       {
           yAxis.X = (float)Math.cos(heading);
           yAxis.Y = (float)Math.sin(heading);
           xAxis.X = (float)-Math.sin(heading);
           xAxis.Y = (float)Math.cos(heading);
       }

       public void LookAt(Vector3 origin, Vector3 target)
       {
           LookAt(origin, target, new Vector3(0f, 0f, 1f));
       }

       public void LookAt(Vector3 origin, Vector3 target, Vector3 upDirection)
       {
           this.origin = origin;
           Vector3 at = Vector3.subtract(target, origin);
           at.normalize();

           LookDirection(at, upDirection);
       }

       //endregion Public Methods

       protected boolean IsFinite()
       {
           if (xAxis.isFinite() && yAxis.isFinite() && zAxis.isFinite())
               return true;
           else
               return false;
       }

       protected void Orthonormalize()
       {
           // Make sure the axis are orthagonal and normalized
           xAxis.normalize();
//           yAxis -= xAxis * (xAxis * yAxis);
           yAxis = Vector3.subtract(yAxis, Vector3.multiply(xAxis, Vector3.multiply(xAxis, yAxis)));
           yAxis.normalize();
           zAxis = Vector3.cross(xAxis, yAxis);
       }
}
