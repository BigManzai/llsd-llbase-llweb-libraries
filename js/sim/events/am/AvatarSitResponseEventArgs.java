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
package com.ngt.jopenmetaverse.shared.sim.events.am;

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;

/// <summary>Contains the response data returned from the simulator in response to a <see cref="RequestSit"/></summary>
	public class AvatarSitResponseEventArgs extends EventArgs
	{
		private  UUID m_ObjectID;
		private  boolean m_Autopilot;
		private  Vector3 m_CameraAtOffset;
		private  Vector3 m_CameraEyeOffset;
		private  boolean m_ForceMouselook;
		private  Vector3 m_SitPosition;
		private  Quaternion m_SitRotation;

		/// <summary>Get the ID of the primitive the agent will be sitting on</summary>
		public UUID getObjectID() {return m_ObjectID;}
		/// <summary>True if the simulator Autopilot functions were involved</summary>
		public boolean getAutopilot() {return m_Autopilot;}
		/// <summary>Get the camera offset of the agent when seated</summary>
		public Vector3 getCameraAtOffset() {return m_CameraAtOffset;}
		/// <summary>Get the camera eye offset of the agent when seated</summary>
		public Vector3 getCameraEyeOffset() {return m_CameraEyeOffset;}
		/// <summary>True of the agent will be in mouselook mode when seated</summary>
		public boolean getForceMouselook() {return m_ForceMouselook;}
		/// <summary>Get the position of the agent when seated</summary>
		public Vector3 getSitPosition() {return m_SitPosition;}
		/// <summary>Get the rotation of the agent when seated</summary>
		public Quaternion getSitRotation() {return m_SitRotation;}

		/// <summary>Construct a new instance of the AvatarSitResponseEventArgs object</summary>
		public AvatarSitResponseEventArgs(UUID objectID, boolean autoPilot, Vector3 cameraAtOffset,
				Vector3 cameraEyeOffset, boolean forceMouselook, Vector3 sitPosition, Quaternion sitRotation)
		{
			this.m_ObjectID = objectID;
			this.m_Autopilot = autoPilot;
			this.m_CameraAtOffset = cameraAtOffset;
			this.m_CameraEyeOffset = cameraEyeOffset;
			this.m_ForceMouselook = forceMouselook;
			this.m_SitPosition = sitPosition;
			this.m_SitRotation = sitRotation;
		}
	}
