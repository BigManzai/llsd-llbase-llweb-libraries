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

import java.util.EnumSet;

import com.ngt.jopenmetaverse.shared.sim.AgentManager.ScriptSensorTypeFlags;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;

/// <summary>
/// Data containing script sensor requests which allow an agent to know the specific details
/// of a primitive sending script sensor requests
/// </summary>
public class ScriptSensorReplyEventArgs extends EventArgs
{
	private  UUID m_RequestorID;
	private  UUID m_GroupID;
	private  String m_Name;
	private  UUID m_ObjectID;
	private  UUID m_OwnerID;
	private  Vector3 m_Position;
	private  float m_Range;
	private  Quaternion m_Rotation;
	private  EnumSet<ScriptSensorTypeFlags> m_Type;
	private  Vector3 m_Velocity;

	/// <summary>Get the ID of the primitive sending the sensor</summary>
	public UUID getRequestorID() {return m_RequestorID;}
	/// <summary>Get the ID of the group associated with the primitive</summary>
	public UUID getGroupID() {return m_GroupID;}
	/// <summary>Get the name of the primitive sending the sensor</summary>
	public String getName() {return m_Name;}
	/// <summary>Get the ID of the primitive sending the sensor</summary>
	public UUID getObjectID() {return m_ObjectID;}
	/// <summary>Get the ID of the owner of the primitive sending the sensor</summary>
	public UUID getOwnerID() {return m_OwnerID;}
	/// <summary>Get the position of the primitive sending the sensor</summary>
	public Vector3 getPosition() {return m_Position;}
	/// <summary>Get the range the primitive specified to scan</summary>
	public float getRange() {return m_Range;}
	/// <summary>Get the rotation of the primitive sending the sensor</summary>
	public Quaternion getRotation() {return m_Rotation;}
	/// <summary>Get the type of sensor the primitive sent</summary>
	public EnumSet<ScriptSensorTypeFlags> getType() {return m_Type;}
	/// <summary>Get the velocity of the primitive sending the sensor</summary>
	public Vector3 getVelocity() {return m_Velocity;}

	/// <summary>
	/// Construct a new instance of the ScriptSensorReplyEventArgs
	/// </summary>
	/// <param name="requestorID">The ID of the primitive sending the sensor</param>
	/// <param name="groupID">The ID of the group associated with the primitive</param>
	/// <param name="name">The name of the primitive sending the sensor</param>
	/// <param name="objectID">The ID of the primitive sending the sensor</param>
	/// <param name="ownerID">The ID of the owner of the primitive sending the sensor</param>
	/// <param name="position">The position of the primitive sending the sensor</param>
	/// <param name="range">The range the primitive specified to scan</param>
	/// <param name="rotation">The rotation of the primitive sending the sensor</param>
	/// <param name="type">The type of sensor the primitive sent</param>
	/// <param name="velocity">The velocity of the primitive sending the sensor</param>
	public ScriptSensorReplyEventArgs(UUID requestorID, UUID groupID, String name,
			UUID objectID, UUID ownerID, Vector3 position, float range, Quaternion rotation,
			EnumSet<ScriptSensorTypeFlags> type, Vector3 velocity)
	{
		this.m_RequestorID = requestorID;
		this.m_GroupID = groupID;
		this.m_Name = name;
		this.m_ObjectID = objectID;
		this.m_OwnerID = ownerID;
		this.m_Position = position;
		this.m_Range = range;
		this.m_Rotation = rotation;
		this.m_Type = type;
		this.m_Velocity = velocity;
	}
}

