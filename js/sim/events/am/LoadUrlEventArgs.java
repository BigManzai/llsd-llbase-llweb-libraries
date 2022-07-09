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
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Contains the data sent when a primitive sends a request 
	/// to an agent to open the specified URL</summary>
	public class LoadUrlEventArgs extends EventArgs
	{
		private  String m_ObjectName;
		private  UUID m_ObjectID;
		private  UUID m_OwnerID;
		private  boolean m_OwnerIsGroup;
		private  String m_Message;
		private  String m_URL;

		/// <summary>Get the name of the object sending the request</summary>
		public String getObjectName() { return m_ObjectName; } 
		/// <summary>Get the ID of the object sending the request</summary>
		public UUID getObjectID() { return m_ObjectID; } 
		/// <summary>Get the ID of the owner of the object sending the request</summary>
		public UUID getOwnerID() { return m_OwnerID; } 
		/// <summary>True if the object is owned by a group</summary>
		public boolean getOwnerIsGroup() { return m_OwnerIsGroup; } 
		/// <summary>Get the message sent with the request</summary>
		public String getMessage() { return m_Message; } 
		/// <summary>Get the URL the object sent</summary>
		public String getURL() { return m_URL; } 

		/// <summary>
		/// Construct a new instance of the LoadUrlEventArgs
		/// </summary>
		/// <param name="objectName">The name of the object sending the request</param>
		/// <param name="objectID">The ID of the object sending the request</param>
		/// <param name="ownerID">The ID of the owner of the object sending the request</param>
		/// <param name="ownerIsGroup">True if the object is owned by a group</param>
		/// <param name="message">The message sent with the request</param>
		/// <param name="URL">The URL the object sent</param>
		public LoadUrlEventArgs(String objectName, UUID objectID, UUID ownerID, boolean ownerIsGroup, String message, String URL)
		{
			this.m_ObjectName = objectName;
			this.m_ObjectID = objectID;
			this.m_OwnerID = ownerID;
			this.m_OwnerIsGroup = ownerIsGroup;
			this.m_Message = message;
			this.m_URL = URL;
		}
	}

