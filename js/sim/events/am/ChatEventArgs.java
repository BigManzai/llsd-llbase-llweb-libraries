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

import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.ChatAudibleLevel;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.ChatSourceType;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.ChatType;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;

	/// <summary>
		/// 
		/// </summary>
		public class ChatEventArgs extends EventArgs
		{
			private  Simulator m_Simulator;
			private  String m_Message;
			private  ChatAudibleLevel m_AudibleLevel;
			private  ChatType m_Type;
			private  ChatSourceType m_SourceType;
			private  String m_FromName;
			private  UUID m_SourceID;
			private  UUID m_OwnerID;
			private  Vector3 m_Position;

			/// <summary>Get the simulator sending the message</summary>
			public Simulator getSimulator() {  return m_Simulator; } 
			/// <summary>Get the message sent</summary>
			public String getMessage() { return m_Message; } 
			/// <summary>Get the audible level of the message</summary>
			public ChatAudibleLevel getAudibleLevel() {  return m_AudibleLevel; } 
			/// <summary>Get the type of message sent: whisper, shout, etc</summary>
			public ChatType getType() { return m_Type; } 
			/// <summary>Get the source type of the message sender</summary>
			public ChatSourceType getSourceType() { return m_SourceType; } 
			/// <summary>Get the name of the agent or object sending the message</summary>
			public String getFromName() { return m_FromName; } 
			/// <summary>Get the ID of the agent or object sending the message</summary>
			public UUID getSourceID() { return m_SourceID; } 
			/// <summary>Get the ID of the object owner, or the agent ID sending the message</summary>
			public UUID getOwnerID() { return m_OwnerID; } 
			/// <summary>Get the position of the agent or object sending the message</summary>
			public Vector3 getPosition() { return m_Position; } 

			/// <summary>
			/// Construct a new instance of the ChatEventArgs object
			/// </summary>
			/// <param name="simulator">Sim from which the message originates</param>
			/// <param name="message">The message sent</param>
			/// <param name="audible">The audible level of the message</param>
			/// <param name="type">The type of message sent: whisper, shout, etc</param>
			/// <param name="sourceType">The source type of the message sender</param>
			/// <param name="fromName">The name of the agent or object sending the message</param>
			/// <param name="sourceId">The ID of the agent or object sending the message</param>
			/// <param name="ownerid">The ID of the object owner, or the agent ID sending the message</param>
			/// <param name="position">The position of the agent or object sending the message</param>
			public ChatEventArgs(Simulator simulator, String message, ChatAudibleLevel audible, ChatType type,
					ChatSourceType sourceType, String fromName, UUID sourceId, UUID ownerid, Vector3 position)
			{
				this.m_Simulator = simulator;
				this.m_Message = message;
				this.m_AudibleLevel = audible;
				this.m_Type = type;
				this.m_SourceType = sourceType;
				this.m_FromName = fromName;
				this.m_SourceID = sourceId;
				this.m_Position = position;
				this.m_OwnerID = ownerid;
			}
		}
