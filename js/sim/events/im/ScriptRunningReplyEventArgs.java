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
package com.ngt.jopenmetaverse.shared.sim.events.im;

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

public class ScriptRunningReplyEventArgs extends EventArgs
{
	private UUID m_ObjectID;
	private UUID m_ScriptID;
	private boolean m_IsMono;
	private boolean m_IsRunning;

	public UUID getObjectID() {return m_ObjectID;}

	public UUID getScriptID() {return m_ScriptID;}
	public boolean getIsMono() {return m_IsMono;}
	public boolean getIsRunning() {return m_IsRunning;}

	public ScriptRunningReplyEventArgs(UUID objectID, UUID sctriptID, boolean isMono, boolean isRunning)
	{
		this.m_ObjectID = objectID;
		this.m_ScriptID = sctriptID;
		this.m_IsMono = isMono;
		this.m_IsRunning = isRunning;
	}
}
