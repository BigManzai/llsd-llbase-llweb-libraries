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

import com.ngt.jopenmetaverse.shared.sim.InternalDictionary;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Data sent by the simulator to indicate the active/changed animations
	/// applied to your agent</summary>
	public class AnimationsChangedEventArgs extends EventArgs
	{
		private  InternalDictionary<UUID, Integer> m_Animations;

		/// <summary>Get the dictionary that contains the changed animations</summary>
		public InternalDictionary<UUID, Integer> getAnimations() {return m_Animations;}

		/// <summary>
		/// Construct a new instance of the AnimationsChangedEventArgs class
		/// </summary>
		/// <param name="agentAnimations">The dictionary that contains the changed animations</param>
		public AnimationsChangedEventArgs(InternalDictionary<UUID, Integer> agentAnimations)
		{
			this.m_Animations = agentAnimations;
		}

	}
