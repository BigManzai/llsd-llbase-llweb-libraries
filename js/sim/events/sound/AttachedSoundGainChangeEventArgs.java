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
package com.ngt.jopenmetaverse.shared.sim.events.sound;

import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Provides data for the <see cref="SoundManager.AttachedSoundGainChange"/> event</summary>
/// <remarks>The <see cref="SoundManager.AttachedSoundGainChange"/> event occurs when an attached sound
/// changes its volume level</remarks>
public class AttachedSoundGainChangeEventArgs extends EventArgs
  {
      private  Simulator m_Simulator;
      private  UUID m_ObjectID;
      private  float m_Gain;

      /// <summary>Simulator where the event originated</summary>
      public Simulator getSimulator() {return m_Simulator; }
      /// <summary>Get the ID of the Object</summary>
      public UUID getObjectID() {return m_ObjectID; }
      /// <summary>Get the volume level</summary>
      public float getGain() {return m_Gain; }

      /// <summary>
      /// Construct a new instance of the AttachedSoundGainChangedEventArgs class
      /// </summary>
      /// <param name="sim">Simulator where the event originated</param>
      /// <param name="objectID">The ID of the Object</param>
      /// <param name="gain">The new volume level</param>
      public AttachedSoundGainChangeEventArgs(Simulator sim, UUID objectID, float gain)
      {
          this.m_Simulator = sim;
          this.m_ObjectID = objectID;
          this.m_Gain = gain;
      }
  }
  