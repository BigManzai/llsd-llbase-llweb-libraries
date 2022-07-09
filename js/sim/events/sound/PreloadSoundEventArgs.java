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


/// <summary>Provides data for the <see cref="AvatarManager.AvatarAppearance"/> event</summary>
/// <remarks>The <see cref="AvatarManager.AvatarAppearance"/> event occurs when the simulator sends
/// the appearance data for an avatar</remarks>
/// <example>
/// The following code example uses the <see cref="AvatarAppearanceEventArgs.AvatarID"/> and <see cref="AvatarAppearanceEventArgs.VisualParams"/>
/// properties to display the selected shape of an avatar on the <see cref="Console"/> window.
/// <code>
///     // subscribe to the event
///     Client.Avatars.AvatarAppearance += Avatars_AvatarAppearance;
/// 
///     // handle the data when the event is raised
///     void Avatars_AvatarAppearance(object sender, AvatarAppearanceEventArgs e)
///     {
///         Console.WriteLine("The Agent {0} is using a {1} shape.", e.AvatarID, (e.VisualParams[31] &gt; 0) : "male" ? "female")
///     }
/// </code>
/// </example>

public class PreloadSoundEventArgs extends EventArgs
  {
      private  Simulator m_Simulator;
      private  UUID m_SoundID;
      private  UUID m_OwnerID;
      private  UUID m_ObjectID;

      /// <summary>Simulator where the event originated</summary>
      public Simulator getSimulator() {return m_Simulator; }
      /// <summary>Get the sound asset id</summary>
      public UUID getSoundID() {return m_SoundID; }
      /// <summary>Get the ID of the owner</summary>
      public UUID getOwnerID() {return m_OwnerID; }
      /// <summary>Get the ID of the Object</summary>
      public UUID getObjectID() {return m_ObjectID; }

      /// <summary>
      /// Construct a new instance of the PreloadSoundEventArgs class
      /// </summary>
      /// <param name="sim">Simulator where the event originated</param>
      /// <param name="soundID">The sound asset id</param>
      /// <param name="ownerID">The ID of the owner</param>
      /// <param name="objectID">The ID of the object</param>
      public PreloadSoundEventArgs(Simulator sim, UUID soundID, UUID ownerID, UUID objectID)
      {
          this.m_Simulator = sim;
          this.m_SoundID = soundID;
          this.m_OwnerID = ownerID;
          this.m_ObjectID = objectID;
      }
  }