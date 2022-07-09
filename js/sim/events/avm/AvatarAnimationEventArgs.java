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
package com.ngt.jopenmetaverse.shared.sim.events.avm;

import java.util.List;

import com.ngt.jopenmetaverse.shared.sim.AvatarManager.Animation;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Provides data for the <see cref="AvatarManager.AvatarAnimation"/> event</summary>
/// <remarks>The <see cref="AvatarManager.AvatarAnimation"/> event occurs when the simulator sends
/// the animation playlist for an agent</remarks>
/// <example>
/// The following code example uses the <see cref="AvatarAnimationEventArgs.AvatarID"/> and <see cref="AvatarAnimationEventArgs.Animations"/>
/// properties to display the animation playlist of an avatar on the <see cref="Console"/> window.
/// <code>
///     // subscribe to the event
///     Client.Avatars.AvatarAnimation += Avatars_AvatarAnimation;
///     
///     private void Avatars_AvatarAnimation(Object sender, AvatarAnimationEventArgs e)
///     {
///         // create a dictionary of "known" animations from the Animations class using System.Reflection
///         Dictionary&lt;UUID, string&gt; systemAnimations = new Dictionary&lt;UUID, string&gt;();
///         Type type = typeof(Animations);
///         System.Reflection.FieldInfo[] fields = type.GetFields(System.Reflection.BindingFlags.Public | System.Reflection.BindingFlags.Static);
///         foreach (System.Reflection.FieldInfo field in fields)
///         {
///             systemAnimations.Add((UUID)field.GetValue(type), field.Name);
///         }
///
///         // find out which animations being played are known animations and which are assets
///         foreach (Animation animation in e.Animations)
///         {
///             if (systemAnimations.ContainsKey(animation.AnimationID))
///             {
///                 Console.WriteLine("{0} is playing {1} ({2}) sequence {3}", e.AvatarID,
///                     systemAnimations[animation.AnimationID], animation.AnimationSequence);
///             }
///             else
///             {
///                 Console.WriteLine("{0} is playing {1} (Asset) sequence {2}", e.AvatarID,
///                     animation.AnimationID, animation.AnimationSequence);
///             }
///         }
///     }
/// </code>
/// </example>
  public class AvatarAnimationEventArgs extends EventArgs
    {
        private UUID m_AvatarID;
        private List<Animation> m_Animations;

        /// <summary>Get the ID of the agent</summary>
        public UUID getAvatarID() {return m_AvatarID;} 
        /// <summary>Get the list of animations to start</summary>
        public List<Animation> getAnimations() {return m_Animations;} 

        /// <summary>
        /// Construct a new instance of the AvatarAnimationEventArgs class
        /// </summary>
        /// <param name="avatarID">The ID of the agent</param>
        /// <param name="anims">The list of animations to start</param>
        public AvatarAnimationEventArgs(UUID avatarID, List<Animation> anims)
        {
            this.m_AvatarID = avatarID;
            this.m_Animations = anims;
        }
    }