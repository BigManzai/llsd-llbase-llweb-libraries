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
import com.ngt.jopenmetaverse.shared.protocol.primitives.TextureEntryFace;
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
    ///     void Avatars_AvatarAppearance(Object sender, AvatarAppearanceEventArgs e)
    ///     {
    ///         Console.WriteLine("The Agent {0} is using a {1} shape.", e.AvatarID, (e.VisualParams[31] &gt; 0) : "male" ? "female")
    ///     }
    /// </code>
    /// </example>
    public class AvatarAppearanceEventArgs extends EventArgs
    {

        private Simulator m_Simulator;
        private UUID m_AvatarID;
        private boolean m_IsTrial;
        private TextureEntryFace m_DefaultTexture;
        private TextureEntryFace[] m_FaceTextures;
        private List<Byte> m_VisualParams;

        /// <summary>Get the Simulator this request is from of the agent</summary>
        public Simulator getSimulator() {return m_Simulator;} 
        /// <summary>Get the ID of the agent</summary>
        public UUID getAvatarID() {return m_AvatarID;} 
        /// <summary>true if the agent is a trial account</summary>
        public boolean getIsTrial() {return m_IsTrial;} 
        /// <summary>Get the default agent texture</summary>
        public TextureEntryFace getDefaultTexture() {return m_DefaultTexture; }
        /// <summary>Get the agents appearance layer textures</summary>
        public TextureEntryFace[] getFaceTextures() {return m_FaceTextures; }
        /// <summary>Get the <see cref="VisualParams"/> for the agent</summary>
        public List<Byte> getVisualParams() {return m_VisualParams; }

        /// <summary>
        /// Construct a new instance of the AvatarAppearanceEventArgs class
        /// </summary>
        /// <param name="sim">The simulator request was from</param>
        /// <param name="avatarID">The ID of the agent</param>
        /// <param name="isTrial">true of the agent is a trial account</param>
        /// <param name="defaultTexture">The default agent texture</param>
        /// <param name="faceTextures">The agents appearance layer textures</param>
        /// <param name="visualParams">The <see cref="VisualParams"/> for the agent</param>
        public AvatarAppearanceEventArgs(Simulator sim, UUID avatarID, boolean isTrial, TextureEntryFace defaultTexture,
            TextureEntryFace[] faceTextures, List<Byte> visualParams)
        {
            this.m_Simulator = sim;
            this.m_AvatarID = avatarID;
            this.m_IsTrial = isTrial;
            this.m_DefaultTexture = defaultTexture;
            this.m_FaceTextures = faceTextures;
            this.m_VisualParams = visualParams;
        }
    }