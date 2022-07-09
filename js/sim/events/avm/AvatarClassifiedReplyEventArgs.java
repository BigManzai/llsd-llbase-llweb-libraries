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

import java.util.Map;

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

 public class AvatarClassifiedReplyEventArgs extends EventArgs
    {
        private UUID m_AvatarID;
        private Map<UUID, String> m_Classifieds;

        /// <summary>Get the ID of the avatar</summary>
        public UUID getAvatarID() {return m_AvatarID; }
        public Map<UUID, String> getClassifieds() {return m_Classifieds; }

        public AvatarClassifiedReplyEventArgs(UUID avatarid, Map<UUID, String> classifieds)
        {
            this.m_AvatarID = avatarid;
            this.m_Classifieds = classifieds;
        }
    }