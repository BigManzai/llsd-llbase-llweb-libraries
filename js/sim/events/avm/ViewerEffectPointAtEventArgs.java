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

import com.ngt.jopenmetaverse.shared.sim.AgentManager.PointAtType;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3d;

  public class ViewerEffectPointAtEventArgs extends EventArgs
    {
        private Simulator m_Simulator;
        private UUID m_SourceID;
        private UUID m_TargetID;
        private Vector3d m_TargetPosition;
        private PointAtType m_PointType;
        private float m_Duration;
        private UUID m_EffectID;

        public Simulator getSimulator() {return m_Simulator; }
        public UUID getSourceID() {return m_SourceID; }
        public UUID getTargetID() {return m_TargetID; }
        public Vector3d getTargetPosition() {return m_TargetPosition; }
        public PointAtType getPointType() {return m_PointType; }
        public float getDuration() {return m_Duration; }
        public UUID getEffectID() {return m_EffectID; }

        public ViewerEffectPointAtEventArgs(Simulator simulator, UUID sourceID, UUID targetID, Vector3d targetPos, PointAtType pointType, float duration, UUID id)
        {
            this.m_Simulator = simulator;
            this.m_SourceID = sourceID;
            this.m_TargetID = targetID;
            this.m_TargetPosition = targetPos;
            this.m_PointType = pointType;
            this.m_Duration = duration;
            this.m_EffectID = id;
        }
    }