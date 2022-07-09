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
package com.ngt.jopenmetaverse.shared.sim.events.asm;

import java.util.SortedMap;
import java.util.TreeMap;

import com.ngt.jopenmetaverse.shared.sim.AssetManager.ChannelType;
import com.ngt.jopenmetaverse.shared.sim.AssetManager.SourceType;
import com.ngt.jopenmetaverse.shared.sim.AssetManager.StatusCode;
import com.ngt.jopenmetaverse.shared.sim.AssetManager.TargetType;
import com.ngt.jopenmetaverse.shared.sim.events.ManualResetEvent;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.sim.Simulator;

  /// <summary>
    /// 
    /// </summary>
    public class AssetDownload extends Transfer
    {
        public UUID AssetID;
        public ChannelType Channel;
        public SourceType Source;
        public TargetType Target;
        public StatusCode Status;
        public float Priority;
        public Simulator Simulator;
        public SortedMap<Integer, byte[]> pmap;
        public MethodDelegate<Void, AssetReceivedCallbackArgs> Callback;
        public ManualResetEvent HeaderReceivedEvent = new ManualResetEvent(false);

        public AssetDownload()
        {
           super();
           pmap = new TreeMap<Integer, byte[]>();
        }
    }

