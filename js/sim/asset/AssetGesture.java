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
package com.ngt.jopenmetaverse.shared.sim.asset;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.ngt.jopenmetaverse.shared.sim.AssetManager.TransferError;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
 /// <summary>
    /// Represents a sequence of animations, sounds, and chat actions
    /// </summary>
    public class AssetGesture extends Asset
    {
    	
        //region Enums
        /// <summary>
        /// Type of gesture step
        /// </summary>
        public enum GestureStepType 
        {
            Animation(0),
            Sound(1),
            Chat(2),
            Wait(3),
            EOF(4);
            private int index;
            GestureStepType(int index)
    		{
    			this.index = index;
    		}     

    		public int getIndex()
    		{
    			return index;
    		}

    		private static final Map<Integer,GestureStepType> lookup  
    		= new HashMap<Integer,GestureStepType>();

    		static {
    			for(GestureStepType s : EnumSet.allOf(GestureStepType.class))
    				lookup.put(s.getIndex(), s);
    		}

    		public static GestureStepType get(Integer index)
    		{
    			return lookup.get(index);
    		}
        }
        //endregion

        //region Gesture step classes
        /// <summary>
        /// Base class for gesture steps
        /// </summary>
        public abstract class GestureStep
        {
            /// <summary>
            /// Retururns what kind of gesture step this is
            /// </summary>
            public abstract GestureStepType getGestureStepType();
        }

        /// <summary>
        /// Describes animation step of a gesture
        /// </summary>
        public class GestureStepAnimation extends GestureStep
        {
            /// <summary>
            /// Returns what kind of gesture step this is
            /// </summary>
        	@Override
            public  GestureStepType getGestureStepType()
            {
                return GestureStepType.Animation; 
            }

            /// <summary>
            /// If true, this step represents start of animation, otherwise animation stop
            /// </summary>
            public boolean AnimationStart = true;

            /// <summary>
            /// Animation asset <see cref="UUID"/>
            /// </summary>
            public UUID ID;

            /// <summary>
            /// Animation inventory name
            /// </summary>
            public String Name;

            @Override
            public String toString()
            {
                if (AnimationStart)
                {
                    return "Start animation: " + Name;
                }
                else
                {
                    return "Stop animation: " + Name;
                }
            }
        }

        /// <summary>
        /// Describes sound step of a gesture
        /// </summary>
        public class GestureStepSound extends GestureStep
        {
            /// <summary>
            /// Returns what kind of gesture step this is
            /// </summary>
            @Override
	public GestureStepType getGestureStepType()
            {
                return GestureStepType.Sound; 
            }

            /// <summary>
            /// Sound asset <see cref="UUID"/>
            /// </summary>
            public UUID ID;

            /// <summary>
            /// Sound inventory name
            /// </summary>
            public String Name;

            @Override
	public String toString()
            {
                return "Sound: " + Name;
            }

        }

        /// <summary>
        /// Describes sound step of a gesture
        /// </summary>
        public class GestureStepChat extends GestureStep
        {
            /// <summary>
            /// Returns what kind of gesture step this is
            /// </summary>
            @Override
	public GestureStepType getGestureStepType()
            {
                return GestureStepType.Chat; 
            }

            /// <summary>
            /// Text to output in chat
            /// </summary>
            public String Text;

            @Override
	public String toString()
            {
                return "Chat: " + Text;
            }
        }

        /// <summary>
        /// Describes sound step of a gesture
        /// </summary>
        public class GestureStepWait extends GestureStep
        {
            /// <summary>
            /// Returns what kind of gesture step this is
            /// </summary>
            @Override
	public GestureStepType getGestureStepType()
            {
                return GestureStepType.Wait; 
            }

            /// <summary>
            /// If true in this step we wait for all animations to finish
            /// </summary>
            public boolean WaitForAnimation;

            /// <summary>
            /// If true gesture player should wait for the specified amount of time
            /// </summary>
            public boolean WaitForTime;

            /// <summary>
            /// Time in seconds to wait if WaitForAnimation is false
            /// </summary>
            public float WaitTime;

            @Override
	public String  toString()
            {
                StringBuilder ret = new StringBuilder("-- Wait for: ");

                if (WaitForAnimation)
                {
                    ret.append("(animations to finish) ");
                }

                if (WaitForTime)
                {
                    ret.append(String.format("(time %s)", WaitTime));
                }

                return ret.toString();
            }
        }

        /// <summary>
        /// Describes the final step of a gesture
        /// </summary>
        public class GestureStepEOF extends GestureStep
        {
            /// <summary>
            /// Returns what kind of gesture step this is
            /// </summary>
            @Override
	public GestureStepType getGestureStepType()
            {
                return GestureStepType.EOF; 
            }

            @Override
	public String toString()
            {
                return "End of guesture sequence";
            }
        }

        //endregion
    	
    	
        /// <summary>
        /// Returns asset type
        /// </summary>
        @Override
	public AssetType getAssetType()
        {
            return AssetType.Gesture; 
        }

        /// <summary>
        /// Keyboard key that triggers the gestyre
        /// </summary>
        public byte TriggerKey;

        /// <summary>
        /// Modifier to the trigger key
        /// </summary>
        //uint
        public long TriggerKeyMask;

        /// <summary>
        /// String that triggers playing of the gesture sequence
        /// </summary>
        public String Trigger;

        /// <summary>
        /// Text that replaces trigger in chat once gesture is triggered
        /// </summary>
        public String ReplaceWith;

        /// <summary>
        /// Sequence of gesture steps
        /// </summary>
        public List<GestureStep> Sequence;

        /// <summary>
        /// Constructs guesture asset
        /// </summary>
        public AssetGesture() { }

        /// <summary>
        /// Constructs guesture asset
        /// </summary>
        /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
        /// <param name="assetData">A byte array containing the raw asset data</param>
        public AssetGesture(UUID assetID, byte[] assetData)
        {
        	super(assetID, assetData);
        }

        /// <summary>
        /// Encodes gesture asset suitable for uplaod
        /// </summary>
        @Override
	public void Encode()
        {
            StringBuilder sb = new StringBuilder();
            sb.append("2\n");
            sb.append(TriggerKey + "\n");
            sb.append(TriggerKeyMask + "\n");
            sb.append(Trigger + "\n");
            sb.append(ReplaceWith + "\n");

            int count = 0;
            if (Sequence != null)
            {
                count = Sequence.size();
            }

            sb.append(count + "\n");

            boolean continueDecoding = true;
            for (int i = 0; i < count && continueDecoding; i++)
            {
                GestureStep step = Sequence.get(i);
                sb.append((int)step.getGestureStepType().getIndex() + "\n");

                switch (step.getGestureStepType())
                {
                    case EOF:
                    	continueDecoding = false;
                    	break;
                    case Animation:
                        GestureStepAnimation animstep = (GestureStepAnimation)step;
                        sb.append(animstep.Name + "\n");
                        sb.append(animstep.ID + "\n");

                        if (animstep.AnimationStart)
                        {
                            sb.append("0\n");
                        }
                        else
                        {
                            sb.append("1\n");
                        }
                        break;

                    case Sound:
                        GestureStepSound soundstep = (GestureStepSound)step;
                        sb.append(soundstep.Name + "\n");
                        sb.append(soundstep.ID + "\n");
                        sb.append("0\n");
                        break;

                    case Chat:
                        GestureStepChat chatstep = (GestureStepChat)step;
                        sb.append(chatstep.Text + "\n");
                        sb.append("0\n");
                        break;

                    case Wait:
                        GestureStepWait waitstep = (GestureStepWait)step;
                        sb.append(String.format("{%f}\n", waitstep.WaitTime));
                        int waitflags = 0;

                        if (waitstep.WaitForTime)
                        {
                            waitflags |= 0x01;
                        }

                        if (waitstep.WaitForAnimation)
                        {
                            waitflags |= 0x02;
                        }

                        sb.append(waitflags + "\n");
                        break;
                }
            }
            AssetData = Utils.stringToBytesWithTrailingNullByte(sb.toString());
        }

        /// <summary>
        /// Decodes gesture assset into play sequence
        /// </summary>
        /// <returns>true if the asset data was decoded successfully</returns>
        @Override
	public boolean Decode()
        {
            try
            {
                String[] lines = Utils.bytesWithTrailingNullByteToString(AssetData).split("\n");
                Sequence = new ArrayList<GestureStep>();

                int i = 0;

                // version
                int version = Integer.parseInt(lines[i++]);
                if (version != 2)
                {
                    throw new Exception("Only know how to decode version 2 of gesture asset");
                }

                TriggerKey = Byte.parseByte(lines[i++]);
                TriggerKeyMask = Long.parseLong(lines[i++]);
                Trigger = lines[i++];
                ReplaceWith = lines[i++];

                int count = Integer.parseInt(lines[i++]);

                if (count < 0)
                {
                    throw new Exception("Wrong number of gesture steps");
                }

                boolean continueDecoding = true;
                for (int n = 0; n < count && continueDecoding; n++)
                {
                    GestureStepType type = GestureStepType.get(Integer.parseInt(lines[i++]));

                    switch (type)
                    {
                        case EOF:
                        	continueDecoding = false;
                        	break;

                        case Animation:
                            {
                                GestureStepAnimation step = new GestureStepAnimation();
                                step.Name = lines[i++];
                                step.ID = new UUID(lines[i++]);
                                int flags = Integer.parseInt(lines[i++]);

                                if (flags == 0)
                                {
                                    step.AnimationStart = true;
                                }
                                else
                                {
                                    step.AnimationStart = false;
                                }

                                Sequence.add(step);
                                break;
                            }

                        case Sound:
                            {
                                GestureStepSound step = new GestureStepSound();
                                step.Name = lines[i++].replace("\r", "");
                                step.ID = new UUID(lines[i++]);
                                int flags = Integer.parseInt(lines[i++]);

                                Sequence.add(step);
                                break;
                            }

                        case Chat:
                            {
                                GestureStepChat step = new GestureStepChat();
                                step.Text = lines[i++];
                                int flags = Integer.parseInt(lines[i++]);

                                Sequence.add(step);
                                break;
                            }

                        case Wait:
                            {
                                GestureStepWait step = new GestureStepWait();
                                step.WaitTime = Float.parseFloat(lines[i++]);
                                int flags = Integer.parseInt(lines[i++]);

                                step.WaitForTime = (flags & 0x01) != 0;
                                step.WaitForAnimation = (flags & 0x02) != 0;
                                Sequence.add(step);
                                break;
                            }

                    }
                }
                return true;
            }
            catch (Exception ex)
            {
                JLogger.error("Decoding gesture asset failed:" + Utils.getExceptionStackTraceAsString(ex));
                return false;
            }
        }
    }