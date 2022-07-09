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
package com.ngt.jopenmetaverse.shared.protocol.primitives;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.protocol.BitPack;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;


    /// <summary>
        /// Complete structure for the particle system
        /// </summary>
        public class ParticleSystem
        {
            /// <summary>
            /// Particle source pattern
            /// </summary>
            public enum SourcePattern
            {
                /// <summary>None</summary>
                None ((byte)0),
                /// <summary>Drop particles from source position with no force</summary>
                Drop ((byte)0x01),
                /// <summary>"Explode" particles in all directions</summary>
                Explode ((byte)0x02),
                /// <summary>Particles shoot across a 2D area</summary>
                Angle ((byte)0x04),
                /// <summary>Particles shoot across a 3D Cone</summary>
                AngleCone ((byte)0x08),
                /// <summary>Inverse of AngleCone (shoot particles everywhere except the 3D cone defined</summary>
                AngleConeEmpty ((byte)0x10);
          		private byte index;
          		SourcePattern(byte index)
        		{
        			this.index = index;
        		}     

        		public byte getIndex()
        		{
        			return index;
        		}
        		
        		private static final Map<Byte,SourcePattern> lookup  = new HashMap<Byte,SourcePattern>();

        		static {
        			for(SourcePattern s : EnumSet.allOf(SourcePattern.class))
        				lookup.put(s.getIndex(), s);
        		}

        		public static SourcePattern get(Byte index)
        		{
        			return lookup.get(index);
        		}
            }

            /// <summary>
            /// Particle Data Flags
            /// </summary>
            //.get(Flags)
            public enum ParticleDataFlags 
            {
                /// <summary>None</summary>
                None (0L),
                /// <summary>Interpolate color and alpha from start to end</summary>
                InterpColor (0x001L),
                /// <summary>Interpolate scale from start to end</summary>
                InterpScale (0x002L),
                /// <summary>Bounce particles off particle sources Z height</summary>
                Bounce (0x004L),
                /// <summary>velocity of particles is dampened toward the simulators wind</summary>
                Wind (0x008L),
                /// <summary>Particles follow the source</summary>
                FollowSrc (0x010L),
                /// <summary>Particles point towards the direction of source's velocity</summary>
                FollowVelocity (0x020L),
                /// <summary>Target of the particles</summary>
                TargetPos (0x040L),
                /// <summary>Particles are sent in a straight line</summary>
                TargetLinear (0x080L),
                /// <summary>Particles emit a glow</summary>
                Emissive (0x100L),
                /// <summary>used for point/grab/touch</summary>
                Beam (0x200L);
                
          		private long index;
          		ParticleDataFlags(long index)
        		{
        			this.index = index;
        		}     

        		public long getIndex()
        		{
        			return index;
        		}
        		
        		private static final Map<Long,ParticleDataFlags> lookup  = new HashMap<Long,ParticleDataFlags>();

        		static {
        			for(ParticleDataFlags s : EnumSet.allOf(ParticleDataFlags.class))
        				lookup.put(s.getIndex(), s);
        		}

                public static EnumSet<ParticleDataFlags> get(Long index)
                {
                        EnumSet<ParticleDataFlags> enumsSet = EnumSet.allOf(ParticleDataFlags.class);
                        for(Entry<Long,ParticleDataFlags> entry: lookup.entrySet())
                        {
                                if((entry.getKey().longValue() | index) != index)
                                {
                                        enumsSet.remove(entry.getValue());
                                }
                        }
                        return enumsSet;
                }
                
                public static long getIndex(EnumSet<ParticleDataFlags> enumSet)
                {
                        long ret = 0;
                        for(ParticleDataFlags s: enumSet)
                        {
                                ret |= s.getIndex();
                        }
                        return ret;
                }

            }

            /// <summary>
            /// Particle Flags Enum
            /// </summary>
            //.get(Flags)
            public enum ParticleFlags
            {
                /// <summary>None</summary>
                None (0L),
                /// <summary>Acceleration and velocity for particles are
                /// relative to the object rotation</summary>
                ObjectRelative (0x01L),
                /// <summary>Particles use new 'correct' angle parameters</summary>
                UseNewAngle (0x02L);
                
          		private long index;
          		ParticleFlags(long index)
        		{
        			this.index = index;
        		}     

        		public long getIndex()
        		{
        			return index;
        		}
        		
        		private static final Map<Long,ParticleFlags> lookup  = new HashMap<Long,ParticleFlags>();

        		static {
        			for(ParticleFlags s : EnumSet.allOf(ParticleFlags.class))
        				lookup.put(s.getIndex(), s);
        		}

                public static EnumSet<ParticleFlags> get(Long index)
                {
                        EnumSet<ParticleFlags> enumsSet = EnumSet.allOf(ParticleFlags.class);
                        for(Entry<Long,ParticleFlags> entry: lookup.entrySet())
                        {
                                if((entry.getKey().longValue() | index) != index)
                                {
                                        enumsSet.remove(entry.getValue());
                                }
                        }
                        return enumsSet;
                }
                
                public static long getIndex(EnumSet<ParticleFlags> enumSet)
                {
                        long ret = 0;
                        for(ParticleFlags s: enumSet)
                        {
                                ret |= s.getIndex();
                        }
                        return ret;
                }

            }


            public long CRC;
            /// <summary>Particle Flags</summary>
            /// <remarks>There appears to be more data packed in to this area
            /// for many particle systems. It doesn't appear to be flag values
            /// and serialization breaks unless there is a flag for every
            /// possible bit so it is left as an unsigned integer</remarks>
            public long PartFlags;
            /// <summary><seealso cref="T:SourcePattern"/> pattern of particles</summary>
            public SourcePattern Pattern;
            /// <summary>A <see langword="float"/> representing the maximimum age (in seconds) particle will be displayed</summary>
            /// <remarks>Maximum value is 30 seconds</remarks>
            public float MaxAge;
            /// <summary>A <see langword="float"/> representing the number of seconds, 
            /// from when the particle source comes into view, 
            /// or the particle system's creation, that the object will emits particles; 
            /// after this time period no more particles are emitted</summary>
            public float StartAge;
            /// <summary>A <see langword="float"/> in radians that specifies where particles will not be created</summary>
            public float InnerAngle;
            /// <summary>A <see langword="float"/> in radians that specifies where particles will be created</summary>
            public float OuterAngle;
            /// <summary>A <see langword="float"/> representing the number of seconds between burts.</summary>
            public float BurstRate;
            /// <summary>A <see langword="float"/> representing the number of meters
            /// around the center of the source where particles will be created.</summary>
            public float BurstRadius;
            /// <summary>A <see langword="float"/> representing in seconds, the minimum speed between bursts of new particles 
            /// being emitted</summary>
            public float BurstSpeedMin;
            /// <summary>A <see langword="float"/> representing in seconds the maximum speed of new particles being emitted.</summary>
            public float BurstSpeedMax;
            /// <summary>A <see langword="byte"/> representing the maximum number of particles emitted per burst</summary>
            public byte BurstPartCount;
            /// <summary>A <see cref="T:Vector3"/> which represents the velocity (speed) from the source which particles are emitted</summary>
            public Vector3 AngularVelocity;
            /// <summary>A <see cref="T:Vector3"/> which represents the Acceleration from the source which particles are emitted</summary>
            public Vector3 PartAcceleration;
            /// <summary>The <see cref="T:UUID"/> Key of the texture displayed on the particle</summary>
            public UUID Texture;
            /// <summary>The <see cref="T:UUID"/> Key of the specified target object or avatar particles will follow</summary>
            public UUID Target;
            /// <summary>Flags of particle from <seealso cref="T:ParticleDataFlags"/></summary>
            public EnumSet<ParticleDataFlags> PartDataFlags;
            /// <summary>Max Age particle system will emit particles for</summary>
            public float PartMaxAge;
            /// <summary>The <see cref="T:Color4"/> the particle has at the beginning of its lifecycle</summary>
            public Color4 PartStartColor;
            /// <summary>The <see cref="T:Color4"/> the particle has at the ending of its lifecycle</summary>
            public Color4 PartEndColor;
            /// <summary>A <see langword="float"/> that represents the starting X size of the particle</summary>
            /// <remarks>Minimum value is 0, maximum value is 4</remarks>
            public float PartStartScaleX;
            /// <summary>A <see langword="float"/> that represents the starting Y size of the particle</summary>
            /// <remarks>Minimum value is 0, maximum value is 4</remarks>
            public float PartStartScaleY;
            /// <summary>A <see langword="float"/> that represents the ending X size of the particle</summary>
            /// <remarks>Minimum value is 0, maximum value is 4</remarks>
            public float PartEndScaleX;
            /// <summary>A <see langword="float"/> that represents the ending Y size of the particle</summary>
            /// <remarks>Minimum value is 0, maximum value is 4</remarks>
            public float PartEndScaleY;

            public ParticleSystem()
            {
            	this(new byte[0], 0);
            }
            
            /// <summary>
            /// Decodes a byte[] array into a ParticleSystem Object
            /// </summary>
            /// <param name="data">ParticleSystem object</param>
            /// <param name="pos">Start position for BitPacker</param>
            public ParticleSystem(byte[] data, int pos)
            {
                // TODO: Not sure exactly how many bytes we need here, so partial 
                // (truncated) data will cause an exception to be thrown
                if (data.length > 0)
                {
                    BitPack pack = new BitPack(data, pos);

                    CRC = pack.UnpackLBits(32);
                    PartFlags = pack.UnpackLBits(32);
                    Pattern = SourcePattern.get(pack.UnpackByte());
                    MaxAge = pack.UnpackFixed(false, 8, 8);
                    StartAge = pack.UnpackFixed(false, 8, 8);
                    InnerAngle = pack.UnpackFixed(false, 3, 5);
                    OuterAngle = pack.UnpackFixed(false, 3, 5);
                    BurstRate = pack.UnpackFixed(false, 8, 8);
                    BurstRadius = pack.UnpackFixed(false, 8, 8);
                    BurstSpeedMin = pack.UnpackFixed(false, 8, 8);
                    BurstSpeedMax = pack.UnpackFixed(false, 8, 8);
                    BurstPartCount = pack.UnpackByte();
                    float x = pack.UnpackFixed(true, 8, 7);
                    float y = pack.UnpackFixed(true, 8, 7);
                    float z = pack.UnpackFixed(true, 8, 7);
                    AngularVelocity = new Vector3(x, y, z);
                    x = pack.UnpackFixed(true, 8, 7);
                    y = pack.UnpackFixed(true, 8, 7);
                    z = pack.UnpackFixed(true, 8, 7);
                    PartAcceleration = new Vector3(x, y, z);
                    Texture = pack.UnpackUUID();
                    Target = pack.UnpackUUID();

                    PartDataFlags = ParticleDataFlags.get(pack.UnpackLBits(32));
                    PartMaxAge = pack.UnpackFixed(false, 8, 8);
                    byte r = pack.UnpackByte();
                    byte g = pack.UnpackByte();
                    byte b = pack.UnpackByte();
                    byte a = pack.UnpackByte();
                    PartStartColor = new Color4(r, g, b, a);
                    r = pack.UnpackByte();
                    g = pack.UnpackByte();
                    b = pack.UnpackByte();
                    a = pack.UnpackByte();
                    PartEndColor = new Color4(r, g, b, a);
                    PartStartScaleX = pack.UnpackFixed(false, 3, 5);
                    PartStartScaleY = pack.UnpackFixed(false, 3, 5);
                    PartEndScaleX = pack.UnpackFixed(false, 3, 5);
                    PartEndScaleY = pack.UnpackFixed(false, 3, 5);
                }
                else
                {
                    CRC = PartFlags = 0;
                    Pattern = SourcePattern.None;
                    MaxAge = StartAge = InnerAngle = OuterAngle = BurstRate = BurstRadius = BurstSpeedMin =
                        BurstSpeedMax = 0.0f;
                    BurstPartCount = 0;
                    AngularVelocity = PartAcceleration = Vector3.Zero;
                    Texture = Target = UUID.Zero;
                    PartDataFlags = ParticleDataFlags.get(ParticleDataFlags.None.getIndex());
                    PartMaxAge = 0.0f;
                    PartStartColor = PartEndColor = Color4.Black;
                    PartStartScaleX = PartStartScaleY = PartEndScaleX = PartEndScaleY = 0.0f;
                }
            }

            /// <summary>
            /// Generate byte[] array from particle data
            /// </summary>
            /// <returns>Byte array</returns>
            public byte[] GetBytes() throws Exception
            {
                byte[] bytes = new byte[86];
                BitPack pack = new BitPack(bytes, 0);

                pack.PackBits(CRC, 32);
                pack.PackBits(PartFlags & 0xffffffff, 32);
                pack.PackBits(Pattern.getIndex() & 0xff, 8);
                pack.PackFixed(MaxAge, false, 8, 8);
                pack.PackFixed(StartAge, false, 8, 8);
                pack.PackFixed(InnerAngle, false, 3, 5);
                pack.PackFixed(OuterAngle, false, 3, 5);
                pack.PackFixed(BurstRate, false, 8, 8);
                pack.PackFixed(BurstRadius, false, 8, 8);
                pack.PackFixed(BurstSpeedMin, false, 8, 8);
                pack.PackFixed(BurstSpeedMax, false, 8, 8);
                pack.PackBits(BurstPartCount, 8);
                pack.PackFixed(AngularVelocity.X, true, 8, 7);
                pack.PackFixed(AngularVelocity.Y, true, 8, 7);
                pack.PackFixed(AngularVelocity.Z, true, 8, 7);
                pack.PackFixed(PartAcceleration.X, true, 8, 7);
                pack.PackFixed(PartAcceleration.Y, true, 8, 7);
                pack.PackFixed(PartAcceleration.Z, true, 8, 7);
                pack.PackUUID(Texture);
                pack.PackUUID(Target);

                pack.PackBits(ParticleDataFlags.getIndex(PartDataFlags), 32);
                pack.PackFixed(PartMaxAge, false, 8, 8);
                pack.PackColor(PartStartColor);
                pack.PackColor(PartEndColor);
                pack.PackFixed(PartStartScaleX, false, 3, 5);
                pack.PackFixed(PartStartScaleY, false, 3, 5);
                pack.PackFixed(PartEndScaleX, false, 3, 5);
                pack.PackFixed(PartEndScaleY, false, 3, 5);

                return bytes;
            }

            public OSD GetOSD()
            {
                OSDMap map = new OSDMap();

                map.put("crc", OSD.FromLong(CRC));
                map.put("part_flags", OSD.FromLong(PartFlags));
                map.put("pattern", OSD.FromInteger((int)Pattern.getIndex()));
                map.put("max_age", OSD.FromReal(MaxAge));
                map.put("start_age", OSD.FromReal(StartAge));
                map.put("inner_angle", OSD.FromReal(InnerAngle));
                map.put("outer_angle", OSD.FromReal(OuterAngle));
                map.put("burst_rate", OSD.FromReal(BurstRate));
                map.put("burst_radius", OSD.FromReal(BurstRadius));
                map.put("burst_speed_min", OSD.FromReal(BurstSpeedMin));
                map.put("burst_speed_max", OSD.FromReal(BurstSpeedMax));
                map.put("burst_part_count", OSD.FromInteger(BurstPartCount));
                map.put("ang_velocity", OSD.FromVector3(AngularVelocity));
                map.put("part_acceleration", OSD.FromVector3(PartAcceleration));
                map.put("texture", OSD.FromUUID(Texture));
                map.put("target", OSD.FromUUID(Target));

                return map;
            }

            public static ParticleSystem FromOSD(OSD osd)
            {
                ParticleSystem partSys = new ParticleSystem();
                OSDMap map = (OSDMap) osd;

                if (map != null)
                {
                    partSys.CRC = map.get("crc").asLong();
                    partSys.PartFlags = map.get("part_flags").asLong();
                    partSys.Pattern = SourcePattern.get((byte)map.get("pattern").asInteger());
                    partSys.MaxAge = (float)map.get("max_age").asReal();
                    partSys.StartAge = (float)map.get("start_age").asReal();
                    partSys.InnerAngle = (float)map.get("inner_angle").asReal();
                    partSys.OuterAngle = (float)map.get("outer_angle").asReal();
                    partSys.BurstRate = (float)map.get("burst_rate").asReal();
                    partSys.BurstRadius = (float)map.get("burst_radius").asReal();
                    partSys.BurstSpeedMin = (float)map.get("burst_speed_min").asReal();
                    partSys.BurstSpeedMax = (float)map.get("burst_speed_max").asReal();
                    partSys.BurstPartCount = (byte)map.get("burst_part_count").asInteger();
                    partSys.AngularVelocity = map.get("ang_velocity").asVector3();
                    partSys.PartAcceleration = map.get("part_acceleration").asVector3();
                    partSys.Texture = map.get("texture").asUUID();
                    partSys.Target = map.get("target").asUUID();
                }

                return partSys;
            }
        }

        //endregion Subclasses