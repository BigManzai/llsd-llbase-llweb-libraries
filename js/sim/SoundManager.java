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
package com.ngt.jopenmetaverse.shared.sim;

import java.math.BigInteger;
import java.util.Observable;

import com.ngt.jopenmetaverse.shared.protocol.AttachedSoundGainChangePacket;
import com.ngt.jopenmetaverse.shared.protocol.AttachedSoundPacket;
import com.ngt.jopenmetaverse.shared.protocol.PacketType;
import com.ngt.jopenmetaverse.shared.protocol.PreloadSoundPacket;
import com.ngt.jopenmetaverse.shared.protocol.SoundTriggerPacket;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.PacketReceivedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.sound.*;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.SoundFlags;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class SoundManager {
	  //region Private Members
      private GridClient Client;
      //endregion

      //region Event Handling
      
    /// <summary>Raised when the simulator sends us data containing
    /// sound</summary>
      private EventObservable<AttachedSoundEventArgs> onAttachedSound = new EventObservable<AttachedSoundEventArgs>();
      public void registerOnAttachedSound(EventObserver<AttachedSoundEventArgs> o)
      {
    	  onAttachedSound.addObserver(o);
      }
      public void unregisterOnAttachedSound(EventObserver<AttachedSoundEventArgs> o) 
      {
    	  onAttachedSound.deleteObserver(o);
      }
      
    ///<summary>Raises the AttachedSoundGainChange Event</summary>
    /// <param name="e">A AttachedSoundGainChangeEventArgs object containing
    /// the data sent from the simulator</param>
      private EventObservable<AttachedSoundGainChangeEventArgs> onAttachedSoundGainChange = new EventObservable<AttachedSoundGainChangeEventArgs>();
      public void registerOnAttachedSoundGainChange(EventObserver<AttachedSoundGainChangeEventArgs> o)
      {
    	  onAttachedSoundGainChange.addObserver(o);
      }
      public void unregisterOnAttachedSoundGainChange(EventObserver<AttachedSoundGainChangeEventArgs> o) 
      {
    	  onAttachedSoundGainChange.deleteObserver(o);
      }
      
    /// <summary>Raised when the simulator sends us data containing
    /// ...</summary>
      private EventObservable<SoundTriggerEventArgs> onSoundTrigger = new EventObservable<SoundTriggerEventArgs>();
      public void registerOnSoundTrigger(EventObserver<SoundTriggerEventArgs> o)
      {
    	  onSoundTrigger.addObserver(o);
      }
      public void unregisterOnSoundTrigger(EventObserver<SoundTriggerEventArgs> o) 
      {
    	  onSoundTrigger.deleteObserver(o);
      }
      
    /// <summary>Raised when the simulator sends us data containing
    /// ...</summary>
      private EventObservable<PreloadSoundEventArgs> onPreloadSound = new EventObservable<PreloadSoundEventArgs>();
      public void registerOnPreloadSound(EventObserver<PreloadSoundEventArgs> o)
      {
    	  onPreloadSound.addObserver(o);
      }
      public void unregisterOnPreloadSound(EventObserver<PreloadSoundEventArgs> o) 
      {
    	  onPreloadSound.deleteObserver(o);
      }
      //endregion

      /// <summary>
      /// Construct a new instance of the SoundManager class, used for playing and receiving
      /// sound assets
      /// </summary>
      /// <param name="client">A reference to the current GridClient instance</param>
      public SoundManager(GridClient client)
      {
          Client = client;
          
          // Client.network.RegisterCallback(PacketType.AttachedSound, AttachedSoundHandler);

          Client.network.RegisterCallback(PacketType.AttachedSound, new EventObserver<PacketReceivedEventArgs>()
        		  { 
        	  @Override
        	  public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		  try{ AttachedSoundHandler(o, arg);}
        		  catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	  }}
        		  );
          // Client.network.RegisterCallback(PacketType.AttachedSoundGainChange, AttachedSoundGainChangeHandler);

          Client.network.RegisterCallback(PacketType.AttachedSoundGainChange, new EventObserver<PacketReceivedEventArgs>()
        		  { 
        	  @Override
        	  public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		  try{ AttachedSoundGainChangeHandler(o, arg);}
        		  catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	  }}
        		  );
          // Client.network.RegisterCallback(PacketType.PreloadSound, PreloadSoundHandler);

          Client.network.RegisterCallback(PacketType.PreloadSound, new EventObserver<PacketReceivedEventArgs>()
        		  { 
        	  @Override
        	  public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		  try{ PreloadSoundHandler(o, arg);}
        		  catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	  }}
        		  );
          // Client.network.RegisterCallback(PacketType.SoundTrigger, SoundTriggerHandler);

          Client.network.RegisterCallback(PacketType.SoundTrigger, new EventObserver<PacketReceivedEventArgs>()
        		  { 
        	  @Override
        	  public void handleEvent(Observable o,PacketReceivedEventArgs arg) {
        		  try{ SoundTriggerHandler(o, arg);}
        		  catch(Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e));}
        	  }}
        		  );
      }

      //region public methods

      /// <summary>
      /// Plays a sound in the current region at full volume from avatar position
      /// </summary>
      /// <param name="soundID">UUID of the sound to be played</param>
      public void PlaySound(UUID soundID)
      {
          SendSoundTrigger(soundID, Client.self.getSimPosition(), 1.0f);
      }

      /// <summary>
      /// Plays a sound in the current region at full volume
      /// </summary>
      /// <param name="soundID">UUID of the sound to be played.</param>
      /// <param name="position">position for the sound to be played at. Normally the avatar.</param>
      public void SendSoundTrigger(UUID soundID, Vector3 position)
      {
          SendSoundTrigger(soundID, Client.self.getSimPosition(), 1.0f);
      }

      /// <summary>
      /// Plays a sound in the current region
      /// </summary>
      /// <param name="soundID">UUID of the sound to be played.</param>
      /// <param name="position">position for the sound to be played at. Normally the avatar.</param>
      /// <param name="gain">volume of the sound, from 0.0 to 1.0</param>
      public void SendSoundTrigger(UUID soundID, Vector3 position, float gain)
      {
          SendSoundTrigger(soundID, Client.network.getCurrentSim().Handle, position, gain);
      }
      /// <summary>
      /// Plays a sound in the specified sim
      /// </summary>
      /// <param name="soundID">UUID of the sound to be played.</param>
      /// <param name="sim">UUID of the sound to be played.</param>
      /// <param name="position">position for the sound to be played at. Normally the avatar.</param>
      /// <param name="gain">volume of the sound, from 0.0 to 1.0</param>
      public void SendSoundTrigger(UUID soundID, Simulator sim, Vector3 position, float gain)
      {
          SendSoundTrigger(soundID, sim.Handle, position, gain);
      }

      /// <summary>
      /// Play a sound asset
      /// </summary>
      /// <param name="soundID">UUID of the sound to be played.</param>
      /// <param name="handle">handle id for the sim to be played in.</param>
      /// <param name="position">position for the sound to be played at. Normally the avatar.</param>
      /// <param name="gain">volume of the sound, from 0.0 to 1.0</param>
      public void SendSoundTrigger(UUID soundID, BigInteger handle, Vector3 position, float gain)
      {
          SoundTriggerPacket soundtrigger = new SoundTriggerPacket();
          soundtrigger.SoundData = new SoundTriggerPacket.SoundDataBlock();
          soundtrigger.SoundData.SoundID = soundID;
          soundtrigger.SoundData.ObjectID = UUID.Zero;
          soundtrigger.SoundData.OwnerID = UUID.Zero;
          soundtrigger.SoundData.ParentID = UUID.Zero;
          soundtrigger.SoundData.Handle = handle;
          soundtrigger.SoundData.Position = position;
          soundtrigger.SoundData.Gain = gain;

          Client.network.SendPacket(soundtrigger);
      }

      //endregion
      //region Packet Handlers


      /// <summary>Process an incoming packet and raise the appropriate events</summary>
      /// <param name="sender">The sender</param>
      /// <param name="e">The EventArgs object containing the packet data</param>
      protected void AttachedSoundHandler(Object sender, PacketReceivedEventArgs e)
      {            
          if (onAttachedSound != null)
          {
              AttachedSoundPacket sound = (AttachedSoundPacket)e.getPacket();

              onAttachedSound.raiseEvent(new AttachedSoundEventArgs(e.getSimulator(), sound.DataBlock.SoundID, sound.DataBlock.OwnerID, sound.DataBlock.ObjectID, 
                  sound.DataBlock.Gain, SoundFlags.get(sound.DataBlock.Flags)));                
          }
      }

      /// <summary>Process an incoming packet and raise the appropriate events</summary>
      /// <param name="sender">The sender</param>
      /// <param name="e">The EventArgs object containing the packet data</param>
      protected void AttachedSoundGainChangeHandler(Object sender, PacketReceivedEventArgs e)
      {            
          if (onAttachedSoundGainChange != null)
          {
              AttachedSoundGainChangePacket change = (AttachedSoundGainChangePacket)e.getPacket();
              onAttachedSoundGainChange.raiseEvent(new AttachedSoundGainChangeEventArgs(e.getSimulator(), change.DataBlock.ObjectID, change.DataBlock.Gain));                
          }
      }

      /// <summary>Process an incoming packet and raise the appropriate events</summary>
      /// <param name="sender">The sender</param>
      /// <param name="e">The EventArgs object containing the packet data</param>
      protected void PreloadSoundHandler(Object sender, PacketReceivedEventArgs e)
      {            
          if (onPreloadSound != null)
          {
              PreloadSoundPacket preload = (PreloadSoundPacket)e.getPacket();

              for (PreloadSoundPacket.DataBlockBlock data : preload.DataBlock)
              {
                  onPreloadSound.raiseEvent(new PreloadSoundEventArgs(e.getSimulator(), data.SoundID, data.OwnerID, data.ObjectID));                    
              }
          }
      }

      /// <summary>Process an incoming packet and raise the appropriate events</summary>
      /// <param name="sender">The sender</param>
      /// <param name="e">The EventArgs object containing the packet data</param>
      protected void SoundTriggerHandler(Object sender, PacketReceivedEventArgs e)
      {            
          if (onSoundTrigger != null)
          {
              SoundTriggerPacket trigger = (SoundTriggerPacket)e.getPacket();
              onSoundTrigger.raiseEvent(new SoundTriggerEventArgs(e.getSimulator(),
                      trigger.SoundData.SoundID,
                      trigger.SoundData.OwnerID,
                      trigger.SoundData.ObjectID,
                      trigger.SoundData.ParentID,
                      trigger.SoundData.Gain,
                      trigger.SoundData.Handle,
                      trigger.SoundData.Position));                
          }            
      }
      //endregion
  }
