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

import com.ngt.jopenmetaverse.shared.protocol.AgentThrottlePacket;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class AgentThrottle {
    /// <summary>Maximum bits per second for resending unacknowledged packets</summary>
    public float getResend()
    {
    	return resend; 
    }
    
    public void setResend(float value)
    {
            if (value > 150000.0f) resend = 150000.0f;
            else if (value < 10000.0f) resend = 10000.0f;
            else resend = value;
    }
    
    /// <summary>Maximum bits per second for LayerData terrain</summary>
    public float getLand()
    {
    	return land; 
    }
    
    public void setLand(float value)
    {
            if (value > 170000.0f) land = 170000.0f;
            else if (value < 0.0f) land = 0.0f; // We don't have control of these so allow throttling to 0
            else land = value;
    }
    
    /// <summary>Maximum bits per second for LayerData wind data</summary>
    public float getWind()
    {
        return wind; 
    }
    
    public void setWind(float value)
    {
            if (value > 34000.0f) wind = 34000.0f;
            else if (value < 0.0f) wind = 0.0f; // We don't have control of these so allow throttling to 0
            else wind = value;
    }
    
    /// <summary>Maximum bits per second for LayerData clouds</summary>
    public float getCloud()
    {
      return cloud;
    }
    
    public void setCloud(float value)
    {
            if (value > 34000.0f) cloud = 34000.0f;
            else if (value < 0.0f) cloud = 0.0f; // We don't have control of these so allow throttling to 0
            else cloud = value;
    }
    
    /// <summary>Unknown, includes object data</summary>
    public float getTask()
    {
        return task;
    }
    
    public void setTask(float value )
    {
            if (value > 446000.0f) task = 446000.0f;
            else if (value < 4000.0f) task = 4000.0f;
            else task = value;
    }
    
    /// <summary>Maximum bits per second for textures</summary>
    public float getTexture()
    {
        return texture;
    }
    
    public void setTexture(float value)
    {
            if (value > 446000.0f) texture = 446000.0f;
            else if (value < 4000.0f) texture = 4000.0f;
            else texture = value;
    }
    
    /// <summary>Maximum bits per second for downloaded assets</summary>
    public float getAsset()
    {
        return asset; 
    }

    public void setAsset(float value)
    {
            if (value > 220000.0f) asset = 220000.0f;
            else if (value < 10000.0f) asset = 10000.0f;
            else asset = value;
    }
    
    /// <summary>Maximum bits per second the entire connection, divided up
    /// between invidiual streams using default multipliers</summary>
    public float getTotal()
    {
        return resend + land + wind + cloud + task + texture + asset;
    }

    public void setTotal(float value)
    {
            // Sane initial values
            resend = (value * 0.1f);
            land = (float)(value * 0.52f / 3f);
            wind = (float)(value * 0.05f);
            cloud = (float)(value * 0.05f);
            task = (float)(value * 0.704f / 3f);
            texture = (float)(value * 0.704f / 3f);
            asset = (float)(value * 0.484f / 3f);
    }
    
    
    private GridClient Client;
    private float resend;
    private float land;
    private float wind;
    private float cloud;
    private float task;
    private float texture;
    private float asset;

    /// <summary>
    /// Default constructor, uses a default high total of 1500 KBps (1536000)
    /// </summary>
    public AgentThrottle(GridClient client)
    {
        Client = client;
        setTotal(1536000.0f);
    }

    /// <summary>
    /// Constructor that decodes an existing AgentThrottle packet in to
    /// individual values
    /// </summary>
    /// <param name="data">Reference to the throttle data in an AgentThrottle
    /// packet</param>
    /// <param name="pos">Offset position to start reading at in the 
    /// throttle data</param>
    /// <remarks>This is generally not needed in clients as the server will
    /// never send a throttle packet to the client</remarks>
    public AgentThrottle(byte[] data, int pos)
    {
        byte[] adjData = data;

//        if (!BitConverter.IsLittleEndian)
//        {
//            byte[] newData = new byte[7 * 4];
//            Utils.arraycopy(data, pos, newData, 0, 7 * 4);
//
//            for (int i = 0; i < 7; i++)
//                Array.Reverse(newData, i * 4, 4);
//
//            adjData = newData;
//        }
//        else
//        {
//            adjData = data;
//        }

        resend = Utils.bytesToFloatLit(adjData, pos); pos += 4;
        land = Utils.bytesToFloatLit(adjData, pos); pos += 4;
        wind = Utils.bytesToFloatLit(adjData, pos); pos += 4;
        cloud = Utils.bytesToFloatLit(adjData, pos); pos += 4;
        task = Utils.bytesToFloatLit(adjData, pos); pos += 4;
        texture = Utils.bytesToFloatLit(adjData, pos); pos += 4;
        asset = Utils.bytesToFloatLit(adjData, pos);
    }

    /// <summary>
    /// Send an AgentThrottle packet to the current server using the 
    /// current values
    /// </summary>
    public void Set()
    {
        Set(Client.network.getCurrentSim());
    }

    /// <summary>
    /// Send an AgentThrottle packet to the specified server using the 
    /// current values
    /// </summary>
    public void Set(Simulator simulator)
    {
        AgentThrottlePacket throttle = new AgentThrottlePacket();
        throttle.AgentData.AgentID = Client.self.getAgentID();
        throttle.AgentData.SessionID = Client.self.getSessionID();
        throttle.AgentData.CircuitCode = Client.network.getCircuitCode();
        throttle.Throttle.GenCounter = 0;
        throttle.Throttle.Throttles = this.ToBytes();

        Client.network.SendPacket(throttle, simulator);
    }

    /// <summary>
    /// Convert the current throttle values to a byte array that can be put
    /// in an AgentThrottle packet
    /// </summary>
    /// <returns>Byte array containing all the throttle values</returns>
    public byte[] ToBytes()
    {
        byte[] data = new byte[7 * 4];
        int i = 0;

        Utils.arraycopy(Utils.floatToBytesLit(resend), 0, data, i, 4); i += 4;
        Utils.arraycopy(Utils.floatToBytesLit(land), 0, data, i, 4); i += 4;
        Utils.arraycopy(Utils.floatToBytesLit(wind), 0, data, i, 4); i += 4;
        Utils.arraycopy(Utils.floatToBytesLit(cloud), 0, data, i, 4); i += 4;
        Utils.arraycopy(Utils.floatToBytesLit(task), 0, data, i, 4); i += 4;
        Utils.arraycopy(Utils.floatToBytesLit(texture), 0, data, i, 4); i += 4;
        Utils.arraycopy(Utils.floatToBytesLit(asset), 0, data, i, 4); i += 4;

        return data;
    }
}
