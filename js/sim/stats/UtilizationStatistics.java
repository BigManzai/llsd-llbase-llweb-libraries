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
package com.ngt.jopenmetaverse.shared.sim.stats;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.atomic.AtomicLong;

public class UtilizationStatistics {

    public enum Type
    {
        Packet,
        Message
    }
	
    public class Stat
    {
        private Type Type;
        private AtomicLong TxCount = new AtomicLong();
        private AtomicLong RxCount = new AtomicLong();
        private AtomicLong TxBytes = new AtomicLong();
        private AtomicLong RxBytes = new AtomicLong();

        public Stat(Type type, long txCount, long rxCount, long txBytes, long rxBytes)
        {
            this.Type = type;
            this.TxCount.set(txCount);
            this.RxCount.set(rxCount);
            this.TxBytes.set(txBytes);
            this.RxBytes.set(rxBytes);
        }

		public long getTxCount() {
			return TxCount.get();
		}

		public void setTxCount(long txCount) {
			TxCount.set(txCount);
		}

		public void addTxCount(long delta) {
			TxCount.addAndGet(delta);
		}
		
		public long getRxCount() {
			return RxCount.get();
		}

		public void setRxCount(long rxCount) {
			RxCount.set(rxCount);
		}

		public void addRxCount(long delta) {
			RxCount.addAndGet(delta);
		}
		
		
		public long getTxBytes() {
			return TxBytes.get();
		}

		public void setTxBytes(long txBytes) {
			TxBytes.set(txBytes);
		}

		public void addTxBytes(long delta) {
			TxBytes.addAndGet(delta);
		}
		
		
		public long getRxBytes() {
			return RxBytes.get();
		}

		public void setRxBytes(long rxBytes) {
			RxBytes.set(rxBytes);
		}
		
		public void addRxBytes(long delta) {
			RxBytes.addAndGet(delta);
		}
    }
            
    private Map<String, Stat> m_StatsCollection;

    public UtilizationStatistics()
    {
        m_StatsCollection = new HashMap<String, Stat>();
    }

    //internal accessibility
    public void Update(String key, Type Type, long txBytes, long rxBytes)
    {            
    	synchronized (m_StatsCollection)
        {
            if(m_StatsCollection.containsKey(key))
            {
                Stat stat = m_StatsCollection.get(key);
                if (rxBytes > 0)
                {
//                    Interlocked.Increment(ref stat.RxCount);
//                    Interlocked.Add(ref stat.RxBytes, rxBytes);
                	stat.addRxCount(1L);
                	stat.addRxBytes(rxBytes);
                }

                if (txBytes > 0)
                {
//                    Interlocked.Increment(ref stat.TxCount);
//                    Interlocked.Add(ref stat.TxBytes, txBytes);
                    stat.addTxCount(1L);
                	stat.addTxBytes(txBytes);
                }
                                                                       
            } else {
                Stat stat;
                if (txBytes > 0)
                    stat = new Stat(Type, 1, 0, txBytes, 0);
                else
                    stat = new Stat(Type, 0, 1, 0, rxBytes);

                m_StatsCollection.put(key, stat);
            }
        }
    }

    public Map<String, Stat> GetStatistics()
    {
    	synchronized(m_StatsCollection)
        {
            return new HashMap<String, Stat>(m_StatsCollection);
        }
    }
}
	
