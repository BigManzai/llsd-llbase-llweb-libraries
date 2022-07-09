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
package com.ngt.jopenmetaverse.shared.sim.events;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;

public class ManualResetEvent implements IResetEvent {
    private volatile CountDownLatch event;
    private final Integer mutex;
    
    public ManualResetEvent(boolean signalled) {
            mutex = new Integer(-1);
            if (signalled) {
                    event = new CountDownLatch(0);
            } else {
                    event = new CountDownLatch(1);
            }
    }
    
    public void set() {
            event.countDown();
    }
    
    public void reset() {
            synchronized (mutex) {
                    if (event.getCount() == 0) {
                            event = new CountDownLatch(1);
                    }
            }
    }
    
    public void waitOne() throws InterruptedException {
            event.await();
    }
    
    public boolean waitOne(int timeout) throws InterruptedException {
            return event.await(timeout, TimeUnit.MILLISECONDS);
    }
    
    public boolean isSignalled() {
            return event.getCount() == 0;
    }
}
