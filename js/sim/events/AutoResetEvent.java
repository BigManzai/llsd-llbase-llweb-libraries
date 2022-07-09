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

import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;


public final class AutoResetEvent implements IResetEvent{
	private final Semaphore event;
	private final Integer mutex;

	public AutoResetEvent(boolean signalled) {
		event = new Semaphore(signalled ? 1 : 0);
		mutex = new Integer(-1);

	}

	public void set() {
		synchronized (mutex) {
			if (event.availablePermits() == 0)
				event.release();        
		}
	}

	public void reset() {
		event.drainPermits();
	}

	public void waitOne() throws InterruptedException {
		event.acquire();
	}

	public boolean waitOne(int timeout) throws InterruptedException {
		return event.tryAcquire(timeout, TimeUnit.MILLISECONDS);
	}       

	public boolean isSignalled() {
		return event.availablePermits() > 0;
	}       
}
