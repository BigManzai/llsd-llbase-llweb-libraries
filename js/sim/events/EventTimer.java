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

import java.util.Timer;
import java.util.TimerTask;

public class EventTimer {

	private Timer timer;
	private TimerTask task;
	private long delay;
	
	public EventTimer(TimerTask task)
	{
		timer = new Timer();
		this.task=task;
	}

	public void schedule(long delay)
	{
		timer.schedule(task, delay);
	}

	public void schedule(long delay, int interval)
	{
		timer.schedule(task, delay, interval);
	}
	
	public void cancel()
	{
		timer.cancel();
	}
	
	public void reschedule(long delay)
	{
		cancel();
		schedule(delay);
	}
	
	public void reschedule(long delay, int interval)
	{
		cancel();
		schedule(delay, interval);
	}
}
