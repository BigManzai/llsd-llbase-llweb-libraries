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

import java.util.TimerTask;
import java.util.concurrent.TimeUnit;

import com.ngt.jopenmetaverse.shared.util.JLogger;

public class ThreadPoolFactory {
	private static ThreadPool threadPool = new DefaultThreadPoolExecutor();
	
	public static ThreadPool getThreadPool()
	{
		return threadPool;
	}
	
	public static ThreadPool getNewInstance(int corePoolSize, int maximumPoolSize, 
    		long keepAliveTime, TimeUnit unit, int capacity)
	{
		return new DefaultThreadPoolExecutor(corePoolSize, maximumPoolSize, keepAliveTime, 
				unit, capacity);
	}	
	
	public static void executeParallel(Runnable[] tasks, int capacity) throws InterruptedException
	{
		//FIXME a dirty implement of executing parallel tasks
		if(tasks.length > 0)
		{
		final ThreadPool pool = new DefaultThreadPoolExecutor(capacity, capacity, 10, 
				TimeUnit.SECONDS, tasks.length);
		final AutoResetEvent event = new AutoResetEvent(false); 
		final EventTimer timer = new EventTimer(new TimerTask(){
			@Override
			public void run() {
				JLogger.debug(String.format("Waiting for parallel tasks to get finished: Queue Size: %d Thread Count: %d", 
						pool.getQueueSize(), pool.getActiveThreadCount()));
				if(pool.getActiveThreadCount() ==0 && pool.getQueueSize() ==0 )
				{
					pool.shutdown();
					this.cancel();
					JLogger.debug("All the tasks got finished...");
					event.set();
				}
			}
		});
		JLogger.debug("Start executing the tasks...");		
		pool.execute(tasks);
		timer.schedule(1000, 1000);
		event.waitOne();
		JLogger.debug("Execute Parallel returing...");
		}
		else
			JLogger.warn("0 number of tasks are provided to get executed in parallel");
	}
}
