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
package com.ngt.jopenmetaverse.shared.util;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Random;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

public class JLogger {

	static Logger logger;
	static String loggerDirURL;
	static Random rdm;
	
	/*
	 * Static Block, called only once 
	 */
	static {
		logger = Logger.getLogger("ApplicationRoot");
		rdm = new Random(Utils.getUnixTime());
		loggerDirURL =  "openmetaverse_data/logs";
		File f = new File(loggerDirURL);
		if(!f.exists())
			f.mkdirs();
	}
	
//	public static void logPkt(String pktName, byte[] bytes)
//	{
//		if(logger.isLoggable(Level.INFO))
//				{
//		try {
//			long now = Utils.getUnixTime();
//			File f = new File(FileUtils.combineFilePath(loggerDirURL, pktName + "_" + now + "_" + rdm.nextInt()));
//			f.createNewFile();
//			FileOutputStream fs = new FileOutputStream(f);
//			fs.write(bytes);
//			FileUtils.closeStream(fs);
//		} catch (IOException e) {
//			JLogger.warn(Utils.getExceptionStackTraceAsString(e));
//		}
//				}
//	}
	
	
	public static void debug(String msg)
	{
//		logger.log(Level.ALL, msg);
//		System.out.println(msg);
		logger.log(Level.INFO, msg);		
	}	
	
	public static void info(String msg)
	{
		logger.log(Level.INFO, msg);		
	}

	public static void warn(String msg)
	{
		logger.log(Level.WARNING, msg);
	}
	
	public static void error(String msg)
	{
		logger.log(Level.SEVERE, msg);
	}
	
	
	public static void debug(String loggerName, String msg)
	{
//		logger.log(Level.ALL, msg);
//		System.out.println(msg);
		Logger.getLogger(loggerName).log(Level.INFO, msg);		
	}	
	
	public static void info(String loggerName, String msg)
	{
		Logger.getLogger(loggerName).log(Level.INFO, msg);		
	}

	public static void warn(String loggerName, String msg)
	{
		Logger.getLogger(loggerName).log(Level.WARNING, msg);
	}
	
	public static void error(String loggerName, String msg)
	{
		Logger.getLogger(loggerName).log(Level.SEVERE, msg);
	}
	
	public static void setLevel(Level newLevel)
	{
		logger.setLevel(newLevel);
	}
	
	public static Logger getLogger(String logger)
	{
		return Logger.getLogger(logger);
	}
	
	public static void addHandler(Handler handler)
	{
		logger.addHandler(handler);
	}

	public static Handler[] getHandlers()
	{
		return logger.getHandlers();
	}
	
	public static void deleteHandler(Handler[] handlerList)
	{
		for(Handler h: handlerList)
		logger.removeHandler(h);
	}

	public static void error(Exception e) {
		error(Utils.getExceptionStackTraceAsString(e));
	}
	
	public static void warn(Exception e) {
		warn(Utils.getExceptionStackTraceAsString(e));
	}	
	
}
