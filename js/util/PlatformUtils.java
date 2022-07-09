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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.net.UnknownHostException;

public class PlatformUtils {
	public static String getPlatformOS() {
		String platform = "";
		if (isWindows()) {
			platform = "Win";
		} else if (isMac()) {
			platform = "Mac";			
		} else if (isUnix()) {
			platform = "Linux";
		} else if (isSolaris()) {
			platform = "Solaris";
		} else {
			platform = "";
		}

		return platform;
	}

	/*
	 * Return 
	 *  If success, Mac address of the current active interface
	 *  otherwise, emmpty string
	 */
	public static String getMACAddress()
	{
		StringBuilder sb = new StringBuilder();
		try{
		InetAddress ip;
		ip = InetAddress.getLocalHost();	 
		NetworkInterface network = NetworkInterface.getByInetAddress(ip);

		byte[] mac = network.getHardwareAddress();
		for (int i = 0; i < mac.length; i++) {
			sb.append(String.format("%02X%s", mac[i], (i < mac.length - 1) ? "-" : ""));		
		}
		}
		catch(Exception e)
		{
		}
		return sb.toString();
	}

	public static boolean tryParseInetAddress(String ip,InetAddress[] result) 
	{
		try {
			result[0] = InetAddress.getByName(ip);
		} catch (UnknownHostException e) {
			return false;
		}
		return true;
	}
	
	
	public static boolean isWindows() {

		String os = System.getProperty("os.name").toLowerCase();
		// windows
		return (os.indexOf("win") >= 0);

	}

	public static boolean isMac() {

		String os = System.getProperty("os.name").toLowerCase();
		// Mac
		return (os.indexOf("mac") >= 0);

	}

	public static boolean isUnix() {

		String os = System.getProperty("os.name").toLowerCase();
		// linux or unix
		return (os.indexOf("nix") >= 0 || os.indexOf("nux") >= 0);

	}

	public static boolean isSolaris() {

		String os = System.getProperty("os.name").toLowerCase();
		// Solaris
		return (os.indexOf("sunos") >= 0);

	}

	/*
	 * Sleep for given amount of mili seconds
	 */
	public static void sleep(int timeout)
	{
		try {
			Thread.sleep(timeout);
		} catch (InterruptedException e) {
			JLogger.info(e.getMessage());
		}
	}
	
	/*
	 * Silently try to close the stream with raising exception
	 */
	public static void closeStream(InputStream is)
	{
		try {
			is.close();
		} catch (IOException e) {
			JLogger.warn("Error in closing the input stream " + Utils.getExceptionStackTraceAsString(e));
		}
	}

	/*
	 * Silently try to close the stream with raising exception
	 */
	public static void closeStream(OutputStream is)
	{
		try {
			is.close();
		} catch (IOException e) {
			JLogger.warn("Error in closing the output stream " + Utils.getExceptionStackTraceAsString(e));
		}
	}
	
}
