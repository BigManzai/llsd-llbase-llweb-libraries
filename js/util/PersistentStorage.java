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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;

public class PersistentStorage {
	public static void serializeObject(Object obj, String dstpath) throws FileNotFoundException, IOException
	{
		File f = new File(dstpath);
		ObjectOutput ObjOut = new ObjectOutputStream(new FileOutputStream(f));
		ObjOut.writeObject(obj);
		ObjOut.close();
	}

	public static Object deserializeObject(String srcpath) throws FileNotFoundException, IOException, ClassNotFoundException
	{
		File f = new File(srcpath);
		ObjectInputStream obj = new ObjectInputStream(new FileInputStream(f));
		Object object = obj.readObject();
		obj.close();
		return object;
	}
}
