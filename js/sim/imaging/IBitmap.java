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
package com.ngt.jopenmetaverse.shared.sim.imaging;

import com.ngt.jopenmetaverse.shared.exception.NotSupportedException;

public interface IBitmap {
 public boolean hasPixelFormat(PixelFormat pixelFormat) throws NotSupportedException;
 public int getRGB(int x, int y);
 public int getHeight();
 public int getWidth();
 public String getPixelFormatAsString();
 public PixelFormat getPixelFormat(); 
 public IBitmap createIBitmap(int w, int h, int[] pixels);
 public void setRGB(int w, int h, int i);
 
 public void resize(int w, int h);
 public void tile(int tiles);
 public void rotateAndFlip(double radians, boolean flipx, boolean flipy);
 
 public IBitmap cloneAndResize(int w, int h);
 public IBitmap cloneAndTile(int tiles);
 public IBitmap cloneRotateAndFlip(double radians, boolean flipx, boolean flipy);
 
 public IBitmap createImageWithSolidColor(int width, int height, int r, int g, int b, int a);
 
 public byte[] exportTGA() throws Exception;
 public byte[] exportRAW() throws Exception;
 public int[] exportPixels() throws Exception;
 
 public void dumpToFile(String filePath) throws Exception;
 public void dispose();
}
