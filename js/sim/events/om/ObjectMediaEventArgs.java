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
package com.ngt.jopenmetaverse.shared.sim.events.om;

import com.ngt.jopenmetaverse.shared.protocol.primitives.MediaEntry;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;


public class ObjectMediaEventArgs extends EventArgs
{
    /// <summary>
    /// Indicates if the operation was successful
    /// </summary>
    private boolean Success;

    /// <summary>
    /// Media version string
    /// </summary>
    private String Version;

    /// <summary>
    /// Array of media entries indexed by face number
    /// </summary>
    private MediaEntry[] FaceMedia;
    
    public boolean isSuccess() {
		return Success;
	}

	public void setSuccess(boolean success) {
		Success = success;
	}

	public String getVersion() {
		return Version;
	}

	public void setVersion(String version) {
		Version = version;
	}

	public MediaEntry[] getFaceMedia() {
		return FaceMedia;
	}

	public void setFaceMedia(MediaEntry[] faceMedia) {
		FaceMedia = faceMedia;
	}

	public ObjectMediaEventArgs(boolean success, String version, MediaEntry[] faceMedia)
    {
        this.Success = success;
        this.Version = version;
        this.FaceMedia = faceMedia;
    }
}
