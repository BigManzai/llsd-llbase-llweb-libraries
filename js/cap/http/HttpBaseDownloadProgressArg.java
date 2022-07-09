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
package com.ngt.jopenmetaverse.shared.cap.http;

import org.apache.http.HttpResponse;
import org.apache.http.client.methods.HttpRequestBase;

public class HttpBaseDownloadProgressArg {
	HttpRequestBase request; 
	HttpResponse response; 
	int bytesReceived;
	int totalBytesToReceive;
	
	public HttpBaseDownloadProgressArg(HttpRequestBase request, HttpResponse response,
			int bytesReceived, int totalBytesToReceive) {
		super();
		this.request = request;
		this.response = response;
		this.bytesReceived = bytesReceived;
		this.totalBytesToReceive = totalBytesToReceive;
	}
	public HttpRequestBase getRequest() {
		return request;
	}
	public void setRequest(HttpRequestBase request) {
		this.request = request;
	}
	public HttpResponse getResponse() {
		return response;
	}
	public void setResponse(HttpResponse response) {
		this.response = response;
	}
	public int getBytesReceived() {
		return bytesReceived;
	}
	public void setBytesReceived(int bytesReceived) {
		this.bytesReceived = bytesReceived;
	}
	public int getTotalBytesToReceive() {
		return totalBytesToReceive;
	}
	public void setTotalBytesToReceive(int totalBytesToReceive) {
		this.totalBytesToReceive = totalBytesToReceive;
	}
}
