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
package com.ngt.jopenmetaverse.shared.sim.login;

public class LoginResponseCallbackArg {

	boolean loginSuccess;
	boolean redirect;
	String message;
	String reason;
	LoginResponseData replyData;
	
	public LoginResponseCallbackArg(boolean loginSuccess, boolean redirect,
			String message, String reason, LoginResponseData replyData) {
		super();
		this.loginSuccess = loginSuccess;
		this.redirect = redirect;
		this.message = message;
		this.reason = reason;
		this.replyData = replyData;
	}
	public boolean isLoginSuccess() {
		return loginSuccess;
	}
	public void setLoginSuccess(boolean loginSuccess) {
		this.loginSuccess = loginSuccess;
	}
	public boolean isRedirect() {
		return redirect;
	}
	public void setRedirect(boolean redirect) {
		this.redirect = redirect;
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	public String getReason() {
		return reason;
	}
	public void setReason(String reason) {
		this.reason = reason;
	}
	public LoginResponseData getReplyData() {
		return replyData;
	}
	public void setReplyData(LoginResponseData replyData) {
		this.replyData = replyData;
	}
}
