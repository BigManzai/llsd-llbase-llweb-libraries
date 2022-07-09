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
package com.ngt.jopenmetaverse.shared.sim.events.am;

import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

/// <summary>Contains the currency balance</summary>
public class BalanceEventArgs extends EventArgs
{
	private int m_Balance;

	/// <summary>
	/// Get the currenct balance
	/// </summary>
	public int getBalance() {  return m_Balance; } 

	/// <summary>
	/// Construct a new BalanceEventArgs object
	/// </summary>
	/// <param name="balance">The currenct balance</param>
	public BalanceEventArgs(int balance)
	{
		this.m_Balance = balance;
	}
}
