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

import com.ngt.jopenmetaverse.shared.sim.AgentManager.TransactionInfo;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;

/// <summary>Contains the transaction summary when an item is purchased, 
	/// money is given, or land is purchased</summary>
	public class MoneyBalanceReplyEventArgs extends EventArgs
	{
		private  UUID m_TransactionID;
		private  boolean m_Success;
		private  int m_Balance;
		private  int m_MetersCredit;
		private  int m_MetersCommitted;
		private  String m_Description;
		private TransactionInfo m_TransactionInfo;

		/// <summary>Get the ID of the transaction</summary>
		public UUID getTransactionID() { return m_TransactionID; } 
		/// <summary>True of the transaction was successful</summary>
		public boolean getSuccess() { return m_Success; } 
		/// <summary>Get the remaining currency balance</summary>
		public int getBalance() { return m_Balance; } 
		/// <summary>Get the meters credited</summary>
		public int getMetersCredit() { return m_MetersCredit; } 
		/// <summary>Get the meters comitted</summary>
		public int getMetersCommitted() { return m_MetersCommitted; } 
		/// <summary>Get the description of the transaction</summary>
		public String getDescription() { return m_Description; } 
		/// <summary>Detailed transaction information</summary>
		public TransactionInfo getTransactionInfo() { return m_TransactionInfo; } 
		/// <summary>
		/// Construct a new instance of the MoneyBalanceReplyEventArgs object
		/// </summary>
		/// <param name="transactionID">The ID of the transaction</param>
		/// <param name="transactionSuccess">True of the transaction was successful</param>
		/// <param name="balance">The current currency balance</param>
		/// <param name="metersCredit">The meters credited</param>
		/// <param name="metersCommitted">The meters comitted</param>
		/// <param name="description">A brief description of the transaction</param>
		public MoneyBalanceReplyEventArgs(UUID transactionID, boolean transactionSuccess, int balance, int metersCredit, int metersCommitted, String description, TransactionInfo transactionInfo)
		{
			this.m_TransactionID = transactionID;
			this.m_Success = transactionSuccess;
			this.m_Balance = balance;
			this.m_MetersCredit = metersCredit;
			this.m_MetersCommitted = metersCommitted;
			this.m_Description = description;
			this.m_TransactionInfo = transactionInfo;
		}
	}

