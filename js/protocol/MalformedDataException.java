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
package com.ngt.jopenmetaverse.shared.protocol;


    /// <summary>
    /// Thrown when a packet could not be successfully deserialized
    /// </summary>
    public class MalformedDataException extends Exception
    {
        /// <summary>
        /// Default constructor
        /// </summary>
        public MalformedDataException() 
        { }

        /// <summary>
        /// Constructor that takes an additional error message
        /// </summary>
        /// <param name="Message">An error message to attach to this exception</param>
        public MalformedDataException(String message)
        {
        	super(message);
        }
    }