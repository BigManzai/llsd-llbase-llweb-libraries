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
package com.ngt.jopenmetaverse.shared.sim.buffers;

public class WrappedObject<T> {

//    private T _instance;
//    internal readonly ObjectPoolSegment<T> _owningSegment;
//    internal readonly ObjectPoolBase<T> _owningObjectPool;
//    private bool _disposed = false;
//
//    internal WrappedObject(ObjectPoolBase<T> owningPool, ObjectPoolSegment<T> ownerSegment, T activeInstance)
//    {
//        _owningObjectPool = owningPool;
//        _owningSegment = ownerSegment;
//        _instance = activeInstance;
//    }
//
//    ~WrappedObject()
//    {
//#if !PocketPC
//        // If the AppDomain is being unloaded, or the CLR is 
//        // shutting down, just exit gracefully
//        if (Environment.HasShutdownStarted)
//            return;
//#endif
//
//        // Object Resurrection in Action!
//        GC.ReRegisterForFinalize(this);
//
//        // Return this instance back to the owning queue
//        _owningObjectPool.CheckIn(_owningSegment, _instance);
//    }
//
//    /// <summary>
//    /// Returns an instance of the class that has been checked out of the Object Pool.
//    /// </summary>
//    public T Instance
//    {
//        get
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("WrappedObject");
//            return _instance;
//        }
//    }
//
//    /// <summary>
//    /// Checks the instance back into the object pool
//    /// </summary>
//    public void Dispose()
//    {
//        if (_disposed)
//            return;
//
//        _disposed = true;
//        _owningObjectPool.CheckIn(_owningSegment, _instance);
//        GC.SuppressFinalize(this);
//    }
}
