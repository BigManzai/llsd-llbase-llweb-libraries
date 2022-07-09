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

public class ObjectPoolBase<T>
	{
//    private int _itemsPerSegment = 32;
//    private int _minimumSegmentCount = 1;
//
//    // A segment won't be eligible for cleanup unless it's at least this old...
//    private TimeSpan _minimumAgeToCleanup = new TimeSpan(0, 5, 0);
//
//    // ever increasing segment counter
//    private int _activeSegment = 0;
//
//
//    private Dictionary<int, ObjectPoolSegment<T>> _segments = new Dictionary<int, ObjectPoolSegment<T>>();
////    private object _syncRoot = new object();
////    private object _timerLock = new object();
//
//    // create a timer that starts in 5 minutes, and gets called every 5 minutes.
//    System.Threading.Timer _timer;
//    int _cleanupFrequency;
//
//    /// <summary>
//    /// Creates a new instance of the ObjectPoolBase class. Initialize MUST be called
//    /// after using this constructor.
//    /// </summary>
//    protected ObjectPoolBase()
//    {
//    }
//
//    /// <summary>
//    /// Creates a new instance of the ObjectPool Base class.        
//    /// </summary>
//    /// <param name="itemsPerSegment">The object pool is composed of segments, which 
//    /// are allocated whenever the size of the pool is exceeded. The number of items
//    /// in a segment should be large enough that allocating a new segmeng is a rare
//    /// thing. For example, on a server that will have 10k people logged in at once, 
//    /// the receive buffer object pool should have segment sizes of at least 1000 
//    /// byte arrays per segment.
//    /// </param>
//    /// <param name="minimumSegmentCount">The minimun number of segments that may exist.</param>
//    /// <param name="gcOnPoolGrowth">Perform a full GC.Collect whenever a segment is allocated, and then again after allocation to compact the heap.</param>
//    /// <param name="cleanupFrequenceMS">The frequency which segments are checked to see if they're eligible for cleanup.</param>
//    protected ObjectPoolBase(int itemsPerSegment, int minimumSegmentCount, bool gcOnPoolGrowth, int cleanupFrequenceMS)
//    {
//        Initialize(itemsPerSegment, minimumSegmentCount, gcOnPoolGrowth, cleanupFrequenceMS);
//    }
//
//    protected void Initialize(int itemsPerSegment, int minimumSegmentCount, bool gcOnPoolGrowth, int cleanupFrequenceMS)
//    {
//        _itemsPerSegment = itemsPerSegment;
//        _minimumSegmentCount = minimumSegmentCount;
//        _gc = gcOnPoolGrowth;
//
//        // force garbage collection to make sure these new long lived objects
//        // cause as little fragmentation as possible
//        if (_gc)
//            System.GC.Collect();
//
//        lock (_syncRoot)
//        {
//            while (_segments.Count < this.MinimumSegmentCount)
//            {
//                ObjectPoolSegment<T> segment = CreateSegment(false);
//                _segments.Add(segment.SegmentNumber, segment);
//            }
//        }
//
//        // This forces a compact, to make sure our objects fill in any holes in the heap. 
//        if (_gc)
//        {
//            System.GC.Collect();
//        }
//
//        _timer = new Timer(CleanupThreadCallback, null, cleanupFrequenceMS, cleanupFrequenceMS);
//    }
//
//    /// <summary>
//    /// Forces the segment cleanup algorithm to be run. This method is intended 
//    /// primarly for use from the Unit Test libraries.
//    /// </summary>
//    internal void ForceCleanup()
//    {
//        CleanupThreadCallback(null);
//    }
//
//    private void CleanupThreadCallback(object state)
//    {
//        if (_disposed)
//            return;
//
//        if (Monitor.TryEnter(_timerLock) == false)
//            return;
//
//        try
//        {
//            lock (_syncRoot)
//            {
//                // If we're below, or at, or minimum segment count threshold, 
//                // there's no point in going any further.
//                if (_segments.Count <= _minimumSegmentCount)
//                    return;
//
//                for (int i = _activeSegment; i > 0; i--)
//                {
//                    ObjectPoolSegment<T> segment;
//                    if (_segments.TryGetValue(i, out segment) == true)
//                    {
//                        // For the "old" segments that were allocated at startup, this will
//                        // always be false, as their expiration dates are set at infinity. 
//                        if (segment.CanBeCleanedUp())
//                        {
//                            _segments.Remove(i);
//                            segment.Dispose();
//                        }
//                    }
//                }
//            }
//        }
//        finally
//        {
//            Monitor.Exit(_timerLock);
//        }
//    }
//
//    /// <summary>
//    /// Responsible for allocate 1 instance of an object that will be stored in a segment. 
//    /// </summary>
//    /// <returns>An instance of whatever objec the pool is pooling.</returns>
//    protected abstract T GetObjectInstance();
//
//
//    private ObjectPoolSegment<T> CreateSegment(bool allowSegmentToBeCleanedUp)
//    {
//        if (_disposed)
//            throw new ObjectDisposedException("ObjectPoolBase");
//
//        if (allowSegmentToBeCleanedUp)
//            Logger.Log("Creating new object pool segment", Helpers.LogLevel.Info);
//
//        // This method is called inside a lock, so no interlocked stuff required.
//        int segmentToAdd = _activeSegment;
//        _activeSegment++;
//
//        Queue<T> buffers = new Queue<T>();
//        for (int i = 1; i <= this._itemsPerSegment; i++)
//        {
//            T obj = GetObjectInstance();
//            buffers.Enqueue(obj);
//        }
//
//        // certain segments we don't want to ever be cleaned up (the initial segments)
//        DateTime cleanupTime = (allowSegmentToBeCleanedUp) ? DateTime.Now.Add(this._minimumAgeToCleanup) : DateTime.MaxValue;
//        ObjectPoolSegment<T> segment = new ObjectPoolSegment<T>(segmentToAdd, buffers, cleanupTime);
//
//        return segment;
//    }
//
//
//    /// <summary>
//    /// Checks in an instance of T owned by the object pool. This method is only intended to be called
//    /// by the <c>WrappedObject</c> class.
//    /// </summary>
//    /// <param name="owningSegment">The segment from which the instance is checked out.</param>
//    /// <param name="instance">The instance of <c>T</c> to check back into the segment.</param>
//    internal void CheckIn(ObjectPoolSegment<T> owningSegment, T instance)
//    {
//        lock (_syncRoot)
//        {
//            owningSegment.CheckInObject(instance);
//        }
//    }
//
//    /// <summary>
//    /// Checks an instance of <c>T</c> from the pool. If the pool is not sufficient to 
//    /// allow the checkout, a new segment is created. 
//    /// </summary>
//    /// <returns>A <c>WrappedObject</c> around the instance of <c>T</c>. To check
//    /// the instance back into the segment, be sureto dispose the WrappedObject 
//    /// when finished. </returns>
//    public WrappedObject<T> CheckOut()
//    {
//        if (_disposed)
//            throw new ObjectDisposedException("ObjectPoolBase");
//
//        // It's key that this CheckOut always, always, uses a pooled object
//        // from the oldest available segment. This will help keep the "newer" 
//        // segments from being used - which in turn, makes them eligible
//        // for deletion.
//
//
//        lock (_syncRoot)
//        {
//            ObjectPoolSegment<T> targetSegment = null;
//
//            // find the oldest segment that has items available for checkout
//            for (int i = 0; i < _activeSegment; i++)
//            {
//                ObjectPoolSegment<T> segment;
//                if (_segments.TryGetValue(i, out segment) == true)
//                {
//                    if (segment.AvailableItems > 0)
//                    {
//                        targetSegment = segment;
//                        break;
//                    }
//                }
//            }
//
//            if (targetSegment == null)
//            {
//                // We couldn't find a sigment that had any available space in it,
//                // so it's time to create a new segment.
//
//                // Before creating the segment, do a GC to make sure the heap 
//                // is compacted.
//                if (_gc) GC.Collect();
//
//                targetSegment = CreateSegment(true);
//
//                if (_gc) GC.Collect();
//
//                _segments.Add(targetSegment.SegmentNumber, targetSegment);
//            }
//
//            WrappedObject<T> obj = new WrappedObject<T>(this, targetSegment, targetSegment.CheckOutObject());
//            return obj;
//        }
//    }
//
//    /// <summary>
//    /// The total number of segments created. Intended to be used by the Unit Tests.
//    /// </summary>
//    public int TotalSegments
//    {
//        get
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("ObjectPoolBase");
//
//            lock (_syncRoot)
//            {
//                return _segments.Count;
//            }
//        }
//    }
//
//    /// <summary>
//    /// The number of items that are in a segment. Items in a segment 
//    /// are all allocated at the same time, and are hopefully close to 
//    /// each other in the managed heap. 
//    /// </summary>
//    public int ItemsPerSegment
//    {
//        get
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("ObjectPoolBase");
//
//            return _itemsPerSegment;
//        }
//    }
//
//    /// <summary>
//    /// The minimum number of segments. When segments are reclaimed, 
//    /// this number of segments will always be left alone. These
//    /// segments are allocated at startup.
//    /// </summary>
//    public int MinimumSegmentCount
//    {
//        get
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("ObjectPoolBase");
//
//            return _minimumSegmentCount;
//        }
//    }
//
//    /// <summary>
//    /// The age a segment must be before it's eligible for cleanup. 
//    /// This  is used to prevent thrash, and typical values are in 
//    /// the 5 minute range.
//    /// </summary>
//    public TimeSpan MinimumSegmentAgePriorToCleanup
//    {
//        get
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("ObjectPoolBase");
//
//            return _minimumAgeToCleanup;
//        }
//        set
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("ObjectPoolBase");
//
//            _minimumAgeToCleanup = value;
//        }
//    }
//
//    /// <summary>
//    /// The frequence which the cleanup thread runs. This is typically
//    /// expected to be in the 5 minute range. 
//    /// </summary>
//    public int CleanupFrequencyMilliseconds
//    {
//        get
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("ObjectPoolBase");
//
//            return _cleanupFrequency;
//        }
//        set
//        {
//            if (_disposed)
//                throw new ObjectDisposedException("ObjectPoolBase");
//
//            Interlocked.Exchange(ref _cleanupFrequency, value);
//
//            _timer.Change(_cleanupFrequency, _cleanupFrequency);
//        }
//    }
//
//    #region IDisposable Members
//
//    public void Dispose()
//    {
//        if (_disposed)
//            return;
//
//        Dispose(true);
//
//        GC.SuppressFinalize(this);
//    }
//
//    protected virtual void Dispose(bool disposing)
//    {
//        if (disposing)
//        {
//            lock (_syncRoot)
//            {
//                if (_disposed)
//                    return;
//
//                _timer.Dispose();
//                _disposed = true;
//
//                foreach (KeyValuePair<int, ObjectPoolSegment<T>> kvp in _segments)
//                {
//                    try
//                    {
//                        kvp.Value.Dispose();
//                    }
//                    catch (Exception) { }
//                }
//
//                _segments.Clear();
//            }
//        }
//    }
//
//    #endregion
}
