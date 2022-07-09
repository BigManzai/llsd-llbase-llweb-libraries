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
package com.ngt.jopenmetaverse.shared.sim;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import com.ngt.jopenmetaverse.shared.types.Action;
import com.ngt.jopenmetaverse.shared.types.Predicate;


/// <summary>
/// The InternalDictionary class is used through the library for storing key/value pairs.
/// It is intended to be a replacement for the generic Dictionary class and should 
/// be used in its place. It contains several methods for allowing access to the data from
/// outside the library that are read only and thread safe.
/// 
/// </summary>
/// <typeparam name="TKey">Key <see langword="Tkey"/></typeparam>
/// <typeparam name="TValue">Value <see langword="TValue"/></typeparam>
public class InternalDictionary<T, E> {
        /// <summary>Internal dictionary that this class wraps around. Do not
        /// modify or enumerate the contents of this dictionary without locking
        /// on this member</summary>
        private Map<T, E> Dictionary;

        /// <summary>
        /// Gets the number of Key/Value pairs contained in the <seealso cref="T:InternalDictionary"/>
        /// </summary>
        public int getCount() { return Dictionary.size(); } 

        /// <summary>
        /// Initializes a new instance of the <seealso cref="T:InternalDictionary"/> Class 
        /// with the specified key/value, has the default initial capacity.
        /// </summary>
        /// <example>
        /// <code>
        /// // initialize a new InternalDictionary named testDict with a string as the key and an int as the value.
        /// public InternalDictionary&lt;string, int&gt; testDict = new InternalDictionary&lt;string, int&gt;();
        /// </code>
        /// </example>
        public InternalDictionary()
        {
            Dictionary = new HashMap<T, E>();
        }

        /// <summary>
        /// Initializes a new instance of the <seealso cref="T:InternalDictionary"/> Class 
        /// with the specified key/value, has its initial valies copied from the specified 
        /// <seealso cref="T:System.Collections.Generic.Dictionary"/>
        /// </summary>
        /// <param name="dictionary"><seealso cref="T:System.Collections.Generic.Dictionary"/>
        /// to copy initial values from</param>
        /// <example>
        /// <code>
        /// // initialize a new InternalDictionary named testAvName with a UUID as the key and an string as the value.
        /// // populates with copied values from example KeyNameCache Dictionary.
        /// 
        /// // create source dictionary
        /// Dictionary&lt;UUID, string&gt; KeyNameCache = new Dictionary&lt;UUID, string&gt;();
        /// KeyNameCache.Add("8300f94a-7970-7810-cf2c-fc9aa6cdda24", "Jack Avatar");
        /// KeyNameCache.Add("27ba1e40-13f7-0708-3e98-5819d780bd62", "Jill Avatar");
        /// 
        /// // Initialize new dictionary.
        /// public InternalDictionary&lt;UUID, string&gt; testAvName = new InternalDictionary&lt;UUID, string&gt;(KeyNameCache);
        /// </code>
        /// </example>
        public InternalDictionary(Map<T, E> dictionary)
        {
            Dictionary = new HashMap<T, E>(dictionary);
        }

        /// <summary>
        /// Initializes a new instance of the <seealso cref="T:OpenMetaverse.InternalDictionary"/> Class 
        /// with the specified key/value, With its initial capacity specified.
        /// </summary>
        /// <param name="capacity">Initial size of dictionary</param>
        /// <example>
        /// <code>
        /// // initialize a new InternalDictionary named testDict with a string as the key and an int as the value, 
        /// // initially allocated room for 10 entries.
        /// public InternalDictionary&lt;string, int&gt; testDict = new InternalDictionary&lt;string, int&gt;(10);
        /// </code>
        /// </example>
        public InternalDictionary(int capacity)
        {
            Dictionary = new HashMap<T, E>(capacity);
        }

        
        public Map<T, E> getDictionary() {
			return Dictionary;
		}


		/// <summary>
        /// Try to get entry from <seealso cref="T:OpenMetaverse.InternalDictionary"/> with specified key 
        /// </summary>
        /// <param name="key">Key to use for lookup</param>
        /// <param name="value">Value returned</param>
        /// <returns><see langword="true"/> if specified key exists,  <see langword="false"/> if not found</returns>
        /// <example>
        /// <code>
        /// // find your avatar using the Simulator.ObjectsAvatars InternalDictionary:
        ///    Avatar av;
        ///    if (Client.Network.CurrentSim.ObjectsAvatars.TryGetValue(Client.Self.AgentID, out av))
        ///        Console.WriteLine("Found Avatar {0}", av.Name);
        /// </code>
        /// <seealso cref="Simulator.ObjectsAvatars"/>
        /// </example>
        public E  get(T key)
        {
            synchronized (Dictionary)
            {
                return Dictionary.get(key);
            }
        }

        /// <summary>
        /// Finds the specified match.
        /// </summary>
        /// <param name="match">The match.</param>
        /// <returns>Matched value</returns>
        /// <example>
        /// <code>
        /// // use a delegate to find a prim in the ObjectsPrimitives InternalDictionary
        /// // with the ID 95683496
        /// uint findID = 95683496;
        /// Primitive findPrim = sim.ObjectsPrimitives.Find(
        ///             delegate(Primitive prim) { return prim.ID == findID; });
        /// </code>
        /// </example>
        public E Find(Predicate<E> pred)
        {
            synchronized (Dictionary)
            {
                for (E value : Dictionary.values())
                {
                    if (pred.match(value))
                        return value;
                }
            }
            return null;
        }

        /// <summary>Find All items in an <seealso cref="T:InternalDictionary"/></summary>
        /// <param name="match">return matching items.</param>
        /// <returns>a <seealso cref="T:System.Collections.Generic.List"/> containing found items.</returns>
        /// <example>
        /// Find All prims within 20 meters and store them in a List
        /// <code>
        /// int radius = 20;
        /// List&lt;Primitive&gt; prims = Client.Network.CurrentSim.ObjectsPrimitives.FindAll(
        ///         delegate(Primitive prim) {
        ///             Vector3 pos = prim.Position;
        ///             return ((prim.ParentID == 0) &amp;&amp; (pos != Vector3.Zero) &amp;&amp; (Vector3.Distance(pos, location) &lt; radius));
        ///         }
        ///    ); 
        ///</code>
        ///</example>
        public List<E> FindAll(Predicate<E> pred)
        {
            List<E> found = new ArrayList<E>();
            
            synchronized (Dictionary)
            {
                for (E value : Dictionary.values())
                {
                    if (pred.match(value))
                    	found.add(value);
                }
            }
            return found;
        }
//
//        /// <summary>Find All items in an <seealso cref="T:InternalDictionary"/></summary>
//        /// <param name="match">return matching keys.</param>
//        /// <returns>a <seealso cref="T:System.Collections.Generic.List"/> containing found keys.</returns>
//        /// <example>
//        /// Find All keys which also exist in another dictionary
//        /// <code>
//        /// List&lt;UUID&gt; matches = myDict.FindAll(
//        ///         delegate(UUID id) {
//        ///             return myOtherDict.ContainsKey(id);
//        ///         }
//        ///    ); 
//        ///</code>
//        ///</example>
//        public List<TKey> FindAll(Predicate<TKey> match)
//        {
//            List<TKey> found = new List<TKey>();
//            lock (Dictionary)
//            {
//                foreach (KeyValuePair<TKey, TValue> kvp in Dictionary)
//                {
//                    if (match(kvp.Key))
//                        found.Add(kvp.Key);
//                }
//            }
//            return found;
//        }
//
//        /// <summary>Perform an <seealso cref="T:System.Action"/> on each entry in an <seealso cref="T:OpenMetaverse.InternalDictionary"/></summary>
//        /// <param name="action"><seealso cref="T:System.Action"/> to perform</param>
//        /// <example>
//        /// <code>
//        /// // Iterates over the ObjectsPrimitives InternalDictionary and prints out some information.
//        /// Client.Network.CurrentSim.ObjectsPrimitives.ForEach(
//        ///     delegate(Primitive prim)
//        ///     {
//        ///         if (prim.Text != null)
//        ///         {
//        ///             Console.WriteLine("NAME={0} ID = {1} TEXT = '{2}'", 
//        ///                 prim.PropertiesFamily.Name, prim.ID, prim.Text);
//        ///         }
//        ///     });
//        ///</code>
//        ///</example>
//        public void ForEach(Action<TValue> action)
//        {
//            lock (Dictionary)
//            {
//                foreach (TValue value in Dictionary.Values)
//                {
//                    action(value);
//                }
//            }
//        }
//
//        /// <summary>Perform an <seealso cref="T:System.Action"/> on each key of an <seealso cref="T:OpenMetaverse.InternalDictionary"/></summary>
//        /// <param name="action"><seealso cref="T:System.Action"/> to perform</param>
//        public void ForEach(Action<TKey> action)
//        {
//            lock (Dictionary)
//            {
//                foreach (TKey key in Dictionary.Keys)
//                {
//                    action(key);
//                }
//            }
//        }
//
//        /// <summary>
//        /// Perform an <seealso cref="T:System.Action"/> on each KeyValuePair of an <seealso cref="T:OpenMetaverse.InternalDictionary"/>
//        /// </summary>
//        /// <param name="action"><seealso cref="T:System.Action"/> to perform</param>
//        public void ForEach(Action<KeyValuePair<TKey, TValue>> action)
//        {
//            lock (Dictionary)
//            {
//                foreach (KeyValuePair<TKey, TValue> entry in Dictionary)
//                {
//                    action(entry);
//                }
//            }
//        }

        /// <summary>Check if Key exists in Dictionary</summary>
        /// <param name="key">Key to check for</param>
        /// <returns><see langword="true"/> if found, <see langword="false"/> otherwise</returns>
        public boolean containsKey(T key)
        {
            synchronized (Dictionary)
            {
            return Dictionary.containsKey(key);
            }
        }

        /// <summary>Check if Value exists in Dictionary</summary>
        /// <param name="value">Value to check for</param>
        /// <returns><see langword="true"/> if found, <see langword="false"/> otherwise</returns>
        public boolean containsValue(T value)
        {
            synchronized (Dictionary)
            {
            return Dictionary.containsValue(value);
            }
        }

        /// <summary>
        /// Adds the specified key to the dictionary, dictionary locking is not performed, 
        /// <see cref="SafeAdd"/>
        /// </summary>
        /// <param name="key">The key</param>
        /// <param name="value">The value</param>
        public void add(T key, E value)
        {
            synchronized (Dictionary)
            {
                Dictionary.put(key, value);
            }
        }

        /// <summary>
        /// Removes the specified key, dictionary locking is not performed
        /// </summary>
        /// <param name="key">The key.</param>
        /// <returns><see langword="true"/> if successful, <see langword="false"/> otherwise</returns>
        public E remove(T key)
        {
            synchronized (Dictionary)
            {
                return Dictionary.remove(key);
            }
        }
        
        public void foreach(Action<Entry<T, E>> action)
        {
            synchronized (Dictionary)
            {
            	for(Entry<T, E> entry: Dictionary.entrySet())
            	{
            		action.execute(entry);
            	}
            }        	
        }
}
