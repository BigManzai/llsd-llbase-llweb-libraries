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
package com.ngt.jopenmetaverse.shared.sim.inventory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.exception.nm.InventoryException;
import com.ngt.jopenmetaverse.shared.sim.GridClient;
import com.ngt.jopenmetaverse.shared.sim.InventoryManager;
import com.ngt.jopenmetaverse.shared.sim.InventoryManager.InventoryFolder;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.EventObservable;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.im.InventoryObjectAddedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.im.InventoryObjectRemovedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.im.InventoryObjectUpdatedEventArgs;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.PersistentStorage;
import com.ngt.jopenmetaverse.shared.util.Utils;

    /// <summary>
    /// Responsible for maintaining inventory structure. Inventory constructs nodes
    /// and manages node children as is necessary to maintain a coherant hirarchy.
    /// Other classes should not manipulate or create InventoryNodes explicitly. When
    /// A node's parent changes (when a folder is moved, for example) simply pass
    /// Inventory the updated InventoryFolder and it will make the appropriate changes
    /// to its internal representation.
    /// </summary>
    public class Inventory
    {
     
    	private EventObservable<InventoryObjectUpdatedEventArgs> onInventoryObjectUpdated = new EventObservable<InventoryObjectUpdatedEventArgs>();
    	public void registerOnInventoryObjectUpdated(EventObserver<InventoryObjectUpdatedEventArgs> o)
    	{
    		onInventoryObjectUpdated.addObserver(o);
    	}
    	public void unregisterOnInventoryObjectUpdated(EventObserver<InventoryObjectUpdatedEventArgs> o) 
    	{
    		onInventoryObjectUpdated.deleteObserver(o);
    	}
    	
    	private EventObservable<InventoryObjectRemovedEventArgs> onInventoryObjectRemoved = new EventObservable<InventoryObjectRemovedEventArgs>();
    	public void registerOnInventoryObjectRemoved(EventObserver<InventoryObjectRemovedEventArgs> o)
    	{
    		onInventoryObjectRemoved.addObserver(o);
    	}
    	public void unregisterOnInventoryObjectRemoved(EventObserver<InventoryObjectRemovedEventArgs> o) 
    	{
    		onInventoryObjectRemoved.deleteObserver(o);
    	}
    	
    	private EventObservable<InventoryObjectAddedEventArgs> onInventoryObjectAdded = new EventObservable<InventoryObjectAddedEventArgs>();
    	public void registerOnInventoryObjectAdded(EventObserver<InventoryObjectAddedEventArgs> o)
    	{
    		onInventoryObjectAdded.addObserver(o);
    	}
    	public void unregisterOnInventoryObjectAdded(EventObserver<InventoryObjectAddedEventArgs> o) 
    	{
    		onInventoryObjectAdded.deleteObserver(o);
    	} 	   	
    	//TODO Need to implement
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<InventoryObjectUpdatedEventArgs> m_InventoryObjectUpdated;
//
//        ///<summary>Raises the InventoryObjectUpdated Event</summary>
//        /// <param name="e">A InventoryObjectUpdatedEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnInventoryObjectUpdated(InventoryObjectUpdatedEventArgs e)
//        {
//            EventHandler<InventoryObjectUpdatedEventArgs> handler = m_InventoryObjectUpdated;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private object m_InventoryObjectUpdatedLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// ...</summary>
//        public event EventHandler<InventoryObjectUpdatedEventArgs> InventoryObjectUpdated 
//        {
//            add { lock (m_InventoryObjectUpdatedLock) { m_InventoryObjectUpdated += value; } }
//            remove { lock (m_InventoryObjectUpdatedLock) { m_InventoryObjectUpdated -= value; } }
//        }
//       
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<InventoryObjectRemovedEventArgs> m_InventoryObjectRemoved;
//
//        ///<summary>Raises the InventoryObjectRemoved Event</summary>
//        /// <param name="e">A InventoryObjectRemovedEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnInventoryObjectRemoved(InventoryObjectRemovedEventArgs e)
//        {
//            EventHandler<InventoryObjectRemovedEventArgs> handler = m_InventoryObjectRemoved;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private object m_InventoryObjectRemovedLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// ...</summary>
//        public event EventHandler<InventoryObjectRemovedEventArgs> InventoryObjectRemoved 
//        {
//            add { lock (m_InventoryObjectRemovedLock) { m_InventoryObjectRemoved += value; } }
//            remove { lock (m_InventoryObjectRemovedLock) { m_InventoryObjectRemoved -= value; } }
//        }
//        
//        /// <summary>The event subscribers, null of no subscribers</summary>
//        private EventHandler<InventoryObjectAddedEventArgs> m_InventoryObjectAdded;
//
//        ///<summary>Raises the InventoryObjectAdded Event</summary>
//        /// <param name="e">A InventoryObjectAddedEventArgs object containing
//        /// the data sent from the simulator</param>
//        protected virtual void OnInventoryObjectAdded(InventoryObjectAddedEventArgs e)
//        {
//            EventHandler<InventoryObjectAddedEventArgs> handler = m_InventoryObjectAdded;
//            if (handler != null)
//                handler(this, e);
//        }
//
//        /// <summary>Thread sync lock object</summary>
//        private object m_InventoryObjectAddedLock = new object();
//
//        /// <summary>Raised when the simulator sends us data containing
//        /// ...</summary>
//        public event EventHandler<InventoryObjectAddedEventArgs> InventoryObjectAdded 
//        {
//            add { lock (m_InventoryObjectAddedLock) { m_InventoryObjectAdded += value; } }
//            remove { lock (m_InventoryObjectAddedLock) { m_InventoryObjectAdded -= value; } }
//        }
       
        /// <summary>
        /// The root folder of this avatars inventory
        /// </summary>
        public InventoryFolder getRootFolder()
        {
           return (InventoryFolder) _RootNode.getData();
        }
        
        public void setRootFolder(InventoryFolder value)
            {
                UpdateNodeFor(value);
                _RootNode = Items.get(value.UUID);
                JLogger.debug("Found Root Node : " + contains(value.UUID) + _RootNode.getData().UUID.toString());
            }

        /// <summary>
        /// The default shared library folder
        /// </summary>
        public InventoryFolder getLibraryFolder()
        {
           return (InventoryFolder)_LibraryRootNode.getData();
        }
        
        public void setLibraryFolder(InventoryFolder value)
            {
                UpdateNodeFor(value);
                _LibraryRootNode = Items.get(value.UUID);
            }

        private InventoryNode _LibraryRootNode;
        private InventoryNode _RootNode;
        
        /// <summary>
        /// The root node of the avatars inventory
        /// </summary>
        public InventoryNode getRootNode()
        {
            return _RootNode;
        }

        /// <summary>
        /// The root node of the default shared library
        /// </summary>
        public InventoryNode getLibraryRootNode()
        {
            return _LibraryRootNode;
        }

        public UUID getOwner() {
            return _Owner;
        }

        private UUID _Owner;

        private GridClient Client;
        //private InventoryManager Manager;
        public Map<UUID, InventoryNode> Items = new HashMap<UUID, InventoryNode>();

        public Inventory(GridClient client, InventoryManager manager)
        {
        	 this(client, manager, client.self.getAgentID());
        }

        public Inventory(GridClient client, InventoryManager manager, UUID owner)
        {
            Client = client;
            //Manager = manager;
            _Owner = owner;
            if (owner.equals(UUID.Zero))
                JLogger.warn("Inventory owned by nobody!");
            Items = new HashMap<UUID, InventoryNode>();
        }

        public List<InventoryBase> GetContents(InventoryFolder folder) throws InventoryException
        {
            return GetContents(folder.UUID);
        }

        /// <summary>
        /// Returns the contents of the specified folder
        /// </summary>
        /// <param name="folder">A folder's UUID</param>
        /// <returns>The contents of the folder corresponding to <code>folder</code></returns>
        /// <exception cref="InventoryException">When <code>folder</code> does not exist in the inventory</exception>
        public List<InventoryBase> GetContents(UUID folder) throws InventoryException
        {
            InventoryNode folderNode;
            if ((folderNode = Items.get(folder))==null)
                throw new InventoryException("Unknown folder: " + folder);
            synchronized (folderNode.getNodes().syncRoot)
            {
                List<InventoryBase> contents = new ArrayList<InventoryBase>(folderNode.getNodes().size());
                for (InventoryNode node : folderNode.getNodes().values())
                {
                    contents.add(node.getData());
                }
                return contents;
            }
        }

        /// <summary>
        /// Updates the state of the InventoryNode and inventory data structure that
        /// is responsible for the InventoryObject. If the item was previously not added to inventory,
        /// it adds the item, and updates structure accordingly. If it was, it updates the 
        /// InventoryNode, changing the parent node if <code>item.parentUUID</code> does 
        /// not match <code>node.Parent.Data.UUID</code>.
        /// 
        /// You can not set the inventory root folder using this method
        /// </summary>
        /// <param name="item">The InventoryObject to store</param>
        public void UpdateNodeFor(InventoryBase item)
        {
            synchronized (Items)
            {
                InventoryNode itemParent = null;
                if (!item.ParentUUID.equals(UUID.Zero) && 
                		((itemParent = Items.get(item.ParentUUID)) == null))
                {
                    // OK, we have no data on the parent, let's create a fake one.
                    InventoryFolder fakeParent = new InventoryFolder(item.ParentUUID);
                    fakeParent.DescendentCount = 1; // Dear god, please forgive me.
                    itemParent = new InventoryNode(fakeParent);
                    Items.put(item.ParentUUID, itemParent);
                    // Unfortunately, this breaks the nice unified tree
                    // while we're waiting for the parent's data to come in.
                    // As soon as we get the parent, the tree repairs itself.
                    //Logger.DebugLog("Attempting to update inventory child of " +
                    //    item.ParentUUID.ToString() + " when we have no local reference to that folder", Client);

                    if (Client.settings.FETCH_MISSING_INVENTORY)
                    {
                        // Fetch the parent
                        List<UUID> fetchreq = new ArrayList<UUID>(1);
                        fetchreq.add(item.ParentUUID);                        
                    }
                }

                InventoryNode itemNode;
                if ((itemNode = Items.get(item.UUID)) != null) // We're updating.
                {
                    InventoryNode oldParent = itemNode.getParent();
                    // Handle parent change
                    if (oldParent == null || itemParent == null 
                    		|| itemParent.getData().UUID.equals(oldParent.getData().UUID))
                    {
                        if (oldParent != null)
                        {
                            synchronized (oldParent.getNodes().syncRoot)
                            { oldParent.getNodes().remove(item.UUID);}
                        }
                        if (itemParent != null)
                        {
                            synchronized (itemParent.getNodes().syncRoot)
                                {itemParent.getNodes().put(item.UUID, itemNode);}
                        }
                    }

                    itemNode.setParent(itemParent);

                    if (onInventoryObjectUpdated != null)
                    {
                        onInventoryObjectUpdated.raiseEvent(new InventoryObjectUpdatedEventArgs(itemNode.getData(), item));
                    }

                    itemNode.setData(item);
                }
                else // We're adding.
                {
                    itemNode = new InventoryNode(item, itemParent);
                    Items.put(item.UUID, itemNode);
                    JLogger.debug("Adding Inventory Node: " + item.UUID.toString());
                    if (onInventoryObjectAdded != null)
                    {
                        onInventoryObjectAdded.raiseEvent(new InventoryObjectAddedEventArgs(item));
                    }
                }
            }
        }

        public InventoryNode GetNodeFor(UUID uuid)
        {
            return Items.get(uuid);
        }

        /// <summary>
        /// Removes the InventoryObject and all related node data from Inventory.
        /// </summary>
        /// <param name="item">The InventoryObject to remove.</param>
        public void RemoveNodeFor(InventoryBase item)
        {
            synchronized (Items)
            {
                InventoryNode node;
                if ((node = Items.get(item.UUID)) != null)
                {
                    if (node.getParent() != null)
                        synchronized (node.getParent().getNodes().syncRoot)
                            {node.getParent().getNodes().remove(item.UUID);}
                    
                    Items.remove(item.UUID);
                    if (onInventoryObjectRemoved != null)
                    {
                        onInventoryObjectRemoved.raiseEvent(new InventoryObjectRemovedEventArgs(item));
                    }                    
                }

                // In case there's a new parent:
                InventoryNode newParent;
                if ((newParent= Items.get(item.ParentUUID))!=null)
                {
                    synchronized (newParent.getNodes().syncRoot)
                        {newParent.getNodes().remove(item.UUID);}
                }
            }
        }

        /// <summary>
        /// Used to find out if Inventory contains the InventoryObject
        /// specified by <code>uuid</code>.
        /// </summary>
        /// <param name="uuid">The UUID to check.</param>
        /// <returns>true if inventory contains uuid, false otherwise</returns>
        public boolean contains(UUID uuid)
        {
            return Items.containsKey(uuid);
        }

        public void printItems()
        {
        	JLogger.debug("Printing Inventory Items: ");
        	for(Entry<UUID, InventoryNode> e: Items.entrySet())
        	{
        		JLogger.debug(e.getKey().toString() + " ==> "  + e.getValue().getData().UUID.toString());
        	}
        			
        }
        
        public boolean contains(InventoryBase obj)
        {
            return contains(obj.UUID);
        }

        /// <summary>
        /// Saves the current inventory structure to a cache file
        /// </summary>
        /// <param name="filename">Name of the cache file to save to</param>
        public void SaveToDisk(String filename)
        {
	        try
	        {
//                using (Stream stream = File.Open(filename, FileMode.Create))
//                {
//                    BinaryFormatter bformatter = new BinaryFormatter();
//                    synchronized (Items)
//                    {
//                        Logger.Log("Caching " + Items.Count.ToString() + " inventory items to " + filename, Helpers.LogLevel.Info);
//                        foreach (KeyValuePair<UUID, InventoryNode> kvp in Items)
//                        {
//                            bformatter.Serialize(stream, kvp.Value);
//                        }
//                    }
//                }
	        	PersistentStorage.serializeObject(Items.values(), filename);
	        }
            catch (Exception e)
            {
                JLogger.error("Error saving inventory cache to disk :"+Utils.getExceptionStackTraceAsString(e));
            }
        }

        /// <summary>
        /// Loads in inventory cache file into the inventory structure. Note only valid to call after login has been successful.
        /// </summary>
        /// <param name="filename">Name of the cache file to load</param>
        /// <returns>The number of inventory items sucessfully reconstructed into the inventory node tree</returns>
        public int RestoreFromDisk(String filename)
        {
            Collection<InventoryNode> nodes = new ArrayList<InventoryNode>();
            int item_count = 0;

            try
            {
//                if (!File.Exists(filename))
//                    return -1;
//
//                using (Stream stream = File.Open(filename, FileMode.Open))
//                {
//                    BinaryFormatter bformatter = new BinaryFormatter();
//
//                    while (stream.Position < stream.Length)
//                    {
//                        OpenMetaverse.InventoryNode node = (InventoryNode)bformatter.Deserialize(stream);
//                        nodes.Add(node);
//                        item_count++;
//                    }
//                }
            	nodes = (Collection<InventoryNode> )PersistentStorage.deserializeObject(filename);
            	item_count = nodes.size();
            }
            catch (Exception e)
            {
                JLogger.error("Error accessing inventory cache file :"+Utils.getExceptionStackTraceAsString(e));
                return -1;
            }

            JLogger.info("Read " + item_count + " items from inventory cache file");

            item_count = 0;
            List<InventoryNode> del_nodes = new ArrayList<InventoryNode>(); //nodes that we have processed and will delete
            List<UUID> dirty_folders = new ArrayList<UUID>(); // Tainted folders that we will not restore items into

            // Because we could get child nodes before parents we must itterate around and only add nodes who have
            // a parent already in the list because we must update both child and parent to link together
            // But sometimes we have seen orphin nodes due to bad/incomplete data when caching so we have an emergency abort route
            int stuck = 0;
            
            while (nodes.size() != 0 && stuck<5)
            {
                for (InventoryNode node : nodes)
                {
                    InventoryNode pnode;
                    if (node.getParentID().equals(UUID.Zero))
                    {
                        //We don't need the root nodes "My Inventory" etc as they will already exist for the correct
                        // user of this cache.
                        del_nodes.add(node);
                        item_count--;
                    }
                    else if((pnode= Items.get(node.getData().UUID))!=null)
                    {
                        //We already have this it must be a folder
                        if (node.getData() instanceof InventoryFolder)
                        {
                            InventoryFolder cache_folder = (InventoryFolder)node.getData();
                            InventoryFolder server_folder = (InventoryFolder)pnode.getData();

                            if (cache_folder.Version != server_folder.Version)
                            {
                                JLogger.debug("Inventory Cache/Server version mismatch on " + node.getData().Name + " " + cache_folder.Version + " vs " + server_folder.Version);
                                pnode.setNeedsUpdate(true);
                                dirty_folders.add(node.getData().UUID);
                            }
                            else
                            {
                                pnode.setNeedsUpdate(false);
                            }

                            del_nodes.add(node);
                        }
                    }
                    else if ((pnode = Items.get(node.getParentID()))!=null)
                    {
                        if (node.getData() != null)
                        {
                            // If node is folder, and it does not exist in skeleton, mark it as 
                            // dirty and don't process nodes that belong to it
                            if (node.getData() instanceof InventoryFolder && !(Items.containsKey(node.getData().UUID)))
                            {
                                dirty_folders.add(node.getData().UUID);
                            }

                            //Only add new items, this is most likely to be run at login time before any inventory
                            //nodes other than the root are populated. Don't add non existing folders.
                            if (!Items.containsKey(node.getData().UUID) && !dirty_folders.contains(pnode.getData().UUID) && !(node.getData() instanceof InventoryFolder))
                            {
                                Items.put(node.getData().UUID, node);
                                node.setParent(pnode); //Update this node with its parent
                                pnode.getNodes().put(node.getData().UUID, node); // Add to the parents child list
                                item_count++;
                            }
                        }

                        del_nodes.add(node);
                    }
                }

                if (del_nodes.size() == 0)
                    stuck++;
                else
                    stuck = 0;

                //Clean up processed nodes this loop around.
                for (InventoryNode node : del_nodes)
                    nodes.remove(node);

                del_nodes.clear();
            }

            JLogger.info("Reassembled " + item_count + " items from inventory cache file");
            return item_count;
        }

        //region Operators

        /// <summary>
        /// By using the bracket operator on this class, the program can get the 
        /// InventoryObject designated by the specified uuid. If the value for the corresponding
        /// UUID is null, the call is equivelant to a call to <code>RemoveNodeFor(this[uuid])</code>.
        /// If the value is non-null, it is equivelant to a call to <code>UpdateNodeFor(value)</code>,
        /// the uuid parameter is ignored.
        /// </summary>
        /// <param name="uuid">The UUID of the InventoryObject to get or set, ignored if set to non-null value.</param>
        /// <returns>The InventoryObject corresponding to <code>uuid</code>.</returns>
        public InventoryBase get(UUID uuid)
        {
                InventoryNode node = Items.get(uuid);
                return node.getData();
        }
        
        public void put(UUID uuid, InventoryBase value) 
        {
                if (value != null)
                {
                	JLogger.debug(String.format("Adding Inventory Item: Name=%s %s", value.Name, value.UUID.toString() ));
                    // Log a warning if there is a UUID mismatch, this will cause problems
                    if (!value.UUID.equals(uuid))
                        JLogger.warn("Inventory[uuid]: uuid " + uuid.toString() + " is not equal to value.UUID " +
                            value.UUID.toString());

                    UpdateNodeFor(value);
                }
                else
                {
                	JLogger.debug("Deleting Inventory Item");
                    InventoryNode node;
                    if ((node = Items.get(uuid))!=null)
                    {
                        RemoveNodeFor(node.getData());
                    }
                }
        }
        //endregion Operators
}
