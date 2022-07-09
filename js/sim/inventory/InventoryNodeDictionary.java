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

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import com.ngt.jopenmetaverse.shared.types.UUID;

public class InventoryNodeDictionary 
{
	  protected Map<UUID, InventoryNode> Dictionary = new HashMap<UUID, InventoryNode>();
      protected InventoryNode parent;
      protected Object syncRoot = new Object();

      public InventoryNode getParent()
      {
    	  return parent; 
    	  }
      public void setParent(InventoryNode value)
      {
        parent = value; 
      }

      public Object getSyncRoot() { return syncRoot; } 

      public int size() { return Dictionary.size(); } 

      public InventoryNodeDictionary(InventoryNode parentNode)
      {
          parent = parentNode;
      }

      public InventoryNode get(UUID key)
      {
          return (InventoryNode)this.Dictionary.get(key); 
      }
      
      public void put(UUID key, InventoryNode value)
      {
              value.setParent(parent);
              synchronized (syncRoot) 
              {this.Dictionary.put(key, value);}
      }

      public Set<UUID> keySet() { return this.Dictionary.keySet(); }
      public Collection<InventoryNode> values() { return this.Dictionary.values(); } 


      public void remove(UUID key)
      {
    	  synchronized (syncRoot) 
    	  {this.Dictionary.remove(key);}
      }

      public boolean containsKey(UUID key)
      {
          return this.Dictionary.containsKey(key);
      }
}
