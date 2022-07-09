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
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import com.ngt.jopenmetaverse.shared.protocol.primitives.Primitive;
import com.ngt.jopenmetaverse.shared.types.UUID;

public class PrimitiveDictionary extends InternalDictionary<Long, Primitive>
{
    private Map<Long, Set<Primitive>> parentPrimitives;
    private Map<UUID, Primitive> primitivesByID;
    
    
	public PrimitiveDictionary() {
		super();
		parentPrimitives = new HashMap<Long, Set<Primitive>>();
		primitivesByID  = new HashMap<UUID, Primitive>();
	}

	public PrimitiveDictionary(int capacity) {
		super(capacity);
		parentPrimitives = new HashMap<Long, Set<Primitive>>(capacity);
		primitivesByID  = new HashMap<UUID, Primitive>(capacity);
	}

	public PrimitiveDictionary(Map<Long, Primitive> dictionary) 
	{
		super(dictionary);
		parentPrimitives = new HashMap<Long, Set<Primitive>>();
		primitivesByID  = new HashMap<UUID, Primitive>();
		
		//Index the parent and children for faster searching
		for(Entry<Long, Primitive> e: dictionary.entrySet())
		{
			addChild(e.getValue());
		}
	}
	
	@Override
    public void add(Long key, Primitive value)
	{
		super.add(key, value);
		synchronized(primitivesByID)
		{
			primitivesByID.put(value.ID, value);
		}
		addChild(value);
	}
	
	@Override
    public Primitive remove(Long key)
	{
		Primitive p = super.remove(key);
		synchronized(primitivesByID)
		{
			primitivesByID.remove(p.ID);
		}
		removeChild(p);
		return p;
	}
	
	public Primitive get(UUID id)
	{
		synchronized(primitivesByID)
		{
			return primitivesByID.get(id);
		}
	}
	
	/*
	 * @return list of Primitives. If not children exist, en empty list is returned
	 */
	public List<Primitive> getChildren(Long parentID)
	{
		synchronized(parentPrimitives)
		{
			if(parentPrimitives.containsKey(parentID))
			{
				return new ArrayList<Primitive>(parentPrimitives.get(parentID));
			}
		}
		return new ArrayList<Primitive>();
	}
	

	/*
	 * Add a child to the parent, if exist the parent and child is already not added
	 */
	protected void removeChild(Primitive p)
	{
		synchronized(parentPrimitives)
		{
//			//if it does have a parent
//			if(p.ParentID != 0)
//			{
				//if the child collection is null
				if(parentPrimitives.containsKey(p.ParentID))
				{
					//if child is already not there
					if(!parentPrimitives.get(p.ParentID).contains(p))
						parentPrimitives.get(p.ParentID).remove(p);
				}
//			}
		}
	}
	
	/*
	 * Add a child to the parent, if exist the parent and child is already not added
	 */
	protected void addChild(Primitive p)
	{
		synchronized(parentPrimitives)
		{
			//if it does have a parent
//			if(p.ParentID != 0)
//			{
				//if the child collection is null
				if(!parentPrimitives.containsKey(p.ParentID))
					parentPrimitives.put(p.ParentID, new HashSet<Primitive>());

				//if child is already not there
				if(!parentPrimitives.get(p.ParentID).contains(p))
					parentPrimitives.get(p.ParentID).add(p);	
//			}
		}
	}
	
	
}
