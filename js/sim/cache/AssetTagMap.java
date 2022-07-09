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
package com.ngt.jopenmetaverse.shared.sim.cache;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.SortedMap;
import java.util.TreeMap;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class AssetTagMap {
    private Map<String, Long> assets = new HashMap<String, Long>();    
    private SortedMap<Long, List<String>> timestamps = new TreeMap<Long, List<String>>();
    
    public AssetTagMap()
    { }
    
    public synchronized void assetAdded(String assetID)
    {	
    	Long currentTime = Utils.getUnixTime();
    	if(assets.containsKey(assetID))
    	{
    		long timestamp = assets.get(assetID);
    		List<String> tmpAssetIds = timestamps.get(timestamp);
    		tmpAssetIds.remove(assetID);
    		if(tmpAssetIds.isEmpty())
    			timestamps.remove(timestamp);
    	}
    	
    	if(!timestamps.containsKey(currentTime))
    		timestamps.put(currentTime, new ArrayList<String>());
    	
		assets.put(assetID, currentTime);
		timestamps.get(currentTime).add(assetID);
    }

    public synchronized void assetRemoved(String assetID)
    {
    	if(assets.containsKey(assetID))
    	{
    		long timestamp = assets.get(assetID);
    		List<String> tmpAssetIds = timestamps.get(timestamp);
    		tmpAssetIds.remove(assetID);
    		if(tmpAssetIds.isEmpty())
    			timestamps.remove(timestamp);
    		
    		assets.remove(assetID);
    	}
    }    
    
    public synchronized void assetAccessed(String assetID)
    {
    	assetAdded(assetID);
    } 
    
	public synchronized List<String> getAssets()
    {
		List<String> list = new ArrayList<String>();

		for(Entry<Long, List<String>> e :timestamps.entrySet())
		{
			System.out.println(String.format("Adding Assets= %d for timestamp %d", e.getValue().size(), e.getKey()));
			list.addAll(e.getValue());
		}
    	 return list;
    }
 
    public synchronized void clear()
    {
    	assets.clear();
    	timestamps.clear();
    }
}