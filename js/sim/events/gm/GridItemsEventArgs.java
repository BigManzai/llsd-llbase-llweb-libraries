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
package com.ngt.jopenmetaverse.shared.sim.events.gm;

import java.util.List;

import com.ngt.jopenmetaverse.shared.sim.GridManager.GridItemType;
import com.ngt.jopenmetaverse.shared.sim.GridManager.MapItem;
import com.ngt.jopenmetaverse.shared.sim.events.EventArgs;

public class GridItemsEventArgs extends EventArgs
{
    private  GridItemType m_Type;
    private  List<MapItem> m_Items;

    public GridItemType getType() { return m_Type; } 
    public List<MapItem> getItems() { return m_Items; } 

    public GridItemsEventArgs(GridItemType type, List<MapItem> items)
    {
        this.m_Type = type;
        this.m_Items = items;
    }
}
