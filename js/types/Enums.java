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
package com.ngt.jopenmetaverse.shared.types;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

public class Enums
{
//    /// <summary>
//    /// Attribute class that allows extra attributes to be attached to ENUMs
//    /// </summary>
//    public class EnumInfoAttribute : Attribute
//    {
//        /// <summary>Text used when presenting ENUM to user</summary>
//        public string Text = string.Empty;
//
//        /// <summary>Default initializer</summary>
//        public EnumInfoAttribute() { }
//
//        /// <summary>Text used when presenting ENUM to user</summary>
//        public EnumInfoAttribute(string text)
//        {
//            this.Text = text;
//        }
//    }

    /// <summary>
    /// The different types of grid assets
    /// </summary>
    public static enum AssetType
    {
        /// <summary>Unknown asset type</summary>
        Unknown ((byte)-1),
        /// <summary>Texture asset, stores in JPEG2000 J2C stream format</summary>
        Texture ((byte)0),
        /// <summary>Sound asset</summary>
        Sound ((byte)1),
        /// <summary>Calling card for another avatar</summary>
        CallingCard ((byte)2),
        /// <summary>Link to a location in world</summary>
        Landmark ((byte)3),
        // <summary>Legacy script asset, you should never see one of these</summary>
        //[Obsolete]
        //Script = 4,
        /// <summary>Collection of textures and parameters that can be 
        /// worn by an avatar</summary>
        Clothing ((byte)5),
        /// <summary>Primitive that can contain textures, sounds, 
        /// scripts and more</summary>
        Object ((byte)6),
        /// <summary>Notecard asset</summary>
        Notecard ((byte)7),
        /// <summary>Holds a collection of inventory items</summary>
        Folder ((byte)8),
        /// <summary>Root inventory folder</summary>
        RootFolder ((byte)9),
        /// <summary>Linden scripting language script</summary>
        LSLText ((byte)10),
        /// <summary>LSO bytecode for a script</summary>
        LSLBytecode ((byte)11),
        /// <summary>Uncompressed TGA texture</summary>
        TextureTGA ((byte)12),
        /// <summary>Collection of textures and shape parameters that can
        /// be worn</summary>
        Bodypart ((byte)13),
        /// <summary>Trash folder</summary>
        TrashFolder ((byte)14),
        /// <summary>Snapshot folder</summary>
        SnapshotFolder ((byte)15),
        /// <summary>Lost and found folder</summary>
        LostAndFoundFolder ((byte)16),
        /// <summary>Uncompressed sound</summary>
        SoundWAV ((byte)17),
        /// <summary>Uncompressed TGA non-square image, not to be used as a
        /// texture</summary>
        ImageTGA ((byte)18),
        /// <summary>Compressed JPEG non-square image, not to be used as a
        /// texture</summary>
        ImageJPEG ((byte)19),
        /// <summary>Animation</summary>
        Animation ((byte)20),
        /// <summary>Sequence of animations, sounds, chat, and pauses</summary>
        Gesture ((byte)21),
        /// <summary>Simstate file</summary>
        Simstate ((byte)22),
        /// <summary>Contains landmarks for favorites</summary>
        FavoriteFolder ((byte)23),
        /// <summary>Asset is a link to another inventory item</summary>
        Link ((byte)24),
        /// <summary>Asset is a link to another inventory folder</summary>
        LinkFolder ((byte)25),
        /// <summary>Beginning of the range reserved for ensembles</summary>
        EnsembleStart ((byte)26),
        /// <summary>End of the range reserved for ensembles</summary>
        EnsembleEnd ((byte)45),
        /// <summary>Folder containing inventory links to wearables and attachments
        /// that are part of the current outfit</summary>
        CurrentOutfitFolder ((byte)46),
        /// <summary>Folder containing inventory items or links to
        /// inventory items of wearables and attachments
        /// together make a full outfit</summary>
        OutfitFolder ((byte)47),
        /// <summary>Root folder for the folders of type OutfitFolder</summary>
        MyOutfitsFolder ((byte)48),
        /// <summary>Linden mesh format</summary>
        Mesh ((byte)49);
        
        		private byte index;
        		AssetType(byte index)
        		{
        			this.index = index;
        		}     

        		public byte getIndex()
        		{
        			return index;
        		}
          		private static final Map<Byte,AssetType> lookup  = new HashMap<Byte,AssetType>();

        		static {
        			for(AssetType s : EnumSet.allOf(AssetType.class))
        				lookup.put(s.getIndex(), s);
        		}

        		public static AssetType get(Byte index)
        		{
        			return lookup.get(index);
        		}	
        
    }

    /// <summary>
    /// Inventory Item Types, eg Script, Notecard, Folder, etc
    /// </summary>
    public enum InventoryType 
    {
        /// <summary>Unknown</summary>
        Unknown ((byte)-1),
        /// <summary>Texture</summary>
        Texture ((byte)0),
        /// <summary>Sound</summary>
        Sound ((byte)1),
        /// <summary>Calling Card</summary>
        CallingCard ((byte)2),
        /// <summary>Landmark</summary>
        Landmark ((byte)3),
        /*
        /// <summary>Script</summary>
        //[Obsolete("See LSL")] Script ((byte)4,
        /// <summary>Clothing</summary>
        //[Obsolete("See Wearable")] Clothing ((byte)5,
        /// <summary>Object, both single and coalesced</summary>
         */
        Object ((byte)6),
        /// <summary>Notecard</summary>
        Notecard ((byte)7),
        /// <summary></summary>
        Category ((byte)8),
        /// <summary>Folder</summary>
        Folder ((byte)8),
        /// <summary></summary>
        RootCategory ((byte)9),
        /// <summary>an LSL Script</summary>
        LSL ((byte)10),
        /*
        /// <summary></summary>
        //[Obsolete("See LSL")] LSLBytecode = 11,
        /// <summary></summary>
        //[Obsolete("See Texture")] TextureTGA = 12,
        /// <summary></summary>
        //[Obsolete] Bodypart = 13,
        /// <summary></summary>
        //[Obsolete] Trash = 14,
         */
        /// <summary></summary>
        Snapshot ((byte)15),
        /*
        /// <summary></summary>
        //[Obsolete] LostAndFound = 16,
         */
        /// <summary></summary>
        Attachment ((byte)17),
        /// <summary></summary>
        Wearable ((byte)18),
        /// <summary></summary>
        Animation ((byte)19),
        /// <summary></summary>
        Gesture ((byte)20),

        /// <summary></summary>
        Mesh ((byte)22);
        
        		private byte index;
        InventoryType(byte index)
        		{
        			this.index = index;
        		}     

        		public byte getIndex()
        		{
        			return index;
        		}
        		private static final Map<Byte,InventoryType> lookup  = new HashMap<Byte,InventoryType>();

        		static {
        			for(InventoryType s : EnumSet.allOf(InventoryType.class))
        				lookup.put(s.getIndex(), s);
        		}

        		public static InventoryType get(Byte index)
        		{
        			return lookup.get(index);
        		}	
    }

    /// <summary>
    /// Item Sale Status
    /// </summary>
    public enum SaleType 
    {
        /// <summary>Not for sale</summary>
        Not ((byte)0),
        /// <summary>The original is for sale</summary>
        Original ((byte)1),
        /// <summary>Copies are for sale</summary>
        Copy ((byte)2),
        /// <summary>The contents of the object are for sale</summary>
        Contents ((byte)3);
        
  		private byte index;
  		SaleType(byte index)
    		{
    			this.index = index;
    		}     

    		public byte getIndex()
    		{
    			return index;
    		}
    		
    		private static final Map<Byte,SaleType> lookup  = new HashMap<Byte,SaleType>();

    		static {
    			for(SaleType s : EnumSet.allOf(SaleType.class))
    				lookup.put(s.getIndex(), s);
    		}

    		public static SaleType get(Byte index)
    		{
    			return lookup.get(index);
    		}	
    		
    }

    /// <summary>
    /// Types of wearable assets
    /// </summary>
    public enum WearableType 
    {
        /// <summary>Body shape</summary>
        Shape ((byte)0),
        /// <summary>Skin textures and attributes</summary>
        Skin ((byte)1),
        /// <summary>Hair</summary>
        Hair ((byte)2),
        /// <summary>Eyes</summary>
        Eyes ((byte)3),
        /// <summary>Shirt</summary>
        Shirt ((byte)4),
        /// <summary>Pants</summary>
        Pants ((byte)5),
        /// <summary>Shoes</summary>
        Shoes ((byte)6),
        /// <summary>Socks</summary>
        Socks ((byte)7),
        /// <summary>Jacket</summary>
        Jacket ((byte)8),
        /// <summary>Gloves</summary>
        Gloves ((byte)9),
        /// <summary>Undershirt</summary>
        Undershirt ((byte)10),
        /// <summary>Underpants</summary>
        Underpants ((byte)11),
        /// <summary>Skirt</summary>
        Skirt ((byte)12),
        /// <summary>Alpha mask to hide parts of the avatar</summary>
        Alpha ((byte)13),
        /// <summary>Tattoo</summary>
        Tattoo ((byte)14),
        /// <summary>Physics</summary>
        Physics ((byte)15),
        /// <summary>Invalid wearable asset</summary>
        Invalid ((byte)255);
        
          		private byte index;
          		WearableType(byte index)
            		{
            			this.index = index;
            		}     

            		public byte getIndex()
            		{
            			return index;
            		}
            		private static final Map<Byte,WearableType> lookup  = new HashMap<Byte,WearableType>();

            		static {
            			for(WearableType s : EnumSet.allOf(WearableType.class))
            				lookup.put(s.getIndex(), s);
            		}

            		public static WearableType get(Byte index)
            		{
            			return lookup.get(index);
            		}	
    }
}
