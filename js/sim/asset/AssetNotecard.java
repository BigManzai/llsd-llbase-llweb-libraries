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
package com.ngt.jopenmetaverse.shared.sim.asset;

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions.PermissionMask;
import com.ngt.jopenmetaverse.shared.sim.InventoryManager;
import com.ngt.jopenmetaverse.shared.sim.inventory.InventoryItem;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.Enums.InventoryType;
import com.ngt.jopenmetaverse.shared.types.Enums.SaleType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
  /// <summary>
    /// Represents a string of characters encoded with specific formatting properties
    /// </summary>
    public class AssetNotecard extends Asset
    {
        /// <summary>Override the base classes AssetType</summary>
        @Override
	public AssetType getAssetType() { return AssetType.Notecard; } 

        /// <summary>A text string containing main text of the notecard</summary>
        public String BodyText;

        /// <summary>List of <see cref="OpenMetaverse.InventoryItem"/>s embedded on the notecard</summary>
        public List<InventoryItem> EmbeddedItems;

        /// <summary>Construct an Asset of type Notecard</summary>
        public AssetNotecard() { }

        /// <summary>
        /// Construct an Asset object of type Notecard
        /// </summary>
        /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
        /// <param name="assetData">A byte array containing the raw asset data</param>
        public AssetNotecard(UUID assetID, byte[] assetData)
        {
            super(assetID, assetData);
        }

        /// <summary>
        /// Encode the raw contents of a string with the specific Linden Text properties
        /// </summary>
        @Override
	public void Encode()
        {
        	//TODO need to verify
//            String body = BodyText ?? String.Empty;
            String body = BodyText == null ?  "" : BodyText;
        	
            StringBuilder output = new StringBuilder();
            output.append("Linden text version 2\n");
            output.append("{\n");
            output.append("LLEmbeddedItems version 1\n");
            output.append("{\n");

            int count = 0;

            if (EmbeddedItems != null)
            {
                count = EmbeddedItems.size();
            }

            output.append("count " + count + "\n");

            if (count > 0)
            {
                output.append("{\n");

                for (int i = 0; i < EmbeddedItems.size(); i++)
                {
                    InventoryItem item = EmbeddedItems.get(i);

                    output.append("ext char index " + i + "\n");

                    output.append("\tinv_item\t0\n");
                    output.append("\t{\n");

                    output.append("\t\titem_id\t" + item.UUID + "\n");
                    output.append("\t\tparent_id\t" + item.ParentUUID + "\n");

                    output.append("\tpermissions 0\n");
                    output.append("\t{\n");
                    //TODO need to verify
//                    output.Append("\t\tbase_mask\t" + ((uint)item.Permissions.BaseMask).ToString("x").PadLeft(8, '0') + "\n");
//                    output.Append("\t\towner_mask\t" + ((uint)item.Permissions.OwnerMask).ToString("x").PadLeft(8, '0') + "\n");
//                    output.Append("\t\tgroup_mask\t" + ((uint)item.Permissions.GroupMask).ToString("x").PadLeft(8, '0') + "\n");
//                    output.Append("\t\teveryone_mask\t" + ((uint)item.Permissions.EveryoneMask).ToString("x").PadLeft(8, '0') + "\n");
//                    output.Append("\t\tnext_owner_mask\t" + ((uint)item.Permissions.NextOwnerMask).ToString("x").PadLeft(8, '0') + "\n");
                    output.append("\t\tbase_mask\t" + Utils.longToHexString(PermissionMask.getIndex(item.Permissions.BaseMask)) + "\n");
                    output.append("\t\towner_mask\t" + Utils.longToHexString(PermissionMask.getIndex(item.Permissions.OwnerMask)) + "\n");
                    output.append("\t\tgroup_mask\t" + Utils.longToHexString(PermissionMask.getIndex(item.Permissions.GroupMask)) + "\n");
                    output.append("\t\teveryone_mask\t" + Utils.longToHexString(PermissionMask.getIndex(item.Permissions.EveryoneMask)) + "\n");
                    output.append("\t\tnext_owner_mask\t" + Utils.longToHexString(PermissionMask.getIndex(item.Permissions.NextOwnerMask)) + "\n");
                    
                    output.append("\t\tcreator_id\t" + item.CreatorID + "\n");
                    output.append("\t\towner_id\t" + item.OwnerID + "\n");
                    output.append("\t\tlast_owner_id\t" + item.LastOwnerID + "\n");
                    output.append("\t\tgroup_id\t" + item.GroupID + "\n");
                    if (item.GroupOwned) output.append("\t\tgroup_owned\t1\n");
                    output.append("\t}\n");

                    if (Permissions.hasPermissions(item.Permissions.BaseMask, 
                    		PermissionMask.get(PermissionMask.Modify.getIndex() | PermissionMask.Copy.getIndex() | PermissionMask.Transfer.getIndex())) 
                    		|| item.AssetUUID == UUID.Zero)
                    {
                        output.append("\t\tasset_id\t" + item.AssetUUID + "\n");
                    }
                    else
                    {
                        output.append("\t\tshadow_id\t" + InventoryManager.EncryptAssetID(item.AssetUUID) + "\n");
                    }
                    
                    output.append("\t\ttype\t" + Utils.AssetTypeToString(item.AssetType) + "\n");
                    output.append("\t\tinv_type\t" + Utils.InventoryTypeToString(item.InventoryType) + "\n");
//                    output.Append("\t\tflags\t" + item.Flags.ToString().PadLeft(8, '0') + "\n");
                    output.append("\t\tflags\t" + Utils.longToHexString(item.Flags) + "\n");

                    
                    output.append("\tsale_info\t0\n");
                    output.append("\t{\n");
                    output.append("\t\tsale_type\t" + Utils.SaleTypeToString(item.SaleType) + "\n");
                    output.append("\t\tsale_price\t" + item.SalePrice + "\n");
                    output.append("\t}\n");

                    output.append("\t\tname\t" + item.Name.replace('|', '_') + "|\n");
                    output.append("\t\tdesc\t" + item.Description.replace('|', '_') + "|\n");
                    output.append("\t\tcreation_date\t" + Utils.dateToUnixTime(item.CreationDate) + "\n");

                    output.append("\t}\n");

                    if (i != EmbeddedItems.size() - 1)
                    {
                        output.append("}\n{\n");
                    }
                }

                output.append("}\n");
            }

            output.append("}\n");
            output.append("Text length " + (Utils.stringToBytesWithTrailingNullByte(body).length - 1) + "\n");
            output.append(body + "}\n");

            AssetData = Utils.stringToBytesWithTrailingNullByte(output.toString());
        }

        /// <summary>
        /// Decode the raw asset data including the Linden Text properties
        /// </summary>
        /// <returns>true if the AssetData was successfully decoded</returns>
        @Override
	public boolean Decode() throws UnsupportedEncodingException
        {
            String data = Utils.bytesWithTrailingNullByteToString(AssetData);
            EmbeddedItems = new ArrayList<InventoryItem>();
            BodyText = "";

            try
            {
                String[] lines = data.split("\n");
                int i = 0;
                Matcher m;
                
                // Version
                if(!(m = Pattern.compile("Linden text version\\s+(\\d+)").matcher(lines[i++])).find())
                    throw new Exception("could not determine version");
                int notecardVersion = Integer.parseInt(m.group(1));
                if (notecardVersion < 1 || notecardVersion > 2)
                    throw new Exception("unsuported version");
                if(!(m = Pattern.compile("\\s*{$").matcher(lines[i++])).find())
                    throw new Exception("wrong format");
                
//                if (!(m = Regex.Match(lines[i++], @"Linden text version\s+(\d+)")).Success)
//                    throw new Exception("could not determine version");
//                int notecardVersion = int.Parse(m.Groups[1].Value);
//                if (notecardVersion < 1 || notecardVersion > 2)
//                    throw new Exception("unsuported version");
//                if (!(m = Regex.Match(lines[i++], @"\s*{$")).Success)
//                    throw new Exception("wrong format");
    
                
                // Embedded items header

                if(!(m = Pattern.compile("LLEmbeddedItems version\\s+(\\d+)").matcher(lines[i++])).find())
                    throw new Exception("could not determine embedded items version version");
                if (!m.group(1).equals("1"))
                    throw new Exception("unsuported embedded item version");                
                if(!(m = Pattern.compile("\\s*{$").matcher(lines[i++])).find())
                    throw new Exception("wrong format");
                
                
//                if (!(m = Regex.Match(lines[i++], @"LLEmbeddedItems version\s+(\d+)")).Success)
//                    throw new Exception("could not determine embedded items version version");
//                if (m.Groups[1].Value != "1")
//                    throw new Exception("unsuported embedded item version");
//                if (!(m = Regex.Match(lines[i++], @"\s*{$")).Success)
//                    throw new Exception("wrong format");

                // Item count
                if(!(m = Pattern.compile("count\\s+(\\d+)").matcher(lines[i++])).find())
                    throw new Exception("wrong format");
                int count = Integer.parseInt(m.group(1));
                
//                if (!(m = Regex.Match(lines[i++], @"count\s+(\d+)")).Success)
//                    throw new Exception("wrong format");
//                int count = int.Parse(m.Groups[1].Value);

                // Decode individual items
                for (int n = 0; n < count; n++)
                {
                    if(!(m = Pattern.compile("\\s*{$").matcher(lines[i++])).find())
                        throw new Exception("wrong format");
                    	
//                    if (!(m = Regex.Match(lines[i++], @"\s*{$")).Success)
//                        throw new Exception("wrong format");

                    // Index
                    if(!(m = Pattern.compile("ext char index\\s+(\\d+)").matcher(lines[i++])).find())
                        throw new Exception("missing ext char index");
                    //warning CS0219: The variable `index' is assigned but its value is never used
                    //int index = int.Parse(m.Groups[1].Value);
                    
//                    if (!(m = Regex.Match(lines[i++], @"ext char index\s+(\d+)")).Success)
//                        throw new Exception("missing ext char index");
                    //warning CS0219: The variable `index' is assigned but its value is never used
                    //int index = int.Parse(m.Groups[1].Value);

                    // Inventory item
                    if(!(m = Pattern.compile("inv_item\\s+0").matcher(lines[i++])).find())
                        throw new Exception("missing inv item");
                    
//                    
//                    if (!(m = Regex.Match(lines[i++], @"inv_item\s+0")).Success)
//                        throw new Exception("missing inv item");

                    // Item itself
                    UUID uuid = UUID.Zero;
                    UUID creatorID = UUID.Zero;
                    UUID ownerID = UUID.Zero;
                    UUID lastOwnerID = UUID.Zero;
                    UUID groupID = UUID.Zero;
                    Permissions permissions = Permissions.NoPermissions;
                    int salePrice = 0;
                    SaleType saleType = SaleType.Not;
                    UUID parentUUID = UUID.Zero;
                    UUID assetUUID = UUID.Zero;
                    AssetType assetType = AssetType.Unknown;
                    InventoryType inventoryType = InventoryType.Unknown;
                    //uint
                    long flags = 0;
                    String name = "";
                    String description = "";
                    Date creationDate = Utils.Epoch;

                    while (true)
                    {
                        if(!(m = Pattern.compile("([^\\s]+)(\\s+)?(.*)?").matcher(lines[i++])).find())
                            throw new Exception("wrong format");

//                        if (!(m = Regex.Match(lines[i++], @"([^\s]+)(\s+)?(.*)?")).Success)
//                            throw new Exception("wrong format");
                        
                        String key = m.group(1);
                        StringBuilder val = new StringBuilder(m.group(3));
                        if (key.equals("{"))
                            continue;
                        if (key.equals("}"))
                            break;
                        else if (key.equals("permissions"))
                        {
                        	//uint
                            long baseMask = 0;
                            long ownerMask = 0;
                            long groupMask = 0;
                            long everyoneMask = 0;
                            long nextOwnerMask = 0;

                            while (true)
                            {
                                if(!(m = Pattern.compile("([^\\s]+)(\\s+)?([^\\s]+)?").matcher(lines[i++])).find())
                                    throw new Exception("wrong format");
                            	
//                                if (!(m = Regex.Match(lines[i++], @"([^\s]+)(\s+)?([^\s]+)?")).Success)
//                                    throw new Exception("wrong format");
                                String pkey = m.group(1);
                                String pval = m.group(3);

                                if (pkey.equals("{"))
                                    continue;
                                if (pkey.equals("}"))
                                    break;
                                else if (pkey.equals("creator_id"))
                                {
                                    creatorID = new UUID(pval);
                                }
                                else if (pkey.equals("owner_id"))
                                {
                                    ownerID = new UUID(pval);
                                }
                                else if (pkey.equals("last_owner_id"))
                                {
                                    lastOwnerID = new UUID(pval);
                                }
                                else if (pkey.equals("group_id"))
                                {
                                    groupID = new UUID(pval);
                                }
                                else if (pkey.equals("base_mask"))
                                {
                                	baseMask = Utils.hexStringToUInt(pval, false);
//                                    baseMask = Utils.hexStringToUInt(pval, false);
                                }
                                else if (pkey.equals("owner_mask"))
                                {
                                    ownerMask = Utils.hexStringToUInt(pval, false);
                                }
                                else if (pkey.equals("group_mask"))
                                {
                                    groupMask = Utils.hexStringToUInt(pval, false);
                                }
                                else if (pkey.equals("everyone_mask"))
                                {
                                    everyoneMask = Utils.hexStringToUInt(pval, false);
                                }
                                else if (pkey.equals("next_owner_mask"))
                                {
                                    nextOwnerMask = Utils.hexStringToUInt(pval, false);
                                }
                            }
                            permissions = new Permissions(baseMask, everyoneMask, groupMask, nextOwnerMask, ownerMask);
                        }
                        else if (key.equals("sale_info"))
                        {
                            while (true)
                            {
                                if(!(m = Pattern.compile("([^\\s]+)(\\s+)?([^\\s]+)?").matcher(lines[i++])).find())
                                    throw new Exception("wrong format");

//                                if (!(m = Regex.Match(lines[i++], @"([^\s]+)(\s+)?([^\s]+)?")).Success)
//                                    throw new Exception("wrong format");
                                
                                String pkey = m.group(1);
                                String pval = m.group(3);

                                if (pkey == "{")
                                    continue;
                                if (pkey == "}")
                                    break;
                                else if (pkey.equals("sale_price"))
                                {
                                    salePrice = Integer.parseInt(pval);
                                }
                                else if (pkey.equals("sale_type"))
                                {
                                    saleType = Utils.StringToSaleType(pval);
                                }
                            }
                        }
                        else if (key.equals("item_id"))
                        {
                            uuid = new UUID(val.toString());
                        }
                        else if (key.equals("parent_id"))
                        {
                            parentUUID = new UUID(val.toString());
                        }
                        else if (key.equals("asset_id"))
                        {
                            assetUUID = new UUID(val.toString());
                        }
                        else if (key.equals("type"))
                        {
                            assetType = Utils.StringToAssetType(val.toString());
                        }
                        else if (key.equals("inv_type"))
                        {
                            inventoryType = Utils.StringToInventoryType(val.toString());
                        }
                        else if (key.equals("flags"))
                        {
                            flags = Utils.hexStringToUInt(val.toString(), false);
                        }
                        else if (key.equals("name"))
                        {                       
                            name = val.deleteCharAt(val.lastIndexOf("|")).toString();
                        }
                        else if (key.equals("desc"))
                        {
                            description = val.deleteCharAt(val.lastIndexOf("|")).toString();
                        }
                        else if (key.equals("creation_date"))
                        {
                            creationDate = Utils.unixTimeToDate(Integer.parseInt(val.toString()));
                        }
                    }
                    InventoryItem finalEmbedded = InventoryManager.CreateInventoryItem(inventoryType, uuid);

                    finalEmbedded.CreatorID = creatorID;
                    finalEmbedded.OwnerID = ownerID;
                    finalEmbedded.LastOwnerID = lastOwnerID;
                    finalEmbedded.GroupID = groupID;
                    finalEmbedded.Permissions = permissions;
                    finalEmbedded.SalePrice = salePrice;
                    finalEmbedded.SaleType = saleType;
                    finalEmbedded.ParentUUID = parentUUID;
                    finalEmbedded.AssetUUID = assetUUID;
                    finalEmbedded.AssetType = assetType;
                    finalEmbedded.Flags = flags;
                    finalEmbedded.Name = name;
                    finalEmbedded.Description = description;
                    finalEmbedded.CreationDate = creationDate;

                    EmbeddedItems.add(finalEmbedded);

//                    if (!(m = Regex.Match(lines[i++], @"\s*}$")).Success)
//                        throw new Exception("wrong format");

                    if(!(m = Pattern.compile("\\s*}$").matcher(lines[i++])).find())
                        throw new Exception("wrong format");
          
                }

                // Text size
                
                if(!(m = Pattern.compile("\\s*}$").matcher(lines[i++])).find())
                    throw new Exception("wrong format");

                if(!(m = Pattern.compile("Text length\\s+(\\d+)").matcher(lines[i++])).find())
                    throw new Exception("could not determine text length");
                
//                if (!(m = Regex.Match(lines[i++], @"\s*}$")).Success)
//                    throw new Exception("wrong format");
//                if (!(m = Regex.Match(lines[i++], @"Text length\s+(\d+)")).Success)
//                    throw new Exception("could not determine text length");

                // Read the rest of the notecard
                while (i < lines.length)
                {
                    BodyText += lines[i++] + "\n";
                }
                StringBuilder tmp1 = new StringBuilder(BodyText);
                BodyText = tmp1.deleteCharAt(tmp1.lastIndexOf("}")).toString();
                return true;
            }
            catch (Exception ex)
            {
                JLogger.error("Decoding notecard asset failed: " + Utils.getExceptionStackTraceAsString(ex));
                return false;
            }
        }
    }