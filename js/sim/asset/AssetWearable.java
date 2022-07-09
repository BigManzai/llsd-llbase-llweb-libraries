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

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.ngt.jopenmetaverse.shared.sim.AppearanceManager.AvatarTextureIndex;
import com.ngt.jopenmetaverse.shared.types.Enums;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions;
import com.ngt.jopenmetaverse.shared.protocol.primitives.Permissions.PermissionMask;

  /// <summary>
    /// Represents a Wearable Asset, Clothing, Hair, Skin, Etc
    /// </summary>
    public abstract class AssetWearable extends Asset
    {
        /// <summary>A string containing the name of the asset</summary>
        public String Name = "";
        /// <summary>A string containing a short description of the asset</summary>
        public String Description = "";
        /// <summary>The Assets WearableType</summary>
        public Enums.WearableType WearableType = Enums.WearableType.Shape;
        /// <summary>The For-Sale status of the object</summary>
        public Enums.SaleType ForSale;
        /// <summary>An Integer representing the purchase price of the asset</summary>
        public int SalePrice;
        /// <summary>The <seealso cref="UUID"/> of the assets creator</summary>
        public UUID Creator;
        /// <summary>The <seealso cref="UUID"/> of the assets current owner</summary>
        public UUID Owner;
        /// <summary>The <seealso cref="UUID"/> of the assets prior owner</summary>
        public UUID LastOwner;
        /// <summary>The <seealso cref="UUID"/> of the Group this asset is set to</summary>
        public UUID Group;
        /// <summary>True if the asset is owned by a <seealso cref="Group"/></summary>
        public boolean GroupOwned;
        /// <summary>The Permissions mask of the asset</summary>
        public Permissions Permissions;
        /// <summary>A Map containing Key/Value pairs of the objects parameters</summary>
        public Map<Integer, Float> Params = new HashMap<Integer, Float>();
        /// <summary>A Map containing Key/Value pairs where the Key is the textures Index and the Value is the Textures <seealso cref="UUID"/></summary>
        public Map<AvatarTextureIndex, UUID> Textures = new HashMap<AvatarTextureIndex, UUID>();

        /// <summary>Initializes a new instance of an AssetWearable object</summary>
        public AssetWearable() { }

        /// <summary>Initializes a new instance of an AssetWearable object with parameters</summary>
        /// <param name="assetID">A unique <see cref="UUID"/> specific to this asset</param>
        /// <param name="assetData">A byte array containing the raw asset data</param>
        public AssetWearable(UUID assetID, byte[] assetData) 
         { super(assetID, assetData);}

        /// <summary>
        /// Decode an assets byte encoded data to a string
        /// </summary>
        /// <returns>true if the asset data was decoded successfully</returns>
        @Override
        public boolean Decode()
        {
            if (AssetData == null || AssetData.length == 0)
                return false;

            int version = -1;
            Permissions = new Permissions();

            try
            {
                String data = Utils.trim(Utils.bytesWithTrailingNullByteToString(AssetData), new char[]{'\0'});
                
                data = data.replace("\r", "");
                String[] lines = data.split("\n");
                for (int stri = 0; stri < lines.length; stri++)
                {
                    if (stri == 0)
                    {
                        String versionstring = lines[stri];
                        version = Integer.parseInt(versionstring.split(" ")[2]);
                        if (version != 22 && version != 18 && version != 16 && version != 15)
                            return false;
                    }
                    else if (stri == 1)
                    {
                        Name = lines[stri];
                    }
                    else if (stri == 2)
                    {
                        Description = lines[stri];
                    }
                    else
                    {
                        String line = lines[stri].trim();
                        String[] fields = line.split("\t");

                        if (fields.length == 1)
                        {
                            fields = line.split(" ");
                            if (fields[0].equals("parameters"))
                            {
                                int count = Integer.parseInt(fields[1]) + stri;
                                for (; stri < count; )
                                {
                                    stri++;
                                    line = lines[stri].trim();
                                    fields = line.split(" ");

                                    int id = 0;

                                    // Special handling for -0 edge case
                                    if (!fields[0].equals("-0"))
                                    {
                                    	//JLogger.debug("Going to parse to integer: " + fields[0] + "\n" + Utils.bytesToHexDebugString(Utils.stringToBytes(fields[0]), ""));
                                        id = Integer.parseInt(fields[0]);
                                    }

                                    if (fields[1].equals(","))
                                        fields[1] = "0";
                                    else
                                        fields[1] = fields[1].replace(',', '.');

//                                    float weight = Float.parseFloat(fields[1], System.Globalization.NumberStyles.Float,   Utils.EnUsCulture.NumberFormat);
                                    float weight = Float.parseFloat(fields[1]);
                                    Params.put(id, weight);
                                }
                            }
                            else if (fields[0].equals("textures"))
                            {
                                int count = Integer.parseInt(fields[1]) + stri;
                                for (; stri < count; )
                                {
                                    stri++;
                                    line = lines[stri].trim();
                                    fields = line.split(" ");

                                    AvatarTextureIndex id = AvatarTextureIndex.get(Integer.parseInt(fields[0]));
                                    UUID texture = new UUID(fields[1]);

                                    Textures.put(id,  texture);
                                }
                            }
                            else if (fields[0].equals("type"))
                            {
                                WearableType = WearableType.get((byte)Integer.parseInt(fields[1]));
                            }

                        }
                        else if (fields.length == 2)
                        {
                        	if(fields[0].equals("creator_mask"))
                        	{
                        		// Deprecated, apply this as the base mask
                        		Permissions.BaseMask = PermissionMask.get(Utils.hexStringToUInt(fields[1], false));
                        	}
                        	else if(fields[0].equals("base_mask"))
                        	{
                        		Permissions.BaseMask = PermissionMask.get(Utils.hexStringToUInt(fields[1], false));
                        	}
                        	else if(fields[0].equals("owner_mask"))
                        	{
                        		Permissions.OwnerMask = PermissionMask.get(Utils.hexStringToUInt(fields[1], false));
                        	}
                        	else if(fields[0].equals("group_mask"))
                        	{
                        		Permissions.GroupMask = PermissionMask.get(Utils.hexStringToUInt(fields[1], false));
                        	}
                        	else if(fields[0].equals("everyone_mask")) {
                        		Permissions.EveryoneMask = PermissionMask.get(Utils.hexStringToUInt(fields[1], false));
                        	}
                        	else if(fields[0].equals("next_owner_mask")) {
                        		Permissions.NextOwnerMask = PermissionMask.get(Utils.hexStringToUInt(fields[1], false));
                        	}
                        	else if(fields[0].equals("creator_id")) {
                        		Creator = new UUID(fields[1]);
                        	}
                        	else if(fields[0].equals("owner_id")) {
                        		Owner = new UUID(fields[1]);
                        	}
                        	else if(fields[0].equals("last_owner_id")) {
                        		LastOwner = new UUID(fields[1]);
                        	}
                        	else if(fields[0].equals("group_id")) {
                        		Group = new UUID(fields[1]);
                        	}
                        	else if(fields[0].equals("group_owned")) {
                        		GroupOwned = (Integer.parseInt(fields[1]) != 0);
                        	}
                        	else if(fields[0].equals("sale_type")) {
                        		ForSale = Utils.StringToSaleType(fields[1]);
                        	}
                        	else if(fields[0].equals("sale_price")) {
                        		SalePrice = Integer.parseInt(fields[1]);
                        	}
                        	else if(fields[0].equals("sale_info")) {
                        		// Container for sale_type and sale_price, ignore
                        	}
                        	else if(fields[0].equals("perm_mask")) {
                        		// Deprecated, apply this as the next owner mask
                        		Permissions.NextOwnerMask = PermissionMask.get(Utils.hexStringToUInt(fields[1], false));
                        	}
                        	else
                        		return false;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                JLogger.warn("Failed decoding wearable asset, type = " + this.getAssetType() + " ID" + this.getAssetID() + ": " + Utils.getExceptionStackTraceAsString(ex));
                return false;
            }

            return true;
        }

        /// <summary>
        /// Encode the assets string represantion into a format consumable by the asset server
        /// </summary>
        @Override
        public void Encode()
        {
            final String NL = "\n";

            StringBuilder data = new StringBuilder("LLWearable version 22\n");
            data.append(Name); data.append(NL); data.append(NL);
            data.append("\tpermissions 0\n\t{\n");
            data.append("\t\tbase_mask\t"); data.append(Utils.uintToHexString(PermissionMask.getIndex(Permissions.BaseMask))); data.append(NL);
            data.append("\t\towner_mask\t"); data.append(Utils.uintToHexString(PermissionMask.getIndex(Permissions.OwnerMask))); data.append(NL);
            data.append("\t\tgroup_mask\t"); data.append(Utils.uintToHexString(PermissionMask.getIndex(Permissions.GroupMask))); data.append(NL);
            data.append("\t\teveryone_mask\t"); data.append(Utils.uintToHexString(PermissionMask.getIndex(Permissions.EveryoneMask))); data.append(NL);
            data.append("\t\tnext_owner_mask\t"); data.append(Utils.uintToHexString(PermissionMask.getIndex(Permissions.NextOwnerMask))); data.append(NL);
            data.append("\t\tcreator_id\t"); data.append(Creator.toString()); data.append(NL);
            data.append("\t\towner_id\t"); data.append(Owner.toString()); data.append(NL);
            data.append("\t\tlast_owner_id\t"); data.append(LastOwner.toString()); data.append(NL);
            data.append("\t\tgroup_id\t"); data.append(Group.toString()); data.append(NL);
            if (GroupOwned) data.append("\t\tgroup_owned\t1\n");
            data.append("\t}\n");
            data.append("\tsale_info\t0\n");
            data.append("\t{\n");
            data.append("\t\tsale_type\t"); data.append(Utils.SaleTypeToString(ForSale)); data.append(NL);
            data.append("\t\tsale_price\t"); data.append(SalePrice); data.append(NL);
            data.append("\t}\n");
            data.append("type "); data.append((int)WearableType.getIndex()); data.append(NL);

            data.append("parameters "); data.append(Params.size()); data.append(NL);
            for (Entry<Integer, Float> param : Params.entrySet())
            {
                data.append(param.getKey()); data.append(" "); data.append(Helpers.FloatToTerseString(param.getValue())); data.append(NL);
            }

            data.append("textures "); data.append(Textures.size()); data.append(NL);
            for (Entry<AvatarTextureIndex, UUID> texture : Textures.entrySet())
            {
                data.append((byte)texture.getKey().getIndex()); data.append(" "); data.append(texture.getValue().toString()); data.append(NL);
            }

            AssetData = Utils.stringToBytesWithTrailingNullByte(data.toString());
        }
    }