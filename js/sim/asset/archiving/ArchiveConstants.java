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
package com.ngt.jopenmetaverse.shared.sim.asset.archiving;

import java.util.HashMap;
import java.util.Map;

import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;

/// <summary>
/// Constants for the archiving module
/// </summary>
public class ArchiveConstants
{
    /// <summary>
    /// The location of the archive control file
    /// </summary>
    public final static String CONTROL_FILE_PATH = "archive.xml";

    /// <summary>
    /// Path for the assets held in an archive
    /// </summary>
    public final static String ASSETS_PATH = "assets/";

    /// <summary>
    /// Path for the prims file
    /// </summary>
    public final static String OBJECTS_PATH = "objects/";

    /// <summary>
    /// Path for terrains.  Technically these may be assets, but I think it's quite nice to split them out.
    /// </summary>
    public final static String TERRAINS_PATH = "terrains/";

    /// <summary>
    /// Path for region settings.
    /// </summary>
    public final static String SETTINGS_PATH = "settings/";

    /// <summary>
    /// The character the separates the uuid from extension information in an archived asset filename
    /// </summary>
    public final static String ASSET_EXTENSION_SEPARATOR = "_";

    /// <summary>
    /// Extensions used for asset types in the archive
    /// </summary>
    public final static Map<AssetType, String> ASSET_TYPE_TO_EXTENSION = new HashMap<AssetType, String>();
    public final static Map<String, AssetType> EXTENSION_TO_ASSET_TYPE = new HashMap<String, AssetType>();

    static
    {
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Animation, ASSET_EXTENSION_SEPARATOR + "animation.bvh");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Bodypart, ASSET_EXTENSION_SEPARATOR + "bodypart.txt");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.CallingCard, ASSET_EXTENSION_SEPARATOR + "callingcard.txt");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Clothing, ASSET_EXTENSION_SEPARATOR + "clothing.txt");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Folder, ASSET_EXTENSION_SEPARATOR + "folder.txt");   // Not sure if we'll ever see this
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Gesture, ASSET_EXTENSION_SEPARATOR + "gesture.txt");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.ImageJPEG, ASSET_EXTENSION_SEPARATOR + "image.jpg");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.ImageTGA, ASSET_EXTENSION_SEPARATOR + "image.tga");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Landmark, ASSET_EXTENSION_SEPARATOR + "landmark.txt");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.LostAndFoundFolder, ASSET_EXTENSION_SEPARATOR + "lostandfoundfolder.txt");   // Not sure if we'll ever see this
        ASSET_TYPE_TO_EXTENSION.put(AssetType.LSLBytecode, ASSET_EXTENSION_SEPARATOR + "bytecode.lso");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.LSLText, ASSET_EXTENSION_SEPARATOR + "script.lsl");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Notecard, ASSET_EXTENSION_SEPARATOR + "notecard.txt");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Object, ASSET_EXTENSION_SEPARATOR + "object.xml");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.RootFolder, ASSET_EXTENSION_SEPARATOR + "rootfolder.txt");   // Not sure if we'll ever see this
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Simstate, ASSET_EXTENSION_SEPARATOR + "simstate.bin");   // Not sure if we'll ever see this
        ASSET_TYPE_TO_EXTENSION.put(AssetType.SnapshotFolder, ASSET_EXTENSION_SEPARATOR + "snapshotfolder.txt");   // Not sure if we'll ever see this
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Sound, ASSET_EXTENSION_SEPARATOR + "sound.ogg");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.SoundWAV, ASSET_EXTENSION_SEPARATOR + "sound.wav");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.Texture, ASSET_EXTENSION_SEPARATOR + "texture.jp2");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.TextureTGA, ASSET_EXTENSION_SEPARATOR + "texture.tga");
        ASSET_TYPE_TO_EXTENSION.put(AssetType.TrashFolder, ASSET_EXTENSION_SEPARATOR + "trashfolder.txt");   // Not sure if we'll ever see this

        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "animation.bvh", AssetType.Animation);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "bodypart.txt", AssetType.Bodypart);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "callingcard.txt", AssetType.CallingCard);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "clothing.txt", AssetType.Clothing);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "folder.txt", AssetType.Folder);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "gesture.txt", AssetType.Gesture);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "image.jpg", AssetType.ImageJPEG);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "image.tga", AssetType.ImageTGA);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "landmark.txt", AssetType.Landmark);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "lostandfoundfolder.txt", AssetType.LostAndFoundFolder);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "bytecode.lso", AssetType.LSLBytecode);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "script.lsl", AssetType.LSLText);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "notecard.txt", AssetType.Notecard);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "object.xml", AssetType.Object);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "rootfolder.txt", AssetType.RootFolder);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "simstate.bin", AssetType.Simstate);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "snapshotfolder.txt", AssetType.SnapshotFolder);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "sound.ogg", AssetType.Sound);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "sound.wav", AssetType.SoundWAV);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "texture.jp2", AssetType.Texture);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "texture.tga", AssetType.TextureTGA);
        EXTENSION_TO_ASSET_TYPE.put(ASSET_EXTENSION_SEPARATOR + "trashfolder.txt", AssetType.TrashFolder);
    }
}