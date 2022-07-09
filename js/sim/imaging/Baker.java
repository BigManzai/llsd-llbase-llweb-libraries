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
package com.ngt.jopenmetaverse.shared.sim.imaging;


import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import com.ngt.jopenmetaverse.shared.protocol.Helpers;
import com.ngt.jopenmetaverse.shared.sim.AppearanceManager;
import com.ngt.jopenmetaverse.shared.sim.AppearanceManager.AvatarTextureIndex;
import com.ngt.jopenmetaverse.shared.sim.AppearanceManager.BakeType;
import com.ngt.jopenmetaverse.shared.sim.Settings;
import com.ngt.jopenmetaverse.shared.sim.asset.AssetTexture;
import com.ngt.jopenmetaverse.shared.sim.imaging.ManagedImage.ImageChannels;
import com.ngt.jopenmetaverse.shared.sim.visual.VisualParams.VisualAlphaParam;
import com.ngt.jopenmetaverse.shared.types.Color4;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// A set of textures that are layered on texture of each other and "baked"
/// in to a single texture, for avatar appearances
/// </summary>
public class Baker {
        //region Properties
        /// <summary>Final baked texture</summary>
        public AssetTexture getBakedTexture() { return bakedTexture; } 
        /// <summary>Component layers</summary>
        public List<AppearanceManager.TextureData> getTextures() { return textures; } 
        /// <summary>Width of the final baked image and scratchpad</summary>
        public int getBakeWidth() {return bakeWidth; } 
        /// <summary>Height of the final baked image and scratchpad</summary>
        public int getBakeHeight() {return bakeHeight; } 
        /// <summary>Bake type</summary>
        public BakeType getBakeType() {return bakeType; } 
        /// <summary>Is this one of the 3 skin bakes</summary>
        private boolean isSkin() 
        {return bakeType == BakeType.Head || bakeType == BakeType.LowerBody || bakeType == BakeType.UpperBody; } 
        //endregion

        //region Private fields
        /// <summary>Final baked texture</summary>
        private AssetTexture bakedTexture;
        /// <summary>Component layers</summary>
        private List<AppearanceManager.TextureData> textures = new ArrayList<AppearanceManager.TextureData>();
        /// <summary>Width of the final baked image and scratchpad</summary>
        private int bakeWidth;
        /// <summary>Height of the final baked image and scratchpad</summary>
        private int bakeHeight;
        /// <summary>Bake type</summary>
        private BakeType bakeType;
        //endregion

        //region Constructor
        /// <summary>
        /// Default constructor
        /// </summary>
        /// <param name="bakeType">Bake type</param>
        public Baker(BakeType bakeType)
        {
            this.bakeType = bakeType;

            if (bakeType == BakeType.Eyes)
            {
                bakeWidth = 128;
                bakeHeight = 128;
            }
            else
            {
                bakeWidth = 512;
                bakeHeight = 512;
            }
        }
        //endregion

        //region Public methods
        /// <summary>
        /// Adds layer for baking
        /// </summary>
        /// <param name="tdata">TexturaData struct that contains texture and its params</param>
        public void AddTexture(AppearanceManager.TextureData tdata)
        {
            synchronized (textures)
            {
                textures.add(tdata);
            }
        }

        public void Bake() throws Exception
        {
            bakedTexture = new AssetTexture(new ManagedImage(bakeWidth, bakeHeight,
            		ImageChannels.get(ManagedImage.ImageChannels.Color.getIndex() 
            				| ManagedImage.ImageChannels.Alpha.getIndex() 
            				| ManagedImage.ImageChannels.Bump.getIndex())));
            
            // Base color for eye bake is white, color of layer0 for others
            if (bakeType == BakeType.Eyes)
            {
                InitBakedLayerColor(Color4.White);
            }
            else if (textures.size() > 0)
            {
                InitBakedLayerColor(textures.get(0).Color);
            }

            // Do we have skin texture?
            boolean SkinTexture = textures.size() > 0 && textures.get(0).Texture != null;
            
            if (bakeType == BakeType.Head)
            {
                DrawLayer(LoadResourceLayer("head_color.tga"), false);
                AddAlpha(bakedTexture.Image, LoadResourceLayer("head_alpha.tga"));
                MultiplyLayerFromAlpha(bakedTexture.Image, LoadResourceLayer("head_skingrain.tga"));
            }

            if (!SkinTexture && bakeType == BakeType.UpperBody)
            {
                DrawLayer(LoadResourceLayer("upperbody_color.tga"), false);
            }

            if (!SkinTexture && bakeType == BakeType.LowerBody)
            {
                DrawLayer(LoadResourceLayer("lowerbody_color.tga"), false);
            }

            ManagedImage alphaWearableTexture = null;

            
            
//            FileUtils.writeBytes(new File("openmetaverse_data/logs/initial_" + bakeType  + ".tga"), bakedTexture.Image.ExportTGA());
            
            // Layer each texture on top of one other, applying alpha masks as we go
            for (int i = 0; i < textures.size(); i++)
            {
            	JLogger.debug("Starting Baking: " + bakeType + " index " + i) ;
            	            	
                // Skip if we have no texture on this layer
                if (textures.get(i).Texture == null) 
                {
                	JLogger.debug("textures.get(i).Texture is null for baketype: " + bakeType);
                	continue;
                }

                // Is this Alpha wearable and does it have an alpha channel?
                if ((textures.get(i).TextureIndex.getIndex() >= AvatarTextureIndex.LowerAlpha.getIndex()) &&
                    (textures.get(i).TextureIndex.getIndex() <= AvatarTextureIndex.HairAlpha.getIndex()))
                {
//                    System.out.println("this is Alpha wearable and does it have an alpha channel");
                    if (textures.get(i).Texture.Image.Alpha != null)
                    {
                        alphaWearableTexture = textures.get(i).Texture.Image.Clone();
                    }
                	JLogger.debug("Skipping Alpha wearable and does it have an alpha channel Baking: " + bakeType + " index " + i) ;
                    continue;
                }

                // Don't draw skin on head bake first
                // For head bake skin texture is drawn last, go figure
                if (bakeType == BakeType.Head && i == 0) 
                {
                	continue;
                }
                
                if(textures.get(i).Texture == null)
                	JLogger.debug("textures.get(i).Texture is null");

//            	System.out.println("Cloning the texture image ...." );
                ManagedImage texture = textures.get(i).Texture.Image.Clone();
                //TODO Remove Debugging statments
               
//                FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType + "-texture-layer-" + i  + "-" + textures.get(i).TextureID.toString() + ".tga"), texture.ExportTGA());

                // Resize texture to the size of baked layer
                // FIXME: if texture is smaller than the layer, don't stretch it, tile it
                if (texture.Width != bakeWidth || texture.Height != bakeHeight)
                {
                    try 
                    {
//                    	System.out.println("Resizing the image...");
                    	texture.ResizeNearestNeighbor(bakeWidth, bakeHeight); 
                    }
                    catch (Exception e) { JLogger.warn(Utils.getExceptionStackTraceAsString(e));continue; }
                }

                // Special case for hair layer for the head bake
                // If we don't have skin texture, we discard hair alpha
                // and apply hair pattern over the texture
                if (!SkinTexture && bakeType == BakeType.Head && i == 1)
                {
                    if (texture.Alpha != null)
                    {
                        for (int j = 0; j < texture.Alpha.length; j++) texture.Alpha[j] = (byte)255;
                    }
                    MultiplyLayerFromAlpha(texture, LoadResourceLayer("head_hair.tga"));
                }

                // Aply tint and alpha masks except for skin that has a texture
                // on layer 0 which always overrides other skin settings
                if (!(isSkin() && i == 0))
                {
                    ApplyTint(texture, textures.get(i).Color);
                    
                    // For hair bake, we skip all alpha masks
                    // and use one from the texture, for both
                    // alpha and morph layers
                    if (bakeType == BakeType.Hair)
                    {
                        if (texture.Alpha != null)
                        {
                            bakedTexture.Image.Bump = texture.Alpha;
                        }
                        else
                        {
                            for (int j = 0; j < bakedTexture.Image.Bump.length; j++) 
                            	bakedTexture.Image.Bump[j] = (byte)Utils.UByteMaxValue;
                        }
                    }
                    // Apply parametrized alpha masks
                    else if (textures.get(i).AlphaMasks != null && textures.get(i).AlphaMasks.size() > 0)
                    {
                        // Combined mask for the layer, fully transparent to begin with
                        ManagedImage combinedMask = new ManagedImage(bakeWidth, bakeHeight, ImageChannels.get(ManagedImage.ImageChannels.Alpha.getIndex()));

                        int addedMasks = 0;

                        // First add mask in normal blend mode
                        for (Entry<VisualAlphaParam, Float> kvp : textures.get(i).AlphaMasks.entrySet())
                        {
                            if (!MaskBelongsToBake(kvp.getKey().TGAFile)) continue;

                            if (kvp.getKey().MultiplyBlend == false && (kvp.getValue() > 0f || !kvp.getKey().SkipIfZero))
                            {
                                ApplyAlpha(combinedMask, kvp.getKey(), kvp.getValue());
//                                FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType  + "-layer-" + i + "-mask-" + addedMasks + ".tga"), combinedMask.ExportTGA());
                                addedMasks++;
                            }
                        }

                        // If there were no mask in normal blend mode make aplha fully opaque
                        if (addedMasks == 0) 
                        	for (int l = 0; l < combinedMask.Alpha.length; l++) 
                        		combinedMask.Alpha[l] = (byte)Utils.UByteMaxValue;

                        // Add masks in multiply blend mode
                        for (Entry<VisualAlphaParam, Float> kvp : textures.get(i).AlphaMasks.entrySet())
                        {
                            if (!MaskBelongsToBake(kvp.getKey().TGAFile)) continue;

                            if (kvp.getKey().MultiplyBlend == true && (kvp.getValue() > 0f || !kvp.getKey().SkipIfZero))
                            {
                                ApplyAlpha(combinedMask, kvp.getKey(), kvp.getValue());
                                addedMasks++;
//                                FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType  + "-layer-" + i + "-mask-" + addedMasks + ".tga"), combinedMask.ExportTGA());
                            }
                        }

                        if (addedMasks > 0)
                        {
                            // Apply combined alpha mask to the cloned texture
                            AddAlpha(texture, combinedMask);

                            // Is this layer used for morph mask? If it is, use its
                            // alpha as the morth for the whole bake
                            if (textures.get(i).TextureIndex == AppearanceManager.MorphLayerForBakeType(bakeType))
                            {
                                bakedTexture.Image.Bump = combinedMask.Alpha;
                            }
                        }
//                        FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType + "-masked-texture-" + i + ".tga"), texture.ExportTGA());
//                        System.out.println(Utils.bytesToHexDebugString(texture.ExportTGA(), 1000, bakeType + "-masked-texture-" + i + ".tga"));
                    }
                }

                boolean useAlpha = i == 0 && (bakeType == BakeType.Skirt || bakeType == BakeType.Hair);
                DrawLayer(texture, useAlpha);
//                FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType + "-layer-" + i + ".tga"), texture.ExportTGA());
//                System.out.println(Utils.bytesToHexDebugString(texture.ExportTGA(), 1000, bakeType + "-layer-" + i + ".tga"));

            }

            // For head, we add skin last
            if (SkinTexture && bakeType == BakeType.Head)
            {
                ManagedImage texture = textures.get(0).Texture.Image.Clone();
                if (texture.Width != bakeWidth || texture.Height != bakeHeight)
                {
                    try { texture.ResizeNearestNeighbor(bakeWidth, bakeHeight); }
                    catch (Exception e) {JLogger.warn(Utils.getExceptionStackTraceAsString(e)); }
                }
                DrawLayer(texture, false);
//                FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType + "_texture0" + ".tga"), texture.ExportTGA());
//                FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType + "_addedSkin" + ".tga"), bakedTexture.Image.ExportTGA());
            }

            // Apply any alpha wearable textures to make parts of the avatar disappear
            if (alphaWearableTexture != null)
            {
                AddAlpha(bakedTexture.Image, alphaWearableTexture);
            }

            // We are done, encode asset for finalized bake
            bakedTexture.Encode();
            
            //TODO Testing Code
//            String filepath = "openmetaverse_data/logs/" + UUID.Random().toString();
//            FileUtils.saveJpgImage(bakeType.toString(), "openmetaverse_data/logs/" + UUID.Random().toString(), 
//            		bakedTexture.Image.Width, bakedTexture.Image.Height, bakedTexture.Image.ExportPixels());
            
//            FileUtils.writeBytes(new File("openmetaverse_data/logs/" + bakeType + ".tga"), bakedTexture.Image.ExportTGA());
//            FileUtils.writeBytes(new File(filepath+"245.j2k"), bakedTexture.AssetData);
        }

        private static Object ResourceSync = new Object();

        public static ManagedImage LoadResourceLayer(String fileName)
        {
            try
            {
//            	return LoadResourceLayer2(fileName);

                IBitmap bitmap = null;
                synchronized (ResourceSync)
                {
                	String resourcePath = null;
                    if((resourcePath = Helpers.GetResourcePath(fileName, Settings.RESOURCE_DIR))!=null)
                    {
                    	JLogger.debug("Got Resource Path: " + resourcePath);
                        bitmap = LoadTGAClass.LoadTGA(resourcePath);
                    }
                }
                if (bitmap == null)
                {
                	JLogger.error(String.format("Failed loading resource file: %s", fileName));
                    return null;
                }
                else
                {
                    return new ManagedImage(bitmap);
                }
            }
            catch (Exception e)
            {
                JLogger.error(String.format("Failed loading resource file: %s (%s)", fileName, Utils.getExceptionStackTraceAsString(e)));
                return null;
            }
        }

//        public static ManagedImage LoadResourceLayer2(String fileName) throws IOException, NotSupportedException, NotImplementedException
//        {
//            IBitmap bitmap = null;
//            synchronized (ResourceSync)
//            {
//            	String resourcePath = null;
//                if((resourcePath = Helpers.GetResourcePath(fileName, Settings.RESOURCE_DIR))!=null)
//                {
//                	JLogger.debug("Got Resource Path: " + resourcePath);
//                    bitmap = LoadTGAClass.LoadTGA(resourcePath);
//                }
//            }
//        	
//        	String resourcePath = null;
//        	byte[] bytes = null;
//            if((resourcePath = Helpers.GetResourcePath(fileName + ".bin", Settings.RESOURCE_DIR + "/compiled"))!=null)
//            {
//            	JLogger.debug("Got Resource Path: " + resourcePath);
//                bytes =FileUtils.readBytes(new File(resourcePath));
//            }
//            
//         // RGBA
//            int Height = bitmap.getHeight();
//            int Width = bitmap.getWidth();
//			for (int h = 0; h < Height; h++)
//			{
//				for (int w = 0; w < Width; w++)
//				{
//					int pos = (Height - 1 - h) * Width + w;
//					int srcPos = h * Width + w;
//
//					int origColor = bitmap.getRGB(w, h); 
//					
//					bitmap.setRGB(w, h, Utils.ubyteToInt(bytes[pos * 4 + 0]) << 16 | 
//					Utils.ubyteToInt(bytes[pos * 4 + 1]) << 8 |
//					Utils.ubyteToInt(bytes[pos * 4 + 2]) |
//					Utils.ubyteToInt(bytes[pos * 4 + 3]) << 24);
//					
//					int newColor = bitmap.getRGB(w, h);
//					
//					
//					if(origColor != newColor)
//						System.out.println(String.format("X %d Y %d orig color %d New color %d", w, h, origColor, bitmap.getRGB(w, h)));
//				}
//			}
//            return  new ManagedImage(bitmap);
//        }
        
        /// <summary>
        /// Converts avatar texture index (face) to Bake type
        /// </summary>
        /// <param name="index">Face number (AvatarTextureIndex)</param>
        /// <returns>BakeType, layer to which this texture belongs to</returns>
        public static BakeType BakeTypeFor(AvatarTextureIndex index)
        {
            switch (index)
            {
                case HeadBodypaint:
                    return BakeType.Head;

                case UpperBodypaint:
                case UpperGloves:
                case UpperUndershirt:
                case UpperShirt:
                case UpperJacket:
                    return BakeType.UpperBody;

                case LowerBodypaint:
                case LowerUnderpants:
                case LowerSocks:
                case LowerShoes:
                case LowerPants:
                case LowerJacket:
                    return BakeType.LowerBody;

                case EyesIris:
                    return BakeType.Eyes;

                case Skirt:
                    return BakeType.Skirt;

                case Hair:
                    return BakeType.Hair;

                default:
                    return BakeType.Unknown;
            }
        }
        //endregion

        //region Private layer compositing methods

        private boolean MaskBelongsToBake(String mask)
        {
            if ((bakeType == BakeType.LowerBody && mask.contains("upper"))
                || (bakeType == BakeType.LowerBody && mask.contains("shirt"))
                || (bakeType == BakeType.UpperBody && mask.contains("lower")))
            {
                return false;
            }
            else
            {
                return true;
            }
        }

        private boolean DrawLayer(ManagedImage source, boolean addSourceAlpha)
        {
            if (source == null) return false;

            boolean sourceHasColor;
            boolean sourceHasAlpha;
            boolean sourceHasBump;

            sourceHasColor = ((ImageChannels.and(source.Channels , ManagedImage.ImageChannels.Color)) != 0 &&
                    source.Red != null && source.Green != null && source.Blue != null);
            sourceHasAlpha = ((ImageChannels.and(source.Channels, ManagedImage.ImageChannels.Alpha)) != 0 && source.Alpha != null);
            sourceHasBump = ((ImageChannels.and(source.Channels , ManagedImage.ImageChannels.Bump)) != 0 && source.Bump != null);

            addSourceAlpha = (addSourceAlpha && sourceHasAlpha);

            byte alpha = (byte)Utils.UByteMaxValue;
            byte alphaInv = (byte)(Utils.UByteMaxValue - Utils.ubyteToInt(alpha));

            byte[] bakedRed = bakedTexture.Image.Red;
            byte[] bakedGreen = bakedTexture.Image.Green;
            byte[] bakedBlue = bakedTexture.Image.Blue;
            byte[] bakedAlpha = bakedTexture.Image.Alpha;
            byte[] bakedBump = bakedTexture.Image.Bump;

            byte[] sourceRed = source.Red;
            byte[] sourceGreen = source.Green;
            byte[] sourceBlue = source.Blue;
            byte[] sourceAlpha = sourceHasAlpha ? source.Alpha : null;
            byte[] sourceBump = sourceHasBump ? source.Bump : null;

            int i = 0;
            for (int y = 0; y < bakeHeight; y++)
            {
                for (int x = 0; x < bakeWidth; x++)
                {
                    if (sourceHasAlpha)
                    {
                        alpha = sourceAlpha[i];
                        alphaInv = (byte)(Utils.UByteMaxValue - Utils.ubyteToInt(alpha));
                    }

                    if (sourceHasColor)
                    {
                        bakedRed[i] = (byte)((Utils.ubyteToInt(bakedRed[i]) * Utils.ubyteToInt(alphaInv) 
                        		+ Utils.ubyteToInt(sourceRed[i]) * Utils.ubyteToInt(alpha)) >> 8);
                        bakedGreen[i] = (byte)((Utils.ubyteToInt(bakedGreen[i]) * Utils.ubyteToInt(alphaInv) 
                        		+ Utils.ubyteToInt(sourceGreen[i]) * Utils.ubyteToInt(alpha)) >> 8);
                        bakedBlue[i] = (byte)((Utils.ubyteToInt(bakedBlue[i]) * Utils.ubyteToInt(alphaInv) 
                        		+ Utils.ubyteToInt(sourceBlue[i]) * Utils.ubyteToInt(alpha)) >> 8);
                    }

                    if (addSourceAlpha)
                    {
                        if (Utils.ubyteToInt(sourceAlpha[i]) < Utils.ubyteToInt(bakedAlpha[i]))
                        {
                            bakedAlpha[i] = sourceAlpha[i];
                        }
                    }

                    if (sourceHasBump)
                        bakedBump[i] = sourceBump[i];

//                    System.out.println(String.format("Baked: <R %d G %d B %d A %d>  Alpha %d AlphaInv %d ", 
//                    		bakedRed[i], bakedGreen[i], bakedBlue[i], bakedAlpha[i], alpha, alphaInv));
                    
                    ++i;
                }
            }

            return true;
        }

        /// <summary>
        /// Make sure images exist, resize source if needed to match the destination
        /// </summary>
        /// <param name="dest">Destination image</param>
        /// <param name="src">Source image</param>
        /// <returns>Sanitization was succefull</returns>
        private boolean SanitizeLayers(ManagedImage dest, ManagedImage src)
        {
            if (dest == null || src == null) return false;

            if ((ImageChannels.and(dest.Channels, ManagedImage.ImageChannels.Alpha)) == 0)
            {
                dest.ConvertChannels(ImageChannels.get(ImageChannels.or(dest.Channels, ManagedImage.ImageChannels.Alpha)));
            }

            if (dest.Width != src.Width || dest.Height != src.Height)
            {
                try { src.ResizeNearestNeighbor(dest.Width, dest.Height); }
                catch (Exception e) { return false; }
            }

            return true;
        }


        private void ApplyAlpha(ManagedImage dest, VisualAlphaParam param, float val)
        {
            ManagedImage src = LoadResourceLayer(param.TGAFile);

            if (dest == null || src == null || src.Alpha == null) return;

            if ((ImageChannels.and(dest.Channels, ManagedImage.ImageChannels.Alpha)) == 0)
            {
                dest.ConvertChannels(ImageChannels.get(ImageChannels.or(dest.Channels, ManagedImage.ImageChannels.Alpha)));
            }

            if (dest.Width != src.Width || dest.Height != src.Height)
            {
                try { src.ResizeNearestNeighbor(dest.Width, dest.Height); }
                catch (Exception e) { JLogger.warn(Utils.getExceptionStackTraceAsString(e));return; }
            }

            for (int i = 0; i < dest.Alpha.length; i++)
            {
                byte alpha = Utils.ubyteToInt(src.Alpha[i]) <= ((1 - val) * 255) ? (byte)0 : (byte)255;

                if (param.MultiplyBlend)
                {
                    dest.Alpha[i] =  addAlpha(dest.Alpha[i], alpha);
                }
                else
                {
                    if (Utils.ubyteToInt(alpha) > Utils.ubyteToInt(dest.Alpha[i]))
                    {
                        dest.Alpha[i] = alpha;
                    }
                }
            }
        }

        private byte addAlpha(byte c, byte a)
        {
        	return (byte)(( (c&0xff) * (a&0xff) ) >> 8);
        }
        	
        private void AddAlpha(ManagedImage dest, ManagedImage src)
        {
            if (!SanitizeLayers(dest, src)) return;
                        
            for (int i = 0; i < dest.Alpha.length; i++)
            {
                if (Utils.ubyteToInt(src.Alpha[i]) < Utils.ubyteToInt(dest.Alpha[i]))
                {
                    dest.Alpha[i] = src.Alpha[i];
                }
            }
        }

        private void MultiplyLayerFromAlpha(ManagedImage dest, ManagedImage src)
        {
            if (!SanitizeLayers(dest, src)) return;

            for (int i = 0; i < dest.Red.length; i++)
            {
                dest.Red[i] = addAlpha(dest.Red[i], src.Alpha[i]);
                dest.Green[i] = addAlpha(dest.Green[i], src.Alpha[i]);
                dest.Blue[i] = addAlpha(dest.Blue[i], src.Alpha[i]);
            }
        }

        private void ApplyTint(ManagedImage dest, Color4 src)
        {
            if (dest == null) return;
            
            byte rByte = Utils.floatToByte(src.getR(), 0f, 1f);
            byte gByte = Utils.floatToByte(src.getG(), 0f, 1f);
            byte bByte = Utils.floatToByte(src.getB(), 0f, 1f);
            
            for (int i = 0; i < dest.Red.length; i++)
            {
                dest.Red[i] = addAlpha(dest.Red[i], rByte);
                dest.Green[i] = addAlpha(dest.Green[i], gByte);
                dest.Blue[i] = addAlpha(dest.Blue[i], bByte);
            }
        }

        /// <summary>
        /// Fills a baked layer as a solid *appearing* color. The colors are 
        /// subtly dithered on a 16x16 grid to prevent the JPEG2000 stage from 
        /// compressing it too far since it seems to cause upload failures if 
        /// the image is a pure solid color
        /// </summary>
        /// <param name="color">Color of the base of this layer</param>
        private void InitBakedLayerColor(Color4 color)
        {
            InitBakedLayerColor(color.getR(), color.getG(), color.getB());
        }

        /// <summary>
        /// Fills a baked layer as a solid *appearing* color. The colors are 
        /// subtly dithered on a 16x16 grid to prevent the JPEG2000 stage from 
        /// compressing it too far since it seems to cause upload failures if 
        /// the image is a pure solid color
        /// </summary>
        /// <param name="r">Red value</param>
        /// <param name="g">Green value</param>
        /// <param name="b">Blue value</param>
        private void InitBakedLayerColor(float r, float g, float b)
        {
            byte rByte = Utils.floatToByte(r, 0f, 1f);
            byte gByte = Utils.floatToByte(g, 0f, 1f);
            byte bByte = Utils.floatToByte(b, 0f, 1f);

//            System.out.println(String.format(" InitBakedLayerColor Color %d %d %d", rByte, gByte, bByte));
            
            byte rAlt, gAlt, bAlt;

            rAlt = rByte;
            gAlt = gByte;
            bAlt = bByte;

            if (Utils.ubyteToInt(rByte) < Utils.UByteMaxValue)
                rAlt++;
            else rAlt--;

            if (Utils.ubyteToInt(gByte) < Utils.UByteMaxValue)
                gAlt++;
            else gAlt--;

            if (Utils.ubyteToInt(bByte) < Utils.UByteMaxValue)
                bAlt++;
            else bAlt--;

            int i = 0;

            byte[] red = bakedTexture.Image.Red;
            byte[] green = bakedTexture.Image.Green;
            byte[] blue = bakedTexture.Image.Blue;
            byte[] alpha = bakedTexture.Image.Alpha;
            byte[] bump = bakedTexture.Image.Bump;

            for (int y = 0; y < bakeHeight; y++)
            {
                for (int x = 0; x < bakeWidth; x++)
                {
                    if (((x ^ y) & 0x10) == 0)
                    {
                        red[i] = rAlt;
                        green[i] = gByte;
                        blue[i] = bByte;
                        alpha[i] = (byte)Utils.UByteMaxValue;
                        bump[i] = 0;
                    }
                    else
                    {
                        red[i] = rByte;
                        green[i] = gAlt;
                        blue[i] = bAlt;
                        alpha[i] = (byte)Utils.UByteMaxValue;
                        bump[i] = 0;
                    }

                    ++i;
                }
            }

        }
        //endregion
}
