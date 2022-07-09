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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.List;
import java.util.Observable;
import java.util.TimerTask;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.zip.DeflaterInputStream;
import java.util.zip.DeflaterOutputStream;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import com.ngt.jopenmetaverse.shared.sim.GridClient;
import com.ngt.jopenmetaverse.shared.sim.Settings;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.EventTimer;
import com.ngt.jopenmetaverse.shared.sim.events.MethodDelegate;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPool;
import com.ngt.jopenmetaverse.shared.sim.events.ThreadPoolFactory;
import com.ngt.jopenmetaverse.shared.sim.events.asm.ComputeAssetCacheFilenameEventArgs;
import com.ngt.jopenmetaverse.shared.sim.events.asm.ImageDownload;
import com.ngt.jopenmetaverse.shared.sim.events.nm.DisconnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginProgressEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginStatus;
import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.FileUtils;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;
import com.ngt.jopenmetaverse.shared.util.ZlibCompression;

/// <summary>
/// Class that handles the local asset cache
/// </summary>
public class AssetCache implements IAssetCache
{
	// User can plug in a routine to compute the asset cache location
	//        public delegate String ComputeAssetCacheFilenameDelegate(String cacheDir, UUID assetID);

	//        public ComputeAssetCacheFilenameDelegate ComputeAssetCacheFilename = null;

	MethodDelegate<String, ComputeAssetCacheFilenameEventArgs> computeAssetCacheFilenameDelegate = null;

	public MethodDelegate<String, ComputeAssetCacheFilenameEventArgs> getComputeAssetCacheFilenameDelegate() {
		return computeAssetCacheFilenameDelegate;
	}
	public void setComputeAssetCacheFilenameDelegate(
			MethodDelegate<String, ComputeAssetCacheFilenameEventArgs> computeAssetCacheFilenameDelegate) {
		this.computeAssetCacheFilenameDelegate = computeAssetCacheFilenameDelegate;
	}

	private static ThreadPool threadPool = ThreadPoolFactory.getThreadPool();


	private GridClient Client;
	//	private Thread cleanerThread;
	private AtomicBoolean isCleanerThread = new AtomicBoolean(false); 
	private EventTimer cleanerTimer;
	private double pruneInterval = 1000 * 60 * 5;
	private boolean autoPruneEnabled = true;
	private AssetTagMap assetTagMap = new AssetTagMap();

	/// <summary>
	/// Allows setting weather to periodicale prune the cache if it grows too big
	/// Default is enabled, when caching is enabled
	/// </summary>
	public boolean getAutoPruneEnabled()
	{
		return autoPruneEnabled; 
	}

	public void setAutoPruneEnabled(boolean value)
	{
		autoPruneEnabled = value;

		if (autoPruneEnabled)
		{
			SetupTimer();
		}
		else
		{
			DestroyTimer();
		}
	}

	/// <summary>
	/// How long (in ms) between cache checks (default is 5 min.) 
	/// </summary>
	public void setAutoPruneInterval(double value)
	{
		pruneInterval = value;
		SetupTimer();
	}

	public double getAutoPruneInterval()
	{
		return pruneInterval;
	}

	/// <summary>
	/// Default constructor
	/// </summary>
	/// <param name="client">A reference to the GridClient object</param>
	public AssetCache(GridClient client)
	{
		Client = client;          
		Client.network.RegisterLoginProgressCallback(new EventObserver<LoginProgressEventArgs>()
				{
			@Override
			public void handleEvent(Observable o, LoginProgressEventArgs e) {
				if (e.getStatus() == LoginStatus.Success)
				{
					SetupTimer();
				}}});

		Client.network.RegisterOnDisconnectedCallback(new EventObserver<DisconnectedEventArgs>()
				{
			@Override
			public void handleEvent(Observable o, DisconnectedEventArgs e) {
				DestroyTimer();
			}
				});

		//            Client.network.LoginProgress += delegate(object sender, LoginProgressEventArgs e)
		//            {
		//                if (e.Status == LoginStatus.Success)
		//                {
		//                    SetupTimer();
		//                }
		//            };
		//
		//            Client.Network.Disconnected += delegate(object sender, DisconnectedEventArgs e) { DestroyTimer(); };
	}


	/// <summary>
	/// Disposes cleanup timer
	/// </summary>
	private void DestroyTimer()
	{
		if (cleanerTimer != null)
		{
			cleanerTimer.cancel();
			cleanerTimer = null;
		}
	}

	/// <summary>
	/// Only create timer when needed
	/// </summary>
	private void SetupTimer()
	{
		if (Operational() && autoPruneEnabled && Client.network.getConnected())
		{
			if (cleanerTimer == null)
			{
				cleanerTimer = new EventTimer(new TimerTask()
				{
					@Override
					public void run() {
						cleanerTimer_Elapsed();
					}                		
				});

				//                    cleanerTimer = new System.Timers.Timer(pruneInterval);
				//                    cleanerTimer.Elapsed += new System.Timers.ElapsedEventHandler(cleanerTimer_Elapsed);
			}
			cleanerTimer.schedule((long)pruneInterval, (int)pruneInterval);

			//                cleanerTimer.Interval = pruneInterval;
			//                cleanerTimer.Enabled = true;
		}
	}

	/// <summary>
	/// Return bytes read from the local asset cache, null if it does not exist
	/// </summary>
	/// <param name="assetID">UUID of the asset we want to get</param>
	/// <returns>Raw bytes of the asset, or null on failure</returns>
	public byte[] getCachedAssetBytes(UUID assetID)
	{
		if (!Operational())
		{
			return null;
		}
		try
		{
			byte[] data = null;
			File f = new File(FileName(assetID));
			if (f.exists())
			{
				JLogger.debug("Reading " + FileName(assetID) + " from asset cache.");
				FileInputStream fs = new FileInputStream(f); 
				//                    data = File.ReadAllBytes(FileName(assetID));
				data = FileUtils.readBytes(fs);
				fs.close();
			}
			else
			{
				JLogger.debug("Reading " + StaticFileName(assetID) + " from static asset cache.");
				//                    data = File.ReadAllBytes(StaticFileName(assetID));
				f = new File(StaticFileName(assetID));
				if(f.exists())
				{
					FileInputStream fs = new FileInputStream(f); 
					data = FileUtils.readBytes(fs);
				}
			}
			assetTagMap.assetAccessed(assetID.toString());
			return data;
		}
		catch (Exception ex)
		{
			JLogger.warn("Failed reading asset from cache (" + ex.getMessage() + ")");
			return null;
		}
	}

	/// <summary>
	/// Return bytes read from the local asset cache, null if it does not exist
	/// </summary>
	/// <param name="assetID">UUID of the asset we want to get</param>
	/// <returns>Raw bytes of the asset, or null on failure</returns>
	public byte[] getCachedAssetBytes(String assetID)
	{
		if (!Operational())
		{
			return null;
		}
		try
		{
			byte[] data = null;
			File f = new File(FileName(assetID));
			if (f.exists())
			{
				JLogger.debug("Reading " + FileName(assetID) + " from asset cache.");
				FileInputStream fs = new FileInputStream(f); 
				//                    data = File.ReadAllBytes(FileName(assetID));
				data = FileUtils.readBytes(fs);
				fs.close();
			}
			else
			{
				JLogger.debug("Reading " + StaticFileName(assetID) + " from static asset cache.");
				//                    data = File.ReadAllBytes(StaticFileName(assetID));
				f = new File(StaticFileName(assetID));
				if(f.exists())
				{
					FileInputStream fs = new FileInputStream(f); 
					data = FileUtils.readBytes(fs);
				}
			}
			assetTagMap.assetAccessed(assetID.toString());
			return data;
		}
		catch (Exception ex)
		{
			JLogger.warn("Failed reading asset from cache (" + ex.getMessage() + ")");
			return null;
		}
	}
	

	/// <summary>
	/// Returns ImageDownload object of the
	/// image from the local image cache, null if it does not exist
	/// </summary>
	/// <param name="imageID">UUID of the image we want to get</param>
	/// <returns>ImageDownload object containing the image, or null on failure</returns>
	public ImageDownload getCachedImage(UUID imageID)
	{
		if (!Operational())
			return null;

		byte[] imageData = getCachedAssetBytes(imageID);
		if (imageData == null)
			return null;
		ImageDownload transfer = new ImageDownload();
		transfer.AssetType = AssetType.Texture;
		transfer.ID = imageID;
		transfer.Simulator = Client.network.getCurrentSim();
		transfer.Size = imageData.length;
		transfer.Success = true;
		transfer.Transferred = imageData.length;
		transfer.AssetData = imageData;
		return transfer;
	}

	/// <summary>
	/// Saves an asset to the local cache
	/// </summary>
	/// <param name="assetID">UUID of the asset</param>
	/// <param name="assetData">Raw bytes the asset consists of</param>
	/// <returns>Weather the operation was successfull</returns>
	public boolean saveAssetToCache(UUID assetID, byte[] assetData)
	{
		if (!Operational())
		{
			return false;
		}

		try
		{
			JLogger.debug("Saving " + FileName(assetID) + " to asset cache.");
			File dir = new File(Client.settings.ASSET_CACHE_DIR);
			if (!dir.exists())
			{
				//                    Directory.CreateDirectory(Client.settings.ASSET_CACHE_DIR);
				new File(Client.settings.ASSET_CACHE_DIR).mkdirs();
			}

			//                File.WriteAllBytes(FileName(assetID), assetData);
			File f = new File(FileName(assetID));
			FileOutputStream fos = new FileOutputStream(f);
			FileUtils.writeBytes(fos, assetData);
			assetTagMap.assetAdded(assetID.toString());
			fos.close();
		}
		catch (Exception ex)
		{
			JLogger.warn("Failed saving asset to cache (" + ex.getMessage() + ")" + "\n" + Utils.getExceptionStackTraceAsString(ex));
			return false;
		}

		return true;
	}

	/// <summary>
			/// Saves an asset to the local cache
			/// </summary>
			/// <param name="assetID">UUID of the asset</param>
			/// <param name="assetData">Raw bytes the asset consists of</param>
			/// <returns>Weather the operation was successfull</returns>
			public boolean saveAssetToCache(String assetID, byte[] assetData)
			{
				if (!Operational())
				{
					return false;
				}

				try
				{
					JLogger.debug("Saving " + FileName(assetID) + " to asset cache.");
					File dir = new File(Client.settings.ASSET_CACHE_DIR);
					if (!dir.exists())
					{
						//                    Directory.CreateDirectory(Client.settings.ASSET_CACHE_DIR);
						new File(Client.settings.ASSET_CACHE_DIR).mkdirs();
					}

					//                File.WriteAllBytes(FileName(assetID), assetData);
					File f = new File(FileName(assetID));
					FileOutputStream fos = new FileOutputStream(f);
					FileUtils.writeBytes(fos, assetData);
					assetTagMap.assetAdded(assetID.toString());
					fos.close();
				}
				catch (Exception ex)
				{
					JLogger.warn("Failed saving asset to cache (" + ex.getMessage() + ")" + "\n" + Utils.getExceptionStackTraceAsString(ex));
					return false;
				}

				return true;
			}
			
	
	/// <summary>
	/// Get the file name of the asset stored with gived UUID
	/// </summary>
	/// <param name="assetID">UUID of the asset</param>
	/// <returns>Null if we don't have that UUID cached on disk, file name if found in the cache folder</returns>
	public String AssetFileName(UUID assetID)
	{
		if (!Operational())
		{
			return null;
		}

		String fileName = FileName(assetID);
		File f = new File(fileName);
		if (f.exists())
			return fileName;
		else
			return null;
	}

	public String AssetFileName(String assetID)
	{
		if (!Operational())
		{
			return null;
		}

		String fileName = FileName(assetID);
		File f = new File(fileName);
		if (f.exists())
			return fileName;
		else
			return null;
	}
	
	
	public File getAssetFile(UUID assetID)
	{
		if (!Operational())
		{
			return null;
		}

		String fileName = FileName(assetID);
		File f = new File(fileName);
		if (f.exists())
			return f;
		else
			return null;
	}
	
    public File getAssetFile(String assetID)
    {
            if (!Operational())
            {
                    return null;
            }

            String fileName = FileName(assetID);
            File f = new File(fileName);
            if (f.exists())
                    return f;
            else
                    return null;
    }

    
    
    /// <summary>
    /// Constructs a file name of the cached asset
    /// </summary>
    /// <param name="assetID">UUID of the asset</param>
    /// <returns>String with the file name of the cahced asset</returns>
    private String FileName(String assetID)
    {
          if (computeAssetCacheFilenameDelegate != null)
          {
                  return computeAssetCacheFilenameDelegate.execute(new ComputeAssetCacheFilenameEventArgs(Client.settings.ASSET_CACHE_DIR, assetID));
          }
            return Client.settings.ASSET_CACHE_DIR + "/" + assetID.toString();
    }

	
	
	/// <summary>
	/// Checks if the asset exists in the local cache
	/// </summary>
	/// <param name="assetID">UUID of the asset</param>
	/// <returns>True is the asset is stored in the cache, otherwise false</returns>
	public boolean hasAsset(UUID assetID)
	{
		if (!Operational())
			return false;
		else
		{
			String fileName = FileName(assetID);
			File f = new File(fileName);
			if (f.exists())
				return true;
			else
			{
				fileName = StaticFileName(assetID);
				return new File(fileName).exists();
			}
		}
	}

	/// <summary>
		/// Checks if the asset exists in the local cache
		/// </summary>
		/// <param name="assetID">UUID of the asset</param>
		/// <returns>True is the asset is stored in the cache, otherwise false</returns>
		public boolean hasAsset(String assetID)
		{
			if (!Operational())
				return false;
			else
			{
				String fileName = FileName(assetID);
				File f = new File(fileName);
				if (f.exists())
					return true;
				else
				{
					fileName = StaticFileName(assetID);
					return new File(fileName).exists();
				}
			}
		}
	
	/// <summary>
	/// Wipes out entire cache
	/// </summary>
	public void clear()
	{
		String cacheDir = Client.settings.ASSET_CACHE_DIR;
		File dir = new File(cacheDir);
		assetTagMap.clear();
		if (!dir.exists())
		{
			return;
		}

		//            DirectoryInfo di = new DirectoryInfo(cacheDir);
		//            // We save file with UUID as file name, only delete those
		//            FileInfo[] files = di.GetFiles("????????-????-????-????-????????????", SearchOption.TopDirectoryOnly);

//		File[] files = getFileList(cacheDir, "????????-????-????-????-????????????", true);
		File[] files = FileUtils.getFileList(cacheDir, "\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}", true);

		
		int num = 0;
		for (File file : files)
		{
			file.delete();
			++num;
		}

		JLogger.debug("Wiped out " + num + " files from the cache directory.");
	}

	/// <summary>
	/// Brings cache size to the 90% of the max size
	/// </summary>
	public void prune()
	{
		String cacheDir = Client.settings.ASSET_CACHE_DIR;
		File dir = new File(cacheDir);

		if (!dir.exists())
		{
			return;
		}

		//            DirectoryInfo di = new DirectoryInfo(cacheDir);
		//            // We save file with UUID as file name, only count those
		//            FileInfo[] files = di.GetFiles("????????-????-????-????-????????????", SearchOption.TopDirectoryOnly);

		File[] files = FileUtils.getFileList(cacheDir, "\\w{8}-\\w{4}-\\w{4}-\\w{4}-\\w{12}", true);
		long size = GetFileSize(files);

		if (size > Client.settings.ASSET_CACHE_MAX_SIZE)
		{
			//                Array.Sort(files, new SortFilesByAccesTimeHelper());
			List<String> assetIDs = assetTagMap.getAssets();
			long targetSize = (long)(Client.settings.ASSET_CACHE_MAX_SIZE * 0.9);
			int num = 0;
			File file = null;
			for (String assetID : assetIDs)
			{
				if((file = getAssetFile(assetID))!=null)
				{
					++num;
					size -= file.length();
					file.delete();
					if (size < targetSize)
					{
						break;
					}
				}
			}
			JLogger.debug(num + " files deleted from the cache, cache size now: " + NiceFileSize(size));
		}
		else
		{
			JLogger.debug("Cache size is " + NiceFileSize(size) + ", file deletion not needed");
		}

	}

	/// <summary>
	/// Asynchronously brings cache size to the 90% of the max size
	/// </summary>
	public void beginPrune()
	{
		//TODO need to verify 
		//            // Check if the background cache cleaning thread is active first
		//            if (cleanerThread != null && cleanerThread.isAlive())
		//            {
		//                return;
		//            }

		if(!isCleanerThread.get())
		{
			isCleanerThread.set(true);
			threadPool.execute(new Runnable() {
				public void run() {
					isCleanerThread.set(true);
					synchronized (this)
					{
						//                cleanerThread = new Thread(new ThreadStart(this.Prune));
						//                cleanerThread.IsBackground = true;
						//                cleanerThread.Start();
						prune();
					}
					isCleanerThread.set(false);
				}
			});
		}
	}

	/// <summary>
	/// Constructs a file name of the cached asset
	/// </summary>
	/// <param name="assetID">UUID of the asset</param>
	/// <returns>String with the file name of the cahced asset</returns>
	private String FileName(UUID assetID)
	{
		if (computeAssetCacheFilenameDelegate != null)
		{
			return computeAssetCacheFilenameDelegate.execute(new ComputeAssetCacheFilenameEventArgs(Client.settings.ASSET_CACHE_DIR, assetID.toString()));
		}
		return Client.settings.ASSET_CACHE_DIR + "/" + assetID.toString();
	}

	/// <summary>
	/// Constructs a file name of the static cached asset
	/// </summary>
	/// <param name="assetID">UUID of the asset</param>
	/// <returns>String with the file name of the static cached asset</returns>
	private String StaticFileName(UUID assetID)
	{
		return Settings.RESOURCE_DIR + "/" + "static_assets" + "/" + assetID.toString();
	}

	/// <summary>
		/// Constructs a file name of the static cached asset
		/// </summary>
		/// <param name="assetID">UUID of the asset</param>
		/// <returns>String with the file name of the static cached asset</returns>
		private String StaticFileName(String assetID)
		{
			return Settings.RESOURCE_DIR + "/" + "static_assets" + "/" + assetID.toString();
		}
	
	
	/// <summary>
	/// Adds up file sizes passes in a FileInfo array
	/// </summary>
	private long GetFileSize(File[] files)
	{
		long ret = 0;
		for (File file : files)
		{
			ret += file.length();
		}
		return ret;
	}

	/// <summary>
	/// Checks whether caching is enabled
	/// </summary>
	private boolean Operational()
	{
		return Client.settings.USE_ASSET_CACHE;
	}

	/// <summary>
	/// Periodically prune the cache
	/// </summary>
	private void cleanerTimer_Elapsed()
	{
		beginPrune();
	}

	/// <summary>
	/// Nicely formats file sizes
	/// </summary>
	/// <param name="byteCount">Byte size we want to output</param>
	/// <returns>String with humanly readable file size</returns>
	private String NiceFileSize(long byteCount)
	{
		String size = "0 Bytes";
		if (byteCount >= 1073741824)
			size = String.format("%f", byteCount / 1073741824.0) + " GB";
		else if (byteCount >= 1048576)
			size = String.format("%f", byteCount / 1048576.0) + " MB";
		else if (byteCount >= 1024)
			size = String.format("%f", byteCount / 1024.0) + " KB";
		else if (byteCount > 0 && byteCount < 1024)
			size = byteCount + " Bytes";

		return size;
	}

	//region Cached image save and load
	private final static String COMPRESSED_IMAGE_MAGIC_HEADER = "jomv_compressed_img";
	
	private String getCompressedImageName(UUID assetID)
	{
		return assetID.toString() + ".imggz";
	}
	
	/*
	 * use AssetCache LoadCompressedImage method  
	 */
	public LoadCachedImageResult loadCompressedImageFromCache(UUID textureID) throws IOException
	{
		String textureFileName = getCompressedImageName(textureID);
		LoadCachedImageResult r = null;
		if(!hasAsset(textureFileName))
		{
			return null;
		}
		else
		{
			int i = 0;
			r = new LoadCachedImageResult();
			byte[] cachedData = getCachedAssetBytes(textureFileName);
			//Check if the file is actually compressed texture image
//			byte[] header = new byte[36];
//			Utils.arraycopy(cachedData, 0, header, 0, header.length);
			if (!COMPRESSED_IMAGE_MAGIC_HEADER.equals(Utils.bytesToString(cachedData, 0, COMPRESSED_IMAGE_MAGIC_HEADER.length())))
			{
				return null;
			}
			
			i += COMPRESSED_IMAGE_MAGIC_HEADER.length();

			if (cachedData[i++] != 1) // check version
			{
				return null;
			}
			
			r.hasAlpha = cachedData[i++] == 1;
			r.fullAlpha = cachedData[i++] == 1;
			r.isMask = cachedData[i++] == 1;
			
			int uncompressedSize = Utils.bytesToInt(cachedData, i);
			i += 4;

			textureID = new UUID(cachedData, i);
			i += 16;

//			r.data = new byte[uncompressedSize];
			ByteArrayOutputStream out = new ByteArrayOutputStream(); 
			ByteArrayInputStream compressed = new ByteArrayInputStream(cachedData, i,  cachedData.length - i); 
			ZlibCompression.decompressFile(compressed, out);
			r.data = out.toByteArray();
			
//			Utils.arraycopy(cachedData, i, r.data, 0, cachedData.length - i);
//			bis.close();
		}
		return r;
		}
	
	public boolean compressAndSaveImageToCache(UUID textureID, byte[] tgaData, boolean hasAlpha, boolean fullAlpha, boolean isMask) throws IOException
	{
		
		ByteArrayOutputStream fis = new ByteArrayOutputStream(); 
		int i = 0;
		// magic header
		fis.write(Utils.stringToBytes(COMPRESSED_IMAGE_MAGIC_HEADER), 0, COMPRESSED_IMAGE_MAGIC_HEADER.length());
		i += COMPRESSED_IMAGE_MAGIC_HEADER.length();

		// version
		fis.write((byte)1);
		i++;

		// texture info
		fis.write(hasAlpha ? (byte)1 : (byte)0);
		fis.write(fullAlpha ? (byte)1 : (byte)0);
		fis.write(isMask ? (byte)1 : (byte)0);
		i += 3;

		// texture size
		byte[] uncompressedSize = Utils.intToBytes(tgaData.length);
		fis.write(uncompressedSize, 0, uncompressedSize.length);
		i += uncompressedSize.length;

		// texture id
		byte[] id = new byte[16];
		textureID.ToBytes(id, 0);
		fis.write(id, 0, 16);
		i += 16;

//		System.out.println("Header Size " + i + " Date to be compressed " + tgaData.length);
		
		ByteArrayOutputStream compressed = new ByteArrayOutputStream(); 
		ZlibCompression.compressFile(new ByteArrayInputStream(tgaData), compressed);
		
		fis.write(compressed.toByteArray());
		saveAssetToCache(getCompressedImageName(textureID), fis.toByteArray());
		
//		compressed.close();
		fis.close();
		return true;
	}

	
	
	//        /// <summary>
	//        /// Helper class for sorting files by their last accessed time
	//        /// </summary>
	//        private class SortFilesByAccesTimeHelper : IComparer<FileInfo>
	//        {
	//            int IComparer<FileInfo>.Compare(File f1, File f2)
	//            {
	//                if (f1.LastAccessTime > f2.LastAccessTime)
	//                    return 1;
	//                if (f1.LastAccessTime < f2.LastAccessTime)
	//                    return -1;
	//                else
	//                    return 0;
	//            }
	//        }

}