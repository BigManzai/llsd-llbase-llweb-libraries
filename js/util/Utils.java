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
package com.ngt.jopenmetaverse.shared.util;

import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Random;
import java.util.logging.Logger;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang.ArrayUtils;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

import com.ngt.jopenmetaverse.shared.types.Enums.AssetType;
import com.ngt.jopenmetaverse.shared.types.Enums.InventoryType;
import com.ngt.jopenmetaverse.shared.types.Enums.SaleType;
import com.ngt.jopenmetaverse.shared.types.EnumsPrimitive.AttachmentPoint;
import com.ngt.jopenmetaverse.shared.types.UUID;

public class Utils {
	static Logger logger = Logger.getLogger("Utils");

	// / <summary>
	// / Operating system
	// / </summary>
	public static enum Platform {
		// / <summary>Unknown</summary>
		Unknown,
		// / <summary>Microsoft Windows</summary>
		Windows,
		// / <summary>Microsoft Windows CE</summary>
		WindowsCE,
		// / <summary>Linux</summary>
		Linux,
		// / <summary>Apple OSX</summary>
		OSX;
	}

	// / <summary>
	// / Runtime platform
	// / </summary>
	public static enum Runtime {
		// / <summary>.NET runtime</summary>
		Windows,
		// / <summary>Mono runtime: http://www.mono-project.com/</summary>
		Mono
	}

	public final static float E = (float) Math.E;
	public final static float LOG10E = 0.4342945f;
	public final static float LOG2E = 1.442695f;
	public final static float PI = (float) Math.PI;
	public final static float TWO_PI = (float) (Math.PI * 2.0d);
	public final static float PI_OVER_TWO = (float) (Math.PI / 2.0d);
	public final static float PI_OVER_FOUR = (float) (Math.PI / 4.0d);
	// / <summary>Used for converting degrees to radians</summary>
	public final static float DEG_TO_RAD = (float) (Math.PI / 180.0d);
	// / <summary>Used for converting radians to degrees</summary>
	public final static float RAD_TO_DEG = (float) (180.0d / Math.PI);

	// /// <summary>Provide a single instance of the CultureInfo class to
	// /// help parsing in situations where the grid assumes an en-us
	// /// culture</summary>
	// public static final System.Globalization.CultureInfo EnUsCulture =
	// new System.Globalization.CultureInfo("en-us");
	//
	// /// <summary>UNIX epoch in DateTime format</summary>
	public static final Date Epoch = new Date(1970, 1, 1);
	//
	// Number of milliseconds since 12:00 midnight, January 1, 0001 A.D. (C.E.)
	// in the GregorianCalendar calendar
	// to Epoch, January 1, 1970 00:00:00.000 GMT (Gregorian)
	// Useful to convert C# DateTime to Java Date objects
	public static final long MilliSecOrg2Epoch = 210512742688361L;

	public static final byte[] EmptyBytes = new byte[0];
	//
	// /// <summary>Provide a single instance of the MD5 class to avoid making
	// /// duplicate copies and handle thread safety</summary>
	// private static final System.Security.Cryptography.MD5 MD5Builder =
	// new System.Security.Cryptography.MD5CryptoServiceProvider();
	//
	// /// <summary>Provide a single instance of the SHA-1 class to avoid
	// /// making duplicate copies and handle thread safety</summary>
	// private static final System.Security.Cryptography.SHA1 SHA1Builder =
	// new System.Security.Cryptography.SHA1CryptoServiceProvider();
	//
	// private static final System.Security.Cryptography.SHA256 SHA256Builder =
	// new System.Security.Cryptography.SHA256Managed();

	// / <summary>Provide a single instance of a random number generator
	// / to avoid making duplicate copies and handle thread safety</summary>
	private static final Random RNG = new Random();

	// region Math

	// / <summary>
	// / clamp a given value between a range
	// / </summary>
	// / <param name="value">Value to clamp</param>
	// / <param name="min">Minimum allowable value</param>
	// / <param name="max">Maximum allowable value</param>
	// / <returns>A value inclusively between lower and upper</returns>
	public static float clamp(float value, float min, float max) {
		// First we check to see if we're greater than the max
		value = (value > max) ? max : value;

		// Then we check to see if we're less than the min.
		value = (value < min) ? min : value;

		// There's no check to see if min > max.
		return value;
	}

	// / <summary>
	// / clamp a given value between a range
	// / </summary>
	// / <param name="value">Value to clamp</param>
	// / <param name="min">Minimum allowable value</param>
	// / <param name="max">Maximum allowable value</param>
	// / <returns>A value inclusively between lower and upper</returns>
	public static double clamp(double value, double min, double max) {
		// First we check to see if we're greater than the max
		value = (value > max) ? max : value;

		// Then we check to see if we're less than the min.
		value = (value < min) ? min : value;

		// There's no check to see if min > max.
		return value;
	}

	// / <summary>
	// / clamp a given value between a range
	// / </summary>
	// / <param name="value">Value to clamp</param>
	// / <param name="min">Minimum allowable value</param>
	// / <param name="max">Maximum allowable value</param>
	// / <returns>A value inclusively between lower and upper</returns>
	public static int clamp(int value, int min, int max) {
		// First we check to see if we're greater than the max
		value = (value > max) ? max : value;

		// Then we check to see if we're less than the min.
		value = (value < min) ? min : value;

		// There's no check to see if min > max.
		return value;
	}

	// / <summary>
	// / Round a floating-point value to the nearest integer
	// / </summary>
	// / <param name="val">Floating point number to round</param>
	// / <returns>Integer</returns>
	public static int round(float val) {
		return (int) Math.floor(val + 0.5f);
	}

	// / <summary>
	// / Test if a single precision float is a finite number
	// / </summary>
	public static boolean isFinite(float value) {
		return !(Float.isNaN(value) || Float.isInfinite(value));
	}

	// / <summary>
	// / Test if a double precision float is a finite number
	// / </summary>
	public static boolean isFinite(double value) {
		return !(Double.isNaN(value) || Double.isInfinite(value));
	}

	// / <summary>
	// / Get the distance between two floating-point values
	// / </summary>
	// / <param name="value1">First value</param>
	// / <param name="value2">Second value</param>
	// / <returns>The distance between the two values</returns>
	public static float distance(float value1, float value2) {
		return Math.abs(value1 - value2);
	}

	public static float hermite(float value1, float tangent1, float value2,
			float tangent2, float amount) {
		// All transformed to double not to lose precission
		// Otherwise, for high numbers of param:amount the result is NaN instead
		// of Infinity
		double v1 = value1, v2 = value2, t1 = tangent1, t2 = tangent2, s = amount, result;
		double sCubed = s * s * s;
		double sSquared = s * s;

		if (amount == 0f)
			result = value1;
		else if (amount == 1f)
			result = value2;
		else
			result = (2d * v1 - 2d * v2 + t2 + t1) * sCubed
					+ (3d * v2 - 3d * v1 - 2d * t1 - t2) * sSquared + t1 * s
					+ v1;
		return (float) result;
	}

	public static double hermite(double value1, double tangent1, double value2,
			double tangent2, double amount) {
		// All transformed to double not to lose precission
		// Otherwise, for high numbers of param:amount the result is NaN instead
		// of Infinity
		double v1 = value1, v2 = value2, t1 = tangent1, t2 = tangent2, s = amount, result;
		double sCubed = s * s * s;
		double sSquared = s * s;

		if (amount == 0d)
			result = value1;
		else if (amount == 1f)
			result = value2;
		else
			result = (2d * v1 - 2d * v2 + t2 + t1) * sCubed
					+ (3d * v2 - 3d * v1 - 2d * t1 - t2) * sSquared + t1 * s
					+ v1;
		return result;
	}

	public static float lerp(float value1, float value2, float amount) {
		return value1 + (value2 - value1) * amount;
	}

	public static double lerp(double value1, double value2, double amount) {
		return value1 + (value2 - value1) * amount;
	}

	public static float smoothStep(float value1, float value2, float amount) {
		// It is expected that 0 < amount < 1
		// If amount < 0, return value1
		// If amount > 1, return value2
		float result = Utils.clamp(amount, 0f, 1f);
		return Utils.hermite(value1, 0f, value2, 0f, result);
	}

	public static double smoothStep(double value1, double value2, double amount) {
		// It is expected that 0 < amount < 1
		// If amount < 0, return value1
		// If amount > 1, return value2
		double result = Utils.clamp(amount, 0f, 1f);
		return Utils.hermite(value1, 0f, value2, 0f, result);
	}

	public static float toDegrees(float radians) {
		// This method uses double precission internally,
		// though it returns single float
		// Factor = 180 / pi
		return (float) (radians * 57.295779513082320876798154814105);
	}

	public static float toRadians(float degrees) {
		// This method uses double precission internally,
		// though it returns single float
		// Factor = pi / 180
		return (float) (degrees * 0.017453292519943295769236907684886);
	}

	// / <summary>
	// / Compute the MD5 hash for a byte array
	// / </summary>
	// / <param name="data">Byte array to compute the hash for</param>
	// / <returns>MD5 hash of the input data</returns>
	public static byte[] MD5(byte[] data) throws NoSuchAlgorithmException {
		return MessageDigest.getInstance("MD5").digest(data);
	}

	public static String MD5(String data) throws NoSuchAlgorithmException {
		return bytesToHexString(MD5(Utils.stringToBytes(data)), false);
	}

	// /// <summary>
	// /// Compute the SHA1 hash for a byte array
	// /// </summary>
	// /// <param name="data">Byte array to compute the hash for</param>
	// /// <returns>SHA1 hash of the input data</returns>
	// public static byte[] SHA1(byte[] data)
	// {
	// lock (SHA1Builder)
	// return SHA1Builder.ComputeHash(data);
	// }

	// /// <summary>
	// /// Calculate the SHA1 hash of a given String
	// /// </summary>
	// /// <param name="value">The String to hash</param>
	// /// <returns>The SHA1 hash as a String</returns>
	// public static String SHA1String(String value)
	// {
	// StringBuilder digest = new StringBuilder(40);
	// byte[] hash = SHA1(Encoding.UTF8.GetBytes(value));
	//
	// // Convert the hash to a hex String
	// foreach (byte b in hash)
	// digest.appendFormat(Utils.EnUsCulture, "{0:x2}", b);
	//
	// return digest.toString();
	// }

	// /// <summary>
	// /// Compute the SHA256 hash for a byte array
	// /// </summary>
	// /// <param name="data">Byte array to compute the hash for</param>
	// /// <returns>SHA256 hash of the input data</returns>
	// public static byte[] SHA256(byte[] data)
	// {
	// lock (SHA256Builder)
	// return SHA256Builder.ComputeHash(data);
	// }
	//
	// /// <summary>
	// /// Calculate the SHA256 hash of a given String
	// /// </summary>
	// /// <param name="value">The String to hash</param>
	// /// <returns>The SHA256 hash as a String</returns>
	// public static String SHA256String(String value)
	// {
	// StringBuilder digest = new StringBuilder(64);
	// byte[] hash = SHA256(Encoding.UTF8.GetBytes(value));
	//
	// // Convert the hash to a hex String
	// foreach (byte b in hash)
	// digest.appendFormat(Utils.EnUsCulture, "{0:x2}", b);
	//
	// return digest.toString();
	// }
	//
	// /// <summary>
	// /// Calculate the MD5 hash of a given String
	// /// </summary>
	// /// <param name="password">The password to hash</param>
	// /// <returns>An MD5 hash in String format, with $1$ prepended</returns>
	// public static String MD5(String password)
	// {
	// StringBuilder digest = new StringBuilder(32);
	// byte[] hash = MD5(ASCIIEncoding.Default.GetBytes(password));
	//
	// // Convert the hash to a hex String
	// foreach (byte b in hash)
	// digest.appendFormat(Utils.EnUsCulture, "{0:x2}", b);
	//
	// return "$1$" + digest.toString();
	// }
	//
	// /// <summary>
	// /// Calculate the MD5 hash of a given String
	// /// </summary>
	// /// <param name="value">The String to hash</param>
	// /// <returns>The MD5 hash as a String</returns>
	// public static String MD5String(String value)
	// {
	// StringBuilder digest = new StringBuilder(32);
	// byte[] hash = MD5(Encoding.UTF8.GetBytes(value));
	//
	// // Convert the hash to a hex String
	// foreach (byte b in hash)
	// digest.appendFormat(Utils.EnUsCulture, "{0:x2}", b);
	//
	// return digest.toString();
	// }

	// / <summary>
	// / Generate a random double precision floating point value
	// / </summary>
	// / <returns>Random value of type double</returns>
	public static double RandomDouble() {
		return RNG.nextDouble();
	}

	// endregion Math

	// region Platform

	// /// <summary>
	// /// Get the current running platform
	// /// </summary>
	// /// <returns>Enumeration of the current platform we are running
	// on</returns>
	// public static Platform GetRunningPlatform()
	// {
	// final String OSX_CHECK_FILE = "/Library/Extensions.kextcache";
	//
	// if (Environment.OSVersion.Platform == PlatformID.WinCE)
	// {
	// return Platform.WindowsCE;
	// }
	// else
	// {
	// int plat = (int)Environment.OSVersion.Platform;
	//
	// if ((plat != 4) && (plat != 128))
	// {
	// return Platform.Windows;
	// }
	// else
	// {
	// if (System.IO.File.Exists(OSX_CHECK_FILE))
	// return Platform.OSX;
	// else
	// return Platform.Linux;
	// }
	// }
	// }
	//
	// /// <summary>
	// /// Get the current running runtime
	// /// </summary>
	// /// <returns>Enumeration of the current runtime we are running
	// on</returns>
	// public static Runtime GetRunningRuntime()
	// {
	// Type t = Type.GetType("Mono.Runtime");
	// if (t != null)
	// return Runtime.Mono;
	// else
	// return Runtime.Windows;
	// }

	// endregion Platform

	// region String Arrays

	private static final String[] _AssetTypeNames = new String[] { "texture", // 0
			"sound", // 1
			"callcard", // 2
			"landmark", // 3
			"script", // 4
			"clothing", // 5
			"object", // 6
			"notecard", // 7
			"category", // 8
			"root", // 9
			"lsltext", // 10
			"lslbyte", // 11
			"txtr_tga", // 12
			"bodypart", // 13
			"trash", // 14
			"snapshot", // 15
			"lstndfnd", // 16
			"snd_wav", // 17
			"img_tga", // 18
			"jpeg", // 19
			"animatn", // 20
			"gesture", // 21
			"simstate", // 22
			"favorite", // 23
			"link", // 24
			"linkfolder", // 25
			"", // 26
			"", // 27
			"", // 28
			"", // 29
			"", // 30
			"", // 31
			"", // 32
			"", // 33
			"", // 34
			"", // 35
			"", // 36
			"", // 37
			"", // 38
			"", // 39
			"", // 40
			"", // 41
			"", // 42
			"", // 43
			"", // 44
			"", // 45
			"curoutfit", // 46
			"outfit", // 47
			"myoutfits", // 48
			"mesh", // 49
	};

	private static final String[] _InventoryTypeNames = new String[] {
			"texture", // 0
			"sound", // 1
			"callcard", // 2
			"landmark", // 3
			"", // 4
			"", // 5
			"object", // 6
			"notecard", // 7
			"category", // 8
			"root", // 9
			"script", // 10
			"", // 11
			"", // 12
			"", // 13
			"", // 14
			"snapshot", // 15
			"", // 16
			"attach", // 17
			"wearable", // 18
			"animation", // 19
			"gesture", // 20
			"", // 21
			"mesh" // 22
	};

	private static final String[] _SaleTypeNames = new String[] { "not",
			"orig", "copy", "cntn" };

	private static final String[] _AttachmentPointNames = new String[] { "",
			"ATTACH_CHEST", "ATTACH_HEAD", "ATTACH_LSHOULDER",
			"ATTACH_RSHOULDER", "ATTACH_LHAND", "ATTACH_RHAND", "ATTACH_LFOOT",
			"ATTACH_RFOOT", "ATTACH_BACK", "ATTACH_PELVIS", "ATTACH_MOUTH",
			"ATTACH_CHIN", "ATTACH_LEAR", "ATTACH_REAR", "ATTACH_LEYE",
			"ATTACH_REYE", "ATTACH_NOSE", "ATTACH_RUARM", "ATTACH_RLARM",
			"ATTACH_LUARM", "ATTACH_LLARM", "ATTACH_RHIP", "ATTACH_RULEG",
			"ATTACH_RLLEG", "ATTACH_LHIP", "ATTACH_LULEG", "ATTACH_LLLEG",
			"ATTACH_BELLY", "ATTACH_RPEC", "ATTACH_LPEC",
			"ATTACH_HUD_CENTER_2", "ATTACH_HUD_TOP_RIGHT",
			"ATTACH_HUD_TOP_CENTER", "ATTACH_HUD_TOP_LEFT",
			"ATTACH_HUD_CENTER_1", "ATTACH_HUD_BOTTOM_LEFT",
			"ATTACH_HUD_BOTTOM", "ATTACH_HUD_BOTTOM_RIGHT" };

	// endregion String Arrays

	// region BytesTo

	// /// <summary>
	// /// Convert the first two bytes starting in the byte array in
	// /// little endian ordering to a signed short integer
	// /// </summary>
	// /// <param name="bytes">An array two bytes or longer</param>
	// /// <returns>A signed short integer, will be zero if a short can't be
	// /// read at the given position</returns>
	// public static short bytesToInt16Lit(byte[] bytes)
	// {
	// return bytesToInt16Lit(bytes, 0);
	// }
	//
	// /// <summary>
	// /// Convert the first two bytes starting at the given position in
	// /// little endian ordering to a signed short integer
	// /// </summary>
	// /// <param name="bytes">An array two bytes or longer</param>
	// /// <param name="pos">Position in the array to start reading</param>
	// /// <returns>A signed short integer, will be zero if a short can't be
	// /// read at the given position</returns>
	// public static short bytesToInt16Lit(byte[] bytes, int pos)
	// {
	// // if (bytes.length <= pos + 1) return 0;
	// // return (short)(bytes[pos] + (bytes[pos + 1] << 8));
	//
	// return
	// ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN).getShort(pos);
	// }

	// / <summary>
	// / Convert the first two bytes starting in the byte array in
	// / little endian ordering to a signed short integer
	// / </summary>
	// / <param name="bytes">An array two bytes or longer</param>
	// / <returns>A signed short integer, will be zero if a short can't be
	// / read at the given position</returns>
	public static short bytesToInt16(byte[] bytes) {
		return bytesToInt16(bytes, 0);
	}

	// / <summary>
	// / Convert the first two bytes starting at the given position in
	// / little endian ordering to a signed short integer
	// / </summary>
	// / <param name="bytes">An array two bytes or longer</param>
	// / <param name="pos">Position in the array to start reading</param>
	// / <returns>A signed short integer, will be zero if a short can't be
	// / read at the given position</returns>
	public static short bytesToInt16(byte[] bytes, int pos) {
		// if (bytes.length <= pos + 1) return 0;
		// return (short)(bytes[pos] + (bytes[pos + 1] << 8));

		return ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN).getShort(pos);
	}

	// / <summary>
	// / Convert the first four bytes starting at the given position in
	// / little endian ordering to a signed integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <param name="pos">Position to start reading the int from</param>
	// / <returns>A signed integer, will be zero if an int can't be read
	// / at the given position</returns>
	public static int bytesToIntLit(byte[] bytes, int pos) {
		// if (bytes.length < pos + 4) return 0;
		return ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
				.getInt(pos);
	}

	// / <summary>
	// / Convert the first four bytes starting at the given position in
	// / big endian ordering to a signed integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <param name="pos">Position to start reading the int from</param>
	// / <returns>A signed integer, will be zero if an int can't be read
	// / at the given position</returns>
	public static int bytesToInt(byte[] bytes, int pos) {
		// //System.out.println(Utils.bytesToHexString(bytes, "To Int B"));
		// if (bytes.length < pos + 4) return 0;
		// return (int)(bytes[pos + 3] + (bytes[pos + 2] << 8) + (bytes[pos + 1]
		// << 16) + (bytes[pos + 0] << 24));
		return ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN).getInt(pos);
	}

	// / <summary>
	// / Convert the first four bytes of the given array in little endian
	// / ordering to a signed integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>A signed integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static int bytesToIntLit(byte[] bytes) {
		return bytesToIntLit(bytes, 0);
	}

	// / <summary>
	// / Convert the first four bytes of the given array in big endian
	// / ordering to a signed integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>A signed integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static int bytesToInt(byte[] bytes) {
		return bytesToInt(bytes, 0);
	}

	// / <summary>
	// / Convert the first eight bytes of the given array in little endian
	// / ordering to a signed long integer
	// / </summary>
	// / <param name="bytes">An array eight bytes or longer</param>
	// / <returns>A signed long integer, will be zero if the array contains
	// / less than eight bytes</returns>
	public static long bytesToInt64Lit(byte[] bytes) {
		return bytesToInt64Lit(bytes, 0);
	}

	// / <summary>
	// / Convert the first eight bytes starting at the given position in
	// / little endian ordering to a signed long integer
	// / </summary>
	// / <param name="bytes">An array eight bytes or longer</param>
	// / <param name="pos">Position to start reading the long from</param>
	// / <returns>A signed long integer, will be zero if a long can't be read
	// / at the given position</returns>
	public static long bytesToInt64Lit(byte[] bytes, int pos) {
		// if (bytes.length < pos + 8) return 0;
		// return (long)
		// ((long)bytes[pos + 0] +
		// ((long)bytes[pos + 1] << 8) +
		// ((long)bytes[pos + 2] << 16) +
		// ((long)bytes[pos + 3] << 24) +
		// ((long)bytes[pos + 4] << 32) +
		// ((long)bytes[pos + 5] << 40) +
		// ((long)bytes[pos + 6] << 48) +
		// ((long)bytes[pos + 7] << 56));
		return ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
				.getLong(pos);
	}

	// / <summary>
	// / Convert the first eight bytes of the given array in little endian
	// / ordering to a signed long integer
	// / </summary>
	// / <param name="bytes">An array eight bytes or longer</param>
	// / <returns>A signed long integer, will be zero if the array contains
	// / less than eight bytes</returns>
	public static long bytesToInt64(byte[] bytes) {
		return bytesToInt64(bytes, 0);
	}

	// / <summary>
	// / Convert the first eight bytes starting at the given position in
	// / little endian ordering to a signed long integer
	// / </summary>
	// / <param name="bytes">An array eight bytes or longer</param>
	// / <param name="pos">Position to start reading the long from</param>
	// / <returns>A signed long integer, will be zero if a long can't be read
	// / at the given position</returns>
	public static long bytesToInt64(byte[] bytes, int pos) {
		// if (bytes.length < pos + 8) return 0;
		// return (long)
		// ((long)bytes[pos + 7] +
		// ((long)bytes[pos + 6] << 8) +
		// ((long)bytes[pos + 5] << 16) +
		// ((long)bytes[pos + 4] << 24) +
		// ((long)bytes[pos + 3] << 32) +
		// ((long)bytes[pos + 2] << 40) +
		// ((long)bytes[pos + 1] << 48) +
		// ((long)bytes[pos + 0] << 56));
		return ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN).getLong(pos);
	}

	// /// <summary>
	// /// Convert the first two bytes starting at the given position in
	// /// little endian ordering to an unsigned short
	// /// </summary>
	// /// <param name="bytes">Byte array containing the ushort</param>
	// /// <param name="pos">Position to start reading the ushort from</param>
	// /// <returns>An unsigned short, will be zero if a ushort can't be read
	// /// at the given position</returns>
	// public static ushort BytesToUInt16(byte[] bytes, int pos)
	// {
	// if (bytes.length <= pos + 1) return 0;
	// return (ushort)(bytes[pos] + (bytes[pos + 1] << 8));
	// }
	//
	// /// <summary>
	// /// Convert two bytes in little endian ordering to an unsigned short
	// /// </summary>
	// /// <param name="bytes">Byte array containing the ushort</param>
	// /// <returns>An unsigned short, will be zero if a ushort can't be
	// /// read</returns>
	// public static ushort BytesToUInt16(byte[] bytes)
	// {
	// return BytesToUInt16(bytes, 0);
	// }
	//
	// /// <summary>
	// /// Convert the first four bytes starting at the given position in
	// /// little endian ordering to an unsigned integer
	// /// </summary>
	// /// <param name="bytes">Byte array containing the uint</param>
	// /// <param name="pos">Position to start reading the uint from</param>
	// /// <returns>An unsigned integer, will be zero if a uint can't be read
	// /// at the given position</returns>
	public static long bytesToUInt(byte[] bytes, int pos) {
		if (bytes.length < pos + 4)
			return 0;
		return (((long) (bytes[pos + 3] & 0xFF))
				+ (((long) (bytes[pos + 2] & 0xFF)) << 8)
				+ (((long) (bytes[pos + 1] & 0xFF)) << 16) + (((long) (bytes[pos + 0] & 0xFF)) << 24));
	}

	// / <summary>
	// / Convert the first four bytes of the given array in little endian
	// / ordering to an unsigned integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>An unsigned integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static long bytesToUInt(byte[] bytes) {
		return bytesToUInt(bytes, 0);
	}

	// /// <summary>
	// /// Convert the first four bytes starting at the given position in
	// /// little endian ordering to an unsigned integer
	// /// </summary>
	// /// <param name="bytes">Byte array containing the uint</param>
	// /// <param name="pos">Position to start reading the uint from</param>
	// /// <returns>An unsigned integer, will be zero if a uint can't be read
	// /// at the given position</returns>
	public static long bytesToUIntLit(byte[] bytes, int pos) {
		if (bytes.length < pos + 4)
			return 0;
		return (((long) (bytes[pos + 0] & 0xFF))
				+ (((long) (bytes[pos + 1] & 0xFF)) << 8)
				+ (((long) (bytes[pos + 2] & 0xFF)) << 16) + (((long) (bytes[pos + 3] & 0xFF)) << 24));
	}

	// / <summary>
	// / Convert the first four bytes of the given array in little endian
	// / ordering to an unsigned integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>An unsigned integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static long bytesToUIntLit(byte[] bytes) {
		return bytesToUIntLit(bytes, 0);
	}

	public static BigInteger bytesToULongLit(byte[] bytes, int pos) {
		if (bytes.length < pos + 4)
			return new BigInteger("0");
		// return ((long)bytes[pos + 0] + ((long)bytes[pos + 1] << 8) +
		// ((long)bytes[pos + 2] << 16) + ((long)bytes[pos + 3] << 24));
		byte[] bytes2 = ArrayUtils.subarray(bytes, pos, pos + 8);
		reverse(bytes2);
		return new BigInteger(bytes2);
	}

	// / <summary>
	// / Convert the first four bytes of the given array in little endian
	// / ordering to an unsigned integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>An unsigned integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static BigInteger bytesToULongLit(byte[] bytes) {
		return bytesToULong(bytes, 0);
	}

	public static BigInteger bytesToULong(byte[] bytes, int pos) {
		if (bytes.length < pos + 4)
			return new BigInteger("0");
		// return ((long)bytes[pos + 0] + ((long)bytes[pos + 1] << 8) +
		// ((long)bytes[pos + 2] << 16) + ((long)bytes[pos + 3] << 24));
		return new BigInteger(ArrayUtils.subarray(bytes, pos, pos + 8));
	}

	// / <summary>
	// / Convert the first four bytes of the given array in little endian
	// / ordering to an unsigned integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>An unsigned integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static BigInteger bytesToULong(byte[] bytes) {
		return bytesToULong(bytes, 0);
	}

	// /// <summary>
	// /// Convert the first four bytes starting at the given position in
	// /// little endian ordering to an unsigned integer
	// /// </summary>
	// /// <param name="bytes">Byte array containing the uint</param>
	// /// <param name="pos">Position to start reading the uint from</param>
	// /// <returns>An unsigned integer, will be zero if a uint can't be read
	// /// at the given position</returns>
	public static int bytesToUInt16(byte[] bytes, int pos) {
		if (bytes.length < pos + 2)
			return 0;
		return (bytes[pos + 1] & 0xFF) + ((bytes[pos + 0] & 0xFF) << 8);
	}

	// / <summary>
	// / Convert the first four bytes of the given array in little endian
	// / ordering to an unsigned integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>An unsigned integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static int bytesToUInt16(byte[] bytes) {
		return bytesToUInt16(bytes, 0);
	}

	// /// <summary>
	// /// Convert the first four bytes starting at the given position in
	// /// little endian ordering to an unsigned integer
	// /// </summary>
	// /// <param name="bytes">Byte array containing the uint</param>
	// /// <param name="pos">Position to start reading the uint from</param>
	// /// <returns>An unsigned integer, will be zero if a uint can't be read
	// /// at the given position</returns>
	public static int bytesToUInt16Lit(byte[] bytes, int pos) {
		if (bytes.length < pos + 2)
			return 0;
		return (bytes[pos + 0] & 0xFF) + ((bytes[pos + 1] & 0xFF) << 8);
	}

	// / <summary>
	// / Convert the first four bytes of the given array in little endian
	// / ordering to an unsigned integer
	// / </summary>
	// / <param name="bytes">An array four bytes or longer</param>
	// / <returns>An unsigned integer, will be zero if the array contains
	// / less than four bytes</returns>
	public static int bytesToUInt16Lit(byte[] bytes) {
		return bytesToUInt16Lit(bytes, 0);
	}

	//
	// /// <summary>
	// /// Convert the first eight bytes of the given array in little endian
	// /// ordering to an unsigned 64-bit integer
	// /// </summary>
	// /// <param name="bytes">An array eight bytes or longer</param>
	// /// <returns>An unsigned 64-bit integer, will be zero if the array
	// /// contains less than eight bytes</returns>
	// public static ulong BytesToUInt64(byte[] bytes)
	// {
	// if (bytes.length < 8) return 0;
	// return (ulong)
	// ((ulong)bytes[0] +
	// ((ulong)bytes[1] << 8) +
	// ((ulong)bytes[2] << 16) +
	// ((ulong)bytes[3] << 24) +
	// ((ulong)bytes[4] << 32) +
	// ((ulong)bytes[5] << 40) +
	// ((ulong)bytes[6] << 48) +
	// ((ulong)bytes[7] << 56));
	// }

	// / <summary>
	// / Convert four bytes in little endian ordering to a floating point
	// / value
	// / </summary>
	// / <param name="bytes">Byte array containing a little ending floating
	// / point value</param>
	// / <param name="pos">Starting position of the floating point value in
	// / the byte array</param>
	// / <returns>Single precision value</returns>
	public static float bytesToFloatLit(byte[] bytes, int pos) {
		ByteBuffer buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN);
		return buf.getFloat(pos);
	}

	public static float bytesToFloat(byte[] bytes, int pos) {
		ByteBuffer buf = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN);
		return buf.getFloat(pos);
	}

	public static double bytesToDoubleLit(byte[] bytes, int pos) {
		ByteBuffer buf = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN);
		return buf.getDouble(pos);
	}

	public static double bytesToDouble(byte[] bytes, int pos) {
		// if (!BitConverter.IsLittleEndian)
		// {
		// byte[] newBytes = new byte[8];
		// Buffer.BlockCopy(bytes, pos, newBytes, 0, 8);
		// Array.Reverse(newBytes, 0, 8);
		// return BitConverter.ToDouble(newBytes, 0);
		// }
		// else
		// {
		ByteBuffer buf = ByteBuffer.wrap(bytes).order(ByteOrder.BIG_ENDIAN);
		return buf.getDouble(pos);
		// }
	}

	// endregion BytesTo

	// region ToBytes

	public static byte[] int16ToBytesLit(short value) {
		// byte[] bytes = new byte[2];
		// bytes[0] = (byte)(value % 256);
		// bytes[1] = (byte)((value >> 8) % 256);
		// return bytes;
		return ByteBuffer.allocate(2).order(ByteOrder.LITTLE_ENDIAN)
				.putShort(value).array();
	}

	public static void int16ToBytesLit(short value, byte[] dest, int pos) {
		// dest[pos] = (byte)(value % 256);
		// dest[pos + 1] = (byte)((value >> 8) % 256);
		byte[] bytes = int16ToBytesLit(value);
		System.arraycopy(bytes, 0, dest, pos, 2);
	}

	public static byte[] int16ToBytes(short value) {
		// byte[] bytes = new byte[2];
		// bytes[0] = (byte)(value % 256);
		// bytes[1] = (byte)((value >> 8) % 256);
		// return bytes;
		return ByteBuffer.allocate(2).order(ByteOrder.BIG_ENDIAN)
				.putShort(value).array();
	}

	public static void int16ToBytes(short value, byte[] dest, int pos) {
		// dest[pos] = (byte)(value % 256);
		// dest[pos + 1] = (byte)((value >> 8) % 256);
		byte[] bytes = int16ToBytes(value);
		System.arraycopy(bytes, 0, dest, pos, 2);
	}
	
	public static void uint16ToBytesLit(int value, byte[] bytes, int i) {
		int16ToBytesLit((short)value, bytes, i);
	}  

	public static void uint16ToBytes(int value, byte[] bytes, int i) {
		int16ToBytes((short)value, bytes, i);
	}  
	
	// public static byte[] UInt16ToBytes(ushort value)
	// {
	// byte[] bytes = new byte[2];
	// bytes[0] = (byte)(value % 256);
	// bytes[1] = (byte)((value >> 8) % 256);
	// return bytes;
	// }

	// public static void UInt16ToBytes(ushort value, byte[] dest, int pos)
	// {
	// dest[pos] = (byte)(value % 256);
	// dest[pos + 1] = (byte)((value >> 8) % 256);
	// }
	//
	// public static void UInt16ToBytesBig(ushort value, byte[] dest, int pos)
	// {
	// dest[pos] = (byte)((value >> 8) % 256);
	// dest[pos + 1] = (byte)(value % 256);
	// }

	// / <summary>
	// / Convert an integer to a byte array in little endian format
	// / </summary>
	// / <param name="value">The integer to convert</param>
	// / <returns>A four byte little endian array</returns>
	public static byte[] intToBytesLit(int value) {
		// byte[] bytes = new byte[4];

		// bytes[0] = (byte)(value % 256);
		// bytes[1] = (byte)((value >> 8) % 256);
		// bytes[2] = (byte)((value >> 16) % 256);
		// bytes[3] = (byte)((value >> 24) % 256);

		return ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
				.putInt(value).array();
		// return bytes;
	}

	// / <summary>
	// / Convert an integer to a byte array in big endian format
	// / </summary>
	// / <param name="value">The integer to convert</param>
	// / <returns>A four byte big endian array</returns>
	public static byte[] intToBytes(int value) {
		// byte[] bytes = new byte[4];
		//
		// bytes[0] = (byte)((value >> 24) % 256);
		// bytes[1] = (byte)((value >> 16) % 256);
		// bytes[2] = (byte)((value >> 8) % 256);
		// bytes[3] = (byte)(value % 256);

		return ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(value)
				.array();
	}

	public static void intToBytes(int value, byte[] dest, int pos) {
		// dest[pos] = (byte)(value % 256);
		// dest[pos + 1] = (byte)((value >> 8) % 256);
		// dest[pos + 2] = (byte)((value >> 16) % 256);
		// dest[pos + 3] = (byte)((value >> 24) % 256);
		byte[] bytes = intToBytes(value);
		System.arraycopy(bytes, 0, dest, pos, 4);
	}

	public static void intToBytesLit(int value, byte[] dest, int pos) {
		// dest[pos] = (byte)(value % 256);
		// dest[pos + 1] = (byte)((value >> 8) % 256);
		// dest[pos + 2] = (byte)((value >> 16) % 256);
		// dest[pos + 3] = (byte)((value >> 24) % 256);
		byte[] bytes = intToBytesLit(value);
		System.arraycopy(bytes, 0, dest, pos, 4);
	}

	public static byte[] uintToBytes(long value) {
		// byte[] bytes = new byte[4];
		//
		// bytes[0] = (byte)((value >> 24) % 256);
		// bytes[1] = (byte)((value >> 16) % 256);
		// bytes[2] = (byte)((value >> 8) % 256);
		// bytes[3] = (byte)(value % 256);

		return ArrayUtils.subarray(
				ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN)
						.putLong(value).array(), 4, 8);
	}

	public static void uintToBytes(long value, byte[] dest, int pos) {
		// dest[pos] = (byte)(value % 256);
		// dest[pos + 1] = (byte)((value >> 8) % 256);
		// dest[pos + 2] = (byte)((value >> 16) % 256);
		// dest[pos + 3] = (byte)((value >> 24) % 256);

		byte[] bytes = uintToBytes(value);
		System.arraycopy(bytes, 0, dest, pos, 4);
	}

	public static byte[] uintToBytesLit(long value) {
		return ArrayUtils.subarray(
				ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
						.putLong(value).array(), 0, 4);
	}

	public static void uintToBytesLit(long value, byte[] dest, int pos) {

		byte[] bytes = uintToBytesLit(value);
		System.arraycopy(bytes, 0, dest, pos, 4);
	}

	// public static byte[] UIntToBytes(uint value)
	// {
	// byte[] bytes = new byte[4];
	// bytes[0] = (byte)(value % 256);
	// bytes[1] = (byte)((value >> 8) % 256);
	// bytes[2] = (byte)((value >> 16) % 256);
	// bytes[3] = (byte)((value >> 24) % 256);
	// return bytes;
	// }
	//
	// public static void UIntToBytes(uint value, byte[] dest, int pos)
	// {
	// dest[pos] = (byte)(value % 256);
	// dest[pos + 1] = (byte)((value >> 8) % 256);
	// dest[pos + 2] = (byte)((value >> 16) % 256);
	// dest[pos + 3] = (byte)((value >> 24) % 256);
	// }
	//
	// public static void UIntToBytesBig(uint value, byte[] dest, int pos)
	// {
	// dest[pos] = (byte)((value >> 24) % 256);
	// dest[pos + 1] = (byte)((value >> 16) % 256);
	// dest[pos + 2] = (byte)((value >> 8) % 256);
	// dest[pos + 3] = (byte)(value % 256);
	// }

	// / <summary>
	// / Convert a 64-bit integer to a byte array in little endian format
	// / </summary>
	// / <param name="value">The value to convert</param>
	// / <returns>An 8 byte little endian array</returns>
	public static byte[] int64ToBytesLit(long value) {
		return ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
				.putLong(value).array();
	}

	public static void int64ToBytesLit(long value, byte[] dest, int pos) {
		byte[] bytes = int64ToBytesLit(value);
		System.arraycopy(bytes, 0, dest, pos, 8);
	}

	// / <summary>
	// / Convert a 64-bit integer to a byte array in big endian format
	// / </summary>
	// / <param name="value">The value to convert</param>
	// / <returns>An 8 byte little endian array</returns>
	public static byte[] int64ToBytes(long value) {
		return ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN)
				.putLong(value).array();
	}

	public static void int64ToBytes(long value, byte[] dest, int pos) {
		byte[] bytes = int64ToBytes(value);
		System.arraycopy(bytes, 0, dest, pos, 8);
	}

	public static byte[] ulongToBytes(BigInteger value) {
		byte[] bytes = int64ToBytes(value.longValue());
		return bytes;
	}

	public static void ulongToBytes(BigInteger value, byte[] dest, int pos) {
		byte[] bytes = ulongToBytes(value);
		System.arraycopy(bytes, 0, dest, pos, 8);
	}

	public static byte[] ulongToBytesLit(BigInteger value) {
		byte[] bytes = int64ToBytesLit(value.longValue());
		return bytes;
	}

	public static void ulongToBytesLit(BigInteger value, byte[] dest, int pos) {
		byte[] bytes = ulongToBytesLit(value);
		System.arraycopy(bytes, 0, dest, pos, 8);
	}

	// /// <summary>
	// /// Convert a 64-bit unsigned integer to a byte array in little endian
	// /// format
	// /// </summary>
	// /// <param name="value">The value to convert</param>
	// /// <returns>An 8 byte little endian array</returns>
	// public static byte[] UInt64ToBytes(ulong value)
	// {
	// byte[] bytes = BitConverter.GetBytes(value);
	// if (!BitConverter.IsLittleEndian)
	// Array.Reverse(bytes);
	//
	// return bytes;
	// }
	//
	// public static byte[] UInt64ToBytesBig(ulong value)
	// {
	// byte[] bytes = BitConverter.GetBytes(value);
	// if (BitConverter.IsLittleEndian)
	// Array.Reverse(bytes);
	//
	// return bytes;
	// }
	//
	// public static void UInt64ToBytes(ulong value, byte[] dest, int pos)
	// {
	// byte[] bytes = UInt64ToBytes(value);
	// Buffer.BlockCopy(bytes, 0, dest, pos, 8);
	// }
	//
	// public static void UInt64ToBytesBig(ulong value, byte[] dest, int pos)
	// {
	// byte[] bytes = UInt64ToBytesBig(value);
	// Buffer.BlockCopy(bytes, 0, dest, pos, 8);
	// }

	// / <summary>
	// / Convert a floating point value to four bytes in little endian
	// / ordering
	// / </summary>
	// / <param name="value">A floating point value</param>
	// / <returns>A four byte array containing the value in little endian
	// / ordering</returns>
	public static byte[] floatToBytesLit(float value) {
		return ByteBuffer.allocate(4).order(ByteOrder.LITTLE_ENDIAN)
				.putFloat(value).array();
	}

	public static void floatToBytesLit(float value, byte[] dest, int pos) {
		byte[] bytes = floatToBytesLit(value);
		System.arraycopy(bytes, 0, dest, pos, 4);
	}

	public static byte[] floatToBytes(float value) {
		return ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN)
				.putFloat(value).array();
	}

	public static void floatToBytes(float value, byte[] dest, int pos) {
		byte[] bytes = floatToBytes(value);
		System.arraycopy(bytes, 0, dest, pos, 4);
	}

	public static byte[] doubleToBytesLit(double value) {
		return ByteBuffer.allocate(8).order(ByteOrder.LITTLE_ENDIAN)
				.putDouble(value).array();
	}

	public static byte[] doubleToBytes(double value) {
		// byte[] bytes = BitConverter.GetBytes(value);
		// if (BitConverter.IsLittleEndian)
		// Array.Reverse(bytes);
		return ByteBuffer.allocate(8).order(ByteOrder.BIG_ENDIAN)
				.putDouble(value).array();
	}

	public static void doubleToBytesLit(double value, byte[] dest, int pos) {
		byte[] bytes = doubleToBytesLit(value);
		System.arraycopy(bytes, 0, dest, pos, 8);
	}

	public static void doubleToBytes(double value, byte[] dest, int pos) {
		byte[] bytes = doubleToBytes(value);
		System.arraycopy(bytes, 0, dest, pos, 8);
	}

	// endregion ToBytes

	// region Strings
	
	 /// <summary>
	 /// Converts an unsigned integer to a hexadecimal String
	 /// </summary>
	 /// <param name="i">An unsigned integer to convert to a String</param>
	 /// <returns>A hexadecimal String 10 characters long</returns>
	 /// <example>0x7fffffff</example>
	 public static String uintToHexString(long i)
	 {
	 return Utils.bytesToHexString(Utils.uintToBytes(i), false);
	 }

	 /// <summary>
	 /// Converts an long to a hexadecimal String
	 /// </summary>
	 /// <param name="i">An unsigned integer to convert to a String</param>
	 /// <returns>A hexadecimal String 10 characters long</returns>
	 /// <example>0xffffffff7fffffff</example>
	 public static String longToHexString(long i)
	 {
	 return Utils.bytesToHexString(Utils.int64ToBytes(i), false);
	 }
	 
	 
	// / <summary>
	// / Convert a variable length UTF8 byte array to a String
	// / </summary>
	// / <param name="bytes">The UTF8 encoded byte array to convert</param>
	// / <returns>The decoded String</returns>
	public static String bytesToString(byte[] bytes)
			throws UnsupportedEncodingException {
//		if (bytes.length > 0 && bytes[bytes.length - 1] == 0x00)
//			return new String(bytes, 0, bytes.length - 1, "UTF-8");
//		else
//			// return UTF8Encoding.UTF8.GetString(bytes, 0, bytes.length);
			return new String(bytes, 0, bytes.length, "UTF-8");
	}

	public static String bytesToString(byte[] bytes, String encoding)
			throws UnsupportedEncodingException {
			return new String(bytes, 0, bytes.length, encoding);
	}
	
	public static String bytesToString(byte[] bytes, int index, int count) {
		// if (bytes.length > index + count && bytes[index + count - 1] == 0x00)
		// return UTF8Encoding.UTF8.GetString(bytes, index, count - 1);
		// else
		// return UTF8Encoding.UTF8.GetString(bytes, index, count);

		return new String(bytes, index, count);

	}

	public static String bytesToString(byte[] bytes, int index, int count,
			String charsetName) throws UnsupportedEncodingException {
		return new String(bytes, index, count, charsetName);

	}

	// / <summary>
	// / Convert a variable length UTF8 byte array to a String
	// / </summary>
	// / <param name="bytes">The UTF8 encoded byte array to convert</param>
	// / <returns>The decoded String</returns>
	public static String bytesWithTrailingNullByteToString(byte[] bytes)
			throws UnsupportedEncodingException {
//		if (bytes.length > 0 && bytes[bytes.length - 1] == 0x00)
//			return new String(bytes, 0, bytes.length - 1, "UTF-8");
//		else
//			return new String(bytes, 0, bytes.length, "UTF-8");
		return bytesWithTrailingNullByteToString(bytes, 0, bytes.length, "UTF-8");
	}

	public static String bytesWithTrailingNullByteToString(byte[] bytes, int index, int count) throws UnsupportedEncodingException {
		// if (bytes.length > index + count && bytes[index + count - 1] == 0x00)
		// return UTF8Encoding.UTF8.GetString(bytes, index, count - 1);
		// else
		// return UTF8Encoding.UTF8.GetString(bytes, index, count);

//		return new String(bytes, index, count);

		return bytesWithTrailingNullByteToString(bytes, index, count, "UTF-8");
	}

	public static String bytesWithTrailingNullByteToString(byte[] bytes, int index, int count,
			String charsetName) throws UnsupportedEncodingException {
		if(bytes.length ==0)
			return "";
		
		 if (bytes.length >= index + count && bytes[index + count - 1] == 0x00)
		 return new String(bytes, index, count - 1, charsetName);
		 else
		 return new String(bytes, index, count, charsetName);
//		return new String(bytes, index, count, charsetName);
	}
	
	// / <summary>
	// / Converts a byte array to a String containing hexadecimal characters
	// / </summary>
	// / <param name="bytes">The byte array to convert to a String</param>
	// / <param name="fieldName">The name of the field to prepend to each
	// / line of the String</param>
	// / <returns>A String containing hexadecimal characters on multiple
	// / lines. Each line is prepended with the field name</returns>
	public static String bytesToHexDebugString(byte[] bytes, String fieldName) {
		return bytesToHexDebugString(bytes, bytes.length, fieldName);
	}

	// / <summary>
	// / Converts a byte array to a String containing hexadecimal characters
	// / </summary>
	// / <param name="bytes">The byte array to convert to a String</param>
	// / <param name="length">Number of bytes in the array to parse</param>
	// / <param name="fieldName">A String to prepend to each line of the hex
	// / dump</param>
	// / <returns>A String containing hexadecimal characters on multiple
	// / lines. Each line is prepended with the field name</returns>
	public static String bytesToHexDebugString(byte[] bytes, int length,
			String fieldName) {
		StringBuilder output = new StringBuilder();

		for (int i = 0; i < length; i += 16) {
			if (i != 0)
				output.append('\n');

			if (!isNullOrEmpty(fieldName)) {
				output.append(fieldName);
				output.append(": ");
			}

			for (int j = 0; j < 16; j++) {
				if ((i + j) < length) {
					if (j != 0)
						output.append(' ');

					output.append(String.format("%02X", bytes[i + j]));
				}
			}
		}

		return output.toString();
	}

	public static String bytesToHexString(byte[] bytes, boolean uppercase) {
		StringBuilder output = new StringBuilder();

		for (int j = 0; j < bytes.length; j++) {
			// System.out.println(String.format("%02X", bytes[j]));
			output.append(String.format("%02X", bytes[j]));
		}

		return uppercase ? output.toString() : output.toString().toLowerCase();
	}

	// / <summary>
	// / Convert a String to a UTF8 encoded byte array
	// / </summary>
	// / <param name="str">The String to convert</param>
	// / <returns>A null-terminated UTF8 byte array</returns>
	public static byte[] stringToBytes(String str) {
		return stringToBytes(str, "UTF-8");
	}

	public static byte[] stringToBytes(String str, String encoding) {
		if (isNullOrEmpty(str)) {
			return Utils.EmptyBytes;
		}
//		if (!str.endsWith("\0")) { str += "\0"; }

		try {
			return str.getBytes(encoding);
		} catch (UnsupportedEncodingException e) {
			logger.warning(e.getMessage());
		}
		return Utils.EmptyBytes;
	}
	
	
	// / <summary>
	// / Convert a String to a UTF8 encoded byte array
	// / </summary>
	// / <param name="str">The String to convert</param>
	// / <returns>A null-terminated UTF8 byte array</returns>
	public static byte[] stringToBytesWithTrailingNullByte(String str) {
		if (isNullOrEmpty(str)) {
			return Utils.EmptyBytes;
		}
		
		if (!str.endsWith("\0")) { str += "\0"; }

		try {
			return str.getBytes("UTF-8");
		} catch (UnsupportedEncodingException e) {
			logger.warning(e.getMessage());
		}
		return Utils.EmptyBytes;
	}
	
	// / <summary>
	// / Converts a String containing hexadecimal characters to a byte array
	// / </summary>
	// / <param name="hexString">String containing hexadecimal
	// characters</param>
	// / <param name="handleDirty">If true, gracefully handles null, empty and
	// / uneven Strings as well as stripping unconvertable characters</param>
	// / <returns>The converted byte array</returns>
	public static byte[] hexStringToBytes(String hexString, boolean handleDirty) {
		if (handleDirty) {
			if (isNullOrEmpty(hexString))
				return Utils.EmptyBytes;

			StringBuilder stripped = new StringBuilder(hexString.length());
			char c;

			// remove all non A-F, 0-9, characters
			for (int i = 0; i < hexString.length(); i++) {
				c = hexString.charAt(i);
				if (isHexDigit(c))
					stripped.append(c);
			}

			hexString = stripped.toString();

			// if odd number of characters, discard last character
			if (hexString.length() % 2 != 0) {
				hexString = hexString.substring(0, hexString.length() - 1);
			}
		}

		int bytelength = hexString.length() / 2;
		byte[] bytes = new byte[bytelength];
		int j = 0;

		for (int i = 0; i < bytes.length; i++) {
			bytes[i] = hexToByte(hexString.substring(j, j+2));
			j += 2;
		}

		return bytes;
	}

	public static long hexStringToUInt(String value, boolean handleDirty) {
		return Utils.bytesToUInt(hexStringToBytes(value, handleDirty));
	}

	// / <summary>
	// / Returns true is c is a hexadecimal digit (A-F, a-f, 0-9)
	// / </summary>
	// / <param name="c">Character to test</param>
	// / <returns>true if hex digit, false if not</returns>
	private static boolean isHexDigit(char c) {
		final int numA = 65;
		final int num0 = 48;

		int numChar;

		c = Character.toUpperCase(c);
		numChar = (int) c;

		if (numChar >= numA && numChar < (numA + 6))
			return true;
		else if (numChar >= num0 && numChar < (num0 + 10))
			return true;
		else
			return false;
	}

	// / <summary>
	// / Converts 1 or 2 character String into equivalant byte value
	// / </summary>
	// / <param name="hex">1 or 2 character String</param>
	// / <returns>byte</returns>
	private static byte hexToByte(String hex) {
		if (hex.length() > 2 || hex.length() <= 0)
			throw new IllegalArgumentException(
					"hex must be 1 or 2 characters in length");
		// byte newByte = Byte.Parse(hex,
		// System.Globalization.NumberStyles.HexNumber);
		// byte newByte = DatatypeConverter.parseHexBinary(hex)[0];
		Integer i = Integer.valueOf(hex, 16).intValue();
		return i.byteValue();
	}

	// endregion Strings

	// region Packed Values

	// / <summary>
	// / Convert a float value to a byte given a minimum and maximum range
	// / </summary>
	// / <param name="val">Value to convert to a byte</param>
	// / <param name="lower">Minimum value range</param>
	// / <param name="upper">Maximum value range</param>
	// / <returns>A single byte representing the original float value</returns>
	public static byte floatToByte(float val, float lower, float upper) {
		val = Utils.clamp(val, lower, upper);
		// Normalize the value
		val -= lower;
		val /= (upper - lower);

		return (byte) Math.floor(val * (float) Utils.UByteMaxValue);
	}

	// / <summary>
	// / Convert a byte to a float value given a minimum and maximum range
	// / </summary>
	// / <param name="bytes">Byte array to get the byte from</param>
	// / <param name="pos">Position in the byte array the desired byte is
	// at</param>
	// / <param name="lower">Minimum value range</param>
	// / <param name="upper">Maximum value range</param>
	// / <returns>A float value inclusively between lower and upper</returns>
	public static float byteToFloat(byte[] bytes, int pos, float lower,
			float upper) {
		if (bytes.length <= pos)
			return 0;
		return byteToFloat(bytes[pos], lower, upper);
	}

	// / <summary>
	// / Convert a byte to a float value given a minimum and maximum range
	// / </summary>
	// / <param name="val">Byte to convert to a float value</param>
	// / <param name="lower">Minimum value range</param>
	// / <param name="upper">Maximum value range</param>
	// / <returns>A float value inclusively between lower and upper</returns>
	public static float byteToFloat(byte val, float lower, float upper) {
		final float ONE_OVER_BYTEMAX = 1.0f / (float) Utils.UByteMaxValue;

		float fval = (float) Utils.ubyteToInt(val) * ONE_OVER_BYTEMAX;
		float delta = (upper - lower);
		fval *= delta;
		fval += lower;

		// Test for values very close to zero
		float error = delta * ONE_OVER_BYTEMAX;
		if (Math.abs(fval) < error)
			fval = 0.0f;

		return fval;
	}

	public static int ubyteToInt(byte val) {
		return val & 0x00ff;
	}

	public static int uint16ToInt(short val) {
		return val & 0x00ffff;
	}

	public static short ubyteToShort(byte val) {
		return (short)(val & 0x00ff);
	}
	
	public static float UInt16ToFloat(byte[] bytes, int pos, float lower,
			float upper) {
		int val = bytesToUInt16(bytes, pos);
		return uint16ToFloat(val, lower, upper);
	}

	public static float uint16ToFloat(int val, float lower, float upper) {
		final float ONE_OVER_U16_MAX = 1.0f / (float) (2 * Short.MAX_VALUE + 1);

		float fval = (float) val * ONE_OVER_U16_MAX;
		float delta = upper - lower;
		fval *= delta;
		fval += lower;

		// Make sure zeroes come through as zero
		float maxError = delta * ONE_OVER_U16_MAX;
		if (Math.abs(fval) < maxError)
			fval = 0.0f;

		return fval;
	}

	public static int floatToUInt16(float value, float lower, float upper) {
		float delta = upper - lower;
		value -= lower;
		value /= delta;
		value *= (float) (2 * Short.MAX_VALUE + 1);

		return (int) value;
	}

	// endregion Packed Values

	// region TryParse

	// / <summary>
	// / Attempts to parse a floating point value from a String, using an
	// / EN-US number format
	// / </summary>
	// / <param name="s">String to parse</param>
	// / <param name="result">Resulting floating point number</param>
	// / <returns>True if the parse was successful, otherwise false</returns>
	public static boolean tryParseFloat(String s, float[] result) {
		try {
			float f = Float.parseFloat(s);
			result[0] = f;
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	// / <summary>
	// / Attempts to parse a floating point value from a String, using an
	// / EN-US number format
	// / </summary>
	// / <param name="s">String to parse</param>
	// / <param name="result">Resulting floating point number</param>
	// / <returns>True if the parse was successful, otherwise false</returns>
	public static boolean tryParseDouble(String s, double[] result) {
		try {
			double f = Double.parseDouble(s);
			result[0] = f;
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	// / <summary>
	// / Attempts to parse a Integer value from a String, using an
	// / EN-US number format
	// / </summary>
	// / <param name="s">String to parse</param>
	// / <param name="result">Resulting Integer</param>
	// / <returns>True if the parse was successful, otherwise false</returns>
	public static boolean tryParseInt(String s, int[] result) {
		try {
			int f = Integer.parseInt(s);
			result[0] = f;
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	// / <summary>
	// / Attempts to parse a Long value from a String, using an
	// / EN-US number format
	// / </summary>
	// / <param name="s">String to parse</param>
	// / <param name="result">Resulting Long</param>
	// / <returns>True if the parse was successful, otherwise false</returns>
	public static boolean tryParseLong(String s, long[] result) {
		try {
			long f = Long.parseLong(s);
			result[0] = f;
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	// / <summary>
	// / Attempts to parse a floating point value from a String, using an
	// / EN-US number format
	// / </summary>
	// / <param name="s">String to parse</param>
	// / <param name="result">Resulting floating point number</param>
	// / <returns>True if the parse was successful, otherwise false</returns>
	public static boolean tryParseDate(String s, Date[] result) {
		try {
			DateTimeFormatter parser2 = ISODateTimeFormat.dateTimeParser();
			Date f = parser2.parseDateTime(s).toDate();
			result[0] = f;
			return true;
		} catch (IllegalArgumentException e) {
			return false;
		}
	}

	public static boolean tryParseUUID(String s, UUID[] result) {
		try {
			UUID uuid = new UUID(s);
			result[0] = uuid;
			return true;
		} catch (IllegalArgumentException e) {
			return false;
		}
	}

	// / <summary>
	// / Attempts to parse a floating point value from a String, using an
	// / EN-US number format
	// / </summary>
	// / <param name="s">String to parse</param>
	// / <param name="result">Resulting floating point number</param>
	// / <returns>True if the parse was successful, otherwise false</returns>
	public static boolean tryParseUri(String s, URI[] result) {
		try {
			URI f = new URI(s);
			result[0] = f;
			return true;
		} catch (URISyntaxException e0) {
			try {

				String part = s;

				// String schema = null;
				String path = null;
				String fragment = null;
				String query = null;

				// Parse Schema
				String[] parts = part.split("\\#");
				if (parts.length == 2) {
					part = parts[0];
					fragment = parts[1];
				} else
					part = parts[0];

				// Parse Path
				if (part != null) {
					parts = part.split("\\?");
					if (parts.length == 2) {
						path = parts[0];
						query = parts[1];
					} else
						path = parts[0];
				}

				// System.out.println(String.format("%s:%s:%s", path, query,
				// fragment));
				URI f = new URI(null, null, path, query, fragment);
				// System.out.println("Parsed URI:" + f.toString());
				result[0] = f;
				return true;
			} catch (URISyntaxException e) {
				return false;
			}
		}
	}

	public static boolean tryParseUri2(String s, URI[] result) {
		try {
			URI f = new URI(s);
			result[0] = f;
			if (!f.isAbsolute())
				return false;

			return true;
		} catch (URISyntaxException e0) {
			return false;
		}
	}

	// /// <summary>
	// /// Tries to parse an unsigned 32-bit integer from a hexadecimal String
	// /// </summary>
	// /// <param name="s">String to parse</param>
	// /// <param name="result">Resulting integer</param>
	// /// <returns>True if the parse was successful, otherwise false</returns>
	// public static boolean TryParseHex(String s, out uint result)
	// {
	// return UInt32.TryParse(s, System.Globalization.NumberStyles.HexNumber,
	// EnUsCulture.NumberFormat, out result);
	// }

	// //endregion TryParse
	//
	// //region Enum String Conversion
	//
	// /// <summary>
	// /// Returns text specified in EnumInfo attribute of the enumerator
	// /// To add the text use [EnumInfo(Text = "Some nice text here")] before
	// declaration
	// /// of enum values
	// /// </summary>
	// /// <param name="value">Enum value</param>
	// /// <returns>Text representation of the enum</returns>
	// public static String EnumToText(Enum value)
	// {
	// // Get the type
	// Type type = value.GetType();
	//
	// // Get fieldinfo for this type
	// FieldInfo fieldInfo = type.GetField(value.toString());
	//
	// // Find extended attributes, if any
	// EnumInfoAttribute[] attribs =
	// (EnumInfoAttribute[])fieldInfo.GetCustomAttributes(typeof(EnumInfoAttribute),
	// false);
	//
	// return attribs.length() > 0 ? attribs[0].Text : value.toString();
	// }
	//
	// / <summary>
	// / Takes an AssetType and returns the String representation
	// / </summary>
	// / <param name="type">The source <seealso cref="AssetType"/></param>
	// / <returns>The String version of the AssetType</returns>
	public static String AssetTypeToString(AssetType type) {
		return _AssetTypeNames[(int) type.getIndex()];
	}

	// / <summary>
	// / Translate a String name of an AssetType into the proper Type
	// / </summary>
	// / <param name="type">A String containing the AssetType name</param>
	// / <returns>The AssetType which matches the String name, or
	// AssetType.Unknown if no match was found</returns>
	public static AssetType StringToAssetType(String type) {
		for (int i = 0; i < _AssetTypeNames.length; i++) {
			if (_AssetTypeNames[i].equals(type))
				return AssetType.get((byte) i);
		}

		return AssetType.Unknown;
	}

	// / <summary>
	// / Convert an InventoryType to a String
	// / </summary>
	// / <param name="type">The <seealso cref="T:InventoryType"/> to
	// convert</param>
	// / <returns>A String representation of the source</returns>
	public static String InventoryTypeToString(InventoryType type) {
		return _InventoryTypeNames[(int) type.getIndex()];
	}

	// / <summary>
	// / Convert a String into a valid InventoryType
	// / </summary>
	// / <param name="type">A String representation of the InventoryType to
	// convert</param>
	// / <returns>A InventoryType object which matched the type</returns>
	public static InventoryType StringToInventoryType(String type) {
		for (int i = 0; i < _InventoryTypeNames.length; i++) {
			if (_InventoryTypeNames[i] == type)
				return InventoryType.get((byte) i);
		}

		return InventoryType.Unknown;
	}

	// / <summary>
	// / Convert a SaleType to a String
	// / </summary>
	// / <param name="type">The <seealso cref="T:SaleType"/> to convert</param>
	// / <returns>A String representation of the source</returns>
	public static String SaleTypeToString(SaleType type) {
		return _SaleTypeNames[(int) type.getIndex()];
	}

	// / <summary>
	// / Convert a String into a valid SaleType
	// / </summary>
	// / <param name="value">A String representation of the SaleType to
	// convert</param>
	// / <returns>A SaleType object which matched the type</returns>
	public static SaleType StringToSaleType(String value) {
		for (int i = 0; i < _SaleTypeNames.length; i++) {
			if (value.equals(_SaleTypeNames[i]))
				return SaleType.get((byte) i);
		}

		return SaleType.Not;
	}

	// / <summary>
	// / Converts a String used in LLSD to AttachmentPoint type
	// / </summary>
	// / <param name="value">String representation of AttachmentPoint to
	// convert</param>
	// / <returns>AttachmentPoint enum</returns>
	public static AttachmentPoint StringToAttachmentPoint(String value) {
		for (int i = 0; i < _AttachmentPointNames.length; i++) {
			if (value.equals(_AttachmentPointNames[i]))
				return AttachmentPoint.get((byte) i);
		}

		return AttachmentPoint.Default;
	}

	// endregion Enum String Conversion

	// region Miscellaneous

	// / <summary>
	// / Copy a byte array
	// / </summary>
	// / <param name="bytes">Byte array to copy</param>
	// / <returns>A copy of the given byte array</returns>
	public static byte[] CopyBytes(byte[] bytes) {
		if (bytes == null)
			return null;

		byte[] newBytes = new byte[bytes.length];
		Utils.arraycopy(bytes, 0, newBytes, 0, bytes.length);
		return newBytes;
	}

	// / <summary>
	// / Packs to 32-bit unsigned integers in to a 64-bit unsigned integer
	// / </summary>
	// / <param name="a">The left-hand (or X) value</param>
	// / <param name="b">The right-hand (or Y) value</param>
	// / <returns>A 64-bit integer containing the two 32-bit input
	// values</returns>
	public static long intsToLong(int a, int b) {
		return ((long) a << 32) | (long) b;
	}

	public static long uintsToLong(long a, long b) {
		return ((long) a << 32) | (long) b;
	}

	public static BigInteger uintsToULong(long a, long b) {
		return new BigInteger(int64ToBytes(((long) a << 32) | (long) b));
	}

	// / <summary>
	// / Unpacks two 32-bit unsigned integers from a 64-bit unsigned integer
	// / </summary>
	// / <param name="a">The 64-bit input integer</param>
	// / <param name="b">The left-hand (or X) output value</param>
	// / <param name="c">The right-hand (or Y) output value</param>
	public static void longToInts(long a, int[] b) {
		b[0] = (int) (a >> 32);
		b[1] = (int) (a & 0x00000000FFFFFFFFL);
	}

	// / <summary>
	// / Unpacks two 32-bit unsigned integers from a 64-bit unsigned integer
	// / </summary>
	// / <param name="a">The 64-bit input integer</param>
	// / <param name="b">The left-hand (or X) output value</param>
	// / <param name="c">The right-hand (or Y) output value</param>
	public static void longToUInts(long a, long[] b) {
		b[0] = (a >> 32);
		b[1] = (a & 0x00000000FFFFFFFFL);
	}

	//
	// /// <summary>
	// /// Convert an IP address object to an unsigned 32-bit integer
	// /// </summary>
	// /// <param name="address">IP address to convert</param>
	// /// <returns>32-bit unsigned integer holding the IP address
	// bits</returns>
	// public static uint IPToUInt(System.Net.IPAddress address)
	// {
	// byte[] bytes = address.GetAddressBytes();
	// return (uint)((bytes[3] << 24) + (bytes[2] << 16) + (bytes[1] << 8) +
	// bytes[0]);
	// }
	//
	// / <summary>
	// / Gets a unix timestamp for the current time
	// / </summary>
	// / <returns>An unsigned integer representing a unix timestamp for
	// now</returns>
	public static long getUnixTime() {
		return Calendar.getInstance().getTimeInMillis();
	}

	// / <summary>
	// / Convert a UNIX timestamp to a native DateTime object
	// / </summary>
	// / <param name="timestamp">An unsigned integer representing a UNIX
	// / timestamp</param>
	// / <returns>A DateTime object containing the same time specified in
	// / the given timestamp</returns>
	public static Date unixTimeToDate(long timestamp) {
		Calendar calendar = Calendar.getInstance();
		calendar.setTimeInMillis(timestamp * 1000);
		return calendar.getTime();
	}

	// / <summary>
	// / Convert a UNIX timestamp to a native DateTime object
	// / </summary>
	// / <param name="timestamp">A signed integer representing a UNIX
	// / timestamp</param>
	// / <returns>A DateTime object containing the same time specified in
	// / the given timestamp</returns>
	public static Date unixTimeToDate(int timestamp) {
		return unixTimeToDate((long) timestamp);
	}

	// / <summary>
	// / Convert a native DateTime object to a UNIX timestamp
	// / </summary>
	// / <param name="time">A DateTime object you want to convert to a
	// / timestamp</param>
	// / <returns>An unsigned integer representing a UNIX timestamp</returns>
	public static long dateToUnixTime(Date time) {
		return time.getTime();
	}

	// /// <summary>
	// /// Swap two values
	// /// </summary>
	// /// <typeparam name="T">Type of the values to swap</typeparam>
	// /// <param name="lhs">First value</param>
	// /// <param name="rhs">Second value</param>
	// //TODO Following will not work in java
	// public static void swap(Object lhs, Object rhs)
	// {
	// Object temp = lhs;
	// lhs = rhs;
	// rhs = temp;
	// }

	// /// <summary>
	// /// Try to parse an enumeration value from a String
	// /// </summary>
	// /// <typeparam name="T">Enumeration type</typeparam>
	// /// <param name="strType">String value to parse</param>
	// /// <param name="result">Enumeration value on success</param>
	// /// <returns>True if the parsing succeeded, otherwise false</returns>
	// public static boolean EnumTryParse<T>(String strType, out T result)
	// {
	// Type t = typeof(T);
	//
	// if (Enum.IsDefined(t, strType))
	// {
	// result = (T)Enum.Parse(t, strType, true);
	// return true;
	// }
	// else
	// {
	// foreach (String value in Enum.GetNames(typeof(T)))
	// {
	// if (value.Equals(strType, StringComparison.OrdinalIgnoreCase))
	// {
	// result = (T)Enum.Parse(typeof(T), value);
	// return true;
	// }
	// }
	// result = default(T);
	// return false;
	// }
	// }

	// / <summary>
	// / Swaps the high and low words in a byte. Converts aaaabbbb to bbbbaaaa
	// / </summary>
	// / <param name="value">Byte to swap the words in</param>
	// / <returns>Byte value with the words swapped</returns>
	public static byte swapWords(byte value) {
		return (byte) (((value & 0xF0) >> 4) | ((value & 0x0F) << 4));
	}

	// /// <summary>
	// /// Attempts to convert a String representation of a hostname or IP
	// /// address to a <seealso cref="System.Net.IPAddress"/>
	// /// </summary>
	// /// <param name="hostname">Hostname to convert to an IPAddress</param>
	// /// <returns>Converted IP address object, or null if the conversion
	// /// failed</returns>
	// public static IPAddress HostnameToIPv4(String hostname)
	// {
	// // Is it already a valid IP?
	// IPAddress ip;
	// if (IPAddress.TryParse(hostname, out ip))
	// return ip;
	//
	// IPAddress[] hosts = Dns.GetHostEntry(hostname).AddressList;
	//
	// for (int i = 0; i < hosts.length(); i++)
	// {
	// IPAddress host = hosts[i];
	//
	// if (host.AddressFamily == System.Net.Sockets.AddressFamily.InterNetwork)
	// return host;
	// }
	//
	// return null;
	// }

	// endregion Miscellaneous
	
	public static String trimEnd(String str, char[] chars)
	{
		return str.replaceAll("[" + new String(chars) + "]+$", "");
	}

	
	public static String trimStart(String str, char[] chars)
	{
		return str.replaceAll("^[" + new String(chars) + "]+", "");
	}
	
	public static String trim(String str, char[] chars)
	{
		return trimStart(trimEnd(str, chars), chars);
	}
	
	public static boolean isNullOrEmpty(String str) {
		return str == null || str.isEmpty();
	}

	public static int indexOfAny(String str, char[] chars) {
		int j = Integer.MAX_VALUE, k=-1;
		for (int i = 0; i < chars.length; i++) {
			k = str.indexOf(chars[i]);
			if (k >= 0 && k < j)
				j = k;
		}
		return (j == Integer.MAX_VALUE) ? -1 : j;
	}

	public static void arraycopy(byte[] bytes, int srcPos, byte[] dest,
			int destPos, int length) {
		System.arraycopy(bytes, srcPos, dest, destPos, length);
	}

	public static void reverse(byte[] array) {
		ArrayUtils.reverse(array);
	}

	public static String getExceptionStackTraceAsString(Exception exception) {
		StringWriter sw = new StringWriter();
		exception.printStackTrace(new PrintWriter(sw));
		return sw.toString();
	}

	public static long IPToUInt(InetAddress ip) {
		return Utils.bytesToUInt(ip.getAddress());
	}

	public static InetAddress UIntToIP(long addr) throws UnknownHostException {
		return Inet4Address.getByAddress(Utils.uintToBytes(addr));
	}

	public static byte booleanToBytes(boolean set) {
		return (byte) (set ? 0x01 : 0x00);
	}
	
	public static String encodeBase64String(byte[] bytes)
	{
		try {
			return Utils.bytesToString(Base64.encodeBase64(bytes));
		} catch (UnsupportedEncodingException e) {
			JLogger.error(Utils.getExceptionStackTraceAsString(e));
			return "";
		}
	}

	public static byte[] decodeBase64String(String str)
	{
		return Base64.decodeBase64(Utils.stringToBytes(str));
	}
	
	 public static String TrimAt0(String s)
     {
         int pos = s.indexOf("\0");
         if (pos >= 0)
         {
             return s.substring(0, pos);
         }
         else
         {
             return s;
         }
     }
	
	public static List<Integer> deepCopy(List<Integer> src)
	{
		List<Integer> dst = new ArrayList<Integer>();
		for(Integer i: src)
		{
			dst.add(i.intValue());
		}
		return dst;
	}
	 
	public static short UByteMaxValue = 0x0ff;


	
	
}
