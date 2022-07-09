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

import java.io.Serializable;

import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class Color4 implements Comparable<Color4>, Serializable
{
	/**
	 * 
	 */
	private static final long serialVersionUID = 2458051273363647723L;
	/// <summary>Red</summary>
	private float R;
	/// <summary>Green</summary>
	private float G;
	/// <summary>Blue</summary>
	private float B;
	/// <summary>Alpha</summary>
	private float A;

	//region Constructors

	public Color4()
	{
		this(0f, 0f, 0f, 0f);	
	}

	public float getR() {
		return R;
	}

	public void setR(float r) {
		R = r;
		normalizeColor();
	}

	public float getG() {
		return G;
	}

	public void setG(float g) {
		G = g;
		normalizeColor();
	}

	public float getB() {
		return B;
	}

	public void setB(float b) {
		B = b;
		normalizeColor();
	}

	public float getA() {
		return A;
	}

	public void setA(float a) {
		A = a;
		normalizeColor();
	}

	/*
	 * Removed following constructor, as floating point constructor will always take precedence	
	 */
	/// <summary>
	/// 
	/// </summary>
	/// <param name="r"></param>
	/// <param name="g"></param>
	/// <param name="b"></param>
	/// <param name="a"></param>

	
	
	//FIXME need to handle colors in the range of 1 - 256. Should we clamp 
	// the values or normalize them
	private void normalizeColor()
	{
//		if (Utils.round(R) > 1f || Utils.round(G) > 1f || Utils.round(B) > 1f || Utils.round(A) > 1f)
//		{
//			final float quanta = (255);
//		 
//			R = (float)R / quanta;
//			G = (float)G / quanta;
//			B = (float)B / quanta;
//			A = (float)A / quanta;
//		}
//		else
//		{
//			R = Utils.clamp(R, 0, 1);
//			G = Utils.clamp(G, 0, 1);
//			B = Utils.clamp(B, 0, 1);
//			A = Utils.clamp(A, 0, 1);
//		}

		if (R > 1.5f || G > 1.5f || B > 1.5f || A > 1.5f)
		{
			JLogger.debug("Attempting to initialize Color4 with out of range values <" + R + "," + G + "," + B + "," + A +">");
		}
		
		if (R > 1f || G > 1f || B > 1f || A > 1f)
		{
			float max = (float)Math.max(A, Math.max(G, Math.max(R, B)));
			if(max > 0)
			{
				R = R / max;
				G = G / max;
				B = B / max;
				A = A / max;
			}
		}
	}
	
	private void checkSantity()
	{
		//TODO this method can be removed once the testing has been performed..
		if (Utils.round(R) > 1f || Utils.round(G) > 1f || Utils.round(B) > 1f || Utils.round(A) > 1f)
			throw new IllegalArgumentException(
					"Attempting to initialize Color4 with out of range values <" + R + "," + G + "," + B + "," + A +">");
	}

	public Color4(int r, int g, int b, int a)
	{
		// Quick check to see if someone is doing something obviously wrong
		// like using float values from 0.0 - 255.0
		R = (float)r/255;
		G = (float)g/255;
		B = (float)b/255;
		A = (float)a/255;
		normalizeColor();
	}
	
	
	public Color4(float r, float g, float b, float a)
	{
		// Quick check to see if someone is doing something obviously wrong
		// like using float values from 0.0 - 255.0
		R = r;
		G = g;
		B = b;
		A = a;
		normalizeColor();
	}

	public Color4(double r, double g, double b, double a)
	{
		this((float)r, (float)g, (float)b, (float)a); 
	}
	/// <summary>
	/// Builds a color from a byte array (Little Endian)
	/// </summary>
	/// <param name="byteArray">Byte array containing a 16 byte color</param>
	/// <param name="pos">Beginning position in the byte array</param>
	/// <param name="inverted">True if the byte array stores inverted values,
	/// otherwise false. For example the color black (fully opaque) inverted
	/// would be 0xFF 0xFF 0xFF 0x00</param>
	public Color4(byte[] byteArray, int pos, boolean inverted)
	{
		R = G = B = A = 0f;
		fromBytes(byteArray, pos, inverted);

		normalizeColor();
	}

	/// <summary>
	/// Returns the raw bytes for this vector (Liitle Endian)
	/// </summary>
	/// <param name="byteArray">Byte array containing a 16 byte color</param>
	/// <param name="pos">Beginning position in the byte array</param>
	/// <param name="inverted">True if the byte array stores inverted values,
	/// otherwise false. For example the color black (fully opaque) inverted
	/// would be 0xFF 0xFF 0xFF 0x00</param>
	/// <param name="alphaInverted">True if the alpha value is inverted in
	/// addition to whatever the inverted parameter is. Setting inverted true
	/// and alphaInverted true will flip the alpha value back to non-inverted,
	/// but keep the other color bytes inverted</param>
	/// <returns>A 16 byte array containing R, G, B, and A</returns>
	public Color4(byte[] byteArray, int pos, boolean inverted, boolean alphaInverted)
	{
		R = G = B = A = 0f;
		fromBytes(byteArray, pos, inverted, alphaInverted);
		normalizeColor();
		checkSantity();
	}

	/// <summary>
	/// Copy finalructor
	/// </summary>
	/// <param name="color">Color to copy</param>
	public Color4(Color4 color)
	{
		R = color.R;
		G = color.G;
		B = color.B;
		A = color.A;
		normalizeColor();
		checkSantity();
	}

	//endregion Constructors

	//region Public Methods

	/// <summary>
	/// IComparable.compareTo implementation
	/// </summary>
	/// <remarks>Sorting ends up like this: |--Grayscale--||--Color--|.
	/// Alpha is only used when the colors are otherwise equivalent</remarks>
	public int compareTo(Color4 color)
	{
		float thisHue = getHue();
		float thatHue = color.getHue();

		if (thisHue < 0f && thatHue < 0f)
		{
			// Both monochromatic
			if (R == color.R)
			{
				// Monochromatic and equal, compare alpha
				return Float.compare(A, color.A);
			}
			else
			{
				// Compare lightness
				return Float.compare(R, color.R);
			}
		}
		else
		{
			if (thisHue == thatHue)
			{
				// RGB is equal, compare alpha
				return Float.compare(A, color.A);
			}
			else
			{
				// Compare hues
				return Float.compare(thisHue, thatHue);
			}
		}
	}

	public void fromBytes(byte[] byteArray, int pos, boolean inverted)
	{
		final float quanta = 1.0f / 255.0f;

		if (inverted)
		{
			R = (float)(255 - byteArray[pos]) * quanta;
			G = (float)(255 - byteArray[pos + 1]) * quanta;
			B = (float)(255 - byteArray[pos + 2]) * quanta;
			A = (float)(255 - byteArray[pos + 3]) * quanta;
		}
		else
		{
			R = (float)byteArray[pos] * quanta;
			G = (float)byteArray[pos + 1] * quanta;
			B = (float)byteArray[pos + 2] * quanta;
			A = (float)byteArray[pos + 3] * quanta;
		}

		normalizeColor();
		checkSantity();
	}

	/// <summary>
	/// Builds a color from a byte array
	/// </summary>
	/// <param name="byteArray">Byte array containing a 16 byte color</param>
	/// <param name="pos">Beginning position in the byte array</param>
	/// <param name="inverted">True if the byte array stores inverted values,
	/// otherwise false. For example the color black (fully opaque) inverted
	/// would be 0xFF 0xFF 0xFF 0x00</param>
	/// <param name="alphaInverted">True if the alpha value is inverted in
	/// addition to whatever the inverted parameter is. Setting inverted true
	/// and alphaInverted true will flip the alpha value back to non-inverted,
	/// but keep the other color bytes inverted</param>
	public void fromBytes(byte[] byteArray, int pos, boolean inverted, boolean alphaInverted)
	{
		fromBytes(byteArray, pos, inverted);

		if (alphaInverted)
			A = 1.0f - A;
	}

	public byte[] getBytes()
	{
		return getBytes(false);
	}

	public byte[] getBytes(boolean inverted)
	{
		byte[] byteArray = new byte[4];
		toBytes(byteArray, 0, inverted);
		return byteArray;
	}

	public byte[] getFloatBytes()
	{
		byte[] bytes = new byte[16];
		toFloatBytes(bytes, 0);
		return bytes;
	}

	/// <summary>
	/// Writes the raw bytes for this color to a byte array
	/// </summary>
	/// <param name="dest">Destination byte array</param>
	/// <param name="pos">Position in the destination array to start
	/// writing. Must be at least 16 bytes before the end of the array</param>
	public void toBytes(byte[] dest, int pos)
	{
		toBytes(dest, pos, false);
	}

	/// <summary>
	/// Serializes this color into four bytes in a byte array
	/// </summary>
	/// <param name="dest">Destination byte array</param>
	/// <param name="pos">Position in the destination array to start
	/// writing. Must be at least 4 bytes before the end of the array</param>
	/// <param name="inverted">True to invert the output (1.0 becomes 0
	/// instead of 255)</param>
	public void toBytes(byte[] dest, int pos, boolean inverted)
	{
		dest[pos + 0] = Utils.floatToByte(R, 0f, 1f);
		dest[pos + 1] = Utils.floatToByte(G, 0f, 1f);
		dest[pos + 2] = Utils.floatToByte(B, 0f, 1f);
		dest[pos + 3] = Utils.floatToByte(A, 0f, 1f);

		if (inverted)
		{
			dest[pos + 0] = (byte)(255 - Utils.ubyteToInt(dest[pos + 0]));
			dest[pos + 1] = (byte)(255 - Utils.ubyteToInt(dest[pos + 1]));
			dest[pos + 2] = (byte)(255 - Utils.ubyteToInt(dest[pos + 2]));
			dest[pos + 3] = (byte)(255 - Utils.ubyteToInt(dest[pos + 3]));
		}
	}

	/// <summary>
	/// Writes the raw bytes for this color to a byte array
	/// </summary>
	/// <param name="dest">Destination byte array</param>
	/// <param name="pos">Position in the destination array to start
	/// writing. Must be at least 16 bytes before the end of the array</param>
	public void toFloatBytes(byte[] dest, int pos)
	{   
		System.arraycopy(Utils.floatToBytes(R), 0, dest, pos, 4);
		System.arraycopy(Utils.floatToBytes(G), 0, dest, pos+4, 4);
		System.arraycopy(Utils.floatToBytes(B), 0, dest, pos+8, 4);
		System.arraycopy(Utils.floatToBytes(A), 0, dest, pos+12, 4);
	}

	public float getHue()
	{
		final float HUE_MAX = 360f;

		float max = Math.max(Math.max(R, G), B);
		float min = Math.min(Math.min(R, B), B);

		if (max == min)
		{
			// Achromatic, hue is undefined
			return -1f;
		}
		else if (R == max)
		{
			float bDelta = (((max - B) * (HUE_MAX / 6f)) + ((max - min) / 2f)) / (max - min);
			float gDelta = (((max - G) * (HUE_MAX / 6f)) + ((max - min) / 2f)) / (max - min);
			return bDelta - gDelta;
		}
		else if (G == max)
		{
			float rDelta = (((max - R) * (HUE_MAX / 6f)) + ((max - min) / 2f)) / (max - min);
			float bDelta = (((max - B) * (HUE_MAX / 6f)) + ((max - min) / 2f)) / (max - min);
			return (HUE_MAX / 3f) + rDelta - bDelta;
		}
		else // B == max
		{
			float gDelta = (((max - G) * (HUE_MAX / 6f)) + ((max - min) / 2f)) / (max - min);
			float rDelta = (((max - R) * (HUE_MAX / 6f)) + ((max - min) / 2f)) / (max - min);
			return ((2f * HUE_MAX) / 3f) + gDelta - rDelta;
		}
	}

	/// <summary>
	/// Ensures that values are in range 0-1
	/// </summary>
	public void clampValues()
	{
		if (R < 0f)
			R = 0f;
		if (G < 0f)
			G = 0f;
		if (B < 0f)
			B = 0f;
		if (A < 0f)
			A = 0f;
		if (R > 1f)
			R = 1f;
		if (G > 1f)
			G = 1f;
		if (B > 1f)
			B = 1f;
		if (A > 1f)
			A = 1f;
	}

	//endregion Public Methods

	//region Static Methods

	/// <summary>
	/// Create an RGB color from a hue, saturation, value combination
	/// </summary>
	/// <param name="hue">Hue</param>
	/// <param name="saturation">Saturation</param>
	/// <param name="value">Value</param>
	/// <returns>An fully opaque RGB color (alpha is 1.0)</returns>
	public static Color4 fromHSV(double hue, double saturation, double value)
	{
		double r = 0d;
		double g = 0d;
		double b = 0d;

		if (saturation == 0d)
		{
			// If s is 0, all colors are the same.
			// This is some flavor of gray.
			r = value;
			g = value;
			b = value;
		}
		else
		{
			double p;
			double q;
			double t;

			double fractionalSector;
			int sectorNumber;
			double sectorPos;

			// The color wheel consists of 6 sectors.
			// Figure out which sector you//re in.
			sectorPos = hue / 60d;
			sectorNumber = (int)(Math.floor(sectorPos));

			// get the fractional part of the sector.
			// That is, how many degrees into the sector
			// are you?
			fractionalSector = sectorPos - sectorNumber;

			// Calculate values for the three axes
			// of the color. 
			p = value * (1d - saturation);
			q = value * (1d - (saturation * fractionalSector));
			t = value * (1d - (saturation * (1d - fractionalSector)));

			// Assign the fractional colors to r, g, and b
			// based on the sector the angle is in.
			switch (sectorNumber)
			{
			case 0:
				r = value;
				g = t;
				b = p;
				break;
			case 1:
				r = q;
				g = value;
				b = p;
				break;
			case 2:
				r = p;
				g = value;
				b = t;
				break;
			case 3:
				r = p;
				g = q;
				b = value;
				break;
			case 4:
				r = t;
				g = p;
				b = value;
				break;
			case 5:
				r = value;
				g = p;
				b = q;
				break;
			}
		}

		return new Color4((float)r, (float)g, (float)b, 1f);
	}

	/// <summary>
	/// Performs linear interpolation between two colors
	/// </summary>
	/// <param name="value1">Color to start at</param>
	/// <param name="value2">Color to end at</param>
	/// <param name="amount">Amount to interpolate</param>
	/// <returns>The interpolated color</returns>
	public static Color4 lerp(Color4 value1, Color4 value2, float amount)
	{
		return new Color4(
				Utils.lerp(value1.R, value2.R, amount),
				Utils.lerp(value1.G, value2.G, amount),
				Utils.lerp(value1.B, value2.B, amount),
				Utils.lerp(value1.A, value2.A, amount));
	}

	//endregion Static Methods

	//region Overrides

	public String toString()
	{
		return "<"+ R + ", " + G + ", " + B + ", " + A + ">";
	}

	public String toRGBString()
	{
		return ""+ R + ", " + G + ", " + B + ", " + A;        
	}

	public boolean equals(Object obj)
	{
		return (obj instanceof Color4) ? equals((Color4)obj) : false;
	}

	public boolean equals(Color4 other)
	{
		return equals(this, other);
	}

	public int hashCode()
	{
		return (new Float(R)).hashCode() ^ (new Float(G)).hashCode() ^ (new Float(B)).hashCode() ^ (new Float(A)).hashCode();
	}

	//endregion Overrides

	//region Operators

	public static boolean equals(Color4 lhs, Color4 rhs)
	{
		return (lhs.R == rhs.R) && (lhs.G == rhs.G) && (lhs.B == rhs.B) && (lhs.A == rhs.A);
	}

	public static boolean notEquals(Color4 lhs, Color4 rhs)
	{
		return !(equals(lhs, rhs));
	}

	public static Color4 add(Color4 lhs, Color4 rhs)
	{
		Color4 result = new Color4();
		result.R = lhs.R + rhs.R;
		result.G = lhs.G + rhs.G;
		result.B = lhs.B + rhs.B;
		result.A = lhs.A + rhs.A;
		result.clampValues();

		return result;
	}

	public static Color4 subtract(Color4 lhs, Color4 rhs)
	{
		Color4 result = new Color4();
		result.R = lhs.R - rhs.R;
		result.G = lhs.G - rhs.G;
		result.B = lhs.B - rhs.B;
		result.A = lhs.A - rhs.A;
		result.clampValues();

		return result;
	}

	public static Color4 multiply(Color4 lhs, Color4 rhs)
	{
		Color4 result = new Color4();
		result.R = lhs.R * rhs.R;
		result.G = lhs.G * rhs.G;
		result.B = lhs.B * rhs.B;
		result.A = lhs.A * rhs.A;
		result.clampValues();

		return result;
	}

	//endregion Operators

	/// <summary>A Color4 with zero RGB values and fully opaque (alpha 1.0)</summary>
	public final static Color4 Black = new Color4(0f, 0f, 0f, 1f);

	/// <summary>A Color4 with full RGB values (1.0) and fully opaque (alpha 1.0)</summary>
	public final static Color4 White = new Color4(1f, 1f, 1f, 1f);
}
