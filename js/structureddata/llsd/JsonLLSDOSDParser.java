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
package com.ngt.jopenmetaverse.shared.structureddata.llsd;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;
import com.google.gson.JsonPrimitive;
import com.google.gson.stream.JsonWriter;
import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDUUID;
import com.ngt.jopenmetaverse.shared.structureddata.OSDUri;
import com.ngt.jopenmetaverse.shared.structureddata.OSDDate;
import com.ngt.jopenmetaverse.shared.structureddata.OSDException;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;

/*
 * Reference http://tools.ietf.org/html/draft-hamrick-llsd-00#section-3.2
 */
/// </summary>
public class JsonLLSDOSDParser
{
	//
	/// <summary>
	/// 
	/// </summary>
	/// <param name="xmlData"></param>
	/// <returns></returns>
	public static OSD DeserializeLLSDJson(byte[] xmlData)
	{
		return DeserializeLLSDJson(new ByteArrayInputStream(xmlData));
	}

	//        public static OSD DeserializeLLSDJson(IStream xmlStream)
	//        {
	//            return DeserializeLLSDJson(new XmlTextReader(xmlStream));
	//        }

	/// <summary>
	/// 
	/// </summary>
	/// <param name="xmlData"></param>
	/// <returns></returns>
	public static OSD DeserializeLLSDJson(String xmlData)
	{
		byte[] bytes = Utils.stringToBytes(xmlData);
		return DeserializeLLSDJson(bytes);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="xmlData"></param>
	/// <returns></returns>
	public static OSD DeserializeLLSDJson(InputStream jsonStream)
	{
		try
		{
			JsonParser parser = new JsonParser();
			BufferedReader in
			= new BufferedReader(new InputStreamReader(jsonStream));

			JsonElement obj = parser.parse(in);

			OSD ret = ParseLLSDJsonRoot(obj);

			return ret;
		}
		catch(Exception e)
		{
			e.printStackTrace();
			return new OSD();
		}
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="data"></param>
	/// <returns></returns>
	public static byte[] SerializeLLSDJsonBytes(OSD data) throws Exception
	{
		return Utils.stringToBytes(SerializeLLSDJsonString(data));
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="data"></param>
	/// <returns></returns>
	public static String SerializeLLSDJsonString(OSD data) throws Exception
	{

		StringWriter swriter = new StringWriter();
		JsonWriter jwriter = new JsonWriter(swriter);
	     jwriter.setIndent(" ");
		jwriter.beginArray();
		
		SerializeLLSDJsonElement(jwriter, data);
		
		jwriter.endArray();
		
		return swriter.getBuffer().toString();		
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="writer"></param>
	/// <param name="data"></param>
	public static void SerializeLLSDJsonElement(JsonWriter doc, OSD data) throws IOException
	{
		switch (data.getType())
		{
		case Unknown:
			//	                    writer.WriteStartElement("", "undef", "");
			//	                    writer.WriteEndElement();
			doc.nullValue();
			break;
		case Boolean:
			//	                    writer.WriteStartElement("", "boolean", "");
			//	                    writer.WriteString(data.asString());
			//	                    writer.WriteEndElement();

			doc.value(data.asBoolean());
			break;
		case Integer:
			//	                    writer.WriteStartElement("", "integer", "");
			//	                    writer.WriteString(data.asString());
			//	                    writer.WriteEndElement();

			doc.value(data.asInteger());

			break;
		case Real:
			//	                    writer.WriteStartElement("", "real", "");
			//	                    writer.WriteString(data.asString());
			//	                    writer.WriteEndElement();

			doc.value(data.asReal());			
			break;
		case String:
			//	                    writer.WriteStartElement("", "string", "");
			//	                    writer.WriteString(data.asString());
			//	                    writer.WriteEndElement();

			doc.value(data.asString());
			break;
		case UUID:
			//	                    writer.WriteStartElement("", "uuid", "");
			//	                    writer.WriteString(data.asString());
			//	                    writer.WriteEndElement();

			doc.value(((OSDUUID)data).asString());			
			break;
		case Date:
			//	                    writer.WriteStartElement("", "date", "");
			//	                    writer.WriteString(data.asString());
			//	                    writer.WriteEndElement();

			doc.value(((OSDDate)data).asString());			
			
			break;
		case URI:
			//	                    writer.WriteStartElement("", "uri", "");
			//	                    writer.WriteString(data.asString());
			//	                    writer.WriteEndElement();
			
			doc.value(((OSDUri)data).asString());			
			

			break;
		case Binary:

			doc.beginArray();
			byte[] bytes = data.asBinary();
			for(int i = 0; i < bytes.length; i++)
			{
				doc.value(Utils.ubyteToInt(bytes[i]));
			}
			
			doc.endArray();
			
			break;
		case Map:
			OSDMap map = (OSDMap)data;

			doc.beginObject();
			for(Map.Entry<String, OSD> entry: map.entrySet())
			{
				JsonWriter tempWriter = doc.name(entry.getKey());
				
				SerializeLLSDJsonElement(tempWriter, entry.getValue());				
			}			
			doc.endObject();
			
			break;
		case Array:
			OSDArray array = (OSDArray)data;
			
			doc.beginArray();
			for (int i = 0; i < array.count(); i++)
			{
				SerializeLLSDJsonElement(doc, array.get(i));
			}
			
			doc.endArray();
			
			break;
		}
	}

	//        /// <summary>
	//        /// 
	//        /// </summary>
	//        /// <param name="xmlData"></param>
	//        /// <param name="error"></param>
	//        /// <returns></returns>
	//        public static boolean TryValidateLLSDXml(XmlTextReader xmlData, out String error)
	//        {
	//            lock (XmlValidationLock)
	//            {
	//                LastXmlErrors = "";
	//                XmlTextReader = xmlData;
	//
	//                CreateLLSDXmlSchema();
	//
	//                XmlReaderSettings readerSettings = new XmlReaderSettings();
	//                readerSettings.ValidationType = ValidationType.Schema;
	//                readerSettings.Schemas.Add(XmlSchema);
	//                readerSettings.ValidationEventHandler += new ValidationEventHandler(LLSDXmlSchemaValidationHandler);
	//
	//                XmlReader reader = XmlReader.Create(xmlData, readerSettings);
	//
	//                try
	//                {
	//                    while (reader.Read()) { }
	//                }
	//                catch (XmlException)
	//                {
	//                    error = LastXmlErrors;
	//                    return false;
	//                }
	//
	//                if (LastXmlErrors == "")
	//                {
	//                    error = null;
	//                    return true;
	//                }
	//                else
	//                {
	//                    error = LastXmlErrors;
	//                    return false;
	//                }
	//            }
	//        }

	/// <summary>
	/// 
	/// </summary>
	/// <param name="reader"></param>
	/// <returns></returns>

	private static OSD ParseLLSDJsonRoot(JsonElement obj) throws OSDException, URISyntaxException
	{		
		OSD ret = new OSD();

		if(!obj.isJsonArray())
			throw new OSDException("Json Top Element must be array");

		JsonArray rootArray = obj.getAsJsonArray();
		Iterator<JsonElement> iterator = rootArray.iterator();
		while(iterator.hasNext())
		{
			ret = ParseLLSDJsonElement(iterator.next());
			break;
		}

		return ret;
	}


	private static OSD ParseLLSDJsonElement(JsonElement reader) throws OSDException, URISyntaxException
	{
		OSD ret = new OSD();
		if(reader.isJsonNull())
		{
			//System.out.println("Got Json Null...");
			return new OSD();
		}
		else if(reader.isJsonPrimitive())
		{
			//System.out.println("Got Json Primitive...");
			JsonPrimitive primitive = reader.getAsJsonPrimitive();
			OSD[] result = new OSD[1];
			if(primitive.isBoolean())
			{
				//System.out.println("Got Json Primitive Boolean...");
				ret = OSD.FromBoolean(primitive.getAsBoolean());
			}
			else if(stringToNumber(primitive.getAsString(), result))
			{
				return result[0];
			}
			else if(primitive.isString())
			{
				UUID[] resultUUID = new UUID[1];
				Date[] resultDate = new Date[1];
				URI[] resultURI = new URI[1];
//				double[] resultFloat = new double[1];
				
				String s = primitive.getAsString();
				//System.out.println("Got Json Primitive String...");
				//TODO Json do not permit NaN and Infinity numbers verify
//				if(Utils.tryParseDouble(s, resultFloat))
//				{
//					System.out.println("Got Json Primitive Float...");
//					ret =  OSD.FromReal(resultFloat[0]);				
//				}
//				else 
					if(!s.equals("") &&  Utils.tryParseUUID(s, resultUUID))
				{
					//System.out.println("Got Json Primitive UUID...");
					ret =  OSD.FromUUID(resultUUID[0]);
				}
				else if(Utils.tryParseDate(s, resultDate))
				{
					//System.out.println("Got Json Primitive Date...");
					ret = OSD.FromDate(resultDate[0]);
				}
					/*
					 * TODO Issue Following will only parse URI that are absolute URI
					 */
				else if(Utils.tryParseUri2(s, resultURI))
				{
					//System.out.println("Got Json Primitive URI...");
					ret = OSD.FromUri(resultURI[0]);
				}
				else
				{
					ret = OSD.FromString(s);					
				}
			}
		}
		else if(reader.isJsonArray())
		{
			//System.out.println("Got Json Array...");
			JsonArray jarray = reader.getAsJsonArray();
			byte[][] result = new byte[1][];
			/*TODO Issue how to represent array of bytes in json, 
			 * as it will mapped to binary
			 */
			if(tryParseBinary(jarray, result))
			{
				//System.out.println("Got Json Binary...");
				return OSD.FromBinary(result[0]);
			}
			return ParseLLSDJsonArray(jarray);
		}
		else if(reader.isJsonObject())
		{
			//System.out.println("Got Json Map...");
			JsonObject jobject = reader.getAsJsonObject(); 
			return ParseLLSDJsonMap(jobject);
		}
		return ret;
	}

	private static OSDMap ParseLLSDJsonMap(JsonObject jobject) throws OSDException, URISyntaxException
	{

		//System.out.println("Parsing Map...");

		OSDMap map = new OSDMap();
		for(Entry<String, JsonElement> entry :jobject.entrySet())
		{
			//System.out.println("Parsing Map Key..." + entry.getKey());
			map.put(entry.getKey(), ParseLLSDJsonElement(entry.getValue()));
		}

		return map;
	}

	private static OSDArray ParseLLSDJsonArray(JsonArray jarray) throws OSDException, URISyntaxException
	{
		//System.out.println("Parsing Array...");

		OSDArray array = new OSDArray();

		for(int i = 0; i < jarray.size(); i++)
		{
			array.add(ParseLLSDJsonElement(jarray.get(i)));
		}
		return array;
	}        


	private static boolean tryParseBinary(JsonArray jarray, byte[][] result)
	{
		result[0] = new byte[jarray.size()];
		for(int i=0; i < jarray.size(); i++)
		{
			JsonElement jele = jarray.get(i);
			int[] iarray = new int[1];
			if(!tryParseInteger(jele, iarray) 
					|| ((iarray[0] & 0xff) != iarray[0]))
			{
				return false;
			}
			result[0][i] = (byte)iarray[0]; 
		}
		return true;
	}
	
	private static boolean stringToNumber(String value, OSD[] result) {
	    try {
	      long longValue = Long.parseLong(value);
	      if (longValue >= Integer.MIN_VALUE && longValue <= Integer.MAX_VALUE) {
	    	  result[0] = OSD.FromInteger((int)longValue);
	    	  return true;
	      }
	      result[0] = OSD.FromLong(longValue);
	      return true;
	    } catch (NumberFormatException ignored) {
	    }

	    try {
	    	result[0] = OSD.FromReal(new Double(value));
	    	return true;
	    } catch (NumberFormatException ignored) {
	      return false;
	    }
	  }
	
	private static boolean tryParseInteger(JsonElement value, int[] result) {
		if(value.isJsonPrimitive() && value.getAsJsonPrimitive().isNumber())
		{
			Number num = value.getAsJsonPrimitive().getAsNumber();
			 try {
			      long longValue = Long.parseLong(num.toString());
			      if (longValue >= Integer.MIN_VALUE && longValue <= Integer.MAX_VALUE) {
			    	  result[0] = (int)longValue;
			    	  return true;
			      }
			      else
			    	  return false;
			    } catch (NumberFormatException ignored) {
			    	return false;
			    }
		}
		else 
			return false;
	  }
	
}
