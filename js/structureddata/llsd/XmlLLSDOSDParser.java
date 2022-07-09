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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.StringWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.apache.commons.codec.binary.Base64;
import org.w3c.dom.Attr;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDException;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// </summary>
public class XmlLLSDOSDParser
{
	//        private static Schema XmlSchema;
	//        private static DocumentBuilder XmlTextReader;
	//        private static String LastXmlErrors = "";
	//        private static Object XmlValidationLock = new Object();
	//
	/// <summary>
	/// 
	/// </summary>
	/// <param name="xmlData"></param>
	/// <returns></returns>
	public static OSD DeserializeLLSDXml(byte[] xmlData)
	{
		return DeserializeLLSDXml(new ByteArrayInputStream(xmlData));
	}

	//        public static OSD DeserializeLLSDXml(IStream xmlStream)
	//        {
	//            return DeserializeLLSDXml(new XmlTextReader(xmlStream));
	//        }

	/// <summary>
	/// 
	/// </summary>
	/// <param name="xmlData"></param>
	/// <returns></returns>
	public static OSD DeserializeLLSDXml(String xmlData)
	{
		byte[] bytes = Utils.stringToBytes(xmlData);
		return DeserializeLLSDXml(bytes);
	}

	/// <summary>
	/// 
	/// </summary>
	/// <param name="xmlData"></param>
	/// <returns></returns>
	public static OSD DeserializeLLSDXml(InputStream xmlData)
	{
		try
		{
			DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
			Document doc = dBuilder.parse(xmlData);

			//Get the root name
			Element root = doc.getDocumentElement();

			OSD ret = ParseLLSDXmlRoot(root);

			return ret;
		}
		catch(Exception e)
		{
			JLogger.error("Error in parsing xml: " + Utils.getExceptionStackTraceAsString(e));
			e.printStackTrace();
			return new OSD();
		}
	}

	        /// <summary>
	        /// 
	        /// </summary>
	        /// <param name="data"></param>
	        /// <returns></returns>
	        public static byte[] SerializeLLSDXmlBytes(OSD data) throws Exception
	        {
	            return Utils.stringToBytes(SerializeLLSDXmlString(data));
	        }
	
	        /// <summary>
	        /// 
	        /// </summary>
	        /// <param name="data"></param>
	        /// <returns></returns>
	        public static String SerializeLLSDXmlString(OSD data) throws Exception
	        {
//	            StringWriter sw = new StringWriter();
//	            XmlTextWriter writer = new XmlTextWriter(sw);
//	            writer.Formatting = Formatting.None;
//	
//	            writer.WriteStartElement("", "llsd", "");
//	            SerializeLLSDXmlElement(writer, data);
//	            writer.WriteEndElement();
//	
//	            writer.Close();
	
	        	DocumentBuilderFactory docFactory = DocumentBuilderFactory.newInstance();
	    		DocumentBuilder docBuilder = docFactory.newDocumentBuilder();
	     
	    		// root elements
	    		Document doc = docBuilder.newDocument();
	    		Element rootElement = doc.createElement("llsd");
	    		doc.appendChild(rootElement);
	            
	    		SerializeLLSDXmlElement(doc, rootElement, data);
	    		
	    		TransformerFactory transformerFactory = TransformerFactory.newInstance();
	    		Transformer transformer = transformerFactory.newTransformer();
	    		
	    		DOMSource source = new DOMSource(doc);
	    		StringWriter writer = new StringWriter();
	    		StreamResult result = new StreamResult(writer);
	    		transformer.transform(source, result);
	            return writer.getBuffer().toString();
	        }
	
	        /// <summary>
	        /// 
	        /// </summary>
	        /// <param name="writer"></param>
	        /// <param name="data"></param>
	        public static void SerializeLLSDXmlElement(Document doc, Element rootElement, OSD data)
	        {
	        	Element newChild = null;
	            switch (data.getType())
	            {
	                case Unknown:
//	                    writer.WriteStartElement("", "undef", "");
//	                    writer.WriteEndElement();
	                    newChild = doc.createElement("undef");
	                    rootElement.appendChild(newChild);
	                    break;
	                case Boolean:
//	                    writer.WriteStartElement("", "boolean", "");
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("boolean");
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    break;
	                case Integer:
//	                    writer.WriteStartElement("", "integer", "");
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("integer");
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    
	                    break;
	                case Real:
//	                    writer.WriteStartElement("", "real", "");
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                	
	                    newChild = doc.createElement("real");
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    break;
	                case String:
//	                    writer.WriteStartElement("", "string", "");
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                	
	                    newChild = doc.createElement("string");
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    break;
	                case UUID:
//	                    writer.WriteStartElement("", "uuid", "");
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("uuid");
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    
	                    break;
	                case Date:
//	                    writer.WriteStartElement("", "date", "");
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("date");
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    
	                    break;
	                case URI:
//	                    writer.WriteStartElement("", "uri", "");
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("uri");
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    
	                    break;
	                case Binary:
//	                    writer.WriteStartElement("", "binary", "");
//	                        writer.WriteStartAttribute("", "encoding", "");
//	                        writer.WriteString("base64");
//	                        writer.WriteEndAttribute();
//	                    writer.WriteString(data.asString());
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("binary");
	                    
	                    Attr attr = doc.createAttribute("encoding");
	            		attr.setValue("base64");
	            		newChild.setAttributeNode(attr);
	                    
	                    newChild.appendChild(doc.createTextNode(data.asString()));
	                    rootElement.appendChild(newChild);
	                    
	                    break;
	                case Map:
	                    OSDMap map = (OSDMap)data;
//	                    writer.WriteStartElement("", "map", "");
//	                    foreach (KeyValuePair<String, OSD> kvp in map)
//	                    {
//	                        writer.WriteStartElement("", "key", "");
//	                        writer.WriteString(kvp.Key);
//	                        writer.WriteEndElement();
//	
//	                        SerializeLLSDXmlElement(writer, kvp.Value);
//	                    }
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("map");
	                    
	                    for(Map.Entry<String, OSD> entry: map.entrySet())
	                    {
	                    	Node keyNode = doc.createElement("key");
	                    	keyNode.appendChild(doc.createTextNode(entry.getKey()));
	                    	newChild.appendChild(keyNode);
	                    	SerializeLLSDXmlElement(doc, newChild, entry.getValue());
	                    }
	                    
	                    rootElement.appendChild(newChild);
	                    
	                    break;
	                case Array:
	                    OSDArray array = (OSDArray)data;
	                    
//	                    writer.WriteStartElement("", "array", "");
//	                    for (int i = 0; i < array.Count; i++)
//	                    {
//	                        SerializeLLSDXmlElement(writer, array[i]);
//	                    }
//	                    writer.WriteEndElement();
	                    
	                    newChild = doc.createElement("array");
	                    for (int i = 0; i < array.count(); i++)
	                    {
	                        SerializeLLSDXmlElement(doc, newChild, array.get(i));
	                    }
	                    rootElement.appendChild(newChild);
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
	
	private static OSD ParseLLSDXmlRoot(Node reader) throws OSDException, URISyntaxException
	{
		if (reader.getNodeType() != Node.ELEMENT_NODE || !reader.getNodeName().equals("llsd"))
			throw new OSDException("Expected an element llsd");
		
		NodeList nodeList = reader.getChildNodes();
		OSD ret = new OSD();
		
		for(int i = 0; i < nodeList.getLength(); i++)
		{
			if(nodeList.item(i).getNodeType() == Node.ELEMENT_NODE)
			{
				//System.out.println(nodeList.item(i).getNodeName());
				ret = ParseLLSDXmlElement(nodeList.item(i));
				break;
			}
		}
		return ret;
	}
	
	
	private static OSD ParseLLSDXmlElement(Node reader) throws OSDException, URISyntaxException
	{
		if (reader.getNodeType() != Node.ELEMENT_NODE)
			throw new OSDException("Expected an element");

		String type = reader.getNodeName();
		OSD ret;

		////System.out.println("Parsing Element..." + type);
		
		if(type.equals("undef"))
		{
			if (!reader.hasChildNodes())
			{
				return new OSD();
			}
			ret = new OSD();
		}
		else if(type.equals("boolean"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromBoolean(false);
			}

			String s = reader.getFirstChild().getNodeValue().trim();

			if (s.equals("true") || s.equals("1"))
				ret = OSD.FromBoolean(true);
			else
				ret = OSD.FromBoolean(false);
		}
		else if(type.equals("integer"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromInteger(0);
			}

			String s = reader.getFirstChild().getNodeValue().trim();
			int[] result = new int[1];
			if(Utils.tryParseInt(s, result))
			{
				ret = OSD.FromInteger(result[0]);
			}
			else
				ret = OSD.FromInteger(0);
		}
		else if(type.equals("real"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromReal(0d);
			}

			double[] value = new double[]{0d};
			String s = reader.getFirstChild().getNodeValue().trim().toLowerCase();

			if (s.equals("nan"))
				value[0] = Double.NaN;
			else
				Utils.tryParseDouble(s, value);

			ret = OSD.FromReal(value[0]);
		}
		else if(type.equals("uuid"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromUUID(UUID.Zero);
			}

			UUID[] value = new UUID[]{UUID.Zero};
			String s = reader.getFirstChild().getNodeValue().trim();
			UUID.TryParse(s, value);
			ret = OSD.FromUUID(value[0]);
		}
		else if(type.equals("date"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromDate(Utils.Epoch);
			}
			Date[] value = new Date[]{Utils.Epoch};
			String s = reader.getFirstChild().getNodeValue().trim();
			Utils.tryParseDate(s, value);
			ret = OSD.FromDate(value[0]);
		}
		else if(type.equals("string"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromString("");
			}

			String s = reader.getFirstChild().getNodeValue().trim();
			ret = OSD.FromString(s);

		}
		else if(type.equals("binary"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromBinary(Utils.EmptyBytes);
			}

			NamedNodeMap namedNodeMap = reader.getAttributes();
			Node attr = namedNodeMap.getNamedItem("encoding");
			if (attr != null && !attr.getNodeValue().equals("base64"))
				throw new OSDException("Unsupported binary encoding: " + attr.getNodeValue());
			
			try
			{
				String s = reader.getFirstChild().getNodeValue().trim();
				byte[] bytes = Utils.decodeBase64String(s);

				ret = OSD.FromBinary(bytes);
			}
			catch (Exception ex)
			{
				throw new OSDException("Binary decoding exception: " + ex.getMessage());
			}

		}
		else if(type.equals("uri"))
		{
			if (!reader.hasChildNodes())
			{
				return OSD.FromUri(new URI(""));
			}

			String s = reader.getFirstChild().getNodeValue().trim();
			ret = OSD.FromUri(new URI(s));
		}
		else if(type.equals("map"))
		{
			return ParseLLSDXmlMap(reader);
		}
		else if(type.equals("array"))
		{
			return ParseLLSDXmlArray(reader);
		}
		else
		{
			ret = null;
		}
		
		return ret;
	}

	private static OSDMap ParseLLSDXmlMap(Node reader) throws OSDException, URISyntaxException
	{
		if (reader.getNodeType() != Node.ELEMENT_NODE|| !reader.getNodeName().equals("map"))
			throw new OSDException("Expected <map>");

		////System.out.println("Parsing Map...");
		
		OSDMap map = new OSDMap();

		if (!reader.hasChildNodes())
		{
			return map;
		}

		NodeList nodeList = reader.getChildNodes();
		
		List<Node> nl = new ArrayList<Node>();
		
		//Remove the Non Element Nodes
		for(int i = 0; i < nodeList.getLength(); i+=1)
		{
			if(nodeList.item(i).getNodeType() == Node.ELEMENT_NODE)
			{
				nl.add(nodeList.item(i));
			}
		}
		
		for(int i = 0; i < nl.size(); i+=2)
		{
			//System.out.println(i);
				Node keyNode = nl.get(i);
				Node valueNode = nl.get(i+1);
			
				if (keyNode == null || !keyNode.getNodeName().equals("key"))
					throw new OSDException("Expected <key>");
			
				if (valueNode == null)
					throw new OSDException("Expected value for a key");				
				//System.out.println(keyNode.getFirstChild().getNodeValue());
				String key = keyNode.getFirstChild().getNodeValue().trim();
				map.put(key, ParseLLSDXmlElement(valueNode));
		}
		
		return map;
	}

	private static OSDArray ParseLLSDXmlArray(Node reader) throws OSDException, URISyntaxException
	{
		if (reader.getNodeType() != Node.ELEMENT_NODE|| !reader.getNodeName().equals("array"))
			throw new OSDException("Expected <array>");

		//System.out.println("Parsing Array...");
		
		OSDArray array = new OSDArray();

		if (!reader.hasChildNodes())
		{
			return array;
		}

		NodeList nodeList = reader.getChildNodes();
		
		for(int i = 0; i < nodeList.getLength(); i++)
		{
			if(nodeList.item(i).getNodeType() == Node.ELEMENT_NODE)
			{
				array.add(ParseLLSDXmlElement(nodeList.item(i)));
			}
		}
		return array;
	}        

	//        private static void SkipWhitespace(XmlTextReader reader)
	//        {
	//            while (
	//                reader.NodeType == XmlNodeType.Comment ||
	//                reader.NodeType == XmlNodeType.Whitespace ||
	//                reader.NodeType == XmlNodeType.SignificantWhitespace ||
	//                reader.NodeType == XmlNodeType.XmlDeclaration)
	//            {
	//                reader.Read();
	//            }
	//        }

	//        private static void CreateLLSDXmlSchema()
	//        {
	//            if (XmlSchema == null)
	//            {
	//                //region XSD
	//                String schemaText = "<?xml version=\"1.0\" encoding=\"utf-8\"?>" + 
	//"<xs:schema elementFormDefault=\"qualified\" xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">" + 
	//  "<xs:import schemaLocation=\"xml.xsd\" namespace=\"http://www.w3.org/XML/1998/namespace\" />" + 
	//  "<xs:element name=\"uri\" type=\"xs:String\" />" + 
	//  "<xs:element name=\"uuid\" type=\"xs:String\" />" + 
	//  "<xs:element name=\"KEYDATA\">"  + 
	//    "<xs:complexType>" + 
	//      "<xs:sequence>" + 
	//        "<xs:element ref=\"key\" />" + 
	//        "<xs:element ref=\"DATA\" />" + 
	//      "</xs:sequence>" + 
	//    "</xs:complexType>" +
	//  "</xs:element>" + 
	//  "<xs:element name=\"date\" type=\"xs:String\" />" + 
	//  "<xs:element name=\"key\" type=\"xs:String\" />" + 
	//  "<xs:element name=\"boolean\" type=\"xs:String\" />" + 
	//  "<xs:element name=\"undef\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:sequence>" + 
	//        "<xs:element ref=\"EMPTY\" />" + 
	//      "</xs:sequence>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//  "<xs:element name=\"map\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:sequence>" + 
	//        "<xs:element minOccurs=\"0\" maxOccurs=\"unbounded\" ref=\"KEYDATA\" />" + 
	//      "</xs:sequence>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//  "<xs:element name=\"real\" type=\"xs:String\" />" + 
	//  "<xs:element name=\"ATOMIC\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:choice>" + 
	//        "<xs:element ref=\"undef\" />" + 
	//        "<xs:element ref=\"boolean\" />" + 
	//        "<xs:element ref=\"integer\" />" + 
	//        "<xs:element ref=\"real\" />" + 
	//        "<xs:element ref=\"uuid\" />" + 
	//        "<xs:element ref=\"String\" />" + 
	//        "<xs:element ref=\"date\" />" + 
	//        "<xs:element ref=\"uri\" />" + 
	//        "<xs:element ref=\"binary\" />" + 
	//      "</xs:choice>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//  "<xs:element name=\"DATA\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:choice>" + 
	//        "<xs:element ref=\"ATOMIC\" />" + 
	//        "<xs:element ref=\"map\" />" + 
	//        "<xs:element ref=\"array\" />" + 
	//      "</xs:choice>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//  "<xs:element name=\"llsd\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:sequence>" + 
	//        "<xs:element ref=\"DATA\" />" + 
	//      "</xs:sequence>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//  "<xs:element name=\"binary\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:simpleContent>" + 
	//        "<xs:extension base=\"xs:String\">" + 
	//          "<xs:attribute default=\"base64\" name=\"encoding\" type=\"xs:String\" />"+
	//        "</xs:extension>" + 
	//      "</xs:simpleContent>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//  "<xs:element name=\"array\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:sequence>" + 
	//        "<xs:element minOccurs=\"0\" maxOccurs=\"unbounded\" ref=\"DATA\" />" + 
	//      "</xs:sequence>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//  "<xs:element name=\"integer\" type=\"xs:String\" />" + 
	//  "<xs:element name=\"String\">" + 
	//    "<xs:complexType>" + 
	//      "<xs:simpleContent>" + 
	//        "<xs:extension base=\"xs:String\">" + 
	//          "<xs:attribute ref=\"xml:space\" />" + 
	//        "</xs:extension>" + 
	//      "</xs:simpleContent>" + 
	//    "</xs:complexType>" + 
	//  "</xs:element>" + 
	//"</xs:schema>" 
	//  ;
	//                //endregion XSD
	//
	//                MemoryStream stream = new MemoryStream(Encoding.ASCII.GetBytes(schemaText));
	//
	//                XmlSchema = new XmlSchema();
	//                XmlSchema = XmlSchema.Read(stream, new ValidationEventHandler(LLSDXmlSchemaValidationHandler));
	//            }
	//        }
	//
	//        private static void LLSDXmlSchemaValidationHandler(Object sender, ValidationEventArgs args)
	//        {
	//            String error = sprintf(error, "Line: {0} - Position: {1} - {2}", XmlTextReader.LineNumber, XmlTextReader.LinePosition,
	//                args.Message);
	//
	//            if (LastXmlErrors == "")
	//                LastXmlErrors = error;
	//            else
	//                LastXmlErrors += Environment.NewLine + error;
	//        }
}
