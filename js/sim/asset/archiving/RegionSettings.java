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

import java.io.InputStream;
import java.io.StringWriter;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

public class RegionSettings
{
	public boolean AllowDamage;
	public boolean AllowLandResell;
	public boolean AllowLandJoinDivide;
	public boolean BlockFly;
	public boolean BlockLandShowInSearch;
	public boolean BlockTerraform;
	public boolean DisableCollisions;
	public boolean DisablePhysics;
	public boolean DisableScripts;
	public int MaturityRating;
	public boolean RestrictPushing;
	public int AgentLimit;
	public float ObjectBonus;

	public UUID TerrainDetail0;
	public UUID TerrainDetail1;
	public UUID TerrainDetail2;
	public UUID TerrainDetail3;
	public float TerrainHeightRange00;
	public float TerrainHeightRange01;
	public float TerrainHeightRange10;
	public float TerrainHeightRange11;
	public float TerrainStartHeight00;
	public float TerrainStartHeight01;
	public float TerrainStartHeight10;
	public float TerrainStartHeight11;

	public float WaterHeight;
	public float TerrainRaiseLimit;
	public float TerrainLowerLimit;
	public boolean UseEstateSun;
	public boolean FixedSun;

	public static RegionSettings FromStream(InputStream stream)
	{
		RegionSettings settings = new RegionSettings();

		try
		{
			DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
			Document doc = dBuilder.parse(stream);

			//Get the root name
			Element root = doc.getDocumentElement();   

			//            System.Globalization.NumberFormatInfo nfi = Utils.EnUsCulture.NumberFormat;

			if (root.getNodeType() != Node.ELEMENT_NODE || !root.getNodeName().equals("RegionSettings"))
				throw new Exception("Expected an element RegionSettings..");

			NodeList regionalNodeList = root.getChildNodes();

			for(int i=0; i < regionalNodeList.getLength(); i++)
			{
				Node node = regionalNodeList.item(i);
				if (node.getNodeType() == Node.ELEMENT_NODE)
				{
					if(node.getNodeName().equals("Terrain"))
						parseTerrainSettings(settings, node);
					else if(node.getNodeName().equals("General"))
						parseGeneralSettings(settings, node);
					else if(node.getNodeName().equals("GroundTextures"))
						parseGroundTexturesSettings(settings, node);
				}
			}
			//                xtr.ReadStartElement("RegionSettings");
			//                xtr.ReadStartElement("General");
			//
			//                while (xtr.Read() && xtr.NodeType != XmlNodeType.EndElement)
			//                {
			//                    switch (xtr.Name)
			//                    {
			//                        case "AllowDamage":
			//                            settings.AllowDamage = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "AllowLandResell":
			//                            settings.AllowLandResell = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "AllowLandJoinDivide":
			//                            settings.AllowLandJoinDivide = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "BlockFly":
			//                            settings.BlockFly = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "BlockLandShowInSearch":
			//                            settings.BlockLandShowInSearch = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "BlockTerraform":
			//                            settings.BlockTerraform = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "DisableCollisions":
			//                            settings.DisableCollisions = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "DisablePhysics":
			//                            settings.DisablePhysics = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "DisableScripts":
			//                            settings.DisableScripts = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "MaturityRating":
			//                            settings.MaturityRating = Int32.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "RestrictPushing":
			//                            settings.RestrictPushing = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "AgentLimit":
			//                            settings.AgentLimit = Int32.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "ObjectBonus":
			//                            settings.ObjectBonus = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                    }
			//                }
			//
			//                xtr.ReadEndElement();
			//                xtr.ReadStartElement("GroundTextures");
			//
			//                while (xtr.Read() && xtr.NodeType != XmlNodeType.EndElement)
			//                {
			//                    switch (xtr.Name)
			//                    {
			//                        case "Texture1":
			//                            settings.TerrainDetail0 = UUID.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "Texture2":
			//                            settings.TerrainDetail1 = UUID.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "Texture3":
			//                            settings.TerrainDetail2 = UUID.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "Texture4":
			//                            settings.TerrainDetail3 = UUID.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "ElevationLowSW":
			//                            settings.TerrainStartHeight00 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "ElevationLowNW":
			//                            settings.TerrainStartHeight01 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "ElevationLowSE":
			//                            settings.TerrainStartHeight10 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "ElevationLowNE":
			//                            settings.TerrainStartHeight11 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "ElevationHighSW":
			//                            settings.TerrainHeightRange00 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "ElevationHighNW":
			//                            settings.TerrainHeightRange01 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "ElevationHighSE":
			//                            settings.TerrainHeightRange10 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "ElevationHighNE":
			//                            settings.TerrainHeightRange11 = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                    }
			//                }
			//
			//                xtr.ReadEndElement();
			//                xtr.ReadStartElement("Terrain");
			//
			//                while (xtr.Read() && xtr.NodeType != XmlNodeType.EndElement)
			//                {
			//                    switch (xtr.Name)
			//                    {
			//                        case "WaterHeight":
			//                            settings.WaterHeight = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "TerrainRaiseLimit":
			//                            settings.TerrainRaiseLimit = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "TerrainLowerLimit":
			//                            settings.TerrainLowerLimit = Single.Parse(xtr.ReadElementContentAsString(), nfi);
			//                            break;
			//                        case "UseEstateSun":
			//                            settings.UseEstateSun = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                        case "FixedSun":
			//                            settings.FixedSun = Boolean.Parse(xtr.ReadElementContentAsString());
			//                            break;
			//                    }
			//                }


		}
		catch(Exception e)
		{
			JLogger.error("Error in parsing Regional Settings: " + Utils.getExceptionStackTraceAsString(e));
		}
		return settings;
	}

	private static void parseTerrainSettings(RegionSettings settings, Node node) throws Exception
	{
		if (node.getNodeType() != Node.ELEMENT_NODE || !node.getNodeName().equals("Terrain"))
			throw new Exception("Expected an element RegionSettings.Terrain Node..Got name name: " + node.getNodeName());

		NodeList childNodes = node.getChildNodes();
		for(int i =0; i< childNodes.getLength(); i++)
		{
			Node childNode = childNodes.item(i);
			String type = childNode.getNodeName();

			if(type.equals("WaterHeight"))
			{
				settings.WaterHeight = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("TerrainRaiseLimit"))
			{
				settings.TerrainRaiseLimit = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("TerrainLowerLimit"))
			{
				settings.TerrainLowerLimit = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("UseEstateSun"))
			{
				settings.UseEstateSun = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("FixedSun"))
			{
				settings.FixedSun = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
		}
	}

	private static void parseGroundTexturesSettings(RegionSettings settings, Node node) throws Exception
	{
		if (node.getNodeType() != Node.ELEMENT_NODE || !node.getNodeName().equals("GroundTextures"))
			throw new Exception("Expected an element RegionSettings.GroundTextures Node.. Got name name: " + node.getNodeName());

		NodeList childNodes = node.getChildNodes();
		for(int i =0; i< childNodes.getLength(); i++)
		{
			Node childNode = childNodes.item(i);
			String type = childNode.getNodeName();

			if(type.equals("Texture1"))
			{
				settings.TerrainDetail0 = UUID.Parse(childNode.getFirstChild().getNodeValue().trim());
				System.out.println(settings.TerrainDetail0.toString());
			}
			else if (type.equals("Texture2"))
			{
				settings.TerrainDetail1 = UUID.Parse(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("Texture3"))
			{
				settings.TerrainDetail2 = UUID.Parse(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("Texture4"))
			{
				settings.TerrainDetail3 = UUID.Parse(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationLowSW"))
			{
				settings.TerrainStartHeight00 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationLowNW"))
			{
				settings.TerrainStartHeight01 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationLowSE"))
			{
				settings.TerrainStartHeight10 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationLowNE"))
			{
				settings.TerrainStartHeight11 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationHighSW"))
			{
				settings.TerrainHeightRange00 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationHighNW"))
			{
				settings.TerrainHeightRange01 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationHighSE"))
			{
				settings.TerrainHeightRange10 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
			else if (type.equals("ElevationHighNE"))
			{
				settings.TerrainHeightRange11 = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
		}
	}


	private static void parseGeneralSettings(RegionSettings settings, Node node) throws Exception
	{
		if (node.getNodeType() != Node.ELEMENT_NODE || !node.getNodeName().equals("General"))
			throw new Exception("Expected an element RegionSettings.General Node..Got Node Name: " + node.getNodeName());


		NodeList generalChildNodes = node.getChildNodes();
		for(int i =0; i< generalChildNodes.getLength(); i++)
		{
			Node childNode = generalChildNodes.item(i);
			String type = childNode.getNodeName();

			if(type.equals("AllowDamage"))
			{
				settings.AllowDamage = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("AllowLandResell"))
			{
				settings.AllowLandResell = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("AllowLandJoinDivide"))
			{
				settings.AllowLandJoinDivide = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("BlockFly"))
			{
				settings.BlockFly = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("BlockLandShowInSearch"))
			{
				settings.BlockLandShowInSearch = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("BlockTerraform"))
			{
				settings.BlockTerraform = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("DisableCollisions"))
			{
				settings.DisableCollisions = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("DisablePhysics"))
			{
				settings.DisablePhysics = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("DisableScripts"))
			{
				settings.DisableScripts = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("MaturityRating"))
			{
				settings.MaturityRating = Integer.parseInt(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("RestrictPushing"))
			{
				settings.RestrictPushing = Boolean.parseBoolean(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("AgentLimit"))
			{
				settings.AgentLimit = Integer.parseInt(childNode.getFirstChild().getNodeValue().trim());
			}
			else if(type.equals("ObjectBonus"))
			{
				settings.ObjectBonus = Float.parseFloat(childNode.getFirstChild().getNodeValue().trim());
			}
		}
	}
	
	private static String nodeToString(Node node) throws TransformerFactoryConfigurationError, TransformerException {
		StringWriter sw = new StringWriter();
		
		 Transformer t = TransformerFactory.newInstance().newTransformer();
		 t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
		 t.setOutputProperty(OutputKeys.INDENT, "yes");
		 t.transform(new DOMSource(node), new StreamResult(sw));
		
		return sw.toString();
		}
}



