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

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.URI;
import java.util.Date;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;

import com.ngt.jopenmetaverse.shared.structureddata.OSD;
import com.ngt.jopenmetaverse.shared.structureddata.OSDArray;
import com.ngt.jopenmetaverse.shared.structureddata.OSDException;
import com.ngt.jopenmetaverse.shared.structureddata.OSDMap;
import com.ngt.jopenmetaverse.shared.structureddata.OSDType;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.Utils;

/// </summary>
    public class NotationalLLSDOSDParser
    {
    	 private final static String baseIndent = "  ";

         private final static char undefNotationValue = '!';

         private final static char trueNotationValueOne = '1';
         private final static char trueNotationValueTwo = 't';
         private final static char[] trueNotationValueTwoFull = { 't', 'r', 'u', 'e' };
         private final static char trueNotationValueThree = 'T';
         private final static char[] trueNotationValueThreeFull = { 'T', 'R', 'U', 'E' };

         private final static char falseNotationValueOne = '0';
         private final static char falseNotationValueTwo = 'f';
         private final static char[] falseNotationValueTwoFull = { 'f', 'a', 'l', 's', 'e' };
         private final static char falseNotationValueThree = 'F';
         private final static char[] falseNotationValueThreeFull = { 'F', 'A', 'L', 'S', 'E' };

         private final static char integerNotationMarker = 'i';
         private final static char realNotationMarker = 'r';
         private final static char uuidNotationMarker = 'u';
         private final static char binaryNotationMarker = 'b';
         private final static char StringNotationMarker = 's';
         private final static char uriNotationMarker = 'l';
         private final static char dateNotationMarker = 'd';

         private final static char arrayBeginNotationMarker = '[';
         private final static char arrayEndNotationMarker = ']';

         private final static char mapBeginNotationMarker = '{';
         private final static char mapEndNotationMarker = '}';
         private final static char kommaNotationDelimiter = ',';
         private final static char keyNotationDelimiter = ':';

         private final static char sizeBeginNotationMarker = '(';
         private final static char sizeEndNotationMarker = ')';
         private final static char doubleQuotesNotationMarker = '"';
         private final static char singleQuotesNotationMarker = '\'';

         public static OSD DeserializeLLSDNotation(String notationData) throws OSDException, IOException
         {
             StringReader reader = new StringReader(notationData);
             OSD osd = DeserializeLLSDNotation(reader);
             reader.close();
             return osd;
         }

         public static OSD DeserializeLLSDNotation(StringReader reader) throws OSDException, IOException
         {
             OSD osd = DeserializeLLSDNotationElement(reader);
             return osd;
         }

         public static String SerializeLLSDNotation(OSD osd) throws IOException, OSDException
         {
             StringWriter writer = SerializeLLSDNotationStream(osd);
             String s = writer.toString();
             writer.close();

             return s;
         }

         public static StringWriter SerializeLLSDNotationStream(OSD osd) throws OSDException
         {
             StringWriter writer = new StringWriter();

             SerializeLLSDNotationElement(writer, osd);
             return writer;
         }

         public static String SerializeLLSDNotationFormatted(OSD osd) throws IOException, OSDException
         {
             StringWriter writer = SerializeLLSDNotationStreamFormatted(osd);
             String s = writer.toString();
             writer.close();

             return s;
         }

         public static StringWriter SerializeLLSDNotationStreamFormatted(OSD osd) throws OSDException
         {
             StringWriter writer = new StringWriter();

             String indent = "";
             SerializeLLSDNotationElementFormatted(writer, indent, osd);
             return writer;
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="reader"></param>
         /// <returns></returns>
         private static OSD DeserializeLLSDNotationElement(StringReader reader) throws OSDException, IOException
         {
             int character = ReadAndSkipWhitespace(reader);
             if (character < 0)
                 return new OSD(); // server returned an empty file, so we're going to pass along a null LLSD object

//        	 System.out.print((char)character);
             
             OSD osd;
             int matching;
             switch ((char)character)
             {
                 case undefNotationValue:
                     osd = new OSD();
                     break;
                 case trueNotationValueOne:
                     osd = OSD.FromBoolean(true);
                     break;
                 case trueNotationValueTwo:
                     matching = BufferCharactersEqual(reader, trueNotationValueTwoFull, 1);
                     if (matching > 1 && matching < trueNotationValueTwoFull.length)
                         throw new OSDException("Notation LLSD parsing: True value parsing error:");
                     osd = OSD.FromBoolean(true);
                     break;
                 case trueNotationValueThree:
                     matching = BufferCharactersEqual(reader, trueNotationValueThreeFull, 1);
                     if (matching > 1 && matching < trueNotationValueThreeFull.length)
                         throw new OSDException("Notation LLSD parsing: True value parsing error:");
                     osd = OSD.FromBoolean(true);
                     break;
                 case falseNotationValueOne:
                     osd = OSD.FromBoolean(false);
                     break;
                 case falseNotationValueTwo:
                     matching = BufferCharactersEqual(reader, falseNotationValueTwoFull, 1);
                     if (matching > 1 && matching < falseNotationValueTwoFull.length)
                         throw new OSDException("Notation LLSD parsing: True value parsing error:");
                     osd = OSD.FromBoolean(false);
                     break;
                 case falseNotationValueThree:
                     matching = BufferCharactersEqual(reader, falseNotationValueThreeFull, 1);
                     if (matching > 1 && matching < falseNotationValueThreeFull.length)
                         throw new OSDException("Notation LLSD parsing: True value parsing error:");
                     osd = OSD.FromBoolean(false);
                     break;
                 case integerNotationMarker:
                     osd = DeserializeLLSDNotationInteger(reader);
                     break;
                 case realNotationMarker:
                     osd = DeserializeLLSDNotationReal(reader);
                     break;
                 case uuidNotationMarker:
                     char[] uuidBuf = new char[36];
                     if (reader.read(uuidBuf, 0, 36) < 36)
                         throw new OSDException("Notation LLSD parsing: Unexpected end of stream in UUID.");
                     UUID[] lluuid = new UUID[1];
                     if (!UUID.TryParse(new String(uuidBuf), lluuid))
                         throw new OSDException("Notation LLSD parsing: Invalid UUID discovered.");
                     osd = OSD.FromUUID(lluuid[0]);
                     break;
                 case binaryNotationMarker:
                     byte[] bytes = Utils.EmptyBytes;
                     reader.mark(2);
                     int bChar = reader.read();
                     if (bChar < 0)
                     {
                    	 reader.reset();
                         throw new OSDException("Notation LLSD parsing: Unexpected end of stream in binary.");
                     }
                     if ((char)bChar == sizeBeginNotationMarker)
                     {
                    	 reader.reset();
                         throw new OSDException("Notation LLSD parsing: Raw binary encoding not supported.");
                     }
                     else if (Character.isDigit((char)bChar))
                     {
                    	 reader.reset();
                         char[] charsBaseEncoding = new char[2];
                         if (reader.read(charsBaseEncoding, 0, 2) < 2)
                             throw new OSDException("Notation LLSD parsing: Unexpected end of stream in binary.");
                         int baseEncoding[] = new int[1];
                         if (!Utils.tryParseInt(new String(charsBaseEncoding), baseEncoding))
                             throw new OSDException("Notation LLSD parsing: Invalid binary encoding base.");
                         if (baseEncoding[0] == 64)
                         {
                             if (reader.read() < 0)
                                 throw new OSDException("Notation LLSD parsing: Unexpected end of stream in binary.");
                             String bytes64 = GetStringDelimitedBy(reader, doubleQuotesNotationMarker);
                             
                             bytes = Utils.decodeBase64String(bytes64);
                         }
                         else
                         {
                             throw new OSDException("Notation LLSD parsing: Encoding base" + baseEncoding + " + not supported.");
                         }
                     }
                     osd = OSD.FromBinary(bytes);
                     break;
                 case StringNotationMarker:
                     int numChars = GetLengthInBrackets(reader);
                     if (reader.read() < 0)
                         throw new OSDException("Notation LLSD parsing: Unexpected end of stream in String.");
                     char[] chars = new char[numChars];
                     if (reader.read(chars, 0, numChars) < numChars)
                         throw new OSDException("Notation LLSD parsing: Unexpected end of stream in String.");
                     if (reader.read() < 0)
                         throw new OSDException("Notation LLSD parsing: Unexpected end of stream in String.");
                     osd = OSD.FromString(new String(chars));
                     break;
                 case singleQuotesNotationMarker:
                     String sOne = GetStringDelimitedBy(reader, singleQuotesNotationMarker);
                     osd = OSD.FromString(sOne);
                     break;
                 case doubleQuotesNotationMarker:
                     String sTwo = GetStringDelimitedBy(reader, doubleQuotesNotationMarker);
                     osd = OSD.FromString(sTwo);
                     break;
                 case uriNotationMarker:
                     if (reader.read() < 0)
                         throw new OSDException("Notation LLSD parsing: Unexpected end of stream in String.");
                     String sUri = GetStringDelimitedBy(reader, doubleQuotesNotationMarker);
                     //System.out.println("URI: " + sUri);
                     URI[] uri = new URI[1];
                     if(Utils.tryParseUri(sUri, uri))
                     {
                         osd = OSD.FromUri(uri[0]);
                     }
                     else
                         throw new OSDException("Notation LLSD parsing: Invalid Uri format detected.");                    
                     break;
                 case dateNotationMarker:
                     if (reader.read() < 0)
                         throw new OSDException("Notation LLSD parsing: Unexpected end of stream in date.");
                     String date = GetStringDelimitedBy(reader, doubleQuotesNotationMarker);
                     Date dt[] = new Date[1];
                     if (!Utils.tryParseDate(date, dt))
                         throw new OSDException("Notation LLSD parsing: Invalid date discovered.");
                     osd = OSD.FromDate(dt[0]);
                     break;
                 case arrayBeginNotationMarker:
                     osd = DeserializeLLSDNotationArray(reader);
                     break;
                 case mapBeginNotationMarker:
                     osd = DeserializeLLSDNotationMap(reader);
                     break;
                 default:
                     throw new OSDException("Notation LLSD parsing: Unknown type marker '" + (char)character + "'.");
             }
             return osd;
         }

         private static OSD DeserializeLLSDNotationInteger(StringReader reader) throws OSDException, IOException
         {
             int character;
             StringBuilder s = new StringBuilder();
             reader.mark(2);
             if (((character = reader.read()) > 0) && ((char)character == '-'))
                 s.append((char)character);
             else
            	 reader.reset();
             
             reader.mark(2);
             while ((character = reader.read()) > 0 &&
                            Character.isDigit((char)character))
             {
                 s.append((char)character);
                 reader.mark(2);
             }
             reader.reset();
             int integer[] = new int[1];
             if (!Utils.tryParseInt(s.toString(), integer))
                 throw new OSDException("Notation LLSD parsing: Can't parse integer value." + s.toString());

             return OSD.FromInteger(integer[0]);
         }

         private static OSD DeserializeLLSDNotationReal(StringReader reader) throws IOException, OSDException
         {
             int character;
             StringBuilder s = new StringBuilder();
             reader.mark(2);
             if (((character = reader.read()) > 0) &&
                 ((char)character == '-' && (char)character == '+'))
                 s.append((char)character);
             else
            	 reader.reset();
             
             reader.mark(2);
             while (((character = reader.read()) > 0) &&
                    (Character.isDigit((char)character) || (char)character == '.' ||
                     (char)character == 'e' || (char)character == 'E' ||
                     (char)character == '+' || (char)character == '-'))
             {
                 s.append((char)character);
                 reader.mark(2);
             }
             double dbl[] = new double[1];
             if (!Utils.tryParseDouble(s.toString(), dbl))
                 throw new OSDException("Notation LLSD parsing: Can't parse real value: " + s.toString());

        	 reader.reset();
             return OSD.FromReal(dbl[0]);
         }

         private static OSD DeserializeLLSDNotationArray(StringReader reader) throws OSDException, IOException
         {
//        	 System.out.println("Deserializing Array...");
             int character;
             OSDArray osdArray = new OSDArray();
             while (((character = PeekAndSkipWhitespace(reader)) > 0) &&
                   ((char)character != arrayEndNotationMarker))
             {
//            	 System.out.print((char)character);
                 osdArray.add(DeserializeLLSDNotationElement(reader));

                 character = ReadAndSkipWhitespace(reader);
//            	 System.out.print((char)character);
                 if (character < 0)
                     throw new OSDException("Notation LLSD parsing: Unexpected end of array discovered.");
                 else if ((char)character == kommaNotationDelimiter)
                     continue;
                 else if ((char)character == arrayEndNotationMarker)
                     break;
             }
             if (character < 0)
                 throw new OSDException("Notation LLSD parsing: Unexpected end of array discovered.");

             return (OSD)osdArray;
         }

         private static OSD DeserializeLLSDNotationMap(StringReader reader) throws IOException, OSDException
         {
             int character;
             OSDMap osdMap = new OSDMap();
             while (((character = PeekAndSkipWhitespace(reader)) > 0) &&
                   ((char)character != mapEndNotationMarker))
             {
                 OSD osdKey = DeserializeLLSDNotationElement(reader);
                 if (osdKey.getType() != OSDType.String)
                     throw new OSDException("Notation LLSD parsing: Invalid key in map");
                 String key = osdKey.asString();

                 character = ReadAndSkipWhitespace(reader);
                 if ((char)character != keyNotationDelimiter)
                     throw new OSDException("Notation LLSD parsing: Unexpected end of stream in map.");
                 if ((char)character != keyNotationDelimiter)
                     throw new OSDException("Notation LLSD parsing: Invalid delimiter in map.");

                 osdMap.put(key, DeserializeLLSDNotationElement(reader));
                 character = ReadAndSkipWhitespace(reader);
                 if (character < 0)
                     throw new OSDException("Notation LLSD parsing: Unexpected end of map discovered.");
                 else if ((char)character == kommaNotationDelimiter)
                     continue;
                 else if ((char)character == mapEndNotationMarker)
                     break;
             }
             if (character < 0)
                 throw new OSDException("Notation LLSD parsing: Unexpected end of map discovered.");

             return (OSD)osdMap;
         }

         private static void SerializeLLSDNotationElement(StringWriter writer, OSD osd) throws OSDException
         {

             switch (osd.getType())
             {
                 case Unknown:
                     writer.write(undefNotationValue);
                     break;
                 case Boolean:
                     if (osd.asBoolean())
                         writer.write(trueNotationValueTwo);
                     else
                         writer.write(falseNotationValueTwo);
                     break;
                 case Integer:
                     writer.write(integerNotationMarker);
                     writer.write(osd.asString());
                     break;
                 case Real:
                     writer.write(realNotationMarker);
                     writer.write(osd.asString());
                     break;
                 case UUID:
                     writer.write(uuidNotationMarker);
                     writer.write(osd.asString());
                     break;
                 case String:
                     writer.write(singleQuotesNotationMarker);
                     writer.write(EscapeCharacter(osd.asString(), singleQuotesNotationMarker));
                     writer.write(singleQuotesNotationMarker);
                     break;
                 case Binary:
                     writer.write(binaryNotationMarker);
                     writer.write("64");
                     writer.write(doubleQuotesNotationMarker);
                     writer.write(osd.asString());
                     writer.write(doubleQuotesNotationMarker);
                     break;
                 case Date:
                     writer.write(dateNotationMarker);
                     writer.write(doubleQuotesNotationMarker);
                     writer.write(osd.asString());
                     writer.write(doubleQuotesNotationMarker);
                     break;
                 case URI:
                     writer.write(uriNotationMarker);
                     writer.write(doubleQuotesNotationMarker);
                     writer.write(EscapeCharacter(osd.asString(), doubleQuotesNotationMarker));
                     writer.write(doubleQuotesNotationMarker);
                     break;
                 case Array:
                     SerializeLLSDNotationArray(writer, (OSDArray)osd);
                     break;
                 case Map:
                     SerializeLLSDNotationMap(writer, (OSDMap)osd);
                     break;
                 default:
                     throw new OSDException("Notation serialization: Not existing element discovered.");

             }
         }

         private static void SerializeLLSDNotationArray(StringWriter writer, OSDArray osdArray) throws OSDException
         {
             writer.write(arrayBeginNotationMarker);
             int lastIndex = osdArray.count() - 1;

             for (int idx = 0; idx <= lastIndex; idx++)
             {
                 SerializeLLSDNotationElement(writer, osdArray.get(idx));
                 if (idx < lastIndex)
                     writer.write(kommaNotationDelimiter);
             }
             writer.write(arrayEndNotationMarker);
         }

         private static void SerializeLLSDNotationMap(StringWriter writer, OSDMap osdMap) throws OSDException
         {
             writer.write(mapBeginNotationMarker);
             int lastIndex = osdMap.count() - 1;
             int idx = 0;

             for(Map.Entry<String, OSD> kvp :osdMap.entrySet())
             {
                 writer.write(singleQuotesNotationMarker);
                 writer.write(EscapeCharacter(kvp.getKey(), singleQuotesNotationMarker));
                 writer.write(singleQuotesNotationMarker);
                 writer.write(keyNotationDelimiter);
                 SerializeLLSDNotationElement(writer, kvp.getValue());
                 if (idx < lastIndex)
                     writer.write(kommaNotationDelimiter);

                 idx++;
             }
             writer.write(mapEndNotationMarker);
         }

         private static void SerializeLLSDNotationElementFormatted(StringWriter writer, String indent, OSD osd) throws OSDException
         {
             switch (osd.getType())
             {
                 case Unknown:
                     writer.write(undefNotationValue);
                     break;
                 case Boolean:
                     if (osd.asBoolean())
                         writer.write(trueNotationValueTwo);
                     else
                         writer.write(falseNotationValueTwo);
                     break;
                 case Integer:
                     writer.write(integerNotationMarker);
                     writer.write(osd.asString());
                     break;
                 case Real:
                     writer.write(realNotationMarker);
                     writer.write(osd.asString());
                     break;
                 case UUID:
                     writer.write(uuidNotationMarker);
                     writer.write(osd.asString());
                     break;
                 case String:
                     writer.write(singleQuotesNotationMarker);
                     writer.write(EscapeCharacter(osd.asString(), singleQuotesNotationMarker));
                     writer.write(singleQuotesNotationMarker);
                     break;
                 case Binary:
                     writer.write(binaryNotationMarker);
                     writer.write("64");
                     writer.write(doubleQuotesNotationMarker);
                     writer.write(osd.asString());
                     writer.write(doubleQuotesNotationMarker);
                     break;
                 case Date:
                     writer.write(dateNotationMarker);
                     writer.write(doubleQuotesNotationMarker);
                     writer.write(osd.asString());
                     writer.write(doubleQuotesNotationMarker);
                     break;
                 case URI:
                     writer.write(uriNotationMarker);
                     writer.write(doubleQuotesNotationMarker);
                     writer.write(EscapeCharacter(osd.asString(), doubleQuotesNotationMarker));
                     writer.write(doubleQuotesNotationMarker);
                     break;
                 case Array:
                     SerializeLLSDNotationArrayFormatted(writer, indent + baseIndent, (OSDArray)osd);
                     break;
                 case Map:
                     SerializeLLSDNotationMapFormatted(writer, indent + baseIndent, (OSDMap)osd);
                     break;
                 default:
                     throw new OSDException("Notation serialization: Not existing element discovered.");

             }
         }

         private static void SerializeLLSDNotationArrayFormatted(StringWriter writer, String intend, OSDArray osdArray) throws OSDException
         {
             writer.write("\n");
             writer.write(intend);
             writer.write(arrayBeginNotationMarker);
             int lastIndex = osdArray.count() - 1;

             for (int idx = 0; idx <= lastIndex; idx++)
             {
                 if (osdArray.get(idx).getType() != OSDType.Array && osdArray.get(idx).getType() != OSDType.Map)
                     writer.write("\n");
                 writer.write(intend + baseIndent);
                 SerializeLLSDNotationElementFormatted(writer, intend, osdArray.get(idx));
                 if (idx < lastIndex)
                 {
                     writer.write(kommaNotationDelimiter);
                 }
             }
             writer.write("\n");
             writer.write(intend);
             writer.write(arrayEndNotationMarker);
         }

         private static void SerializeLLSDNotationMapFormatted(StringWriter writer, String intend, OSDMap osdMap) throws OSDException
         {
             writer.write('\n');
             writer.write(intend);
             writer.write(mapBeginNotationMarker + "\n");
             int lastIndex = osdMap.count() - 1;
             int idx = 0;

             for(Map.Entry<String, OSD> kvp :osdMap.entrySet())
             {
                 writer.write(intend + baseIndent);
                 writer.write(singleQuotesNotationMarker);
                 writer.write(EscapeCharacter(kvp.getKey(), singleQuotesNotationMarker));
                 writer.write(singleQuotesNotationMarker);
                 writer.write(keyNotationDelimiter);
                 SerializeLLSDNotationElementFormatted(writer, intend, kvp.getValue());
                 if (idx < lastIndex)
                 {
                     writer.write("\n");
                     writer.write(intend + baseIndent);
                     writer.write(kommaNotationDelimiter + "\n");
                 }

                 idx++;
             }
             writer.write("\n");
             writer.write(intend);
             writer.write(mapEndNotationMarker);
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="reader"></param>
         /// <returns></returns>
         public static int PeekAndSkipWhitespace(StringReader reader) throws IOException
         {
             int character;
             reader.mark(2);
             while ((character = reader.read()) > 0)
             {
                 char c = (char)character;
                 if (c == ' ' || c == '\t' || c == '\n' || c == '\r')
                 {
                     reader.mark(2);
                     continue;
                 }
                 else
                     break;
             }
             reader.reset();
             return character;
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="reader"></param>
         /// <returns></returns>
         public static int ReadAndSkipWhitespace(StringReader reader) throws IOException
         {
             int character = PeekAndSkipWhitespace(reader);
             reader.read();
             return character;
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="reader"></param>
         /// <returns></returns>
         public static int GetLengthInBrackets(StringReader reader) throws OSDException, IOException
         {
             int character;
             StringBuilder s = new StringBuilder();
             if (((character = PeekAndSkipWhitespace(reader)) > 0) &&
                      ((char)character == sizeBeginNotationMarker))
             {
                 reader.read();
             }
             while (((character = reader.read()) > 0) &&
                     Character.isDigit((char)character) &&
                   ((char)character != sizeEndNotationMarker))
             {
                 s.append((char)character);
             }
             if (character < 0)
                 throw new OSDException("Notation LLSD parsing: Can't parse length value cause unexpected end of stream.");
             int length[] = new int[1];
             
             if (!Utils.tryParseInt(s.toString(), length))
                 throw new OSDException("Notation LLSD parsing: Can't parse length value.");

             return length[0];
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="reader"></param>
         /// <param name="delimiter"></param>
         /// <returns></returns>
         public static String GetStringDelimitedBy(StringReader reader, char delimiter) throws OSDException, IOException
         {
             int character;
             boolean foundEscape = false;
             StringBuilder s = new StringBuilder();
             while (((character = reader.read()) > 0) &&
                   (((char)character != delimiter) ||
                    ((char)character == delimiter && foundEscape)))
             {
                 if (foundEscape)
                 {
                     foundEscape = false;
                     switch ((char)character)
                     {
                     //TODO Do we need to handle Bell escape sequence
//                         case 'a':
//                             s.append('\a');
//                             break;
                         case 'b':
                             s.append('\b');
                             break;
                         case 'f':
                             s.append('\f');
                             break;
                         case 'n':
                             s.append('\n');
                             break;
                         case 'r':
                             s.append('\r');
                             break;
                         case 't':
                             s.append('\t');
                             break;
                             //TODO Do we need to handle Vertical Tab escape sequence
//                         case 'v':
//                             s.append('\v');
//                             break;
                         default:
                             s.append((char)character);
                             break;
                     }
                 }
                 else if ((char)character == '\\')
                     foundEscape = true;
                 else
                     s.append((char)character);

             }
             if (character < 0)
                 throw new OSDException("Notation LLSD parsing: Can't parse text because unexpected end of stream while expecting a '"
                                             + delimiter + "' character.");

             return s.toString();
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="reader"></param>
         /// <param name="buffer"></param>
         /// <param name="offset"></param>
         /// <returns></returns>
         public static int BufferCharactersEqual(StringReader reader, char[] buffer, int offset) throws IOException
         {

             int character;
             int lastIndex = buffer.length - 1;
             int crrIndex = offset;
             boolean charactersEqual = true;
             reader.mark(2);
             while ((character = reader.read()) > 0 &&
                     crrIndex <= lastIndex &&
                     charactersEqual)
             {
                 if (((char)character) != buffer[crrIndex])
                 {
                     charactersEqual = false;
                     break;
                 }
                 crrIndex++;
                 reader.mark(2);
             }
             reader.reset();
             return crrIndex;
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="s"></param>
         /// <param name="c"></param>
         /// <returns></returns>
         public static String UnescapeCharacter(String s, char c)
         {
             String oldOne = "\\" + c;
             String newOne = Character.toString(c);

             String sOne = s.replace("\\\\", "\\").replace(oldOne, newOne);
             return sOne;
         }

         /// <summary>
         /// 
         /// </summary>
         /// <param name="s"></param>
         /// <param name="c"></param>
         /// <returns></returns>
         public static String EscapeCharacter(String s, char c)
         {
             String oldOne = Character.toString(c);
             String newOne = "\\" + c;

             String sOne = s.replace("\\", "\\\\").replace(oldOne, newOne);
             return sOne;
         }
     }
