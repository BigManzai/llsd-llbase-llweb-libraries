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
package com.ngt.jopenmetaverse.shared.structureddata;

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.TimeZone;

import org.joda.time.DateTime;
import org.joda.time.format.DateTimeFormatter;
import org.joda.time.format.ISODateTimeFormat;

import com.ngt.jopenmetaverse.shared.util.Utils;

/// <summary>
/// 
/// </summary>
public final class OSDDate extends OSD
{
  private Date value;

  public OSDType getType()
  {
  	return type;
  }
  
  public OSDDate()
  {
	  super();
	  type =  OSDType.Date;
  }
  
  public OSDDate(Date value)
  {
	  this();
      this.value = value;
  }

  public  String asString()
  {
//      String format;
//      Calendar calendar = Calendar.getInstance();
//      calendar.setTime(value);
//      
//      if (calendar.get(Calendar.MILLISECOND) > 0)
//          format = "yyyy-MM-ddTHH:mm:ss.ffZ";
//      else
//          format = "yyyy-MM-ddTHH:mm:ssZ";
//      SimpleDateFormat formatter = new SimpleDateFormat(format);
      DateTimeFormatter formatter = ISODateTimeFormat.dateTime().withZoneUTC();
//      formatter.setTimeZone(TimeZone.getTimeZone("UTC"));
      return formatter.print(new DateTime(value));
  }

  public  int asInteger()
  {
      return (int) (value.getTime()/1000);
  }

  public  long asLong()
  {
      return (long) value.getTime()/1000;
  }

  public  byte[] asBinary()
  {
	  //TODO Why we need to return Little endian form of double byte array
      return Utils.doubleToBytesLit(1.0*(value.getTime()/1000));
  }

  public  Date asDate() { return value; }
  public  String toString() { return asString(); }
}