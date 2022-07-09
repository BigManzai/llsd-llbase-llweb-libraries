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
package com.ngt.jopenmetaverse.shared.protocol;

import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;
import com.ngt.jopenmetaverse.shared.util.Utils;

    /// <summary>
    /// A Name Value pair with additional settings, used in the protocol
    /// primarily to transmit avatar names and active group in Object packets
    /// </summary>
    public class NameValue
    {
        //region Enums

        /// <summary>Type of the value</summary>
        public enum ValueType
        {
            /// <summary>Unknown</summary>
            Unknown (-1),
            /// <summary>String value</summary>
            String (0),
            /// <summary></summary>
            F32 (1),
            /// <summary></summary>
            S32 (2),
            /// <summary></summary>
            VEC3 (3),
            /// <summary></summary>
            U32 (4),
            /// <summary>Deprecated</summary>
            CAMERA (5),
            /// <summary>String value, but designated as an asset</summary>
            Asset (6),
            /// <summary></summary>
            U64 (7);
            
      		private int index;
      		ValueType(int index)
    		{
    			this.index = index;
    		}     

    		public int getIndex()
    		{
    			return index;
    		}
    		
    		private static final Map<Integer,ValueType> lookup  = new HashMap<Integer,ValueType>();

    		static {
    			for(ValueType s : EnumSet.allOf(ValueType.class))
    				lookup.put(s.getIndex(), s);
    		}

    		public static ValueType get(int index)
    		{
    			return lookup.get(index);
    		}
        }

        /// <summary>
        /// 
        /// </summary>
        public enum ClassType
        {
            /// <summary></summary>
            Unknown (-1),
            /// <summary></summary>
            ReadOnly (0),
            /// <summary></summary>
            ReadWrite (1),
            /// <summary></summary>
            Callback (2);
            
      		private int index;
      		ClassType(int index)
    		{
    			this.index = index;
    		}     

    		public int getIndex()
    		{
    			return index;
    		}
    		
    		private static final Map<Integer,ClassType> lookup  = new HashMap<Integer,ClassType>();

    		static {
    			for(ClassType s : EnumSet.allOf(ClassType.class))
    				lookup.put(s.getIndex(), s);
    		}

    		public static ClassType get(int index)
    		{
    			return lookup.get(index);
    		}
            
        }

        /// <summary>
        /// 
        /// </summary>
        public enum SendtoType
        {
            /// <summary></summary>
            Unknown (-1),
            /// <summary></summary>
            Sim (0),
            /// <summary></summary>
            DataSim (1),
            /// <summary></summary>
            SimViewer (2),
            /// <summary></summary>
            DataSimViewer (3);
            
      		private int index;
      		SendtoType(int index)
    		{
    			this.index = index;
    		}     

    		public int getIndex()
    		{
    			return index;
    		}
    		
    		private static final Map<Integer,SendtoType> lookup  = new HashMap<Integer,SendtoType>();

    		static {
    			for(SendtoType s : EnumSet.allOf(SendtoType.class))
    				lookup.put(s.getIndex(), s);
    		}

    		public static SendtoType get(int index)
    		{
    			return lookup.get(index);
    		}
        }

        //endregion Enums


        /// <summary></summary>
        public String Name;
        /// <summary></summary>
        public ValueType Type;
        /// <summary></summary>
        public ClassType Class;
        /// <summary></summary>
        public SendtoType Sendto;
        /// <summary></summary>
        public Object Value;


        private static final String[] TypeStrings = new String[]
        {
            "STRING",
            "F32",
            "S32",
            "VEC3",
            "U32",
            "ASSET",
            "U64"
        };
        private static final String[] ClassStrings = new String[]
        {
            "R",    // Read-only
            "RW",   // Read-write
            "CB"    // Callback
        };
        private static final String[] SendtoStrings = new String[]
        {
            "S",    // Sim
            "DS",   // Data Sim
            "SV",   // Sim Viewer
            "DSV"   // Data Sim Viewer
        };
        private static final char[] Separators = new char[]
        {
            ' ',
            '\n',
            '\t',
            '\r'
        };

        public NameValue()
        {
        	
        }
        
        /// <summary>
        /// Constructor that takes all the fields as parameters
        /// </summary>
        /// <param name="name"></param>
        /// <param name="valueType"></param>
        /// <param name="classType"></param>
        /// <param name="sendtoType"></param>
        /// <param name="value"></param>
        public NameValue(String name, ValueType valueType, ClassType classType, SendtoType sendtoType, Object value)
        {
            Name = name;
            Type = valueType;
            Class = classType;
            Sendto = sendtoType;
            Value = value;
        }

        /// <summary>
        /// Constructor that takes a single line from a NameValue field
        /// </summary>
        /// <param name="data"></param>
        public NameValue(String data)
        {
            int i;

            // Name
            
            i = Utils.indexOfAny(data, Separators);
            if (i < 1)
            {
                Name = "";
                Type = ValueType.Unknown;
                Class = ClassType.Unknown;
                Sendto = SendtoType.Unknown;
                Value = null;
                JLogger.debug("Unable to get proper index");
                return;
            }
            Name = data.substring(0, i);
            data = data.substring(i + 1);
            
            JLogger.debug("Name=" + Name + " : data=" + data);
            
            // Type
            i = Utils.indexOfAny(data, Separators);
            if (i > 0)
            {
                Type = GetValueType(data.substring(0, i));
                data = data.substring(i + 1);

                // Class
                i = Utils.indexOfAny(data,Separators);
                if (i > 0)
                {
                    Class = GetClassType(data.substring(0, i));
                    data = data.substring(i + 1);

                    // Sendto
                    i = Utils.indexOfAny(data,Separators);
                    if (i > 0)
                    {
                        Sendto = GetSendtoType(data.substring(0, 1));
                        data = data.substring(i + 1);
                    }
                }
            }

            // Value
            //FIXME why the type,  class and sendto are hardcoded here ?
            Type = ValueType.String;
            Class = ClassType.ReadOnly;
            Sendto = SendtoType.Sim;
            Value = null;
            SetValue(data);
        }

        public static String NameValuesToString(NameValue[] values)
        {
            if (values == null || values.length == 0)
                return "";

            StringBuilder output = new StringBuilder();

            for (int i = 0; i < values.length; i++)
            {
                NameValue value = values[i];

                if (value.Value != null)
                {
                    String newLine = (i < values.length - 1) ? "\n" : "";
                    output.append(String.format("%s %s %s %s %s%s", value.Name, TypeStrings[(int)value.Type.getIndex()],
                        ClassStrings[(int)value.Class.getIndex()], SendtoStrings[(int)value.Sendto.getIndex()], value.Value.toString(), newLine));
                }
            }

            return output.toString();
        }

        private void SetValue(String value)
        {
            switch (Type)
            {
                case Asset:
                case String:
                    Value = value;
                    break;
                case F32:
                {
                    float[] temp = new float[1];
                    Utils.tryParseFloat(value, temp);
                    Value = temp[0];
                    break;
                }
                case S32:
                {
                    int[] temp = new int[1];
                    Utils.tryParseInt(value, temp);
                    Value = temp[0];
                    break;
                }
                case U32:
                {
                    long[] temp = new long[1];
                    Utils.tryParseLong(value, temp);
                    Value = temp[0];
                    break;
                }
                case U64:
                {
                	//TODO CHeck if require Big Integer 
                    long[] temp = new long[1];
                    Utils.tryParseLong(value, temp);
                    Value = temp[0];
                    break;
                }
                case VEC3:
                {
                    Vector3[] temp = new Vector3[0];
                    Vector3.TryParse(value, temp);
                    Value = temp[0];
                    break;
                }
                default:
                    Value = null;
                    break;
            }
        }

        private static ValueType GetValueType(String value)
        {
            ValueType type = ValueType.Unknown;

            for (int i = 0; i < TypeStrings.length; i++)
            {
                if (value.equals(TypeStrings[i]))
                {
                    type = ValueType.get(i);
                    break;
                }
            }

            if (type == ValueType.Unknown)
                type = ValueType.String;

            return type;
        }

        private static ClassType GetClassType(String value)
        {
            ClassType type = ClassType.Unknown;

            for (int i = 0; i < ClassStrings.length; i++)
            {
                if (value.equals(ClassStrings[i]))
                {
                    type = ClassType.get(i);
                    break;
                }
            }

            if (type == ClassType.Unknown)
                type = ClassType.ReadOnly;

            return type;
        }

        private static SendtoType GetSendtoType(String value)
        {
            SendtoType type = SendtoType.Unknown;

            for (int i = 0; i < SendtoStrings.length; i++)
            {
                if (value.equals(SendtoStrings[i]))
                {
                    type = SendtoType.get(i);
                    break;
                }
            }

            if (type == SendtoType.Unknown)
                type = SendtoType.Sim;

            return type;
        }
    }
