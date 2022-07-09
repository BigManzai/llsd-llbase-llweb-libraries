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
package com.ngt.jopenmetaverse.shared.sim.login;

import java.util.ArrayList;
import java.util.List;

import com.ngt.jopenmetaverse.shared.sim.GridClient;
import com.ngt.jopenmetaverse.shared.types.UUID;
import com.ngt.jopenmetaverse.shared.util.PlatformUtils;

public class LoginParams {

	 /// <summary>The URL of the Login Server</summary>
    public String URI;
    /// <summary>The number of milliseconds to wait before a login is considered
    /// failed due to timeout</summary>
    public int Timeout;
    /// <summary>The request method</summary>
    /// <remarks>login_to_simulator is currently the only supported method</remarks>
    public String MethodName;
    /// <summary>The Agents First name</summary>
    public String FirstName;
    /// <summary>The Agents Last name</summary>
    public String LastName;
    /// <summary>A md5 hashed password</summary>
    /// <remarks>plaintext password will be automatically hashed</remarks>
    public String Password;
    /// <summary>The agents starting location once logged in</summary>
    /// <remarks>Either "last", "home", or a string encoded URI 
    /// containing the simulator name and x/y/z coordinates e.g: uri:hooper&amp;128&amp;152&amp;17</remarks>
    public String Start;
    /// <summary>A string containing the client software channel information</summary>
    /// <example>Second Life Release</example>
    public String Channel;
    /// <summary>The client software version information</summary>
    /// <remarks>The official viewer uses: Second Life Release n.n.n.n 
    /// where n is replaced with the current version of the viewer</remarks>
    public String Version;
    /// <summary>A string containing the platform information the agent is running on</summary>
    public String Platform;
    /// <summary>A string hash of the network cards Mac Address</summary>
    public String MAC;
    /// <summary>Unknown or deprecated</summary>
    public String ViewerDigest;
    /// <summary>A string hash of the first disk drives ID used to identify this clients uniqueness</summary>
    public String ID0;
    /// <summary>A string containing the viewers Software, this is not directly sent to the login server but 
    /// instead is used to generate the Version string</summary>
    public String UserAgent;
    /// <summary>A string representing the software creator. This is not directly sent to the login server but
    /// is used by the library to generate the Version information</summary>
    public String Author;
    /// <summary>If true, this agent agrees to the Terms of Service of the grid its connecting to</summary>
    public boolean AgreeToTos;
    /// <summary>Unknown</summary>
    public boolean ReadCritical;
    /// <summary>An array of string sent to the login server to enable various options</summary>
    public String[] Options;

    /// <summary>A randomly generated ID to distinguish between login attempts. This value is only used
    /// internally in the library and is never sent over the wire</summary>
    public UUID LoginID;

    /// <summary>
    /// Default constuctor, initializes sane default values
    /// </summary>
    public LoginParams()
    {
        List<String> options = new ArrayList<String>();
        options.add("inventory-root");
        options.add("inventory-skeleton");
        options.add("inventory-lib-root");
        options.add("inventory-lib-owner");
        options.add("inventory-skel-lib");
        options.add("initial-outfit");
        options.add("gestures");
        options.add("event_categories");
        options.add("event_notifications");
        options.add("classified_categories");
        options.add("buddy-list");
        options.add("ui-config");
        options.add("tutorial_settings");
        options.add("login-flags");
        options.add("global-textures");
        options.add("adult_compliant");

        this.Options = options.toArray(new String[0]);
        this.MethodName = "login_to_simulator";
        this.Start = "last";
        this.Platform = PlatformUtils.getPlatformOS();
        this.MAC = PlatformUtils.getMACAddress();
        this.ViewerDigest = "";
        this.ID0 = PlatformUtils.getMACAddress();
        this.AgreeToTos = true;
        this.ReadCritical = true;
    }
    

    /// <summary>
    /// Instantiates new LoginParams object and fills in the values
    /// </summary>
    /// <param name="client">Instance of GridClient to read settings from</param>
    /// <param name="firstName">Login first name</param>
    /// <param name="lastName">Login last name</param>
    /// <param name="password">Password</param>
    /// <param name="channel">Login channnel (application name)</param>
    /// <param name="version">Client version, should be application name + version number</param>
    public LoginParams(GridClient client, String firstName, String lastName, String password, String channel, String version)
    {
    	this();
        this.URI = client.settings.LOGIN_SERVER;
        this.Timeout = client.settings.LOGIN_TIMEOUT;
        this.FirstName = firstName;
        this.LastName = lastName;
        this.Password = password;
        this.Channel = channel;
        this.Version = version;
    }

    /// <summary>
    /// Instantiates new LoginParams object and fills in the values
    /// </summary>
    /// <param name="client">Instance of GridClient to read settings from</param>
    /// <param name="firstName">Login first name</param>
    /// <param name="lastName">Login last name</param>
    /// <param name="password">Password</param>
    /// <param name="channel">Login channnel (application name)</param>
    /// <param name="version">Client version, should be application name + version number</param>
    /// <param name="loginURI">URI of the login server</param>
    public LoginParams(GridClient client, String firstName, String lastName, String password, String channel, String version, String loginURI)
    {
    	this(client, firstName, lastName, password, channel, version);
        this.URI = loginURI;
    }
}
