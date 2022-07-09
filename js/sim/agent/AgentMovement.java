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
package com.ngt.jopenmetaverse.shared.sim.agent;

import java.util.Observable;
import java.util.Timer;
import java.util.TimerTask;

import com.ngt.jopenmetaverse.shared.protocol.AgentUpdatePacket;
import com.ngt.jopenmetaverse.shared.protocol.SetAlwaysRunPacket;
import com.ngt.jopenmetaverse.shared.sim.AgentManager;
import com.ngt.jopenmetaverse.shared.sim.GridClient;
import com.ngt.jopenmetaverse.shared.sim.Settings;
import com.ngt.jopenmetaverse.shared.sim.Simulator;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.AgentFlags;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.AgentState;
import com.ngt.jopenmetaverse.shared.sim.AgentManager.ControlFlags;
import com.ngt.jopenmetaverse.shared.sim.events.EventObserver;
import com.ngt.jopenmetaverse.shared.sim.events.nm.DisconnectedEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginProgressEventArgs;
import com.ngt.jopenmetaverse.shared.sim.login.LoginStatus;
import com.ngt.jopenmetaverse.shared.types.Quaternion;
import com.ngt.jopenmetaverse.shared.types.Vector3;
import com.ngt.jopenmetaverse.shared.util.JLogger;

public class AgentMovement
{
	/// <summary>
	/// Camera controls for the agent, mostly a thin wrapper around
	/// CoordinateFrame. This class is only responsible for state
	/// tracking and math, it does not send any packets
	/// </summary>
	public static class AgentCamera
	{
		/// <summary></summary>
		public float Far;

		/// <summary>The camera is a local frame of reference inside of
		/// the larger grid space. This is where the math happens</summary>
		private CoordinateFrame Frame;

		/// <summary></summary>
		public Vector3 getPosition()
		{
			return Frame.getOrigin();
		}

		public void setPosition(Vector3 value)
		{
			Frame.setOrigin(value); 
		}

		/// <summary></summary>
		public Vector3 getAtAxis()
		{
			return Frame.getXAxis();
		}

		public void setAtAxis(Vector3 value)
		{
			Frame.setYAxis(value);
		}

		/// <summary></summary>
		public Vector3 getLeftAxis()
		{
			return Frame.getXAxis();
		}

		public void setLeftAxis(Vector3 value)
		{
			Frame.setXAxis(value);
		}

		/// <summary></summary>
		public Vector3 getUpAxis()
		{
			return Frame.getZAxis();
		}

		public void setUpAxis(Vector3 value)
		{
			Frame.setZAxis(value);
		}

		/// <summary>
		/// Default constructor
		/// </summary>
		public AgentCamera()
		{
			Frame = new CoordinateFrame(new Vector3(128f, 128f, 20f));
			Far = 128f;
		}

		public void Roll(float angle) throws Exception
		{
			Frame.Roll(angle);
		}

		public void Pitch(float angle) throws Exception
		{
			Frame.Pitch(angle);
		}

		public void Yaw(float angle) throws Exception
		{
			Frame.Yaw(angle);
		}

		public void LookDirection(Vector3 target)
		{
			Frame.LookDirection(target);
		}

		public void LookDirection(Vector3 target, Vector3 upDirection)
		{
			Frame.LookDirection(target, upDirection);
		}

		public void LookDirection(double heading)
		{
			Frame.LookDirection(heading);
		}

		public void LookAt(Vector3 position, Vector3 target)
		{
			Frame.LookAt(position, target);
		}

		public void LookAt(Vector3 position, Vector3 target, Vector3 upDirection)
		{
			Frame.LookAt(position, target, upDirection);
		}

		public void SetPositionOrientation(Vector3 position, float roll, float pitch, float yaw) throws Exception
		{
			Frame.origin = position;

			Frame.ResetAxes();

			Frame.Roll(roll);
			Frame.Pitch(pitch);
			Frame.Yaw(yaw);
		}
	}


	/// <summary> 
	/// Agent movement and camera control
	/// 
	/// Agent movement is controlled by setting specific <seealso cref="T:AgentManager.ControlFlags"/>
	/// After the control flags are set, An AgentUpdate is required to update the simulator of the specified flags
	/// This is most easily accomplished by setting one or more of the AgentMovement properties
	/// 
	/// Movement of an avatar is always based on a compass direction, for example AtPos will move the 
	/// agent from West to East or forward on the X Axis, AtNeg will of course move agent from 
	/// East to West or backward on the X Axis, LeftPos will be South to North or forward on the Y Axis
	/// The Z axis is Up, finer grained control of movements can be done using the Nudge properties
	/// </summary> 

	//region Properties

	/// <summary>Move agent positive along the X axis</summary>
	public boolean getAtPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_AT_POS);
	}

	public void setAtPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_AT_POS, value);
	}

	/// <summary>Move agent negative along the X axis</summary>
	public boolean getAtNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_AT_NEG);
	}

	public void setAtNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_AT_NEG, value);
	}

	/// <summary>Move agent positive along the Y axis</summary>
	public boolean getLeftPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LEFT_POS);
	}

	public void setLeftPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LEFT_POS, value);
	}
	/// <summary>Move agent negative along the Y axis</summary>
	public boolean getLeftNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LEFT_NEG);
	}

	public void setLeftNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LEFT_NEG, value);
	}
	/// <summary>Move agent positive along the Z axis</summary>
	public boolean getUpPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_UP_POS);
	}

	public void setUpPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_UP_POS, value);
	}
	/// <summary>Move agent negative along the Z axis</summary>
	public boolean getUpNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_UP_NEG);
	}

	public void setUpNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_UP_NEG, value);
	}
	/// <summary></summary>
	public boolean getPitchPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_PITCH_POS);
	}

	public void setPitchPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_PITCH_POS, value);
	}
	/// <summary></summary>
	public boolean getPitchNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_PITCH_NEG);
	}

	public void setPitchNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_PITCH_NEG, value);
	}
	/// <summary></summary>
	public boolean getYawPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_YAW_POS);
	}

	public void setYawPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_YAW_POS, value);
	}
	/// <summary></summary>
	public boolean getYawNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_YAW_NEG);
	}

	public void setYawNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_YAW_NEG, value);
	}
	/// <summary></summary>
	public boolean getFastAt()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FAST_AT);
	}

	public void setFastAt(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FAST_AT, value);
	}
	/// <summary></summary>
	public boolean getFastLeft()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FAST_LEFT);
	}

	public void setFastLeft(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FAST_LEFT, value);
	}
	/// <summary></summary>
	public boolean getFastUp()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FAST_UP);
	}

	public void setFastUp(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FAST_UP, value);
	}
	/// <summary>Causes simulator to make agent fly</summary>
	public boolean getFly()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FLY);
	}

	public void setFly(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FLY, value);
	}
	/// <summary>Stop movement</summary>
	public boolean getStop()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_STOP); 
	}

	public void setStop(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_STOP, value);
	}
	/// <summary>Finish animation</summary>
	public boolean getFinishAnim()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FINISH_ANIM);
	}

	public void setFinishAnim(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_FINISH_ANIM, value);
	}
	/// <summary>Stand up from a sit</summary>
	public boolean getStandUp()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_STAND_UP);
	}

	public void setStandUp(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_STAND_UP, value);
	}
	/// <summary>Tells simulator to sit agent on ground</summary>
	public boolean getSitOnGround()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_SIT_ON_GROUND);
	}

	public void setSitOnGround(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_SIT_ON_GROUND, value);
	}
	/// <summary>Place agent into mouselook mode</summary>
	public boolean getMouselook()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_MOUSELOOK);
	}

	public void setMouselook(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_MOUSELOOK, value);
	}
	/// <summary>Nudge agent positive along the X axis</summary>
	public boolean getNudgeAtPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_AT_POS);
	}

	public void setNudgeAtPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_AT_POS, value);
	}
	/// <summary>Nudge agent negative along the X axis</summary>
	public boolean getNudgeAtNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_AT_NEG); 
	}

	public void setNudgeAtNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_AT_NEG, value);
	}
	/// <summary>Nudge agent positive along the Y axis</summary>
	public boolean getNudgeLeftPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_LEFT_POS);
	}

	public void setNudgeLeftPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_LEFT_POS, value);
	}
	/// <summary>Nudge agent negative along the Y axis</summary>
	public boolean getNudgeLeftNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_LEFT_NEG);
	}

	public void setNudgeLeftNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_LEFT_NEG, value);
	}
	/// <summary>Nudge agent positive along the Z axis</summary>
	public boolean getNudgeUpPos()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_UP_POS);
	}

	public void setNudgeUpPos(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_UP_POS, value);
	}
	/// <summary>Nudge agent negative along the Z axis</summary>
	public boolean getNudgeUpNeg()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_UP_NEG);
	}

	public void setNudgeUpNeg(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_NUDGE_UP_NEG, value);
	}
	/// <summary></summary>
	public boolean getTurnLeft()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_TURN_LEFT);
	}

	public void setTurnLeft(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_TURN_LEFT, value);
	}
	/// <summary></summary>
	public boolean getTurnRight()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_TURN_RIGHT);
	}

	public void setTurnRight(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_TURN_RIGHT, value);
	}
	/// <summary>Tell simulator to mark agent as away</summary>
	public boolean getAway()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_AWAY);
	}

	public void setAway(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_AWAY, value);
	}
	/// <summary></summary>
	public boolean getLButtonDown()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LBUTTON_DOWN);
	}

	public void setLButtonDown(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LBUTTON_DOWN, value);
	}
	/// <summary></summary>
	public boolean getLButtonUp()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LBUTTON_UP);
	}

	public void setLButtonUp(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_LBUTTON_UP, value);
	}
	/// <summary></summary>
	public boolean getMLButtonDown()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_ML_LBUTTON_DOWN);
	}

	public void setMLButtonDown(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_ML_LBUTTON_DOWN, value);
	}
	/// <summary></summary>
	public boolean getMLButtonUp()
	{
		return GetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_ML_LBUTTON_UP);
	}

	public void setMLButtonUp(boolean value)
	{
		SetControlFlag(AgentManager.ControlFlags.AGENT_CONTROL_ML_LBUTTON_UP, value);
	}
	/// <summary>
	/// Returns "always run" value, or changes it by sending a SetAlwaysRunPacket
	/// </summary>
	public boolean getAlwaysRun()
	{
		return alwaysRun;
	}

	public void setAlwaysRun(boolean value)
	{
		alwaysRun = value;
		SetAlwaysRunPacket run = new SetAlwaysRunPacket();
		run.AgentData.AgentID = Client.self.getAgentID();
		run.AgentData.SessionID = Client.self.getSessionID();
		run.AgentData.AlwaysRun = alwaysRun;
		Client.network.SendPacket(run);
	}

	/// <summary>The current value of the agent control flags</summary>
	//uint
	public long getAgentControls()
	{
		return agentControls;
	}

	/// <summary>Gets or sets the interval in milliseconds at which
	/// AgentUpdate packets are sent to the current simulator. Setting
	/// this to a non-zero value will also enable the packet sending if
	/// it was previously off, and setting it to zero will disable</summary>
	public int getUpdateInterval()
	{
		return updateInterval;
	}

	public void setUpdateInterval(int value)
	{
		if (value > 0)
		{
			if (updateTimer != null)
			{
				//				updateTimer.Change(value, value);
				createUpdateTimer(value, value);
			}
			updateInterval = value;
		}
		else
		{
			if (updateTimer != null)
			{
				//				updateTimer.Change(Timeout.Infinite, Timeout.Infinite);
				//				createUpdateTimer();
				CleanupTimer();
				updateTimer = new Timer();
			}
			updateInterval = 0;
		}
	}

	/// <summary>Gets or sets whether AgentUpdate packets are sent to
	/// the current simulator</summary>
	public boolean getUpdateEnabled()
	{
		return (updateInterval != 0);
	}

	/// <summary>Reset movement controls every time we send an update</summary>
	public boolean getAutoResetControls()
	{
		return autoResetControls;
	}
	public void setAutoResetControls(boolean value)
	{
		autoResetControls = value;
	}

	//endregion Properties

	/// <summary>Agent camera controls</summary>
	public AgentCamera Camera;
	/// <summary>Currently only used for hiding your group title</summary>
	public AgentFlags Flags = AgentFlags.None;
	/// <summary>Action state of the avatar, which can currently be
	/// typing and editing</summary>
	public AgentState State = AgentState.None;
	/// <summary></summary>
	public Quaternion BodyRotation = Quaternion.Identity;
	/// <summary></summary>
	public Quaternion HeadRotation = Quaternion.Identity;

	//region Change tracking
	/// <summary></summary>
	private Quaternion LastBodyRotation;
	/// <summary></summary>
	private Quaternion LastHeadRotation;
	/// <summary></summary>
	private Vector3 LastCameraCenter;
	/// <summary></summary>
	private Vector3 LastCameraXAxis;
	/// <summary></summary>
	private Vector3 LastCameraYAxis;
	/// <summary></summary>
	private Vector3 LastCameraZAxis;
	/// <summary></summary>
	private float LastFar;
	//endregion Change tracking

	private boolean alwaysRun;
	private GridClient Client;
	//uint
	private long agentControls;
	private int duplicateCount;
	private AgentState lastState;
	/// <summary>Timer for sending AgentUpdate packets</summary>
	private Timer updateTimer;
	private int updateInterval;
	private boolean autoResetControls;

	private class LoginProgressObserver extends EventObserver<LoginProgressEventArgs>
	{
		@Override
		public void handleEvent(Observable o, LoginProgressEventArgs arg) {
			Network_OnConnected(o, arg);
		}
	} 

	private class DisconnectedObserver extends EventObserver<DisconnectedEventArgs>
	{
		@Override
		public void handleEvent(Observable o, DisconnectedEventArgs arg) {
			Network_OnDisconnected(o, arg);
		}
	} 

	/// <summary>Default constructor</summary>
	public AgentMovement(GridClient client)
	{
		Client = client;
		Camera = new AgentCamera();
		Client.network.RegisterLoginProgressCallback(new LoginProgressObserver());                
		Client.network.RegisterOnDisconnectedCallback(new DisconnectedObserver());
		updateInterval = Settings.DEFAULT_AGENT_UPDATE_INTERVAL;
	}

	private void CleanupTimer()
	{
		if (updateTimer != null)
		{
			updateTimer.cancel();
			updateTimer = null;
		}
	}

	private void Network_OnDisconnected(Object sender, DisconnectedEventArgs e)
	{
		CleanupTimer();
	}

	private void Network_OnConnected(Object sender, LoginProgressEventArgs e)
	{
		if (e.getStatus() == LoginStatus.Success)
		{
			//			CleanupTimer();
			////			updateTimer = new Timer(new TimerCallback(UpdateTimer_Elapsed), null, updateInterval, updateInterval);
			//			updateTimer = new Timer();
			//			updateTimer.schedule(new TimerTask()
			//			{ @Override
			//				public void run() {
			//					UpdateTimer_Elapsed(null);
			//				}
			//			}, updateInterval, updateInterval);

			createUpdateTimer(updateInterval, updateInterval);
		}
	}

	private void createUpdateTimer(int delay, int period)
	{
		CleanupTimer();
		updateTimer = new Timer();
		updateTimer.schedule(new TimerTask()
		{ @Override
			public void run() {
			UpdateTimer_Elapsed(null);
		}
		}, delay, period);
	}

	/// <summary>
	/// Send an AgentUpdate with the camera set at the current agent
	/// position and pointing towards the heading specified
	/// </summary>
	/// <param name="heading">Camera rotation in radians</param>
	/// <param name="reliable">Whether to send the AgentUpdate reliable
	/// or not</param>
	public void UpdateFromHeading(double heading, boolean reliable)
	{
		Camera.setPosition(Client.self.getSimPosition());
		Camera.LookDirection(heading);

		BodyRotation.Z = (float)Math.sin(heading / 2.0d);
		BodyRotation.W = (float)Math.cos(heading / 2.0d);
		HeadRotation = BodyRotation;

		SendUpdate(reliable);
	}

	/// <summary>
	/// Rotates the avatar body and camera toward a target position.
	/// This will also anchor the camera position on the avatar
	/// </summary>
	/// <param name="target">Region coordinates to turn toward</param>
	public boolean TurnToward(Vector3 target)
	{
		if (Client.settings.SEND_AGENT_UPDATES)
		{
			Quaternion parentRot = Quaternion.Identity;

			if (Client.self.getSittingOn() > 0)
			{
				if (!Client.network.getCurrentSim().ObjectsPrimitives.containsKey(Client.self.getSittingOn()))
				{
					JLogger.warn("Attempted TurnToward but parent prim is not in dictionary");
					return false;
				}
				else parentRot = Client.network.getCurrentSim().ObjectsPrimitives.get(Client.self.getSittingOn()).Rotation;
			}

			Quaternion between = Vector3.rotationBetween(Vector3.UnitX, Vector3.normalize(Vector3.subtract(target, Client.self.getSimPosition())));
			Quaternion rot = Quaternion.multiply(between, Quaternion.divide(Quaternion.Identity, parentRot));

			BodyRotation = rot;
			HeadRotation = rot;
			Camera.LookAt(Client.self.getSimPosition(), target);

			SendUpdate();

			return true;
		}
		else
		{
			JLogger.warn("Attempted TurnToward but agent updates are disabled");
			return false;
		}
	}

	/// <summary>
	/// Send new AgentUpdate packet to update our current camera 
	/// position and rotation
	/// </summary>
	public void SendUpdate()
	{
		SendUpdate(false, Client.network.getCurrentSim());
	}

	/// <summary>
	/// Send new AgentUpdate packet to update our current camera 
	/// position and rotation
	/// </summary>
	/// <param name="reliable">Whether to require server acknowledgement
	/// of this packet</param>
	public void SendUpdate(boolean reliable)
	{
		SendUpdate(reliable, Client.network.getCurrentSim());
	}

	/// <summary>
	/// Send new AgentUpdate packet to update our current camera 
	/// position and rotation
	/// </summary>
	/// <param name="reliable">Whether to require server acknowledgement
	/// of this packet</param>
	/// <param name="simulator">Simulator to send the update to</param>
	public void SendUpdate(boolean reliable, Simulator simulator)
	{
		// Since version 1.40.4 of the Linden simulator, sending this update
		// causes corruption of the agent position in the simulator
		if (simulator != null && (!simulator.AgentMovementComplete))
			return;

		Vector3 origin = Camera.getPosition();
		Vector3 xAxis = Camera.getLeftAxis();
		Vector3 yAxis = Camera.getAtAxis();
		Vector3 zAxis = Camera.getUpAxis();

		// Attempted to sort these in a rough order of how often they might change
		if (agentControls == 0 &&
				yAxis == LastCameraYAxis &&
				origin == LastCameraCenter &&
				State == lastState &&
				HeadRotation == LastHeadRotation &&
				BodyRotation == LastBodyRotation &&
				xAxis == LastCameraXAxis &&
				Camera.Far == LastFar &&
				zAxis == LastCameraZAxis)
		{
			++duplicateCount;
		}
		else
		{
			duplicateCount = 0;
		}

		if (Client.settings.DISABLE_AGENT_UPDATE_DUPLICATE_CHECK || duplicateCount < 10)
		{
			// Store the current state to do duplicate checking
			LastHeadRotation = HeadRotation;
			LastBodyRotation = BodyRotation;
			LastCameraYAxis = yAxis;
			LastCameraCenter = origin;
			LastCameraXAxis = xAxis;
			LastCameraZAxis = zAxis;
			LastFar = Camera.Far;
			lastState = State;

			// Build the AgentUpdate packet and send it
			AgentUpdatePacket update = new AgentUpdatePacket();
			update.header.Reliable = reliable;

			update.AgentData.AgentID = Client.self.getAgentID();
			update.AgentData.SessionID = Client.self.getSessionID();
			update.AgentData.HeadRotation = HeadRotation;
			update.AgentData.BodyRotation = BodyRotation;
			update.AgentData.CameraAtAxis = yAxis;
			update.AgentData.CameraCenter = origin;
			update.AgentData.CameraLeftAxis = xAxis;
			update.AgentData.CameraUpAxis = zAxis;
			update.AgentData.Far = Camera.Far;
			update.AgentData.State = (byte)State.getIndex();
			update.AgentData.ControlFlags = agentControls;
			update.AgentData.Flags = (byte)Flags.getIndex();

			Client.network.SendPacket(update, simulator);

			if (autoResetControls) {
				ResetControlFlags();
			}
		}
	}

	/// <summary>
	/// Builds an AgentUpdate packet entirely from parameters. This
	/// will not touch the state of Self.Movement or
	/// Self.Movement.Camera in any way
	/// </summary>
	/// <param name="controlFlags"></param>
	/// <param name="position"></param>
	/// <param name="forwardAxis"></param>
	/// <param name="leftAxis"></param>
	/// <param name="upAxis"></param>
	/// <param name="bodyRotation"></param>
	/// <param name="headRotation"></param>
	/// <param name="farClip"></param>
	/// <param name="reliable"></param>
	/// <param name="flags"></param>
	/// <param name="state"></param>
	public void SendManualUpdate(AgentManager.ControlFlags controlFlags, Vector3 position, Vector3 forwardAxis,
			Vector3 leftAxis, Vector3 upAxis, Quaternion bodyRotation, Quaternion headRotation, float farClip,
			AgentFlags flags, AgentState state, boolean reliable)
	{
		// Since version 1.40.4 of the Linden simulator, sending this update
		// causes corruption of the agent position in the simulator
		if (Client.network.getCurrentSim() != null && (!Client.network.getCurrentSim().getHandshakeComplete()))
			return;

		AgentUpdatePacket update = new AgentUpdatePacket();

		update.AgentData.AgentID = Client.self.getAgentID();
		update.AgentData.SessionID = Client.self.getSessionID();
		update.AgentData.BodyRotation = bodyRotation;
		update.AgentData.HeadRotation = headRotation;
		update.AgentData.CameraCenter = position;
		update.AgentData.CameraAtAxis = forwardAxis;
		update.AgentData.CameraLeftAxis = leftAxis;
		update.AgentData.CameraUpAxis = upAxis;
		update.AgentData.Far = farClip;
		update.AgentData.ControlFlags = (long)controlFlags.getIndex();
		update.AgentData.Flags = (byte)flags.getIndex();
		update.AgentData.State = (byte)state.getIndex();

		update.header.Reliable = reliable;

		Client.network.SendPacket(update);
	}

	private boolean GetControlFlag(ControlFlags flag)
	{
		return (agentControls & (long)flag.getIndex()) != 0;
	}

	private void SetControlFlag(ControlFlags flag, boolean value)
	{
		if (value) agentControls |= flag.getIndex();
		else agentControls &= ~(flag.getIndex());
	}

	private void ResetControlFlags()
	{
		// Reset all of the flags except for persistent settings like
		// away, fly, mouselook, and crouching
		agentControls &=
				(long)(ControlFlags.AGENT_CONTROL_AWAY.getIndex() |
						ControlFlags.AGENT_CONTROL_FLY.getIndex() |
						ControlFlags.AGENT_CONTROL_MOUSELOOK.getIndex() |
						ControlFlags.AGENT_CONTROL_UP_NEG.getIndex());
	}

	private void UpdateTimer_Elapsed(Object obj)
	{
		if (Client.network.getConnected() && Client.settings.SEND_AGENT_UPDATES)
		{
			//Send an AgentUpdate packet
			SendUpdate(false, Client.network.getCurrentSim());
		}
	}
}
