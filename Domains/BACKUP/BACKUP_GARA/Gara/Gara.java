import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.clay.*;

import CSAI.unipa.clay.*;
import CSAI.unipa.abstractrobot.*;

public class Gara extends ControlSystemMFN150Explore
	{
	public final static boolean DEBUG = true;
	private NodeVec2            turret_configuration;
	private NodeVec2            steering_configuration;
        private NodeBoolean         sonar_configuration;
	private NodeDouble          gripper_fingers_configuration;
        private NodeBoolean         laser_configuration;
	private double              mtggain = 1.0;     // move to goal gain 
	private double              oldmtggain = 1.0;
	private double              avoidgain = 0.7; // avoid obstacle 
	private double              oldavoidgain = 0.8; 
	private double              noisegain = 0.2; // noise
	private double              oldnoisegain = 0.2;
	private NodeInt             state_monitor;
	private i_FSA_ba            STATE_MACHINE;
        private NodeVec2Array       PS_SONAR;
        private NodeVec2Array       PS_LASER;


	/**
	 * Configure the control system using Clay.
	 */
	public void configure()
		{

		//======
		// Set some initial hardware configurations.
		//======

		abstract_robot.setObstacleMaxRange(3.0); 

		abstract_robot.setBaseSpeed(0.4*abstract_robot.MAX_TRANSLATION);

		//abbassamento dell'artiglio
		abstract_robot.setGripperHeight(-1,0);   

		//modalit� trigger
		abstract_robot.setGripperFingers(-1,-1); 
                
                abstract_robot.setSonarPrecision(SonarObjectSensor.MEDIUM);
                

		//======
		// perceptual schemas
		//======
		//--- robot's global position
		NodeVec2
		PS_GLOBAL_POS = new v_GlobalPosition_r(abstract_robot);

		//--- obstacles
		NodeVec2Array // the sonar readings
		PS_OBS = new va_Obstacles_r(abstract_robot);
                
                PS_SONAR = new va_SonarSensed_r(abstract_robot);
                
                PS_LASER = new va_LaserSensed_r(abstract_robot);
                
                NodeVec2Array
                PS_TEMP = new va_Merge_vava(PS_LASER, PS_OBS);
                
                NodeVec2Array
                PS_ALL_OBS = new va_Merge_vava(PS_TEMP, PS_SONAR);
                
		//--- homebase1 
		NodeVec2      // the goal location
		PS_HOMEBASE1_GLOBAL = new v_FixedPoint_(8.5,-8.5);
		NodeVec2      // make it egocentric
		PS_HOMEBASE1 = new v_GlobalToEgo_rv(abstract_robot,
				PS_HOMEBASE1_GLOBAL);

		//--- homebase2
		NodeVec2      // the goal location
		PS_HOMEBASE2_GLOBAL = new v_FixedPoint_(-9.2,9.2);
                NodeVec2      // make it egocentric
		PS_HOMEBASE2 = new v_GlobalToEgo_rv(abstract_robot,
				PS_HOMEBASE2_GLOBAL);
                
                //--- homebase3
		NodeVec2      // the goal location
		PS_HOMEBASE3_GLOBAL = new v_FixedPoint_(8.5,7.7);
                NodeVec2      // make it egocentric
		PS_HOMEBASE3 = new v_GlobalToEgo_rv(abstract_robot,
				PS_HOMEBASE3_GLOBAL);
                
                //--- homebase4
		NodeVec2      // the goal location
		PS_HOMEBASE4_GLOBAL = new v_FixedPoint_(2.7,-4.5);
                NodeVec2      // make it egocentric
		PS_HOMEBASE4 = new v_GlobalToEgo_rv(abstract_robot,
				PS_HOMEBASE4_GLOBAL);

                //--- homebase5
		NodeVec2      // the goal location
		PS_HOMEBASE5_GLOBAL = new v_FixedPoint_(-3.0,6.5);
                NodeVec2      // make it egocentric
		PS_HOMEBASE5 = new v_GlobalToEgo_rv(abstract_robot,
				PS_HOMEBASE5_GLOBAL);


		//--- targets of visual class 0
		NodeVec2Array 
		PS_TARGETS0_EGO = 
			new va_VisualObjects_r(0,abstract_robot); 
		// array di bandiere classe visuale 0 ossia blu
		NodeVec2Array 
		PS_TARGETS0_GLOBAL = 
			new va_Add_vav(PS_TARGETS0_EGO, PS_GLOBAL_POS);


		//--- type of object in the gripper
		NodeInt
		PS_IN_GRIPPER = new i_InGripper_r(abstract_robot); 
		// ogg. Nell'artiglio

		//======
		// Perceptual Features
		//======


		// close to homebase1 
		NodeBoolean
		PF_CLOSE_TO_HOMEBASE1 = new b_Close_vv(0.4, PS_GLOBAL_POS,
			PS_HOMEBASE1_GLOBAL);

		// close to homebase2 
		NodeBoolean
		PF_CLOSE_TO_HOMEBASE2 = new b_Close_vv(0.4, PS_GLOBAL_POS,
			PS_HOMEBASE2_GLOBAL);
		
		// close to homebase3
		NodeBoolean
		PF_CLOSE_TO_HOMEBASE3 = new b_Close_vv(0.4, PS_GLOBAL_POS,
			PS_HOMEBASE3_GLOBAL);
		
		// close to homebase4
		NodeBoolean
		PF_CLOSE_TO_HOMEBASE4 = new b_Close_vv(0.4, PS_GLOBAL_POS,
			PS_HOMEBASE4_GLOBAL);
		
		// close to homebase5 
		NodeBoolean
		PF_CLOSE_TO_HOMEBASE5 = new b_Close_vv(0.4, PS_GLOBAL_POS,
			PS_HOMEBASE5_GLOBAL);



		// is something in the gripper?
		NodeBoolean
		//PF_TARGET0_IN_GRIPPER = new b_Equal_i(0,PS_IN_GRIPPER); per prendere una bandiera e non muoversi perche' non e' quella giusta 6 valore fittizzio non presente nel dsc
                PF_TARGET0_IN_GRIPPER = new b_Equal_i(0,PS_IN_GRIPPER);
		// se l'oggetto e' blu ok  



		//======
		// motor schemas
		//======
		// avoid obstacles
		NodeVec2
		MS_AVOID_OBSTACLES = new v_AvoidSonar_va(3.0, abstract_robot.RADIUS + 0.1,
			PS_ALL_OBS, PS_GLOBAL_POS);

		// move to goal1
		NodeVec2
		MS_MOVE_TO_HOMEBASE1 = new v_LinearAttraction_v(0.4,0.0,PS_HOMEBASE1);
		
		// move to goal2
		NodeVec2
		MS_MOVE_TO_HOMEBASE2 = new v_LinearAttraction_v(0.4,0.0,PS_HOMEBASE2);
		
		// move to goal3
		NodeVec2
		MS_MOVE_TO_HOMEBASE3 = new v_LinearAttraction_v(0.4,0.0,PS_HOMEBASE3); 
		
		// move to goal4
		NodeVec2
		MS_MOVE_TO_HOMEBASE4 = new v_LinearAttraction_v(0.4,0.0,PS_HOMEBASE4);

		// move to goal5
		NodeVec2
		MS_MOVE_TO_HOMEBASE5 = new v_LinearAttraction_v(0.4,0.0,PS_HOMEBASE5);

		// noise vector
		NodeVec2
		MS_NOISE_VECTOR = new v_Noise_(10,5);


		//======
		// AS_GO_HOME1
		//======
		v_StaticWeightedSum_va 
		AS_GO_HOME1 = new v_StaticWeightedSum_va();

		AS_GO_HOME1.weights[0]  = avoidgain;
		AS_GO_HOME1.embedded[0] = MS_AVOID_OBSTACLES;

		AS_GO_HOME1.weights[1]  = noisegain;
		AS_GO_HOME1.embedded[1] = MS_NOISE_VECTOR;

		AS_GO_HOME1.weights[2]  = mtggain;
		AS_GO_HOME1.embedded[2] = MS_MOVE_TO_HOMEBASE1;


		//======
		// AS_GO_HOME2
		//======
		v_StaticWeightedSum_va 
		AS_GO_HOME2 = new v_StaticWeightedSum_va();

		AS_GO_HOME2.weights[0]  = avoidgain;
		AS_GO_HOME2.embedded[0] = MS_AVOID_OBSTACLES;

		AS_GO_HOME2.weights[1]  = noisegain;
		AS_GO_HOME2.embedded[1] = MS_NOISE_VECTOR;

		AS_GO_HOME2.weights[2]  = mtggain;
		AS_GO_HOME2.embedded[2] = MS_MOVE_TO_HOMEBASE2;


		//======
		// AS_GO_HOME3
		//======
		v_StaticWeightedSum_va 
		AS_GO_HOME3 = new v_StaticWeightedSum_va();

		AS_GO_HOME3.weights[0]  = avoidgain;
		AS_GO_HOME3.embedded[0] = MS_AVOID_OBSTACLES;

		AS_GO_HOME3.weights[1]  = noisegain;
		AS_GO_HOME3.embedded[1] = MS_NOISE_VECTOR;

		AS_GO_HOME3.weights[2]  = mtggain;
		AS_GO_HOME3.embedded[2] = MS_MOVE_TO_HOMEBASE3;


		//======
		// AS_GO_HOME4
		//======
		v_StaticWeightedSum_va 
		AS_GO_HOME4 = new v_StaticWeightedSum_va();

		AS_GO_HOME4.weights[0]  = avoidgain;
		AS_GO_HOME4.embedded[0] = MS_AVOID_OBSTACLES;

		AS_GO_HOME4.weights[1]  = noisegain;
		AS_GO_HOME4.embedded[1] = MS_NOISE_VECTOR;

		AS_GO_HOME4.weights[2]  = mtggain;
		AS_GO_HOME4.embedded[2] = MS_MOVE_TO_HOMEBASE4;


		//======
		// AS_GO_HOME5
		//======
		v_StaticWeightedSum_va 
		AS_GO_HOME5 = new v_StaticWeightedSum_va();

		AS_GO_HOME5.weights[0]  = avoidgain;
		AS_GO_HOME5.embedded[0] = MS_AVOID_OBSTACLES;

		AS_GO_HOME5.weights[1]  = noisegain;
		AS_GO_HOME5.embedded[1] = MS_NOISE_VECTOR;

		AS_GO_HOME5.weights[2]  = mtggain;
		AS_GO_HOME5.embedded[2] = MS_MOVE_TO_HOMEBASE5;


		//======
		// STATE_MACHINE
		//======
		STATE_MACHINE = new i_FSA_ba();

		STATE_MACHINE.state = 0;
		
		// STATE 0 GO_TO_HOMEBASE1
		STATE_MACHINE.triggers[0][0]  = PF_CLOSE_TO_HOMEBASE1;
		STATE_MACHINE.follow_on[0][0] = 1; // transition to GO_TO_HOME2

		// STATE 1 GO_TO_HOMEBASE2
		STATE_MACHINE.triggers[1][0]  = PF_CLOSE_TO_HOMEBASE2;
		STATE_MACHINE.follow_on[1][0] = 2; // transition to GO_TO_HOME3

		// STATE 2 GO_TO_HOMEBASE3
		STATE_MACHINE.triggers[2][0]  = PF_CLOSE_TO_HOMEBASE3;
		STATE_MACHINE.follow_on[2][0] = 3; // transition to GO_TO_HOME4
		
		// STATE 3 GO_TO_HOMEBASE4
		STATE_MACHINE.triggers[3][0]  = PF_CLOSE_TO_HOMEBASE4;
		STATE_MACHINE.follow_on[3][0] = 4; // transition to GO_TO_HOME5
		
		//--ERROREEEEEEE STATE 4 GO_TO_HOMEBASE5
		//STATE_MACHINE.triggers[4][0]  = PF_CLOSE_TO_HOMEBASE5;
		//STATE_MACHINE.follow_on[4][0] = 5; // transition to GO_TO_HOME5


		state_monitor = STATE_MACHINE;


		//======
		// STEERING
		//======
		v_Select_vai
		STEERING = new v_Select_vai((NodeInt)STATE_MACHINE);

		STEERING.embedded[0] = AS_GO_HOME1;
		STEERING.embedded[1] = AS_GO_HOME2;
		STEERING.embedded[2] = AS_GO_HOME3;
		STEERING.embedded[3] = AS_GO_HOME4;
		STEERING.embedded[4] = AS_GO_HOME5;


		//======
		// TURRET
		//======
		v_Select_vai
		TURRET = new v_Select_vai((NodeInt)STATE_MACHINE);

		TURRET.embedded[0] = AS_GO_HOME1;
		TURRET.embedded[1] = AS_GO_HOME2;
		TURRET.embedded[2] = AS_GO_HOME3;
		TURRET.embedded[3] = AS_GO_HOME4;
		TURRET.embedded[4] = AS_GO_HOME5;

		//======
		// GRIPPER_FINGERS
		//======
		d_Select_i
		GRIPPER_FINGERS = new d_Select_i(STATE_MACHINE);

		GRIPPER_FINGERS.embedded[0] = 1;  // open in GO_TO_HOMEBASE1
		GRIPPER_FINGERS.embedded[1] = -1; // trigger in GO_TO_FLAG
		GRIPPER_FINGERS.embedded[2] = 0;  // closed in GO_TO_HOMEBASE2
                
                
                //======
		// SONAR_CONFIGURATION
		//======
                d_SonarControl_ir 
                SONAR_CONFIGURATION = new d_SonarControl_ir(STATE_MACHINE, abstract_robot);
                
                SONAR_CONFIGURATION.sonarActived[0] = new int[]{0,1,3,5,7,9,11,13,15};
                SONAR_CONFIGURATION.sonarPrecision[0] = SonarObjectSensor.HIGH;
                SONAR_CONFIGURATION.sonarActived[1] = new int[]{1,2,3};
                SONAR_CONFIGURATION.sonarPrecision[1] = SonarObjectSensor.MEDIUM;
                SONAR_CONFIGURATION.sonarActived[2] = new int[]{2,4,10,12,13,14,15};
                SONAR_CONFIGURATION.sonarPrecision[2] = SonarObjectSensor.MEDIUM;
				SONAR_CONFIGURATION.sonarActived[3] = new int[]{14,15};
                SONAR_CONFIGURATION.sonarPrecision[3] = SonarObjectSensor.MEDIUM;
                SONAR_CONFIGURATION.sonarActived[4] = new int[]{};
                SONAR_CONFIGURATION.sonarPrecision[4] = SonarObjectSensor.MEDIUM;
                
                //======
		// LASER_CONFIGURATION
		//======
                b_Select_ir
                LASER_CONFIGURATION = new b_Select_ir(STATE_MACHINE, abstract_robot);
                
                LASER_CONFIGURATION.embedded[0] = false;
                LASER_CONFIGURATION.embedded[1] = false;
                LASER_CONFIGURATION.embedded[2] = true;
				LASER_CONFIGURATION.embedded[3] = true;
				LASER_CONFIGURATION.embedded[4] = false;
                
                laser_configuration = LASER_CONFIGURATION;
                sonar_configuration = SONAR_CONFIGURATION;
		turret_configuration = TURRET;
		steering_configuration = STEERING;
		gripper_fingers_configuration = GRIPPER_FINGERS;
		}
		
	/**
	 * Called every timestep to allow the control system to
	 * run.
	 */
	public int takeStep()
            {
		Vec2	result;
		double	dresult;
                boolean bresult;
		long	curr_time = abstract_robot.getTime();

		// STEER
		result = steering_configuration.Value(curr_time);
		abstract_robot.setSteerHeading(curr_time, result.t);
		abstract_robot.setSpeed(curr_time, result.r);

		// TURRET
		result = turret_configuration.Value(curr_time);
		abstract_robot.setTurretHeading(curr_time, result.t);

		//SONAR
                bresult = sonar_configuration.Value(curr_time);
                if(bresult)
                    System.out.println("Cambio le impostazioni del sonar");
                
                //LASER
                bresult = laser_configuration.Value(curr_time);
                if(bresult)
                    System.out.println("Cambio le impostazioni del laser");

                // STATE DISPLAY  aggiungere gli altri nomi agli stati
		int state=STATE_MACHINE.Value(curr_time);

                if(state==0) 
                    abstract_robot.setDisplayString("Vado Home 1");
				if(state==1)
                    abstract_robot.setDisplayString("Vado Home 2");
                if(state==2)
                    abstract_robot.setDisplayString("Vado Home 3");	
				if(state==3)
                    abstract_robot.setDisplayString("Vado Home 4");	
				if(state==4)
                    abstract_robot.setDisplayString("Vado Home 5");			

		return(CSSTAT_OK);
            }
	}

