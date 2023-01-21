
import	EDU.gatech.cc.is.util.*;
import	EDU.gatech.cc.is.abstractrobot.*;
import	EDU.gatech.cc.is.clay.*;
import  EDU.gatech.cc.is.communication.*;
import  java.util.*;


public class Oby1Kenoby_2 extends ControlSystemMFN150
	{
	public final static boolean DEBUG = true;
	private NodeVec2	turret_configuration;
	private NodeVec2	steering_configuration;
	private NodeDouble gripper_fingers_configuration;
	private NodeDouble gripper_height_configuration;
	private double mtggain = 1.0;     // move to goal gain 
	private double avoidgain = 0.8; // avoid obstacle 
	private double noisegain = 0.2; // noise
    private double swirlgain = 1.0; //swirl
	private NodeInt 	state_monitor;
	private i_FSA_ba STATE_MACHINE;
	private Enumeration BufMsg;
	private boolean fermo=true;
	private boolean RicevutoDa2=false;


	/**
	 * Configure the control system using Clay.
	 */
	public void configure()
		{


		//======
		// Set some initial hardware configurations.
		//======

		abstract_robot.setObstacleMaxRange(3.0); 

		abstract_robot.setBaseSpeed(0.8*abstract_robot.MAX_TRANSLATION);

		//abbassamento dell'artiglio
		abstract_robot.setGripperHeight(-1,0);   

		//modalit� trigger
		abstract_robot.setGripperFingers(-1,-1); 


        // definisco il buffer dei messaggi.
        BufMsg=abstract_robot.getReceiveChannel();
        
		//======
		// perceptual schemas
		//======
		//--- robot's global position
		NodeVec2
		PS_GLOBAL_POS = new v_GlobalPosition_r(abstract_robot);

		//--- obstacles
		NodeVec2Array // the sonar readings
		PS_OBS = new va_Obstacles_r(abstract_robot);

		//--- homebase1 
		NodeVec2      // the goal location
		PS_HOMEBASE1_GLOBAL = new v_FixedPoint_(10,0.0);
		NodeVec2      // make it egocentric
		PS_HOMEBASE1 = new v_GlobalToEgo_rv(abstract_robot,
				PS_HOMEBASE1_GLOBAL);

		//--- homebase2
		NodeVec2      // the goal location
		PS_HOMEBASE2_GLOBAL = new v_FixedPoint_(0.0,3.0);
		NodeVec2      // make it egocentric
		PS_HOMEBASE2 = new v_GlobalToEgo_rv(abstract_robot,
				PS_HOMEBASE2_GLOBAL);


		//--- Flag 
		NodeVec2      // the goal location
		PS_FLAG_GLOBAL = new v_FixedPoint_(7.0,3.0);
		NodeVec2      // make it egocentric
		PS_FLAG = new v_GlobalToEgo_rv(abstract_robot,
				PS_FLAG_GLOBAL);


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

		// close to flag 
		NodeBoolean
		PF_CLOSE_TO_FLAG = new b_Close_vv(0.4, PS_GLOBAL_POS,
			PS_FLAG_GLOBAL);


		//======
		// motor schemas
		//======
		// avoid obstacles
		NodeVec2
		MS_AVOID_OBSTACLES = new v_Avoid_va(3.0,
			abstract_robot.RADIUS + 0.1,
			PS_OBS);

		// move to goal1
		NodeVec2
		MS_MOVE_TO_HOMEBASE1 = new v_LinearAttraction_v(0.4,0.0,PS_HOMEBASE1);

		// move to goal2
		NodeVec2
		MS_MOVE_TO_HOMEBASE2 = new v_LinearAttraction_v(0.4,0.0,PS_HOMEBASE2);

		// move to flag
		NodeVec2
		MS_MOVE_TO_FLAG= new v_LinearAttraction_v(0.4,0.0,PS_FLAG);


		// noise vector
		NodeVec2
		MS_NOISE_VECTOR = new v_Noise_(10,5);

        // swirl to home1
        NodeVec2
        MS_SWIRL_TO_H1 = new v_Swirl_vav(2.0,abstract_robot.RADIUS+0.22,PS_OBS,PS_HOMEBASE1);

        // swirl to home2
        NodeVec2
        MS_SWIRL_TO_H2 = new v_Swirl_vav(2.0,abstract_robot.RADIUS+0.22,PS_OBS,PS_HOMEBASE2);


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

		AS_GO_HOME1.weights[3]  = swirlgain;
		AS_GO_HOME1.embedded[3] = MS_SWIRL_TO_H1;

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

        AS_GO_HOME2.weights[3]  = swirlgain;
		AS_GO_HOME2.embedded[3] = MS_SWIRL_TO_H2;


		//======
		// AS_GO_FLAG
		//======
		v_StaticWeightedSum_va 
		AS_GO_FLAG = new v_StaticWeightedSum_va();

		AS_GO_FLAG.weights[0]  = avoidgain;
		AS_GO_FLAG.embedded[0] = MS_AVOID_OBSTACLES;

		AS_GO_FLAG.weights[1]  = noisegain;
		AS_GO_FLAG.embedded[1] = MS_NOISE_VECTOR;

		AS_GO_FLAG.weights[2]  = mtggain;
		AS_GO_FLAG.embedded[2] = MS_MOVE_TO_FLAG;



		//======
		// STATE_MACHINE
		//======
		STATE_MACHINE = new i_FSA_ba();

		STATE_MACHINE.state = 0;
		
		// STATE 0 GO_TO_HOMEBASE1
		STATE_MACHINE.triggers[0][0]  = PF_CLOSE_TO_HOMEBASE1;
		STATE_MACHINE.follow_on[0][0] = 1; // transition to GO_TO_FLAG

		// STATE 1 GO_TO_FLAG 
		STATE_MACHINE.triggers[1][0]  = PF_CLOSE_TO_FLAG;
		STATE_MACHINE.follow_on[1][0] = 2; // transition to ACQUIRE

		    // STATE 2 ACQUIRE
		
		STATE_MACHINE.triggers[2][0]  = PF_CLOSE_TO_HOMEBASE2;
		STATE_MACHINE.follow_on[2][0] = 3; 

		state_monitor = STATE_MACHINE;


		//======
		// STEERING
		//======
		v_Select_vai
		STEERING = new v_Select_vai((NodeInt)STATE_MACHINE);

		STEERING.embedded[0] = AS_GO_HOME1;
		STEERING.embedded[1] = AS_GO_FLAG;
		STEERING.embedded[2] = AS_GO_HOME2;
        STEERING.embedded[3] = AS_GO_HOME2;


		//======
		// TURRET
		//======
		v_Select_vai
		TURRET = new v_Select_vai((NodeInt)STATE_MACHINE);
		
		TURRET.embedded[0] = AS_GO_HOME1;
		TURRET.embedded[1] = AS_GO_FLAG;
		TURRET.embedded[2] = AS_GO_HOME2;
        TURRET.embedded[3] = AS_GO_HOME2;

	
		
		turret_configuration = TURRET;
		steering_configuration = STEERING;
		}
		
	/**
	 * Called every timestep to allow the control system to
	 * run.
	 */
	public int takeStep()
		{
		Vec2	result;
		long	curr_time = abstract_robot.getTime();
        int state = STATE_MACHINE.Value(curr_time);

		//Comunicazione
		if (BufMsg.hasMoreElements()){
   			LongMessage RicevutoDa = (LongMessage)BufMsg.nextElement();
 			if (BufMsg.hasMoreElements()){
      				StringMessage RicevutoMsg = (StringMessage)BufMsg.nextElement();
      				if (RicevutoMsg.val=="OK vado" && RicevutoDa.val==2){ 
      					System.out.println("Robot 1--> Ricevuto dal robot 2 il messaggio: "+RicevutoMsg.val);
      					RicevutoDa2=true;
      				} 
      				if(RicevutoMsg.val=="Vai Robot1" && RicevutoDa.val==0){
					LongMessage msg1 = new LongMessage();
					msg1.val=abstract_robot.getPlayerNumber(curr_time);
					abstract_robot.broadcast(msg1);
					StringMessage msg2 = new StringMessage();
					msg2.val="OK vado";
					abstract_robot.broadcast(msg2);
					System.out.println("Robot 1--> Ricevuto dal robot 0 il messaggio: "+RicevutoMsg.val);
					System.out.println("Robot 1--> OK vado");
					fermo=false;
				}      
      			}
   		}
        // resto fermo aspettando comunicazione.
		if (fermo) //aspetto prima di inviare
		{
			System.out.println("SONO FERMO");
			return(CSSTAT_OK);
		} 
		//else se diventa falso invio il messaggio
		
		// STEER
		result = steering_configuration.Value(curr_time);
		abstract_robot.setSteerHeading(curr_time, result.t);
		abstract_robot.setSpeed(curr_time, result.r);
		
		// TURRET
		result = turret_configuration.Value(curr_time);
		abstract_robot.setTurretHeading(curr_time, result.t);
        
        if(state==0) 
          abstract_robot.setDisplayString("Vado Home 1");
		if(state==1)
		  abstract_robot.setDisplayString("Stessa strada");
		if(state==2)
		  abstract_robot.setDisplayString("Vado Home 2 Finale");
			

		return(CSSTAT_OK);
		}
	}

