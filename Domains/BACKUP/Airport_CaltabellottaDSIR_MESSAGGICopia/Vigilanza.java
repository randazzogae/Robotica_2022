


import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.clay.*;

import  CSAI.unipa.clay.*;
import  CSAI.unipa.knowledgment.*;
import  CSAI.unipa.abstractrobot.*;
import CSAI.unipa.util.mapAttributes;
import sonar_strategy.*;


public class Vigilanza extends ControlSystemMFN150Explore {
    
    public final static boolean DEBUG = true;
    private NodeVec2	turret_configuration;
    private NodeVec2	steering_configuration;
    private NodeDouble  gripper_fingers_configuration;
    private NodeDouble  gripper_height_configuration;
    private NodeDouble  sonar_configuration;
    private NodeGain    mtggain = new NodeGain(1.0);
    private NodeGain    swirlgain = new NodeGain(0.0);
    private NodeGain    avoidgain = new NodeGain(0.8);
    private NodeGain    noisegain = new NodeGain(0.4);
    private NodeInt 	state_monitor;
    private i_FSA_ba    STATE_MACHINE;
    private NodeVec2Array PS_OBS;
	private NodeVec2 PS_GLOBAL_POS;
	Vec2 result;
    double dresult;
    
    private NodeMap     MAP;
    
    private NodeVec2Array PS_SONAR;
    private NodeVec2Array       PS_LASER;
    private NodeVec2Array    PS_TEMP;
    private NodeVec2Array    PS_ALL_OBS;
    private StaticSonarStrategy sonar_strategy;	//istanzio una SonarStrategy
    
   
    
    public void configure() {
        
        //INITIAL CONFIGURATION
        abstract_robot.setBaseSpeed(0.8*abstract_robot.MAX_TRANSLATION);
        abstract_robot.setObstacleMaxRange(3.0);
        abstract_robot.setGripperHeight(-1, 0);
        abstract_robot.setGripperFingers(-1, -1);
        
        MAP = new NodeMap(-12.0, 22.0, 12.0, -12.0, 0.45, 0.35, "MAPPA ", true, abstract_robot); 
        
        
        /**********************************************************************
                                   PERCEPTUAL SCHEMAS
        **********************************************************************/
        
        //THE POSITION OF THE ROBOT
        PS_GLOBAL_POS = new v_GlobalPosition_r(abstract_robot);
        
        //THE POSITION OF THE OBSTACLES
        PS_OBS = new va_OnlyObstacles_r(abstract_robot);
        
        PS_SONAR = new va_SonarSensed_r(abstract_robot);
        
        PS_LASER = new va_LaserSensed_r(abstract_robot);
        
        //PS_TEMP = new va_Merge_vava(PS_LASER, PS_OBS);
        
        PS_ALL_OBS = new va_Merge_vava(PS_OBS, PS_SONAR);
        
        //THE ZONE WHERE TO MOVE
        NodeVec2
        PS_CHOOSED_ZONE = new v_ChooseZone_mv(MAP, PS_GLOBAL_POS, mapAttributes.NORD, mapAttributes.EST, mapAttributes.SUD, mapAttributes.OVEST);
        
        NodeVec2
        PS_CLOSEST_ZONE = new v_Closest_mr(MAP, abstract_robot);
        
        //HOMEBASE
        NodeVec2
        PS_HOMEBASE_GLOBAL = new v_FixedPoint_(20.0, 0.0);
        NodeVec2
        PS_HOMEBASE = new v_GlobalToEgo_rv(abstract_robot, PS_HOMEBASE_GLOBAL);        
        
        /**********************************************************************
                                   PERCEPTUAL FEATURES
        **********************************************************************/
        
        //CLOSE TO HOMEBASE
        NodeBoolean
        PF_CLOSE_HOMEBASE = new b_Close_vv(0.4, PS_GLOBAL_POS, PS_HOMEBASE_GLOBAL);    
        
        
        /**********************************************************************
                                   MOTOR SCHEMAS
        **********************************************************************/
        
        NodeVec2
        MS_AVOID_OBSTACLES = new v_Avoid_va(3.0, abstract_robot.RADIUS + 0.1, PS_OBS);
        
        NodeVec2
        MS_MOVE_ZONE = new v_LinearAttraction_v(0.0, 0.0, PS_CHOOSED_ZONE);
        
        NodeVec2
        MS_MOVE_CLOSEST_ZONE = new v_LinearAttraction_v(0.0, 0.0, PS_CLOSEST_ZONE);
        
        NodeVec2
        MS_MOVE_TO_HOMEBASE = new v_LinearAttraction_v( 0.4, 0.0, PS_HOMEBASE);
        
        // swirl obstacles left wrt noise
        NodeVec2
        MS_SWIRL_OBSTACLES_LEFT = new v_SwirlLeft_va(2.0,abstract_robot.RADIUS + 0.1,PS_OBS);
        
        NodeVec2
        MS_NOISE = new v_Noise_(5, 2);
        
        /************
         AS GO HOME 
        ************/
        
        v_StaticWeightedSum_va        
        AS_GO_HOME =  new v_StaticWeightedSum_va();
        
        AS_GO_HOME.embedded[0] = MS_MOVE_TO_HOMEBASE;
        AS_GO_HOME.weights[0] = mtggain.Value(abstract_robot.getTime());
        
        AS_GO_HOME.embedded[1] = MS_AVOID_OBSTACLES;
        AS_GO_HOME.weights[1] = avoidgain.Value(abstract_robot.getTime());
        
        AS_GO_HOME.embedded[2] = MS_NOISE;
        AS_GO_HOME.weights[2] = noisegain.Value(abstract_robot.getTime());
        
        
        
        /************
         AS GO ZONE 
        ************/
        
        v_StaticWeightedSum_va        
        AS_GO_ZONE =  new v_StaticWeightedSum_va();
        
        AS_GO_ZONE.embedded[0] = MS_MOVE_ZONE;
        AS_GO_ZONE.weights[0] = mtggain.Value(abstract_robot.getTime());
        
        AS_GO_ZONE.embedded[1] = MS_AVOID_OBSTACLES;
        AS_GO_ZONE.weights[1] = avoidgain.Value(abstract_robot.getTime());
        
        AS_GO_ZONE.embedded[2] = MS_NOISE;
        AS_GO_ZONE.weights[2] = noisegain.Value(abstract_robot.getTime());
        
        /************
         AS GO CLOSEST ZONE 
        ************/
        
        v_StaticWeightedSum_va        
        AS_GO_CLOSEST_ZONE =  new v_StaticWeightedSum_va();
        
        AS_GO_CLOSEST_ZONE.embedded[0] = MS_MOVE_CLOSEST_ZONE;
        AS_GO_CLOSEST_ZONE.weights[0] = mtggain.Value(abstract_robot.getTime());
        
        AS_GO_CLOSEST_ZONE.embedded[1] = MS_AVOID_OBSTACLES;
        AS_GO_CLOSEST_ZONE.weights[1] = avoidgain.Value(abstract_robot.getTime());
        
        AS_GO_CLOSEST_ZONE.embedded[2] = MS_NOISE;
        AS_GO_CLOSEST_ZONE.weights[2] = noisegain.Value(abstract_robot.getTime());
        
        
        //======
        // AS_SWIRL_LEFT
        //======
        v_StaticWeightedSum_va 
        AS_SWIRL_LEFT = new v_StaticWeightedSum_va();

        AS_SWIRL_LEFT.weights[0]  = swirlgain.Value(abstract_robot.getTime());
        AS_SWIRL_LEFT.embedded[0] = MS_SWIRL_OBSTACLES_LEFT;

        AS_SWIRL_LEFT.weights[1]  = noisegain.Value(abstract_robot.getTime());
        AS_SWIRL_LEFT.embedded[1] = MS_NOISE;
		
        AS_SWIRL_LEFT.weights[2]  = avoidgain.Value(abstract_robot.getTime());
        AS_SWIRL_LEFT.embedded[2] = MS_AVOID_OBSTACLES;
        
        AS_SWIRL_LEFT.weights[3]  = mtggain.Value(abstract_robot.getTime());
        AS_SWIRL_LEFT.embedded[3] = MS_MOVE_ZONE;

        
        /***************************
           STATE MACHINE
        ****************************/
        
        STATE_MACHINE = new i_FSA_ba();
        STATE_MACHINE.state = 0;
        //STATE_MACHINE.triggers[0][0] = PF_CLOSE_HOMEBASE;
        //STATE_MACHINE.follow_on[0][0] = 1;
        
        state_monitor = STATE_MACHINE;
        
        /***************************
           STEERING
        ****************************/        
        
        v_Select_vai
        STEERING = new v_Select_vai(STATE_MACHINE);
        
        STEERING.embedded[0] = AS_GO_ZONE;

        /***************************
           TURRET
        ****************************/     
        
        v_Select_vai
        TURRET = new v_Select_vai(STATE_MACHINE);
        TURRET = STEERING;
        
        /***************************
           GRIPPER FINGERS
        ****************************/   
        
        d_Select_i
        GRIPPER_FINGERS = new d_Select_i(STATE_MACHINE);
        
        GRIPPER_FINGERS.embedded[0] = 1;
        GRIPPER_FINGERS.embedded[1] = 1;

        /***************************
           GRIPPER HEIGHT
        ****************************/   
        
        d_Select_i
        GRIPPER_HEIGHT = new d_Select_i(STATE_MACHINE);
        
        GRIPPER_HEIGHT.embedded[0] = 1;
        GRIPPER_HEIGHT.embedded[1] = 1;
        
        /****************************
            SONAR CONFIGURATION
         ***************************/
        
           
        sonar_configuration = new d_Rotator_(0.0, 0.1);
        
        turret_configuration = TURRET;
        steering_configuration = STEERING;
        gripper_fingers_configuration = GRIPPER_FINGERS;
        gripper_height_configuration = GRIPPER_HEIGHT;

        sonar_strategy = new StaticSonarStrategy();	//Inizializzo la strategia
		sonar_strategy.start_strategy(abstract_robot);
               
    }    
    
    public int takeStep() {
        
       
        long curr_time = abstract_robot.getTime();
        
        MAP.setCellVisited(curr_time);
        
        MAP.setObstacle(PS_SONAR.Value(curr_time), abstract_robot.getPrecisionRivelation(), curr_time);

        //STEER
        result = steering_configuration.Value(curr_time);
        abstract_robot.setSteerHeading(curr_time, result.t);
        abstract_robot.setSpeed(curr_time, result.r);
        
        //TURRET
        result = turret_configuration.Value(curr_time);
        abstract_robot.setTurretHeading(curr_time, result.t);
        
        //GRIPPER FINGERS
        dresult = gripper_fingers_configuration.Value(curr_time);
        abstract_robot.setGripperFingers(curr_time, dresult);
        
        //GRIPPER FINGERS
       // dresult = gripper_height_configuration.Value(curr_time);
        //abstract_robot.setGripperHeight(curr_time, dresult);
        
        
        
        //STATE DISPLAY
        int state=STATE_MACHINE.Value(curr_time);
        
        //if(state==0)
            abstract_robot.setDisplayString("Vigilanza");
        
        //aggiorno i sonar secondo la strategia
        sonar_strategy.apply_strategy(abstract_robot, state);
        
        return(CSSTAT_OK);
    }
    
}
