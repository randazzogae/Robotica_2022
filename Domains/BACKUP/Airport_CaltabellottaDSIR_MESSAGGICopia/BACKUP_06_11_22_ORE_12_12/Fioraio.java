
import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.abstractrobot.*;
import	EDU.gatech.cc.is.clay.*;


public class Fioraio extends ControlSystemMFN150 {
    public final static boolean DEBUG = true;
    private NodeVec2	turret_configuration;
    private NodeVec2	steering_configuration;
    private NodeDouble gripper_fingers_configuration;
    private NodeDouble gripper_height_configuration;
    private double mtggain = 1.0;     // move to goal gain
    private double avoidgain = 0.8; // avoid obstacle
    private double noisegain = 0.8; // noise
    private NodeInt 	state_monitor;
    private i_FSA_ba STATE_MACHINE;
    
    
    /**
     * Configure the control system using Clay.
     */
    public void configure() {
           
        //======
        // Set some initial hardware configurations.
        //======
        
        abstract_robot.setObstacleMaxRange(3.0);
        
        abstract_robot.setBaseSpeed(0.5*abstract_robot.MAX_TRANSLATION);
        
        //abbassamento dell'artiglio
        abstract_robot.setGripperHeight(-1,0);
        
        //modalità trigger
        abstract_robot.setGripperFingers(-1,-1);
        
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
        PS_HOMEBASE1_GLOBAL = new v_FixedPoint_(17.03,-7.40);
        NodeVec2      // make it egocentric
        PS_HOMEBASE1 = new v_GlobalToEgo_rv(abstract_robot,
        PS_HOMEBASE1_GLOBAL);
 
        //======
        // Perceptual Features
        //======
 
        // close to homebase1
        NodeBoolean
        PF_CLOSE_TO_HOMEBASE1 = new b_Close_vv(0.4, PS_GLOBAL_POS,
        PS_HOMEBASE1_GLOBAL);
        
       
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
        // STATE_MACHINE
        //======
        STATE_MACHINE = new i_FSA_ba();
        
        STATE_MACHINE.state = 0;
        
        // STATE 0 GO_TO_HOMEBASE1
        STATE_MACHINE.triggers[0][0]  = PF_CLOSE_TO_HOMEBASE1;
        STATE_MACHINE.follow_on[0][0] = 0; // transition to GO_TO_FLAG
 
        
        state_monitor = STATE_MACHINE;
        
        //======
        // STEERING
        //======
        v_Select_vai
        STEERING = new v_Select_vai((NodeInt)STATE_MACHINE);
        
        STEERING.embedded[0] = AS_GO_HOME1;
        
        //======
        // TURRET
        //======
        v_Select_vai
        TURRET = new v_Select_vai((NodeInt)STATE_MACHINE);
        
        TURRET.embedded[0] = AS_GO_HOME1;
        
        //======
        // GRIPPER_FINGERS
        //======
        d_Select_i
        GRIPPER_FINGERS = new d_Select_i(STATE_MACHINE);
        
        GRIPPER_FINGERS.embedded[0] = 1;  // open in GO_TO_HOMEBASE1
           
        d_Select_i GRIPPER_HEIGHT = new d_Select_i(STATE_MACHINE);
        
        GRIPPER_HEIGHT.embedded[0]=0;
       
        turret_configuration = TURRET;
        steering_configuration = STEERING;
        gripper_fingers_configuration = GRIPPER_FINGERS;
        gripper_height_configuration = GRIPPER_HEIGHT;
    }
    
    /**
     * Called every timestep to allow the control system to
     * run.
     */
    public int takeStep() {
        Vec2	result;
        double	dresult;
        long	curr_time = abstract_robot.getTime();
        Vec2	p;
        
        // STEER
        result = steering_configuration.Value(curr_time);
        abstract_robot.setSteerHeading(curr_time, result.t);
        abstract_robot.setSpeed(curr_time, result.r);
        
        // TURRET
        result = turret_configuration.Value(curr_time);
        abstract_robot.setTurretHeading(curr_time, result.t);
        
        // FINGERS
        dresult=gripper_fingers_configuration.Value(curr_time);
        abstract_robot.setGripperFingers(curr_time,dresult);
        
        // HEIGHT
        dresult=gripper_height_configuration.Value(curr_time);
        abstract_robot.setGripperHeight(curr_time,dresult);
        
        
        
        // STATE DISPLAY  aggiungere gli altri nomi agli stati
        int state=STATE_MACHINE.Value(curr_time);
        
        
        if(state==0)
            abstract_robot.setDisplayString("Fioraio");
              
        return(CSSTAT_OK);
    }
}

