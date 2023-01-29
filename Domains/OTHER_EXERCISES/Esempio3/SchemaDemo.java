
import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.abstractrobot.*;
import	EDU.gatech.cc.is.clay.*;


public class SchemaDemo extends ControlSystemMFN150 {
    public final static boolean DEBUG = true;
    private NodeVec2	turret_configuration;
    private NodeVec2	steering_configuration;
    private NodeDouble gripper_fingers_configuration;
    private NodeDouble gripper_height_configuration;
    private double mtggain = 1.0;     // move to goal gain
    private double avoidgain = 0.8; // avoid obstacle
    private double noisegain = 0.2; // noise
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
        PS_HOMEBASE1_GLOBAL = new v_FixedPoint_(10.0,0.0);
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
        
        
        //--- targets of visual class 0
        NodeVec2Array
        PS_TARGETS0_EGO =
        new va_VisualObjects_r(0,abstract_robot);
        // array di bandiere classe visuale 0 ossia blu
        
        //--- targets of visual class 5
        NodeVec2Array
        PS_TARGETS5_EGO =
        new va_VisualObjects_r(5,abstract_robot);
        // array di bandiere classe visuale 5 ossia verde
        
        
        //--- targets of visual class 6
        NodeVec2Array
        PS_TARGETS6_EGO =
        new va_VisualObjects_r(6,abstract_robot);
        // array di bandiere classe visuale 6 ossia rosso
        
        
        //Uniamo gli array delle bandiere di vc 5 e 6 (coordinate egocentriche)
        NodeVec2Array PS_TARGETS56 = new va_Merge_vava(PS_TARGETS5_EGO, PS_TARGETS6_EGO );
        
        //Rendiamo lengthcoordinate globali per effettuare il filtraggio
        NodeVec2Array PS_TARGETS56_GLOBAL = new va_Add_vav(PS_TARGETS56, PS_GLOBAL_POS);
        
        //Filtriamo gli oggetti di vc 5 e 6 da una zona circolare con centro in PS_FLAG_GLOBAL e raggio 1
        //PS_TARGETS_GLOBAL_FILTRATI contiene gli oggetti che NON devo prendere
        NodeVec2Array PS_TARGETS_GLOBAL_FILTRATI = new va_FilterOutClose_vva(1.00, PS_FLAG_GLOBAL, PS_TARGETS56_GLOBAL );
        
        //Bandierine filtrate in coordinate egocentriche
        NodeVec2Array PS_TARGETS_PERSONALI= new va_Subtract_vav(PS_TARGETS_GLOBAL_FILTRATI, PS_GLOBAL_POS);
        
        //Fondiamo oggetti, ovvero le bandiere da prendere e quelle che invece devo filtrare
        //In questo modo il robot sa ciò che deve prendere e ciò che deve ignorare
        NodeVec2Array PS_TARGETS_TO_KEEP = new va_Merge_vava(PS_TARGETS0_EGO,PS_TARGETS_PERSONALI);
        
        //bandiera più vicina tra quelle viste (e non filtrate)
        NodeVec2 PS_CLOSEST_TARGET_RV = new v_Closest_va(PS_TARGETS_TO_KEEP);
        
           
        //--- type of object in the gripper
        NodeInt
        PS_IN_GRIPPER = new i_InGripper_r(abstract_robot);
        // ogg. Nell'artiglio
         
        
        //======
        // Perceptual Features
        //======
        NodeBoolean
        PF_SOMETHING_VISIBLE=new b_NonZero_v(PS_CLOSEST_TARGET_RV);

        
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
        PF_CLOSE_TO_FLAG_ZONE = new b_Close_vv(0.4, PS_GLOBAL_POS,
        PS_FLAG_GLOBAL);
        
        // is something in the gripper?
        NodeBoolean
        PF_TARGET0_IN_GRIPPER = new b_Equal_i(0,PS_IN_GRIPPER);
        // se l'oggetto e' blu ok
        
       
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
        
        
        NodeVec2 MS_MOVE_TO_BLUFLAG = new v_LinearAttraction_v(0.0,0.0,PS_CLOSEST_TARGET_RV);
        
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
        // AS_GO_CLOSEST_BLU_FLAG
        //======
        v_StaticWeightedSum_va
        AS_GO_CLOSEST_BLU_FLAG = new v_StaticWeightedSum_va();
        
        AS_GO_CLOSEST_BLU_FLAG.weights[0]  = avoidgain;
        AS_GO_CLOSEST_BLU_FLAG.embedded[0] = MS_AVOID_OBSTACLES;
        
        AS_GO_CLOSEST_BLU_FLAG.weights[1]  = noisegain;
        AS_GO_CLOSEST_BLU_FLAG.embedded[1] = MS_NOISE_VECTOR;
        
        AS_GO_CLOSEST_BLU_FLAG.weights[2]  = mtggain;
        AS_GO_CLOSEST_BLU_FLAG.embedded[2] = MS_MOVE_TO_BLUFLAG;
        
        //======
        // STATE_MACHINE
        //======
        STATE_MACHINE = new i_FSA_ba();
        
        STATE_MACHINE.state = 0;
        
        // STATE 0 GO_TO_HOMEBASE1
        STATE_MACHINE.triggers[0][0]  = PF_CLOSE_TO_HOMEBASE1;
        STATE_MACHINE.follow_on[0][0] = 1; // transition to GO_TO_FLAG
        
        
        // STATE 1 SOMETHING VISIBLE
        
        STATE_MACHINE.triggers[1][0]  = PF_SOMETHING_VISIBLE;
        STATE_MACHINE.follow_on[1][0] = 2;
        STATE_MACHINE.triggers[1][1]  = PF_CLOSE_TO_FLAG_ZONE;
        STATE_MACHINE.follow_on[1][1] = 4;
        
        
        // STATE 2 GO_TO_FLAG
        
        STATE_MACHINE.triggers[2][0]  = PF_TARGET0_IN_GRIPPER;
        STATE_MACHINE.follow_on[2][0] = 3; // transition to ACQUIRE
        
        
        // STATE 3 ACQUIRE
        
        STATE_MACHINE.triggers[3][0]  = PF_CLOSE_TO_HOMEBASE2;
        STATE_MACHINE.follow_on[3][0] = 1;
        
        state_monitor = STATE_MACHINE;
        
        
        //======
        // STEERING
        //======
        v_Select_vai
        STEERING = new v_Select_vai((NodeInt)STATE_MACHINE);
        
        STEERING.embedded[0] = AS_GO_HOME1;
        STEERING.embedded[1] = AS_GO_FLAG;
        STEERING.embedded[2] = AS_GO_CLOSEST_BLU_FLAG;
        STEERING.embedded[3] = AS_GO_HOME2;
        STEERING.embedded[4] = AS_GO_HOME1;
        
        //======
        // TURRET
        //======
        v_Select_vai
        TURRET = new v_Select_vai((NodeInt)STATE_MACHINE);
        
        TURRET.embedded[0] = AS_GO_HOME1;
        TURRET.embedded[1] = AS_GO_FLAG;
        TURRET.embedded[2] = AS_GO_CLOSEST_BLU_FLAG;
        TURRET.embedded[3] = AS_GO_HOME2;
        TURRET.embedded[4] = AS_GO_HOME1;
        
        //======
        // GRIPPER_FINGERS
        //======
        d_Select_i
        GRIPPER_FINGERS = new d_Select_i(STATE_MACHINE);
        
        GRIPPER_FINGERS.embedded[0] = 1;  // open in GO_TO_HOMEBASE1
        GRIPPER_FINGERS.embedded[1] = -1; // trigger in GO_TO_FLAG
        GRIPPER_FINGERS.embedded[2] = -1;  // closed in GO_TO_HOMEBASE2
        GRIPPER_FINGERS.embedded[3] = 0;
        GRIPPER_FINGERS.embedded[4] = 1;  // open in GO_TO_HOMEBASE1
        
        
        d_Select_i GRIPPER_HEIGHT = new d_Select_i(STATE_MACHINE);
        
        GRIPPER_HEIGHT.embedded[0]=0;
        GRIPPER_HEIGHT.embedded[1]=0;
        GRIPPER_HEIGHT.embedded[2]=0;
        GRIPPER_HEIGHT.embedded[3]=0;
        GRIPPER_HEIGHT.embedded[4]=0;
        
        
        
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
            abstract_robot.setDisplayString("Vado Home 1");
        if(state==1)
            abstract_robot.setDisplayString("Vado zona bandiere");
        if(state==2)
            abstract_robot.setDisplayString("Bandiera più vicina");
        if(state==3)
            abstract_robot.setDisplayString("Vado Home 2");
        if(state==4)
            abstract_robot.setDisplayString("Vado Home 1 Final");
        
        
        return(CSSTAT_OK);
    }
}

