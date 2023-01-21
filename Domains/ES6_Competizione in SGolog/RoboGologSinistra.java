

import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.communication.*;

import CSAI.unipa.clay.*;
import CSAI.unipa.abstractrobot.*;
import CSAI.unipa.SGolog.*;
import CSAI.unipa.knowledgment.*;

import java.awt.*;
import java.io.*;
import java.util.Enumeration;


public class RoboGologSinistra extends ControlSystemMFN150Explore{
    public final static boolean DEBUG = true;
    private NodeVec2            turret_configuration;
    private NodeVec2            steering_configuration;
    private NodeBoolean         sonar_configuration;
    private NodeDouble          gripper_fingers_configuration;
    private NodeBoolean         laser_configuration;
    private double              mtggain = 1.0;     // move to goal gain 
    private double              avoidgain = 0.8; // avoid obstacle 
    private double              noisegain = 0.2; // noise
    private double              swirlgain = 0.7;
    private NodeInt             state_monitor;
    private i_FSA_ba            STATE_MACHINE;
    private NodeVec2Array       PS_SONAR;
    private NodeVec2            PS_RED_BIN;
    private NodeVec2            PS_GREEN_BIN;
    private NodeVec2            PS_BLUE_BIN;
    private NodeVec2            PS_RED_ATTRACTOR;
    private NodeVec2            PS_GREEN_ATTRACTOR;
    private NodeVec2            PS_BLUE_ATTRACTOR;
    private NodeBooleanDinamic  PF_OTHER_ROBOT;
    
    private NodeMap             MAP;
    private v_DinamicPoint_     PS_ATTRACTOR;
    
    private Enumeration bufferMessage;

    private sGologClient        client;
    private SGologPlan          plan;
    private int                     id;
    private int                     other_id;


    /**
     * Configure the control system using Clay.
     */
    public void configure(){

        //======
        // Set some initial hardware configurations.
        //======
        
        id = abstract_robot.getPlayerNumber(abstract_robot.getTime());
        
        if (id == 0)
            other_id = 1;
        else
            other_id = 0;

        abstract_robot.setObstacleMaxRange(3.0); 

        abstract_robot.setBaseSpeed(0.5*abstract_robot.MAX_TRANSLATION); //DI DEFAULT LA VELOCITA' E' 0.4, IO L'HO MESSA A 0.9

        //abbassamento dell'artiglio
        abstract_robot.setGripperHeight(-1,0);   

        //modalit� trigger
        abstract_robot.setGripperFingers(-1,-1);     
        
        //MAP = new NodeMap(-7,7,10,-10,2,1.7,"Mappa destra",false,abstract_robot);
        MAP = new NodeMap(-8,8,11,-11,2,1.7,"Mappa destra",false,abstract_robot);

        bufferMessage = abstract_robot.getReceiveChannel();
       
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
        
        NodeVec2Array
        PS_ALL_OBS = new va_Merge_vava(PS_SONAR, PS_OBS);
        
        PS_ATTRACTOR = new v_DinamicPoint_(PS_GLOBAL_POS, 0.0, 0.0);
        
        NodeVec2
        PS_CLOSEST_ZONE = new v_Closest_mr(MAP, abstract_robot);

        //--- targets of visual class 0
        NodeVec2Array 
        PS_RED_FLAG_EGO = 
                new va_VisualObjects_r(0,abstract_robot); 
        
        //--- targets of visual class 1
        NodeVec2Array 
        PS_GREEN_FLAG_EGO = 
                new va_VisualObjects_r(1,abstract_robot); 
        
        //--- targets of visual class 1
        NodeVec2Array 
        PS_BLUE_FLAG_EGO = 
                new va_VisualObjects_r(2,abstract_robot); 
        
        NodeVec2Array
        PS_TEMP = new va_Merge_vava(PS_GREEN_FLAG_EGO, PS_RED_FLAG_EGO);
        
        NodeVec2Array
        PS_ALL_TARGET = new va_Merge_vava(PS_TEMP, PS_BLUE_FLAG_EGO);
        
        NodeVec2
        PS_CLOSEST_FLAG = new v_Closest_va(PS_ALL_TARGET);
        
        //--- type of object in the gripper
        NodeInt
        PS_IN_GRIPPER = new i_InGripper_r(abstract_robot); 

        //======
        // Perceptual Features
        //======

        NodeBoolean
        PF_CLOSE_TO = new b_CloseDinamic_vv(0.6, PS_GLOBAL_POS, PS_ATTRACTOR);
        
        NodeBoolean
        PF_SOMETHING_VISIBLE=new b_NonZero_v(PS_CLOSEST_FLAG);
        
        PF_OTHER_ROBOT = new NodeBooleanDinamic(false);

        NodeBoolean
        PF_RED_FLAG_IN_GRIPPER = new b_Equal_i(0, PS_IN_GRIPPER);
        
        NodeBoolean
        PF_GREEN_FLAG_IN_GRIPPER = new b_Equal_i(1, PS_IN_GRIPPER);
        
        NodeBoolean
        PF_BLUE_FLAG_IN_GRIPPER = new b_Equal_i(2, PS_IN_GRIPPER);
        
        NodeBoolean 
        PF_SOMETHING_IN_GRIPPER = new b_Persist_s (4.0,new b_NonNegative_s(PS_IN_GRIPPER));

        NodeBoolean
        PF_NOT_SOMETHING_VISIBLE= new b_Not_s(PF_SOMETHING_VISIBLE);
        
        NodeBoolean
        PF_TRIGGER_FLAG = new b_Or_bb(PF_NOT_SOMETHING_VISIBLE, PF_SOMETHING_IN_GRIPPER);
        
        //======
        // motor schemas
        //======
        // avoid obstacles
        NodeVec2
        MS_AVOID_OBSTACLES = new v_AvoidSonar_va(3.0, abstract_robot.RADIUS + 0.1,
                PS_OBS, PS_GLOBAL_POS);

        NodeVec2
        MS_MOVE_TO = new v_LinearAttraction_v(0.4,0.0,PS_ATTRACTOR);
        
        NodeVec2
        MS_MOVE_TO_FLAG = new v_LinearAttraction_v(0.4,0.0,PS_CLOSEST_FLAG);
        
        //THE LINEAR ATTRACTION TO TEMP ATTRACTOR
        NodeVec2
        MS_MOVE_TO_CLOSEST_ZONE = new v_LinearAttraction_v( 0.4, 0.0, PS_CLOSEST_ZONE);

        // noise vector
        NodeVec2
        MS_NOISE_VECTOR = new v_Noise_(10,5);
        
        NodeVec2
        MS_SWIRL_CLOSEST_ZONE = new v_Swirl_vav(1.5, abstract_robot.RADIUS + 0.18,
                                            PS_OBS, PS_CLOSEST_ZONE);
        
        NodeVec2
        MS_SWIRL_TO = new v_Swirl_vav(1.5, abstract_robot.RADIUS + 0.18, 
                                           PS_OBS, PS_ATTRACTOR);
        //======
        // AS_GOTO
        //======
        v_StaticWeightedSum_va 
        AS_GOTO = new v_StaticWeightedSum_va();

        AS_GOTO.weights[0]  = avoidgain;
        AS_GOTO.embedded[0] = MS_AVOID_OBSTACLES;

        AS_GOTO.weights[1]  = noisegain;
        AS_GOTO.embedded[1] = MS_NOISE_VECTOR;

        AS_GOTO.weights[2]  = mtggain;
        AS_GOTO.embedded[2] = MS_MOVE_TO;
        
        AS_GOTO.embedded[3] = MS_SWIRL_TO;
        AS_GOTO.weights[3] = swirlgain;    
      
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
        
        /************
         AS GO CLOSEST ZONE
        ************/
        
        v_StaticWeightedSum_va        
        AS_GO_CLOSEST_ZONE =  new v_StaticWeightedSum_va();
        
        AS_GO_CLOSEST_ZONE.embedded[0] = MS_MOVE_TO_CLOSEST_ZONE;
        AS_GO_CLOSEST_ZONE.weights[0] = mtggain;
        
        AS_GO_CLOSEST_ZONE.embedded[1] = MS_AVOID_OBSTACLES;
        AS_GO_CLOSEST_ZONE.weights[1] = avoidgain;
        
        AS_GO_CLOSEST_ZONE.embedded[2] = MS_NOISE_VECTOR;
        AS_GO_CLOSEST_ZONE.weights[2] = noisegain;        
        
        AS_GO_CLOSEST_ZONE.embedded[3] = MS_SWIRL_CLOSEST_ZONE;
        AS_GO_CLOSEST_ZONE.weights[3] = swirlgain;      
        

        File eclipseProgram = new File(".\\Database\\sGolog.pl");
        client = new sGologClient(eclipseProgram);

        eclipseProgram = new File(".\\Database\\Competition.pl");
        client.consultFile(eclipseProgram); 

        plan = new SGologPlan(abstract_robot); 
        
        plan.debugging(false);

        plan.addAction("goto", 
                       AS_GOTO, 
                       AS_GOTO,
                       PF_CLOSE_TO, 
                       0, 
                       new int[]{}, 
                       NodeAction.NO_LASER);
                       
       plan.addAction("open_gripper",
                       NodeAction.NO_STEERING,
                       NodeAction.NO_TURRET,
                       NodeAction.IMMEDIATE_TRIGGER,
                       1,
                       NodeAction.NO_SONAR, 
                       NodeAction.NO_LASER);
                       
       plan.addAction("go_flag", 
                       AS_GO_FLAG, 
                       AS_GO_FLAG,
                       PF_TRIGGER_FLAG, 
                       -1, 
                       NodeAction.NO_SONAR, 
                       NodeAction.NO_LASER);

        plan.addAction("wander", 
                        AS_GO_CLOSEST_ZONE,
                        AS_GO_CLOSEST_ZONE,
                        PF_SOMETHING_VISIBLE,
                        NodeAction.NO_GRIPPER,
                        new int[]{0,2,4,6,8,10,12,14},
                        NodeAction.NO_LASER);
                        
        plan.addAction("send_message",
                        NodeAction.NO_STEERING,
                        NodeAction.NO_TURRET,
                        NodeAction.IMMEDIATE_TRIGGER,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);
        
        plan.addAction("wait",
                        AS_GOTO,
                        AS_GOTO,
                        PF_OTHER_ROBOT,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);

        plan.addAction("something_visible", PF_SOMETHING_VISIBLE);
        
        plan.addAction("red_flag", PF_RED_FLAG_IN_GRIPPER);
        
        plan.addAction("green_flag", PF_GREEN_FLAG_IN_GRIPPER);
        
        plan.addAction("blue_flag", PF_BLUE_FLAG_IN_GRIPPER);
        
        plan.addAction("can_go", PF_OTHER_ROBOT);

        plan.setPlan(client.invoke("do(execute,s0,S)"),0);   

        //======
        // STATE_MACHINE
        //======
        STATE_MACHINE = new i_FSA_ba();

        STATE_MACHINE.state = 0;

        STATE_MACHINE.triggers[0][0]  = plan.isFinished(abstract_robot.getTime());
        STATE_MACHINE.follow_on[0][0] = 0;

        state_monitor = STATE_MACHINE;

        //======
        // STEERING
        //======
        v_Select_vai
        STEERING = new v_Select_vai((NodeInt)STATE_MACHINE);

        /* 
         * BISOGNA PASSARE ALLO STATO GOLOG IL RISULTATO DI QUESTO METODO
         */
        STEERING.embedded[0] = plan.getSteering(abstract_robot.getTime());

        //======
        // TURRET
        //======
        v_Select_vai
        TURRET = new v_Select_vai((NodeInt)STATE_MACHINE);

        TURRET.embedded[0] = plan.getTurret(abstract_robot.getTime());

        //======
        // GRIPPER_FINGERS
        //======
        d_Select_i
        GRIPPER_FINGERS = new d_Select_i(STATE_MACHINE);

        GRIPPER_FINGERS.embedded[0] = plan.getGripperFingers(abstract_robot.getTime());

        //======
        // SONAR_CONFIGURATION
        //======
        d_SonarControl_ir 
        SONAR_CONFIGURATION = new d_SonarControl_ir(STATE_MACHINE, abstract_robot);

        SONAR_CONFIGURATION.sonarActived[0] = plan.getSonar(abstract_robot.getTime());
        SONAR_CONFIGURATION.sonarPrecision[0] = SonarObjectSensor.MEDIUM;

        //======
        // LASER_CONFIGURATION
        //======
        b_Select_ir
        LASER_CONFIGURATION = new b_Select_ir(STATE_MACHINE, abstract_robot);

        LASER_CONFIGURATION.embedded[0] = plan.getLaser(abstract_robot.getTime());

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
    public int takeStep(){
        
        Vec2	result;
        double	dresult;
        boolean bresult;
        long	curr_time = abstract_robot.getTime();
        String  action;

        //STATE DISPLAY  aggiungere gli altri nomi agli stati
        int state=STATE_MACHINE.Value(curr_time);
        
        MAP.setCellVisited(curr_time);
        
        MAP.setObstacle(PS_SONAR.Value(curr_time), abstract_robot.getPrecisionRivelation(), curr_time);

        plan.update(curr_time, state);
        
        if(plan.isChangedState()){
            action = plan.getActionName();
            if (action.startsWith("goto(")){
                if(plan.getParameter()[0].equals("red_bin")){
                    PS_ATTRACTOR.setValue(curr_time, 9.0, -5.5);
                }
                else
                if(plan.getParameter()[0].equals("green_bin")){
                    PS_ATTRACTOR.setValue(curr_time, 9.0, 0.0);
                }
                else
                if(plan.getParameter()[0].equals("blue_bin")){
                    PS_ATTRACTOR.setValue(curr_time, 9.0, 6.0);
                }
                else
                if(plan.getParameter()[0].equals("red_attractor")){
                    PS_ATTRACTOR.setValue(curr_time, 6.0, -5.5);
                }
                else
                if(plan.getParameter()[0].equals("green_attractor")){
                    PS_ATTRACTOR.setValue(curr_time, 6.0, 0.0);
                }
                else
                if(plan.getParameter()[0].equals("blue_attractor")){
                    PS_ATTRACTOR.setValue(curr_time, 6.0, 6.0);
                }
            }
             else
                if(action.startsWith("wait"))
            {
                System.out.println("ROBO VERDE-wait");
            }
             else
            if(action.startsWith("send_message(am_at_attractor")){
                try
                {                        
                    LongMessage idMessage = new LongMessage();
                    idMessage.val = abstract_robot.getPlayerNumber(curr_time);
                    abstract_robot.unicast(other_id, idMessage);
                    StringMessage message = new StringMessage();
                    message.val = "Pronto";
                    abstract_robot.unicast(other_id, message);
                }
            catch(CommunicationException e){}  
            }
            else if(action.startsWith("wander")) PF_OTHER_ROBOT.setValue(false);
        }
        
        if(bufferMessage.hasMoreElements()) 
        {
            try
            {
                LongMessage idMessage = (LongMessage) bufferMessage.nextElement();
                if(idMessage.val == other_id && bufferMessage.hasMoreElements()) 
                {
                    StringMessage message = (StringMessage)bufferMessage.nextElement();
                    if(message.val == "Pronto") 
                    {
                        PF_OTHER_ROBOT.setValue(true);
                    }
                }
            }
            catch(ClassCastException e){}
        }
        return(CSSTAT_OK);
    }
}

