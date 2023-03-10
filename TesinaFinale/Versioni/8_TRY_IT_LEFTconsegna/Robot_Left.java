

import EDU.gatech.cc.is.abstractrobot.*;
import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.communication.*;

import CSAI.unipa.clay.*;
import CSAI.unipa.abstractrobot.*;
import CSAI.unipa.SGolog.*;
import CSAI.unipa.knowledgment.*;
import CSAI.unipa.communication.PointMessage;

import java.awt.*;
import java.io.*;
import java.util.Enumeration;
import java.util.Date;
import java.util.Random;

public class Robot_Left extends ControlSystemMFN150Explore{
    public final static boolean DEBUG = false;

    private NodeVec2            turret_configuration;
    private NodeVec2            steering_configuration;
    private NodeBoolean         sonar_configuration;
    private NodeDouble          gripper_fingers_configuration;
    private NodeBoolean         laser_configuration;

    private NodeBooleanDinamic PF_EXIT_STALL; //trigger per gestire le situazioni di stallo
    private NodeBooleanDinamic PF_NOT_ANTIROOM,PF_NOT_ROOM;
    private NodeGain            mtggain = new NodeGain(1.0);
    private NodeGain            swirlgain = new NodeGain(0.7);
    private NodeGain            avoidgain = new NodeGain(0.8);
    private NodeGain            noisegain = new NodeGain(0.3);
    private NodeInt             state_monitor;
    private NodeGain             gripper=new NodeGain(1.0);
    private i_FSA_ba            STATE_MACHINE;
    private NodeVec2Array       PS_SONAR;
    private NodeVec2            PS_RED_BIN;
    private NodeVec2            PS_GREEN_BIN;
    private NodeVec2            PS_BLUE_BIN;
    private NodeVec2            PS_RED_ATTRACTOR;
    private NodeVec2            PS_GREEN_ATTRACTOR;
    private NodeVec2            PS_BLUE_ATTRACTOR;
    private NodeVec2            PS_CLOSEST_FLAG;
   	private NodeInt             PS_IN_GRIPPER;
    private v_DinamicPoint_     PS_ATTRACTOR,PS_SPECIAL_ATTRACTOR,PS_ATTRACTOR_FLAG;
    private NodeVec2            PS_GLOBAL_POS;
    private v_ChooseZone_mv_Try_it CHOOSE_ZONE;
    private Random              random;
    private NodeBooleanDinamic  PF_OTHER_ROBOT,PF_STALL,PF_ROOM,PF_ENTRY,PF_EXIT_CENTRE,PF_CENTRE;
    private NodeBoolean         PF_CLOSE_TO,PF_SOMETHING_IN_GRIPPER;
    private NodeBoolean         PF_CLOSE_ZONE_FLAG;
    private NodeBooleanDinamic  PF_DEADLOCK,PF_EMPTY_MEMORY,PF_NOT_EMPTY_MEMORY,PF_EXIT;
    private NodeBooleanDinamic  PF_GOTO_SPECIAL_ZONE,PF_STOP_GOTO_SPECIAL_ZONE,PF_EQUAL_FLAG;
    private NodeBooleanDinamic  PF_TIME_OUT,PF_LAB,PF_COME_FROM_LOW,PF_BOUNDS;
    private NodeBooleanDinamic  PF_IN_LOW_LAB,PF_IN_CENTRE_LAB,PF_HAVE_FLAG,PF_HAVE_FLAG_LAB,PF_IN_LAB;
   	private NodeBooleanDinamic	PF_IN_HIGH_LAB,PF_STALL_FLAG,PF_USE_LAB,PF_USE_LAB_RED_BLUE;
    private NodeBooleanDinamic PF_EXPLORE_CELLA,PF_NEXT_CELLA;
    private NodeBooleanDinamic PF_NOT_LABIRINTO; //scatta nel momento in cui un robot non puo' piu' visitare un labirinto (tempo esaurito)
    private Enumeration bufferMessage;
    private sGologClient        client;
    private SGologPlan          bring_plan,lab_plan,explore_plan;
    private NodeMap             MAP;
    private NodeMemory_Try_it   flag_memory;
    private v_Closest_var_Try_it flag_closest;
    private NodeInt MIOPUNTEGGIO;
    private static Res MOSTRA =new Res("Try_it");

    //protected static Report MOSTRA_REPORT =new Report("Report");
    protected static ReportCelle STATUS_CELLE =new ReportCelle("Report Celle");
    private Date       stall_ms;
    private Date       stall1_ms;
    private int         id;
    private int         other_id;
    private int         count_3=0,count_7=0;
    private int         count_in_room=0,count_in_centre=0;
    private int         stall=0,stall9=0;
    private int         tempo=0;
    private double old_teta=0.0;
    private long        idFlag_other;
    private Vec2  flag;
    private v_LinearAttraction_v_Try_it linear_attraction,linear_attraction2;
    private Location localizer;
    private RoomLocation roomLocation;
   	private boolean begin_flag=true;
    private  boolean red_room_Left=false,green_room_Left=false,blue_room_Left=false;
    private  boolean red_room_Right=false,green_room_Right=false,blue_room_Right=false,lab_room=false;
    private boolean    come_from_low=true;
    private static boolean lab=false;
	private boolean delete =true;  // Variabile utilizzata per eliminare una bandiera dalla memoria
    private boolean start1 =true;
    private boolean start =true;
    private Date   time_start9_ms;
    private boolean begin8=true;
    private boolean begin1=true;
    private boolean begin4=true;
    private Stalling antistallo;
    private Vec2 lab_flag=new Vec2(0,0);
    private boolean onlyOneTime=false; //serve per fare una determinata cosa (ad esempio stampare) una sola volta nel takeStep()
    private double punto[]=new double[4]; //serve per l'esplorazione di celle non visitate
    private double percorso_consegna[]=new double[8];
    private boolean check=false;
     /**
     * Configure the control system using Clay.
     */


    public void configure(){

        SimpleInterface ab=abstract_robot;
        stall_ms=new Date();
        MIOPUNTEGGIO = new i_myscore_r(abstract_robot,"Try_it");

        //======
        // Set some initial hardware configurations.
        //======

        id = abstract_robot.getPlayerNumber(abstract_robot.getTime());
        random=new Random();
    switch(id){
     	case 0:
     		other_id=1;
     		break;
     	case 1:
     		other_id=0;
     		break;
     	case 2:
     		other_id=3;
     		break;
     	case 3:
     		other_id=2;
     		break;
     }

        abstract_robot.setObstacleMaxRange(3.0);
        abstract_robot.setBaseSpeed(1.0*abstract_robot.MAX_TRANSLATION);
        flag=new Vec2();

        //abbassamento dell'artiglio
        abstract_robot.setGripperHeight(-1,0);

        //modalit??? trigger
        abstract_robot.setGripperFingers(-1,-1);

        File eclipseProgram = new File(".\\Database_Try_it\\sGolog_Try_it.pl");

        client = new sGologClient(eclipseProgram);

        eclipseProgram = new File(".\\Database_Try_it\\Competition_Try_it.pl");

        client.consultFile(eclipseProgram);

        bring_plan = new SGologPlan(abstract_robot);

        explore_plan = new SGologPlan(abstract_robot);

        lab_plan = new SGologPlan(abstract_robot);

        MAP = new NodeMap(0.0, 44.0,30.0, 0.0,0.8,0.7,"Mappa",false,abstract_robot);


	    flag_memory=new NodeMemory_Try_it(true,2,1,0,ab);

	    localizer=new Location();

	    antistallo= new Stalling();
        antistallo.resetI(); //setta a 0 la variabile i utile per l'antistallo     

        bufferMessage = abstract_robot.getReceiveChannel();



        //======
        // perceptual schemas
        //======
        //--- robot's global position

        PS_GLOBAL_POS = new v_GlobalPosition_r(abstract_robot);

        roomLocation=new RoomLocation(PS_GLOBAL_POS);

    //   NodeVec2
        CHOOSE_ZONE=new v_ChooseZone_mv_Try_it(MAP,PS_GLOBAL_POS,0,2,1,3);
        NodeVec2 PS_CHOOSE_ZONE=CHOOSE_ZONE;

	    NodeVec2 bin_red=new v_FixedPoint_(2.5, 23);
	    NodeVec2 bin_green=new v_FixedPoint_(2.5, 14);
	    NodeVec2 bin_blue=new v_FixedPoint_(2.5,5);

	    flag_closest=new v_Closest_var_Try_it(flag_memory,ab,bin_red,bin_green,bin_blue);

        //--- obstacles
        NodeVec2Array // the sonar readings
        PS_OBS = new va_Obstacles_r(abstract_robot);

        PS_SONAR = new va_SonarSensed_r(abstract_robot);

        NodeVec2Array
        PS_ALL_OBS = new va_Merge_vava(PS_SONAR, PS_OBS);

        PS_ATTRACTOR = new v_DinamicPoint_(PS_GLOBAL_POS, 0.0, 0.0);

        PS_ATTRACTOR_FLAG= new v_DinamicPoint_(PS_GLOBAL_POS,27.0,30.0);

        PS_SPECIAL_ATTRACTOR= new v_DinamicPoint_(PS_GLOBAL_POS,0.0,0.0);

        NodeVec2
        PS_CLOSEST_ZONE = new v_Closest_mr(MAP, abstract_robot);



        //--- targets of visual class 0
        NodeVec2Array
        PS_RED_FLAG_EGO =
                new va_VisualObjects_r(2,abstract_robot);

        //--- targets of visual class 1
        NodeVec2Array
        PS_GREEN_FLAG_EGO =
                new va_VisualObjects_r(1,abstract_robot);

        //--- targets of visual class 1
        NodeVec2Array
        PS_BLUE_FLAG_EGO =
                new va_VisualObjects_r(0,abstract_robot);

        NodeVec2Array
        PS_TEMP = new va_Merge_vava(PS_GREEN_FLAG_EGO, PS_RED_FLAG_EGO);

        NodeVec2Array
        PS_TEMP2 = new va_Merge_vav(PS_TEMP, PS_ATTRACTOR_FLAG);

        NodeVec2Array
        PS_ALL_TARGET = new va_Merge_vava(PS_TEMP, PS_BLUE_FLAG_EGO);

  //      NodeVec2
        PS_CLOSEST_FLAG = new v_Closest_va(PS_ALL_TARGET);


        //--- type of object in the gripper
    //    NodeInt
        PS_IN_GRIPPER = new i_InGripper_r(abstract_robot);





        //======
        // Perceptual Features
        //======


        PF_CLOSE_TO = new b_CloseDinamic_vv(0.4, PS_GLOBAL_POS, PS_ATTRACTOR);

        NodeBoolean
        PF_SOMETHING_VISIBLE=new b_NonZero_v(PS_CLOSEST_FLAG);

        NodeBoolean
        PF_SOMETHING_VISIBLE_MEMORY=new b_NonZero_v(PS_ATTRACTOR_FLAG);

        NodeBoolean
        PF_FLAG_VISIBLE=new b_NonZero_v(PS_ATTRACTOR_FLAG);

        PF_OTHER_ROBOT = new NodeBooleanDinamic(false);

        NodeBoolean
        PF_RED_FLAG_IN_GRIPPER = new b_Equal_i(2, PS_IN_GRIPPER);

        NodeBoolean
        PF_GREEN_FLAG_IN_GRIPPER = new b_Equal_i(1, PS_IN_GRIPPER);

        NodeBoolean
        PF_BLUE_FLAG_IN_GRIPPER = new b_Equal_i(0, PS_IN_GRIPPER);


        PF_SOMETHING_IN_GRIPPER = new b_Persist_s (4.0,new b_NonNegative_s(PS_IN_GRIPPER));

        NodeBoolean
        PF_NOT_SOMETHING_IN_GRIPPER = new b_Not_s(PF_SOMETHING_IN_GRIPPER);

        NodeBoolean
        PF_NOT_SOMETHING_VISIBLE= new b_Not_s(PF_SOMETHING_VISIBLE);

        NodeBoolean
        PF_NOT_SOMETHING_VISIBLE_MEMORY= new b_Not_s(PF_SOMETHING_VISIBLE_MEMORY);

        PF_CLOSE_ZONE_FLAG = new b_CloseDinamic_vv(0.6, PS_GLOBAL_POS, PS_ATTRACTOR_FLAG);

        PF_DEADLOCK=new NodeBooleanDinamic(false);

        PF_STALL=new NodeBooleanDinamic(false);

        PF_STALL_FLAG=new NodeBooleanDinamic(false);

        PF_TIME_OUT=new NodeBooleanDinamic(false);

        PF_EMPTY_MEMORY=new NodeBooleanDinamic(false);

        PF_ROOM=new NodeBooleanDinamic(false);

        PF_LAB=new NodeBooleanDinamic(false);

        PF_USE_LAB=new NodeBooleanDinamic(false);

        PF_USE_LAB_RED_BLUE=new NodeBooleanDinamic(false);

        PF_COME_FROM_LOW=new NodeBooleanDinamic(false);

        PF_IN_LOW_LAB=new NodeBooleanDinamic(false);

        PF_IN_HIGH_LAB=new NodeBooleanDinamic(false);

        PF_IN_CENTRE_LAB=new NodeBooleanDinamic(false);

        PF_HAVE_FLAG=new NodeBooleanDinamic(false);

        PF_HAVE_FLAG_LAB=new NodeBooleanDinamic(false);

        PF_IN_LAB=new NodeBooleanDinamic(false);

        PF_BOUNDS=new NodeBooleanDinamic(false);

        PF_EXPLORE_CELLA=new NodeBooleanDinamic(false);

        PF_NEXT_CELLA=new NodeBooleanDinamic(false);



        NodeBoolean
        PF_NOT_ROOM= new b_Not_s(PF_ROOM);

        PF_CENTRE=new NodeBooleanDinamic(false);

        NodeBoolean
        PF_NOT_CENTRE= new b_Not_s(PF_CENTRE);


        PF_ENTRY=new NodeBooleanDinamic(false);

        PF_NOT_EMPTY_MEMORY = new NodeBooleanDinamic(true);

        PF_EXIT=new NodeBooleanDinamic(false);

        PF_EXIT_CENTRE=new NodeBooleanDinamic(false);


        PF_GOTO_SPECIAL_ZONE=new NodeBooleanDinamic(false);

        PF_STOP_GOTO_SPECIAL_ZONE=new NodeBooleanDinamic(false);

        PF_EQUAL_FLAG=new NodeBooleanDinamic(false);

        NodeBoolean
        PF_ANY_MEMORY_AND_NOT_ROOM=new b_And_bb(PF_NOT_ROOM,PF_NOT_EMPTY_MEMORY);

        NodeBoolean
        PF_LAB_ENTRY=new b_And_bb(PF_LAB,PF_CLOSE_TO);

        NodeBoolean
        PF_EXIT_EXPLORE=new b_Or_bb(PF_SOMETHING_VISIBLE,PF_CLOSE_TO);

        NodeBoolean
        PF_CLOSE_TO_OR_IN_LAB=new b_Or_bb(PF_IN_LAB,PF_CLOSE_TO);

        NodeBoolean
        PF_FLAG_OR_STALL=new b_Or_bb(PF_SOMETHING_IN_GRIPPER,PF_STALL_FLAG);

        NodeBoolean
        PF_CLOSE_ZONE_FLAG_OR_SOMETHING_VISIBLE=new b_Or_bb(PF_SOMETHING_VISIBLE,PF_CLOSE_ZONE_FLAG);

        //CREO UN NUOVO TRIGGER PER GESTIRE LE SITUAZIONI DI STALLO
        PF_EXIT_STALL=new NodeBooleanDinamic(false);



        //CREO UN NUOVO TRIGGER PER GESTIRE LE SITUAZIONI IN CUI UN ROBOT NON PUO' PIU' VISITARE UN'ANTICAMERA

        PF_NOT_ANTIROOM=new NodeBooleanDinamic(false);
        PF_NOT_ROOM=new NodeBooleanDinamic(false);
        PF_NOT_LABIRINTO=new NodeBooleanDinamic(false);


        //======
        // motor schemas
        //======

        // avoid obstacles
        NodeVec2
        MS_AVOID_OBSTACLES = new v_AvoidSonar_va(3.0, abstract_robot.RADIUS + 0.1,
                PS_OBS, PS_GLOBAL_POS);

        NodeVec2
        MS_MOVE_TO_FLAG = new v_LinearAttraction_v(0.4,0.0,PS_CLOSEST_FLAG);

        NodeVec2
        MS_MOVE_TO_ZONE_FLAG = new v_LinearAttraction_v( 0.4, 0.0, PS_ATTRACTOR_FLAG);

        // noise vector
        NodeVec2
        MS_NOISE_VECTOR = new v_Noise_(10,5);

        NodeVec2
        MS_SWIRL_CLOSEST_ZONE = new v_Swirl_vav(1.5, abstract_robot.RADIUS + 0.18,
                                            PS_OBS, PS_CLOSEST_ZONE);

        NodeVec2
        MS_SWIRL_TO = new v_Swirl_vav(1.5, abstract_robot.RADIUS + 0.18,
                                           PS_OBS, PS_ATTRACTOR);

   //   v_LinearAttraction_v_Try_it
        linear_attraction=new v_LinearAttraction_v_Try_it(0.4,0.0,PS_SPECIAL_ATTRACTOR);

        NodeVec2
        MS_MOVE_TO_SPECIAL_ZONE=linear_attraction;

        linear_attraction2=new	v_LinearAttraction_v_Try_it(0.4,0.0,PS_ATTRACTOR);

        NodeVec2
        MS_MOVE_TO = linear_attraction2;

        NodeVec2
        MS_MOVE_TO_CHOOSE_ZONE=new v_LinearAttraction_v(0.4,0.0,PS_CHOOSE_ZONE);

        //======
        // AS_NOT_ANTIROOM
        //======
        v_StaticWeightedSum_va
                AS_NOT_ANTIROOM = new v_StaticWeightedSum_va();


        AS_NOT_ANTIROOM.weights[0]  = avoidgain.Value(abstract_robot.getTime());
        AS_NOT_ANTIROOM.embedded[0] = MS_AVOID_OBSTACLES;

        AS_NOT_ANTIROOM.weights[1]  = noisegain.Value(abstract_robot.getTime());
        AS_NOT_ANTIROOM.embedded[1] = MS_NOISE_VECTOR;

        AS_NOT_ANTIROOM.weights[2]  = mtggain.Value(abstract_robot.getTime());
        AS_NOT_ANTIROOM.embedded[2] = MS_MOVE_TO;

        //======
        // AS_GOTO
        //======
        v_StaticWeightedSum_va
        AS_GOTO = new v_StaticWeightedSum_va();

        AS_GOTO.weights[0]  = avoidgain.Value(abstract_robot.getTime());
        AS_GOTO.embedded[0] = MS_AVOID_OBSTACLES;

        AS_GOTO.weights[1]  = noisegain.Value(abstract_robot.getTime());
        AS_GOTO.embedded[1] = MS_NOISE_VECTOR;

        AS_GOTO.weights[2]  = mtggain.Value(abstract_robot.getTime());
        AS_GOTO.embedded[2] = MS_MOVE_TO;

        AS_GOTO.embedded[3] = MS_SWIRL_TO;
        AS_GOTO.weights[3] = swirlgain.Value(abstract_robot.getTime());


        //======
        // AS_GOTO_ZONE_FLAG
        //======
        v_StaticWeightedSum_va
        AS_GO_ZONE_FLAG = new v_StaticWeightedSum_va();


        AS_GO_ZONE_FLAG.weights[0]  = avoidgain.Value(abstract_robot.getTime());
        AS_GO_ZONE_FLAG.embedded[0] = MS_AVOID_OBSTACLES;

        AS_GO_ZONE_FLAG.weights[1]  = noisegain.Value(abstract_robot.getTime());
        AS_GO_ZONE_FLAG.embedded[1] = MS_NOISE_VECTOR;

        AS_GO_ZONE_FLAG.weights[2]  = mtggain.Value(abstract_robot.getTime());
        AS_GO_ZONE_FLAG.embedded[2] = MS_MOVE_TO_ZONE_FLAG;


        //======
        // AS_GOTO_ZONE_FLAG
        //======
        v_StaticWeightedSum_va
        AS_GO_FLAG = new v_StaticWeightedSum_va();
        

        
        
   /*     AS_GO_FLAG.weights[0]  = avoidgain.Value(abstract_robot.getTime());  
        AS_GO_FLAG.embedded[0] = MS_AVOID_OBSTACLES;

        AS_GO_FLAG.weights[1]  = noisegain.Value(abstract_robot.getTime());
        AS_GO_FLAG.embedded[1] = MS_NOISE_VECTOR;
*/
        AS_GO_FLAG.weights[0]  = mtggain.Value(abstract_robot.getTime());
        AS_GO_FLAG.embedded[0] = MS_MOVE_TO_FLAG;


        //======
        // AS_GOTO_SPECIAL_ZONE
        //======
        v_StaticWeightedSum_va
        AS_GOTO_SPECIAL_ZONE = new v_StaticWeightedSum_va();


        AS_GOTO_SPECIAL_ZONE.weights[0]  = avoidgain.Value(abstract_robot.getTime());
        AS_GOTO_SPECIAL_ZONE.embedded[0] = MS_AVOID_OBSTACLES;

        AS_GOTO_SPECIAL_ZONE.weights[1]  = noisegain.Value(abstract_robot.getTime());
        AS_GOTO_SPECIAL_ZONE.embedded[1] = MS_NOISE_VECTOR;

        AS_GOTO_SPECIAL_ZONE.weights[2]  = mtggain.Value(abstract_robot.getTime());
        AS_GOTO_SPECIAL_ZONE.embedded[2] = MS_MOVE_TO_SPECIAL_ZONE;


        /************
         AS GO CHOOSE ZONE
        *************/

        v_StaticWeightedSum_va
        AS_GO_CHOOSE_ZONE =  new v_StaticWeightedSum_va();

        AS_GO_CHOOSE_ZONE.embedded[0] = MS_MOVE_TO_CHOOSE_ZONE;
        AS_GO_CHOOSE_ZONE.weights[0] = mtggain.Value(abstract_robot.getTime());

        AS_GO_CHOOSE_ZONE.embedded[1] = MS_AVOID_OBSTACLES;
        AS_GO_CHOOSE_ZONE.weights[1] = avoidgain.Value(abstract_robot.getTime());

        AS_GO_CHOOSE_ZONE.embedded[2] = MS_NOISE_VECTOR;
        AS_GO_CHOOSE_ZONE.weights[2] = 0.5;


        AS_GO_CHOOSE_ZONE.embedded[3] = MS_SWIRL_CLOSEST_ZONE;
        AS_GO_CHOOSE_ZONE.weights[3] = swirlgain.Value(abstract_robot.getTime());



		bring_plan.debugging(false);

		bring_plan.addAction("exit",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO,
                       0,
                       new int[]{0,4,8,12},
                       NodeAction.LASER_ON);


        bring_plan.addAction("goto",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);
        
         bring_plan.addAction("first_step_red",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("second_step_red",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("third_step_red",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("fourth_step_red",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("calculate_route_red",
                        NodeAction.NO_STEERING,
                        NodeAction.NO_TURRET,
                        NodeAction.IMMEDIATE_TRIGGER,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);

         bring_plan.addAction("first_step_green",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("second_step_green",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("third_step_green",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("fourth_step_green",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("calculate_route_green",
                        NodeAction.NO_STEERING,
                        NodeAction.NO_TURRET,
                        NodeAction.IMMEDIATE_TRIGGER,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);

         bring_plan.addAction("first_step_blue",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("second_step_blue",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("third_step_blue",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("fourth_step_blue",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        bring_plan.addAction("calculate_route_blue",
                        NodeAction.NO_STEERING,
                        NodeAction.NO_TURRET,
                        NodeAction.IMMEDIATE_TRIGGER,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);

       bring_plan.addAction("goto_lab",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO,
                       0,
                       new int[]{0,4,8,12},
                       NodeAction.LASER_ON);

       bring_plan.addAction("open_gripper",
                       NodeAction.NO_STEERING,
                       NodeAction.NO_TURRET,
                       NodeAction.IMMEDIATE_TRIGGER,
                       1,
                       NodeAction.NO_SONAR,
                       NodeAction.NO_LASER);

        bring_plan.addAction("send_message",
                        NodeAction.NO_STEERING,
                        NodeAction.NO_TURRET,
                        NodeAction.IMMEDIATE_TRIGGER,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);

        bring_plan.addAction("reset_timer",
                        NodeAction.NO_STEERING,
                        NodeAction.NO_TURRET,
                        NodeAction.IMMEDIATE_TRIGGER,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);

        bring_plan.addAction("wait",
                        AS_GOTO,
                        AS_GOTO,
                        PF_OTHER_ROBOT,
                        NodeAction.NO_GRIPPER,
                        NodeAction.NO_SONAR,
                        NodeAction.NO_LASER);


        bring_plan.addAction("room", PF_ROOM);

        bring_plan.addAction("red_flag", PF_RED_FLAG_IN_GRIPPER);

        bring_plan.addAction("green_flag", PF_GREEN_FLAG_IN_GRIPPER);

        bring_plan.addAction("blue_flag", PF_BLUE_FLAG_IN_GRIPPER);

        bring_plan.addAction("can_go", PF_OTHER_ROBOT);

        bring_plan.addAction("in_lab", PF_IN_LAB);

        bring_plan.addAction("in_low_lab", PF_IN_LOW_LAB);

        bring_plan.addAction("in_high_lab", PF_IN_HIGH_LAB);

		bring_plan.addAction("in_centre_lab", PF_IN_CENTRE_LAB);

		bring_plan.addAction("have_flag", PF_HAVE_FLAG);

		bring_plan.addAction("use_lab", PF_USE_LAB);

		bring_plan.addAction("use_lab_red_blue", PF_USE_LAB_RED_BLUE);

        bring_plan.setPlan(client.invoke("do(bring_flag,s0,S)"),2);

        explore_plan.debugging(false);

        explore_plan.addAction("goto",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO_OR_IN_LAB,
                       0,
                       new int[]{0,2,4,6,8,10,12,14},
                       NodeAction.LASER_ON);

        explore_plan.setPlan(client.invoke("do(visita_cella_libera,s0,S)"),10);


        lab_plan.debugging(false);

		lab_plan.addAction("goto_lab",
                       AS_GOTO,
                       AS_GOTO,
                       PF_CLOSE_TO,
                       0,
                       new int[]{0,4,8,12},
                       NodeAction.LASER_ON);

 		lab_plan.addAction("go_flag",
                       AS_GO_FLAG,
                       AS_GO_FLAG,
    			       PF_FLAG_OR_STALL,
                       -1,
                       NodeAction.NO_SONAR,
                       NodeAction.NO_LASER);

        lab_plan.addAction("explore",
                        AS_GOTO,
                        AS_GOTO,
                        PF_EXIT_EXPLORE,
                        NodeAction.NO_GRIPPER,
                        new int[]{0,2,4,6,8,10,12,14},
                        NodeAction.NO_LASER);

		lab_plan.addAction("something_visible_lab", PF_SOMETHING_VISIBLE);
		lab_plan.addAction("come_from_low", PF_COME_FROM_LOW);
		lab_plan.addAction("in_low_lab", PF_IN_LOW_LAB);
		lab_plan.addAction("in_high_lab", PF_IN_HIGH_LAB);
		lab_plan.addAction("in_centre_lab", PF_IN_CENTRE_LAB);
		lab_plan.addAction("have_flag", PF_HAVE_FLAG);
        lab_plan.setPlan(client.invoke("do(explore_lab,s0,S)"),8);



        //======
        // STATE_MACHINE
        //======
        STATE_MACHINE = new i_FSA_ba();


        STATE_MACHINE.state = 0;


        STATE_MACHINE.triggers[0][0]  = PF_CLOSE_TO;     //Stato 0:ATTACK
        STATE_MACHINE.follow_on[0][0] = 1;



        // Stato 1:   
        STATE_MACHINE.triggers[1][0]  = PF_CLOSE_ZONE_FLAG_OR_SOMETHING_VISIBLE ;
        STATE_MACHINE.follow_on[1][0] = 5;

        STATE_MACHINE.triggers[1][1]=PF_DEADLOCK;
        STATE_MACHINE.follow_on[1][1]=1;

        STATE_MACHINE.triggers[1][2]=PF_EMPTY_MEMORY;
        STATE_MACHINE.follow_on[1][2]=4;

        STATE_MACHINE.triggers[1][3]=PF_GOTO_SPECIAL_ZONE;
        STATE_MACHINE.follow_on[1][3]=3;

        STATE_MACHINE.triggers[1][4]=PF_EXIT;
        STATE_MACHINE.follow_on[1][4]=6;

        STATE_MACHINE.triggers[1][5]=PF_ENTRY;
        STATE_MACHINE.follow_on[1][5]=7;



    	// Stato 2:
		STATE_MACHINE.triggers[2][0]  = bring_plan.isFinished(abstract_robot.getTime());
        STATE_MACHINE.follow_on[2][0] = 1;



        // Stato 3:
        STATE_MACHINE.triggers[3][0]  =  PF_CLOSE_ZONE_FLAG;
		STATE_MACHINE.follow_on[3][0] = 5;

		STATE_MACHINE.triggers[3][1]  = PF_STOP_GOTO_SPECIAL_ZONE;
		STATE_MACHINE.follow_on[3][1] = 1;



        // Stato 4:

        STATE_MACHINE.triggers[4][0]  =  PF_SOMETHING_VISIBLE; //ESCO SE C'E' QUALCOSA DI VISIBILE

        STATE_MACHINE.follow_on[4][0] = 1;



        STATE_MACHINE.triggers[4][1]  =  PF_EXIT; //ESCO SE SONO IN STALLO DA TANTO TEMPO

        STATE_MACHINE.follow_on[4][1] = 6;



        STATE_MACHINE.triggers[4][2]  =  PF_ANY_MEMORY_AND_NOT_ROOM; //ESCO SE LA MEMORIA NON E' VUOTA

        STATE_MACHINE.follow_on[4][2] = 1;



        STATE_MACHINE.triggers[4][3]  =  PF_ENTRY; //SI APPROSSIMA AL LABIRINTO

        STATE_MACHINE.follow_on[4][3] = 7;



        STATE_MACHINE.triggers[4][4]  =  PF_BOUNDS;

        STATE_MACHINE.follow_on[4][4] = 6;



        STATE_MACHINE.triggers[4][5]  =  PF_NOT_ANTIROOM; //SE SEI ALLO STATO 4 E SCATTA TALE TRIGGER VAI NELLO STATO (DSIR) 9

        STATE_MACHINE.follow_on[4][5] = 9;


        STATE_MACHINE.triggers[4][6]  =  PF_NOT_ROOM;

        STATE_MACHINE.follow_on[4][6] = 1;


        STATE_MACHINE.triggers[4][7]  =  PF_EXPLORE_CELLA;

        STATE_MACHINE.follow_on[4][7] = 10;



        // Stato 5:
        STATE_MACHINE.triggers[5][0]  =  PF_SOMETHING_IN_GRIPPER;
		STATE_MACHINE.follow_on[5][0] = 2;

    	STATE_MACHINE.triggers[5][1]  =  PF_STALL;
    	STATE_MACHINE.follow_on[5][1] = 1;



        // Stato 6:
        STATE_MACHINE.triggers[6][0]=PF_CLOSE_TO;
        STATE_MACHINE.follow_on[6][0]=1;

        STATE_MACHINE.triggers[6][1]=PF_EXIT_STALL; //SE SIAMO ALLO STATO 6 PER PIU' DI UN CERTO TOT DI TEMPO ALLO SCATTA IL TRIGGER CHE CI PORTA ALLO STATO 1
        STATE_MACHINE.follow_on[6][1]=1;



        // Stato 7:
		STATE_MACHINE.triggers[7][0]  =  PF_LAB_ENTRY;
		STATE_MACHINE.follow_on[7][0] = 8;

        STATE_MACHINE.triggers[7][1]=PF_CLOSE_TO;
        STATE_MACHINE.follow_on[7][1]=1;   //era 4

        STATE_MACHINE.triggers[7][2]=PF_NOT_LABIRINTO;
        STATE_MACHINE.follow_on[7][2]=4;   //era 4



   		// Stato 8:
        STATE_MACHINE.triggers[8][0]  = lab_plan.isFinished(abstract_robot.getTime());
        STATE_MACHINE.follow_on[8][0] = 2;

        STATE_MACHINE.triggers[8][0]  = PF_EXIT_STALL;
        STATE_MACHINE.follow_on[8][0] = 4;

        // Stato 9: Se un robot si trova in tale stato, vuol dire che non pu?? visitare una certa anticamera

        STATE_MACHINE.triggers[9][0]  = PF_EXIT;
        STATE_MACHINE.follow_on[9][0] = 2;

        STATE_MACHINE.triggers[9][1]  = PF_EXIT_STALL;
        STATE_MACHINE.follow_on[9][1] = 2;

        //Stato 10: stato in cui un robot, mediante un punto attrattore, va nella prima cella non visitata di una zona
        STATE_MACHINE.triggers[10][0]  = explore_plan.isFinished(abstract_robot.getTime());
        STATE_MACHINE.follow_on[10][0] = 4; //torna in explore

        STATE_MACHINE.triggers[10][1]  =  PF_SOMETHING_VISIBLE; //ESCO SE C'E' QUALCOSA DI VISIBILE
        STATE_MACHINE.follow_on[10][1] = 1;

        STATE_MACHINE.triggers[10][2]  =  PF_ENTRY; //SI APPROSSIMA AL LABIRINTO

        STATE_MACHINE.follow_on[10][2] = 7;

        
        STATE_MACHINE.triggers[10][3]  =  PF_EXIT_STALL; //STALLO

        STATE_MACHINE.follow_on[10][3] = 2;



        state_monitor = STATE_MACHINE;





        //======
        // STEERING
        //======
        v_Select_vai
        STEERING = new v_Select_vai((NodeInt)STATE_MACHINE);


        STEERING.embedded[0] = AS_GOTO;
		STEERING.embedded[1] = AS_GO_ZONE_FLAG;
	    STEERING.embedded[2] = bring_plan.getSteering(abstract_robot.getTime());
        STEERING.embedded[3] = AS_GOTO_SPECIAL_ZONE;
		STEERING.embedded[4] = AS_GO_CHOOSE_ZONE;
       	STEERING.embedded[5] = AS_GO_ZONE_FLAG;
       	STEERING.embedded[6] = AS_GOTO;
       	STEERING.embedded[7] = AS_GOTO;
       	STEERING.embedded[8] = lab_plan.getSteering(abstract_robot.getTime());
        STEERING.embedded[9] = AS_NOT_ANTIROOM;
        STEERING.embedded[10] = explore_plan.getSteering(abstract_robot.getTime());


        //======
        // TURRET
        //======
        v_Select_vai
        TURRET = new v_Select_vai((NodeInt)STATE_MACHINE);

        TURRET.embedded[0] = AS_GOTO;
        TURRET.embedded[1] = AS_GO_ZONE_FLAG;
        TURRET.embedded[2] = bring_plan.getTurret(abstract_robot.getTime());
        TURRET.embedded[3] = AS_GOTO_SPECIAL_ZONE;
	    TURRET.embedded[4] = AS_GO_CHOOSE_ZONE;
	    TURRET.embedded[5] = AS_GO_ZONE_FLAG;
        TURRET.embedded[6] = AS_GOTO;
        TURRET.embedded[7] = AS_GOTO;
		TURRET.embedded[8] = lab_plan.getTurret(abstract_robot.getTime());
        TURRET.embedded[9] = AS_NOT_ANTIROOM;
        TURRET.embedded[10] = explore_plan.getTurret(abstract_robot.getTime());

        //======
        // GRIPPER_FINGERS
        //======
        d_Select_i
        GRIPPER_FINGERS = new d_Select_i(STATE_MACHINE);

        GRIPPER_FINGERS.embedded[0] = 1;
     	GRIPPER_FINGERS.embedded[1] = 1;
     	GRIPPER_FINGERS.embedded[2] = bring_plan.getGripperFingers(abstract_robot.getTime());
     	GRIPPER_FINGERS.embedded[3] = 1;
     	GRIPPER_FINGERS.embedded[4] =(int)gripper.Value(abstract_robot.getTime());
     	GRIPPER_FINGERS.embedded[5] = -1;
     	GRIPPER_FINGERS.embedded[6] = 0;
     	GRIPPER_FINGERS.embedded[7] = 0;
     	GRIPPER_FINGERS.embedded[8] = lab_plan.getGripperFingers(abstract_robot.getTime());
        GRIPPER_FINGERS.embedded[9] = 0;
        GRIPPER_FINGERS.embedded[10] = explore_plan.getGripperFingers(abstract_robot.getTime());

        //======
        // SONAR_CONFIGURATION
        //======
        d_SonarControl_ir
        SONAR_CONFIGURATION = new d_SonarControl_ir(STATE_MACHINE, abstract_robot);

        SONAR_CONFIGURATION.sonarActived[0] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[0] = SonarObjectSensor.MEDIUM;

        SONAR_CONFIGURATION.sonarActived[1] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[1] = SonarObjectSensor.MEDIUM;

		SONAR_CONFIGURATION.sonarActived[2] = bring_plan.getSonar(abstract_robot.getTime());
        SONAR_CONFIGURATION.sonarPrecision[2] = SonarObjectSensor.MEDIUM;

        SONAR_CONFIGURATION.sonarActived[3] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[3] = SonarObjectSensor.MEDIUM;

        SONAR_CONFIGURATION.sonarActived[4] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[4] = SonarObjectSensor.MEDIUM;

   		SONAR_CONFIGURATION.sonarActived[5] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[5] = SonarObjectSensor.MEDIUM;

        SONAR_CONFIGURATION.sonarActived[6] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[6] = SonarObjectSensor.MEDIUM;

   		SONAR_CONFIGURATION.sonarActived[7] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[7] = SonarObjectSensor.MEDIUM;

        SONAR_CONFIGURATION.sonarActived[8] = lab_plan.getSonar(abstract_robot.getTime());
        SONAR_CONFIGURATION.sonarPrecision[8] = SonarObjectSensor.MEDIUM;

        SONAR_CONFIGURATION.sonarActived[9] = new int[]{0,4,8,12};
        SONAR_CONFIGURATION.sonarPrecision[9] = SonarObjectSensor.MEDIUM;

        SONAR_CONFIGURATION.sonarActived[10] = explore_plan.getSonar(abstract_robot.getTime());
        SONAR_CONFIGURATION.sonarPrecision[10] = SonarObjectSensor.MEDIUM;


        //======
        // LASER_CONFIGURATION
        //======
        b_Select_ir
        LASER_CONFIGURATION = new b_Select_ir(STATE_MACHINE, abstract_robot);

        LASER_CONFIGURATION.embedded[0] = false;
  		LASER_CONFIGURATION.embedded[1] = false;
 		LASER_CONFIGURATION.embedded[2] = bring_plan.getLaser(abstract_robot.getTime());
		LASER_CONFIGURATION.embedded[3] = false;
		LASER_CONFIGURATION.embedded[4] = false;
		LASER_CONFIGURATION.embedded[5] = false;
		LASER_CONFIGURATION.embedded[6] = false;
		LASER_CONFIGURATION.embedded[7] = false;
		LASER_CONFIGURATION.embedded[8] = lab_plan.getLaser(abstract_robot.getTime());
        LASER_CONFIGURATION.embedded[9] = false;
        LASER_CONFIGURATION.embedded[10] = explore_plan.getLaser(abstract_robot.getTime());


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
    public int takeStep() throws ArrayIndexOutOfBoundsException{
       
       /* try
        {
            //bla bla
        }
        catch(ArrayIndexOutOfBoundsException ex)
        {
            ex.printStackTrace();
        } */
        Vec2	result;
        double	dresult;
        boolean bresult;
        long	curr_time = abstract_robot.getTime();
        String  action;
        double x,y;
        double x_robot=PS_GLOBAL_POS.Value(curr_time).x;
        double y_robot=PS_GLOBAL_POS.Value(curr_time).y;

        //System.out.println("X: " + Math.round(x_robot*100.0)/100.0 + " ---- Y: "+ Math.round(y_robot*100.0)/100.0);

 		swirlgain.setValue(0.2);
        avoidgain.setValue(0.8);
    	noisegain.setValue(0.2);


    	MOSTRA.setScore(MIOPUNTEGGIO.Value(curr_time));

        int state=STATE_MACHINE.Value(curr_time);


       //ANTISTALLO DSIR - incremento la variabile i ad ogni takestep()
		antistallo.addI();
        
        
        //Controllo se il robot rimane troppo tempo fermo in una posizione e lo sblocco
        /*if(Math.abs(x_robot-PS_GLOBAL_POS.Value(curr_time).x)<0.1 && Math.abs(y_robot-PS_GLOBAL_POS.Value(curr_time).y)<0.1)
        {
            // incremento
            MOSTRA_REPORT.setReport("SONO FERMOOOOOOO");
            swirlgain.setValue(0.8);
            avoidgain.setValue(0.8);
            noisegain.setValue(0.8);

        } */
        //cuscinetti
       /* if(localizer.areClosedAllZones() && state!=2)
        {
            if(localizer.amInCella55()&&localizer.amInCella63())
            {
                punto=localizer.getPuntoCella(33);
                PS_ATTRACTOR.setValue(curr_time, punto[0],punto[1]);
                MOSTRA_REPORT.setReport("Cuscinetto attivo basso sx");
            }
            else if(localizer.amInCella62()&&localizer.amInCella70())
            {
                punto=localizer.getPuntoCella(53);
                PS_ATTRACTOR.setValue(curr_time, punto[0],punto[1]);
                MOSTRA_REPORT.setReport("Cuscinetto attivo basso dx");
            }
            else if(localizer.amInCella8()&&localizer.amInCella16())
            {
                punto=localizer.getPuntoCella(37);
                PS_ATTRACTOR.setValue(curr_time, punto[0],punto[1]);
                MOSTRA_REPORT.setReport("Cuscinetto attivo alto dx");
            }
            else if(localizer.amInCella1()&&localizer.amInCella9())
            {
                punto=localizer.getPuntoCella(18);
                PS_ATTRACTOR.setValue(curr_time, 11.5 , 23.0);
                MOSTRA_REPORT.setReport("Cuscinetto attivo alto sx");
            }
        }*/

        //INFORMAZIONI DI DEBUG
        //NOTA: HO NOTATO CHE STAMPANDO LE SEGUENTI INFORMAZIONI RALLENTA DI BRUTTO
        //System.out.println("Sono il robot avente id " + id + " e mi trovo nello stato " + state + "con timer pari a " + antistallo.getI());
        //System.out.println(antistallo.getI());
        //System.out.println("------ID ROBOT: " + id + " " + localizer.testPositionDsir(x_robot,y_robot) + "COORDINATE ROBOT: " + "(" + x_robot + ";" + y_robot + ")");
        localizer.setPositionRobot(x_robot,y_robot);

        localizer.debugDSIR(id,STATUS_CELLE);
        //localizer.debugZoneDSIR();

        flag_memory.setValue(curr_time);
    try{
        MAP.setCellVisited(curr_time);

        MAP.setObstacle(PS_SONAR.Value(curr_time), abstract_robot.getPrecisionRivelation(), curr_time);
    
        Vec2 position=MAP.getPosition(curr_time);
        roomLocation.Value(curr_time);
    }catch(Exception e){
        e.printStackTrace();
    }
        // PF usati per mappare fluenti golog
 		if(roomLocation.in_lab_down()){
 			PF_IN_LOW_LAB.setValue(true);
 		}else PF_IN_LOW_LAB.setValue(false);

		if(roomLocation.in_lab_top()){
 			PF_IN_HIGH_LAB.setValue(true);
 		}else PF_IN_HIGH_LAB.setValue(false);

		if(roomLocation.in_lab_middle()){
 			PF_IN_CENTRE_LAB.setValue(true);
 		}else PF_IN_CENTRE_LAB.setValue(false);



 		if(roomLocation.in_lab()){
 			PF_IN_LAB.setValue(true);
 		}else PF_IN_LAB.setValue(false);


 		// Messagistica

		if(bufferMessage.hasMoreElements())
        {

            try
            {
                LongMessage idMessage = (LongMessage) bufferMessage.nextElement();
                if(idMessage.val == other_id && bufferMessage.hasMoreElements())
                {
                    StringMessage message = (StringMessage)bufferMessage.nextElement();
                    /*if(message.val=="Bandiera nuova")
                    {
                    	PointMessage flagPosition=(PointMessage) bufferMessage.nextElement();
                    	Vec2 pos=new Vec2 (flagPosition.xValue,flagPosition.yValue); 		
                    	LongMessage visualMessage =(LongMessage) bufferMessage.nextElement();
                    	int vis=(int)visualMessage.val;
                    	flag_memory.push(pos,vis,curr_time);
                    }
                    else if(message.val=="Delete Flag")
                    {
                    	PointMessage flagPosition=(PointMessage) bufferMessage.nextElement();
                    	Vec2 pos=new Vec2 (flagPosition.xValue,flagPosition.yValue);
                    	flag_memory.pop_only(pos,curr_time); 	
                    }
                    else */
                     if(message.val== "Pronto")
                    {

                        LongMessage idFlag=(LongMessage) bufferMessage.nextElement();
                        idFlag_other=idFlag.val;
                    /*	if(idFlag.val==PS_IN_GRIPPER.intValue(curr_time)){
                    		PF_EQUAL_FLAG.setValue(true);
                    	}
                    	else PF_EQUAL_FLAG.setValue(false);
                    */
                    	PF_OTHER_ROBOT.setValue(true);

            		}
                }
           }
            catch(ClassCastException e){}

        }
        if(roomLocation.in_room()){
        	PF_ROOM.setValue(true);
        }
        else PF_ROOM.setValue(false);

       /*  //------PER TESTARE IL FATTO CHE UN ROBOT NON ENTRI NELLE STANZE DECOMMENTARE LE SEGUENTI RIGHE DI CODICE

         //-----Inizio controllo stanze
            if(roomLocation.in_blue_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 9, 3);
                MOSTRA_REPORT.setReport("Trasu e niescio");
            
            }
            else if(roomLocation.in_red_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 9, 25.0);
                MOSTRA_REPORT.setReport("Trasu e niescio");
              
            }
            else if(roomLocation.in_green_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 12.0, 14.0);
                MOSTRA_REPORT.setReport("Trasu e niescio");
            
            }
            else if(roomLocation.in_red_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 35.3, 25.0);
                MOSTRA_REPORT.setReport("Trasu e niescio");
              
            }

            else if(roomLocation.in_green_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 32.0, 14.0);
                MOSTRA_REPORT.setReport("Trasu e niescio");
               
            }
            else if(roomLocation.in_blue_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 35.3, 3.0);
                MOSTRA_REPORT.setReport("Trasu e niescio");
                
                }*/

                //-----Fine controllo stanze
      	

      	// Stato 0: Il Robot si porta verso una zona che gli consente di osservare
      	//          la dislocazione delle bandierine nella zona avversaria.

       //--------------------------NOTA IMPORTANTE:
        //PER TESTARE AL MEGLIO QUESTA VERSIONE, IN PARTICOLARE SE UNA ZONA VIENE CHIUSA O MENO, ABBIAMO MODIFICATO IL PUNTO ATTRATTORE
        //DEI ROBOT NELLA FASE DI ATTACK  (GLI ORIGINALI SONO COMMENTATI)
//oppo
        if(y_robot>27.5)
        {
           // linear_attraction.setRot_teta(1.5);
            linear_attraction2.setRot_teta(1.5);
        }
        if(y_robot<0.6 && y_robot>0)
        {
          //  linear_attraction.setRot_teta(-1.5);
            linear_attraction2.setRot_teta(-0.5);
        }
     //   MOSTRA_REPORT.setReport( "colore bandiera "+ idFlag_other + "\tid_robot" + id);
    /*    if(x_robot>11 && x_robot<11.7 && y_robot>24)
        {
            
           //  linear_attraction.setRot_teta(3);
           linear_attraction2.setRot_teta(3);
        }*/
      /*  //labirinto
        if(x_robot>=17 && x_robot<=25 && y_robot<22.5)  //parte superiore
        {
            
           //  linear_attraction.setRot_teta(4.5);
             linear_attraction2.setRot_teta(4.5);
        }
        if(x_robot>=25 && x_robot<25.5 && y_robot<=22 && y_robot>=8)  //parte dx
        {
            
          //   linear_attraction.setRot_teta(3);
             linear_attraction2.setRot_teta(3);
        }*/
        if(x_robot>=19 && x_robot<=27 && y_robot<=8 && y_robot>=7)  //parte inferiore
        {
            
           //  linear_attraction.setRot_teta(1.5);
             linear_attraction2.setRot_teta(1.0);
        }
        /*
        if(x_robot<=19 && y_robot<=17 && y_robot>=8)  //parte sx
        {
            
            // linear_attraction.setRot_teta(0.0);
             linear_attraction2.setRot_teta(0.0);
        }*/
        /*
        if(x_robot>32.4 && x_robot<33)
        {
            linear_attraction.setRot_teta(0);
           linear_attraction2.setRot_teta(0);
        }*/

        /*if(y_robot>27.5&& y_robot<28) {     
            //linear_attraction.setRot_teta(1.5);    
             linear_attraction2.setRot_teta(0.15); } 
        else if(y_robot<0.7) {    
             //linear_attraction.setRot_teta(-1.5);     
             linear_attraction2.setRot_teta(-0.15); } 
        if(x_robot>11 && x_robot<11.6&&y_robot>24 &&y_robot<28) {     
            //    // linear_attraction.setRot_teta(3);    
            linear_attraction2.setRot_teta(3); } */
        
            /*if(x_robot>11 && x_robot<11.6&&y_robot>16 &&y_robot<22) 
            {     
                //     // linear_attraction.setRot_teta(3);   
                  linear_attraction2.setRot_teta(3.425); 
            }
             if(x_robot>11 && x_robot<11.6&&y_robot>6 &&y_robot<12) {     // 
                 // linear_attraction.setRot_teta(3);    
                  linear_attraction2.setRot_teta(3.3); }
                  /* else if(y_robot>32.7 && y_robot<33) { 
                        //linear_attraction.setRot_teta(0);
            linear_attraction2.setRot_teta(0); }*/
            

        if(state==0){
        	if(y_robot<15.0)
        		//PS_ATTRACTOR.setValue(curr_time,24.0,7.0);
                PS_ATTRACTOR.setValue(curr_time,32.0,15.0);
        	else
                PS_ATTRACTOR.setValue(curr_time,32.0,27.0);
        		//PS_ATTRACTOR.setValue(curr_time,26.0,17.0);

                // STEER
            result = steering_configuration.Value(curr_time);
            abstract_robot.setSteerHeading(curr_time, result.t);
            abstract_robot.setSpeed(curr_time, result.r);

              // TURRET
            result = turret_configuration.Value(curr_time);
            abstract_robot.setTurretHeading(curr_time, abstract_robot.getTurretHeading(curr_time)+0.4);
            abstract_robot.setDisplayString("Attack");

          }


         // Stato 1: Calcola la bandierina pi??? vicina ad ogni take step,
         //          si dirige verso di essa, affronta le problematiche relative al recupero
         //          delle bandiere
         if(state==1){
            //resetto i trigger:
            PF_ENTRY.setValue(false);
            PF_EXIT_STALL.setValue(false);
            PF_EMPTY_MEMORY.setValue(false);
            if(PF_EMPTY_MEMORY.Value(curr_time)==true)
            {
                PF_EMPTY_MEMORY.setValue(true); //VAI SUBITO ALLO STATO 4...
                return(CSSTAT_OK); //..SENZA ESEGUIRE ULTERIORI ISTRUZIONI DI QUESTO STATO 1

            }
            if(antistallo.getI()>=400)
                {
                    System.out.println("ATTIVO ANTISTALLO DSIR");
                    PS_ATTRACTOR.setValue(curr_time,x_robot-0.5,y_robot-0.5);
                }

         	delete=true;

         	begin4=true;
         	PF_HAVE_FLAG.setValue(false);
            count_3=0;
       		delete=false;  // Utilizzata per eliminare la bandierina nello stato 5
       		flag=flag_closest.Value(curr_time);

               //la modifica si deve fare qua mi sa
    		if((roomLocation.flag_in_red_Left(flag)&&roomLocation.closed_red_Left())||(roomLocation.flag_in_green_Left(flag)&&roomLocation.closed_green_Left())||(roomLocation.flag_in_blue_Left(flag)&&roomLocation.closed_blue_Left())||(roomLocation.flag_in_red_Right(flag)&&roomLocation.closed_red_Right())||(roomLocation.flag_in_green_Right(flag)&&roomLocation.closed_green_Right())||(roomLocation.flag_in_blue_Right(flag)&&roomLocation.closed_blue_Right())||(roomLocation.flag_in_lab(flag)&&(roomLocation.closed_lab_Left()||roomLocation.closed_lab_Right())))
    		{
    			PF_ENTRY.setValue(true);
    		}
    		else PF_ENTRY.setValue(false);

    		if(roomLocation.in_lab())
    		{
                antistallo.resetI(); //RESETTO i per tenere conto del tempo che impiega il robot nello stato successivo
    			PF_EXIT.setValue(true); //vado allo stato 6
    		}else PF_EXIT.setValue(false);

    		//Se la bandiera ??? nel lab faccio credere al robot che si trova
   			//in un altro punto che risulta  utile per farlo entrare nel lab
    		if(roomLocation.flag_in_lab(flag)){
    			if(x_robot<22){
    				flag.x=16;
    				flag.y=20;
    			}
    			else{
    				flag.x=28;
    				flag.y=10;
    			}
    		}

    		PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
           	PS_ATTRACTOR.setValue(curr_time,flag.x,flag.y);


    		if(roomLocation.in_room()){
    			count_in_room++;

    			abstract_robot.setDisplayString("Bandierina nella stanza");
    			if(count_in_room>120){
    				flag_memory.pop(flag,curr_time);
    				PF_EXIT.setValue(true);
    			}
    			else PF_EXIT.setValue(false);
        	}
        	else{
				abstract_robot.setDisplayString("Vado alla bandierina vista pi?? vicina");
                allontanati(curr_time,x_robot, y_robot,flag);
                //allontanati(curr_time,x_robot,y_robot);
        		 count_in_room=0;
        	}
            	if((flag_memory.getLength(curr_time)==0)){
            		PF_NOT_EMPTY_MEMORY.setValue(false);
                    antistallo.resetI();
            		PF_EMPTY_MEMORY.setValue(true); //VAI ALLO STATO 4
                    abstract_robot.setDisplayString("NIENTEEEE");
                    PS_ATTRACTOR.setValue(curr_time,x_robot-0.2,y_robot+0.3);
                    //return(CSSTAT_OK);
           		}
            	else
            	{
            	 PF_NOT_EMPTY_MEMORY.setValue(true); 
            	 PF_EMPTY_MEMORY.setValue(false);
                 //return(CSSTAT_OK);
            	}



            if(begin1==true){
     			begin1=false;
                antistallo.update();
               	}
               	if(antistallo.isStall(10000)){
                   	begin1=true;
                   	flag_memory.pop(flag,curr_time);
           		    PF_DEADLOCK.setValue(true);
                    }
                else PF_DEADLOCK.setValue(false);


           	/*if(localizer.in_special_case(x_robot,y_robot,flag.x,flag.y)){
           		PF_GOTO_SPECIAL_ZONE.setValue(true);
           		}
           		else PF_GOTO_SPECIAL_ZONE.setValue(false); */



            // STEER
            	result = steering_configuration.Value(curr_time);
            	abstract_robot.setSteerHeading(curr_time, result.t);
            	abstract_robot.setSpeed(curr_time, result.r);

              // TURRET
            result = turret_configuration.Value(curr_time);
            abstract_robot.setTurretHeading(curr_time,result.t); //abstract_robot.getTurretHeading(curr_time)+0.4);
          //  abstract_robot.setTurretHeading(curr_time, result.t);
          	dresult=gripper_fingers_configuration.Value(curr_time);
		    abstract_robot.setGripperFingers(curr_time,dresult);
            //return(CSSTAT_OK);
         }



 	// Stato 2:  Consegna delle bandiere.Sono previsti 3 casi:
 	//						a. Bandierina nella stanza;
 	//                      b. Bandierina in una posizione indefinita al di fuori delle
 	//                         stanze o del labirinto;
 	//                      c. Il Robot recupera una bandiera e sceglie di usare il
 	//                         labirinto come porta di scambio tra zona sinistra e destra.
    if(state==2){

    	delete=true;
    	begin8=true;
    	stall=0;
   		tempo=0;
   		PF_HAVE_FLAG.setValue(true);
        if(PF_HAVE_FLAG_LAB.Value(curr_time)==true){
        	PF_HAVE_FLAG_LAB.setValue(false);
          	flag_memory.pop(lab_flag,curr_time);
          	//System.out.println("CAncella flag lab ROBOT:"+abstract_robot.getPlayerNumber(curr_time));
            //System.out.println("Flag:"+lab_flag.x+","+lab_flag.y);
        }
        else
        {
         	 flag_memory.pop(flag,curr_time);
      	}

      	if(roomLocation.use_lab_right()){
      		PF_USE_LAB.setValue(true);
      	}
      	else PF_USE_LAB.setValue(false);

      	if(roomLocation.use_lab_right_redBlue()){
      		PF_USE_LAB_RED_BLUE.setValue(true);
      	}
      	else PF_USE_LAB_RED_BLUE.setValue(false);

        bring_plan.update(curr_time, state);
        if(bring_plan.isChangedState()){
            action = bring_plan.getActionName();
            if(action.startsWith("reset_timer")) //aggiunta questa azione per resettare il timer alla fine del piano bring_flag
            {
                abstract_robot.setDisplayString("RESETTO TIMER");
                antistallo.resetI();
            }
            else
            if(action.startsWith("calculate_route_red")) //
            {
                abstract_robot.setDisplayString("calculate_route_red");
                if(!roomLocation.in_lab())
                {
                    check=true;
                if(localizer.amInZonaSud()){
                    percorso_consegna[0]=15.0;
                    percorso_consegna[1]=7.0;    //cella33
                    percorso_consegna[2]=15.0;
                    percorso_consegna[3]=11.0;   //cella27
                    percorso_consegna[4]=15.0;
                    percorso_consegna[5]=17.0; //24
                    percorso_consegna[6]=15.0;
                    percorso_consegna[7]=22.0; //inizio corridoio D
                   // check=true;
                }else
                if(localizer.amInZonaNord()){
                    percorso_consegna[0]=13.2;
                    percorso_consegna[1]=24.2;
                    percorso_consegna[2]=12.9;
                    percorso_consegna[3]=23.9;
                    percorso_consegna[4]=12.7;
                    percorso_consegna[5]=23.6;
                    percorso_consegna[6]=12.5;
                    percorso_consegna[7]=23.0; //met?? corridoio D 
                   /* percorso_consegna[0]=x_robot-0.1;
                    percorso_consegna[1]=y_robot;
                    percorso_consegna[2]=percorso_consegna[0]-0.1;
                    percorso_consegna[3]=percorso_consegna[1];
                    percorso_consegna[4]=percorso_consegna[2]-0.1;
                    percorso_consegna[5]=percorso_consegna[1];
                    percorso_consegna[6]=percorso_consegna[4]-0.1;
                    percorso_consegna[7]=percorso_consegna[1]; //met?? corridoio D */
                }else
                if(localizer.amInZonaEst()){
                    percorso_consegna[0]=27.0;
                    percorso_consegna[1]=23.0;
                    percorso_consegna[2]=22.0;
                    percorso_consegna[3]=23.0;
                    percorso_consegna[4]=18.0;
                    percorso_consegna[5]=23.0;
                    percorso_consegna[6]=13.0;
                    percorso_consegna[7]=23.0; //met?? corridoio D
                }else
                if(localizer.amInZonaOvest()){
                    percorso_consegna[0]=15.0;
                    percorso_consegna[1]=22.0;
                    percorso_consegna[2]=14.8;
                    percorso_consegna[3]=22.3;
                    percorso_consegna[4]=14.0;
                    percorso_consegna[5]=22.6;
                    percorso_consegna[6]=13.6;
                    percorso_consegna[7]=23.0; //met?? corridoio D
                }
                }
                else
                {
                    check=false;
                }

            }
            else
            if(action.startsWith("first_step_red")) //
            {
                abstract_robot.setDisplayString("first_step_red");
                
                if(check==true)
                {
                    PS_ATTRACTOR.setValue(curr_time,percorso_consegna[0],percorso_consegna[1]);
                }
                    
               
                	
            }
            else
            if(action.startsWith("second_step_red")) //
            {
                abstract_robot.setDisplayString("second_step_red");
                if(check==true)
                {
                    PS_ATTRACTOR.setValue(curr_time,percorso_consegna[2],percorso_consegna[3]);
                }
                
            }
            else
            if(action.startsWith("third_step_red")) //
            {
                
                abstract_robot.setDisplayString("third_step_red");
                if(check==true)
                {
                PS_ATTRACTOR.setValue(curr_time,percorso_consegna[4],percorso_consegna[5]);
                }
            }
            else
            if(action.startsWith("fourth_step_red")) //
            {
                abstract_robot.setDisplayString("fourth_step_red");
                 if(check==true)
                {
                PS_ATTRACTOR.setValue(curr_time,percorso_consegna[6],percorso_consegna[7]);
                }
                
            }
            if(action.startsWith("calculate_route_green")) //
            {
                abstract_robot.setDisplayString("calculate_route_green"); 
                if(!roomLocation.in_lab())
                {
                    check=true;
                if(localizer.amInZonaSud()){
                    percorso_consegna[0]=15.0;
                    percorso_consegna[1]=7.0;    //cella33
                    percorso_consegna[2]=14.0;
                    percorso_consegna[3]=9.0;   //y tra 7 e 11 , x un po pi??  destra
                    percorso_consegna[4]=13.2;
                    percorso_consegna[5]=11.0; //cella27
                    percorso_consegna[6]=12.5;
                    percorso_consegna[7]=13.5; //inizio corridoio C(quasi a met??)
                    check=true;
                }else
                if(localizer.amInZonaNord()){
                    percorso_consegna[0]=15.0;
                    percorso_consegna[1]=23.0; //met?? corridoio D
                    percorso_consegna[2]=15.0;
                    percorso_consegna[3]=18.0;
                    percorso_consegna[4]=14.0;
                    percorso_consegna[5]=16.0;
                    percorso_consegna[6]=13.0;
                    percorso_consegna[7]=14.0; //met?? corridoio C
                }else
                if(localizer.amInZonaEst()){
                    percorso_consegna[0]=25.0;
                    percorso_consegna[1]=23.0;  //met?? corridoio D
                    percorso_consegna[2]=20.0;
                    percorso_consegna[3]=23.0;
                    percorso_consegna[4]=15.0;
                    percorso_consegna[5]=18.0;
                    percorso_consegna[6]=13.0;
                    percorso_consegna[7]=14.0; //met?? corridoio C
                }else
                if(localizer.amInZonaOvest()){
                    percorso_consegna[0]=14.8;
                    percorso_consegna[1]=14.0;
                    percorso_consegna[2]=14.2;
                    percorso_consegna[3]=14.0;
                    percorso_consegna[4]=14.0;
                    percorso_consegna[5]=14.0;
                    percorso_consegna[6]=13.2;
                    percorso_consegna[7]=14.0; //met?? corridoio C
                }
                }
                else
                {
                    check=false;
                }


            }
            else
            if(action.startsWith("first_step_green")) //
            {
                abstract_robot.setDisplayString("first_step_green");
                if(check==true)
                {
                    PS_ATTRACTOR.setValue(curr_time,percorso_consegna[0],percorso_consegna[1]);
                }
                	
            }
            else
            if(action.startsWith("second_step_green")) //
            {
                abstract_robot.setDisplayString("second_step_green");
                if(check==true)
                {
                    PS_ATTRACTOR.setValue(curr_time,percorso_consegna[2],percorso_consegna[3]);
                }
                
            }
            else
            if(action.startsWith("third_step_green")) //
            {
                abstract_robot.setDisplayString("third_step_green");
                if(check==true)
                {
                PS_ATTRACTOR.setValue(curr_time,percorso_consegna[4],percorso_consegna[5]);
                }
            }
            else
            if(action.startsWith("fourth_step_green")) //
            {
                abstract_robot.setDisplayString("fourth_step_green");
                 if(check==true)
                {
                PS_ATTRACTOR.setValue(curr_time,percorso_consegna[6],percorso_consegna[7]);
                }
                
            }
            if(action.startsWith("calculate_route_blue")) //
            {
                abstract_robot.setDisplayString("calculate_route_blue");
               if(!roomLocation.in_lab())
               {
                check=true;
               if(localizer.amInZonaSud()){     
                    percorso_consegna[0]=13.2;
                    percorso_consegna[1]=4.8;    
                    percorso_consegna[2]=12.9;
                    percorso_consegna[3]=4.8;   
                    percorso_consegna[4]=12.7;
                    percorso_consegna[5]=4.8; 
                    percorso_consegna[6]=12.5;
                    percorso_consegna[7]=5.0; //met?? corridoio B
                    //check=true;
                }else
                if(localizer.amInZonaNord()){
                    percorso_consegna[0]=15.0;
                    percorso_consegna[1]=23.0;  //met?? corridoio D
                    percorso_consegna[2]=14.5;
                    percorso_consegna[3]=17.0;
                    percorso_consegna[4]=13.8;
                    percorso_consegna[5]=11.0;
                    percorso_consegna[6]=13.0;
                    percorso_consegna[7]=5.0; //met?? corridoio B
                }else
                if(localizer.amInZonaEst()){
                    percorso_consegna[0]=30.0;
                    percorso_consegna[1]=5.0;  //met?? corridoio B
                    percorso_consegna[2]=26.0;
                    percorso_consegna[3]=5.0;
                    percorso_consegna[4]=18.0;
                    percorso_consegna[5]=5.0;
                    percorso_consegna[6]=13.5;
                    percorso_consegna[7]=5.0; //met?? corridoio B
                }else
                if(localizer.amInZonaOvest()){
                    percorso_consegna[0]=14.9;
                    percorso_consegna[1]=7.0;
                    percorso_consegna[2]=14.6;
                    percorso_consegna[3]=6.2;
                    percorso_consegna[4]=14.3;
                    percorso_consegna[5]=5.8;
                    percorso_consegna[6]=13.6;
                    percorso_consegna[7]=5.0; //met?? corridoio B
                }
               }
               else
               {
                check=false;
               }

            }
            else
            if(action.startsWith("first_step_blue")) //
            {
                abstract_robot.setDisplayString("first_step_blue");
                if(check==true)
                {
                    PS_ATTRACTOR.setValue(curr_time,percorso_consegna[0],percorso_consegna[1]);
                }
                	
            }
            else
            if(action.startsWith("second_step_blue")) //
            {
                abstract_robot.setDisplayString("second_step_blue");
                if(check==true)
                {
                    PS_ATTRACTOR.setValue(curr_time,percorso_consegna[2],percorso_consegna[3]);
                }
                
            }
            else
            if(action.startsWith("third_step_blue")) //
            {
                abstract_robot.setDisplayString("third_step_blue");
                 if(check==true)
                {
                PS_ATTRACTOR.setValue(curr_time,percorso_consegna[4],percorso_consegna[5]);
                }
            }
            else
            if(action.startsWith("fourth_step_blue")) //
            {
                abstract_robot.setDisplayString("fourth_step_blue");
                 if(check==true)
                {
                PS_ATTRACTOR.setValue(curr_time,percorso_consegna[6],percorso_consegna[7]);
                }
                
            }
            else
            if(action.startsWith("exit")){
            	if(roomLocation.in_red_Right()){
        			PS_ATTRACTOR.setValue(curr_time,32.0,23.0);
        		}
        		if(roomLocation.in_green_Right()){
        			PS_ATTRACTOR.setValue(curr_time,32.0,14.0);
                }
             	if(roomLocation.in_blue_Right()){
        			PS_ATTRACTOR.setValue(curr_time,32.0,5.0);
        		}
        		if(roomLocation.in_red_Left()){
        			PS_ATTRACTOR.setValue(curr_time,12.0,23.0);
        		}
        		if(roomLocation.in_green_Left()){
        			PS_ATTRACTOR.setValue(curr_time,12.0,14.0);
        		}

        		if(roomLocation.in_blue_Left()){
        				PS_ATTRACTOR.setValue(curr_time,12.0,5.0);
        		}
            }
            else
            	if(action.startsWith("goto_lab")){
            		if(bring_plan.getParameter()[0].equals("centre")){
            			linear_attraction2.setRot_teta(0.0);
						if(y_robot<15.0)
        					PS_ATTRACTOR.setValue(curr_time,20.0,12.0);
        				else
        					PS_ATTRACTOR.setValue(curr_time,24.0,18.0);

                	}
                	else
                	linear_attraction2.setRot_teta(0.0);
                	if(bring_plan.getParameter()[0].equals("door1_low")){
                   		PS_ATTRACTOR.setValue(curr_time,30.5,10.0);
                	}
                	else
                	if(bring_plan.getParameter()[0].equals("door2_low")){
                		PS_ATTRACTOR.setValue(curr_time,20.0,12.0);
                	}
                	else
                	if(bring_plan.getParameter()[0].equals("door1_high")){
                		PS_ATTRACTOR.setValue(curr_time,13.5,20.0);
                	}
                	else
                	if(bring_plan.getParameter()[0].equals("door2_high")){
                		PS_ATTRACTOR.setValue(curr_time,24.0,18.0);
                	}
                	else
                	if(bring_plan.getParameter()[0].equals("door2")){
                		if(y_robot<15.0)
        					PS_ATTRACTOR.setValue(curr_time,20.0,11.4);
        				else
        					PS_ATTRACTOR.setValue(curr_time,24.0,18.6);
                	}
 				}else
            	if (action.startsWith("goto(")){
                	if(bring_plan.getParameter()[0].equals("red_bin")){
                            antistallo.resetI();
                            check=false;
                    		PS_ATTRACTOR.setValue(curr_time, 2.5, 23);
                    		PF_OTHER_ROBOT.setValue(false);
                    		PF_HAVE_FLAG.setValue(false);

                	}
                	else
                	if(bring_plan.getParameter()[0].equals("green_bin")){
                    antistallo.resetI();
                    check=false;
                    PS_ATTRACTOR.setValue(curr_time, 2.5, 14);
                    PF_OTHER_ROBOT.setValue(false);
                    PF_HAVE_FLAG.setValue(false);

                	}
                	else
                	if(bring_plan.getParameter()[0].equals("blue_bin")){
                    antistallo.resetI();
                    check=false;
                    PS_ATTRACTOR.setValue(curr_time, 2.5, 5);
                    PF_OTHER_ROBOT.setValue(false);
                    PF_HAVE_FLAG.setValue(false);

                	}
                	else
                	if(bring_plan.getParameter()[0].equals("red_attractor2")){
                        /*if(antistallo.getI()>=800)
                            {
                                System.out.println("ATTIVO ANTISTALLO DSIR");
                                PS_ATTRACTOR.setValue(curr_time,x_robot-0.5,y_robot-0.5);
                                antistallo.resetI();
                        }*/
                			/*linear_attraction2.setRot_teta(0.0);
                        linear_attraction2.setRot_teta(0.0);
                        if(x_robot>=11 && x_robot<=13 && y_robot>=24 && y_robot<27) {
                            linear_attraction2.setRot_teta(1.5);*/
                           // STATUS_CELLE.visualizzaReport("@@@@ho fatto quel che andava fatto@@@@");
                       //}
                        linear_attraction2.setRot_teta(0.0);
    //                    if(localizer.amInCella9() || y_robot<20){
                           /* punto[2] = 11.5; //punto x intermedio davanti ingresso anticamera sx rossa
                            punto[3] = 23.0; //punto y intermedio davanti ingresso anticamera sx rossa*/
     //                       PS_ATTRACTOR.setValue(curr_time, 11.5 , 23.0);

    //                        if (PF_OTHER_ROBOT.Value(curr_time)&& (idFlag_other==PS_IN_GRIPPER.intValue(curr_time))){
                               /* punto[0] = 8.0; //punto x nell' anticamera sx rossa
                                punto[1] = 25; //punto y*/
    //                            PS_ATTRACTOR.setValue(curr_time, 8.0, 25.0);
    /*                        }
                            else{
                			PS_ATTRACTOR.setValue(curr_time, 8.0, 24.4);
                            }
                        }*/
                    	if (PF_OTHER_ROBOT.Value(curr_time)&& (idFlag_other==PS_IN_GRIPPER.intValue(curr_time)))
                    		PS_ATTRACTOR.setValue(curr_time, 8.0, 25.0);
                		else
                			PS_ATTRACTOR.setValue(curr_time, 8.0, 24.4);

                	}
                	else
                	if(bring_plan.getParameter()[0].equals("green_attractor2")){
                			linear_attraction2.setRot_teta(0.0);
                    	if (PF_OTHER_ROBOT.Value(curr_time)&& (idFlag_other==PS_IN_GRIPPER.intValue(curr_time)))
                   			PS_ATTRACTOR.setValue(curr_time, 6.0, 13.2);
                 		else
                			PS_ATTRACTOR.setValue(curr_time, 6.0, 14.8);
                	}
                	else
                	if(bring_plan.getParameter()[0].equals("blue_attractor2")){
                		linear_attraction2.setRot_teta(0.0);
                   		if (PF_OTHER_ROBOT.Value(curr_time)&& (idFlag_other==PS_IN_GRIPPER.intValue(curr_time)))
                    		PS_ATTRACTOR.setValue(curr_time, 8.0, 3.0);
                		else
                			PS_ATTRACTOR.setValue(curr_time, 8.0, 3.6);
                	}
                	else
                	if(bring_plan.getParameter()[0].equals("red_attractor1")){
                       
                        if(antistallo.getI()>=800)
                        {
                                System.out.println("ATTIVO ANTISTALLO DSIR");
                                PS_ATTRACTOR.setValue(curr_time,x_robot,y_robot-1.5);
                                antistallo.resetI();
                        }
                        PS_ATTRACTOR.setValue(curr_time, 11.5, 23.0);
                     		if(x_robot>=19 && x_robot<27 && y_robot>=3&& y_robot<=8){         		
                    		linear_attraction2.setRot_teta(0.35);
                    		//System.out.println("Sono nella stanza 5a");                    		
                    	}
                    	else if(x_robot>=27 && x_robot<=33 && y_robot>=2&& y_robot<=8){         		
                    		linear_attraction2.setRot_teta(0.5);
                    		//System.out.println("Sono nella stanza 5b");                    		
                    	}                    
                    	else if(x_robot>=25 && x_robot<=33 && y_robot>8 && y_robot <=22){
                    		linear_attraction2.setRot_teta(-1.1);
                    		//System.out.println("Sono nella stanza 2-3-4");
                    	}	
                    	// PS_ATTRACTOR.setValue(curr_time, 12, 23.0);
                     	/*	if(x_robot>=19 && x_robot<27 && y_robot>=3&& y_robot<=8){
                    		linear_attraction2.setRot_teta(0.35);
                    		//System.out.println("Sono nella stanza 5a");
                    	}
                    	else if(x_robot>=27 && x_robot<=33 && y_robot>=2&& y_robot<=8){
                    		linear_attraction2.setRot_teta(0.5);
                    		//System.out.println("Sono nella stanza 5b");
                    	}
                    	else if(x_robot>=25 && x_robot<=33 && y_robot>8 && y_robot <=22){
                    		linear_attraction2.setRot_teta(-1.1);
                    		//System.out.println("Sono nella stanza 2-3-4");
                    	}*/

                	}
                	else
                	if(bring_plan.getParameter()[0].equals("green_attractor1")){
                    	PS_ATTRACTOR.setValue(curr_time, 12.4, 14);
                    	if(x_robot>25 && x_robot<33 && y_robot>=15 && y_robot<22){
                    		linear_attraction2.setRot_teta(-1.1);
                    		//System.out.println("Sono nella stanza 2-3a");
                    	}
                    	else if(x_robot>=25 && x_robot<=33 && y_robot<15 && y_robot>=8){
                    		linear_attraction2.setRot_teta(1.2);
                    		//System.out.println("Sono nella stanza 3b-4");
                    	}
                    	else if(x_robot>=22 && x_robot<=33 && y_robot>22 && y_robot<28){
                    		linear_attraction2.setRot_teta(-0.5);
                    		//System.out.println("Sono nella stanza 1");
                    	}
                    	else if(x_robot>=25 && x_robot<=33 && y_robot<15 && y_robot<8){
                    		linear_attraction2.setRot_teta(0.5);
                    		//System.out.println("Sono nella stanza 5");
                    	}
                	}
                	else
                	if(bring_plan.getParameter()[0].equals("blue_attractor1")){
                    	PS_ATTRACTOR.setValue(curr_time, 11.5, 5.0);
                    	    if(x_robot>25 && x_robot<33 && y_robot>8){
                    		linear_attraction2.setRot_teta(1.2);
                    		//System.out.println("Sono nella stanza 1-2-3-4");
                    	}
                    	else{
                    		linear_attraction2.setRot_teta(0.0);
                    	}

                	}
                	else
                	if(bring_plan.getParameter()[0].equals("entry_lab")){
                    	PS_ATTRACTOR.setValue(curr_time, 26, 9);
                    	//System.out.println("use lab");
                     }
                	else
                	if(bring_plan.getParameter()[0].equals("here")){
                    	PS_ATTRACTOR.setValue(curr_time, x_robot, y_robot);
                    	//System.out.println("here");

                     }


            	} else

            	if(action.startsWith("send_message(am_at_attractor")){
                	try
                	{
                    	LongMessage idMessage = new LongMessage();
                    	idMessage.val = abstract_robot.getPlayerNumber(curr_time);
                    	abstract_robot.unicast(other_id, idMessage);
                    	StringMessage message = new StringMessage();
                    	message.val = "Pronto";
                    	abstract_robot.unicast(other_id, message);
                    	LongMessage idFlag=new LongMessage();
                    	idFlag.val=PS_IN_GRIPPER.intValue(curr_time);
                   		abstract_robot.unicast(other_id, idFlag);


                	}
            		catch(CommunicationException e){}
            	  antistallo.resetI();
            	}

        	}



        }

        // Stato 3: Presiede all'impostazione corretta del valore del teta del vettore
        //          d'attrazione lineare
         // Stato 3: Presiede all'impostazione corretta del valore del teta del vettore
        //          d'attrazione lineare
        if(state==3){
        	tempo=0;
        	begin1=true;		
        	if(old_teta!=localizer.getTeta(x_robot,y_robot,flag.x,flag.y)){  //gestisce il passaggio da una zona all'altra azzerando il count_3
        		count_3=0;
        	}
        	
        	linear_attraction.setRot_teta(localizer.getTeta(x_robot,y_robot,flag.x,flag.y));//1.0); metodo di Location che ritorna il teta in base alla osizione del robot e della bandierina
        	count_3++;
        	if(count_3>=localizer.getStopper(x_robot,y_robot,flag.x,flag.y)){//300){//metodo di Location che ritorna il contatore in base ecc,ecc 
        		count_3=0;
				PF_STOP_GOTO_SPECIAL_ZONE.setValue(true);			        		
        	}
        	else 
        	    PF_STOP_GOTO_SPECIAL_ZONE.setValue(false);      	    
        	    
        	
        	
        	PS_SPECIAL_ATTRACTOR.setValue(curr_time,flag.x,flag.y);
        	abstract_robot.setDisplayString(localizer.getString(x_robot,y_robot,flag.x,flag.y));//"Sto andando nella zona alta"); ritorna la stringa in base.....
        	
        	
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
	     	
	     	old_teta=localizer.getTeta(x_robot,y_robot,flag.x,flag.y);
        	
        linear_attraction.setRot_teta(0.0);	
        }


        if(state==4){ //STATO DI EXPLORE

                PF_ENTRY.setValue(false);
                PF_EXIT.setValue(false);
                PF_EXIT_STALL.setValue(false);
                PF_EMPTY_MEMORY.setValue(false);
              if(antistallo.getI()>=700)
                {
                    System.out.println("ATTIVO ANTISTALLO DSIR");
                    PS_ATTRACTOR.setValue(curr_time,x_robot-0.5,y_robot-0.5);
                    antistallo.resetI();
                }
            if(roomLocation.out_of_bounds()){
                PF_BOUNDS.setValue(true); //vai allo stato6
                //PF_NOT_ANTIROOM.setValue(true);
                antistallo.resetI();
                //System.out.println("esco a causa del bounds"+abstract_robot.getPlayerNumber(curr_time));
            }
            if(roomLocation.in_room_centre() && localizer.areClosedAllZones()) //SE SONO NELLA STANZA CENTRALE E LE ZONE STATE TUTTE VISITATE
            {
                if (localizer.amInZona1() || localizer.amInCorridoioD())
                {

                    if (roomLocation.getIsClosedAntiRoomRedLeft() == false)
                    {
                        //vai nell'anticamera sx rossa
                        punto[0] = 9.4; //punto x in anticamera sx rossa
                        punto[1] = 23.0; //punto y in anticamera sx rossa
                        punto[2] = 11.5; //punto x intermedio per andare in anticamera sx rossa
                        punto[3] = 23.0; //punto y intermedio per andare in anticamera sx rossa
                        
                    } else if (roomLocation.getIsClosedAntiRoomRedLeft() == true && roomLocation.getIsClosedAntiRoomRedRight() == false) {
                        punto[0] = 34.6; //punto x in anticamera dx rossa
                        punto[1] = 22.5; //punto y in anticamera dx rossa
                        punto[2] = 32; //punto x intermedio per andare in anticamera dx rossa
                        punto[3] = 22.5; //punto y intermedio per andare in anticamera dx rossa
                        
                    } else if (roomLocation.getIsClosedAntiRoomRedLeft() == true && roomLocation.getIsClosedAntiRoomRedRight() == true) {
                        //vai in zona 2
                        punto[0] = 15.0; //punto x a sinistra verso la zona 2
                        punto[1] = 21.0; //punto y a sinistra verso la zona2
                        punto[2] = 15.0; //punto x intermedio del corridoio (vai a sinistra del corridoio D)
                        punto[3] = 23.0; //punto y intermedio del corridoio (vai a sinistra del corridoio D)
                        
                    }
                    PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10


                }
                else if (localizer.amInZona2() || localizer.amInCorridoioC()) {
                    if (roomLocation.getIsClosedAntiRoomGreenLeft()== false) {
                        punto[0] = 9.4; //punto x nell' anticamera sx verde
                        punto[1] = 13.5; //punto y
                        punto[2] = 11.5; //punto x intermedio per anticamera sx verde
                        punto[3] = 13.5; //punto y intermedio per anticamera sx verde
                       
                    } else //vai in zona 4
                    {
                        punto[0] = 15.0; //punto x a sinistra della zona 4
                        punto[1] = 2.0; //punto y a sinistra della zona4
                        punto[2] = 15.0; //punto x intermedio del corridoio (vai a sinistra del corridoio B)
                        punto[3] = 5.0; //punto y intermedio del corridoio (vai a sinistra del corridoio B)
                        
                    }
                    PF_EXPLORE_CELLA.setValue(true);

                }
                else if (localizer.amInZona4() || localizer.amInCorridoioB()) {
                    if (roomLocation.getIsClosedAntiRoomBlueLeft() == false) {
                        //vai nell'anticamera sx blu
                        punto[0] = 8.5; //punto x in anticamera sx blu
                        punto[1] = 4.5; //punto y in anticamera sx blu
                        punto[2] = 11.5; //punto x intermedio per andare in anticamera sx blu
                        punto[3] = 5; //punto y intermedio per andare in anticamera sx blu
                        
                    }
                    else if (roomLocation.getIsClosedAntiRoomBlueRight() == false && roomLocation.getIsClosedAntiRoomBlueLeft() == true) {
                        //vai nell'anticamera dx blu
                        punto[0] = 34.6; //punto x in anticamera dx blu
                        punto[1] = 4.5; //punto y in anticamera dx blu
                        punto[2] = 32; //punto x intermedio per andare in anticamera dx blu
                        punto[3] = 4.5; //punto y intermedio per andare in anticamera dx blu
                        
                    }  else if (roomLocation.getIsClosedAntiRoomBlueRight() == true && roomLocation.getIsClosedAntiRoomBlueLeft() == true) {
                        //vai in zona 3
                        punto[0] = 31; //punto x a sinistra della zona 3
                        punto[1] = 8.0; //punto y a sinistra della zona3
                        punto[2] = 31; //punto x intermedio del corridoio (vai a destra del corridoio B)
                        punto[3] = 5.0; //punto y intermedio del corridoio (vai a sinistra del corridoio B)
                        
                    }
                    PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10

                }
                else if (localizer.amInZona3() || localizer.amInCorridoioA()) {
                    if (roomLocation.getIsClosedAntiRoomGreenRight()== false) {
                        punto[0] = 34.6; //punto x nell' anticamera sx verde
                        punto[1] = 13.5; //punto y
                        punto[2] = 32; //punto x intermedio per anticamera sx verde
                        punto[3] = 13.5; //punto y intermedio per anticamera sx verde
                        
                    } else //vai in zona 1
                    {
                        punto[0] = 25.0; //punto x a destra della zona 1
                        punto[1] = 25.0; //punto y a destra della zona1
                        punto[2] = 25.0; //punto x intermedio del corridoio (vai a destra del corridoio D)
                        punto[3] = 23.0; //punto y intermedio del corridoio (vai a destra del corridoio D)
                        
                    }
                    PF_EXPLORE_CELLA.setValue(true);
                }
            }
             else if(roomLocation.in_room_centre() && !localizer.areClosedAllZones()) //SE SONO NELLA STANZA CENTRALE E LE ZONE NON SONO ANCORA TUTTE VISITATE
                {

                    if(localizer.amInZona1() || localizer.amInCorridoioD())
                    {
                        if(!localizer.isClosedZona1())
                        {
                            punto=localizer.isClosedZona1Point();
                            antistallo.resetI();
                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                        else //se sei in zona 1 ed e' chiusa zona1
                        {

                            if(localizer.isClosedZona2()){
                                punto[0]=25.0; //punto x a destra verso la zona 3
                                punto[1]=20.0; //punto y a destra verso la zona 3
                                punto[2]=25.0; //punto x intermedio del corridoio (vai a destra del corridoio D)
                            punto[3]=23.0; //punto y intermedio del corridoio (vai a destra del corridoio D)
                            
                            }
                            else //Se sei in zona 1 e zona1 e' chiusa e zona2 e' aperta
                            {
                                punto[0]=15.0; //punto x a sinistra verso la zona 2
                                punto[1]=21.0; //punto y a sinistra verso la zona2
                                 punto[2]=15.0; //punto x intermedio del corridoio (vai a sinistra del corridoio D)
                            punto[3]=23.0; //punto y intermedio del corridoio (vai a sinistra del corridoio D)
                            
                            }


                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                    }
                    if(localizer.amInZona2()|| localizer.amInCorridoioC())
                    {
                        if(!localizer.isClosedZona2()) //NOTA: riuscire a far restiture direttamente un booleano al secondo metodo (anzich?? fare il for)
                        {
                            punto=localizer.isClosedZona2Point();
                            antistallo.resetI();
                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                        else //se sei in zona 2 ed e' chiusa zona2
                        {

                            if(localizer.isClosedZona4()){
                                punto[0]=14.0; //punto x a sinistra della zona 1
                                punto[1]=26.0; //punto y a sinistra della zona 1
                                punto[2]=14.0; //punto x intermedio del corridoio (vai a sinistra del corridoio D)
                                punto[3]=23.0; //punto y intermedio del corridoio (vai a sinistra del corridoio D)
                                
                            }
                            else //Se sei in zona 2 e zona2 e' chiusa e zona4 e' aperta
                            {
                                punto[0]=15.0; //punto x a sinistra della zona 4
                                punto[1]=2.0; //punto y a sinistra della zona4
                                 punto[2]=15.0; //punto x intermedio del corridoio (vai a sinistra del corridoio B)
                            punto[3]=5.0; //punto y intermedio del corridoio (vai a sinistra del corridoio B)
                             
                            }


                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                    }
                    if(localizer.amInZona4() || localizer.amInCorridoioB())
                    {
                        if(!localizer.isClosedZona4()) //NOTA: riuscire a far restiture direttamente un booleano al secondo metodo (anzich?? fare il for)
                        {
                            punto=localizer.isClosedZona4Point();
                            antistallo.resetI();
                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                        else //se sei in zona 4 ed e' chiusa zona4
                        {

                            if(localizer.isClosedZona3()){
                                punto[0]=15.0; //punto x a destra della zona 2
                                punto[1]=8.0; //punto y a destra della zona 2
                                 punto[2]=15.0; //punto x intermedio del corridoio (vai a sinistra del corridoio B)
                            punto[3]=5.0; //punto y intermedio del corridoio (vai a sinistra del corridoio B)
                          
                            }
                            else //Se sei in zona 4 e zona4 e' chiusa e zona3 e' aperta
                            {
                                punto[0]=27.5; //punto x a sinistra della zona 3
                                punto[1]=8.0; //punto y a sinistra della zona3
                                 punto[2]=27.5; //punto x intermedio del corridoio (vai a destra del corridoio B)
                            punto[3]=5.0; //punto y intermedio del corridoio (vai a sinistra del corridoio B)
                            
                            }


                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                    }
                    if(localizer.amInZona3() || localizer.amInCorridoioA())
                    {
                        if(!localizer.isClosedZona3()) //NOTA: riuscire a far restiture direttamente un booleano al secondo metodo (anzich?? fare il for)
                        {
                            punto=localizer.isClosedZona3Point();
                            antistallo.resetI();
                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                        else //se sei in zona 3 ed e' chiusa zona3
                        {

                            if(localizer.isClosedZona1()){
                                punto[0]=27.0; //punto x a destra della zona 4
                                punto[1]=3.0; //punto y a destra della zona 4
                                 punto[2]=27.0; //punto x intermedio del corridoio (vai a destra del corridoio B)
                            punto[3]=5.0; //punto y intermedio del corridoio (vai a destra del corridoio B)
                             
                            }
                            else //Se sei in zona 3 e zona3 e' chiusa e zona1 e' aperta
                            {
                                punto[0]=25.0; //punto x a destra della zona 1
                                punto[1]=25.0; //punto y a destra della zona1
                                 punto[2]=25.0; //punto x intermedio del corridoio (vai a destra del corridoio D)
                            punto[3]=23.0; //punto y intermedio del corridoio (vai a destra del corridoio D)
                            
                            }


                            PF_EXPLORE_CELLA.setValue(true); //vai allo stato 10
                        }
                    }}
                else //SE NON SONO NELLA STANZA CENTRALE OPPURE HO VISITATO TUTTE LE ZONE
                {

             

                begin1=true;
                tempo=0;
                stall=0;
                abstract_robot.setDisplayString("Explore");
                noisegain.setValue(0.5);
    
                
                

                //-----Inizio controllo stanze
            if(roomLocation.in_blue_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 9, 3);
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_red_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 9, 25.0);
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_green_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 12.0, 14.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_red_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 35.3, 25.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                PF_NOT_ROOM.setValue(true);
            }

            else if(roomLocation.in_green_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 32.0, 14.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_blue_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 35.3, 3.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                PF_NOT_ROOM.setValue(true);
                }

                //-----Fine controllo stanze
                if(roomLocation.out_of_bounds()){
                    PF_BOUNDS.setValue(true); //vai allo stato6
                    //PF_NOT_ANTIROOM.setValue(true);
                    antistallo.resetI();
                    //System.out.println("esco a causa del bounds"+abstract_robot.getPlayerNumber(curr_time));
                }
                else
                    PF_BOUNDS.setValue(false);

                if((!red_room_Left&&roomLocation.closed_red_Left())&&!roomLocation.getIsClosedAntiRoomRedLeft()||(!green_room_Left&&roomLocation.closed_green_Left()&&!roomLocation.getIsClosedAntiRoomGreenLeft())||(!blue_room_Left&&roomLocation.closed_blue_Left()&&!roomLocation.getIsClosedAntiRoomBlueLeft())||(!lab_room&&roomLocation.closed_lab_()) ||(!red_room_Right&&roomLocation.closed_red_Right())&&!roomLocation.getIsClosedAntiRoomRedRight()||(!green_room_Right&&roomLocation.closed_green_Right())&&!roomLocation.getIsClosedAntiRoomGreenRight()||(!blue_room_Right&&roomLocation.closed_blue_Right())&&!roomLocation.getIsClosedAntiRoomBlueRight())
                {
                    CHOOSE_ZONE.setPriority(1,2,0,3);
                    PF_ENTRY.setValue(true);
                }
                else PF_ENTRY.setValue(false);

               if(roomLocation.in_room())
                {

                if(roomLocation.in_blue_Left())
                {
                    swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
                    if(roomLocation.getIsClosedAntiRoomBlueLeft()==true && roomLocation.getTempoVisitaAnticameraBlueLeft()>=roomLocation.TEMPO_MASSIMO_VISITA_ANTICAMERA)
                    {
                        //MOSTRA_REPORT.setReport("----------Il robot " + id + " NON PUO' PIU' VISITARE L'ANTICAMERA BLUE DI SINISTRA IN QUANTO HA ESAURITO IL SUO TEMPO A DISPOSIZIONE PER TALE AZIONE\n");
                        if(id==0)
                            STATUS_CELLE.setReportAnticameraR0(4);
                        else
                            STATUS_CELLE.setReportAnticameraR1(4);
                        antistallo.resetI();
                        PF_NOT_ANTIROOM.setValue(true);
                        
                    }
                    else
                    {
                        abstract_robot.setDisplayString("Explore anticamera blue di sinistra");

                        if(roomLocation.getIsClosedAntiRoomBlueLeft()==false)
                        {
                            //MOSTRA_REPORT.setReport("Il robot " + id + " ha visitato l'anticamera blue di sinistra\n");
                            roomLocation.chiudiAnticamera("blue sinistra");
                        }
                        roomLocation.updateTempoVisitaAnticameraBlueLeft();
                    }

                }
                else if(roomLocation.in_red_Left())
                {
                    swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
                    if(roomLocation.getIsClosedAntiRoomRedLeft()==true && roomLocation.getTempoVisitaAnticameraRedLeft()==roomLocation.TEMPO_MASSIMO_VISITA_ANTICAMERA)
                    {
                        //MOSTRA_REPORT.setReport("----------Il robot " + id + " NON PUO' PIU' VISITARE L'ANTICAMERA ROSSA DI SINISTRA IN QUANTO HA ESAURITO IL SUO TEMPO A DISPOSIZIONE PER TALE AZIONE\n");
                        if(id==0)
                            STATUS_CELLE.setReportAnticameraR0(0);
                        else
                            STATUS_CELLE.setReportAnticameraR1(0);
                        antistallo.resetI();
                        PF_NOT_ANTIROOM.setValue(true);
                        
                    }
                    else
                    {
                        abstract_robot.setDisplayString("Explore anticamera rossa di sinistra");
                        if(roomLocation.getIsClosedAntiRoomRedLeft()==false)
                        {
                            //MOSTRA_REPORT.setReport("Il robot " + id + " ha visitato l'anticamera rossa di sinistra\n");
                            roomLocation.chiudiAnticamera("rossa sinistra");
                        }
                        roomLocation.updateTempoVisitaAnticameraRedLeft();
                    }
                }
                else if(roomLocation.in_green_Left())
                {
                    swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
                    if(roomLocation.getIsClosedAntiRoomGreenLeft()==true && roomLocation.getTempoVisitaAnticameraGreenLeft()==roomLocation.TEMPO_MASSIMO_VISITA_ANTICAMERA)
                    {
                        //MOSTRA_REPORT.setReport("----------Il robot " + id + " NON PUO' PIU' VISITARE L'ANTICAMERA VERDE DI SINISTRA IN QUANTO HA ESAURITO IL SUO TEMPO A DISPOSIZIONE PER TALE AZIONE\n");
                        antistallo.resetI();

                        if(id==0)
                            STATUS_CELLE.setReportAnticameraR0(2);
                        else
                            STATUS_CELLE.setReportAnticameraR1(2);
                        PF_NOT_ANTIROOM.setValue(true);
                    }
                    else
                    {
                        abstract_robot.setDisplayString("Explore anticamera verde di sinistra");
                        if(roomLocation.getIsClosedAntiRoomGreenLeft()==false)
                        {
                            //MOSTRA_REPORT.setReport("Il robot " + id + " ha visitato l'anticamera verde di sinistra\n");
                            roomLocation.chiudiAnticamera("verde sinistra");
                        }
                        roomLocation.updateTempoVisitaAnticameraGreenLeft();
                    }
                }
                else if(roomLocation.in_red_Right())
                {
                    swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
                    if(roomLocation.getIsClosedAntiRoomRedRight()==true && roomLocation.getTempoVisitaAnticameraRedRight()==roomLocation.TEMPO_MASSIMO_VISITA_ANTICAMERA)
                    {
                        //MOSTRA_REPORT.setReport("----------Il robot " + id + " NON PUO' PIU' VISITARE L'ANTICAMERA ROSSA DI DESTRA IN QUANTO HA ESAURITO IL SUO TEMPO A DISPOSIZIONE PER TALE AZIONE\n");
                        antistallo.resetI();
                        if(id==0)
                            STATUS_CELLE.setReportAnticameraR0(1);
                        else
                            STATUS_CELLE.setReportAnticameraR1(1);
                        PF_NOT_ANTIROOM.setValue(true);

                    }
                    else
                    {
                        abstract_robot.setDisplayString("Explore anticamera rossa di destra");
                        if(roomLocation.getIsClosedAntiRoomRedRight()==false)
                        {
                            //MOSTRA_REPORT.setReport("Il robot " + id + " ha visitato l'anticamera rossa di destra\n");
                            roomLocation.chiudiAnticamera("rossa destra");
                        }
                        roomLocation.updateTempoVisitaAnticameraRedRight();
                    }
                }

                else if(roomLocation.in_green_Right())
                {
                    swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
                    if(roomLocation.getIsClosedAntiRoomGreenRight()==true && roomLocation.getTempoVisitaAnticameraGreenRight()==roomLocation.TEMPO_MASSIMO_VISITA_ANTICAMERA)
                    {
                        //MOSTRA_REPORT.setReport("----------Il robot " + id + " NON PUO' PIU' VISITARE L'ANTICAMERA VERDE DI DESTRA IN QUANTO HA ESAURITO IL SUO TEMPO A DISPOSIZIONE PER TALE AZIONE\n");
                        antistallo.resetI();
                        if(id==0)
                            STATUS_CELLE.setReportAnticameraR0(3);
                        else
                            STATUS_CELLE.setReportAnticameraR1(3);
                        PF_NOT_ANTIROOM.setValue(true);
                    }
                    else
                    {

                        abstract_robot.setDisplayString("Explore anticamera verde di destra");

                        if(roomLocation.getIsClosedAntiRoomGreenRight()==false)
                        {
                            //MOSTRA_REPORT.setReport("Il robot " + id + " ha visitato l'anticamera verde di destra\n");
                            roomLocation.chiudiAnticamera("verde destra");
                        }
                        roomLocation.updateTempoVisitaAnticameraGreenRight();
                    }
                }
                else if(roomLocation.in_blue_Right())
                {
                    swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
                    if(roomLocation.getIsClosedAntiRoomBlueRight()==true && roomLocation.getTempoVisitaAnticameraBlueRight()==roomLocation.TEMPO_MASSIMO_VISITA_ANTICAMERA)
                    {
                        //MOSTRA_REPORT.setReport("----------Il robot " + id + " NON PUO' PIU' VISITARE L'ANTICAMERA BLUE DI DESTRA IN QUANTO HA ESAURITO IL SUO TEMPO A DISPOSIZIONE PER TALE AZIONE\n");
                        if(id==0)
                            STATUS_CELLE.setReportAnticameraR0(5);
                        else
                            STATUS_CELLE.setReportAnticameraR1(5);
                        antistallo.resetI();
                        PF_NOT_ANTIROOM.setValue(true);
                    }
                    else
                    {
                        abstract_robot.setDisplayString("Explore anticamera blue di destra");

                        if(roomLocation.getIsClosedAntiRoomBlueRight()==false)
                        {
                            //MOSTRA_REPORT.setReport("Il robot " + id + " ha visitato l'anticamera blue di destra\n");
                            roomLocation.chiudiAnticamera("blue destra");
                        }
                        roomLocation.updateTempoVisitaAnticameraBlueRight();
                    }
                }

                if(begin4==true){
                    begin4=false;
                    antistallo.update();
                }
                if(antistallo.isStall(6000)){
                    begin4=true;
                    PF_EXIT.setValue(true); //vai allo stato 6
                    antistallo.resetI();

                }
                else PF_EXIT.setValue(false);

                }

                if(flag_memory.getLength(curr_time)!=0){
                    PF_EMPTY_MEMORY.setValue(false);
                    antistallo.resetI();
                    PF_NOT_EMPTY_MEMORY.setValue(true);
                }
                else{
                    PF_EMPTY_MEMORY.setValue(true);
                    PF_NOT_EMPTY_MEMORY.setValue(false);
                }
                    /*if(roomLocation.closed_lab_()) //SE SONO VICINO AL LABIRINTO
                    {
                        antistallo.resetI();
                        System.out.println("*****************IL ROBOT " +id+" ENTRA NEL LABIRINTO PROVENENDO DALLO STATO 4 (PASSA SEMPRE PER STATO 7)");
                        PF_ENTRY.setValue(true);
                    }*/

                    // STEER
                    result = steering_configuration.Value(curr_time);
                    abstract_robot.setSteerHeading(curr_time, result.t);
                    abstract_robot.setSpeed(curr_time, result.r);

                    // TURRET
                    result = turret_configuration.Value(curr_time);
                    abstract_robot.setTurretHeading(curr_time, abstract_robot.getTurretHeading(curr_time)+0.4);

                    // FINGERS
                    dresult=gripper_fingers_configuration.Value(curr_time);
                    abstract_robot.setGripperFingers(curr_time,dresult);
                } //FINE ELSE



        }
        //FINE ESPLORAZIONE
        
 
 		// Stato 5: E' uno stato intermedio tra la localizzazione e direzione immediata 
 		//          verso la bandiera avvistata (stato 1) e la pronta consegna (stato 2). 
       	if(state==5){
            antistallo.resetI();
       		begin1=true;
        	tempo=0;   
        	flag=flag_closest.Value(curr_time);
        	PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        	PS_ATTRACTOR.setValue(curr_time,flag.x,flag.y);
        
        	if(delete==true){
     		    delete=false;                		
                antistallo.update();    					
               	}
               	if(antistallo.isStall(3500)){
                   	delete=true;           	
                   	flag_memory.pop(flag,curr_time);
           		    PF_STALL.setValue(true);		            		    
                    }
                else PF_STALL.setValue(false);

                //-----Inizio controllo stanze
            if(roomLocation.in_blue_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 9, 3);
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_red_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 9, 25.0);
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_green_Left_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 12.0, 14.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_red_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 35.3, 25.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                PF_NOT_ROOM.setValue(true);
            }

            else if(roomLocation.in_green_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 32.0, 14.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                PF_NOT_ROOM.setValue(true);
            }
            else if(roomLocation.in_blue_Right_room())
            {
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                PS_ATTRACTOR.setValue(curr_time, 35.3, 3.0);
                //MOSTRA_REPORT.setReport("Trasu e niescio");
                try
                {
                    PF_NOT_ROOM.setValue(true);
                }
                catch(Exception ex)
                {
                    ex.printStackTrace();
                }
                
                }

                //-----Fine controllo stanze

            
       
        
             // STEER
            result = steering_configuration.Value(curr_time);
            abstract_robot.setSteerHeading(curr_time, result.t);
            abstract_robot.setSpeed(curr_time, result.r);
        
                
              // TURRET
            result = turret_configuration.Value(curr_time);
            abstract_robot.setTurretHeading(curr_time,result.t); 
            

  
          	dresult=gripper_fingers_configuration.Value(curr_time);
		    abstract_robot.setGripperFingers(curr_time,dresult);
            abstract_robot.setDisplayString("Devo prendere la bandierina");
         }
         
         // Stato 6: Consente al robot di uscire dalle stanze
         if(state==6){

            if(antistallo.getI()>=500)
			{
				abstract_robot.setDisplayString("ATTIVO ANTISTALLO DSIR");
				PF_EXIT_STALL.setValue(true);
			}

         	begin1=true;
         	begin4=true;
         	PF_BOUNDS.setValue(false);
         	
         	CHOOSE_ZONE.setPriority(0,3,1,2);
         	if(roomLocation.in_red_Right()){      	
        		PS_ATTRACTOR.setValue(curr_time,31.0,23.0);
        		PF_EXIT.setValue(false);
        		//System.out.println("Sono nella stanza rossa.Sto uscendo...");
        		red_room_Right=true;
        		}
        		
        	if(roomLocation.in_green_Right()){      	
        		PS_ATTRACTOR.setValue(curr_time,31.0,14.0);
        		PF_EXIT.setValue(false);
        		//System.out.println("Sono nella stanza verde.Sto uscendo...");
        		green_room_Right=true;
        		}
        		
        	if(roomLocation.in_blue_Right()){   	
        		PS_ATTRACTOR.setValue(curr_time,31.0,5.0);
        		PF_EXIT.setValue(false);
        		//System.out.println("Sono nella stanza blue.Sto uscendo...");
        		blue_room_Right=true;
        		}
        		
        	if(roomLocation.in_red_Left()){        		
        		PS_ATTRACTOR.setValue(curr_time,13.0,23.0);
        		//System.out.println("Sono nella stanza rossa.Sto uscendo...");
        		PF_EXIT.setValue(false);
        		red_room_Left=true;
        		}
        		
        	if(roomLocation.in_blue_Left()){        		
        		PS_ATTRACTOR.setValue(curr_time,13.0,5.0);
        		//System.out.println("Sono nella stanza blue.Sto uscendo...");
        		PF_EXIT.setValue(false);
        		blue_room_Left=true;
        		}	
        		
        	if(roomLocation.in_green_Left()){        		
        		PS_ATTRACTOR.setValue(curr_time,13.0,14.0);
				//System.out.println("Sono nella stanza verde.Sto uscendo...");
        		PF_EXIT.setValue(false);
        		green_room_Left=true;
        		}		
        	if(roomLocation.in_lab_down()){
        		PF_EXIT.setValue(false);
        		PS_ATTRACTOR.setValue(curr_time,30,7);
        	}	
        	if(roomLocation.in_lab_top()){
        		PF_EXIT.setValue(false);
        		PS_ATTRACTOR.setValue(curr_time,14,23);
        	}
        		
        	  // STEER
            	result = steering_configuration.Value(curr_time);
            	abstract_robot.setSteerHeading(curr_time, result.t);
            	abstract_robot.setSpeed(curr_time, result.r);
                
              // TURRET
            result = turret_configuration.Value(curr_time);
            abstract_robot.setTurretHeading(curr_time,result.t); 
          	dresult=gripper_fingers_configuration.Value(curr_time);
		    abstract_robot.setGripperFingers(curr_time,dresult);
            abstract_robot.setDisplayString("EXIT");
 
         } 
      
      /*// Stato 7: Questo stato scatta nel momento in cui il robot deve visitare le stanze o
      //          il labirinto
      if(state==7){
      		begin1=true;
      		begin4=true;
         	PF_ENTRY.setValue(false);
         	count_in_room=0;
         	CHOOSE_ZONE.setPriority(1,0,3,2);
         	if(roomLocation.closed_red_Right()){
        		PS_ATTRACTOR.setValue(curr_time,34.0,23.0);
        		abstract_robot.setDisplayString("Entro nella stanza");
        		}

        	if(roomLocation.closed_green_Right()){
        		PS_ATTRACTOR.setValue(curr_time,35.0,14.0);
      			abstract_robot.setDisplayString("Entro nella stanza");
        	}

        	if(roomLocation.closed_blue_Right()){
        		PS_ATTRACTOR.setValue(curr_time,34.0,5.0);
        		abstract_robot.setDisplayString("Entro nella stanza");
        		}
        	if(roomLocation.closed_red_Left()){
        		PS_ATTRACTOR.setValue(curr_time,10.0,23.0);
        		abstract_robot.setDisplayString("Entro nella stanza");
        		}

        	if(roomLocation.closed_green_Left()){
        		PS_ATTRACTOR.setValue(curr_time,9.0,14.0);
      			abstract_robot.setDisplayString("Entro nella stanza");
        	}

        	if(roomLocation.closed_blue_Left()){
        		PS_ATTRACTOR.setValue(curr_time,10.0,5.0);
        		abstract_robot.setDisplayString("Entro nella stanza");
        		}

        	if(roomLocation.closed_lab_Left()){
        		PS_ATTRACTOR.setValue(curr_time,21.0,20.0);
        		abstract_robot.setDisplayString("Entro nel lab dall'alto");
        	}

        	if(roomLocation.closed_lab_Right()){
        		PS_ATTRACTOR.setValue(curr_time,24.0,10.0);
        		abstract_robot.setDisplayString("Entro nel lab dal basso");
        	}

        	if(roomLocation.in_lab()){
                swirlgain.setValue(0.5);
                avoidgain.setValue(0.0);
                noisegain.setValue(0.0);
                if(roomLocation.getIsClosedLabirinto()==true && roomLocation.getTempoVisitaLabirinto()==roomLocation.TEMPO_MASSIMO_VISITA_LABIRINTO)
                {
                    MOSTRA_REPORT.setReport("----------Il robot " + id + " NON PUO' PIU' VISITARE IL LABIRINTO IN QUANTO HA ESAURITO IL SUO TEMPO A DISPOSIZIONE PER TALE AZIONE\n");
                    antistallo.resetI();
                    PF_LAB.setValue(false);
                }
                else
                {
                    //abstract_robot.setDisplayString("Explore labirinto");

                    if(roomLocation.getIsClosedLabirinto()==false)
                    {
                        MOSTRA_REPORT.setReport("Il robot " + id + " ha visitato il labirinto\n");
                        roomLocation.chiudiLabirinto();
                    }
                    roomLocation.updateTempoVisitaLabirinto();
                    PF_LAB.setValue(true);
                }

        	}
        	else PF_LAB.setValue(false);

        	  // STEER
            	result = steering_configuration.Value(curr_time);
            	abstract_robot.setSteerHeading(curr_time, result.t);
            	abstract_robot.setSpeed(curr_time, result.r);

              // TURRET
            result = turret_configuration.Value(curr_time);
            abstract_robot.setTurretHeading(curr_time,result.t);
          	dresult=gripper_fingers_configuration.Value(curr_time);
		    abstract_robot.setGripperFingers(curr_time,dresult);

         }

*/
        // Stato 7: Questo stato scatta nel momento in cui il robot deve visitare le stanze o
        //          il labirinto
        if(state==7){
            begin1=true;
            begin4=true;
            PF_ENTRY.setValue(false);
            count_in_room=0;
            CHOOSE_ZONE.setPriority(1,0,3,2);
            PF_EXIT_STALL.setValue(false); //vai allo stato 7

            if(roomLocation.closed_red_Right()){
                PS_ATTRACTOR.setValue(curr_time,34.0,23.0);
                abstract_robot.setDisplayString("Entro nella stanza");
            }

            if(roomLocation.closed_green_Right()){
                PS_ATTRACTOR.setValue(curr_time,35.0,14.0);
                abstract_robot.setDisplayString("Entro nella stanza");
            }

            if(roomLocation.closed_blue_Right()){
                PS_ATTRACTOR.setValue(curr_time,34.0,5.0);
                abstract_robot.setDisplayString("Entro nella stanza");
            }
            if(roomLocation.closed_red_Left()){
                PS_ATTRACTOR.setValue(curr_time,10.0,23.0);
                abstract_robot.setDisplayString("Entro nella stanza");
            }

            if(roomLocation.closed_green_Left()){
                PS_ATTRACTOR.setValue(curr_time,9.0,14.0);
                abstract_robot.setDisplayString("Entro nella stanza");
            }

            if(roomLocation.closed_blue_Left()){
                PS_ATTRACTOR.setValue(curr_time,10.0,5.0);
                abstract_robot.setDisplayString("Entro nella stanza");
            }

            if(roomLocation.getTempoVisitaLabirinto()<RoomLocation.TEMPO_MASSIMO_VISITA_LABIRINTO) //se si verifica cio', pu?? fare la visita del labirinto
            {
                antistallo.resetI();
                //PF_NOT_LABIRINTO.setValue(true);

                       if(roomLocation.closed_lab_Left()){
                PS_ATTRACTOR.setValue(curr_time,21.0,20.0);
                abstract_robot.setDisplayString("Entro nel lab dall'alto");
                }

            if(roomLocation.closed_lab_Right()){
                PS_ATTRACTOR.setValue(curr_time,24.0,10.0);
                abstract_robot.setDisplayString("Entro nel lab dal basso");
            }

            if(roomLocation.in_lab()){
                PF_LAB.setValue(true);
            }
            else PF_LAB.setValue(false);

            }
            else
            {
                //MOSTRA_REPORT.setReport(">>>>>ATTENZIONE: IL ROBOT " + id + " NON PUO' PIU' VISITARE IL LABIRINTO");
                PF_LAB.setValue(false);

                if(roomLocation.closed_lab_Left()){
                PS_ATTRACTOR.setValue(curr_time,14.5,16.5);
                abstract_robot.setDisplayString("VOLEVO ENTRARE NEL LAB DALL'ALTO :'(");
                //Setto il percorso per farlo allontanare dal labirinto (ricorda che punto[0] e punto[1] sono il punto attrattore finale e punto[2] e punto[3] ?? il punto intermedio)
                /*punto[0]=
                punto[1]=
                punto[2]=
                punto[3]=
                PF_NOT_LABIRINTO.setValue(true);*/
                }

            if(roomLocation.closed_lab_Right()){
                PS_ATTRACTOR.setValue(curr_time,31.5,4.0);
                abstract_robot.setDisplayString("VOLEVO ENTRARE NEL LAB DAL BASSO :'(");

                PF_NOT_LABIRINTO.setValue(true);
            }

            }

     

            // STEER
            result = steering_configuration.Value(curr_time);
            abstract_robot.setSteerHeading(curr_time, result.t);
            abstract_robot.setSpeed(curr_time, result.r);

            // TURRET
            result = turret_configuration.Value(curr_time);
            abstract_robot.setTurretHeading(curr_time,result.t);
            dresult=gripper_fingers_configuration.Value(curr_time);
            abstract_robot.setGripperFingers(curr_time,dresult);

        }

        if(state==9) //stato in cui un robot transita se una certa anticamera ?? chiusa
        {
            //RESETTO I TRIGGER:
            PF_NOT_ANTIROOM.setValue(false);
            PF_EXIT.setValue(false);

            if (antistallo.getI() >= 500) {
                abstract_robot.setDisplayString("ATTIVO ANTISTALLO DSIR");
                PF_EXIT_STALL.setValue(true);
                // NON FUNZIONA COSI, BISOGNA CREARE UN NUOVO TRIGGER CHE TRANSITA DA UNO STATO ALL'ALTRO PF_CLOSE_TO.setValue(true); //sbloccati e vai allo stato 1
            }
            if (roomLocation.in_red_Right()) {
                PS_ATTRACTOR.setValue(curr_time, 32.0, 23.0);
            }
            if (roomLocation.in_green_Right()) {
                PS_ATTRACTOR.setValue(curr_time, 32.0, 14.0);
            }
            if (roomLocation.in_blue_Right()) {
                PS_ATTRACTOR.setValue(curr_time, 32.0, 5.0);
            }
            if (roomLocation.in_red_Left()) {
                PS_ATTRACTOR.setValue(curr_time, 12.0, 23.0);
            }
            if (roomLocation.in_green_Left()) {
                PS_ATTRACTOR.setValue(curr_time, 12.0, 14.0);
            }

            if (roomLocation.in_blue_Left()) {
                PS_ATTRACTOR.setValue(curr_time, 12.0, 5.0);
            }


            result = steering_configuration.Value(curr_time);
            abstract_robot.setSteerHeading(curr_time, result.t);
            abstract_robot.setSpeed(curr_time, result.r);

            // TURRET
            result = turret_configuration.Value(curr_time);
            abstract_robot.setTurretHeading(curr_time, abstract_robot.getTurretHeading(curr_time) + 0.4);
            abstract_robot.setDisplayString("Attack");

            abstract_robot.setDisplayString("STATO DI ANTICAMERA CHIUSA");
            //MOSTRA_REPORT.setReport("_________________Il robot " + id + " ?? nello stato 9\n");
            PF_EXIT.setValue(true); //VAI ALLO STATO 2
        }

		// Stato 8: Implementa l'esplorazione e pi??? in generale l'utilizzo del labirinto 
 		if(state==8){
 		//	PF_HAVE_FLAG.setValue(false);
            
            if((antistallo.getI()>=RoomLocation.TEMPO_MASSIMO_VISITA_LABIRINTO+20 || roomLocation.getTempoVisitaLabirinto()>RoomLocation.TEMPO_MASSIMO_VISITA_LABIRINTO) && !roomLocation.in_lab())
            {
                PF_EXIT_STALL.setValue(true);
            }

            if(roomLocation.getTempoVisitaLabirinto()<=RoomLocation.TEMPO_MASSIMO_VISITA_LABIRINTO)
            {
                roomLocation.updateTempoVisitaLabirinto();
                //MOSTRA_REPORT.setReport("********IL ROBOT " + id + " HA UN TEMPO DI VISITA DEL LABIRINTO DI " + roomLocation.getTempoVisitaLabirinto());
            }
            /*else
            {
                //LO BUTTIAMO FUORI DALLO STATO 8
                PF_NOT_LABIRINTO.setValue(true); //LO MANDIAMO NELLO STATO _______
            }*/

 			lab_plan.update(curr_time, state); 	
 			if(!begin_flag){
 				if(antistallo.isStall(3000)){
 					begin_flag=true; 						
 					flag_memory.pop(lab_flag,curr_time);
 					PF_HAVE_FLAG.setValue(false); 
 					PF_HAVE_FLAG_LAB.setValue(false);
 					PF_STALL_FLAG.setValue(true);
 				}
 			}
 			else
 				{
 					PF_STALL_FLAG.setValue(false);
 				}

 			if(lab_plan.isChangedState()){
 				
            	action = lab_plan.getActionName();
            	if(action.startsWith("goto_lab")){
            		if(lab_plan.getParameter()[0].equals("centre")){
						if(y_robot<15.0)
        					PS_ATTRACTOR.setValue(curr_time,20.0,12.0);
        				else
        					PS_ATTRACTOR.setValue(curr_time,24.0,18.0);
            
                	}
                	else
                	if(lab_plan.getParameter()[0].equals("door1_low")){
                		PS_ATTRACTOR.setValue(curr_time,30.5,10.0);
                	}
                	else
                	if(lab_plan.getParameter()[0].equals("door2_low")){
                		PS_ATTRACTOR.setValue(curr_time,20.0,12.0);
                	}
                	else
                	if(lab_plan.getParameter()[0].equals("door1_high")){
                		PS_ATTRACTOR.setValue(curr_time,13.5,20.0);
                	}
                	else
                	if(lab_plan.getParameter()[0].equals("door2_high")){
                		PS_ATTRACTOR.setValue(curr_time,24.0,18.0);
                	}
                	else
                	if(lab_plan.getParameter()[0].equals("door2")){
                		if(y_robot<15.0)
        					PS_ATTRACTOR.setValue(curr_time,20.0,11.4);
        				else
        					PS_ATTRACTOR.setValue(curr_time,24.0,18.6);
                	}         
 				}
 				if(action.startsWith("explore")){
 					if(lab_plan.getParameter()[0].equals("entry")){
 						if(y_robot<15.0){
 						PF_COME_FROM_LOW.setValue(true);
 						PS_ATTRACTOR.setValue(curr_time,20.0,10.0);
 						}
 						else
 					    {
 					    
 						PF_COME_FROM_LOW.setValue(false);
 						PS_ATTRACTOR.setValue(curr_time,24.0,20.0);
 						}					
 					}
 					else
 					if(lab_plan.getParameter()[0].equals("exit")){
 						if(y_robot>15.0){
 							PS_ATTRACTOR.setValue(curr_time,17.0,20.0);
 						}
 						else
 						{	
 							PS_ATTRACTOR.setValue(curr_time,27.0,10.0);
						} 					
 					}
 					else
 					if(lab_plan.getParameter()[0].equals("attractor1")){
 						if(y_robot<15.0){
 							int y_rand=(Math.abs(random.nextInt())%4)+13;
 							//System.out.println(y_rand);
 							PS_ATTRACTOR.setValue(curr_time,24.6,y_rand);
 							come_from_low=true;
 						}
 						else
 						{
 							int y_rand=(Math.abs(random.nextInt())%4)+13;
 							//System.out.println(y_rand);
 							PS_ATTRACTOR.setValue(curr_time,19.4,y_rand);
 							come_from_low=false;
 						}
 						
 					}
 					else
 					if(lab_plan.getParameter()[0].equals("attractor2")){
 						if(come_from_low){
 							int y_rand=(Math.abs(random.nextInt())%4)+13;
 							//System.out.println(y_rand);
 							PS_ATTRACTOR.setValue(curr_time,19.4,y_rand);
 						}
 						else
 						{
 							int y_rand=(Math.abs(random.nextInt())%4)+13;
 							//System.out.println(y_rand);
 							PS_ATTRACTOR.setValue(curr_time,24.6,y_rand);
 						}
 					}else
 					if(lab_plan.getParameter()[0].equals("door2")){
 						if(come_from_low){
 							PS_ATTRACTOR.setValue(curr_time,24.0,17.8);
 						}
 						else
 						{
 							PS_ATTRACTOR.setValue(curr_time,20.0,12.2);
 						}
 						
 					}
 										 					
 				}
 				if(action.startsWith("go_flag")){
 					begin_flag=false;
 					antistallo.update();
 					 lab_flag=new Vec2(PS_CLOSEST_FLAG.Value(curr_time).x+x_robot,PS_CLOSEST_FLAG.Value(curr_time).y+y_robot);
        
 								
 					flag_memory.pop(lab_flag,curr_time);
 					PF_HAVE_FLAG.setValue(true); 
 					PF_HAVE_FLAG_LAB.setValue(true);
 					abstract_robot.setDisplayString("Vado al flag");
 					//System.out.println("posiziona bandiera"+ PS_CLOSEST_FLAG.Value(curr_time).x+ PS_CLOSEST_FLAG.Value(curr_time).y);
			  
                } 				
 			}
 			abstract_robot.setDisplayString("Labirinto");
            //antistallo.resetI();

 		}
        if(state==10) //vai nella prima cella non libera di una zona o segui un determinato percorso composto da due celle
        {
            //resetto i trigger:
            PF_EXPLORE_CELLA.setValue(false);
            PF_ENTRY.setValue(false);
            PF_EXIT_STALL.setValue(false);
            

       /*     if(roomLocation.in_room_centre() && localizer.areClosedAllZones()) //SE SEI ALLO STATO 10 IN UNA DELLE ANTICAMERE, SEI IN STALLO E QUINDI ESCI
            {
                /*if(roomLocation.in_blue_Left()||roomLocation.in_red_Left()||roomLocation.in_green_Left())
                {
                    punto[0]
                    punto[1]

                }
                else
                {

                }
                PF_EXIT_STALL.setValue(true); //vai allo stato 7
            }
           */
               
            if(antistallo.getI()>=400)
                {
                    System.out.println("ATTIVO ANTISTALLO DSIR");
                    if(x_robot==18.4 && (y_robot>=8.8 && y_robot<=17.1) && !roomLocation.in_lab())  //parete sinistra labirinto
                {
                    PS_ATTRACTOR.setValue(curr_time, x_robot-1.2, y_robot+2.2);
                    System.out.println("***********************MI ALLONTANO DALLA PARETE SINISTRA DEL LABIRINTO");
                    swirlgain.setValue(0.5);
                                avoidgain.setValue(0.0);
                                noisegain.setValue(0.0);
                    //MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE SINISTRA DEL LABIRINTO");
                    
                }
                else if((x_robot>=25 && x_robot<25.5) && (y_robot<=22 && y_robot>=8) && !roomLocation.in_lab() ) //parete destra labirinto
                {
                    PS_ATTRACTOR.setValue(curr_time, x_robot+1.2, y_robot-0.2);
                    //PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
                    System.out.println("***********************MI ALLONTANO DALLA PARETE DESTRA DEL LABIRINTO");
                    swirlgain.setValue(0.5);
                                avoidgain.setValue(0.0);
                                noisegain.setValue(0.0);
        //MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE DESTRA DEL LABIRINTO");
                }
                    PS_ATTRACTOR.setValue(curr_time,x_robot+0.2,y_robot-2);
                    PF_EMPTY_MEMORY.setValue(true); //vai allo stato 4
                }

             if(roomLocation.closed_lab_()) //se sono vicino il labirinto vado allo stato 7
                    {
                        antistallo.resetI();
                        System.out.println("*****************IL ROBOT " +id+" ENTRA NEL LABIRINTO PROVENENDO DALLO STATO 10 (PASSA SEMPRE PER STATO 7)");
                        PF_ENTRY.setValue(true);
                    }
            /*Vec2 secondoPuntoAttrattore=new Vec2();    
            secondoPuntoAttrattore.x=x_robot-0.2;
            secondoPuntoAttrattore.y=y_robot-0.2;
            allontanati(curr_time,x_robot,y_robot,secondoPuntoAttrattore);*/

            explore_plan.update(curr_time, state);
            if(explore_plan.isChangedState()){
                action = explore_plan.getActionName();
            if(action.startsWith("goto(")){
						if(explore_plan.getParameter()[0].equals("prima_cella")){
								//if(id==0) bla bla
                                abstract_robot.setDisplayString("STATO 10 - EXPLORE PT1");
                                PS_ATTRACTOR.setValue(curr_time, punto[2], punto[3]);
                                antistallo.resetI();
						}
						else
						if(explore_plan.getParameter()[0].equals("seconda_cella")){
							 abstract_robot.setDisplayString("STATO 10 - EXPLORE PT2");
                        PS_ATTRACTOR.setValue(curr_time, punto[0], punto[1]);
                        antistallo.resetI();
                        }
            }
            }
            PF_EMPTY_MEMORY.setValue(true); //vai allo stato 4
            
        } //fine stato 10        
               


  return(CSSTAT_OK);
  }
  
   // Consente l'aggiornamento del contatore delle bandierine
  public void trialEnd(){
  	
  	if (MOSTRA!=null) MOSTRA.dispose();
  	
  	
  }

  /*public void allontanati(long	curr_time,double x_robot, double y_robot,Vec2 flag) //si allontana dalle zone in cui si verificano stalli
  {
    if(x_robot==18.4 && (y_robot>=8.8 && y_robot<=17.1)) //parete sinistra labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot-1.2, y_robot-0.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE SINISTRA DEL LABIRINTO");
        MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE SINISTRA DEL LABIRINTO");
        
    }
    else if(x_robot>=17 && x_robot<=25 && y_robot<22.5) //parete superiore labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot+0.1, y_robot+1.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE SUPERIORE DEL LABIRINTO");
        MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE SUPERIORE DEL LABIRINTO");
    }
    else if(x_robot>=25 && x_robot<25.5 && y_robot<=22 && y_robot>=8) //parete destra labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot+1.2, y_robot-0.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE DESTRA DEL LABIRINTO");
        MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE DESTRA DEL LABIRINTO");
    }
    else if(x_robot>=19 && x_robot<=27 && y_robot<=8 && y_robot>=7) //parete inferiore labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot+0.1, y_robot-1.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE INFERIORE  DEL LABIRINTO");
        MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE INFERIORE DEL LABIRINTO");
    }
    
    

  }*/ 

    public void allontanati(long curr_time,double x_robot, double y_robot,Vec2 flag)
    {
        if(x_robot==18.4 && (y_robot>=8.8 && y_robot<=17.1) && !roomLocation.in_lab())  //parete sinistra labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot-1.2, y_robot-0.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE SINISTRA DEL LABIRINTO");
        swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
        //MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE SINISTRA DEL LABIRINTO");
        
    }
    else if(x_robot>=17 && (y_robot<=25 && y_robot>22.5) && !roomLocation.in_lab_top()) //parete superiore labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot+0.1, y_robot+1.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE SUPERIORE DEL LABIRINTO");
        swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
        //MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE SUPERIORE DEL LABIRINTO");
    }
    else if((x_robot>=25 && x_robot<25.5) && (y_robot<=22 && y_robot>=8) && !roomLocation.in_lab() ) //parete destra labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot+1.2, y_robot-0.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE DESTRA DEL LABIRINTO");
        swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
        //MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE DESTRA DEL LABIRINTO");
    }
    else if((x_robot>=19 && x_robot<=27) && (y_robot<=8 && y_robot>=7) && !roomLocation.in_lab()) //parete inferiore labirinto
    {
        PS_ATTRACTOR.setValue(curr_time, x_robot+0.1, y_robot-1.2);
        PS_ATTRACTOR_FLAG.setValue(curr_time,flag.x,flag.y);
        System.out.println("***********************MI ALLONTANO DALLA PARETE INFERIORE  DEL LABIRINTO");
        swirlgain.setValue(0.5);
                    avoidgain.setValue(0.0);
                    noisegain.setValue(0.0);
        //MOSTRA_REPORT.setReport("***********************MI ALLONTANO DALLA PARETE INFERIORE DEL LABIRINTO");
    }
    
    }

  
 }   
 