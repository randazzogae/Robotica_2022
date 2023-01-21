
import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.abstractrobot.*;
import	EDU.gatech.cc.is.clay.*;

import CSAI.unipa.clay.*;
import CSAI.unipa.abstractrobot.*;
import CSAI.unipa.SGolog.*;

import java.awt.*;
import java.io.*;


public class Agent extends ControlSystemMFN150
	{
	public final static boolean DEBUG = true;
	private NodeVec2                turret_configuration;
	private NodeVec2                steering_configuration;
	private NodeDouble              gripper_fingers_configuration;
	private double                  mtggain = 1.0;     // move to goal gain 
	private double                  avoidgain = 0.8; // avoid obstacle 
	private double                  noisegain = 0.2; // noise
        private double                  swirlgain = 0.7;
	private NodeInt                 state_monitor;
	private i_FSA_ba                STATE_MACHINE;
        private v_DinamicPoint_         PS_ATTRACTOR;
        private sGologClient            client;
        private SGologPlan              plan;
        private boolean                 gate_A=false;
        private int                     time = 0;
        private NodeBooleanDinamic      PF_IS_LATE;


	/**
	 * Configure the control system using Clay.
	 */
	public void configure(){

            //======
            // Set some initial hardware configurations.
            //======

            abstract_robot.setObstacleMaxRange(3.0); 

            abstract_robot.setBaseSpeed(0.8*abstract_robot.MAX_TRANSLATION);

            abstract_robot.setGripperHeight(-1,0);   

            abstract_robot.setGripperFingers(-1,0); 

            //======
            // perceptual schemas
            //======
            //--- robot's global position
            NodeVec2
            PS_GLOBAL_POS = new v_GlobalPosition_r(abstract_robot);

            //--- obstacles
            NodeVec2Array // the sonar readings
            PS_OBS = new va_Obstacles_r(abstract_robot);

            PS_ATTRACTOR = new v_DinamicPoint_(PS_GLOBAL_POS, 0.0, 0.0);

            NodeVec2Array 
            PS_FLAG_EGO = new va_VisualObjects_r(2,abstract_robot); 

            NodeInt
            PS_IN_GRIPPER = new i_InGripper_r(abstract_robot); 

            NodeVec2
            PS_CLOSEST_TARGET = new v_Closest_va(PS_FLAG_EGO);


            //======
            // Perceptual Features
            //======

            NodeBoolean
            PF_CLOSE_TO = new b_CloseDinamic_vv(0.6, PS_GLOBAL_POS, PS_ATTRACTOR);

            NodeBoolean
            PF_SOMETHING_VISIBLE=new b_NonZero_v(PS_CLOSEST_TARGET);

            NodeBoolean
            PF_FLAG_IN_GRIPPER = new b_Equal_i(2,PS_IN_GRIPPER);

            NodeBoolean
            PF_BALL_IN_GRIPPER = new b_Equal_i(1,PS_IN_GRIPPER);

            PF_IS_LATE = new NodeBooleanDinamic(false);

            NodeBoolean 
            PF_SOMETHING_IN_GRIPPER = new b_Persist_s (4.0,new b_NonNegative_s(PS_IN_GRIPPER));



            //======
            // motor schemas
            //======
            NodeVec2
            MS_AVOID_OBSTACLES = new v_Avoid_va(3.0,
                    abstract_robot.RADIUS + 0.1,
                    PS_OBS);

            NodeVec2
            MS_MOVE_TO = new v_LinearAttraction_v(0.4,0.0,PS_ATTRACTOR);

            NodeVec2
            MS_NOISE_VECTOR = new v_Noise_(10,5);

            NodeVec2
            MS_SWIRL_TO = new v_Swirl_vav(1.5, abstract_robot.RADIUS + 0.18, 
                                               PS_OBS, PS_ATTRACTOR);

            NodeVec2
            MS_MOVE_TO_TARGET = new v_LinearAttraction_v(0.4,0.0,PS_CLOSEST_TARGET);


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
            // AS_GO_TARGET
            //======
            v_StaticWeightedSum_va 
            AS_GO_TARGET = new v_StaticWeightedSum_va();

            AS_GO_TARGET.weights[0]  = avoidgain;
            AS_GO_TARGET.embedded[0] = MS_AVOID_OBSTACLES;

            AS_GO_TARGET.weights[1]  = noisegain;
            AS_GO_TARGET.embedded[1] = MS_NOISE_VECTOR;

            AS_GO_TARGET.weights[2]  = mtggain;
            AS_GO_TARGET.embedded[2] = MS_MOVE_TO_TARGET;  
            
            File eclipseProgram = new File(".\\Database\\sGolog.pl");
            client = new sGologClient(eclipseProgram);

            eclipseProgram = new File(".\\Database\\airport.pl");
            client.consultFile(eclipseProgram); 

            plan = new SGologPlan(abstract_robot); 

            plan.debugging(true);
            
            plan.addAction("goto", 
                       AS_GOTO, 
                       AS_GOTO,
                       PF_CLOSE_TO, 
                       NodeAction.NO_GRIPPER);
           
           plan.addAction("buy_paper", 
                       AS_GOTO, 
                       AS_GOTO,
                       PF_CLOSE_TO, 
                       NodeAction.NO_GRIPPER);
                       
           plan.addAction("buy_coffee", 
                       AS_GOTO, 
                       AS_GOTO,
                       PF_CLOSE_TO, 
                       NodeAction.NO_GRIPPER);
           
           plan.addAction("smoke", 
                       AS_GOTO, 
                       AS_GOTO,
                       PF_CLOSE_TO, 
                       NodeAction.NO_GRIPPER);
                       
           plan.addAction("board_plane", 
                       AS_GOTO, 
                       AS_GOTO,
                       PF_CLOSE_TO, 
                       NodeAction.NO_GRIPPER);
                       
           
           plan.addAction("it_is_gate_A", PF_SOMETHING_VISIBLE);
           
           plan.addAction("it_is_late", PF_IS_LATE);
           
           plan.setPlan(client.invoke("do(catch_plane,s0,S)"),0);  
            
            
            //======
            // STATE_MACHINE
            //======
            STATE_MACHINE = new i_FSA_ba();

            STATE_MACHINE.state = 0;

            STATE_MACHINE.triggers[0][0]  = plan.isFinished(abstract_robot.getTime());
            STATE_MACHINE.follow_on[0][0] = 1;

            state_monitor = STATE_MACHINE;


            //======
            // STEERING
            //======
            v_Select_vai
            STEERING = new v_Select_vai((NodeInt)STATE_MACHINE);

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

            time++;
            if(time==490){
                PF_IS_LATE.setValue(true);
                System.out.println("\nWARNING: IT IS BECOMING TOO LATE TO SMOKE A CIGARETTE\n");
            }
            int state=STATE_MACHINE.Value(curr_time);
            
            plan.update(curr_time, state);
            
            if(plan.isChangedState()){
                action = plan.getActionName();
                if (action.startsWith("goto(")){
                    if(plan.getParameter()[0].equals("gate_A")){
                        PS_ATTRACTOR.setValue(curr_time, -2.5, -0.2);
                        gate_A=true;
                    }
                    else
                    if(plan.getParameter()[0].equals("gate_B")){
                        PS_ATTRACTOR.setValue(curr_time, 5.0, -6.5);
                    }
                } else
                if (action.startsWith("buy_pa")){
                    PS_ATTRACTOR.setValue(curr_time, -2.5, -6.5);
                } else
                if (action.startsWith("buy_co")){
                    PS_ATTRACTOR.setValue(curr_time, -0.3, 6.6);
                } else
                if (action.startsWith("smoke")){
                    PS_ATTRACTOR.setValue(curr_time, -0.3, 9.5);
                } else
                if (action.startsWith("board")){
                    if(gate_A)
                        PS_ATTRACTOR.setValue(curr_time, -9.5, 9.0);
                    else
                        PS_ATTRACTOR.setValue(curr_time, 9.5, 9.0);
                }
            }

            return(CSSTAT_OK);
        }
    }

