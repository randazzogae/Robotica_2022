
import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.abstractrobot.*;
import	EDU.gatech.cc.is.clay.*;

import CSAI.unipa.clay.*;
import CSAI.unipa.abstractrobot.*;
import CSAI.unipa.SGolog.*;


import java.awt.*;
import java.io.*;
import  EDU.gatech.cc.is.communication.*;

//DA IMPORTARE:
import  java.util.*;
import	java.util.Enumeration;
import	EDU.gatech.cc.is.util.*;







public class Agent extends ControlSystemMFN150
	{
	public final static boolean DEBUG = true;
	private NodeVec2                turret_configuration;
	private NodeVec2                steering_configuration;
	private NodeDouble              gripper_fingers_configuration;
	private double                  mtggain = 1.0;     // move to goal gain 
	private double                  avoidgain = 0.7; // avoid obstacle 
	private double                  noisegain = 0.2; // noise
        private double                  swirlgain = 0.7;
	private NodeInt                 state_monitor;
	private i_FSA_ba                STATE_MACHINE;
        private v_DinamicPoint_         PS_ATTRACTOR;
        private sGologClient            client;
        private SGologPlan              plan;
        private boolean                 gate_A=false;
        private boolean                 gate_C=false;
        private int                     time = 0;
        private NodeBooleanDinamic      PF_MESSAGE_RECEIVED;

        //dichiaro il buffer per i messaggi e definisco il flag per verificare la ricezione del messaggio:
        private Enumeration BufMsg;
        private boolean RicevutoDaRagazza=false;
        private boolean aspetta=false;
      
        //NodeBoolean PF_FLAG_VISIBLE;

	/**
	 * Configure the control system using Clay.
	 */
	public void configure(){

            //======
            // Set some initial hardware configurations.
            //======

            // definisco il buffer dei messaggi.
            BufMsg=abstract_robot.getReceiveChannel();

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
            
            NodeVec2Array 
            PS_FLOWER_EGO = new va_VisualObjects_r(3,abstract_robot);

			NodeVec2Array
			PS_BOTTLE_EGO = new va_VisualObjects_r(4, abstract_robot);

            NodeInt
            PS_IN_GRIPPER = new i_InGripper_r(abstract_robot); 

            NodeVec2
            PS_FLAG = new v_Closest_va(PS_FLAG_EGO);
            
            NodeVec2
            PS_FLOWER = new v_Closest_va(PS_FLOWER_EGO);

			NodeVec2
			PS_BOTTLE = new v_Closest_va(PS_BOTTLE_EGO);


            //======
            // Perceptual Features
            //======

            NodeBoolean
            PF_CLOSE_TO = new b_CloseDinamic_vv(0.6, PS_GLOBAL_POS, PS_ATTRACTOR);

            NodeBoolean
            PF_FLAG_VISIBLE=new b_NonZero_v(PS_FLAG);

            NodeBoolean
            PF_FLOWER_VISIBLE=new b_NonZero_v(PS_FLOWER);

			NodeBoolean
			PF_BOTTLE_VISIBLE = new b_NonZero_v(PS_BOTTLE);
            
            NodeBoolean
            PF_FLOWER_IN_GRIPPER = new b_Equal_i(3,PS_IN_GRIPPER);

			NodeBoolean
			PF_BOTTLE_IN_GRIPPER = new b_Equal_i(4, PS_IN_GRIPPER);

            NodeBoolean
            PF_BALL_IN_GRIPPER = new b_Equal_i(1,PS_IN_GRIPPER);

            PF_MESSAGE_RECEIVED = new NodeBooleanDinamic(true);

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

            //NodeVec2
            //MS_MOVE_TO_TARGET = new v_LinearAttraction_v(0.4,0.0,PS_CLOSEST_TARGET);


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
           // AS_GO_TARGET.embedded[2] = MS_MOVE_TO_TARGET;  
            
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
           
           plan.addAction("get_info", 
                   AS_GOTO, 
                   AS_GOTO,
                   PF_CLOSE_TO, 
                   NodeAction.NO_GRIPPER);
				   
		   plan.addAction("bar", 
                   AS_GOTO, 
                   AS_GOTO,
                   PF_CLOSE_TO, 
                   NodeAction.NO_GRIPPER);

		   plan.addAction("take_bottle",
					 AS_GOTO,
					 AS_GOTO,
					 PF_BOTTLE_IN_GRIPPER,  
					 -1);

		   plan.addAction("recycle",
						AS_GOTO,
						AS_GOTO,
						PF_CLOSE_TO,  
						1);
           
           plan.addAction("buy_flower", 
                   AS_GOTO, 
                   AS_GOTO,
                   PF_FLOWER_IN_GRIPPER, 
                   -1);
           
           plan.addAction("drop_flower", 
                   AS_GOTO, 
                   AS_GOTO,
                   PF_CLOSE_TO, 
                   1);

             plan.addAction("wait", 
                   AS_GOTO, 
                   AS_GOTO,
                   PF_CLOSE_TO, 
                   NodeAction.NO_GRIPPER);
                       
           
           plan.addAction("it_is_gate_A", PF_FLAG_VISIBLE);
           
           plan.addAction("it_is_gate_C", PF_FLOWER_VISIBLE);
           
           plan.addAction("message_received", PF_MESSAGE_RECEIVED);


           plan.addAction("bottle", PF_BOTTLE_VISIBLE);
           
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
            long	curr_time=abstract_robot.getTime();
            String  action;
            
           
           if(aspetta==true)
           {
            abstract_robot.setSpeed(curr_time,0.0);
            //ricevo messaggio
          if (BufMsg.hasMoreElements())
          {
   			LongMessage RicevutoDa = (LongMessage)BufMsg.nextElement();
 			if (BufMsg.hasMoreElements()){
      				StringMessage RicevutoMsg = (StringMessage)BufMsg.nextElement();
      				if (RicevutoMsg.val=="Thank you very much" && RicevutoDa.val==4){
      					//NOTA: 4 e' l'id del fembot (cioè quello che fembot invia con msg1)
                        System.out.println("AGENT COMUNICA DI AVER RICEVUTO DA FEMBOT IL MESSAGGIO: ' "+RicevutoMsg.val+" '");
      					RicevutoDaRagazza=true;
                        System.out.println("------AGENT PUO' ANDARE------");
      				}
      			}
                aspetta=false;
                //PS_ATTRACTOR.setValue(curr_time, 1.00, 2.00); //PUNTO DI ATTESA
                abstract_robot.setSpeed(curr_time,80.0);
                
                
   		}
        else
        {
            //PS_ATTRACTOR.setValue(curr_time, 1.00, 2.00); //PUNTO DI ATTESA
            //abstract_robot.setSpeed(curr_time,0.0);
            return(CSSTAT_OK);
        }
   		}
           
            //System.out.println(PF_MESSAGE_RECEIVED);

            //System.out.println(PF_FLAG_VISIBLE);
            //ricevo messaggio
          if (BufMsg.hasMoreElements()){
   			LongMessage RicevutoDa = (LongMessage)BufMsg.nextElement();
 			if (BufMsg.hasMoreElements()){
      				StringMessage RicevutoMsg = (StringMessage)BufMsg.nextElement();
      				if (RicevutoMsg.val=="SI, SEMPRE FORZA INTER!!" && RicevutoDa.val==4){
      					//NOTA: 4 e' l'id del fembot (cioè quello che fembot invia con msg1)
                        System.out.println("AGENT COMUNICA DI AVER RICEVUTO DA FEMBOT IL MESSAGGIO: ' "+RicevutoMsg.val+" '");
      					RicevutoDaRagazza=true;
                        System.out.println("------AGENT PUO' ANDARE------");
      				}
      			}
   		}

            /*time++;
            //time=490;
            if(time==490){
                PF_MESSAGE_RECEIVED.setValue(true);
                System.out.println("\nWARNING: IT IS BECOMING TOO LATE TO SMOKE A CIGARETTE\n");
            }
            */
            int state=STATE_MACHINE.Value(curr_time);
            
            plan.update(curr_time, state);
            
            if(plan.isChangedState()){
                action = plan.getActionName();
                if (action.startsWith("goto(")){
                    if(plan.getParameter()[0].equals("gate_A")){
                        PS_ATTRACTOR.setValue(curr_time, -2.5, -0.2);
                        gate_A=true;
                    }else
                 if(plan.getParameter()[0].equals("gate_B")){
                    PS_ATTRACTOR.setValue(curr_time, 5.0, -6.5);
                    }else
                 if(plan.getParameter()[0].equals("gate_C")){
                	 PS_ATTRACTOR.setValue(curr_time, 5.0, -6.5);
                	 gate_C=true;
                 }else
                 if(plan.getParameter()[0].equals("imbarco_C")){
                    	 PS_ATTRACTOR.setValue(curr_time, 12.0, 6.5);
                }
				else
                 if(plan.getParameter()[0].equals("bar")){
					     PS_ATTRACTOR.setValue(curr_time, 17.03, 3.70);
                }
				else
					if (plan.getParameter()[0].equals("piazza")){
						PS_ATTRACTOR.setValue(curr_time, 10.1, 0.7);
                        
			    }else 
		     	if (plan.getParameter()[0].equals("cestino")){
					    PS_ATTRACTOR.setValue(curr_time, 11.00, 9.50);
				}else
			    if (plan.getParameter()[0].equals("fioraio")) {
				        PS_ATTRACTOR.setValue(curr_time, 13.7, -7.5);
			    }else
					if (plan.getParameter()[0].equals("ragazza"))
					{
						PS_ATTRACTOR.setValue(curr_time, 1.30, 2.70);
					}
                } else
                if (action.startsWith("buy_pa")){
                    PS_ATTRACTOR.setValue(curr_time, -2.5, -6.5);
                } else
                if (action.startsWith("buy_co")){
                    PS_ATTRACTOR.setValue(curr_time, -0.3, 6.6);
                } else
                if (action.startsWith("drop_flower")){
                    //invio messaggio a FemBot
                    for(int i=0;i<1000;i++);
                    LongMessage msg1 = new LongMessage();
                    msg1.val=abstract_robot.getPlayerNumber(curr_time);
                    abstract_robot.broadcast(msg1);
                    StringMessage msg2 = new StringMessage();
                    msg2.val="I give you this flower";
                    abstract_robot.broadcast(msg2);
                    System.out.println("Agent a FemBot: " + msg2.val);

                    
                    //PS_ATTRACTOR.setValue(curr_time, 1.00, 2.00); //PUNTO DI ATTESA

                    
                } else
                if (action.startsWith("wait")){
                    aspetta=true;  
                    PS_ATTRACTOR.setValue(curr_time, 0.5, 1.40); //PUNTO DI ATTESA
                    
				} else
				if (action.startsWith("take_bott")){    
					PS_ATTRACTOR.setValue(curr_time, 12.50, 7.00);     
				} else
				if (action.startsWith("recycle")){
					PS_ATTRACTOR.setValue(curr_time, 11.00, 9.50);
				} else 
                if (action.startsWith("buy_flower")){
                     PS_ATTRACTOR.setValue(curr_time, 16.03, -6.0);
                } else
                if (action.startsWith("smoke")){
                    PS_ATTRACTOR.setValue(curr_time, -0.3, 9.5);
                } else
                if (action.startsWith("board")){
                    if(gate_A)
                        PS_ATTRACTOR.setValue(curr_time, -9.5, 9.0);
                    else
                        PS_ATTRACTOR.setValue(curr_time, 9.5, 9.0);
                    if(gate_C)
                        PS_ATTRACTOR.setValue(curr_time, 18.0, 9.0);
                } else
                if (action.startsWith("get_info")){
                        PS_ATTRACTOR.setValue(curr_time, 17.03, -0.40);
                }
			}
            return(CSSTAT_OK);
        }


    }

