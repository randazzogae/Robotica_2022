

import	EDU.gatech.cc.is.util.Vec2;
import	EDU.gatech.cc.is.clay.*;
import  EDU.gatech.cc.is.abstractrobot.*;
 

public class i_myscore_r extends NodeInt
  {
    
    private int score=0;
    private boolean gripper_actual=false, gripper_prior=false,
    				position_actual=false, position_prior=false;
    private NodeVec2 PS_GLOBAL_POS,PS_HOMEBASE0_GLOBAL,PS_HOMEBASE1_GLOBAL,
    				 PS_HOMEBASE2_GLOBAL;
    private NodeBoolean PF_CLOSE_TO_HOMEBASE0,PF_CLOSE_TO_HOMEBASE1,PF_CLOSE_TO_HOMEBASE2;
    private NodeInt PS_IN_GRIPPER;
    private String Name;
    
    public i_myscore_r (SimpleInterface abstract_robot,String Name)
    {	 
    	this.Name=Name;
 		 PS_GLOBAL_POS=new v_GlobalPosition_r(abstract_robot);
		//homebase 0
		 PS_HOMEBASE0_GLOBAL=new v_FixedPoint_(2.5,14);
		//homebase 1
		 PS_HOMEBASE1_GLOBAL=new v_FixedPoint_(2.5,5.0);
		//homebase 2
		 PS_HOMEBASE2_GLOBAL=new v_FixedPoint_(2.5,23.0);
		//vicinanza alla homebase 0
		 PF_CLOSE_TO_HOMEBASE0=new b_Close_vv(1.3,PS_GLOBAL_POS,PS_HOMEBASE0_GLOBAL);
		//vicinanza alla homebase 1
		 PF_CLOSE_TO_HOMEBASE1=new b_Close_vv(1.3,PS_GLOBAL_POS,PS_HOMEBASE1_GLOBAL);
		//vicinanza alla homebase 2
		 PF_CLOSE_TO_HOMEBASE2=new b_Close_vv(1.3,PS_GLOBAL_POS,PS_HOMEBASE2_GLOBAL);
		
		 PS_IN_GRIPPER=new i_InGripper_r((GripperActuator) abstract_robot);
    }
    
    public int Value(long curr_time)
    { // Ha qualcosa nel gripper ?
    gripper_actual = (PS_IN_GRIPPER.Value(curr_time)>-1);
    
    if (gripper_actual!=gripper_prior)
    // se c'è stato un cambiamento nel gripper dobbiamo chiederci
    // è vicino ad una delle HB ?	
    position_actual = (PF_CLOSE_TO_HOMEBASE0.Value(curr_time)
    				  					||
    					PF_CLOSE_TO_HOMEBASE1.Value(curr_time)
	    								||
    					PF_CLOSE_TO_HOMEBASE2.Value(curr_time));
    
    if ((gripper_prior)  &&  (!gripper_actual)
    					 &&
       (!position_prior) &&  (position_actual))
       
       {score=score+2;
       //System.out.println(Name+" -> "+score+" flag(s).");
       }

    gripper_prior = gripper_actual;
    position_prior = position_actual;
    return(score);    
    }
}