/*
 * SGologPlane.java
 *
 * Created on 19 dicembre 2002, 12.55
 */

package CSAI.unipa.SGolog;

import EDU.gatech.cc.is.abstractrobot.*;
import EDU.gatech.cc.is.util.*;
import EDU.gatech.cc.is.clay.*;

import com.parctechnologies.eclipse.*;
import java.io.*;
import java.awt.event.*;
import java.awt.*;
import java.util.StringTokenizer;
import javax.swing.text.*;

import CSAI.unipa.clay.*;
import CSAI.unipa.abstractrobot.*;
import CSAI.unipa.SGolog.*;
import EDU.gatech.cc.is.abstractrobot.*;


/**
 *
 * @author Marco Di Stefano & Carmelo Scozzola
 */
public class SGologPlan {
    
    /** the result of the invocation **/
    private List                        goal;
    /** the plan numerically formatted **/
    private List                        numberGoal;
    /** a copy of the plan used to reset **/
    private List                        bufferGoal;
    /** the name of the current action **/
    private String                      action;
    /** the parameters of the current action **/
    private String[]                    param;
    
    /** the robot state **/
    private NodeVec2                    steering;
    private NodeVec2                    turret;
    private NodeBoolean                 trigger;
    private int                         gripper_fingers;
    private int[]                       sonar;
    private int                         laser;
    
    /** the matched actions **/
    private ActionList                  actions;
    /** an instance of the robot **/
    private SimpleInterface             abstract_robot;
    /** the trigger to control the golog state **/
    private NodeBooleanDinamic          finished;
    /** the number of golog state in FSA **/
    private int                         gologState;
    /** the trigger sensed **/
    private List                        sensedTrigger;
    /** if true debugging is actived **/
    private boolean                     debug = false;
    private boolean                     isChangedState = false;
    
    
    /** Creates a new instance of SGologPlane */
    public SGologPlan(SimpleInterface ar) {
        finished = new NodeBooleanDinamic(false);
        actions = new ActionList();
        param = new String[0];
        abstract_robot = ar;
        trigger = NodeAction.IMMEDIATE_TRIGGER;
        sensedTrigger = new List();
        sonar = new int[0];
    }
    
   
    long            lasttime = 0;
    private int     count = 0;
    
    /** each timestep makes golog state go on **/
    public int update(long timestamp, int state){
        
        isChangedState = false;
        /** begins the sgolog state **/
        if(count == 0)
            finished.setValue(false);
        
        /** ends the sgolog state, reset and exit **/
        if(count==(goal.getItemCount()-1)){
            reset();
            return(0);
        }
        
        /** if sgolog state is not finished, and need to check the next action **/
        if ((!finished.Value(timestamp))&&
            (state==gologState)&&
            (trigger.Value(timestamp))&&
            ((timestamp > lasttime)||(timestamp == -1)))
        {
            /*--- reset the timestamp ---*/
            if (timestamp > 0) lasttime = timestamp;
        
            /** repeat until an action il performed **/
            do{
                action = goal.getItem(count++);
                /** do the sense at the trigger **/
                if(action.equals("sense")){
                    count = count + 3;
                    action = goal.getItem(count++);
                    performSense(timestamp, action);
                    action = goal.getItem(count);
                }
                else
                /** there is a sensing and a branch to cut **/    
                if(isSensedTrigger(action)){
                    cutBranch(timestamp, count++);
                    action = goal.getItem(count);
                }
                else
                /** is an action to perform **/    
                if(actions.isInList(action)){
                    readParameter();
                    performAction(timestamp, action);
                    isChangedState = true;
                }
                /** plan is finished, reset and exit **/
                if(count==(goal.getItemCount()-1)){
                    reset();
                    return(0);
                }
            }
            while((action.equals("["))||(action.equals("]"))||(action.equals(",")));            
        }   
        /** configure the robot if is in the golog state **/
        if(state==gologState)
            setRobotConfiguration(timestamp);
        return(0);
    }

    
    public void setPlan(List plan, int state) {
        sensedTrigger = new List();
        param = new String[0];
        count = 0;
        /** the goal **/
        goal = new List();
        trigger = NodeAction.IMMEDIATE_TRIGGER;
        for(int i=0;i<plan.getItemCount();i++)
            goal.add(plan.getItem(i));
        /** the copy of goal **/
        bufferGoal = new List();
        for(int i=0;i<goal.getItemCount();i++)
            bufferGoal.add(goal.getItem(i));
        gologState = state;
        /** creates the numerically formatted goal **/
        formatQuery(goal);
        if(debug){
            System.out.println("\n");
            for(int i=0;i<goal.getItemCount();i++)
                System.out.print(goal.getItem(i));
            System.out.println("\n");
        }
    }    
    
    /** return true if plan is finished **/
    public NodeBoolean isFinished(long timestamp) {
        return finished;
    }
    
    /** return the nodevec2 of steering **/
    public NodeVec2 getSteering(long timestamp) {
        if(steering == null)
            return(new v_GlobalPosition_r(abstract_robot));
        else
            return steering;
    }
    
    /** return the nodevec2 of turret **/
    public NodeVec2 getTurret(long timestamp) {
        if(turret == null)
            return(new v_GlobalPosition_r(abstract_robot));
        else
            return turret;
    }
    
    /** return the int of gripper **/
    public int getGripperFingers(long timestamp) {
        return gripper_fingers;
    }
    
    /** return the int[] of sonar **/
    public int[] getSonar(long timestamp){
        return sonar;
    }
    
    /** return the boolean of laser **/
    public boolean getLaser(long timestamp){
        if(laser==1)
            return true;
        else
            return false;
    }
    
    /** get the parameters of a function **/
    private void readParameter(){
        param = new String[0];
        List tmp = new List();
        /** if #param# there are parameters **/
        if(goal.getItem(count).equals("#param#")){
            count++;
            /** read the number of parameters **/
            int param_number = Integer.parseInt(goal.getItem(count++));
            if(debug)
                System.out.println("Funzione con parametri: "+param_number);
            int num = 0;
            /** repeat until read all the parameters **/
            do{
                String temp = goal.getItem(count++);
                /** if is a parameter add it to tmp, and increase num **/
                if((!temp.equals("["))&&(!temp.equals("]"))){
                    tmp.add(temp);
                    num++;
                }
            }while(num<param_number);
        }
        /** copy tmp into param **/
        param = new String[tmp.getItemCount()];
        for(int i=0;i<tmp.getItemCount();i++){
            param[i] = tmp.getItem(i);
        }
    }
    
    /** perform an action **/
    private void performAction(long timestamp, String action) {
        /** create the string to be displayed by the robot **/
        String p = "(";
        for(int i=0;i<param.length;i++){
            if(i!=0){
                p = p+", ";
            }
            p = p+param[i];
        }
        p = p+")";
        if(debug)
            System.out.println("Eseguo l'azione :"+action+p+"\n");
        /** assign the state of the robot **/
        abstract_robot.setDisplayString("SGolog: "+action+p);
        steering = actions.steeringValue(action);
        turret = actions.turretValue(action);
        sonar = actions.sonarValue(action);    
        gripper_fingers = actions.gripperValue(action);
        laser = actions.laserValue(action);
        trigger = actions.triggerValue(action);
    }
    
    /** make the sense of a trigger **/
    private void performSense(long timestamp, String action) {
        try{
            sensedTrigger.add(action);
            if(debug)
                System.out.println("Effettuo il sense sul trigger :"+action+"\n");
            trigger = NodeAction.IMMEDIATE_TRIGGER;
        }catch(ArrayIndexOutOfBoundsException ie){}
    }
    
    /** set the configuration of the robot dependign by the performing action **/
    private void setRobotConfiguration(long timestamp) {
        // STEERING
        if(steering!=null){
            Vec2 result = steering.Value(timestamp);
            abstract_robot.setSteerHeading(timestamp, result.t);
            abstract_robot.setSpeed(timestamp, result.r);
        }
        // TURRET
        if(turret!=null){
            Vec2 result = turret.Value(timestamp);
            try{
                SimpleN150ExploreSim simple =(SimpleN150ExploreSim)abstract_robot;
                simple.setTurretHeading(timestamp, result.t);
            }
            catch(Exception e){};
            try{
                SimpleN150Sim simple =(SimpleN150Sim)abstract_robot;
                simple.setTurretHeading(timestamp, result.t);
            }
            catch(Exception e){};
        }
        //SONAR
        if(sonar!=NodeAction.NO_SONAR){
            try{
                SonarObjectSensor sensor =(SonarObjectSensor)abstract_robot;
                sensor.turnOffAllSonar();
                for(int i=0;i<sonar.length;i++)
                    sensor.turnOnSonar(sonar[i]);
            }
            catch(Exception e){};
        }
        //LASER
        if(laser!=NodeAction.NO_LASER){
            try{
                LaserFinderObjectSensor sensor =(LaserFinderObjectSensor)abstract_robot;
                if(laser == 1)
                    sensor.turnOnLaser();
                else
                    sensor.turnOffLaser();
            }
            catch(Exception e){};
        }
        //GRIPPER FINGERS
        if(gripper_fingers!=NodeAction.NO_GRIPPER){
            try{
                SimpleN150ExploreSim simple =(SimpleN150ExploreSim)abstract_robot;
                simple.setGripperFingers(timestamp, gripper_fingers);
            }
            catch(Exception e){};
            try{
                SimpleN150Sim simple =(SimpleN150Sim)abstract_robot;
                simple.setGripperFingers(timestamp, gripper_fingers);
            }
            catch(Exception e){};
        }
    }
        
    /** control if is a sensed trigger **/
    private boolean isSensedTrigger(String action) {
        try{
            for(int i=0;i<sensedTrigger.getItemCount();i++)
                if(sensedTrigger.getItem(i).equals(action))
                    return(true);
        }catch(ArrayIndexOutOfBoundsException ie){}
        return(false);
    }
    
    
    private void cutBranch(long timestamp, int count){
        /** control the trigger **/
        param=new String[0];
        boolean bresult = actions.triggerValue(action).Value(timestamp);
        trigger = NodeAction.IMMEDIATE_TRIGGER;
        if(debug)
            System.out.println("Il trigger "+goal.getItem(count-1)+" ha valore "+bresult);
        
        int i = count;
        /** if is trigger false **/
        if(!bresult){
            if(debug)
                System.out.println("\nEseguo un branch cut:");
            String level = numberGoal.getItem(i);
            String act = null;
            do{
                act = numberGoal.getItem(i+1);
                numberGoal.remove(i+1);
                goal.remove(i+1);
            }
            while(!act.equals(level));
        }
        /** if is trigger true **/
        else{
            if(debug)
                System.out.println("\nEseguo un branch cut:");
            String level = numberGoal.getItem(i);
            String act = null;
            int cont = i+1;
            do{
                act = numberGoal.getItem(cont++);
            }
            while(!act.equals(level));
            do{
                act = numberGoal.getItem(cont);
                numberGoal.remove(cont);
                goal.remove(cont);
            }
            while(!act.equals(level));
        }
        if(debug){
            for(int j=0;j<goal.getItemCount();j++)
                System.out.print(goal.getItem(j));
            System.out.println("\n");
        }
    }
    
    /** create the numerically formatted plan **/
    private void formatQuery(List query) {
        int cont = 0;
        numberGoal = new List();
        for(int i=0;i<query.getItemCount();i++){ 
            numberGoal.add(query.getItem(i));
            if(query.getItem(i).equals("[")){
                cont++;
                numberGoal.replaceItem("@"+cont,i);
            }
            else
            if(query.getItem(i).equals("]")){
                cont--;
                numberGoal.replaceItem("@"+cont,i);
            }                         
        }
    }
    
    /** reset the plan **/
    private void reset(){
        if(debug)
            System.out.println("resetto");
        sensedTrigger = new List();
        param = new String[0];
        count = 0;
        goal = new List();
        for(int i=0;i<bufferGoal.getItemCount();i++)
            goal.add(bufferGoal.getItem(i));
        formatQuery(goal);
        if(debug){
            System.out.println("\n");
            for(int i=0;i<goal.getItemCount();i++)
                System.out.print(goal.getItem(i));
            System.out.println("\n");
        }
        finished.setValue(true);
    }
    
    public void addAction(String n, NodeVec2 steer, NodeVec2 turr, NodeBoolean tr, int grip, int[] son_conf, int las){
        actions.add(new NodeAction(n,steer,turr,tr,grip,son_conf,las));
    }
    
    public void addAction(String n, NodeBoolean tr){
        actions.add(new NodeAction(n,NodeAction.NO_STEERING,NodeAction.NO_TURRET,tr,NodeAction.NO_GRIPPER,NodeAction.NO_SONAR,NodeAction.NO_LASER));
    }    
    
    public void addAction(String n, NodeVec2 steer, NodeVec2 turr, NodeBoolean tr, int grip){
        actions.add(new NodeAction(n,steer,turr,tr,grip,NodeAction.NO_SONAR, NodeAction.NO_LASER));
    }
    
    public void debugging(boolean value){
        debug = value;
    }
    
    public String[] getParameter() {
        return param;
    }
    
    public String getActionName(){
        String p = action+"(";
        for(int i=0;i<param.length;i++){
            if(i!=0){
                p = p+", ";
            }
            p = p+param[i];
        }
        p = p+")";
        return p;
    }
    
    public boolean isChangedState(){
        return isChangedState;
    }
    
}
