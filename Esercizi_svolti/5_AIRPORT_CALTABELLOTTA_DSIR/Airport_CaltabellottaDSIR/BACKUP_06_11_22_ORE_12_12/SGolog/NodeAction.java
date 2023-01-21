/*
 * a_Action_vbs.java
 *
 * Created on 10 dicembre 2002, 22.00
 */

package CSAI.unipa.SGolog;

import EDU.gatech.cc.is.clay.*;

import CSAI.unipa.clay.*;

/**
 *
 * @author Marco Di Stefano & Carmelo Scozzola
 */
public class NodeAction extends Node {
    
    public static final boolean DEBUG = Node.DEBUG;
    
    private String                      name;
    private NodeVec2                    steering;
    private NodeVec2                    turret;
    private NodeBoolean                 trigger;
    private int                         gripper;
    private int[]                       sonar;
    private int                         laser;
    
    public static final NodeVec2        NO_STEERING = null;
    public static final NodeVec2        NO_TURRET = null;
    public static final NodeBooleanDinamic     IMMEDIATE_TRIGGER = new NodeBooleanDinamic(true);
    public static final int             NO_GRIPPER = 1000;   
    public static final int[]           NO_SONAR = new int[]{1000};
    public static final int             NO_LASER = -1;
    public static final int             LASER_ON = 1;
    public static final int             LASER_OFF = 0;
    
    
    /** Creates a new instance of a_NodeAction */
    public NodeAction(String n, NodeVec2 steer, NodeVec2 turr, NodeBoolean tr, int grip, int[] son_conf, int las) {
        name = n;
        steering = steer;
        turret = turr;
        trigger = tr;
        gripper = grip;
        sonar = son_conf;
        laser = las;
    }
    
    /** Creates a new instance of a_NodeAction */
    public NodeAction(NodeAction act) {
        name = act.stringValue();
        steering = act.steeringValue();
        turret = act.turretValue();
        trigger = act.triggerValue();
        gripper =  act.gripperValue();
        sonar = act.sonarValue();
        laser = act.laserValue();
    }
    
    /** Creates a new instance of a_NodeAction */
    public NodeAction() {
        name = "";
        steering = NodeAction.NO_STEERING;
        turret = NodeAction.NO_TURRET;
        trigger = NodeAction.IMMEDIATE_TRIGGER;
        gripper = NodeAction.NO_GRIPPER;
        sonar = NodeAction.NO_SONAR;
        laser = NodeAction.NO_LASER;
    }
    
    /**
     * Get the NodeVec2 value.
     *
     * @return the NodeVec2 steering
     */
    public NodeVec2 steeringValue() {
        return steering;
    }
    
    /**
     * Get the NodeVec2 value.
     *
     * @return the NodeVec2 turret
     */
    public NodeVec2 turretValue() {
        return turret;
    }

    
    /**
     * Get the String value.
     *
     * @return the String value
     */
    public String stringValue() {
        return name;
    }
    
    
    /**
     * Get the NodeBoolean value.
     *
     * @return the NodeBoolean trigger
     */
    public NodeBoolean triggerValue() {
        return trigger;
    }
    
    /**
     * Get the int[] value.
     *
     * @return the int[] sonar
     */
    public int[] sonarValue() {
        return sonar;
    }
    
    /**
     * Get the int value.
     *
     * @return the int laser
     */
    public int laserValue() {
        return laser;
    }
    
    /**
     * Get the int value.
     *
     * @return the int gripper
     */
    public int gripperValue() {
        return gripper;
    }
    
    
}
