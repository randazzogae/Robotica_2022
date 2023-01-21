/*
 * ActionList.java
 *
 * Created on 10 dicembre 2002, 22.08
 */

package CSAI.unipa.SGolog;

import EDU.gatech.cc.is.clay.*;

import CSAI.unipa.clay.*;
import java.util.*;

/**
 *
 * @author Marco Di Stefano & Carmelo Scozzola
 */
public class ActionList{
    
    
    public static final boolean DEBUG =     Node.DEBUG;
    private Vector                          embedded;
    
    public ActionList() {
        embedded = new Vector();
    }    
    
    
    /**
     * Get the NodeVec2 value.
     *
     * @param timestamp long indicates time of the request
     * @param String int indicates the name of the action
     * @return the NodeVec2 action
     */
    public NodeVec2 steeringValue(String item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(indexOfString(item));
            return act.steeringValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(new v_Noise_(0));
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(new v_Noise_(0));
        }
    }
    
    /**
     * Get the NodeVec2 value.
     *
     * @param timestamp long indicates time of the request
     * @param item int indicates the position of the action in the array
     * @return the NodeVec2 action
     */
    public NodeVec2 steeringValue(int item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act.steeringValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(new v_Noise_(0));
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(new v_Noise_(0));
        }
    }
    
    /**
     * Get the NodeBoolean value.
     *
     * @param timestamp long indicates time of the request
     * @param item int indicates the position of the trigger in the array
     * @return the NodeBoolean trigger
     */
    public NodeBoolean triggerValue(int item){
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act.triggerValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error b "+ie);
            return(new NodeBooleanDinamic(false));
        }
        catch(NullPointerException ne){
            System.out.println("Error b "+ne);
            return(new NodeBooleanDinamic(false));
        }    
    }
    
    /**
     * Get the NodeBoolean value.
     *
     * @param timestamp long indicates time of the request
     * @param String int indicates the name of the trigger
     * @return the trigger value
     */
    public NodeBoolean triggerValue(String item){
        try{
            NodeAction act = (NodeAction)embedded.elementAt(indexOfString(item));
            return act.triggerValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error b "+ie);
            return(new NodeBooleanDinamic(false));
        }
        catch(NullPointerException ne){
            System.out.println("Error b "+ne);
            return(new NodeBooleanDinamic(false));
        }    
    }
    
    
    
    /**
     * Get the position of the action named item.
     *
     * @param timestamp long indicates time of the request
     * @param item int indicates the name of the action
     * @return the int position of the action named item
     */
    public int indexOfString(String item) {
        for(int i=0; i<embedded.size();i++){
            NodeAction act = (NodeAction)embedded.elementAt(i);
            if(act.stringValue().equals(item)){
                return(i);
            }
        }
        return(-1);
    }
    
    /**
     * Get the String value.
     *
     * @param timestamp long indicates time of the request
     * @param item int indicates the position in the array
     * @return the String value
     */
    public String stringValue(int item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act.stringValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return("");
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return("");
        }
    }
    
    /**
     * Get the NodeBoolean value.
     *
     * @param timestamp long indicates time of the request
     * @param String int indicates the name of the trigger
     * @return the trigger value
     */
    public NodeVec2 turretValue(String item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(indexOfString(item));
            return act.turretValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(new v_Noise_(0));
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(new v_Noise_(0));
        }    
    }
    
    /**
     * Get the NodeBoolean value.
     *
     * @param timestamp long indicates time of the request
     * @param item int indicates the position of the trigger in the array
     * @return the NodeBoolean trigger
     */
    public NodeVec2 turretValue(int item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act.turretValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(new v_Noise_(0));
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(new v_Noise_(0));
        }    
    }
    
    /**
     * Get the dimension of the array
     *
     *@return the ine dimension
     */
    public int getLength() {
        return embedded.size();
    }
    
    /**
     * Get the action of position item in the array
     *
     * @param item int the position of the action
     * @return the NodeAction
     */
    public NodeAction getNodeAction(int item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act;
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(new NodeAction());
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(new NodeAction());
        }    
    }
    
    /**
     * Get the int value.
     *
     * @param timestamp long indicates time of the request
     * @param String int indicates the name of the trigger
     * @return the int gripper value
     */
    public int laserValue(String item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(indexOfString(item));
            return act.laserValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(-1);
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(-1);
        }    
    }
    
    /**
     * Get the int value.
     *
     * @param timestamp long indicates time of the request
     * @param String int indicates the name of the gripper configuration
     * @return the int gripper value
     */
    public int laserValue(int item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act.laserValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(-1);
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(-1);
        }    
    }
    
    /**
     * Get the int value.
     *
     * @param timestamp long indicates time of the request
     * @param String int indicates the position of the gripper configuration
     * @return the int gripper value
     */
    public int gripperValue(int item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act.gripperValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(0);
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(0);
        }    
    }
    
    /**
     * Get the int value.
     *
     * @param timestamp long indicates time of the request
     * @param String int indicates the name of the gripper configuration
     * @return the int gripper value
     */
    public int gripperValue(String item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(indexOfString(item));
            return act.gripperValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(0);
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(0);
        }    
    }
    
    /**
     * Get the int[] value.
     *
     * @param timestamp long indicates time of the request
     * @param item String indicates the name of the gripper configuration
     * @return the int gripper value
     */
    public int[] sonarValue(String item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(indexOfString(item));
            return act.sonarValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(new int[]{});
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(new int[]{});
        }    
    }
    
    /**
     * Get the int[] value.
     *
     * @param timestamp long indicates time of the request
     * @param item int indicates the position of the sonar configuration
     * @return the int gripper value
     */
    public int[] sonarValue(int item) {
        try{
            NodeAction act = (NodeAction)embedded.elementAt(item);
            return act.sonarValue();
        }
        catch(ArrayIndexOutOfBoundsException ie){
            System.out.println("Error "+ie);
            return(new int[]{});
        }
        catch(NullPointerException ne){
            System.out.println("Error "+ne);
            return(new int[]{});
        }   
    }
    
    /**
     * Adds the specified item to the end of scrolling list.
     * @param item the item to be added
     * @since JDK1.1
     */
    public void add(NodeAction item) {
        add(item,-1);
    }    
    
    /**
     * Adds the specified item to the the scrolling list
     * at the position indicated by the index.  The index is
     * zero-based.  If the value of the index is less than zero,
     * or if the value of the index is greater than or equal to
     * the number of embedded in the list, then the item is added
     * to the end of the list.
     * @param       item   the item to be added;
     *             if this parameter is <code>null</code> then the item is
     *             treated as an empty string, <code>""</code>
     * @param       index  the position at which to add the item
     * @since       JDK1.1
     */
    public void add(NodeAction item, int index) {
        if (index < -1 || index >= embedded.size()) {
            index = -1;
        }
        
        if (item == null) {
            item = new NodeAction();
        }
        
        if (index == -1) {
            embedded.addElement(item);
        } else {
            embedded.insertElementAt(item, index);
        }
        
    }
    
    
    /**
     * @deprecated     replaced by <code>remove(String)</code>
     *                        and <code>remove(int)</code>.
     */
    private void delItem(int position) {
        delembedded(position, position);
    }
    
    /**
     * @deprecated As of JDK version 1.1,
     * Not for public use in the future.
     * This method is expected to be retained only as a package
     * private method.
     */
    private synchronized void delembedded(int start, int end) {
        for (int i = end; i >= start; i--) {
            embedded.removeElementAt(i);
        }
    }
    
    /**
     * Gets the item associated with the specified index.
     * @return       an item that is associated with
     *                   the specified index
     * @param        index the position of the item
     * @see          #getItemCount
     */
    public NodeAction getItem(int index) {
        return getItemImpl(index);
    }
    
    /**
     * Gets the number of embedded in the list.
     * @return     the number of embedded in the list
     * @see        #getItem
     * @since      JDK1.1
     */
    public int getItemCount() {
        return embedded.size();
    }
    
    /**
     * Gets the embedded in the list.
     * @return       a string array containing embedded of the list
     * @see          #select
     * @see          #deselect
     * @see          #isIndexSelected
     * @since        JDK1.1
     */
    public synchronized String[] getembedded() {
        String itemCopies[] = new String[embedded.size()];
        embedded.copyInto(itemCopies);
        return itemCopies;
    }

    
    /**
     * Removes the first occurrence of an item from the list.
     * @param        item  the item to remove from the list
     * @exception    IllegalArgumentException
     *                    if the item doesn't exist in the list
     * @since        JDK1.1
     */
    public synchronized void remove(NodeAction item) {
        int index = embedded.indexOf(item);
        if (index < 0) {
            throw new IllegalArgumentException("item " + item +
            " not found in list");
        } else {
            remove(index);
        }
    }
    
    /**
     * Remove the item at the specified position
     * from this scrolling list.
     * @param      position   the index of the item to delete
     * @see        #add(String, int)
     * @since      JDK1.1
     * @exception    ArrayIndexOutOfBoundsException
     *              if the <code>position</code> is less than 0 or
     *              greater than <code>getItemCount()-1</code>
     */
    public void remove(int position) {
        delItem(position);
    }
    
    /**
     * Removes all embedded from this list.
     * @see #remove
     * @see #delembedded
     * @since JDK1.1
     */
    public void removeAll() {
        clear();
    }
    
    /**
     * Replaces the item at the specified index in the scrolling list
     * with the new string.
     * @param       newValue   a new string to replace an existing item
     * @param       index      the position of the item to replace
     * @exception ArrayIndexOutOfBoundsException if <code>index</code>
     * 		is out of range
     */
    public synchronized void replaceItem(NodeAction newValue, int index) {
        remove(index);
        add(newValue, index);
    }
    
    final NodeAction getItemImpl(int index) {
        return (NodeAction)embedded.elementAt(index);
    }
    
    /**
     * @deprecated As of JDK version 1.1,
     * replaced by <code>removeAll()</code>.
     */
    private synchronized void clear() {
        embedded = new Vector();
    }
    
    public boolean isInList(String item){
        int index = indexOfString(item);
        if(index==-1){
            //System.out.println("azione "+item+" non in lista");
            return(false);
        }
        else{
            //System.out.println("azione "+item+" in lista");
            return(true);
        }
    }
    
}
