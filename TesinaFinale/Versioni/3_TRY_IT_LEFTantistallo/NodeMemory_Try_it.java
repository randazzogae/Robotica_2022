/*
 * NodeVec2Array.java
 */

import EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.util.Vec2;
import EDU.gatech.cc.is.abstractrobot.*;

import EDU.gatech.cc.is.communication.*;
import CSAI.unipa.communication.*;
import CSAI.unipa.abstractrobot.*;

/**
 * A Node that returns an array of Vec2 values. 
 * <P>
 * For detailed information on how to configure behaviors, see the
 * <A HREF="../clay/docs/index.html">Clay page</A>.
 * <P>
 * <A HREF="../COPYRIGHT.html">Copyright</A>
 * (c)1997, 1998 Tucker Balch
 *
 * @author Tucker Balch
 * @version $Revision: 1.1 $
 */


public class NodeMemory_Try_it extends NodeVec2Array
	{
            
        private VisualObjectSensor visual_robot;
        private SimpleInterface abstract_robot; 
        public static final boolean DEBUG = Node.DEBUG;
        private boolean control;
        private MultiForageN150ExploreSim multi_robot;

        static SharedMemory  sharedMemory= SharedMemory.getInstance();


        /** Creates a new instance of va_Memory_va */
        public NodeMemory_Try_it(boolean memory_control, int t1, int t2, int t3, SimpleInterface ar) {
           visual_robot = (VisualObjectSensor)ar;
           abstract_robot = ar;
           sharedMemory.setAttractorType1(t1);
           sharedMemory.setAttractorType2(t2);
           sharedMemory.setAttractorType3(t3);
           control = memory_control;
           multi_robot = (MultiForageN150ExploreSim)ar;
        }
    
    
        long	lasttime = 0;
        
        /**
	 * Return a Vec2Array that is the merge of two others.
	 * @param timestamp long, only get new information 
	 * 	if timestamp > than last call or timestamp == -1.
	 * @return the merged list.
	 */    
        public Vec2[] Value(long timestamp) 
        {
            if (DEBUG) System.out.println("va_Obstacles_r: Value()");
        try
        {
                return sharedMemory.value(visual_robot,multi_robot,timestamp,abstract_robot);
            }
            catch(NullPointerException e){System.out.println("Exception in NodeMemory_Try_it");}
            sharedMemory.setLength(0);
            return(new Vec2[0]);
        }
        /**
	 * Set the value of the node.
	 * If you implement a NodeVec2Array_cb, you need to define
	 * this method.
	 *
	 * @param value NodeVec2Array indicates the value to set 
         * @param timestamp long indicates time of the request
	 */
        public int getAttrType1(){return(sharedMemory.getAttractorType1());};
        
        /**
	 * Set the value of the node.
	 * If you implement a NodeVec2Array_cb, you need to define
	 * this method.
	 *
	 * @param value NodeVec2Array indicates the value to set 
         * @param timestamp long indicates time of the request
	 */
        public int getAttrType2(){return(sharedMemory.getAttractorType2());};
        
        /**
	 * Set the value of the node.
	 * If you implement a NodeVec2Array_cb, you need to define
	 * this method.
	 *
	 * @param value NodeVec2Array indicates the value to set 
         * @param timestamp long indicates time of the request
	 */
        public int getAttrType3(){return(sharedMemory.getAttractorType3());};
        
        
        ;
               
        /**
         * Set the value of the node.
        * If you implement a NodeVec2Array_cb, you need to define
        * this method.
         *
        * @param value NodeVec2Array indicates the value to set
        * @param timestamp long indicates time of the request
        */
        
        public void setValue(long timestamp) {
            Value(timestamp);
        }
        public int[] getVisual() {
            return(sharedMemory.getVisual());
        }
        

        

       public double getX(int item, long timestamp) {
            
            return(sharedMemory.getX(item));
        }        
        
        public double getY(int item, long timestamp) {
            
            return(sharedMemory.getY(item));
        }        
        
        public double getr(int item, long timestamp) {

            return(sharedMemory.getr(item));
        }
        
       	public int getLength(long timestamp) {
            
            return(sharedMemory.getLength());
        }     

          
    
        /**
         * Pop up a NodeVec2 from the embedded
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         * @param im1 NodeVec2 indicates the Vector to pop
         * @param timestamp long indicates time of request
         */

        public void pop(Vec2 im1, long timestamp) {
        try
        {
            sharedMemory.pop(im1,multi_robot.getPlayerNumber(timestamp));
        }
        catch(NullPointerException e){System.out.println("Exception in NodeMemory.Pop");}
 
   }

        public void pop_only(Vec2 im1, long timestamp) {
            try
            {
                sharedMemory.pop(im1,multi_robot.getPlayerNumber(timestamp));
            }
            catch(NullPointerException e){System.out.println("Exception in NodeMemory.Pop");}
        }
        /**
         * Pop up a NodeVec2 from the embedded
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         * @param im1 NodeVec2 indicates the Vector to pop
         * @param timestamp long indicates time of request
         */
    
        
        public boolean inMap(long timestamp, NodeVec2 im0)
        {
            return(false);
        }
        
        /**
         * Pop up a NodeVec2 from the embedded
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         *@param im1 NodeVec2 indicates the Vector to pop
         *@param timestamp long indicates time of request
         */
      
            public void push(Vec2 im1, int visual, long timestamp)
        {
           sharedMemory.push(im1,visual,multi_robot.getPlayerNumber(timestamp));
        }
		}
        
