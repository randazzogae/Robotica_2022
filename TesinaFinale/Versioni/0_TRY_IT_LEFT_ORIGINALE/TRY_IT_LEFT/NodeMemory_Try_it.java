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
        private int attractorType1, attractorType2, attractorType3;  
        public static final boolean DEBUG = Node.DEBUG;
        private boolean control;
        private MultiForageN150ExploreSim multi_robot;
        private int length=0;
        
    
        /** Creates a new instance of va_Memory_va */
        public NodeMemory_Try_it(boolean memory_control, int t1, int t2, int t3, SimpleInterface ar) {
           visual_robot = (VisualObjectSensor)ar;
           abstract_robot = ar;
           attractorType1 = t1;
           attractorType2 = t2;
           attractorType3 = t3;
           control = memory_control;
           multi_robot = (MultiForageN150ExploreSim)ar;
        }
    
    
        long	lasttime = 0;
        Vec2[]  last_val = new Vec2[0];
        int[]   visual_number = new int[0];
        
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
            //The vector with all the obstacle seen by robot
            Vec2[] im0_1 = visual_robot.getVisualObjects(timestamp, attractorType1);
            Vec2[] im0_2 = visual_robot.getVisualObjects(timestamp, attractorType2);
            Vec2[] im0_3 = visual_robot.getVisualObjects(timestamp, attractorType3);
            
            //the array with the visual number of the new flags
            int[] visual_new = new int[im0_1.length+im0_2.length+im0_3.length];
        
            //the array of the new flags merging the three
            Vec2[] im0 = new Vec2[im0_1.length+im0_2.length+im0_3.length];
            int count = 0;
            for(int i=0; i<im0_1.length; i++)
            {
                im0[count] = new Vec2(im0_1[i].x, im0_1[i].y);
                visual_new[count++] = attractorType1;
            }
            for(int i=0; i<im0_2.length; i++)
            {
                im0[count] = new Vec2(im0_2[i].x, im0_2[i].y);
                visual_new[count++] = attractorType2;
            }
            for(int i=0; i<im0_3.length; i++)
            {    
                im0[count] = new Vec2(im0_3[i].x, im0_3[i].y);
                visual_new[count++] = attractorType3;           
            }
            
            
            //the position of the robot
            Vec2 position = abstract_robot.getPosition(timestamp);
            
      //      if(((timestamp > lasttime)||(timestamp == -1)) && im0.length!=0)
      //      {
                /*--- reset the timestamp ---*/
        //        if (timestamp > 0) lasttime = timestamp;
            if(im0.length !=0)
            {
                //make it global
                for(int i=0; i<im0.length; i++)
                    im0[i].add(position);
                        
                //im1 is global and is rounded
                Vec2[] im1 = roundVector(im0);
                //im2 is global
                Vec2[] im2 = last_val;     
                
                int[] visual_old = visual_number;
            
                //is a temp vector array in which save only new obstacles and after merge with old
                int k = im1.length;
            
                //Initialization of im3, is 1 if is a new obstacle, -1 if is old
                int[] im3 = new int[im1.length];
                for(int i=0; i<im3.length; i++)
                    im3[i] = 1;
            
            
                //control the objects and decide what are to be saved
                for(int i=0; i<im1.length; i++)
                {
                    for(int j=0; j<im2.length; j++)
                    { 
                        //if condition certainly is a flag that has been moved by robot
                        if((Math.abs(im1[i].x-im2[j].x)<=0.5) && 
                           (Math.abs(im1[i].y-im2[j].y)<=0.5))
                        {                   
                            //-1 because i must not insert it as a new flag
                            im3[i] = -1;
                            //substitute the old flag with this that's the same moved
                            im2[j] = new Vec2(im1[i].x, im1[i].y);
                            k--;
                            break;
                        }
                        //NON SERVE PIù PERCHè HO AGGIUNTO LA CONDIZIONE VEDI SE C'è LA BANDIERINA
                        /*else
                        if((Math.abs(im1[i].x-im2[j].x)<noise_factor+0.2) && 
                           (Math.abs(im1[i].y-im2[j].y)<noise_factor+0.2))
                        {
                            //set to -1 the obstacles that are old
                            im3[i] = new Vec2(-1,0);
                            //im2[j] = new Vec2(im1[i].x, im1[i].y);
                            k--;
                            break;
                        }*/
                    }
                }
                
                //create the vector for the flag and the visual tpe
                last_val= new Vec2[im2.length+k];
                visual_number = new int[last_val.length];
                
                //merge the vector arrays of new and old flags
                int pos = 0;
            
                //this flag says if has been modified the vector array
                boolean flag = false;
                
                //before insert all new obstacles
                for(int i=0; i<im3.length; i++)
                {
                    if(im3[i]==1)
                    {
                        last_val[pos] = new Vec2(im1[i].x, im1[i].y);
                        visual_number[pos] = visual_new[i];
                        flag = true;      
                        
                        try
                        {
                            //Message-------------new flag
                            LongMessage idMessage = new LongMessage();
                            idMessage.val = multi_robot.getPlayerNumber(timestamp);
                            int id = 0;
                            if(idMessage.val == 0)
                                id = 1;
                            if(idMessage.val == 1)
                                id = 0;
                            if(idMessage.val == 2)
                                id = 3;
                            if(idMessage.val == 3)
                                id = 2;
                            multi_robot.unicast(id, idMessage);
            
                            StringMessage message = new StringMessage();
                            message.val = "Bandiera nuova";
                            multi_robot.unicast(id, message);
            
                            PointMessage flagPosition = new PointMessage(last_val[pos].x, last_val[pos].y);
                            multi_robot.unicast(id, flagPosition);
                        
                            LongMessage visualMessage = new LongMessage();
                            visualMessage.val = visual_number[pos];
                            multi_robot.unicast(id, visualMessage);
                        }
                        catch(CommunicationException e){}
                        pos++;
                    }
                }
                
                //and after insert the old obstacles
                for(int i=k; i<last_val.length; i++){
                    last_val[i] = new Vec2(im2[i-k].x, im2[i-k].y);
                    visual_number[i] = visual_old[i-k];
                }
                
                if (flag && control)
                {
                    System.out.println("******************Memoria ROBOT "+multi_robot.getPlayerNumber(timestamp)+" aggiornata******************");
                    for(int i=0; i<last_val.length; i++)
                    {
                        System.out.println(visual_number[i] +" "+last_val[i].toString());
                    }
                    System.out.println();
               	}
      	    }
                length=last_val.length;            
                Vec2[] ret_val = new Vec2[last_val.length];
            
                for(int i = 0; i<ret_val.length; i++)
                            ret_val[i] = new Vec2(last_val[i].x, last_val[i].y);
                return(ret_val);
                
            }
            catch(NullPointerException e){System.out.println("Exception in NodeMemory_Try_it");}
            length=0;
            return(new Vec2[0]);
        }
        
        ;
        
        ;
        
        ;
        
        ;    
        
        /**
	 * Set the value of the node.
	 * If you implement a NodeVec2Array_cb, you need to define
	 * this method.
	 *
	 * @param value NodeVec2Array indicates the value to set 
         * @param timestamp long indicates time of the request
	 */
        public int getAttrType1(){return(attractorType1);};
        
        /**
	 * Set the value of the node.
	 * If you implement a NodeVec2Array_cb, you need to define
	 * this method.
	 *
	 * @param value NodeVec2Array indicates the value to set 
         * @param timestamp long indicates time of the request
	 */
        public int getAttrType2(){return(attractorType2);};
        
        /**
	 * Set the value of the node.
	 * If you implement a NodeVec2Array_cb, you need to define
	 * this method.
	 *
	 * @param value NodeVec2Array indicates the value to set 
         * @param timestamp long indicates time of the request
	 */
        public int getAttrType3(){return(attractorType3);};
        
        
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
        
        

        public double getX(int item, long timestamp) {
            
            return(last_val[item].x);
        }        
        
        public double getY(int item, long timestamp) {
            
            return(last_val[item].y);
        }        
        
        public double getr(int item, long timestamp) {
            
            return(last_val[item].r);
        }     
        
 /*       public double getLength(long timestamp) {
            
            return(last_val.length);
        }  */
        
       	public int getLength(long timestamp) {
            
            return(length);
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
            
            boolean flag=false;
        try
        {
        	 //Message-------------new flag
             LongMessage idMessage = new LongMessage();
             idMessage.val = multi_robot.getPlayerNumber(timestamp);
             int id = 0;
             if(idMessage.val == 0)
             	id = 1;
             if(idMessage.val == 1)
                id = 0;
             if(idMessage.val == 2)
                id = 3;
             if(idMessage.val == 3)
                id = 2;
             multi_robot.unicast(id, idMessage);
            
             StringMessage message = new StringMessage();
             message.val = "Delete Flag";
             multi_robot.unicast(id, message);
            
             PointMessage flagPosition = new PointMessage(im1.x, im1.y);
             multi_robot.unicast(id, flagPosition);                         
          }
           catch(CommunicationException e){}
            
        try
        {
        	int count=0;
            for(int i=0; i<last_val.length; i++)
                if((Math.abs(im1.x - last_val[i].x)<=0.5)    //modificato!!!!!era 0.3
                && (Math.abs(im1.y - last_val[i].y)<=0.5))
                {
                	count++;
                    flag = true;
                   
                }
            if (flag)
            {
                int pos = 0;
                Vec2[] temp = new Vec2[last_val.length-count];
                for(int i=0; i<last_val.length; i++)
                    if((Math.abs(im1.x - last_val[i].x)>0.5) || 
                       (Math.abs(im1.y - last_val[i].y)>0.5))
                    {
                        temp[pos++] = new Vec2(last_val[i].x, last_val[i].y);
                    }          
                    //else
                    //    System.out.println("Tolta bandierina "+last_val[i].toString());
                last_val = temp;
                
                
                if(control)
                {
                    System.out.println("******************Memoria ROBOT "+multi_robot.getPlayerNumber(timestamp)+" decrementata******************");    
                    for(int i=0; i<last_val.length; i++)
                        System.out.println(last_val[i].toString());
                System.out.println("Estratta bandierina "+im1.toString()+"\n");    
                }
            }
        }
        catch(NullPointerException e){System.out.println("Exception in NodeMemory.Pop");}
 
   }
   
   
   public void pop_only(Vec2 im1, long timestamp) {
            
        boolean flag=false;
                 
        try
        {
        	int count=0;
            for(int i=0; i<last_val.length; i++)
                if((Math.abs(im1.x - last_val[i].x)<=0.5)    //modificato!!!!!era 0.3 modificato
                && (Math.abs(im1.y - last_val[i].y)<=0.5))   //anche  in pop().
                {
                	count++;
                    flag = true;
                }
            if (flag)
            {
                int pos = 0;
                Vec2[] temp = new Vec2[last_val.length-count];
                for(int i=0; i<last_val.length; i++)
                    if((Math.abs(im1.x - last_val[i].x)>0.5) || 
                       (Math.abs(im1.y - last_val[i].y)>0.5))
                    {
                        temp[pos++] = new Vec2(last_val[i].x, last_val[i].y);
                    }          
                    //else
                    //    System.out.println("Tolta bandierina "+last_val[i].toString());
                last_val = temp;
                
                
                if(control)
                {
                    System.out.println("******************Memoria ROBOT "+multi_robot.getPlayerNumber(timestamp)+" decrementata******************");    
                    for(int i=0; i<last_val.length; i++)
                        System.out.println(last_val[i].toString());
                System.out.println("Estratta bandierina "+im1.toString()+"\n");    
                }
            }
        }
        catch(NullPointerException e){System.out.println("Exception in NodeMemory.pop_only");}
 
   }                      
        
        public double roundDecimal(double val) {
            
            double temp = val * 10;
            long round = Math.round(temp);
            temp = (double)round / 10;
            //System.out.println(val+" "+round+" "+temp);
            return (temp);
            
        }
        
        public Vec2[] roundVector(Vec2[] im1) {
            
            Vec2[] ret_val = new Vec2[im1.length];
            
            for(int i=0; i<ret_val.length; i++)
                ret_val[i] = new Vec2(roundDecimal(im1[i].x), roundDecimal(im1[i].y));
            
            return(ret_val);
        }
        
        
        /**
         * Pop up a NodeVec2 from the embedded
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         * @param im1 NodeVec2 indicates the Vector to pop
         * @param timestamp long indicates time of request
         */
        public int[] getVisual() {
            
            int[] ret_val = new int[visual_number.length];
            for(int i=0; i<ret_val.length; i++)
                ret_val[i] = visual_number[i];
            return(ret_val);            
            
        }
        
        public boolean inMap(long timestamp, NodeVec2 im0){
                                     
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
            boolean canPush=true;
            
            try
            {
                for(int i=0; i<last_val.length; i++)
                    if((Math.abs(im1.x - last_val[i].x)<=0.5)
                    && (Math.abs(im1.y - last_val[i].y)<=0.5))
                    {
                        canPush = false;
                    }
                if (canPush)
                {
                    //create the vector for the flag and the visual tpe
                    Vec2[] temp= new Vec2[last_val.length+1];
                    int[] temp_visual = new int[last_val.length+1];
                
                    int pos=0;
                    for(int i=0; i<last_val.length; i++)
                    {
                        temp[i] = new Vec2(last_val[i].x, last_val[i].y);
                        temp_visual[i] = visual_number[i];
                        pos++;
                    } 
                    temp[pos] = new Vec2(im1);
                    temp_visual[pos] = visual;      
                
                    last_val = temp;//new Vec2[temp.length];
                    visual_number = temp_visual;//new int[temp_visual.length];
            
                    for(int i = 0; i<last_val.length; i++)
                    {
                        last_val[i] = new Vec2(temp[i]);
                        visual_number[i] = temp_visual[i];
                        //System.out.println("temp "+last_val[i].toString());
                    }

                }
                if(control)
                {
                    System.out.println("******************Memoria ROBOT "+multi_robot.getPlayerNumber(timestamp)+" incrementata******************");    
                    for(int i=0; i<last_val.length; i++)
                        System.out.println(last_val[i].toString());
                }
            
            }
            catch(NullPointerException e){}
        }
        }
