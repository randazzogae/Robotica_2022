/*
 * v_Closest_va.java
 */


import java.lang.*;
import EDU.gatech.cc.is.util.Vec2;
import EDU.gatech.cc.is.util.Units;
import EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.abstractrobot.SimpleInterface;

/**
 * Finds the closest in a list of Vec2s.  Assumes the
 * vectors point egocentrically to the objects, so that
 * the closest one has the shortest r value.
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


public class v_Closest_var_Try_it extends NodeVec2
	{
	/**
	Turns debugging on or off.
	*/
	public static final boolean DEBUG = Node.DEBUG;
	private NodeMemory_Try_it	embedded1;
        private SimpleInterface abstract_robot;
        private Vec2            closestFlag = new Vec2(0,0);
        private boolean         filter = false;
        private Vec2[] Bin = new Vec2[3];
        private int             filterTime = 0;
        private int             time = 0;
        
        
	/**
	Instantiate a v_Closest_va node.
	@param im1 NodeVec2, the embedded node that generates a list
		of items to scan.
	*/
        
	public v_Closest_var_Try_it(NodeMemory_Try_it im1, SimpleInterface ar, NodeVec2 B1, NodeVec2 B2, NodeVec2 B3)
		{
		if (DEBUG) System.out.println("v_Closest_va: instantiated.");
		embedded1 = im1;
                abstract_robot = ar;
                Bin[0] = B1.Value(ar.getTime());
                Bin[1] = B2.Value(ar.getTime());
                Bin[2] = B3.Value(ar.getTime());
		}


	Vec2	last_val = new Vec2();
	long	lasttime = 0;

	/**
	Return a Vec2 representing the closest object, or 0,0 if
	none are visible.
	@param timestamp long, only get new information if timestamp > than last call
                or timestamp == -1.
	@return the vector.
	*/
	public Vec2 Value(long timestamp)
	{
	//	double	tempmag;

	//	if ((timestamp > lasttime)||(timestamp == -1))
	//	{
			/*--- reset the timestamp ---*/
		//	if (timestamp > 0)
	//				 lasttime = timestamp;
                                        
        	Vec2 position = abstract_robot.getPosition(timestamp);

			/*--- reset output ---*/
			last_val.setr(99999999);
                        
        	/*--- get the list of obstacles ---*/
			Vec2[] objs = embedded1.Value(timestamp);
                        
        	int[] visual = embedded1.getVisual();

			/*--- consider each obstacle ---*/
			double closest = 99999999;
                      //int k = 0;
			for(int i = 0; i<objs.length; i++)
            {
                                /*if(
                                  (!filter) 
                                  || 
                                  ((filter) 
                                  && 
                                    (Math.abs(closestFlag.x - objs[i].x) > 0.2) 
                                    && 
                                    (Math.abs(closestFlag.y - objs[i].y) > 0.2)
                                   )
                                  )
                                {*/
             	Vec2 flagToRobot = new Vec2(objs[i].x, objs[i].y);
                flagToRobot.sub(position);  
                Vec2 flagToBin = new Vec2(objs[i].x, objs[i].y);
                //System.out.println("inizio"+visual[1]);
                                
                if(visual[i]==embedded1.getAttrType1())
                	flagToBin.sub(Bin[0]); 
                else if(visual[i]==embedded1.getAttrType2())
                    flagToBin.sub(Bin[1]); 
                else if(visual[i]==embedded1.getAttrType3())
                    flagToBin.sub(Bin[2]); 

               //Vec2 distance = new Vec2(flagToRobot.x, flagToRobot.y);
                Vec2 distance = new Vec2();
                distance.setr(flagToRobot.r+flagToBin.r);
                if (distance.r <= closest)
                {
                	closest = distance.r;
                	last_val = objs[i];
                	//k = i;
                }
         // }
                                //else System.out.println("non considero la bandierina "+closestFlag.toString());
      }
       /*                 if(closest!=99999999)
                        //System.out.println("r :"+closest+"classe: "+visual[k]+" "+last_val.toString());
			if (DEBUG) System.out.println("v_Closest_va.Value: "+
				objs.length+" things to see "+
				"output "+
				last_val);*/

                
	return (new Vec2(last_val.x, last_val.y));
}
        
        /**
         * Set the value of the node.
         * If you implement a NodeVec2_cb, you need to define
         * this method.
         *
         * @param value NodeVec2 indicates the value to set
         * @param timestamp long indicates time of the request
         */
        public void setValue(NodeMemory_Try_it im1, long timestamp) {
            embedded1 = im1;
            Value(timestamp);
        }        
        
        /**
         * Return the length of the NodeVec2Array_cb
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         * @param timestamp long indicates time of request
         */
        public double getLength(long timestamp) {
            if (last_val.r == 0)
                return(0);
            else
                return(1);
        }
        
        /**
         * Return the x value of an item
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         * @param item int indicates the item of the array
         * @param timestamp long indicates time of request
         */
        public double getX(long timestamp) {
            return(last_val.x);
        }
        
        /**
         * Return the y value of an item
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         * @param item int indicates the item of the array
         * @param timestamp long indicates time of request
         */
        public double getY(long timestamp) {
            return(last_val.y);
        }
        
        /**
         * Return the r value of an item
         * If you implement a NodeVec2Array_cb, you need to define
         * this method
         *
         * @param item int indicates the item of the array
         * @param timestamp long indicates time of request
         */
        public double getr(long timestamp) {
            return(last_val.r);
        }
        
        public void filterFlag(Vec2 im0, int time) {
            filter = true;
            closestFlag = im0;
            filterTime = time;
            time = 0;
            //System.out.println("Setto "+closestFlag.toString());
        }      
        
        public void setFilter(boolean value){
            filter = value;
            if(!value)
            {
                filterTime = 0;
                time = 0;
            }
        }
        
}
