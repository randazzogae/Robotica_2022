/*
 * d_ChooseZone_m.java
 *
 * Created on 9 dicembre 2002, 17.10
 */


import EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.util.Vec2;

import CSAI.unipa.knowledgment.NodeMap;
import CSAI.unipa.util.mapAttributes;

/**
 *
 * @author  Marco Di Stefano
 */
 
// Abbiamo aggiunto un metodo per settare il vettore priority 
public class v_ChooseZone_mv_Try_it extends NodeVec2 {
    
    public static final boolean DEBUG = Node.DEBUG;
    private int[]               priority = new int[4];
    private NodeMap             map;
    private NodeVec2            embedded1;
    
    /** Creates a new instance of d_ChooseZone_m */
    public v_ChooseZone_mv_Try_it(NodeMap m, NodeVec2 pos, int f, int s, int t, int q) {
        priority[0] = f;
        priority[1] = s;
        priority[2] = t;
        priority[3] = q;
        map = m;
        embedded1 = pos;
    }
    
    Vec2	last_val = new Vec2();
    long	lasttime = 0;
    
    /**
     * Provides the value of the node.
     * If you implement a NodeDouble, you need to define
     * this method.
     *
     * @param timestamp long indicates time of the request
     * @return the value
     */
    public Vec2 Value(long timestamp) {
        if (DEBUG) System.out.println("d_ChooseZone_mv: Value()");
 
                if ((timestamp > lasttime)||(timestamp == -1))
                        {
                        /*--- reset the timestamp ---*/
                        if (timestamp > 0) lasttime = timestamp;
                        
                        Vec2 position = embedded1.Value(timestamp);
                        
                        int column = map.getCellColumn(position.x);
                        int row = map.getCellRow(position.y);
                        
                        
                        for(int k=0; k<4;k++){
                                if((priority[k]==mapAttributes.NORD) &&
                                    (map.isFree(column, row-1))){
                                    last_val = new Vec2(position.x, 10000);
                                    priority[0] = mapAttributes.NORD;
                                    priority[1] = mapAttributes.EST;
                                    priority[2] = mapAttributes.SUD;
                                    priority[3] = mapAttributes.OVEST;
                                    row = row -1;
                                    break;
                                } else
                                if((priority[k]==mapAttributes.SUD) &&
                                    (map.isFree(column, row+1))){
                                    last_val = new Vec2(position.x, -10000);
                                    row = row +1;
                                    priority[0] = mapAttributes.SUD;
                                    priority[1] = mapAttributes.OVEST;
                                    priority[2] = mapAttributes.NORD;
                                    priority[3] = mapAttributes.EST;
                                    break;
                                } else
                                if((priority[k]==mapAttributes.OVEST) &&
                                    (map.isFree(column-1, row))){
                                    last_val = new Vec2(-10000, position.y);
                                    column=column-1;
                                    priority[0] = mapAttributes.OVEST;
                                    priority[1] = mapAttributes.NORD;
                                    priority[2] = mapAttributes.EST;
                                    priority[3] = mapAttributes.SUD;
                                    break;
                                } else
                                if((priority[k]==mapAttributes.EST) &&
                                    (map.isFree(column+1, row))){
                                    column= column+1;    
                                    last_val = new Vec2(10000, position.y);
                                    priority[0] = mapAttributes.EST;
                                    priority[1] = mapAttributes.SUD;
                                    priority[2] = mapAttributes.OVEST;
                                    priority[3] = mapAttributes.NORD;
                                    break;
                                }
                        }
                        //last_val = new Vec2(map.getXCenter(column),map.getYCenter(row));
                }      
        return(last_val);
    }
    
    public void setPriority(int f,int s,int t,int q){
    	priority[0] = f;
        priority[1] = s;
        priority[2] = t;
        priority[3] = q;
    }
}
                                    
                                    
                                    
                                    
                                    
      