/*
 * MapCanvas.java
 *
 * Created on 6 dicembre 2002, 18.36
 */

package CSAI.unipa.graphics;

import javax.swing.JFrame;
import java.awt.*;
import java.awt.*;

import EDU.gatech.cc.is.abstractrobot.SimpleInterface;

import CSAI.unipa.util.mapAttributes;
import CSAI.unipa.knowledgment.NodeGraphCell;
import CSAI.unipa.knowledgment.NodeMap;

/**
 *
 * @author  Marco Di Stefano
 */
public class MapCanvas extends Canvas {
    
    private Graphics        g;
    private JFrame          parent;
    private int             canvasWidth;
    private int             canvasHeight;
    private int             column;
    private int             row;
    private double          cellDimension;
    private NodeGraphCell[][] map;
    private int[]           robotPosition = new int[2];
    private boolean         drawGrid = true;
    
    //////////////////////
   private	Image buffer;
//	private frame 
    private	Graphics bufferg;
    /** Creates a new instance of MapCanvas */
    public MapCanvas(JFrame p, int w, int h, int c, int r, double dim, int[] rob, NodeGraphCell[][] m) {
        parent = p;
        canvasWidth = w;
        canvasHeight = h;
        this.setSize(canvasWidth, canvasHeight);
        column = c;
        row = r;
        map = m;
        cellDimension = dim;
        robotPosition[0] = rob[0];
        robotPosition[1] = rob[1];
        this.setBackground(Color.white);
        
        
    }
    
    public void resetGraphics() {
	Frame frame = new Frame();
        frame.addNotify();
	buffer = frame.createImage(canvasWidth,canvasHeight);
        
//        buffer = createImage(canvasWidth,canvasHeight);
	if (buffer==null) System.out.println("il maledetto buffer non esiste");
        bufferg = buffer.getGraphics();
        bufferg.setColor(Color.white);
    }
    
    public synchronized void update(Graphics g){
        bufferg.setColor(Color.white);
        bufferg.fillRect(0,0,canvasWidth, canvasHeight);
        
        bufferg.setColor(Color.gray);
        /*-- draw the grid --*/
        if(drawGrid)
        for(int i=0; i<=column; i++){
            bufferg.drawLine((int)(i*cellDimension*mapAttributes.CANVAS_DIMENSION), 0, (int)(i*cellDimension*mapAttributes.CANVAS_DIMENSION), (int)(row*cellDimension*mapAttributes.CANVAS_DIMENSION));
        }
        if(drawGrid)
        for(int i=0; i<=row; i++){
            bufferg.drawLine(0, (int)(i*cellDimension*mapAttributes.CANVAS_DIMENSION), (int)(column*cellDimension*mapAttributes.CANVAS_DIMENSION), (int)(i*cellDimension*mapAttributes.CANVAS_DIMENSION));
        }
        
        /*-- draw the visited or occupied cell --*/
        for(int i=0; i<column; i++)
            for(int j=0; j<row; j++){
                if(map[i][j].getState()==mapAttributes.VISITED){
                    bufferg.setColor(Color.red);
                    bufferg.drawLine((int)(i*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)((j*cellDimension+cellDimension/2)*mapAttributes.CANVAS_DIMENSION), 
                               (int)((i+1)*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)((j*cellDimension+cellDimension/2)*mapAttributes.CANVAS_DIMENSION));
                }else
                if(map[i][j].getOccupied()>0){
                    float intensity = 1-(float)(map[i][j].getOccupied());
                    Color scale = new Color(0,intensity,intensity);
                    bufferg.setColor(scale);
                    if(map[i][j].getState()==mapAttributes.OCCUPIED)
                        g.setColor(Color.black);
                    
                    bufferg.drawLine((int)(i*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)(j*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)((i+1)*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)((j+1)*cellDimension*mapAttributes.CANVAS_DIMENSION));
                    bufferg.drawLine((int)((i+1)*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)(j*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)(i*cellDimension*mapAttributes.CANVAS_DIMENSION), 
                               (int)((j+1)*cellDimension*mapAttributes.CANVAS_DIMENSION));
                }
            }
        
        /*-- draw the robot --*/
        bufferg.setColor(Color.blue);
        bufferg.drawOval((int)((robotPosition[0]*cellDimension+cellDimension/2)*mapAttributes.CANVAS_DIMENSION),
                   (int)((robotPosition[1]*cellDimension+cellDimension/2)*mapAttributes.CANVAS_DIMENSION),
                   (int)(cellDimension/2*mapAttributes.CANVAS_DIMENSION),
                   (int)(cellDimension/2*mapAttributes.CANVAS_DIMENSION));
        
        g.drawImage(buffer, 0, 0, this);
    }
    
    
    public void setPosition(int[] pos){
        robotPosition[0] = pos[0];
        robotPosition[1] = pos[1];
    }
    

}
