/*
 * Un fiore da inserire nei vari dsc
 */

package CSAI.unipa.tesi.Sorbello;

import java.awt.*;
import java.util.Random;
import EDU.gatech.cc.is.util.Vec2;
import EDU.gatech.cc.is.util.Units;
import EDU.gatech.cc.is.communication.Message;


/**
 *Scritto da Caltabellotta Marco, Foti Carmelo, Martino Gioacchino, Sansone di Campobianco Corrado
 */

public class Flower extends EDU.gatech.cc.is.simulation.AttractorSim
	{
	/**
	 * Draw the attractor.
	 */
		public void draw(Graphics g, int w, int h,
		double t, double b, double l, double r)
		{
		top =t; bottom =b; left =l; right = r;
		
		if(picked_up != true)
			{
			double meterspp = (r - l) / (double)w;
			if (DEBUG) System.out.println("meterspp "+meterspp);
			int radius = (int)(RADIUS / meterspp);
			int xpix = (int)((position.x - l) / meterspp);
			int ypix = (int)((double)h - ((position.y - b) / meterspp));
			if (DEBUG) System.out.println("robot at"+
				" at "+xpix+","+ypix);
	
			/*--- draw the main body ---*/
			g.setColor(Color.green);
			g.fillOval(xpix- radius/2, ypix , (radius/2), (radius/2));
			g.fillOval(xpix, ypix + radius/3 , (radius/2), (radius/2));
			g.fillRect(xpix, ypix - radius, (radius/8), radius*3 -(radius/2));
			g.setColor(Color.yellow);
			g.fillOval(xpix - (radius/4), ypix - (radius) -(radius/4), (radius/2)-1,(radius/2)-1);
			g.setColor(Color.red);
			
			g.fillOval(xpix, ypix - radius,	(radius/2), (radius/2));
			g.fillOval(xpix, ypix - (radius + radius/2), (radius/2), (radius/2));
			g.fillOval(xpix- radius/2, ypix - radius, (radius/2), (radius/2));
			g.fillOval(xpix- radius/2, ypix - (radius + radius/2), (radius/2), (radius/2));
			


			}
		}
	}
