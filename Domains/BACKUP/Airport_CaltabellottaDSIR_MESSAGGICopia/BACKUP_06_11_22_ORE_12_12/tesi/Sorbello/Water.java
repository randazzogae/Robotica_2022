/*
 * Una bottiglietta d'acqua da inserire nei vari dsc
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

public class Water extends EDU.gatech.cc.is.simulation.AttractorSim
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
			g.setColor(Color.blue);
			g.fillRect(xpix-radius/2, ypix - (int)(radius*2/4), (int)(radius/1.5), radius);
			g.fillRect(xpix -(int)(radius/10*3), ypix - (int)(radius*1.4)+(int)(radius/2), (int)(radius/3.5), (int)(radius/2));
			}
		}
	}
