

import java.io.*;

/**
 * Controlla se il robot si trova in situazione di stallo basandosi sul tempo di sistema
 */

public class Stalling {

	private long time;
	private int i=0; //timer per lo stallo

	public Stalling(){	}
		
	public void update(){
		time=System.currentTimeMillis();
		}
		
		
	public boolean isStall(double cut){
			return ((System.currentTimeMillis()-time)>cut);
		}
		
	public long getTime(){
		return time;
		}

	public void addI() //incrementa il timer
	{
		i=i+1;
	}

	public int getI() //ottieni il timer
	{
		return i;
	}

	public void resetI() //resetta il timer
	{
		i=0;
	}
		
}