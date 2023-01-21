

import java.io.*;

/**
 * Controlla se il robot si trova in situazione di stallo basandosi sul tempo di sistema
 */

public class Stalling {


private long time;

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
		
}