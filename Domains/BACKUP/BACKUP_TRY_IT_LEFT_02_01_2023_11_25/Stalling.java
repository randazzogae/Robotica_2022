

import java.io.*;

/**
 * Controlla se il robot si trova in situazione di stallo basandosi sul tempo di sistema
 */

public class Stalling {

	private int i=0; //timer per lo stallo

	private int maxTime; //con tale variabile, settiamo il tempo massimo consentito nell'eseguire una determinata azione o nel trovarsi in un determinato stato
						
	//--------VECCHIA IMPLEMENTAZIONE:
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
	
	//-------FINE VECCHIA IMPLEMENTAZIONE

	public void setMaxTime(int maxTime) //AD OGNI TRANSIZIONE DI STATO O AD OGNI AZIONE, SETTIAMO UN DIVERSO TEMPO MASSIMO
	{
		this.maxTime=maxTime;
	}

	public int getMaxTime() //SERVE PER FARE LA VERIFICA MEDIANTE IF 
	{
		return maxTime;
	}

	public void addI() //incrementa il timer
	{
		i=i+1;
	}

	public int getI() //ottieni il timer
	{
		return i;
	}

	public void resetI() //resetta il timer (AD OGNI TRANSIZIONE DI STATO OPPURE AD OGNI CAMBIO DI AZIONE)
	{
		i=0;
	}
		
}