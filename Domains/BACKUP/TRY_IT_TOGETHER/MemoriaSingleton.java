

import java.io.*;

/**
 * COMMENTO
 *
 */

public class MemoriaSingleton {
	private static MemoriaSingleton memory;

	private MemoriaSingleton(){}

	public static MemoriaSingleton getInstance()
	{
		if(memory==null)
		{
			memory=new MemoriaSingleton();
		}
		return memory; 
	}
		
}