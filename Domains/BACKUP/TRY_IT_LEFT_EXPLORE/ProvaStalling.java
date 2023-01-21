

import java.io.*;


public class ProvaStalling {

	public static void main(String args[])
	{
		Stalling antistallo = new Stalling();

		for(;;)
		{
			antistallo.update();
			System.out.println(antistallo.getTime());
		}

	}
		
}