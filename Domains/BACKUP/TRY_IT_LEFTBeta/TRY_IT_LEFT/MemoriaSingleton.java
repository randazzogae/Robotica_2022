

import java.io.*;
import java.util.*;
import EDU.gatech.cc.is.util.Vec2;

/**
 * COMMENTO
 *
 */

public class MemoriaSingleton {
	private static MemoriaSingleton memory;
	private static ArrayList<Vec2> listaBandiere=null;

	private MemoriaSingleton(){
		listaBandiere=new ArrayList<Vec2>();
		}

	public static MemoriaSingleton getInstance()
	{
		if(memory==null)
		{
			memory=new MemoriaSingleton();
		}
		return memory; 
	}

	 public ArrayList<Vec2> getList() {
         return this.listaBandiere;
        }
	public boolean containFlags()
	{
		if (listaBandiere == null || listaBandiere.isEmpty()) {
			System.out.println("Memoria vuota");
			return false;
		}
		return true;
	}
	
	public boolean containFlag(Vec2 flag)
	{
		boolean exist=false;
		for (Vec2 v : listaBandiere)
		{
			if((Math.abs(flag.x - v.x)<=0.5)
					&& (Math.abs(flag.y - v.y)<=0.5))
			{
				exist = true;
				break;
			}
		}
		return exist;
	}

	public void addFlag(Vec2 flag)
	{
		if(!containFlag(flag))
		{
			System.out.println("flag:" + flag + " flagTOSTRING(): " + flag.toString());
			System.out.println("-----AGGIUNGO NELLA MEMORIA LA BANDIERA DI COORDINATE: " + flag.toString() + "---------");
			listaBandiere.add(flag);
			System.out.println("---------------------------------------------------------------------");
		}
		
	}

	public void removeFlag(Vec2 flag)
	{
	
		Iterator<Vec2> iterator = listaBandiere.iterator();

		while (iterator.hasNext())
		{
			System.out.println("STO CICLANDO PER ELIMINARE");
			if (iterator.next().toString().equals(flag.toString()) || containFlag(flag)) 
			{
				
				System.out.println("-----RIMUOVO DALLA MEMORIA LA BANDIERA DI COORDINATE: " + flag.toString() + "---------");
				iterator.remove();
				System.out.println("---------------------------------------------------------------------");
				showList();
				break;
			}
		}

	}

	public void showList()
	{
		System.out.println("---------------------------------------------------------------------");

		System.out.println("Coordinate bandiere in memoria:");

		for (Vec2 flag : listaBandiere) 
		{ 
			System.out.println(flag.toString());
		} 

		System.out.println("---------------------------------------------------------------------");
	}
		
}