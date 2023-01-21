/*
 *
 *  Location.java
 *
 **/




/*
 * Scompone la mappa in zone di differente campo potenziale settando l'angolo teta
 * della classe v_LinearAttraction_v_Try_it.
 * Il robot,quindi,il soggetto ad un campo potenziale differente a secondo della zona
 * in cui si trova e dell'ostacolo che si prepara ad affrontare per arrivare al goal. *
 *
 **/




public class Location {
	
	//Zone in cui si può trovare il robot		    
	    private boolean zone_A;  
		private boolean zone_B;  
		private boolean zone_C;  
		private boolean zone_D;  
		private boolean zone_E;  
		private boolean zone_F;  		
		private boolean zone_G;  
		private boolean zone_H;  
		private boolean zone_I;  
		private boolean zone_L;  
		private boolean zone_M;  
		private boolean zone_N;  
		private boolean zone_O;  
		private boolean zone_P;  

     //	Zone in cui si possono trovare le bandierine	
		private boolean flag_1;  
		private boolean flag_2;
		private boolean flag_3;
		private boolean flag_4;
		private boolean flag_5;
		private boolean flag_6;
		private boolean flag_7;
		private boolean flag_8;
		private boolean flag_9;
		private boolean flag_10;
				
		private boolean any_zone_flag_sx,any_zone_flag_dx; //Indicano se le bandierine si trovano in qualche zona di quelle precedentemente dichiarate

		//------------------INIZIO MODIFICHE DSIR-----------------------------------------

		private boolean cella[]=new boolean[70]; //se la iesima cella e' false vuol dire che non è stata visitata
		
		private double x_rob, y_rob;

		//------------------FINE MODIFICHE DSIR-----------------------------------------

	
	
	public Location(){ }    //costruttore

	public void setPositionRobot(double x_rob, double y_rob)
	{
		setX(x_rob);
		setY(y_rob);
		chiudiCella(); //chiude la cella che si trova in quella posizione
		//BISOGNA FARE IL CHECK DI TUTTE LE CELLE QUI
	}

	public void setX(double x_rob)
	{
		this.x_rob=x_rob;
	}

	public void setY(double y_rob)
	{
		this.y_rob=y_rob;
	}

	public void debugZoneDSIR() //per testare le zone
	{
		if(isClosedZona1())
		{
			System.out.println("------------ZONA 1 CHIUSA!!!!!!!!!!!");
		}
	}

	public void debugDSIR() //per testare la posizione delle celle
	{
		if(amInCella1())
		{
			System.out.println("SONO NELLA CELLA 1 E LA CHIUDO");
		}
		else if(amInCella2())
		{
			System.out.println("SONO NELLA CELLA 2 E LA CHIUDO");
		}
		else if(amInCella3())
		{
			System.out.println("SONO NELLA CELLA 3 E LA CHIUDO");
		}
		else if(amInCella4())
		{
			System.out.println("SONO NELLA CELLA 4 E LA CHIUDO");
		}
		else if(amInCella5())
		{
			System.out.println("SONO NELLA CELLA 5 E LA CHIUDO");
		}
		else if(amInCella6())
		{
			System.out.println("SONO NELLA CELLA 6 E LA CHIUDO");
		}
		else if(amInCella7())
		{
			System.out.println("SONO NELLA CELLA 7 E LA CHIUDO");
		}
		else if(amInCella8())
		{
			System.out.println("SONO NELLA CELLA 8 E LA CHIUDO");
		}
		else if(amInCella9())
		{
			System.out.println("SONO NELLA CELLA 9 E LA CHIUDO");
		}
		else if(amInCella10())
		{
			System.out.println("SONO NELLA CELLA 10 E LA CHIUDO");
		}
		else if(amInCella11())
		{
			System.out.println("SONO NELLA CELLA 11 E LA CHIUDO");
		}
		else if(amInCella12())
		{
			System.out.println("SONO NELLA CELLA 12 E LA CHIUDO");
		}
		else if(amInCella13())
		{
			System.out.println("SONO NELLA CELLA 13 E LA CHIUDO");
		}
		else if(amInCella14())
		{
			System.out.println("SONO NELLA CELLA 14 E LA CHIUDO");
		}
		else if(amInCella15())
		{
			System.out.println("SONO NELLA CELLA 15 E LA CHIUDO");
		}
		else if(amInCella16())
		{
			System.out.println("SONO NELLA CELLA 16 E LA CHIUDO");
		}
		/*
		else if(amInCella17())
		{
			System.out.println("SONO NELLA CELLA 17 E LA CHIUDO");
		}
		else if(amInCella18())
		{
			System.out.println("SONO NELLA CELLA 18 E LA CHIUDO");
		}
		else if(amInCella19())
		{
			System.out.println("SONO NELLA CELLA 19 E LA CHIUDO");
		}
		else if(amInCella20())
		{
			System.out.println("SONO NELLA CELLA 20 E LA CHIUDO");
		}

		*/
	}
	
	

	//--------------------------METODI PER VERIFICARE SE UNA ZONA E' CHIUSA O MENO -------------------------------------
	//IMPLEMENTARE I METODI ISCLOSEDZONA DA 2 A 4 SEGUENDO LA STESSA LOGICA
	public boolean isClosedZona1()
	{
		for(int i=0;i<=15;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
		{
			if(cella[i]==false) //cella[i] è non visitata???
			{
				return false; //se esiste almeno una cella non visitata, la zona 1 non è chiusa
			}
		}
		return true; 
	}

	//versione che ritorna il punto attrattore della prima cella non visitata della zona 1
	public double[] isClosedZona1Point()
	{	
		double point[]=new double[2]; // point[0] sarà la coordinata x e point[1] sarà la coordinata y
		for(int i=0;i<=15;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
		{
			if(cella[i]==false) //cella[i] e' non visitata??
			{
				point=getPuntoCella(i);
				return point;
				
			}
		}
		//se tutte le celle di quella zona sono state visitate, restituisco un punto che serve per farlo muovere un po'
		point[0]=x_rob+1;
		point[1]=y_rob+1;
		return point; 
	}
	

	//-----------------------------------------------------------------------------------------------------------------

	

	//--------------------------METODI PER LOCALIZZARE IL ROBOT -------------------------------------
	 //IMPLEMENTARE I METODI AMINCELLA DA 21 A 70 SEGUENDO LA STESSA LOGICA

	public boolean amInCella20()
	{
		boolean temp = x_rob>=11 && x_rob<=13 && y_rob<20 && y_rob>=18;
		return temp;   
	}

	public boolean amInCella19()
	{
		boolean temp = x_rob>16 && x_rob<=19 && y_rob<22 && y_rob>=20;
		return temp;   
	}

	public boolean amInCella18()
	{
		boolean temp =  x_rob>13 && x_rob<=16 && y_rob<22 && y_rob>=20;
		return temp;   
	}

	public boolean amInCella17()
	{
		boolean temp = x_rob>=11 && x_rob<=13 && y_rob<22 && y_rob>=20;
		return temp;   
	}

	public boolean amInCella16()
	{
		boolean temp = x_rob>31 && x_rob<=33 && y_rob<26 && y_rob>=24; 
		return temp;   
	}
	public boolean amInCella15()
	{
		boolean temp = x_rob>28 && x_rob<=31 && y_rob<26 && y_rob>=24;  
		return temp;  
	}
	public boolean amInCella14()
	{
		boolean temp = x_rob>25 && x_rob<=28 && y_rob<26 && y_rob>=24; 
		return temp; 
	}
	public boolean amInCella13()
	{
		boolean temp = x_rob>22 && x_rob<=25 && y_rob<26 && y_rob>=24; 
		return temp;  
	}
	public boolean amInCella12()
	{
		boolean temp = x_rob>19 && x_rob<=22 && y_rob<26 && y_rob>=24; 
		return temp;  
	}
	public boolean amInCella11()
	{
		boolean temp = x_rob>16 && x_rob<=19 && y_rob<26 && y_rob>=24;
		return temp; 
	}
	public boolean amInCella10()
	{
		boolean temp = x_rob>13 && x_rob<=16 && y_rob<26 && y_rob>=24;
		return temp;
	}

	public boolean amInCella9()
	{
		boolean temp = x_rob>=11 && x_rob<=13 && y_rob<26 && y_rob>=24; 
		return temp;
	}


	public boolean amInCella8()
	{
		boolean temp = x_rob>31 && x_rob<=33 && y_rob<28 && y_rob>=26;
		return temp;   
	}

	public boolean amInCella7()
	{
		boolean temp = x_rob>28 && x_rob<=31 && y_rob<28 && y_rob>=26; 
		return temp;  
	}

	public boolean amInCella6()
	{
		boolean temp = x_rob>25 && x_rob<=28 && y_rob<28 && y_rob>=26;
		return temp;
	}

	public boolean amInCella5()
	{
		boolean temp = x_rob>22 && x_rob<=25 && y_rob<28 && y_rob>=26;
		return temp;    
	}

	public boolean amInCella4()
	{
		boolean temp = x_rob>19 && x_rob<=22 && y_rob<28 && y_rob>=26;
		return temp;
	}

	public boolean amInCella3()
	{
		boolean temp = x_rob>16 && x_rob<=19 && y_rob<28 && y_rob>=26;
		return temp;
	}

	public boolean amInCella2()
	{
		boolean temp = x_rob>13 && x_rob<=16 && y_rob<28 && y_rob>=26;
		return temp;
	}

	public boolean amInCella1()
	{
		boolean temp = x_rob>=11 && x_rob<=13 && y_rob<28 && y_rob>=26;
		return temp;
	}

	
	public boolean amInCorridoioA()
	{
		return x_rob>25 && x_rob<33 && y_rob<16 && y_rob>=12;
	}

	public boolean amInCorridoioB()
	{
		return x_rob>11 && x_rob<=33 && y_rob<6 && y_rob>=4;
	}

	public boolean amInCorridoioC()
	{
		return x_rob>11 && x_rob<19 && y_rob<16 && y_rob>=12;
	}

	public boolean amInCorridoioD()
	{
		return x_rob>11 && x_rob<=33 && y_rob<24 && y_rob>=22;
	}

	//--------------------------FINE METODI PER LOCALIZZARE IL ROBOT -------------------------------------

	public void chiudiCella()
	{
		if(amInCella1())
		{
			if(cella[0]==false)
			{
				cella[0]=true; //true significa che la cella è chiusa
			}	
		}
		else if(amInCella2())
		{
			if(cella[1]==false)
			{
				cella[1]=true;
			}
		}
		else if(amInCella3())
		{
			if(cella[2]==false)
			{
				cella[2]=true;
			}
			
		}
		else if(amInCella4())
		{
			if(cella[3]==false)
			{
				cella[3]=true;
			}			
		}
		else if(amInCella5())
		{
			if(cella[4]==false)
			{
				cella[4]=true;
			}
			
		}
		else if(amInCella6())
		{
			if(cella[5]==false)
			{
				cella[5]=true;
			}	
		}
		else if(amInCella7())
		{
			if(cella[6]==false)
			{
				cella[6]=true;
			}
		}
		else if(amInCella8())
		{
			if(cella[7]==false)
			{
				cella[7]=true;
			}
		}
		else if(amInCella9())
		{
			if(cella[8]==false)
			{
				cella[8]=true;
			}
		}
		else if(amInCella10())
		{
			if(cella[9]==false)
			{
				cella[9]=true;
			}		
		}
		else if(amInCella11())
		{
			if(cella[10]==false)
			{
				cella[10]=true;
			}
		}
		else if(amInCella12())
		{
			if(cella[11]==false)
			{
				cella[11]=true;
			}
		}
		else if(amInCella13())
		{
			if(cella[12]==false)
			{
				cella[12]=true;
			}
		}
		else if(amInCella14())
		{
			if(cella[13]==false)
			{
				cella[13]=true;
			}
		}
		else if(amInCella15())
		{
			if(cella[14]==false)
			{
				cella[14]=true;
			}
		}
		else if(amInCella16())
		{
			if(cella[15]==false)
			{
				cella[15]=true;
			}
		}
	}

	public double[] getPuntoCella(int i) //ritorna il punto attrattore della cella
	{
		double punto[]=new double[2]; 
		//punto[0] sara' la coordinata x e punto[1] sara' la coordinata y

		if(i==0)
		{
			punto[0]=12.0;
			punto[1]=27.0;
			
		}
		else if(i==1)
		{
			punto[0]=14.0;
			punto[1]=27.0;
		}
		else if(i==2)
		{
			punto[0]=17.0;
			punto[1]=27.0;
		}
		else if(i==3)
		{
			punto[0]=20.0;
			punto[1]=27.0;
		}
		else if(i==4)
		{
			punto[0]=24.0;
			punto[1]=27.0;
		}
		else if(i==5)
		{
			punto[0]=27.0;
			punto[1]=27.0;
		}
		else if(i==6)
		{
			punto[0]=30.0;
			punto[1]=27.0;
		}
		else if(i==7)
		{
			punto[0]=32.0;
			punto[1]=27.0;
		}
		else if(i==8)
		{
			punto[0]=12.0;
			punto[1]=25.0;
		}
		else if(i==9)
		{
			punto[0]=14.0;
			punto[1]=25.0;
		}
		else if(i==10)
		{
			punto[0]=17.0;
			punto[1]=25.0;
		}
		else if(i==11)
		{
			punto[0]=20.0;
			punto[1]=25.0;
		}
		else if(i==12)
		{
			punto[0]=24.0;
			punto[1]=25.0;
		}
		else if(i==13)
		{
			punto[0]=27.0;
			punto[1]=25.0;
		}
		else if(i==14)
		{
			punto[0]=30.0;
			punto[1]=25.0;
		}
		else if(i==15)
		{
			punto[0]=32.0;
			punto[1]=25.0;
		}
		int cella_vera=i+1;
		System.out.println("PUNTO ATTRATTORE VERSO LA CELLA " + cella_vera);
		return punto;
	}


	//-------------------MODIFICHE DSIR-----------------------------------
	
	/*
	 *  METODO in_special_case
	 *
	 *Indica se il robot si trova in una zona a cui bisogna applicare un particolare 
	 *angolo teta.Questo,ovviamente,è dipendente anche dalla posizione della bandierina
	 *
	 */
	public boolean in_special_case(double x_rob,double y_rob,double flag_x,double flag_y){
		
		zone_A = x_rob<19 && x_rob>11 && y_rob<8;                //robot in zone A?
		zone_B = x_rob<19 && x_rob>11 && y_rob>=8 && y_rob<12;   //robot in zone B?
		zone_C = x_rob<19 && x_rob>13 && y_rob>=12 && y_rob<15;  //robot in zone C?
		zone_D = x_rob<19 && x_rob>13 && y_rob>=15 && y_rob<18;  //robot in zone D?
		zone_E = x_rob<19 && x_rob>11 && y_rob>=18 && y_rob<22;  //robot in zone E?
		zone_F = x_rob<19 && x_rob>11 && y_rob>=22 && y_rob<28;  //robot in zone F?
		
	    zone_G = x_rob>25 && x_rob<33 && y_rob>22;               //robot in zone G?
		zone_H = x_rob>25 && x_rob<33 && y_rob<=22 && y_rob>18;  //robot in zone H?
		zone_I = x_rob>25 && x_rob<31 && y_rob<=18 && y_rob>15;  //robot in zone I?
		zone_L = x_rob>25 && x_rob<31 && y_rob<=15 && y_rob>12 ;  //robot in zone L?
		zone_M = x_rob>27 && x_rob<33 && y_rob<=12 && y_rob>8;    //robot in zone M?
		zone_N = x_rob>27 && x_rob<33 && y_rob<=8 && y_rob>0;    //robot in zone N?
		
		zone_O = x_rob>=19 && x_rob<=29 && y_rob<8 && y_rob>0;    //robot in zone O?
		zone_P = x_rob>=16 && x_rob<=25 && y_rob<28 && y_rob>22;    //robot in zone P?	
				
		
		flag_1 = flag_x>17 && flag_x<33 && flag_y<28 && flag_y>22;  //flag in zone 1?
		flag_2 = flag_x>25 && flag_x<33 && flag_y<=22 && flag_y>18; //flag in zone 2?
		flag_3 = flag_x>25 && flag_x<33 && flag_y<=18 && flag_y>12; //flag in zone 3?
		flag_4 = flag_x>25 && flag_x<33 && flag_y<=12 && flag_y>8;  //flag in zone 4?
		flag_5 = flag_x>19 && flag_x<33 && flag_y<=8 && flag_y>0;   //flag in zone 5?
		
		flag_6 = flag_x>11 && flag_x<27 && flag_y<=8 && flag_y>0;  //flag in zone 6?
		flag_7 = flag_x>11 && flag_x<19 && flag_y<=12 && flag_y>8; //flag in zone 7?
		flag_8 = flag_x>11 && flag_x<19 && flag_y<=18 && flag_y>12; //flag in zone 8?
		flag_9 = flag_x>11 && flag_x<19 && flag_y<=22 && flag_y>18;  //flag in zone 9?
		flag_10 = flag_x>11 && flag_x<25 && flag_y<28 && flag_y>22;   //flag in zone 10?
		
				
		
		
		any_zone_flag_dx=flag_1 || flag_2 || flag_3 || flag_4 || flag_5;  //la bandierina si trova in almeno una delle zone 1,2,3,4,5?
		
		any_zone_flag_sx=flag_6 || flag_7 || flag_8 || flag_9 || flag_10;  //la bandierina si trova in almeno una delle zone 6,7,8,9,10?
		
		
		
		//Condizioni per valutare se il robot e la bandierina si trovano in un caso particolare,cioè se si trovano in determinate zone
		if((zone_A && (flag_1 || flag_2 || flag_3 || flag_4)) || (zone_B && any_zone_flag_dx) || (zone_C && any_zone_flag_dx) || (zone_D && any_zone_flag_dx) || (zone_E && any_zone_flag_dx) || (zone_F && (flag_2 || flag_3 || flag_4 || flag_5)))
			
			return true;
				
		if((zone_G && (flag_6 || flag_7 || flag_8 || flag_9)) || (zone_H && any_zone_flag_sx) || (zone_I && any_zone_flag_sx) || (zone_L && any_zone_flag_sx) || (zone_M && any_zone_flag_sx) || (zone_N && (flag_7 || flag_8 || flag_9 || flag_10)))
			
			return true;
				
		if(zone_O && (flag_1 || flag_2 || flag_3 || flag_4 || flag_7 || flag_8 || flag_9 || flag_10))
		
			return true;
				
		if(zone_P && (flag_2 || flag_3 || flag_4 || flag_5 || flag_6 || flag_7 || flag_8 || flag_9))
		
			return true;
						
		else
		
			return false;					
		
	
	}
	
	
	/*
	 *  METODO getTeta
	 *
	 * Ritorna l'angolo di rotazione teta del vettore di attrazione lineare in base 
	 * alla zona in cui si trova il robot,agli ostacoli che lo circondano e alla 
	 * posizione della bandierina.
	 *
	 */
	
	public double getTeta(double x_rob,double y_rob,double flag_x,double flag_y){
		
		zone_A = x_rob<19 && x_rob>11 && y_rob<8;                //robot in zone A?
		zone_B = x_rob<19 && x_rob>11 && y_rob>=8 && y_rob<12;   //robot in zone B?
		zone_C = x_rob<19 && x_rob>13 && y_rob>=12 && y_rob<15;  //robot in zone C?
		zone_D = x_rob<19 && x_rob>13 && y_rob>=15 && y_rob<18;  //robot in zone D?
		zone_E = x_rob<19 && x_rob>11 && y_rob>=18 && y_rob<22;  //robot in zone E?
		zone_F = x_rob<16 && x_rob>11 && y_rob>=22 && y_rob<28;  //robot in zone F?
		
	    zone_G = x_rob>25 && x_rob<33 && y_rob>22;               //robot in zone G?
		zone_H = x_rob>25 && x_rob<33 && y_rob<=22 && y_rob>18;  //robot in zone H?
		zone_I = x_rob>25 && x_rob<31 && y_rob<=18 && y_rob>15;  //robot in zone I?
		zone_L = x_rob>25 && x_rob<31 && y_rob<=15 && y_rob>12 ;  //robot in zone L?
		zone_M = x_rob>27 && x_rob<33 && y_rob<=12 && y_rob>8;    //robot in zone M?
		zone_N = x_rob>27 && x_rob<33 && y_rob<=8 && y_rob>0;    //robot in zone N?
		
		zone_O = x_rob>=19 && x_rob<=29 && y_rob<8 && y_rob>0;    //robot in zone O?
		zone_P = x_rob>=15 && x_rob<=25 && y_rob<28 && y_rob>22;    //robot in zone P?	
		
		
		flag_1 = flag_x>17 && flag_x<33 && flag_y<28 && flag_y>22;  //flag in zone 1?
		flag_2 = flag_x>25 && flag_x<33 && flag_y<=22 && flag_y>18; //flag in zone 2?
		flag_3 = flag_x>25 && flag_x<33 && flag_y<=18 && flag_y>12; //flag in zone 3?
		flag_4 = flag_x>25 && flag_x<33 && flag_y<=12 && flag_y>8;  //flag in zone 4?
		flag_5 = flag_x>19 && flag_x<33 && flag_y<=8 && flag_y>0;   //flag in zone 5?
		
		flag_6 = flag_x>11 && flag_x<27 && flag_y<=8 && flag_y>0;  //flag in zone 6?
		flag_7 = flag_x>11 && flag_x<19 && flag_y<=12 && flag_y>8; //flag in zone 7?
		flag_8 = flag_x>11 && flag_x<19 && flag_y<=18 && flag_y>12; //flag in zone 8?
		flag_9 = flag_x>11 && flag_x<19 && flag_y<=22 && flag_y>18;  //flag in zone 9?
		flag_10 = flag_x>11 && flag_x<25 && flag_y<28 && flag_y>22;   //flag in zone 10?
		
		
		
		if(x_rob<20){ //robot in zona sinistra
		
			if((zone_A || zone_B) && (flag_1 || flag_2)){ 
				return 1.2;
			}
			if((zone_C || zone_D || zone_E) && (flag_1 || flag_2)){ 
				return 1.2;
			}			
			if((zone_F) && (flag_2)){ 
				return 0.4;
			}
		
				
			if((zone_A || zone_B) && ( flag_3)){ 
				return -1.15;
			}
			if((zone_C) && (flag_3)){ 
				return -1.5;
			}
			if((zone_D) && (flag_3)){ 
				return 1.5;
			}
			if((zone_E || zone_F) && ( flag_3)){ 
				return 0.8;
			}
		
		
			if((zone_A || zone_B) && ( flag_4)){ 
				return -1.15;
			}
			if((zone_B) && (flag_5)){ 
				return -0.45;
			}		
			if((zone_C || zone_D) && (flag_4 || flag_5)){ 
				return -0.75;
			}
			if((zone_E || zone_F) && (flag_4)){ 
				return 1.0;
			}
			if((zone_E || zone_F) && ( flag_5)){ 
				return -0.45;
			}
			
			
		}
		
		else{  //robot in zona destra
			
			if((zone_G || zone_H) && (flag_6 || flag_7)){ 
				return 1.2;
			}
			if((zone_I || zone_L || zone_M) && (flag_6 || flag_7)){ 
				return 1.2;
			}			
			if((zone_N) && (flag_7)){ 
				return 0.4;
			}
		
				
			if((zone_G || zone_H) && ( flag_8)){ 
				return -1.15;
			}
			if((zone_I) && (flag_8)){ 
				return -1.5;
			}
			if((zone_L) && (flag_8)){ 
				return 1.5;
			}
			if((zone_M) && ( flag_8)){ 
				return 1.2;
			}
			if((zone_N) && ( flag_8)){ 
				return 1.0;
			}
		
		
			if((zone_G || zone_H) && ( flag_9)){ 
				return -0.45;
			}
			if((zone_H) && (flag_10)){ 
				return -0.45;
			}		
			if((zone_I || zone_L) && (flag_9 || flag_10)){ 
				return -0.75;
			}
			if((zone_M || zone_N) && (flag_9)){ 
				return 1.0;
			}
			if((zone_M || zone_N) && ( flag_10)){ 
				return -0.45;
			}
			
		}		
		
		if((zone_O) && ( flag_1 || flag_2 || flag_3 || flag_4 )){ 				
				return -1.2;
		}
		if((zone_O) && ( flag_7 || flag_8 || flag_9 || flag_10 )){ 				
				return 1.2;
		}
		if((zone_P) && ( flag_2 || flag_3 || flag_4 || flag_5)){ 				
				return 1.2;
		}	
		if((zone_P) && ( flag_6 || flag_7 || flag_8 || flag_9)){ 				
				return -1.2;
		}	
		
		
		return 0.0;
		
	}
	
	
	/*
	 *  METODO getStopper
	 *
	 * Ritorna il tempo per cui il robot deve tener conto del campo potenziale 
	 * della zona in cui si trova.
	 *
	 */
	
	
	public int getStopper(double x_rob,double y_rob,double flag_x,double flag_y){
		
			zone_A = x_rob<19 && x_rob>11 && y_rob<8;                //robot in zone A?
		zone_B = x_rob<19 && x_rob>11 && y_rob>=8 && y_rob<12;   //robot in zone B?
		zone_C = x_rob<19 && x_rob>13 && y_rob>=12 && y_rob<15;  //robot in zone C?
		zone_D = x_rob<19 && x_rob>13 && y_rob>=15 && y_rob<18;  //robot in zone D?
		zone_E = x_rob<19 && x_rob>11 && y_rob>=18 && y_rob<22;  //robot in zone E?
		zone_F = x_rob<16 && x_rob>11 && y_rob>=22 && y_rob<28;  //robot in zone F?
		
	    zone_G = x_rob>25 && x_rob<33 && y_rob>22;               //robot in zone G?
		zone_H = x_rob>25 && x_rob<33 && y_rob<=22 && y_rob>18;  //robot in zone H?
		zone_I = x_rob>25 && x_rob<31 && y_rob<=18 && y_rob>15;  //robot in zone I?
		zone_L = x_rob>25 && x_rob<31 && y_rob<=15 && y_rob>12 ;  //robot in zone L?
		zone_M = x_rob>27 && x_rob<33 && y_rob<=12 && y_rob>8;    //robot in zone M?
		zone_N = x_rob>27 && x_rob<33 && y_rob<=8 && y_rob>0;    //robot in zone N?
		
		zone_O = x_rob>=19 && x_rob<=29 && y_rob<8 && y_rob>0;    //robot in zone O?
		zone_P = x_rob>=15 && x_rob<=25 && y_rob<28 && y_rob>22;    //robot in zone P?	
		
		
		flag_1 = flag_x>17 && flag_x<33 && flag_y<28 && flag_y>22;  //flag in zone 1?
		flag_2 = flag_x>25 && flag_x<33 && flag_y<=22 && flag_y>18; //flag in zone 2?
		flag_3 = flag_x>25 && flag_x<33 && flag_y<=18 && flag_y>12; //flag in zone 3?
		flag_4 = flag_x>25 && flag_x<33 && flag_y<=12 && flag_y>8;  //flag in zone 4?
		flag_5 = flag_x>19 && flag_x<33 && flag_y<=8 && flag_y>0;   //flag in zone 5?
		
		flag_6 = flag_x>11 && flag_x<27 && flag_y<=8 && flag_y>0;  //flag in zone 6?
		flag_7 = flag_x>11 && flag_x<19 && flag_y<=12 && flag_y>8; //flag in zone 7?
		flag_8 = flag_x>11 && flag_x<19 && flag_y<=18 && flag_y>12; //flag in zone 8?
		flag_9 = flag_x>11 && flag_x<19 && flag_y<=22 && flag_y>18;  //flag in zone 9?
		flag_10 = flag_x>11 && flag_x<25 && flag_y<28 && flag_y>22;   //flag in zone 10?
		
	if(x_rob<20){	//robot in zona sinistra		
		
		if((zone_A || zone_B) && (flag_1 || flag_2)){ 
			return 250;
		}
		if((zone_C || zone_D) && (flag_1 || flag_2)){ 
			return 150;
		}
		if((zone_E) && (flag_1)){ 
			return 75;
		}				
		if((zone_E || zone_F) && (flag_2)){ 
			return 100;
		}
		
				
		if((zone_A || zone_B) && ( flag_3)){ 
			return 400;
		}
		if((zone_C) && (flag_3)){ 
			return 250;
		}
		if((zone_D) && (flag_3)){ 
			return 250;
		}
		if((zone_E || zone_F) && ( flag_3)){ 
			return 230;
		}
		
		
		if((zone_A || zone_B) && ( flag_4)){ 
			return 350;
		}
		if((zone_B) && (flag_5)){ 
			return 40;
		}		
		if((zone_C || zone_D) && (flag_4 || flag_5)){ 
			return 150;
		}
		if((zone_E || zone_F) && (flag_4)){ 
			return 75;
		}
		if((zone_E || zone_F) && ( flag_5)){ 
			return 200;
		}
	
	}
	
	else{  //robot in zona destra
		
		if((zone_G || zone_H) && (flag_6 || flag_7)){ 
			return 250;
		}
		if((zone_I || zone_L) && (flag_6 || flag_7)){ 
			return 150;
		}
		if((zone_M) && (flag_6)){ 
			return 75;
		}				
		if((zone_M || zone_N) && (flag_7)){ 
			return 100;
		}
		
				
		if((zone_G || zone_H) && ( flag_8)){ 
			return 400;
		}
		if((zone_I) && (flag_8)){ 
			return 250;
		}
		if((zone_L) && (flag_8)){ 
			return 250;
		}
		if((zone_M || zone_N) && ( flag_8)){ 
			return 230;
		}
		
		
		if((zone_G|| zone_H) && ( flag_9)){ 
			return 350;
		}
		if((zone_H) && (flag_10)){ 
			return 40;
		}		
		if((zone_I || zone_L) && (flag_9 || flag_10)){ 
			return 150;
		}
		if((zone_M || zone_N) && (flag_9)){ 
			return 75;
		}
		if((zone_M || zone_N) && ( flag_10)){ 
			return 200;
		}
		
		
	}
	
	if((zone_O) && ( flag_1 || flag_2 || flag_3 || flag_4 || flag_7 || flag_8 || flag_9 || flag_10)){ 				
		return 75;
	}
	if((zone_P) && ( flag_2 || flag_3 || flag_4 || flag_5 || flag_6 || flag_7 || flag_8 || flag_9)){ 				
		return 75;
	}	
		
		
		return 0;		
	
	}
	
	
	/*
	 *  METODO getString
	 *
	 * Ritorna la stringa che indica in quale particolare zona della mappa il robot 
	 * si trova e che verr� visualizzata nella simulazione.
	 *
	 */		
	
	
  public String getString(double x_rob,double y_rob,double flag_x,double flag_y){
  	
  		zone_A = x_rob<19 && x_rob>11 && y_rob<8;                //robot in zone A?
		zone_B = x_rob<19 && x_rob>11 && y_rob>=8 && y_rob<12;   //robot in zone B?
		zone_C = x_rob<19 && x_rob>13 && y_rob>=12 && y_rob<15;  //robot in zone C?
		zone_D = x_rob<19 && x_rob>13 && y_rob>=15 && y_rob<18;  //robot in zone D?
		zone_E = x_rob<19 && x_rob>11 && y_rob>=18 && y_rob<22;  //robot in zone E?
		zone_F = x_rob<16 && x_rob>11 && y_rob>=22 && y_rob<28;  //robot in zone F?
		
	    zone_G = x_rob>25 && x_rob<33 && y_rob>22;               //robot in zone G?
		zone_H = x_rob>25 && x_rob<33 && y_rob<=22 && y_rob>18;  //robot in zone H?
		zone_I = x_rob>25 && x_rob<31 && y_rob<=18 && y_rob>15;  //robot in zone I?
		zone_L = x_rob>25 && x_rob<31 && y_rob<=15 && y_rob>12 ;  //robot in zone L?
		zone_M = x_rob>27 && x_rob<33 && y_rob<=12 && y_rob>8;    //robot in zone M?
		zone_N = x_rob>27 && x_rob<33 && y_rob<=8 && y_rob>0;    //robot in zone N?
		
		zone_O = x_rob>=19 && x_rob<=29 && y_rob<8 && y_rob>0;    //robot in zone O?
		zone_P = x_rob>=15 && x_rob<=25 && y_rob<28 && y_rob>22;    //robot in zone P?	
		
		
		flag_1 = flag_x>17 && flag_x<33 && flag_y<28 && flag_y>22;  //flag in zone 1?
		flag_2 = flag_x>25 && flag_x<33 && flag_y<=22 && flag_y>18; //flag in zone 2?
		flag_3 = flag_x>25 && flag_x<33 && flag_y<=18 && flag_y>12; //flag in zone 3?
		flag_4 = flag_x>25 && flag_x<33 && flag_y<=12 && flag_y>8;  //flag in zone 4?
		flag_5 = flag_x>19 && flag_x<33 && flag_y<=8 && flag_y>0;   //flag in zone 5?
		
		flag_6 = flag_x>11 && flag_x<27 && flag_y<=8 && flag_y>0;  //flag in zone 6?
		flag_7 = flag_x>11 && flag_x<19 && flag_y<=12 && flag_y>8; //flag in zone 7?
		flag_8 = flag_x>11 && flag_x<19 && flag_y<=18 && flag_y>12; //flag in zone 8?
		flag_9 = flag_x>11 && flag_x<19 && flag_y<=22 && flag_y>18;  //flag in zone 9?
		flag_10 = flag_x>11 && flag_x<25 && flag_y<28 && flag_y>22;   //flag in zone 10?
  	
  	if(x_rob<20){	
		
		if((zone_A || zone_B) && (flag_1 || flag_2)){ 
			return "Zone A/B to ZoneFlag 1/2";
		}
		if((zone_C || zone_D) && (flag_1 || flag_2)){ 
			return "Zone C/D to ZoneFlag 1/2";
		}
		if((zone_E) && (flag_1)){ 
			return "Zone E to ZoneFlag 1";
		}				
		if((zone_E || zone_F) && (flag_2)){ 
			return "Zone E/F to ZoneFlag 2";
		}
		
				
		if((zone_A || zone_B) && ( flag_3)){ 
			return "Zone A/B to ZoneFlag 3";
		}
		if((zone_C) && (flag_3)){ 
			return "Zone C to ZoneFlag 3";
		}
		if((zone_D) && (flag_3)){ 
			return "Zone D to ZoneFlag 3";
		}
		if((zone_E || zone_F) && ( flag_3)){ 
			return "Zone E/F to ZoneFlag 3";
		}
		
		
		if((zone_A || zone_B) && ( flag_4)){ 
			return "Zone A/B to ZoneFlag 4";
		}
		if((zone_B) && (flag_5)){ 
			return "Zone B to ZoneFlag 5";
		}		
		if((zone_C || zone_D) && (flag_4 || flag_5)){ 
			return "Zone C/D to ZoneFlag 4/5";
		}
		if((zone_E || zone_F) && (flag_4)){ 
			return "Zone E/F to ZoneFlag 4";
		}
		if((zone_E || zone_F) && ( flag_5)){ 
			return "Zone E/F to ZoneFlag 5";
		}
		
	}
	
	else{
		
		if((zone_G || zone_H) && (flag_6 || flag_7)){ 
			return "Zone G/H to ZoneFlag 6/7";
		}
		if((zone_I || zone_L) && (flag_6 || flag_7)){ 
			return "Zone I/L to ZoneFlag 6/7";
		}
		if((zone_M) && (flag_6)){ 
			return "Zone M to ZoneFlag 6";
		}				
		if((zone_M || zone_N) && (flag_7)){ 
			return "Zone M/N to ZoneFlag 7";
		}
		
				
		if((zone_G || zone_H) && ( flag_8)){ 
			return "Zone G/H to ZoneFlag 8";
		}
		if((zone_I) && (flag_8)){ 
			return "Zone I to ZoneFlag 8";
		}
		if((zone_L) && (flag_8)){ 
			return "Zone L to ZoneFlag 8";
		}
		if((zone_M || zone_N) && ( flag_8)){ 
			return "Zone M/N to ZoneFlag 8";
		}
		
		
		if((zone_G || zone_H) && ( flag_9)){ 
			return "Zone G/H to ZoneFlag 9";
		}
		if((zone_H) && (flag_10)){ 
			return "Zone H to ZoneFlag 10";
		}		
		if((zone_I || zone_L) && (flag_9 || flag_10)){ 
			return "Zone I/L to ZoneFlag 9/10";
		}
		if((zone_M || zone_N) && (flag_9)){ 
			return "Zone M/N to ZoneFlag 9";
		}
		if((zone_M || zone_N) && ( flag_10)){ 
			return "Zone M/N to ZoneFlag 10";
		}
		
	}
	
	if((zone_O) && ( flag_1 || flag_2 || flag_3 || flag_4 || flag_7 || flag_8 || flag_9 || flag_10)){ 				
			return "Zone O to ZoneFlag";
		}
	if((zone_P) && ( flag_2 || flag_3 || flag_4 || flag_5 || flag_6 || flag_7 || flag_8 || flag_9)){ 				
			return "Zone P to ZoneFlag";
		}	
		
		
		return "Vado alla bandierina Location";  // quando il robot non si trova in nessuna delle altre zone
	}
	
	
}
