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

		private static boolean cella[]=new boolean[70]; //se la iesima cella e' false vuol dire che non è stata visitata
		//private boolean zona1,zona2,zona3,zona4; //di default sono false, quindi una zona false non è chiusa. 

		private double x_rob, y_rob;

		//------------------FINE MODIFICHE DSIR-----------------------------------------

	
	
	public Location(){ }    //costruttore

	public void setPositionRobot(double x_rob, double y_rob)
	{
		setX(x_rob);
		setY(y_rob);
		chiudiCella(); //chiude la cella che si trova in quella posizione
	
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

	public void debugDSIR(int id,ReportCelle STATUS_CELLE) //per testare la posizione delle celle
	{
		if(amInCella1())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 1 E LA CHIUDO");
			STATUS_CELLE.setReportCella(0);
		}
		else if(amInCella2())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 2 E LA CHIUDO");
			STATUS_CELLE.setReportCella(1);
		}
		else if(amInCella3())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 3 E LA CHIUDO");
			STATUS_CELLE.setReportCella(2);
		}
		else if(amInCella4())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 4 E LA CHIUDO");
			STATUS_CELLE.setReportCella(3);
		}
		else if(amInCella5())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 5 E LA CHIUDO");
			STATUS_CELLE.setReportCella(4);
		}
		else if(amInCella6())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 6 E LA CHIUDO");
			STATUS_CELLE.setReportCella(5);
		}
		else if(amInCella7())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 7 E LA CHIUDO");
			STATUS_CELLE.setReportCella(6);
		}
		else if(amInCella8())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 8 E LA CHIUDO");
			STATUS_CELLE.setReportCella(7);
		}
		else if(amInCella9())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 9 E LA CHIUDO");
			STATUS_CELLE.setReportCella(8);
		}
		else if(amInCella10())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 10 E LA CHIUDO");
			STATUS_CELLE.setReportCella(9);
		}
		else if(amInCella11())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 11 E LA CHIUDO");
			STATUS_CELLE.setReportCella(10);
		}
		else if(amInCella12())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 12 E LA CHIUDO");
			STATUS_CELLE.setReportCella(11);
		}
		else if(amInCella13())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 13 E LA CHIUDO");
			STATUS_CELLE.setReportCella(12);
		}
		else if(amInCella14())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 14 E LA CHIUDO");
			STATUS_CELLE.setReportCella(13);
		}
		else if(amInCella15())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 15 E LA CHIUDO");
			STATUS_CELLE.setReportCella(14);
		}
		else if(amInCella16())
		{
			//System.out.println("************ROBOT" + id + " : SONO NELLA CELLA 16 E LA CHIUDO");
			STATUS_CELLE.setReportCella(15);
		}
		else if(amInCella17())
		{
			//System.out.println("SONO NELLA CELLA 17 E LA CHIUDO");
			STATUS_CELLE.setReportCella(16);
		}
		else if(amInCella18())
		{
			//System.out.println("SONO NELLA CELLA 18 E LA CHIUDO");
			STATUS_CELLE.setReportCella(17);
		}
		else if(amInCella19())
		{
			//System.out.println("SONO NELLA CELLA 19 E LA CHIUDO");
			STATUS_CELLE.setReportCella(18);
		}
		else if(amInCella20())
		{
			//System.out.println("SONO NELLA CELLA 20 E LA CHIUDO");
			STATUS_CELLE.setReportCella(19);
		}
		else if(amInCella21())
		{
			STATUS_CELLE.setReportCella(20);
		}
		else if(amInCella22())
		{
			STATUS_CELLE.setReportCella(21);
		}
		else if(amInCella23())
		{
			STATUS_CELLE.setReportCella(22);
		}
		else if(amInCella24())
		{
			STATUS_CELLE.setReportCella(23);
		}
		else if(amInCella25())
		{
			STATUS_CELLE.setReportCella(24);
		}
		else if(amInCella26())
		{
			STATUS_CELLE.setReportCella(25);
		}
		else if(amInCella27())
		{
			STATUS_CELLE.setReportCella(26);
		}
		else if(amInCella28())
		{
			STATUS_CELLE.setReportCella(27);
		}
		else if(amInCella29())
		{
			STATUS_CELLE.setReportCella(28);
		}
		else if(amInCella30())
		{
			STATUS_CELLE.setReportCella(29);
		}
		else if(amInCella31())
		{
			STATUS_CELLE.setReportCella(30);
		}
		else if(amInCella32())
		{
			STATUS_CELLE.setReportCella(31);
		}
		else if(amInCella33())
		{
			STATUS_CELLE.setReportCella(32);
		}
		else if(amInCella34())
		{
			STATUS_CELLE.setReportCella(33);
		}
		else if(amInCella35())
		{
			STATUS_CELLE.setReportCella(34);
		}
		else if(amInCella36())
		{
			STATUS_CELLE.setReportCella(35);
		}
		else if(amInCella37())
		{
			STATUS_CELLE.setReportCella(36);
		}
		else if(amInCella38())
		{
			STATUS_CELLE.setReportCella(37);
		}
		else if(amInCella39())
		{
			STATUS_CELLE.setReportCella(38);
		}
		else if(amInCella40())
		{
			STATUS_CELLE.setReportCella(39);
		}
		else if(amInCella41())
		{
			STATUS_CELLE.setReportCella(40);
		}
		else if(amInCella42())
		{
			STATUS_CELLE.setReportCella(41);
		}
		else if(amInCella43())
		{
			STATUS_CELLE.setReportCella(42);
		}
		else if(amInCella44())
		{
			STATUS_CELLE.setReportCella(43);
		}
		else if(amInCella45())

        {

            STATUS_CELLE.setReportCella(44);

        }

        else if(amInCella46())

        {

            STATUS_CELLE.setReportCella(45);

        }

        else if(amInCella47())

        {

            STATUS_CELLE.setReportCella(46);

        }

        else if(amInCella48())

        {

            STATUS_CELLE.setReportCella(47);

        }

        else if(amInCella49())

        {

            STATUS_CELLE.setReportCella(48);

        }

        else if(amInCella50())

        {

            STATUS_CELLE.setReportCella(49);

        }

        else if(amInCella51())

        {

            STATUS_CELLE.setReportCella(50);

        }

        else if(amInCella52())

        {

            STATUS_CELLE.setReportCella(51);

        }

        else if(amInCella53())

        {

            STATUS_CELLE.setReportCella(52);

        }

        else if(amInCella54())

        {

            STATUS_CELLE.setReportCella(53);

        }

        else if(amInCella55())

        {

            STATUS_CELLE.setReportCella(54);

        }

        else if(amInCella56())

        {

            STATUS_CELLE.setReportCella(55);

        }

        else if(amInCella57())

        {

            STATUS_CELLE.setReportCella(56);

        }

        else if(amInCella58())

        {

            STATUS_CELLE.setReportCella(57);

        }

        else if(amInCella59())

        {

            STATUS_CELLE.setReportCella(58);

        }

        else if(amInCella60())

        {

            STATUS_CELLE.setReportCella(59);

        }

        else if(amInCella61())

        {

            STATUS_CELLE.setReportCella(60);

        }

        else if(amInCella62())

        {

            STATUS_CELLE.setReportCella(61);

        }

        else if(amInCella63())

        {

            STATUS_CELLE.setReportCella(62);

        }

        else if(amInCella64())

        {

            STATUS_CELLE.setReportCella(63);

        }

        else if(amInCella65())

        {

            STATUS_CELLE.setReportCella(64);

        }

        else if(amInCella66())

        {

            STATUS_CELLE.setReportCella(65);

        }

        else if(amInCella67())

        {

            STATUS_CELLE.setReportCella(66);

        }

        else if(amInCella68())

        {

            STATUS_CELLE.setReportCella(67);

        }

        else if(amInCella69())

        {

            STATUS_CELLE.setReportCella(68);

        }

        else if(amInCella70())

        {

            STATUS_CELLE.setReportCella(69);

        }

		
	}
	
	

	//--------------------------METODI PER VERIFICARE SE UNA ZONA E' CHIUSA O MENO -------------------------------------
	
	public boolean areClosedAllZones()
	{
		return isClosedZona1() && isClosedZona2() && isClosedZona3() && isClosedZona4();
	}

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
		double point[]=new double[4]; // point[0] sarà la coordinata x e point[1] sarà la coordinata y
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
		point[2]=x_rob+2;
		point[3]=y_rob+2;
		return point; 
	}
	
	public boolean isClosedZona2()
	{
		for(int i=16;i<=34;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
		{
			if(cella[i]==false) //cella[i] è non visitata???
			{
				return false; //se esiste almeno una cella non visitata, la zona 2 non è chiusa
			}

		}
		//aggiungo cella 51 nella zona 2
		if(cella[50]==false) //cella 51 è visitata?
		{
			return false; //se esiste almeno una cella non visitata, la zona 2 non è chiusa
		}
		return true; 
	}

	//versione che ritorna il punto attrattore della prima cella non visitata della zona 2
	public double[] isClosedZona2Point()
	{	
		double point[]=new double[4]; // point[0] sarà la coordinata x e point[1] sarà la coordinata y
		for(int i=16;i<=34;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
		{
			if(cella[i]==false) //cella[i] e' non visitata??
			{
				point=getPuntoCella(i);
				return point;
				
			}
		}

		if(cella[50]==false) //cella[i] e' non visitata??
		{
			point=getPuntoCella(50);
			return point;

		}
		//se tutte le celle di quella zona sono state visitate, restituisco un punto che serve per farlo muovere un po'
		point[0]=x_rob+1;
		point[1]=y_rob+1;
		point[2]=x_rob+2;
		point[3]=y_rob+2;
		return point; 
	}

	public boolean isClosedZona3()
	{
		for(int i=35;i<=53;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
		{
			if(cella[i]==false) //cella[i] è non visitata???
			{
				return false; //se esiste almeno una cella non visitata, la zona 3 non è chiusa
			}
		}
		return true; 
	}

	//versione che ritorna il punto attrattore della prima cella non visitata della zona 3
	public double[] isClosedZona3Point()
	{	
		double point[]=new double[4]; // point[0] sarà la coordinata x e point[1] sarà la coordinata y
		for(int i=35;i<=53;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
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
		point[2]=x_rob+2;
		point[3]=y_rob+2;
		return point; 
	}

	public boolean isClosedZona4()
	{
		for(int i=54;i<=69;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
		{
			if(cella[i]==false) //cella[i] è non visitata???
			{
				return false; //se esiste almeno una cella non visitata, la zona 4 non è chiusa
			}
		}
		return true; 
	}

	//versione che ritorna il punto attrattore della prima cella non visitata della zona 4
	public double[] isClosedZona4Point()
	{	
		double point[]=new double[4]; // point[0] sarà la coordinata x e point[1] sarà la coordinata y
		for(int i=54;i<=69;i++) //-------ATTENZIONE PERCHE' LO 0 NON LO VISITA QUASI MAI (PER NON DIRE MAI)!!!!
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
		point[2]=x_rob+2;
		point[3]=y_rob+2;
		return point; 
	}
	

	//-----------------------------------------------------------------------------------------------------------------

	

	//--------------------------METODI PER LOCALIZZARE IL ROBOT (FASE EXPLORE) -------------------------------------
	 
	//----DEFINIZIONE ZONE:
	public boolean amInZona1(){

        if(amInCella1()|| amInCella2() || amInCella3() || amInCella4() || amInCella5() || amInCella6()|| amInCella7() || amInCella8() || amInCella9() || amInCella10() || amInCella11()|| amInCella12() || amInCella13() || amInCella14() || amInCella15() || amInCella16() ){

            return true;

        }
		return false;
    }


    public boolean amInZona2() {

		if (amInCella17() || amInCella18() || amInCella19() || amInCella20() || amInCella21() || amInCella22() || amInCella23() || amInCella24() || amInCella25() || amInCella26() || amInCella27() || amInCella28() || amInCella29() || amInCella30() || amInCella31() || amInCella32() || amInCella33() || amInCella34() || amInCella35() || amInCella51()) {

			return true;

		}
		return false;
	}


    public boolean amInZona3(){

        if(amInCella36()|| amInCella37() || amInCella38() || amInCella39() || amInCella40() || amInCella41()|| amInCella42() || amInCella43() || amInCella44() || amInCella45() || amInCella46()|| amInCella47() || amInCella48() || amInCella49() || amInCella50() || amInCella52() || amInCella53() || amInCella54() ){

            return true;

        }
		return false;
    }




    public boolean amInZona4(){

        if(amInCella55()|| amInCella56() || amInCella57() || amInCella58() || amInCella59() || amInCella60()|| amInCella61() || amInCella62() || amInCella63() || amInCella64() || amInCella65()|| amInCella66() || amInCella67() || amInCella68() || amInCella69() || amInCella70() ){

            return true;

        }
		return false;

    }

	//----FINE DEFINIZIONE ZONE

	//--------------------------METODI PER LOCALIZZARE IL ROBOT(CONSEGNA_BANDIERA) -------------------------------------
	 
	//----DEFINIZIONE ZONE:
	public boolean amInZonaNord(){
		return x_rob>11.0 && x_rob<33.0 && y_rob<28 && y_rob>=22;
    }


    public boolean amInZonaSud() {
		return x_rob>11.0 && x_rob<33.0 && y_rob<=8.0 && y_rob>0.0;
	}


    public boolean amInZonaEst(){
		return x_rob>25 && x_rob<33 && y_rob<22 && y_rob>8;
    }


    public boolean amInZonaOvest(){
		return x_rob>11 && x_rob<19 && y_rob<22 && y_rob>8;
    }

	//----FINE DEFINIZIONE ZONE


	
	public boolean amInCella70()
	{

		boolean temp = x_rob>31 && x_rob<=33 && y_rob<2 && y_rob>=0;

		return temp;

	}
	public boolean amInCella69()
	{

		boolean temp = x_rob>28 && x_rob<=31 && y_rob<2 && y_rob>=0;

		return temp;

	}
	public boolean amInCella68()
	{

		boolean temp = x_rob>25 && x_rob<=28 && y_rob<2 && y_rob>=0;

		return temp;

	}

	public boolean amInCella67()
	{

		boolean temp = x_rob>22 && x_rob<=25 && y_rob<2 && y_rob>=0;

		return temp;

	}
	public boolean amInCella66()
	{

		boolean temp = x_rob>19 && x_rob<=22 && y_rob<2 && y_rob>=0;

		return temp;

	}
	public boolean amInCella65()
	{

		boolean temp = x_rob>16 && x_rob<=19 && y_rob<2 && y_rob>=0;

		return temp;

	}
	public boolean amInCella64()
	{

		boolean temp = x_rob>13 && x_rob<=16 && y_rob<2 && y_rob>=0;

		return temp;

	}
	public boolean amInCella63()
	{

		boolean temp = x_rob>=11 && x_rob<=13 && y_rob<2 && y_rob>=0;;

		return temp;

	}
	public boolean amInCella62()
	{

		boolean temp = x_rob>31 && x_rob<=33 && y_rob<4 && y_rob>=2;

		return temp;

	}
	public boolean amInCella61()
	{

		boolean temp = x_rob>28 && x_rob<=31 && y_rob<4 && y_rob>=2;

		return temp;

	}
	public boolean amInCella60()
	{

		boolean temp = x_rob>25 && x_rob<=28 && y_rob<4 && y_rob>=2;

		return temp;

	}
	public boolean amInCella59()
	{

		boolean temp = x_rob>22 && x_rob<=25 && y_rob<4 && y_rob>=2;

		return temp;

	}
	public boolean amInCella58()
	{

		boolean temp = x_rob>19 && x_rob<=22 && y_rob<4 && y_rob>=2;

		return temp;

	}

	public boolean amInCella57()
	{

		boolean temp =  x_rob>16 && x_rob<=19 && y_rob<4 && y_rob>=2;

		return temp;

	}
	public boolean amInCella56()
	{

		boolean temp = x_rob>13 && x_rob<=16 && y_rob<4 && y_rob>=2;

		return temp;

	}
	public boolean amInCella55()
	{
		boolean temp = x_rob>=11 && x_rob<=13 && y_rob<4 && y_rob>=2;

		return temp;

	}
	public boolean amInCella54()
	{

		boolean temp = x_rob>31 && x_rob<=33 && y_rob<8 && y_rob>=6;

		return temp;

	}
	public boolean amInCella53()
	{

		boolean temp = x_rob>28 && x_rob<=31 && y_rob<8 && y_rob>=6;

		return temp;

	}
	public boolean amInCella52()
	{

		boolean temp = x_rob>25 && x_rob<=28 && y_rob<8 && y_rob>=6;

		return temp;

	}
	public boolean amInCella51()
	{

		boolean temp = x_rob>22 && x_rob<=25 && y_rob<8 && y_rob>=6;

		return temp;

	}
	public boolean amInCella50()
	{

		boolean temp = x_rob>31 && x_rob<=33 && y_rob<10 && y_rob>=8;

		return temp;

	}
	public boolean amInCella49()
	{

		boolean temp = x_rob>28 && x_rob<=31 && y_rob<10 && y_rob>=8;

		return temp;

	}
	public boolean amInCella48()
	{

		boolean temp = x_rob>25 && x_rob<=28 && y_rob<10 && y_rob>=8;

		return temp;

	}

	public boolean amInCella47()
	{

		boolean temp = x_rob>31 && x_rob<=33 && y_rob<12 && y_rob>=10;

		return temp;

	}
	public boolean amInCella46()
	{

		boolean temp = x_rob>28 && x_rob<=31 && y_rob<12 && y_rob>=10;

		return temp;

	}
	public boolean amInCella45()
	{
		boolean temp = x_rob>25 && x_rob<=28 && y_rob<12 && y_rob>=10;
		return temp;   
	}

	public boolean amInCella44() //SITUAZIONE CRITICA
	{
		boolean temp = x_rob>31 && x_rob<=33 && y_rob<18 && y_rob>=16;
		return temp;   
	}

	public boolean amInCella43()
	{
		boolean temp =x_rob>28 && x_rob<=31 && y_rob<18 && y_rob>=16;
		return temp;   
	}

	public boolean amInCella42()
	{
		boolean temp =x_rob>25 && x_rob<=28 && y_rob<18 && y_rob>=16;
		return temp;   
	}

	public boolean amInCella41()
	{
		boolean temp =x_rob>31 && x_rob<=33 && y_rob<20 && y_rob>=18;
		return temp;   
	}

	public boolean amInCella40()
	{
		boolean temp =x_rob>28 && x_rob<=31 && y_rob<20 && y_rob>=18;
		return temp;   
	}

	public boolean amInCella39()
	{
		boolean temp = x_rob>25 && x_rob<=28 && y_rob<20 && y_rob>=18;
		return temp;   
	}
	public boolean amInCella38()
	{
		boolean temp = x_rob>31 && x_rob<=33 && y_rob<22 && y_rob>=20;
		return temp;   
	}

	public boolean amInCella37()
	{
		boolean temp =x_rob>28 && x_rob<=31 && y_rob<22 && y_rob>=20;
		return temp;   
	}

	public boolean amInCella36()
	{
		boolean temp = x_rob>25 && x_rob<=28 && y_rob<22 && y_rob>=20;
		return temp;   
	}

	public boolean amInCella35()
	{
		boolean temp = x_rob>19 && x_rob<=22 && y_rob<8 && y_rob>=6;
		return temp;   
	}

	public boolean amInCella34()
	{
		boolean temp = x_rob>16 && x_rob<=19 && y_rob<8 && y_rob>=6;
		return temp;   
	}

	public boolean amInCella33()
	{
		boolean temp =x_rob>13 && x_rob<=16 && y_rob<8 && y_rob>=6;
		return temp;   
	}

	public boolean amInCella32()
	{
		boolean temp =x_rob>=11 && x_rob<=13 && y_rob<8 && y_rob>=6;
		return temp;   
	}

	public boolean amInCella31()
	{
		boolean temp =x_rob>16 && x_rob<=19 && y_rob<10 && y_rob>=8;
		return temp;   
	}

	public boolean amInCella30()
	{
		boolean temp = x_rob>13 && x_rob<=16 && y_rob<10 && y_rob>=8;
		return temp;   
	}

	public boolean amInCella29()
	{
		boolean temp =x_rob>=11 && x_rob<=13 && y_rob<10 && y_rob>=8;
		return temp;   
	}

	public boolean amInCella28()
	{
		boolean temp =x_rob>16 && x_rob<=19 && y_rob<12 && y_rob>=10;
		return temp;   
	}

	public boolean amInCella27()
	{
		boolean temp =x_rob>13 && x_rob<=16 && y_rob<12 && y_rob>=10;
		return temp;   
	}

	public boolean amInCella26()
	{
		boolean temp =x_rob>=11 && x_rob<=13 && y_rob<12 && y_rob>=10;
		return temp;   
	}

	public boolean amInCella25()
	{
		boolean temp = x_rob>16 && x_rob<=19 && y_rob<18 && y_rob>=16;
		return temp;   
	}

	public boolean amInCella24()
	{
		boolean temp =x_rob>13 && x_rob<=16 && y_rob<18 && y_rob>=16;
		return temp;   
	}

	public boolean amInCella23()
	{
		boolean temp =x_rob>=11 && x_rob<=13 && y_rob<18 && y_rob>=16;
		return temp;   
	}

	public boolean amInCella22()
	{
		boolean temp = x_rob>16 && x_rob<=19 && y_rob<20 && y_rob>=18;
		return temp;   
	}

	public boolean amInCella21()
	{
		boolean temp = x_rob>13 && x_rob<=16 && y_rob<20 && y_rob>=18;
		return temp;   
	}

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
		else if(amInCella17())
		{
			if(cella[16]==false)
			{
				cella[16]=true;
			}
		}
		else if(amInCella18())
		{
			if(cella[17]==false)
			{
				cella[17]=true;
			}
		}
		else if(amInCella19())
		{
			if(cella[18]==false)
			{
				cella[18]=true;
			}
		}
		else if(amInCella20())
		{
			if(cella[19]==false)
			{
				cella[19]=true;
			}
		}
		else if(amInCella21())
		{
			if(cella[20]==false)
			{
				cella[20]=true;
			}
		}
		else if(amInCella22())
		{
			if(cella[21]==false)
			{
				cella[21]=true;
			}
		}
		else if(amInCella23())
		{
			if(cella[22]==false)
			{
				cella[22]=true;
			}
		}
		else if(amInCella24())
		{
			if(cella[23]==false)
			{
				cella[23]=true;
			}
		}
		else if(amInCella25())
		{
			if(cella[24]==false)
			{
				cella[24]=true;
			}
		}
		else if(amInCella26())
		{
			if(cella[25]==false)
			{
				cella[25]=true;
			}
		}
		else if(amInCella27())
		{
			if(cella[26]==false)
			{
				cella[26]=true;
			}
		}
		else if(amInCella28())
		{
			if(cella[27]==false)
			{
				cella[27]=true;
			}
		}
		else if(amInCella29())
		{
			if(cella[28]==false)
			{
				cella[28]=true;
			}
		}
		else if(amInCella30())
		{
			if(cella[29]==false)
			{
				cella[29]=true;
			}
		}
		else if(amInCella31())
		{
			if(cella[30]==false)
			{
				cella[30]=true;
			}
		}
		else if(amInCella32())
		{
			if(cella[31]==false)
			{
				cella[31]=true;
			}
		}
		else if(amInCella33())
		{
			if(cella[32]==false)
			{
				cella[32]=true;
			}
		}
		else if(amInCella34())
		{
			if(cella[33]==false)
			{
				cella[33]=true;
			}
		}
		else if(amInCella35())
		{
			if(cella[34]==false)
			{
				cella[34]=true;
			}
		}
		else if(amInCella36())
		{
			if(cella[35]==false)
			{
				cella[35]=true;
			}
		}
		else if(amInCella37())
		{
			if(cella[36]==false)
			{
				cella[36]=true;
			}
		}
		else if(amInCella38())
		{
			if(cella[37]==false)
			{
				cella[37]=true;
			}
		}
		else if(amInCella39())
		{
			if(cella[38]==false)
			{
				cella[38]=true;
			}
		}
		else if(amInCella40())
		{
			if(cella[39]==false)
			{
				cella[39]=true;
			}
		}
		else if(amInCella41())
		{
			if(cella[40]==false)
			{
				cella[40]=true;
			}
		}
		else if(amInCella42())
		{
			if(cella[41]==false)
			{
				cella[41]=true;
			}
		}
		else if(amInCella43())
		{
			if(cella[42]==false)
			{
				cella[42]=true;
			}
		}
		else if(amInCella44())
		{
			if(cella[43]==false)
			{
				cella[43]=true;
			}
		}
		else if(amInCella45())
		{
			if(cella[44]==false)
			{
				cella[44]=true;
			}
		}
		else if(amInCella46())
		{
			if(cella[45]==false)
			{
				cella[45]=true;
			}
		}
		else if(amInCella47())
		{
			if(cella[46]==false)
			{
				cella[46]=true;
			}
		}
		else if(amInCella48())
		{
			if(cella[47]==false)
			{
				cella[47]=true;
			}
		}
		else if(amInCella49())
		{
			if(cella[48]==false)
			{
				cella[48]=true;
			}
		}
		else if(amInCella50())
		{
			if(cella[49]==false)
			{
				cella[49]=true;
			}
		}
		else if(amInCella51())
		{
			if(cella[50]==false)
			{
				cella[50]=true;
			}
		}
		else if(amInCella52())
		{
			if(cella[51]==false)
			{
				cella[51]=true;
			}
		}
		else if(amInCella53())
		{
			if(cella[52]==false)
			{
				cella[52]=true;
			}
		}
		else if(amInCella54())
		{
			if(cella[53]==false)
			{
				cella[53]=true;
			}
		}
		else if(amInCella55())
		{
			if(cella[54]==false)
			{
				cella[54]=true;
			}
		}

		else if(amInCella56())
		{
			if(cella[55]==false)
			{
				cella[55]=true;
			}
		}
		else if(amInCella57())
		{
			if(cella[56]==false)
			{
				cella[56]=true;
			}
		}
		else if(amInCella58())
		{
			if(cella[57]==false)
			{
				cella[57]=true;
			}
		}
		else if(amInCella59())
		{
			if(cella[58]==false)
			{
				cella[58]=true;
			}
		}
		else if(amInCella60())
		{
			if(cella[59]==false)
			{
				cella[59]=true;
			}
		}
		else if(amInCella61())
		{
			if(cella[60]==false)
			{
				cella[60]=true;
			}
		}
		else if(amInCella62())
		{
			if(cella[61]==false)
			{
				cella[61]=true;
			}
		}
		else if(amInCella63())
		{
			if(cella[62]==false)
			{
				cella[62]=true;
			}
		}
		else if(amInCella64())
		{
			if(cella[63]==false)
			{
				cella[63]=true;
			}
		}
		else if(amInCella65())
		{
			if(cella[64]==false)
			{
				cella[64]=true;
			}
		}

		else if(amInCella66())
		{
			if(cella[65]==false)
			{
				cella[65]=true;
			}
		}
		else if(amInCella67())
		{
			if(cella[66]==false)
			{
				cella[66]=true;
			}
		}
		else if(amInCella68())
		{
			if(cella[67]==false)
			{
				cella[67]=true;
			}
		}
		else if(amInCella69())
		{
			if(cella[68]==false)
			{
				cella[68]=true;
			}
		}
		else if(amInCella70())
		{
			if(cella[69]==false)
			{
				cella[69]=true;
			}
		}

	}

	public double[] getPuntoCella(int i) //ritorna il punto attrattore della cella
	{
		double punto[]=new double[4]; 
		//punto[0] sara' la coordinata x e punto[1] sara' la coordinata y DEL VERO PUNTO ATTRATTORE FINALE
		//punto[2] e punto[3] saranno rispettivamente x e y del punto prima del punto attrattore (cioè, un punto intermedio)

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
		else if(i==16)
		{
			punto[0]=12;
			punto[1]=21;
		}
		else if(i==17)
		{
			punto[0]=14.5;
			punto[1]=21;
		}
		else if(i==18)
		{
			punto[0]=17.5;
			punto[1]=21;
		}
		else if(i==19)
		{
			punto[0]=12;
			punto[1]=19;
		}
		else if(i==20)
		{
			punto[0]=14.5;
			punto[1]=19;
		}
		else if(i==21)
		{
			punto[0]=17.5;
			punto[1]=19;
		}
		else if(i==22)
		{
			punto[0]=12;
			punto[1]=17;
		}
		else if(i==23)
		{
			punto[0]=14.5;
			punto[1]=17;
		}
		else if(i==24)
		{
			punto[0]=17.5;
			punto[1]=17;
		}
		else if(i==25)
		{
			punto[0]=12;
			punto[1]=11;
		}
		else if(i==26)
		{
			punto[0]=14.5;
			punto[1]=11;
		}
		else if(i==27)
		{
			punto[0]=17.5;
			punto[1]=11;
		}
		else if(i==28)
		{
			punto[0]=12;
			punto[1]=9;
		}
		else if(i==29)
		{
			punto[0]=14.5;
			punto[1]=9;
		}
		else if(i==30)
		{
			punto[0]=17.5;
			punto[1]=9;
		}
		else if(i==31)
		{
			punto[0]=12;
			punto[1]=7;
		}
		else if(i==32)
		{
			punto[0]=14.5;
			punto[1]=7;
		}
		else if(i==33)
		{
			punto[0]=17.5;
			punto[1]=7;
		}
		else if(i==34)
		{
			punto[0]=20.5;
			punto[1]=7;
		}
		else if(i==35)
		{
			punto[0]=26.5;
			punto[1]=21;
		}
		else if(i==36)
		{
			punto[0]=29.5;
			punto[1]=21;
		}
		else if(i==37)
		{
			punto[0]=32;
			punto[1]=21;
		}
		else if(i==38)
		{
			punto[0]=26.5;
			punto[1]=19;
		}
		else if(i==39)
		{
			punto[0]=29.5;
			punto[1]=19;
		}
		else if(i==40)
		{
			punto[0]=32;
			punto[1]=19;
		}
		else if(i==41)
		{
			punto[0]=26.5;
			punto[1]=17;
		}
		else if(i==42)
		{
			punto[0]=29.5;
			punto[1]=17;
		}
		else if(i==43)
		{
			punto[0]=32;
			punto[1]=17;
		}
		else if(i==44)
		{
			punto[0]=26.5;
			punto[1]=11;
		}
		else if (i == 45) {
			punto[0] = 29.5;
			punto[1] = 11.0;
		} else if (i == 46) {
			punto[0] = 32.0;
			punto[1] = 11.0;
		} else if (i == 47) {
			punto[0] = 26.5;
			punto[1] = 9.0;
		} else if (i == 48) {
			punto[0] = 29.5;
			punto[1] = 9.0;
		} else if (i == 49) {
			punto[0] = 32.0;
			punto[1] = 9.0;
		} else if (i == 50) {
			punto[0] = 23.5;
			punto[1] = 7.0;
		} else if (i == 51) {
			punto[0] = 26.5;
			punto[1] = 7.0;
		} else if (i == 52) {
			punto[0] = 29.5;
			punto[1] = 7.0;
		} else if (i == 53) {
			punto[0] = 32.0;
			punto[1] = 7.0;
		} else if (i == 54) {
			punto[0] = 12.0;
			punto[1] = 3.0;
		} else if (i == 55) {
			punto[0] = 14.5;
			punto[1] = 3.0;
		} else if (i == 56) {
			punto[0] = 17.5;
			punto[1] = 3.0;
		} else if (i == 57) {
			punto[0] = 20.5;
			punto[1] = 3.0;
		} else if (i == 58) {
			punto[0] = 23.5;
			punto[1] = 3.0;
		} else if (i == 59) {
			punto[0] = 26.5;
			punto[1] = 3.0;
		} else if (i == 60) {
			punto[0] = 29.5;
			punto[1] = 3.0;
		} else if (i == 61) {
			punto[0] = 32.0;
			punto[1] = 3.0;
		} else if (i == 62) {
			punto[0] = 12.5;
			punto[1] = 1.6;
		} else if (i == 63) {
			punto[0] = 14.5;
			punto[1] = 1.6;
		} else if (i == 64) {
			punto[0] = 17.5;
			punto[1] = 1.6;
		} else if (i == 65) {
			punto[0] = 20.5;
			punto[1] = 1.6;
		} else if (i == 66) {
			punto[0] = 23.5;
			punto[1] = 1.6;
		} else if (i == 67) {
			punto[0] = 26.5;
			punto[1] = 1.6;
		} else if (i == 68) {
			punto[0] = 29.5;
			punto[1] = 1.6;
		} else if (i == 69) {
			punto[0] = 32.0;
			punto[1] = 1.6;
		}


		punto[2]=punto[0];
		punto[3]=punto[1]-1;
		int cella_vera=i+1;
		System.out.println("PUNTO ATTRATTORE VERSO LA CELLA " + cella_vera);
		return punto;
	}


	public boolean checkPosition(double x,double y) //ritorna vero se il robot si trova in quella posizione (x,y)
	{
		return x_rob==x && y_rob==y;
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
