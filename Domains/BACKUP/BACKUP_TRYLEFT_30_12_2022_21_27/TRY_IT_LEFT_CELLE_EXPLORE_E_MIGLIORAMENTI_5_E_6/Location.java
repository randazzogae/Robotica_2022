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

		//CELLE DSIR
		private boolean cella1,cella2,cella3,cella4,cella5,cella6,cella7,cella8,cella9,cella10,cella11,cella12;
		private boolean cella13,cella14,cella15,cella16,cella17,cella18,cella19,cella20,cella21,cella22,cella23;
		private boolean cella24,cella25,cella26,cella27,cella28,cella29,cella30,cella31,cella32,cella33,cella34;
		private boolean cella35,cella36,cella37,cella38,cella39,cella40,cella41,cella42,cella43,cella44,cella45;
		private boolean cella46,cella47,cella48,cella49,cella50,cella51,cella52,cella53,cella54,cella55,cella56;
		private boolean cella57,cella58,cella59,cella60,cella61,cella62,cella63,cella64,cella65,cella66,cella67;
		private boolean cella68,cella69,cella70;
		private boolean corridoioA, corridoioB, corridoioC, corridoioD;
				
		private boolean any_zone_flag_sx,any_zone_flag_dx; //Indicano se le bandierine si trovano in qualche zona di quelle precedentemente dichiarate

		
	
	
	public Location(){ }    //costruttore
	
	public String testPositionDsir(double x_rob,double y_rob) //restituisce una stringa che indica in quale cella si trova il robot
	{
		cella1 = x_rob>=11 && x_rob<=13 && y_rob<28 && y_rob>=26;         //y=28 muro in alto

        cella2 = x_rob>13 && x_rob<=16 && y_rob<28 && y_rob>=26;

        cella3 = x_rob>16 && x_rob<=19 && y_rob<28 && y_rob>=26;

        cella4 = x_rob>19 && x_rob<=22 && y_rob<28 && y_rob>=26;

        cella5 = x_rob>22 && x_rob<=25 && y_rob<28 && y_rob>=26;

        cella6 = x_rob>25 && x_rob<=28 && y_rob<28 && y_rob>=26;

        cella7 = x_rob>28 && x_rob<=31 && y_rob<28 && y_rob>=26;

        cella8 = x_rob>31 && x_rob<=33 && y_rob<28 && y_rob>=26;

		corridoioA = x_rob>25 && x_rob<33 && y_rob<16 && y_rob>=12;

        corridoioB = x_rob>11 && x_rob<=33 && y_rob<6 && y_rob>=4;

        corridoioC = x_rob>11 && x_rob<19 && y_rob<16 && y_rob>=12;

        corridoioD = x_rob>11 && x_rob<=33 && y_rob<24 && y_rob>=22;

		//-----cella1 colonna
        cella9 = x_rob>=11 && x_rob<=13 && y_rob<26 && y_rob>=24;

        cella17 = x_rob>=11 && x_rob<=13 && y_rob<22 && y_rob>=20;

        cella20 = x_rob>=11 && x_rob<=13 && y_rob<20 && y_rob>=18;

        cella23 = x_rob>=11 && x_rob<=13 && y_rob<18 && y_rob>=16;

        cella26 = x_rob>=11 && x_rob<=13 && y_rob<12 && y_rob>=10;

        cella29 = x_rob>=11 && x_rob<=13 && y_rob<10 && y_rob>=8;

        cella32 = x_rob>=11 && x_rob<=13 && y_rob<8 && y_rob>=6;

        cella55 = x_rob>=11 && x_rob<=13 && y_rob<4 && y_rob>=2;

        cella63 = x_rob>=11 && x_rob<=13 && y_rob<2 && y_rob>=0;
		
		//-----cella2 colonna
        cella10 = x_rob>13 && x_rob<=16 && y_rob<26 && y_rob>=24;

        cella18 = x_rob>13 && x_rob<=16 && y_rob<22 && y_rob>=20;

        cella21 = x_rob>13 && x_rob<=16 && y_rob<20 && y_rob>=18;

        cella24 = x_rob>13 && x_rob<=16 && y_rob<18 && y_rob>=16;

        cella27 = x_rob>13 && x_rob<=16 && y_rob<12 && y_rob>=10;

        cella30 = x_rob>13 && x_rob<=16 && y_rob<10 && y_rob>=8;

        cella33 = x_rob>13 && x_rob<=16 && y_rob<8 && y_rob>=6;

        cella56 = x_rob>13 && x_rob<=16 && y_rob<4 && y_rob>=2;

        cella64 = x_rob>13 && x_rob<=16 && y_rob<2 && y_rob>=0;

		//-----cella 3 colonna
        cella11 = x_rob>16 && x_rob<=19 && y_rob<26 && y_rob>=24;

        cella19 = x_rob>16 && x_rob<=19 && y_rob<22 && y_rob>=20;

        cella22 = x_rob>16 && x_rob<=19 && y_rob<20 && y_rob>=18;

        cella25 = x_rob>16 && x_rob<=19 && y_rob<18 && y_rob>=16;

        cella28 = x_rob>16 && x_rob<=19 && y_rob<12 && y_rob>=10;

        cella31 = x_rob>16 && x_rob<=19 && y_rob<10 && y_rob>=8;

        cella34 = x_rob>16 && x_rob<=19 && y_rob<8 && y_rob>=6;

        cella57 = x_rob>16 && x_rob<=19 && y_rob<4 && y_rob>=2;

        cella65 = x_rob>16 && x_rob<=19 && y_rob<2 && y_rob>=0;

		//-----cella 4 colonna

        cella12 = x_rob>19 && x_rob<=22 && y_rob<26 && y_rob>=24;

        cella35 = x_rob>19 && x_rob<=22 && y_rob<8 && y_rob>=6;

        cella58 = x_rob>19 && x_rob<=22 && y_rob<4 && y_rob>=2;

        cella66 = x_rob>19 && x_rob<=22 && y_rob<2 && y_rob>=0; 

		//-----cella 5 colonna

        cella13 = x_rob>22 && x_rob<=25 && y_rob<26 && y_rob>=24;

        cella51 = x_rob>22 && x_rob<=25 && y_rob<8 && y_rob>=6;

        cella59 = x_rob>22 && x_rob<=25 && y_rob<4 && y_rob>=2;

        cella67 = x_rob>22 && x_rob<=25 && y_rob<2 && y_rob>=0;

		//----cella 6 colonna
        cella14 = x_rob>25 && x_rob<=28 && y_rob<26 && y_rob>=24;

        cella36 = x_rob>25 && x_rob<=28 && y_rob<22 && y_rob>=20;

        cella39 = x_rob>25 && x_rob<=28 && y_rob<20 && y_rob>=18;

        cella42 = x_rob>25 && x_rob<=28 && y_rob<18 && y_rob>=16;

        cella45 = x_rob>25 && x_rob<=28 && y_rob<12 && y_rob>=10;

        cella48 = x_rob>25 && x_rob<=28 && y_rob<10 && y_rob>=8;

        cella52 = x_rob>25 && x_rob<=28 && y_rob<8 && y_rob>=6;

        cella60 = x_rob>25 && x_rob<=28 && y_rob<4 && y_rob>=2;

        cella68 = x_rob>25 && x_rob<=28 && y_rob<2 && y_rob>=0;

		//----cella 7 colonna
        cella15 = x_rob>28 && x_rob<=31 && y_rob<26 && y_rob>=24;

        cella37 = x_rob>28 && x_rob<=31 && y_rob<22 && y_rob>=20;

        cella40 = x_rob>28 && x_rob<=31 && y_rob<20 && y_rob>=18;

        cella43 = x_rob>28 && x_rob<=31 && y_rob<18 && y_rob>=16;

        cella46 = x_rob>28 && x_rob<=31 && y_rob<12 && y_rob>=10;

        cella49 = x_rob>28 && x_rob<=31 && y_rob<10 && y_rob>=8;

        cella53 = x_rob>28 && x_rob<=31 && y_rob<8 && y_rob>=6;

        cella61 = x_rob>28 && x_rob<=31 && y_rob<4 && y_rob>=2;

        cella69 = x_rob>28 && x_rob<=31 && y_rob<2 && y_rob>=0;

		//----cella 8 colonna
        cella16 = x_rob>31 && x_rob<=33 && y_rob<26 && y_rob>=24;

        cella38 = x_rob>31 && x_rob<=33 && y_rob<22 && y_rob>=20;

        cella41 = x_rob>31 && x_rob<=33 && y_rob<20 && y_rob>=18;

        cella44 = x_rob>31 && x_rob<=33 && y_rob<18 && y_rob>=16;

        cella47 = x_rob>31 && x_rob<=33 && y_rob<12 && y_rob>=10;

        cella50 = x_rob>31 && x_rob<=33 && y_rob<10 && y_rob>=8;

        cella54 = x_rob>31 && x_rob<=33 && y_rob<8 && y_rob>=6;

        cella62 = x_rob>31 && x_rob<=33 && y_rob<4 && y_rob>=2;

        cella70 = x_rob>31 && x_rob<=33 && y_rob<2 && y_rob>=0;

		if(cella1)
		{
			return "SONO NELLA CELLA 1";
		}
		else if(cella2)
		{
			return "SONO NELLA CELLA 2";
		}
		else if(cella3)
		{
			return "SONO NELLA CELLA 3";
		}
		else if(cella4)
		{
			return "SONO NELLA CELLA 4";
		}
		else if(cella5)
		{
			return "SONO NELLA CELLA 5";
		}
		else if(cella6)
		{
			return "SONO NELLA CELLA 6";
		}
		else if(cella7)
		{
			return "SONO NELLA CELLA 7";
		}
		else if(cella8)
		{
			return "SONO NELLA CELLA 8";
		}
		else if(cella9)
		{
			return "SONO NELLA CELLA 9";
		}
		else if(cella10)
		{
			return "SONO NELLA CELLA 10";
		}
		else if(cella11)
		{
			return "SONO NELLA CELLA 11";
		}
		else if(cella12)
		{
			return "SONO NELLA CELLA 12";
		}
		else if(cella13)
		{
			return "SONO NELLA CELLA 13";
		}
		else if(cella14)
		{
			return "SONO NELLA CELLA 14";
		}
		else if(cella15)
		{
			return "SONO NELLA CELLA 15";
		}
		else if(cella16)
		{
			return "SONO NELLA CELLA 16";
		}
		else if(cella17)
		{
			return "SONO NELLA CELLA 17";
		}
		else if(cella18)
		{
			return "SONO NELLA CELLA 18";
		}
		else if(cella19)
		{
			return "SONO NELLA CELLA 19";
		}
		else if(cella20)
		{
			return "SONO NELLA CELLA 20";
		}
		else if(cella21)
		{
			return "SONO NELLA CELLA 21";
		}
		else if(cella22)
		{
			return "SONO NELLA CELLA 22";
		}
		else if(cella23)
		{
			return "SONO NELLA CELLA 23";
		}
		else if(cella24)
		{
			return "SONO NELLA CELLA 24";
		}
		else if(cella25)
		{
			return "SONO NELLA CELLA 25";
		}
		else if(cella26)
		{
			return "SONO NELLA CELLA 26";
		}
		else if(cella27)
		{
			return "SONO NELLA CELLA 27";
		}
		else if(cella28)
		{
			return "SONO NELLA CELLA 28";
		}
		else if(cella29)
		{
			return "SONO NELLA CELLA 29";
		}
		else if(cella30)
		{
			return "SONO NELLA CELLA 30";
		}
		else if(cella31)
		{
			return "SONO NELLA CELLA 31";
		}
		else if(cella32)
		{
			return "SONO NELLA CELLA 32";
		}
		else if(cella33)
		{
			return "SONO NELLA CELLA 33";
		}
		else if(cella34)
		{
			return "SONO NELLA CELLA 34";
		}
		else if(cella35)
		{
			return "SONO NELLA CELLA 35";
		}
		else if(cella36)
		{
			return "SONO NELLA CELLA 36";
		}
		else if(cella37)
		{
			return "SONO NELLA CELLA 37";
		}
		else if(cella38)
		{
			return "SONO NELLA CELLA 38";
		}
		else if(cella39)
		{
			return "SONO NELLA CELLA 39";
		}
		else if(cella40)
		{
			return "SONO NELLA CELLA 40";
		}
		else if(cella41)
		{
			return "SONO NELLA CELLA 41";
		}
		else if(cella42)
		{
			return "SONO NELLA CELLA 42";
		}
		else if(cella43)
		{
			return "SONO NELLA CELLA 43";
		}
		else if(cella44)
		{
			return "SONO NELLA CELLA 44";
		}
		else if(cella45)
		{
			return "SONO NELLA CELLA 45";
		}
		else if(cella46)
		{
			return "SONO NELLA CELLA 46";
		}
		else if(cella47)
		{
			return "SONO NELLA CELLA 47";
		}
		else if(cella48)
		{
			return "SONO NELLA CELLA 48";
		}
		else if(cella49)
		{
			return "SONO NELLA CELLA 49";
		}
		else if(cella50)
		{
			return "SONO NELLA CELLA 50";
		}
		else if(cella51)
		{
			return "SONO NELLA CELLA 51";
		}
		else if(cella52)
		{
			return "SONO NELLA CELLA 52";
		}
		else if(cella53)
		{
			return "SONO NELLA CELLA 53";
		}
		else if(cella54)
		{
			return "SONO NELLA CELLA 54";
		}
		else if(cella55)
		{
			return "SONO NELLA CELLA 55";
		}
		else if(cella56)
		{
			return "SONO NELLA CELLA 56";
		}
		else if(cella57)
		{
			return "SONO NELLA CELLA 57";
		}
		else if(cella58)
		{
			return "SONO NELLA CELLA 58";
		}
		else if(cella59)
		{
			return "SONO NELLA CELLA 59";
		}
		else if(cella60)
		{
			return "SONO NELLA CELLA 60";
		}
		else if(cella61)
		{
			return "SONO NELLA CELLA 61";
		}
		else if(cella62)
		{
			return "SONO NELLA CELLA 62";
		}
		else if(cella63)
		{
			return "SONO NELLA CELLA 63";
		}
		else if(cella64)
		{
			return "SONO NELLA CELLA 64";
		}
		else if(cella65)
		{
			return "SONO NELLA CELLA 65";
		}
		else if(cella66)
		{
			return "SONO NELLA CELLA 66";
		}
		else if(cella67)
		{
			return "SONO NELLA CELLA 67";
		}
		else if(cella68)
		{
			return "SONO NELLA CELLA 68";
		}
		else if(cella69)
		{
			return "SONO NELLA CELLA 69";
		}
		else if(cella70)
		{
			return "SONO NELLA CELLA 70";
		}
		else if(corridoioA)
		{
			return "SONO NEL CORRIDOIO A";
		}
		else if(corridoioB)
		{
			return "SONO NEL CORRIDOIO B";
		}
		else if(corridoioC)
		{
			return "SONO NEL CORRIDOIO C";
		}
		else if(corridoioD)
		{
			return "SONO NEL CORRIDOIO D";
		}
		

		return "ERROR: NON SONO IN NESSUNA DELLE 70 CELLE ATTUALMENTE DEFINITE";
	}

	//--------------------------METODI PER LOCALIZZARE IL ROBOT -------------------------------------
	public boolean amInCella16(double x_rob,double y_rob)
	{
        cella15 = x_rob>28 && x_rob<=31 && y_rob<26 && y_rob>=24;
		return cella16;   
	}
	public boolean amInCella15(double x_rob,double y_rob)
	{
        cella15 = x_rob>28 && x_rob<=31 && y_rob<26 && y_rob>=24;
		return cella15;   
	}
	public boolean amInCella14(double x_rob,double y_rob)
	{
        cella14 = x_rob>25 && x_rob<=28 && y_rob<26 && y_rob>=24;
		return cella14;   
	}
	public boolean amInCella13(double x_rob,double y_rob)
	{
		cella13 = x_rob>22 && x_rob<=25 && y_rob<26 && y_rob>=24;
		return cella13;   
	}
	public boolean amInCella12(double x_rob,double y_rob)
	{
        cella12 = x_rob>19 && x_rob<=22 && y_rob<26 && y_rob>=24;
		return cella12;   
	}
	public boolean amInCella11(double x_rob,double y_rob)
	{
        cella11 = x_rob>16 && x_rob<=19 && y_rob<26 && y_rob>=24;
		return cella11;   
	}
	public boolean amInCella10(double x_rob,double y_rob)
	{
        cella10 = x_rob>13 && x_rob<=16 && y_rob<26 && y_rob>=24;
		return cella10;   
	}

	public boolean amInCella9(double x_rob,double y_rob)
	{
        cella9 = x_rob>=11 && x_rob<=13 && y_rob<26 && y_rob>=24;
		return cella9;   
	}


	public boolean amInCella8(double x_rob,double y_rob)
	{
        cella8 = x_rob>31 && x_rob<=33 && y_rob<28 && y_rob>=26;
		return cella8;   
	}

	public boolean amInCella7(double x_rob,double y_rob)
	{
        cella7 = x_rob>28 && x_rob<=31 && y_rob<28 && y_rob>=26;
		return cella7;   
	}

	public boolean amInCella6(double x_rob,double y_rob)
	{
        cella6 = x_rob>25 && x_rob<=28 && y_rob<28 && y_rob>=26;
		return cella6;
	}

	public boolean amInCella5(double x_rob,double y_rob)
	{
        cella5 = x_rob>22 && x_rob<=25 && y_rob<28 && y_rob>=26;
		return cella5;
        
	}

	public boolean amInCella4(double x_rob,double y_rob)
	{
        cella4 = x_rob>19 && x_rob<=22 && y_rob<28 && y_rob>=26;
		return cella4;
	}

	public boolean amInCella3(double x_rob,double y_rob)
	{
        cella3 = x_rob>16 && x_rob<=19 && y_rob<28 && y_rob>=26;
		return cella3;
	}

	public boolean amInCella2(double x_rob,double y_rob)
	{
        cella2 = x_rob>13 && x_rob<=16 && y_rob<28 && y_rob>=26;
		return cella2;
	}

	public boolean amInCella1(double x_rob,double y_rob)
	{
		cella1 = x_rob>=11 && x_rob<=13 && y_rob<28 && y_rob>=26;
		return cella1;
	}

	//--------------------------FINE METODI PER LOCALIZZARE IL ROBOT -------------------------------------

	
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
