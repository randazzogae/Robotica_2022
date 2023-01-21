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
	
	
	
	public Location(){ }    //costruttore
	
	
	
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
