
import EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.util.Vec2;


/**
 *  Permette la localizzazione del robot e delle bandiere all'interno delle stanze
 * 
 */

public class RoomLocation {
	
	    private double x_rob;
	    private double y_rob;		
		
		private NodeVec2 position;

public RoomLocation(NodeVec2 pos){
		position=pos;		
}
	


public void Value(long timestamp){
	x_rob=position.Value(timestamp).x;
	y_rob=position.Value(timestamp).y;
			
}

public boolean in_room(){ //ritorna true se si trova in una delle camere (o destra o sinistra)
	
	return in_red_Right() || in_green_Right() || in_blue_Right() || in_red_Left() || in_green_Left() || in_blue_Left(); 
	
	}
public boolean closed_To_Room_Right(){
	
	return closed_red_Right() || closed_green_Right() || closed_blue_Right(); 
	
	}
	
public boolean in_red_Left(){
	return x_rob>7 && x_rob<11 && y_rob>18 && y_rob<28 && (y_rob>((-4*x_rob+98)/3));	
	
}	

public boolean in_green_Left(){
	return x_rob>5 && x_rob<11 &&(y_rob<((-4*x_rob+98)/3))&&((y_rob<(6+2*x_rob)))&&(y_rob>((4*x_rob-14)/3))&&(y_rob>(-2*x_rob+22));
	
}

public boolean in_blue_Left(){
	return x_rob>7 && x_rob<11 && y_rob>0 && y_rob<10&& y_rob<((4*x_rob-14)/3);
	
}

	
public boolean in_red_Right(){
	return x_rob>33 && x_rob<37 && y_rob>18 && y_rob<28 && (y_rob>((4*x_rob-78)/3));	
	
}	

public boolean in_green_Right(){
	return x_rob>33 && x_rob<39 && (y_rob<((4/3)*x_rob-16))&&((y_rob<(94-2*x_rob)))&&(y_rob>((-4*x_rob+162)/3))&&(y_rob>(2*x_rob-66));
	
}

public boolean in_blue_Right(){
	return x_rob>33 && x_rob<37 && y_rob>0 && y_rob<10&& y_rob<((-4*x_rob+162)/3);
	
}



public boolean in_lab_middle(){
	return x_rob>19&&x_rob<25&&y_rob>12&&y_rob<18;
	
}

public boolean in_lab_top(){
	return x_rob>17&&x_rob<25&&y_rob>18&&y_rob<22;
	
}

public boolean in_lab_down(){
	return x_rob>19&&x_rob<27&&y_rob>8&&y_rob<12;
	
}

public boolean in_lab(){
	return in_lab_middle()||in_lab_top()||in_lab_down();
}

public boolean use_lab_right(){
	return (x_rob>25&& x_rob<33 && y_rob>8 &&y_rob<24);
}	

public boolean use_lab_right_redBlue() {
	return (x_rob>25&& x_rob<27.5 && y_rob>8 &&y_rob<22);
}	

public boolean use_lab_left(){
	return (x_rob>11&& x_rob<19 && y_rob>7.5 &&y_rob<24);
}

public boolean use_lab_left_redBlue(){
	return (x_rob>16.5&& x_rob<19 && y_rob>8 &&y_rob<22);
}
	
public boolean closed_red_Left(){
	return x_rob>11 && x_rob<14 && y_rob>18.5 && y_rob<28;	
	
}	

public boolean closed_green_Left(){
	return x_rob>11 && x_rob<14 && y_rob>10 && y_rob<18;
	
}	

public boolean closed_blue_Left(){
	return x_rob>11 && x_rob<14 && y_rob>0 && y_rob<9.5;
	
}


public boolean closed_red_Right(){
	return x_rob>30 && x_rob<33 && y_rob>18.5 && y_rob<28;
}	

public boolean closed_green_Right(){
	return x_rob>30 && x_rob<33 && y_rob>10 && y_rob<18;
	
}	

public boolean closed_blue_Right(){
	return x_rob>30 && x_rob<33 && y_rob>0 && y_rob<9.5;
}

public boolean closed_red(){
	return closed_red_Left() || closed_red_Right();
}	

public boolean closed_green(){
	return closed_green_Left() || closed_green_Right();
}	

public boolean closed_blue(){
	return closed_blue_Left() || closed_blue_Right();
}

public boolean closed_lab_(){
	return closed_lab_Left() || closed_lab_Right();
	
}	

public boolean closed_lab_Left(){
	return x_rob>15&&x_rob<19&&y_rob>18&&y_rob<22;
	
}	

public boolean closed_lab_Right(){
	return x_rob>25&&x_rob<29&&y_rob>8&&y_rob<12;
	
}	

public boolean out_of_bounds(){
	return (x_rob>36&&(y_rob>24||y_rob<4))||(x_rob>38&&y_rob<16.5&&y_rob>11.5)||(x_rob<8&&(y_rob<4||y_rob>24))||(x_rob<6&&y_rob<16.5&&y_rob>11.5);

}


public boolean flag_in_red_Right(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_red=(x_flag>33 && x_flag<36 && y_flag>20 && y_flag<28&& (y_flag>((4*x_flag-78)/3)));					
	return  flag_red;
		
}

public boolean flag_in_green_Right(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_green=(x_flag>33 && x_flag<38 && y_flag>10 && y_flag<20&& (y_flag<((4*x_flag-78)/3))&&((y_flag<(94-2*x_flag)))&&(y_flag>((-4*x_flag+162)/3))&&(y_flag>(2*x_flag-66)));
	return  flag_green;
		
}


public boolean flag_in_blue_Right(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_blue=(x_flag>33 && x_flag<36 && y_flag>0 && y_flag<10&& y_flag<((-4*x_flag+162)/3));
	return  flag_blue;
		
}

public boolean flag_in_red_Left(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_red=x_flag>8 && x_flag<11 && y_flag>18 && y_flag<28 && (y_flag>((-4*x_flag+98)/3));
	return  flag_red;
		
}

public boolean flag_in_green_Left(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_green=  x_flag>6 && x_flag<11 &&(y_flag<((-4*x_flag+98)/3))&&((y_flag<(6+2*x_flag)))&&(y_flag>((4*x_flag-14)/3))&&(y_flag>(-2*x_flag+22));
	return  flag_green;
		
}


public boolean flag_in_blue_Left(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_blue=x_flag>8 && x_flag<11 && y_flag>0 && y_flag<10&& y_flag<((4*x_flag-14)/3);
	return  flag_blue;
		
}


public boolean flag_in_lab_centre(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_lab=x_flag>19&&x_flag<25&&y_flag>12&&y_flag<18;
	return  flag_lab;
		
}

public boolean flag_in_lab_top(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_lab=x_flag>19&&x_flag<25&&y_flag>18&&y_flag<22;
	return  flag_lab;
		
}

public boolean flag_in_lab_down(Vec2 flag){
	double x_flag=flag.x;
	double y_flag=flag.y;
	boolean flag_lab=x_flag>19&&x_flag<25&&y_flag>8&&y_flag<12;
	return  flag_lab;
		
}

public boolean flag_in_lab(Vec2 flag){
	return flag_in_lab_down(flag) || flag_in_lab_top(flag) || flag_in_lab_centre(flag);
}
	
}
	
	