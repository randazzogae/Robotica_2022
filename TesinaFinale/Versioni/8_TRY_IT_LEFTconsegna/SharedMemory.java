import CSAI.unipa.communication.PointMessage;
import EDU.gatech.cc.is.communication.CommunicationException;
import EDU.gatech.cc.is.communication.LongMessage;
import EDU.gatech.cc.is.communication.StringMessage;
import EDU.gatech.cc.is.util.Vec2;
import EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.util.Vec2;
import EDU.gatech.cc.is.abstractrobot.*;

import EDU.gatech.cc.is.communication.*;
import CSAI.unipa.communication.*;
import CSAI.unipa.abstractrobot.*;
import java.io.Serializable;

public class SharedMemory implements Serializable{

    private static SharedMemory instance = null;
    private static int attractorType1;
    private static int attractorType2;
    private static int attractorType3;
    protected static Report MOSTRA_REPORT =new Report("Report");

    private SharedMemory() {}

    public static SharedMemory getInstance() {
        // Crea l'oggetto solo se NON esiste:
        if (instance == null) {
            instance = new SharedMemory();
        }
        return instance;
    }

    public static Vec2[] getLast_val() {
        return last_val;
    }

    private static Vec2[]   last_val = new Vec2[0];

    public static int[] getVisual_number() {
        return visual_number;
    }

    private static int[]   visual_number = new int[0];
    private static int length=0;

    static boolean flag=true;

    public boolean getFlag() {
        return flag;
    }

    public void setFlag(boolean flag) {
        this.flag = flag;
    }



    public int getAttractorType1() {
        return attractorType1;
    }

    public void setAttractorType1(int attractorType1) {
        SharedMemory.attractorType1 = attractorType1;
    }

    public int getAttractorType2() {
        return attractorType2;
    }

    public void setAttractorType2(int attractorType2) {
        SharedMemory.attractorType2 = attractorType2;
    }

    public int getAttractorType3() {
        return attractorType3;
    }

    public static void setAttractorType3(int attractorType3) {
        SharedMemory.attractorType3 = attractorType3;
    }


    public void push(Vec2 im1, int visual,int id)
    {
        boolean canPush=true;

        try
        {
            for(int i=0; i<last_val.length; i++)
                if((Math.abs(im1.x - last_val[i].x)<=0.5)
                        && (Math.abs(im1.y - last_val[i].y)<=0.5))
                {
                    canPush = false;
                    break;
                }
            if (canPush==true)
            {
                //create the vector for the flag and the visual tpe
                Vec2[] temp= new Vec2[last_val.length+1];
                int[] temp_visual = new int[last_val.length+1];

                int pos=0;
                for(int i=0; i<last_val.length; i++)
                {
                    temp[i] = new Vec2(last_val[i].x, last_val[i].y);
                    temp_visual[i] = visual_number[i];
                    pos++;
                }
                temp[pos] = new Vec2(im1);
                temp_visual[pos] = visual;

                last_val = temp;//new Vec2[temp.length];
                visual_number = temp_visual;//new int[temp_visual.length];

                for(int i = 0; i<last_val.length; i++)
                {
                    last_val[i] = new Vec2(temp[i]);
                    visual_number[i] = temp_visual[i];
                }
                System.out.println("******************ROBOT "+id+" ha incrementato la memoria******************");
                visualizzaBandiere();
            }
        }
        catch(NullPointerException e){}
    }
    public int[] getVisual() {

        int[] ret_val = new int[visual_number.length];
        for(int i=0; i<ret_val.length; i++)
            ret_val[i] = visual_number[i];
        return(ret_val);

    }
    /*
    public void pop_only(Vec2 im1) {

        try
        {
            int count=0;
            for(int i=0; i<last_val.length; i++)
                if((Math.abs(im1.x - last_val[i].x)<=0.5)    //modificato!!!!!era 0.3 modificato
                        && (Math.abs(im1.y - last_val[i].y)<=0.5))   //anche  in pop().
                {
                    count++;
                    flag = true;
                }
            if (flag)
            {
                int pos = 0;
                Vec2[] temp = new Vec2[last_val.length-count];
                for(int i=0; i<last_val.length; i++)
                    if((Math.abs(im1.x - last_val[i].x)>0.5) ||
                            (Math.abs(im1.y - last_val[i].y)>0.5))
                    {
                        temp[pos++] = new Vec2(last_val[i].x, last_val[i].y);
                    }
                    else
                    {
                        System.out.println("******************ROBOT "+id+" ha decrementato la memoria******************");
                        System.out.println("Estratta bandierina:\t"+last_val[i].toString());
                    }
                //else
                //    System.out.println("Tolta bandierina "+last_val[i].toString());
                last_val = temp;
            }
        }
        catch(NullPointerException e){System.out.println("Exception in NodeMemory.pop_only");}

    }*/
    public void pop(Vec2 im1, int id)  {
        flag=false;
        int count=0;
        for(int i=0; i<last_val.length; i++)
            if((Math.abs(im1.x - last_val[i].x)<=0.5)
                    && (Math.abs(im1.y - last_val[i].y)<=0.5))
            {
                count++;
                flag = true;

            }
        if (flag)
        {
            int pos = 0;
            Vec2[] temp = new Vec2[last_val.length-count];
            for(int i=0; i<last_val.length; i++)
                if((Math.abs(im1.x - last_val[i].x)>0.5) ||
                        (Math.abs(im1.y - last_val[i].y)>0.5))
                {
                    temp[pos++] = new Vec2(last_val[i].x, last_val[i].y);
                }
            else
                {
                    System.out.println("******************ROBOT "+id+" ha decrementato la memoria******************");
                    System.out.println("Estratta bandierina:\t"+"("+last_val[i].x+" , "+ last_val[i].y+")");
                }
            last_val = temp;
            visualizzaBandiere();
        }
    }
    public void visualizzaBandiere(){
        MOSTRA_REPORT.setTesto(last_val);
        if(last_val.length==0)System.out.println("");
        else {
            System.out.println("----Visualizzo bandiere:----");
            for (int i = 0; i < last_val.length; i++)
                System.out.println("("+last_val[i].x+" , "+last_val[i].y+")");
        }
    }
    public void visualizzaBandiere(String mes){
        MOSTRA_REPORT.setTesto(last_val);
        if(last_val.length!=0)
        {
            System.out.println(mes);
            System.out.println("----Visualizzo bandiere:----");
            for (int i = 0; i < last_val.length; i++)
                System.out.println("("+last_val[i].x+" , "+last_val[i].y+")");
        }
    }

    public double getr(int item) {

        return(last_val[item].r);
    }
    public double getY(int item) {

        return(last_val[item].y);
    }
    public double getX(int item) {

        return(last_val[item].x);
    }
    public Vec2[] value(VisualObjectSensor visual_robot,MultiForageN150ExploreSim multi_robot,long timestamp,SimpleInterface abstract_robot) {

        //The vector with all the obstacle seen by robot
        Vec2[] im0_1 = visual_robot.getVisualObjects(timestamp, attractorType1);
        Vec2[] im0_2 = visual_robot.getVisualObjects(timestamp, attractorType2);
        Vec2[] im0_3 = visual_robot.getVisualObjects(timestamp, attractorType3);
        int[] visual_new = new int[im0_1.length+im0_2.length+im0_3.length];

        int lunghezza1=last_val.length;
        //the array of the new flags merging the three
        Vec2[] im0 = new Vec2[im0_1.length+im0_2.length+im0_3.length];
        int count = 0;
        for(int i=0; i<im0_1.length; i++)
        {
            im0[count] = new Vec2(im0_1[i].x, im0_1[i].y);
            visual_new[count++] = attractorType1;
        }
        for(int i=0; i<im0_2.length; i++)
        {
            im0[count] = new Vec2(im0_2[i].x, im0_2[i].y);
            visual_new[count++] = attractorType2;
        }
        for(int i=0; i<im0_3.length; i++)
        {
            im0[count] = new Vec2(im0_3[i].x, im0_3[i].y);
            visual_new[count++] = attractorType3;
        }
        //the position of the robot
        Vec2 position = abstract_robot.getPosition(timestamp);

        if(im0.length !=0)
        {
            //make it global
            for(int i=0; i<im0.length; i++)
                im0[i].add(position);
            //im1 is global and is rounded
            Vec2[] im1 = roundVector(im0);
            //im2 is global
            Vec2[] im2 = last_val;
            int[] visual_old = visual_number;
            //is a temp vector array in which save only new obstacles and after merge with old
            int k = im1.length;
            //Initialization of im3, is 1 if is a new obstacle, -1 if is old
            int[] im3 = new int[im1.length];
            for(int i=0; i<im3.length; i++)
                im3[i] = 1;
            //control the objects and decide what are to be saved
            for(int i=0; i<im1.length; i++)
            {
                for(int j=0; j<im2.length; j++)
                {
                    //if condition certainly is a flag that has been moved by robot
                    if((Math.abs(im1[i].x-im2[j].x)<=0.5) &&
                            (Math.abs(im1[i].y-im2[j].y)<=0.5))
                    {
                        //-1 because i must not insert it as a new flag
                        im3[i] = -1;
                        //substitute the old flag with this that's the same moved
                        im2[j] = new Vec2(im1[i].x, im1[i].y);
                        k--;
                        break;
                    }
                }
            }
            //create the vector for the flag and the visual tpe
            last_val= new Vec2[im2.length+k];
            visual_number = new int[last_val.length];
            //merge the vector arrays of new and old flags
            int pos = 0;
            //this flag says if has been modified the vector array
            //before insert all new obstacles
            for(int i=0; i<im3.length; i++)
            {
                if(im3[i]==1)
                {
                    last_val[pos] = new Vec2(im1[i].x, im1[i].y);
                    visual_number[pos] = visual_new[i];
                    flag = true;
                    Vec2 posizione= new Vec2(last_val[pos].x, last_val[pos].y);
                    pos++;
                }
            }
            //and after insert the old obstacles
            for(int i=k; i<last_val.length; i++){
                last_val[i] = new Vec2(im2[i-k].x, im2[i-k].y);
                visual_number[i] = visual_old[i-k];
            }
        }
        length=last_val.length;
        Vec2[] ret_val = new Vec2[last_val.length];

        for(int i = 0; i<ret_val.length; i++)
            ret_val[i] = new Vec2(last_val[i].x, last_val[i].y);
            if(lunghezza1!=length)visualizzaBandiere("*****************ROBOT\t"+multi_robot.getPlayerNumber(timestamp)+"\tha aggiornato la memoria******************");
        return(ret_val);
        }
    public int getLength() {

        return(length);
    }
    public void setLength(int length) {

        this.length=length;
    }
    public Vec2[] roundVector(Vec2[] im1) {

        Vec2[] ret_val = new Vec2[im1.length];

        for(int i=0; i<ret_val.length; i++)
            ret_val[i] = new Vec2(roundDecimal(im1[i].x), roundDecimal(im1[i].y));

        return(ret_val);
    }
    public double roundDecimal(double val) {

        double temp = val * 10;
        long round = Math.round(temp);
        temp = (double)round / 10;
        return (temp);

    }
}

