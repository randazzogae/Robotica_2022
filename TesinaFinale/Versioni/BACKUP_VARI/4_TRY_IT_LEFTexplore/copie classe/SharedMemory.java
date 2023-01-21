import CSAI.unipa.communication.PointMessage;
import EDU.gatech.cc.is.communication.CommunicationException;
import EDU.gatech.cc.is.communication.LongMessage;
import EDU.gatech.cc.is.communication.StringMessage;
import EDU.gatech.cc.is.util.Vec2;

import CSAI.unipa.abstractrobot.*;

import java.util.ArrayList;
import EDU.gatech.cc.is.clay.*;
import EDU.gatech.cc.is.util.Vec2;
import EDU.gatech.cc.is.abstractrobot.*;

import EDU.gatech.cc.is.communication.*;
import CSAI.unipa.communication.*;
import CSAI.unipa.abstractrobot.*;

public class SharedMemory {

    private static SharedMemory instance = null;
    private int attractorType1;
    private int attractorType2;
    private int attractorType3;

    // Costruttore invisibile
    private SharedMemory() {}

    public static SharedMemory getInstance() {
        // Crea l'oggetto solo se NON esiste:
        if (instance == null) {
            instance = new SharedMemory();
        }
        return instance;
    }

    public int getAttractorType1() {
        return attractorType1;
    }

    public void setAttractorType1(int attractorType1) {
        this.attractorType1 = attractorType1;
    }

    public int getAttractorType2() {
        return attractorType2;
    }

    public void setAttractorType2(int attractorType2) {
        this.attractorType2 = attractorType2;
    }

    public int getAttractorType3() {
        return attractorType3;
    }

    public void setAttractorType3(int attractorType3) {
        this.attractorType3 = attractorType3;
    }


    Vec2[]  last_val = new Vec2[0];
    int[]   visual_number = new int[0];


    public Vec2[] getLast_val() {
        return last_val;
    }
    private int length=0;

    boolean flag=true;

    public boolean getFlag() {
        return flag;
    }

    public void setFlag(boolean flag) {
        this.flag = flag;
    }

    //Metodo che prende in input la posizione della bandiera da cercare e restituisce la bandiera se presente in
    // memoria codivisa oppure restituisce null se non è presente
    public Vec2 getFlag(Vec2 flag)
    {
        boolean exist=false;
        for (Vec2 v : last_val)
        {
            if((Math.abs(flag.x - v.x)<=0.5)
                    && (Math.abs(flag.y - v.y)<=0.5))
            {
                return v;
            }
        }
        return null;
    }

    public void push(Vec2 im1, int visual,int id)
    {
            if (getFlag(im1)==null)
            {
                addFlag(im1,visual);
                System.out.println("******************ROBOT "+id+" ha incrementato la memoria******************");
                visualizzaBandiere();
            }
    }



    //resituisce una stringa che specifica il colore in base ad un valore int in input
    private String getColore(int c)
    {
        switch(c) {
            case 0:
                return "blue";
            case 1:
                return "green";
            case 2:
                return "red";
        }
        return null;
    }

    //visualizza le bandiere presenti in memoria condivisa
    public void visualizzaBandiere(){
        if(last_val.size()==0)System.out.println("----Non ci sono bandiere in memoria----");
        else {
            System.out.println("----Visualizzo bandiere:----");
            for (int i = 0; i < last_val.size(); i++)
                System.out.println(last_val.get(i) + "\tcolore:\t" + getColore(visual_number.get(i)));
        }
    }
    public int[] getVisual() {
        int[] array = visual_number.stream().mapToInt(i -> i).toArray();
        return array;
    }
    /*
    public void pop_only(Vec2 im1) {
        if(getFlag(im1)!=null)
        {
            removeFlag(im1);
        }
        else
        {
            System.out.println("@@@@OH non ho cancellato nessuna bandiera perche gia lo hai fatto@@@@");
        }
    }*/
    public void pop(Vec2 im1, int id) {
        if(getFlag(im1)!=null)
        {
            System.out.println("******************ROBOT "+id+" ha decrementato la memoria******************");
            System.out.println("Estratta bandierina:\t"+im1.toString());
            removeFlag(im1);
            visualizzaBandiere();
        }
    }
    public double getr(int item) {
        double r= last_val.get(item).r;
        return(r);
    }
    public double getY(int item) {
        double y= last_val.get(item).y;
        return(y);
    }
    public double getX(int item) {
        double x= last_val.get(item).x;
        return(x);
    }
    public Vec2[] value(VisualObjectSensor visual_robot,MultiForageN150ExploreSim multi_robot,long timestamp,SimpleInterface abstract_robot) {

        //The vector with all the obstacle seen by robot
        Vec2[] im0_1 = visual_robot.getVisualObjects(timestamp, attractorType1);
        Vec2[] im0_2 = visual_robot.getVisualObjects(timestamp, attractorType2);
        Vec2[] im0_3 = visual_robot.getVisualObjects(timestamp, attractorType3);

        //the array with the visual number of the new flags
        int[] visual_new = new int[im0_1.length+im0_2.length+im0_3.length];

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

            Vec2[] im2 = last_val.toArray(new Vec2[0]);
            for(int i=0;i<4;i++)
            System.out.println("");
            System.out.println("OOOOOOOOOOOOOOOOOVISUALIZZO im2 DEBUG OOOOOOOOOOOOOOO");
            for(int i=0;i<im2.length;i++)
                System.out.println(im2);
            int[] visual_old = getVisual();
            System.out.println("OOOOOOOOOOOOOOOOOVISUALIZZO visual old DEBUG OOOOOOOOOOOOOOO");
            for(int i=0;i<visual_old.length;i++)
                System.out.println(visual_old);

            System.out.println("OOOOOOOOOOOOOOOOOVISUALIZZO bandiere OOOOOOOOOOOOOOO");
            visualizzaBandiere();
            for(int i=0;i<4;i++)
                System.out.println("");

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
            //last_val= new Vec2[im2.length+k];
            //visual_number = new int[last_val.length];
            //merge the vector arrays of new and old flags
            int pos = 0;

            //this flag says if has been modified the vector array
            boolean flag = false;

            //before insert all new obstacles
            for(int i=0; i<im3.length; i++)
            {
                if(im3[i]==1)
                {
                    last_val.add(pos, new Vec2(im1[i].x, im1[i].y));
                    visual_number.add(pos, visual_new[i]);
                    flag = true;
                    //inserisco in memoria la bandiera
                    Vec2 posizione= new Vec2(last_val.get(pos).x, last_val.get(pos).y);
                    //push(posizione,visual_number.get(pos),id);
                    try
                    {
                        //Message-------------new flag
                        LongMessage idMessage = new LongMessage();
                        idMessage.val = multi_robot.getPlayerNumber(timestamp);
                        int id = 0;
                        if(idMessage.val == 0)
                            id = 1;
                        if(idMessage.val == 1)
                            id = 0;
                        if(idMessage.val == 2)
                            id = 3;
                        if(idMessage.val == 3)
                            id = 2;
                        multi_robot.unicast(id, idMessage);

                        StringMessage message = new StringMessage();
                        message.val = "Bandiera nuova";
                        multi_robot.unicast(id, message);

                        PointMessage flagPosition = new PointMessage(last_val.get(pos).x, last_val.get(pos).y);
                        multi_robot.unicast(id, flagPosition);

                        LongMessage visualMessage = new LongMessage();
                        visualMessage.val = visual_number.get(pos);
                        multi_robot.unicast(id, visualMessage);
                    }
                    catch(CommunicationException e){}
                    pos++;
                }
            }
            //and after insert the old obstacles
            for(int i=k; i<last_val.size(); i++){
                last_val.add(i,new Vec2(im2[i-k].x, im2[i-k].y));
                visual_number.add(i,visual_old[i-k]);
            }

        }
        length=last_val.size();
        Vec2[] ret_val = new Vec2[last_val.size()];

        for(int i = 0; i<ret_val.length; i++)
            ret_val[i] = new Vec2(last_val.get(i).x, last_val.get(i).y);
        return(ret_val);
        }

    public int getLength() {
return(last_val.size());
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

    // metodo per inserire una bandiera in memoria condivisa
    public void addFlag(Vec2 im1,int visual) {
        last_val.add(im1);
        visual_number.add(visual);
    }

    // metodo per eliminare una bandiera in memoria condivisa
    public void removeFlag(Vec2 im1)
    {
        Vec2 flag=getFlag(im1);
        int visual_delete=last_val.indexOf(flag);
        visual_number.remove(visual_delete);
        last_val.remove(flag);
    }
}
