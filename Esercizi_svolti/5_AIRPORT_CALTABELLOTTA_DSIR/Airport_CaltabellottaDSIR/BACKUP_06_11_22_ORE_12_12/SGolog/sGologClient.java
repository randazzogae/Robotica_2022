/*
 * sGologClient.java
 *
 * Created on 19 dicembre 2002, 18.21
 */

package CSAI.unipa.SGolog;

import com.parctechnologies.eclipse.*;
import java.io.*;
import java.awt.event.*;
import java.awt.*;
import java.util.StringTokenizer;
import javax.swing.text.*;

/**
 *
 * @author Marco Di Stefano & Carmelo Scozzola
 */
public class sGologClient {

    private EclipseEngineOptions        eclipseEngineOptions;
  //  private OutOfProcessEclipse         eclipse;
    private EclipseEngine         eclipse;
    private List                        plan = new List();
    private File                        file;
    private static final String         fileSuffix = "pl";
    
    /** Creates a new instance of GologEditor */
    public sGologClient(File dataBase) {
        //---Embedding golog
        eclipseEngineOptions = new EclipseEngineOptions();
        eclipseEngineOptions.setUseQueues(false);
        try {
              //eclipse = new OutOfProcessEclipse(eclipseEngineOptions);
		eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);
	    if (eclipse==null) System.out.println("eclipse mai stato creato");
       }
	
        catch(Exception e) {
	System.out.println(e);	
	//eclipse = EmbeddedEclipse.getInstance();
	//eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);
        }
	//	eclipse = EmbeddedEclipse.getInstance(eclipseEngineOptions);
        // Compile the eclipse program.
        //   eclipse = new OutOfProcessEclipse(eclipseEngineOptions);
        this.consultFile(dataBase);
    }

    /** compile a file **/
    public void consultFile(File f) {
        try {
	    if (f==null) System.out.println("f è null");
	    if (eclipse==null) System.out.println("eclipse è null");
            eclipse.compile(f);
        }
        catch(IOException ioe) {
            System.out.println("File not found");
        }
        catch(EclipseException ee) {
            System.out.println("Eclipse exception");
        }
    }    
    
    /** add a fact using the assert command **/
    public void add(String fact) {
        try{
            eclipse.rpc("assert("+fact+")");
        }
        catch(EclipseException e)
        {
            System.out.println("EclipseException: ");
        }
        catch(IOException ioe)
        {
            System.out.println("IOException: ");
        }
    }
    
    /** delete a fact using retract command **/
    public void delete(String fact) {
        try{
            eclipse.rpc("retract("+fact+")");
        }
        catch(EclipseException e)
        {
            System.out.println("EclipseException: ");
        }
        catch(IOException ioe)
        {
            System.out.println("IOException: ");
        }
    }
    
    /** invoke the result of a query **/
    public List invoke(String query) {
        try {
            
            plan = new List();
            
            CompoundTerm result =eclipse.rpc(query);
                   
            //The third argument of the query is the goal
            Object goal = result.arg(3);
            String string = goal.toString();
            int k=0;
            StringTokenizer tokenizer = new StringTokenizer(string,"[.=] ",true);
            while (tokenizer.hasMoreTokens()) {
                String tmp = tokenizer.nextToken();
                if(tmp.startsWith("com")) {
                    for(int i=0;i<5;i++)
                        tokenizer.nextElement();
                }
                /** if function has parameter add the key word #param#
                 * followed by the number of parameters **/
                else if(tmp.equals("arity")){
                    tmp = tokenizer.nextToken();
                    tmp = tokenizer.nextToken();
                    plan.add("#param#");
                    plan.add(tmp);
                }
                /** these are all terms to be filtered **/
                else if(
                    tmp.startsWith("Compound")||
                    tmp.equals("functor")||
                    tmp.equals("with")||
                    tmp.startsWith("arg")||
                    tmp.startsWith("Atom")||
                    tmp.equals(".")||
                    tmp.equals("=")||
                    tmp.equals(" ")
                ) continue;
                /** add all the other terms to the plan **/
                else if(tmp.equals("[")){ 
                    plan.add(tmp);
                }
                else if(tmp.equals("]")){
                    plan.add(tmp);
                }
                else{
                    plan.add(tmp);
                }
            }
        }catch(Exception e){System.out.println("Exception "+e);}
        return plan;
    }
    
}

