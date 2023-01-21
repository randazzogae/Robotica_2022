package tempo;

import java.util.Timer;
import java.util.TimerTask;

public class Cronometro
{
    int secondsPassed=0;

    Timer myTimer = new Timer();
    TimerTask task = new TimerTask(){
        public void run()
        {
            secondsPassed++;
            System.out.println("Secondi trascorsi: " +secondsPassed);
        }
    };

    public void start()
    {
        myTimer.scheduleAtFixedRate(task,1000,1000);
    }
}