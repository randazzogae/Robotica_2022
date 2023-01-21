import java.util.Timer;
import java.util.TimerTask;

import tempo.Cronometro;

public class ProvaCronometro
{
    public static void main(String args[])
    {
        Cronometro cron = new Cronometro();
        cron.start();
    }
}