

import javax.swing.*;
import java.awt.*;

public class Res {

	private JLabel punti;	// Variabile che memorizza il punteggio
	private JFrame frame; 
	public Res(String S) {this.makeFrame(S);}
	
	public void setScore(int p) {punti.setText(new String(""+p));}
	
	public void makeFrame(String title) {
		Font f1 = new Font("TimesRoman",Font.BOLD,22);	// Inizializza il font per il titolo
		JLabel titolo = new JLabel(title,JLabel.CENTER);// Inizializza il titolo
		titolo.setFont(f1);								// Imposta il font
		Font f2 = new Font("System",Font.BOLD,52);	// Inizializza il font per il punteggio
		punti = new JLabel("0",JLabel.CENTER);			// Inizializza il punteggio
		punti.setFont(f2);								// Imposta il font
		JPanel panel = new JPanel(new BorderLayout());	// Creo il pannello di visualizzazione
		panel.add(titolo,BorderLayout.NORTH);			// Inserisco il titolo nel pannello
		panel.add(punti,BorderLayout.CENTER);			// Inserisco il punteggio nel pannello
		frame = new JFrame(title);						// Creo la finestra
		frame.getContentPane().add(panel);				// Aggiungo il pannello alla finestra
		frame.pack();									// Ridimensiono al minimo la finestra
		frame.setVisible(true);							// Rendo visibile la finestra
		frame.setResizable(false);						// Blocco i bordi della finestra
	}
	public void dispose()
	{if (frame!=null) frame.dispose();}	
}