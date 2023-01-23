

import javax.swing.*;
import java.awt.*;
import	EDU.gatech.cc.is.util.Vec2;

public class Report {

	private TextArea reportAnticamere;	// Variabile che memorizza i vari report delle anticamere
	private TextArea reportAngolo;	// Variabile che memorizza i vari report delle anticamere

	private JFrame frame; 
	public Report(String S) {this.makeFrame(S);}
	
	public void setReport(String s) {
		reportAnticamere.append("\n"+s);
		
		}
	public void setTesto(Vec2[] s) {
		reportAngolo.setText("");
		for(int i=0;i<s.length;i++)
		{
			reportAngolo.append("\n" + s[i]);
		}
	}

	
	public void makeFrame(String title) {
		Font f1 = new Font("TimesRoman",Font.BOLD,22);	// Inizializza il font per il titolo
		JLabel titolo = new JLabel(title,JLabel.CENTER);// Inizializza il titolo
		titolo.setFont(f1);								// Imposta il font
		Font f2 = new Font("System",Font.BOLD,12);	// Inizializza il font per il punteggio

		reportAngolo = new TextArea("REPORT ANGOLO"); 			// Inizializza il punteggio
		reportAngolo.setEditable(false);
		reportAngolo.setBounds(10, 30, 40, 40);
		reportAngolo.setFont(f2);

		reportAnticamere = new TextArea("REPORT DELLE VARIE VISITE DEI ROBOT NELLE VARIE ANTICAMERE:"); 			// Inizializza il punteggio
		reportAnticamere.setEditable(false);
		reportAnticamere.setBounds(10, 30, 40, 40);
		reportAnticamere.setFont(f2);								// Imposta il font
		JPanel panel = new JPanel(new BorderLayout());	// Creo il pannello di visualizzazione
		panel.add(titolo,BorderLayout.NORTH);			// Inserisco il titolo nel pannello
		panel.add(reportAnticamere,BorderLayout.CENTER);			// Inserisco il punteggio nel pannello
		panel.add(reportAngolo,BorderLayout.SOUTH);			// Inserisco il punteggio nel pannello
		frame = new JFrame(title);						// Creo la finestra
		frame.getContentPane().add(panel);				// Aggiungo il pannello alla finestra
		frame.pack();									// Ridimensiono al minimo la finestra
		frame.setVisible(true);							// Rendo visibile la finestra
		frame.setResizable(true);						// Blocco i bordi della finestra
	}
	public void dispose()
	{if (frame!=null) frame.dispose();}	
}