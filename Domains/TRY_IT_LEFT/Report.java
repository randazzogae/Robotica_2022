

import javax.swing.*;
import java.awt.*;

public class Report {

	private TextArea reportAnticamere;	// Variabile che memorizza i vari report delle anticamere

	private JFrame frame; 
	public Report(String S) {this.makeFrame(S);}
	
	public void setReport(String s) { 
		StringBuilder sb = new StringBuilder(reportAnticamere.getText());
		sb.append(System.getProperty("line.separator")); //per andare a capo
		sb.append(s);

		reportAnticamere.setText(sb.toString());
		
		}

	
	public void makeFrame(String title) {
		Font f1 = new Font("TimesRoman",Font.BOLD,22);	// Inizializza il font per il titolo
		JLabel titolo = new JLabel(title,JLabel.CENTER);// Inizializza il titolo
		titolo.setFont(f1);								// Imposta il font
		Font f2 = new Font("System",Font.BOLD,12);	// Inizializza il font per il punteggio
		
		reportAnticamere = new TextArea("REPORT DELLE VARIE VISITE DEI ROBOT NELLE VARIE ANTICAMERE:"); 			// Inizializza il punteggio
		reportAnticamere.setBounds(10, 30, 40, 40);
		reportAnticamere.setFont(f2);								// Imposta il font
		JPanel panel = new JPanel(new BorderLayout());	// Creo il pannello di visualizzazione
		panel.add(titolo,BorderLayout.NORTH);			// Inserisco il titolo nel pannello
		panel.add(reportAnticamere,BorderLayout.CENTER);			// Inserisco il punteggio nel pannello
		frame = new JFrame(title);						// Creo la finestra
		frame.getContentPane().add(panel);				// Aggiungo il pannello alla finestra
		frame.pack();									// Ridimensiono al minimo la finestra
		frame.setVisible(true);							// Rendo visibile la finestra
		frame.setResizable(true);						// Blocco i bordi della finestra
	}
	public void dispose()
	{if (frame!=null) frame.dispose();}	
}