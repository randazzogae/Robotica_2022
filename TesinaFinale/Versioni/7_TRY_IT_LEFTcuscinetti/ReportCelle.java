

import javax.swing.*;
import java.awt.*;

public class ReportCelle {
	private JFrame frame;
	JButton[] bottoni=new JButton[70];
	public ReportCelle(String S) {this.makeFrame(S);}
	public void setReportCella(int c) {
		bottoni[c].setBackground(Color.RED);
		}

	
	public void makeFrame(String title) {

		JPanel panel = new JPanel(new BorderLayout());	// Creo il pannello di visualizzazione

		JPanel panelFinale= new JPanel(new GridLayout(13,8));

		for(int i=0;i<70;i++)
		{
			bottoni[i] = new JButton("" + (i + 1));
			bottoni[i].setBackground(Color.GREEN);
		}
		int k=0;//variabile per settare la cella

		for(int i=0;i<16;i++) {
			panelFinale.add(bottoni[k]);
			k++;
		}
		//k=15 e corrisponde a cella 16
		int kcelledestra=35;
		//setto corridio D
		for(int i=16;i<=23;i++)
			panelFinale.add(new Button("D"));

		for(int i=24;i<=79;i++) {

			if (i <= 26) {
				panelFinale.add(bottoni[k]);
				k++;
			} else if (i >= 27 && i <= 28) panelFinale.add(new Button("L"));
			else if (i >= 29 && i <= 31) {
				panelFinale.add(bottoni[kcelledestra]);
				kcelledestra++;

			} else if (i >= 32 && i <= 34) {
				panelFinale.add(bottoni[k]);
				k++;
			} else if (i >= 35 && i <= 36) panelFinale.add(new Button("L"));
			else if (i >= 37 && i <= 39) {
				panelFinale.add(bottoni[kcelledestra]);
				kcelledestra++;
			} else if (i >= 40 && i <= 42) {
				panelFinale.add(bottoni[k]);
				k++;
			} else if (i >= 43 && i <= 44) panelFinale.add(new Button("L"));
			else if (i >= 45 && i <= 47) {
				panelFinale.add(bottoni[kcelledestra]);
				kcelledestra++;
			}
			//corridoio C e A
			else if (i >= 48 && i <= 50) panelFinale.add(new Button("C"));
			else if (i >= 51 && i <= 52) panelFinale.add(new Button("L"));
			else if (i >= 53 && i <= 55) panelFinale.add(new Button("A"));
			else if (i >= 56 && i <= 58) {
				panelFinale.add(bottoni[k]);
				k++;
			} else if (i >= 59 && i <= 60) panelFinale.add(new Button("L"));
			else if (i >= 61 && i <= 63) {
				panelFinale.add(bottoni[kcelledestra]);
				kcelledestra++;
			} else if (i >= 64 && i <= 66) {
				panelFinale.add(bottoni[k]);
				k++;
			} else if (i >= 67 && i <= 68) panelFinale.add(new Button("L"));
			else if (i >= 69 && i <= 71) {
				panelFinale.add(bottoni[kcelledestra]);
				kcelledestra++;
			} else if (i >= 72 && i <= 75) {
				panelFinale.add(bottoni[k]);
				k++;
			} else if (i >= 76 && i <= 79) {
				panelFinale.add(bottoni[kcelledestra]);
				kcelledestra++;
			}
		}
		k=54;
		//corridoio B
		for(int i=80;i<=87;i++) {
			panelFinale.add(new Button("B"));
		}
		for(int i=88;i<=103;i++) {
			panelFinale.add(bottoni[k]);
			k++;
		}


		panel.add(panelFinale,BorderLayout.CENTER);

		frame = new JFrame(title);
		frame.getContentPane().add(panel);
		frame.pack();
		frame.setVisible(true);
		frame.setResizable(true);
	}
	public void dispose()
	{if (frame!=null) frame.dispose();}	
}