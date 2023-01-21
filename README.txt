1)VERSIONE ANTISTALLO:Attualmente l'antistallo gestisce la situazione di stallo dello stato 6. Se si dovesse 
presentare nuovamente uno stallo per lo stato 2, poichè tale stato esegue il piano e non è possibile
far scattare un trigger per portarlo ad un nuovo stato, nello gestire tale stallo possiamo prevedere semplicemente
un punto attrattore.

2)VERSIONE ANTISTALLO: RESETTARE OPPORTUNAMENTE IL VALORE DELLA variabile i (mediante antistallo.resetI() ) nel 
	takeStep() nello stato precedente a quello in cui avviene lo stallo;. 
il metodo reset riporta a 0 il valore della i
Questo perchè attualmente tale variabile viene incrementata di volta in volta nel takeStep e 
resettata soltanto quando il robot passa allo stato 6 (per gestire appunto  lo stallo in tale stato)
L'ideale sarebbe avere il valore di tale variabile a 0 ogni qualvolta il robot transita in un nuovo stato.
Se dovesse rimanere tempo, centralizzare i tempi di stallo mediante delle apposite variabili nella classe stalling

3)VERSIONE ANTISTALLO: Decommentando le informazioni di debug nel takestep rallenta

4)

5)

6)