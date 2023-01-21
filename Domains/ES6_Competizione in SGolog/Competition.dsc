
//Random Attractors Generator v1.2


//======
// SIMULATION BOUNDARY
//======
//
// bounds left right bottom top
//
// In questa parte vengono settati i quattro contorni dell'ambiente.
// Questo permette di evitare che il robot possa uscire fuori dallo screen di simulazione. 

bounds -12 12 -12 12


//======
// SEED
//======
//
// seed number
//
// Questo settaggio serve a fissare il valore di un seme da usare come parametro.
// Il suo valore di default è -1

seed 3


//======
// TIME
//======
//
// time accel_rate
//
// Questo parametro stabilisce la velocità con cui la simulazione procede
// in relazione al tempo reale.  "time 0.5" farà sì che la simulazione venga
// eseguita a metà della velocità, "time 1.0" farà sì che la simulazione venga eseguita 
// a tempo reale, mentre "time 4.0" farà sì che la simulazione venga eseguita
// ad una velocità quattro volte superiore rispetto al tempo reale.
// Fissare un valore di questo parametro troppo alto farà perdere affidabilità 
// alla simulazione.

time 1.0

//======
// MAX TIME STEP
//======
//
// maxtimestep milliseconds
//
// Questo parametro fissa il tempo massimo (in millisecondi) che può intercorrere
// tra gli step della simulazione. Questa operazione eviterà che la simulazione 
// possa bloccarsi su processori particolarmente lenti.

maxtimestep 200




//======
// WINDOWSIZE
//======
//
// windowsize width height
//
// Viene fissata la dimensione della finestra in cui verrà eseguita la simulazione.

windowsize 600 600


//======
// BACKGROUND COLOR
//======
//
// background color
//
// Questa istruzione fissa il colore di sfondo della finestra come combinazione dei tre
// colori fondamentali RGB espressa in formato esadecimale come "xRRGGBB".

//background xFFFFFF
background x009000

//======
// ROBOTS
//======
//
// robot robottype controlsystem x y theta forecolor backcolor
//              visionclass
//
// Viene definito il robot che viene usato nella simulazione, la sua posizione iniziale, 
// la sua dimensione, ed il colore. In questa definizione deve essere incluso il
// nome completo della classe in cui è stato definito il sistema di controllo
// del robot stesso.

//robot  EDU.gatech.cc.is.abstractrobot.MultiForageN150Sim 
//	Zora2 -4.8 -4.0 0 x000000 xFF0000 2

  
// INSERIRE IN QUESTO PUNTO IL ROBOT ADOPERATO POSIZIONI POSSIBILI: 
// -4.8 -4.0 o 4.8 -4.0	



//======
// OBJECTS
//======
//
// object objecttype x y theta radius forecolor backcolor visionclass
//
// In questa parte vengono definiti gli oggetti presenti nell'ambiente, le loro 
// posizioni, i colori e la classe visuale a cui appartengono. Questo parametro permette
// al robot di riconscere gli oggetti e di distinguerli.


robot  CSAI.unipa.abstractrobot.MultiForageN150ExploreSim 
	RoboGologDestra  4.0 4.0 3.14 xFF0000 x000000 9

robot  CSAI.unipa.abstractrobot.MultiForageN150ExploreSim 
	RoboGologDestra -4.0 -4.0 0.0 xFF0000 x000000 9

robot  CSAI.unipa.abstractrobot.MultiForageN150ExploreSim 
	RoboGologSinistra 4.0 -4.0 3.14 x00FF00 xFFFFFF 8

robot  CSAI.unipa.abstractrobot.MultiForageN150ExploreSim 
	RoboGologSinistra -4.0 4.0 0.0 x00FF00 xFFFFFF 8




// pareti di recinzione

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
-10 -10 -10 10  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
-10 -10 10 -10  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
10 -10 10 3  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
10 10 10 3  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
10 10 -10 10  0.05 xC0C0C0 x000000 3

//Contenitori di bandierine

object EDU.gatech.cc.is.simulation.BinSim 
-9 6 0 0.5 x0000FF x000000 6


object EDU.gatech.cc.is.simulation.BinSim 
+9 6 0 0.5 x0000FF x000000 6

object EDU.gatech.cc.is.simulation.BinSim  
-9   0 0 0.5 x00FF00 x000000 5 


object EDU.gatech.cc.is.simulation.BinSim  
9   0 0 0.5 x00FF00 x000000 5

object EDU.gatech.cc.is.simulation.BinSim  
-9 -5.5 0 0.5 xFF0000 x000000 4 


object EDU.gatech.cc.is.simulation.BinSim  
+9 -5.5 0 0.5 xFF0000 x000000 4 

// muri orizzontali divisori

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
-10 8 -7 8  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
10 8 7 8  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
-10 -8 -7 -8  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
10 -8 7 -8  0.05 xC0C0C0 x000000 3

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
-10 3 -7 3  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
10 3 7 3  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
-10 -3 -7 -3  0.05 xC0C0C0 x000000 3
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 
10 -3 7 -3  0.05 xC0C0C0 x000000 3

object EDU.gatech.cc.is.simulation.ObstacleSim  0  0 0 1 xA0A0A0  x000000  7
object EDU.gatech.cc.is.simulation.ObstacleSim  0 3 0 0.40 xA0A0A0 x000000  7
object EDU.gatech.cc.is.simulation.ObstacleSim  0 -3 0 0.40 xA0A0A0 x000000  7
object EDU.gatech.cc.is.simulation.ObstacleSim  3 0 0 0.40 xA0A0A0 x000000  7
object EDU.gatech.cc.is.simulation.ObstacleSim  -3 0 0 0.40 xA0A0A0 x000000  7





//29 Attractors
object EDU.gatech.cc.is.simulation.AttractorFlagSim
0.4 -7.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-5.0 -6.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
4.6 1.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
3.7 9.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-1.4 -3.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
3.3 -5.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
3.0 6.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-3.4 1.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-2.0 6.0 0 0.4 x0000FF x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim
3.6 -3.0 0 0.4 x0000FF x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim
3.4 8.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-2.0 1.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
4.3 -2.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
4.9 7.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
3.1 -6.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
3.8 -8.0 0 0.4 x0000FF x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim
4.6 5.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-1.2 7.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-4.1 -6.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-1.0 -5.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
4.9 -3.0 0 0.4 x0000FF x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-2.5 -5.0 0 0.4 x0000FF x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-2.8 7.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-4.3 2.0 0 0.4 x0000FF x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-3.2 4.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-2.1 8.0 0 0.4 xFF0000 x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-4.2 8.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
-5.0 5.0 0 0.4 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim
4.9 -8.0 0 0.4 x00FF00 x000000 1


