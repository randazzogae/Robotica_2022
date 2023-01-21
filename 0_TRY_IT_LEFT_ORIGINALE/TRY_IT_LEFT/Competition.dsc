//*************************************************
// mappa.dsc
// File di descrizione dell'ambiente in cui si muovono i vari robots
//*************************************************

//margini: min_x, max_x, min_y, max_y

bounds 0.0 44.0 0.0 30.0

//seme utilizzato nella determinazione random dei movimenti

seed 3

// time accel_rate: one times realtime
time 1.0  

// maxtimestep milliseconds
maxtimestep 300

//dimensione finestra di simulazione
windowsize 600 400

// background color
background xFFFFFF


robot CSAI.unipa.abstractrobot.MultiForageN150ExploreSim TRY_IT_2_LEFT 32.0 5 0 x000000 xFF0000 6 
robot CSAI.unipa.abstractrobot.MultiForageN150ExploreSim TRY_IT_1_LEFT 32.0 23 0 x000000 xFF0000 6 

//perimetro esterno
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 0 0 44 0 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 0 0 0 28 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 44 0 44 28 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 44 28 0 28 0.1 xC0C0C0 x000000 9
//fine perimetro

/*
//lato sinistro
//linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 5 0 5 6 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 0 10 6 10 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 8 0 8 2 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 11 0 11 4 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 8 4 8 6 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 11 6 11 12 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 6 10 8 6 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 8 6 11 10 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 6 10 6 12 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 0 18 6 18 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 6 18 8 22 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 8 22 11 18 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 6 18 6 16 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 11 16 11 22 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 8 22 8 24 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 11 24 11 28 0.1 xC0C0C0 x000000 9
//linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 5 22 5 28 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 8 28 8 26 0.1 xC0C0C0 x000000 9

//lato destro
//linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 39 0 39 6 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 44 10 38 10 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 36 0 36 2 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 33 0 33 4 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 36 4 36 6 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 33 6 33 12 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 38 10 36 6 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 36 6 33 10 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 38 10 38 12 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 44 18 38 18 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 38 18 36 22 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 36 22 33 18 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 38 18 38 16 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 33 16 33 22 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 36 22 36 24 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 33 24 33 28 0.1 xC0C0C0 x000000 9
//linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 39 22 39 28 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 36 28 36 26 0.1 xC0C0C0 x000000 9
*/
//centro
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 21 12 25 12 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 19 18 23 18 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 25 12 25 22 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 19 18 19 8 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 19 8 27 8 0.1 xC0C0C0 x000000 9
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 25 22 17 22 0.1 xC0C0C0 x000000 9


//flags
object EDU.gatech.cc.is.simulation.AttractorFlagSim 26 25 0.8 0.8 x0000FF x000000 0
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 12 12 0.8 0.8 x0000FF x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim 15 6 0.8 0.8 x0000FF x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim 30 6 0.8 0.8 xFF0000 x000000 2
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 30 15 0.8 0.8 xFF0000 x000000 2
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 10 7 0.8 0.8 x0000FF x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim 21 2 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 15 7 0.8 0.8 x0000FF x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim 21 14 0.8 0.8 x0000FF x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim 28 24 0.8 0.8 x0000FF x000000 0
object EDU.gatech.cc.is.simulation.AttractorFlagSim 26 25 0.8 0.8 x0000FF x000000 0
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 11.5 14 0.8 0.8 x0000FF x000000 0
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 12 3 0.8 0.8 x0000FF x000000 0
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 13 24 0.8 0.8 x0000FF x000000 0

//object EDU.gatech.cc.is.simulation.AttractorFlagSim 22 20 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 10 18 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 18 14 0.8 0.8 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim 27 26 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 17 13.5 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 10 17 0.8 0.8 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim 22 26 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 10 17 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 10 19 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 13 6.5 0.8 0.8 x00FF00 x000000 1
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 15 7.8 0.8 0.8 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim 34 3.3 0.8 0.8 x00FF00 x000000 1
object EDU.gatech.cc.is.simulation.AttractorFlagSim 34 4.8 0.8 0.8 x00FF00 x000000 1



object EDU.gatech.cc.is.simulation.AttractorFlagSim 21 2 0.8 0.8 xFF0000 x000000 2
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 29 15 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 12 4 0.8 0.8 xFF0000 x000000 2
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 28 5 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 26 24 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 10 11.5 0.8 0.8 xFF0000 x000000 2
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 28 4 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 30 24 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 13 26 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 15 10 0.8 0.8 xFF0000 x000000 2
//object EDU.gatech.cc.is.simulation.AttractorFlagSim 16 8 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 15 6 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 34 18 0.8 0.8 xFF0000 x000000 2
object EDU.gatech.cc.is.simulation.AttractorFlagSim 35 19 0.8 0.8 xFF0000 x000000 2




// bin sinistra
object EDU.gatech.cc.is.simulation.BinSim  2.5 5 0 0.50 x0000FF x000000 8
object EDU.gatech.cc.is.simulation.BinSim  2.5 14 0 0.50 x00FF00 x000000 8
object EDU.gatech.cc.is.simulation.BinSim  2.5 23 0 0.50 xFF0000 x000000 8

// bin destra
object EDU.gatech.cc.is.simulation.BinSim  41.5 5 0 0.50 x0000FF x000000 8
object EDU.gatech.cc.is.simulation.BinSim  41.5 14 0 0.50 x00FF00 x000000 8
object EDU.gatech.cc.is.simulation.BinSim  41.5 23 0 0.50 xFF0000 x000000 8
