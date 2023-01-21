windowsize 600 300

bounds -5 15 -5 5

seed 3

time 10.0 

maxtimestep 100 

background xFFFFFF

object EDU.gatech.cc.is.simulation.BinSim  10 0 0 0.50 x0000BB x000000 4

object EDU.gatech.cc.is.simulation.BinSim  -4 3 0 0.50 x0000BB x000000 4


robot  CSAI.unipa.abstractrobot.MultiForageN150ExploreSim 
	RoboMarco 0 0 0 x000000 xFF0000 2

// obstacles
object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 10.0 1.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 3.5 3.0 0 0.40 xC0C0C0 x000000 2



// walls
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim -4 1 5 1 0.2 xC0C0C0 x000000 2

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim -4 -1 5 -1 0.2 xC0C0C0 x000000 2

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim -3 2.7 3 5 0.2 xC0C0C0 x000000 2

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 6 -1 8 1 0.2 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 7.0 -1.0 0.7 1.0 xC0C0C0 x000000 2

object EDU.gatech.cc.is.simulation.AttractorFlagSim 
	7.0 3.00 0.4 0.4 x0000FF x000000 0


