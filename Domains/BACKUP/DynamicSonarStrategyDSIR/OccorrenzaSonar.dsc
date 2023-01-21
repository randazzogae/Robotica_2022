windowsize 600 300

bounds -5 15 -5 5

seed 3

time 10.0 

maxtimestep 100 

background xFFFFFF

object EDU.gatech.cc.is.simulation.BinSim  10 0 0 0.50 x0000BB x000000 4

object EDU.gatech.cc.is.simulation.BinSim  0 3 0 0.50 x0000BB x000000 4


robot  CSAI.unipa.abstractrobot.MultiForageN150ExploreSim 
	OccorrenzaSonar 0 0 0 x000000 xFF0000 2

// obstacles
object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 9.0 0.3 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 4.5 4.5 0 0.30 xC0C0C0 x000000 2
 
object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 7.5 4.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 11.0 0.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 10.0 -1.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 1.0 2.8 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim -1.0 3.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim -1.3 4.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 0.0 2.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 10.0 3.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 12.0 2.0 0 0.30 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 6.0 2.0 0 0.30 xC0C0C0 x000000 2

// walls
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim -4 1 3 1 0.2 xC0C0C0 x000000 2

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim -4 -1 5 -1 0.2 xC0C0C0 x000000 2

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 6 -1 8 0.8 0.2 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 7.0 -1.0 0.7 1.0 xC0C0C0 x000000 2

object EDU.gatech.cc.is.simulation.AttractorFlagSim 7.0 3.00 0.4 0.4 x0000FF x000000 0
