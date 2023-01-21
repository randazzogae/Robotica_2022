windowsize 600 300

bounds -5 15 -5 5

seed 3

view_robot_state on

time 10.0 

maxtimestep 100 

background xFFFFFF

object EDU.gatech.cc.is.simulation.BinSim  10 0 0 0.50 x0000BB x000000 4

object EDU.gatech.cc.is.simulation.BinSim  0 3 0 0.50 x0000BB x000000 4


robot  EDU.gatech.cc.is.abstractrobot.MultiForageN150Sim Oby1Kenoby_2 0 0.50 0 x000000 xFF0000 2

robot  EDU.gatech.cc.is.abstractrobot.MultiForageN150Sim Oby1Kenoby_1 -2 0 1  x000000 xFF0000 2



// obstacles
object EDU.gatech.cc.is.simulation.ObstacleSim 10.0 1.0 0 0.30 xC0C0C0 x000000 2

object EDU.gatech.cc.is.simulation.ObstacleSim 3.0 3.0 0 0.30 xC0C0C0 x000000 2

// walls
linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim -4 1 5 1 0.2 xC0C0C0 x000000 2

linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim -4 -1 5 -1 0.2 xC0C0C0 x000000 2

//linearobject EDU.cmu.cs.coral.simulation.LinearObstacleSim 6 -1 8 1 0.2 xC0C0C0 x000000 2

object EDU.cmu.cs.coral.simulation.PolygonObstacleSim 7.0 -1.0 0.7 1.0 xC0C0C0 x000000 2

object EDU.gatech.cc.is.simulation.AttractorFlagSim 
	7.0 3.00 0.5 0.4 x0000FF x000000 0

object EDU.gatech.cc.is.simulation.AttractorFlagSim 
	7.3 3.00 0.5 0.4 x0000FF x000000 0


