# This is an implementation of Occupancy Grid Mapping as Presented
# in Chapter 9 of "Probabilistic Robotics" By Sebastian Thrun et al.
# In particular, this is an implementation of Table 9.1 and 9.2


import scipy.io
import scipy.stats
import numpy as np
import matplotlib.pyplot as plt
from tqdm import tqdm

class Map():
    
    #tramite questa funzione definiamo le impostazioni iniziali della mappa(dimenzioni della mappa e numero di celle per riga xsize e per colonna ysize)
    #e settiamo a 0 tutti valori randomici binari di ogni cella
    #settiamo alpha= spessore ostacoli 
    #settiamo beta= larghezza del raggio laser
    #settiamo la massima lettura del laser(pesumo sia la distanza massima)
    #precalcoliamo le cordinate di tutte le celle per velocizzare
    #settiamo le percentuali(probabilità logaritmica, log-odd )  che rendono una cella libera o occupata
    def __init__(self, xsize, ysize, grid_size):
        self.xsize = xsize+2 # Add extra cells for the borders
        self.ysize = ysize+2
        self.grid_size = grid_size # save this off for future use
        self.log_prob_map = np.zeros((self.xsize, self.ysize)) # set all to zero

        self.alpha = 1.0 # The assumed thickness of obstacles
        self.beta = 5.0*np.pi/180.0 # The assumed width of the laser beam
        self.z_max = 150.0 # The max reading from the laser

        # Pre-allocate the x and y positions of all grid positions into a 3D tensor
        # (pre-allocation = faster)
        self.grid_position_m = np.array([np.tile(np.arange(0, self.xsize*self.grid_size, self.grid_size)[:,None], (1, self.ysize)),
                                         np.tile(np.arange(0, self.ysize*self.grid_size, self.grid_size)[:,None].T, (self.xsize, 1))])

        # Log-Probabilities to add or remove from the map 
        self.l_occ = np.log(0.65/0.35)
        self.l_free = np.log(0.35/0.65)

#passiamo a questa funzione l' oggetto self, la posizione del robot e il dato sensoriale
#prendiamo le coordinate di tutte le celle precedentemente settate  costruiiamo una matrice di coordinate x e una di coordinate y
#costriamo una matrice di tutti iraggi dal robot alla cella
#per ogni dato sensoriale ricevuto(ogni raggio laser) calcola le celle libere e occupate con l'algoritmo di inverse range sensor model  e aggiorna la mappa 
    def update_map(self, pose, z):

        dx = self.grid_position_m.copy() # A tensor of coordinates of all cells
        dx[0, :, :] -= pose[0] # A matrix of all the x coordinates of the cell
        dx[1, :, :] -= pose[1] # A matrix of all the y coordinates of the cell
        theta_to_grid = np.arctan2(dx[1, :, :], dx[0, :, :]) - pose[2] # matrix of all bearings from robot to cell

        # Wrap to +pi / - pi
        # conversione angolo in radianti
        theta_to_grid[theta_to_grid > np.pi] -= 2. * np.pi
        theta_to_grid[theta_to_grid < -np.pi] += 2. * np.pi

        #matrice delle distanze (calcolate utilizzando la norma 2) di tutte le celle rispetto alla posizione del robot
        dist_to_grid = scipy.linalg.norm(dx, axis=0) # matrix of L2 distance to all cells from robot

        # For each laser beam
        for z_i in z:
            r = z_i[0] # range measured (range del raggio laser)
            b = z_i[1] # bearing measured (angolo percepito (misura del raggio laser))

            # Calculate which cells are measured free or occupied, so we know which cells to update
            # Doing it this way is like a billion times faster than looping through each cell (because vectorized numpy is the only way to numpy)
            free_mask = (np.abs(theta_to_grid - b) <= self.beta/2.0) & (dist_to_grid < (r - self.alpha/2.0))
            occ_mask = (np.abs(theta_to_grid - b) <= self.beta/2.0) & (np.abs(dist_to_grid - r) <= self.alpha/2.0)

            # Adjust the cells appropriately
            self.log_prob_map[occ_mask] += self.l_occ
            self.log_prob_map[free_mask] += self.l_free

if __name__ == '__main__':
    
    #carichiamo stato della mappa(o posizione del robot nella mappa?) e misurazioni sensoriali
    # load matlab generated data (located at https://gitlab.magiccvs.byu.edu/superjax/595R/blob/88a577579a75dc744bca7630716211334e04a528/lab6/state_meas_data.mat?)
    data = scipy.io.loadmat('state_meas_data.mat')
    state = data['X']
    meas = data['z']

    #definiamo i parametri della mappa costruendo una mappa 100x100 con celle 1x1
    grid_size = 1.0
    map = Map(int(100/grid_size), int(100/grid_size), grid_size)

    plt.ion() # enable real-time plotting
    plt.figure(1) # create a plot
    #per ogni stato della mappa(posizione del robot) carica, aggiorna la mappa 
    for i in tqdm(range(len(state.T))): #tqdm è una libreria che genera automaticamente una barra di caricamento
        map.update_map(state[:,i], meas[:,:,i].T) # update the map

        # Real-Time Plotting 
        # (comment out these next lines to make it run super fast, matplotlib is painfully slow)
        plt.clf()
        pose = state[:,i]
        circle = plt.Circle((pose[1], pose[0]), radius=3.0, fc='y')
        plt.gca().add_patch(circle)
        arrow = pose[0:2] + np.array([3.5, 0]).dot(np.array([[np.cos(pose[2]), np.sin(pose[2])], [-np.sin(pose[2]), np.cos(pose[2])]]))
        plt.plot([pose[1], arrow[1]], [pose[0], arrow[0]])
        plt.imshow(1.0 - 1./(1.+np.exp(map.log_prob_map)), 'Greys')
        plt.pause(0.005)

    # Final Plotting
    plt.ioff()
    plt.clf()
    plt.imshow(1.0 - 1./(1.+np.exp(map.log_prob_map)), 'Greys') # This is probability
    plt.imshow(map.log_prob_map, 'Greys') # log probabilities (looks really cool)
    plt.show()
