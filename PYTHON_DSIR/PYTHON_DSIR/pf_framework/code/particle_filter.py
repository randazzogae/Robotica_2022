import numpy as np
import scipy.stats
import matplotlib.pyplot as plt
from read_data import read_world, read_sensor_data

#PRIMA DI PROCEDERE, RICORDIAMO I PASSI PER LA REALIZZAZIONE DEL FILTRO PARTICELLARE:

#1)GENERO LA DISTRIBUZIONE PARTICELLARE UNIFORME IN MANIERA CASUALE
#2)PREDIZIONE DEL NEXT STATE DELLE PARTICELLE
#3)UPDATE
#4)RESAMPLE
#5)CALCOLO LA STIMA

#add random seed for generating comparable pseudo random numbers
np.random.seed(123)

#plot preferences, interactive plotting mode
plt.axis([-1, 12, 0, 10]) #asse x da -1 a 12 e asse y da 0 a 10
plt.ion()
plt.show()

 # Visualizes the state of the particle filter.
    #
    # Displays the particle cloud, mean position and landmarks.

def plot_state(particles, landmarks, map_limits):
   
    
    xs = []
    ys = []

    for particle in particles: #scorro tutte le particelle e riempio xs e ys
        xs.append(particle['x'])
        ys.append(particle['y'])

    # landmark positions
    lx=[]
    ly=[]

    #Stesso discorso per i landmarks, cioè li scorro e li appendo in lx e in ly
    for i in range (len(landmarks)):
        lx.append(landmarks[i+1][0])
        ly.append(landmarks[i+1][1])

    # mean pose as current estimate
    estimated_pose = mean_pose(particles)

    # plot filter state
    plt.clf()
    plt.plot(xs, ys, 'r.')
    plt.plot(lx, ly, 'bo',markersize=10)
    plt.quiver(estimated_pose[0], estimated_pose[1], np.cos(estimated_pose[2]), np.sin(estimated_pose[2]), angles='xy',scale_units='xy')
    plt.axis(map_limits)

    plt.pause(0.01)

def initialize_particles(num_particles, map_limits):
    # randomly initialize the particles inside the map limits

    particles = []

    for i in range(num_particles):
        particle = dict()

        # draw x,y and theta coordinate from uniform distribution
        # inside map limits
        particle['x'] = np.random.uniform(map_limits[0], map_limits[1])
        particle['y'] = np.random.uniform(map_limits[2], map_limits[3])
        particle['theta'] = np.random.uniform(-np.pi, np.pi)

        particles.append(particle)

    return particles


 # calculate the mean pose of a particle set.
    #
    # for x and y, the mean position is the mean of the particle coordinates
    #
    # for theta, we cannot simply average the angles because of the wraparound 
    # (jump from -pi to pi). Therefore, we generate unit vectors from the 
    # angles and calculate the angle of their average 
def mean_pose(particles):
   

    # save x and y coordinates of particles
    xs = []
    ys = []

    # save unit vectors corresponding to particle orientations 
    vxs_theta = []
    vys_theta = []

    for particle in particles:
        xs.append(particle['x'])
        ys.append(particle['y'])

        #make unit vector from particle orientation
        vxs_theta.append(np.cos(particle['theta']))
        vys_theta.append(np.sin(particle['theta']))

    #calculate average coordinates
    mean_x = np.mean(xs)
    mean_y = np.mean(ys)
    mean_theta = np.arctan2(np.mean(vys_theta), np.mean(vxs_theta))

    return [mean_x, mean_y, mean_theta]

def sample_motion_model(odometry, particles):
    # Samples new particle positions, based on old positions, the odometry
    # measurements and the motion noise 
    # (probabilistic motion models slide 27)

    #DEFINIAMO MISURE ODOMETRICHE TUTTE QUELLE MISURE OTTENUTE MEDIANTE I SENSORI
    
    delta_rot1 = odometry['r1']
    delta_trans = odometry['t']
    delta_rot2 = odometry['r2']

    # the motion noise parameters: [alpha1, alpha2, alpha3, alpha4]
    noise = [0.1, 0.1, 0.05, 0.05]
    
    # standard deviations of motion noise
    sigma_delta_rot1 = noise[0] * abs(delta_rot1) + noise[1] * delta_trans
    sigma_delta_trans = noise[2] * delta_trans + \
    noise[3] * (abs(delta_rot1) + abs(delta_rot2))
    sigma_delta_rot2 = noise[0] * abs(delta_rot2) + noise[1] * delta_trans

    #Generiamo un nuovo insieme di particelle, ottenute dopo l'update del movimento
    new_particles = []
    
    for particle in particles: 
        new_particle = dict(); #otteniamo un nuovo dizionario
        
        #Sample noisy motion 
        noisy_delta_rot1 = delta_rot1 + np.random.normal(0, sigma_delta_rot1)
        noisy_delta_trans = delta_trans + np.random.normal(0, sigma_delta_trans)
        noisy_delta_rot2 = delta_rot2 + np.random.normal(0, sigma_delta_rot2)
        
        
        # Calcolo la posizione della nuova particella, tenendo conto della vecchia posizione, delle
        #misure odomentrice deltarot1 e deltaTrans, deltarot2 e del noisemotion
        new_particle['x'] = particle['x'] + \
        noisy_delta_trans * np.cos(particle['theta'] + noisy_delta_rot1)
        new_particle['y'] = particle['y'] + \
        noisy_delta_trans * np.sin(particle['theta'] + noisy_delta_rot1)
        new_particle['theta'] = particle['theta'] + \
        noisy_delta_rot1 + noisy_delta_rot2
        new_particles.append(new_particle)
        
    return new_particles

      #Calcola la probabilità di osservazione di tutte le particelle, date che
    #siano le posizioni delle particelle e dei landmark e le misure sensoriali.

    
def eval_sensor_model(sensor_data, particles, landmarks):


    sigma_r = 0.2

    #measured landmark ids and ranges
    ids = sensor_data['id']
    ranges = sensor_data['range']

    weights = []
    # weight each particle
    for particle in particles:
        all_meas_likelihood = 1.0
        # loop for each observed landmark
        for i in range(len(ids)):
            lm_id = ids[i]
            meas_range = ranges[i]
            lx = landmarks[lm_id][0]
            ly = landmarks[lm_id][1]
            px = particle['x']
            py = particle['y']  
            # calculate expected range measurement
            meas_range_exp = np.sqrt((lx - px) ** 2 + (ly - py) ** 2)
            # evaluate sensor model (probability density function of normal distribution)
            meas_likelihood = scipy.stats.norm.pdf(meas_range, meas_range_exp, sigma_r)
            # combine (independent) measurements
            all_meas_likelihood = all_meas_likelihood * meas_likelihood
        
        
        weights.append(all_meas_likelihood)
            # for combining multiple measurements

    #normalize weights
    normalizer = sum(weights)
    weights = weights / normalizer

    return weights

    # Returns a new set of particles obtained by performing
    # stochastic universal sampling, according to the particle weights.
def resample_particles(particles, weights):

    new_particles = []
        # distance between pointers
    step = 1.0 / len(particles)
    # random start of first pointer
    u = np.random.uniform(0, step)
    # where we are along the weights
    c = weights[0]

    # index of weight container and corresponding particle
    i = 0
    new_particles = []
    # loop over all particle weights
    for particle in particles:
        # go through the weights until you find the particle
        # to which the pointer points
        while u > c:
            i = i + 1
            c = c + weights[i]
        # add that particle
        new_particles.append(particles[i])
        # increase the threshold
        u = u + step

    return new_particles

# implementation of a particle filter for robot pose estimation
def main():
         
    print ("Reading landmark positions"); 
    landmarks = read_world("C:pf_framework\code\world.dat")

    print("Reading sensor data"); 
    sensor_readings = read_sensor_data("C:pf_framework\code\sensor_data.dat")

    #initialize the particles
    map_limits = [-1, 12, 0, 10]
    particles = initialize_particles(1000, map_limits)

    #run particle filter
    for timestep in range(int(len(sensor_readings)/2)):

        #plot the current state
        plot_state(particles, landmarks, map_limits)

        #predict particles by sampling from motion model with odometry info
        new_particles = sample_motion_model(sensor_readings[timestep,'odometry'], particles)

        #calculate importance weights according to sensor model
        weights = eval_sensor_model(sensor_readings[timestep, 'sensor'], new_particles, landmarks)

        #resample new particle set according to their importance weights
        particles = resample_particles(new_particles, weights)

    plt.show('hold')

if __name__ == "__main__":
    main()