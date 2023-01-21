import math
import numpy as np
from numpy.random import randn
from asyncio import PriorityQueue
import kf_book.kf_internal as kf_internal
from kf_book.kf_internal import DogSimulation
import numpy as np
import filterpy.stats as stats
from collections import namedtuple
import matplotlib.pyplot as plt
from ipywidgets import interact
from filterpy.common import Q_discrete_white_noise
from kf_book.mkf_internal import plot_track
from filterpy.kalman import KalmanFilter
from filterpy.stats import plot_covariance_ellipse
from kf_book import book_plots as book_plots

def predict(pos, movement):
    return gaussian(pos.mean + movement.mean, pos.var + movement.var)

def gaussian_multiply(g1, g2):
    mean = (g1.var * g2.mean + g2.var * g1.mean) / (g1.var + g2.var)
    variance = (g1.var * g2.var) / (g1.var + g2.var)
    return gaussian(mean, variance)


def update(prior, likelihood):
    posterior = gaussian_multiply(likelihood, prior)
    return posterior

def compute_dog_data(z_var, process_var, count=1, dt=1.):
    "returns track, measurements 1D ndarrays"
    x, vel = 0., 1.
    z_std = math.sqrt(z_var) 
    p_std = math.sqrt(process_var)
    xs, zs = [], []
    for _ in range(count):
        v = vel + (randn() * p_std)
        x += v*dt        
        xs.append(x)
        zs.append(x + randn() * z_std)        
    return np.array(xs), np.array(zs)

def pos_vel_filter(x, P, R, Q=0., dt=1.0):
    """ Returns a KalmanFilter which implements a
    constant velocity model for a state [x dx].T
    """
    
    kf = KalmanFilter(dim_x=2, dim_z=1)
    kf.x = np.array([x[0], x[1]]) # location and velocity
    kf.F = np.array([[1., dt],
                     [0.,  1.]])  # state transition matrix
    kf.H = np.array([[1., 0]])    # Measurement function
    kf.R *= R                     # measurement uncertainty
    if np.isscalar(P):
        kf.P *= P                 # covariance matrix 
    else:
        kf.P[:] = P               # [:] makes deep copy
    if np.isscalar(Q):
        kf.Q = Q_discrete_white_noise(dim=2, dt=dt, var=Q)
    else:
        kf.Q[:] = Q
    return kf

def run(x0=(0.,0.), P=500, R=0, Q=0, dt=1.0, 
        track=None, zs=None,
        count=0, do_plot=True, **kwargs):
    """
    track is the actual position of the dog, zs are the 
    corresponding measurements. 
    """

    # Simulate dog if no data provided. 
    if zs is None:
        track, zs = compute_dog_data(R, Q, count)

    # create the Kalman filter
    kf = pos_vel_filter(x0, R=R, P=P, Q=Q, dt=dt)  

    # run the kalman filter and store the results
    xs, cov = [], []
    for z in zs:
        kf.predict()
        kf.update(z)
        xs.append(kf.x)
        cov.append(kf.P)

    xs, cov = np.array(xs), np.array(cov)
    if do_plot:
        plot_track(xs[:, 0], track, zs, cov, **kwargs)
    
    plot_covariance_ellipse(xs[0], cov[0], edgecolor='r')
    
    for _ in range(5):
        plot_covariance_ellipse(xs[_],cov[_], edgecolor='r')
        book_plots.set_labels(x='position',y='velocity')    
     
    return xs, cov

gaussian = namedtuple('Gaussian', ['mean', 'var'])
gaussian._repr_ = lambda s: f'𝒩(μ={s[0]:.3f}, 𝜎²={s[1]:.3f})'

np.random.seed(13)

process_var = 2. # variance in the dog's movement
sensor_var = 4.5 # variance in the sensor

x = gaussian(0., 20.*2)  # dog's position, N(0, 20*2)
velocity = 1
dt = 1. # time step in seconds
process_model = gaussian(velocity*dt, process_var) # displacement to add to x
  
# simulate dog and get measurements
dog = DogSimulation(
    x0=x.mean, 
    velocity=process_model.mean, 
    measurement_var=sensor_var, 
    process_var=process_model.var)

# create list of measurements
zs = [dog.move_and_sense() for _ in range(10)] 

print('PREDICT\t\t\tUPDATE')
print('     x      var\t\t  z\t    x      var')

# save output in these lists for plotting
xs, predictions, v = [], [], []

for z in zs:    
    prior = predict(x, process_model)   #belief iniziale
    likelihood = gaussian(z, sensor_var)    #misura(osservazione)
    x = update(prior, likelihood)   #belief aggiornato

    # save results
    predictions.append(prior.mean)      #posizioni prima della correzione
    xs.append(x.mean)   #posizioni dopo la correzione
    v.append(x.var)     #varianze dopo la correzione
    
    kf_internal.print_gh(prior, x, z)
    
print()
print(f'final estimate:        {x.mean:10.3f}')
print(f'actual final position: {dog.x:10.3f}')


#Ms è xs restituito da run e Ps è cov restituto da run
P = np.diag([500., 49.])   #P=matrice di covarianza. 
Ms, Ps = run(count=50, R=10, Q=0.01, P=P) #rumore varianza=10 misurazioni=50 varianza processo=0.01
plt.show()

plt.plot(Ms[:,0])  #Ms[x[x[0],x[1]]]     Ms[1:0,1] prima di : scegliamo l'indice dell'array x e dopo i : scegliamo gli indici 0 o 1 a seconda che vogliamo posizione o velocità
book_plots.set_labels(x='time', y='position')
plt.show()
plt.plot(Ms[:,1])
book_plots.set_labels(x='time', y='velocity')
plt.show()

dt = 0.6
x = np.array([0., 5.])
F = np.array([[1., dt], [0, 1.]])
P = np.array([[1.5, 0], [0, 3.]])
plot_covariance_ellipse(x, P, edgecolor='r')

for _ in range(5):
    x = F @ x
    P = F @ P @ F.T
    plot_covariance_ellipse(x, P, edgecolor='k', ls='dashed')
book_plots.set_labels(x='position', y='velocity')

plt.show()