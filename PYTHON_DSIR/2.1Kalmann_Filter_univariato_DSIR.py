from asyncio import PriorityQueue
import kf_book.kf_internal as kf_internal
from kf_book.kf_internal import DogSimulation
import numpy as np
import filterpy.stats as stats
from collections import namedtuple
import matplotlib.pyplot as plt
from ipywidgets import interact

gaussian = namedtuple('Gaussian', ['mean', 'var'])
gaussian._repr_ = lambda s: f'𝒩(μ={s[0]:.3f}, 𝜎²={s[1]:.3f})'

np.random.seed(13)


def predict(pos, movement):
    return gaussian(pos.mean + movement.mean, pos.var + movement.var)

def gaussian_multiply(g1, g2):
    mean = (g1.var * g2.mean + g2.var * g1.mean) / (g1.var + g2.var)
    variance = (g1.var * g2.var) / (g1.var + g2.var)
    return gaussian(mean, variance)


def update(prior, likelihood):
    posterior = gaussian_multiply(likelihood, prior)
    return posterior

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

from kf_book import book_plots as book_plots
from ipywidgets.widgets import IntSlider

# save output in these lists for plotting
xs, predictions, v = [], [], []

# perform Kalman filter
for z in zs:    
    prior = predict(x, process_model)   #belief iniziale
    likelihood = gaussian(z, sensor_var)    #misura
    x = update(prior, likelihood)   #belief aggiornato

    # save results
    predictions.append(prior.mean)
    xs.append(x.mean)
    v.append(x.var)
    
    kf_internal.print_gh(prior, x, z)
    
print()
print(f'final estimate:        {x.mean:10.3f}')
print(f'actual final position: {dog.x:10.3f}')



def plot_filter(step):
    plt.cla()
    step -= 1
    i = step // 3 + 1
 
    book_plots.plot_predictions(predictions[:i])  
    if step % 3 == 0:
        book_plots.plot_measurements(zs[:i-1])
        book_plots.plot_filter(xs[:i-1], var=v[:i-1])
    elif step % 3 == 1:
        book_plots.plot_measurements(zs[:i])
        book_plots.plot_filter(xs[:i-1], var=v[:i-1])
    else:
        book_plots.plot_measurements(zs[:i])
        book_plots.plot_filter(xs[:i], var=v[:i])
        print(xs)
    
    plt.xlim(-1, 10)
    plt.ylim(0, 20)
    plt.legend(loc=2)
    plt.show()
 
interact(plot_filter, step=30)