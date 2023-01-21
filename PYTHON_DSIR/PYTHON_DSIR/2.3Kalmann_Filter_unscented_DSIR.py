from kf_book.book_plots import set_figsize, figsize
import matplotlib.pyplot as plt
from kf_book.nonlinear_plots import plot_nonlinear_func
from numpy.random import normal
import numpy as np
from filterpy.kalman import unscented_transform, MerweScaledSigmaPoints
import scipy.stats as stats
from numpy.random import multivariate_normal
from kf_book.nonlinear_plots import plot_monte_carlo_mean



# creati 500,000 campioni con media 0, std 1
gaussian = (0., 1.)
data = normal(loc=gaussian[0], scale=gaussian[1], size=500000)

def f(x):
    return (np.cos(4*(x/2 + 0.7))) - 1.3*x

def f_nonlinear_xy(x, y):
    return np.array([x + y, .1*x**2 + y*y])

plot_nonlinear_func(data, f)

N = 30000
plt.subplot(121)
plt.scatter(data[:N], range(N), alpha=.2, s=1)
plt.title('Input')
plt.subplot(122)
plt.title('Output')
plt.scatter(f(data[:N]), range(N), alpha=.2, s=1)
plt.show()


#inizializzo media e covarianza
mean = (0., 0.)
p = np.array([[32., 15], [15., 40.]])

# creo punti sigma e pesi 
points = MerweScaledSigmaPoints(n=2, alpha=.3, beta=2., kappa=.1)
sigmas = points.sigma_points(mean, p)

# passo attraverso la funzione non lineare
sigmas_f = np.empty((5, 2))
for i in range(5):
    sigmas_f[i] = f_nonlinear_xy(sigmas[i, 0], sigmas[i ,1])

# uso una trasformazione unscented per ottenere la nuova media e covarianza 
ukf_mean, ukf_cov = unscented_transform(sigmas_f, points.Wm, points.Wc)

# genero punti casuali 
np.random.seed(100)
xs, ys = multivariate_normal(mean=mean, cov=p, size=5000).T

plot_monte_carlo_mean(xs, ys, f_nonlinear_xy, ukf_mean, 'Unscented Mean')
ax = plt.gcf().axes[0]
ax.scatter(sigmas[:,0], sigmas[:,1], c='r', s=30)
plt.show()