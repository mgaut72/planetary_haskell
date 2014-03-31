import matplotlib.pyplot as plt
from matplotlib import animation
import matplotlib
import numpy as np
import sys
import time
from matplotlib import rcParams
rcParams.update({'figure.autolayout': True})

from matplotlib.backends.backend_pdf import PdfPages

def readData():

    while True:
        line = sys.stdin.readline().strip()
        if not line:
            break

        parts = line.split()
        data = [float(x) for x in parts[1:]]
        global_data.append(data)


        line = sys.stdin.readline().strip()
        parts = line.split()
        data = [float(x) for x in parts]
        mercury_data.append(data)

        line = sys.stdin.readline().strip()
        parts = line.split()
        data = [float(x) for x in parts]
        venus_data.append(data)

        line = sys.stdin.readline().strip()
        parts = line.split()
        data = [float(x) for x in parts]
        earth_data.append(data)



global_data = []
mercury_data = []
venus_data = []
earth_data = []

readData()
fig = plt.figure()
ax = fig.add_subplot(1,1,1, xlim=(-1.1, 1.1), ylim=(-1.1, 1.1), aspect='equal', axisbg = 'black')

sun = plt.Circle((-5,-5), radius = 0.1, color='yellow')
mercury = plt.Circle((-5,-5), radius = 0.01, color = 'white')
venus = plt.Circle((-5,-5), radius = 0.03, color = 'red')
earth = plt.Circle((-5,-5), radius = 0.04, color = 'blue')

x = 3
y = 4

def init():
    sun.center = (0, 0)
    #mercury.center = (mercury_data[0][x], mercury_data[0][y])
    #venus.center = (venus_data[0][x], venus_data[0][y])
    #earth.center = (earth_data[0][x], earth_data[0][y])
    ax.add_artist(sun)
    ax.add_artist(mercury)
    ax.add_artist(venus)
    ax.add_artist(earth)
    return sun, mercury, venus, earth,

def animate(i):
    mercury.center = (mercury_data[i][x], mercury_data[i][y])
    venus.center = (venus_data[i][x], venus_data[i][y])
    earth.center = (earth_data[i][x], earth_data[i][y])
    return sun, mercury, venus, earth,


anim = animation.FuncAnimation(fig, animate, frames=len(global_data), init_func=init,
                                interval=50, blit=True, repeat=False)

plt.show()
