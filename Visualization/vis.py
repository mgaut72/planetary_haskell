import matplotlib.pyplot as plt
from matplotlib import animation
import matplotlib
import sys

def readData():

    while True:
        line = sys.stdin.readline().strip()
        if not line:
            break

        parts = line.split()
        data = [float(x) for x in parts]
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
ax = fig.add_subplot(1,1,1, xlim=(-1.3, 1.3), ylim=(-1.3, 1.3), aspect='equal', axisbg = 'black')

sun = plt.Circle((-5,-5), radius = 0.1, color='yellow')
mercury = plt.Circle((-5,-5), radius = 0.01, color = 'white')
venus = plt.Circle((-5,-5), radius = 0.03, color = 'red')
earth = plt.Circle((-5,-5), radius = 0.04, color = 'blue')
global_step = ax.text(0.05, 0.95, "", transform=ax.transAxes, color='white')
time = ax.text(0.55, 0.95, "", transform=ax.transAxes, color='white')

x = 3
y = 4

def init():
    sun.center = (0, 0)
    global_step.set_text("")
    time.set_text("")
    ax.add_artist(sun)
    ax.add_artist(mercury)
    ax.add_artist(venus)
    ax.add_artist(earth)
    return mercury, venus, earth, global_step, time

def animate(i):
    global_step.set_text("step = %d"  % int(global_data[i][0]))
    time.set_text("time = %d earth days"  % int(global_data[i][1] * 365.25))
    mercury.center = (mercury_data[i][x], mercury_data[i][y])
    venus.center = (venus_data[i][x], venus_data[i][y])
    earth.center = (earth_data[i][x], earth_data[i][y])
    return mercury, venus, earth, global_step, time


anim = animation.FuncAnimation(fig, animate, frames=len(global_data), init_func=init,
                                interval=50, blit=True, repeat=False)

plt.show()
