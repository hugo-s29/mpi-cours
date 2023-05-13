import matplotlib.pyplot as plt
import matplotlib
import numpy as np

sinc=np.sinc

matplotlib.use("pgf")
matplotlib.rcParams.update({
    "pgf.texsystem": "pdflatex",
    'font.family': 'serif',
    'text.usetex': True,
    'pgf.rcfonts': False,
})

fig, ax = plt.subplots()

xs = np.arange(0, 10, 0.01/2)

def plot1():
    mod = 0.7 * (4 + np.cos(2*np.pi*0.4*(xs-5)*20) * sinc(2*np.pi*0.4*(xs-4.5)*0.2) / 0.2)
    ma = 0.7 * (4 + sinc(2*np.pi*0.4*(xs-4.5)*0.2) / 0.2)
    mb = 0.7 * (4 - sinc(2*np.pi*0.4*(xs-4.5)*0.2) / 0.2)
    en_max = np.maximum(ma, mb)
    en_min = np.minimum(ma, mb)
    ax.plot(xs, mod, color="#d7827e")
    ax.plot(xs, en_max, color="#286983")
    ax.plot(xs, en_min, color="#286983")

def plot2():
    mod = 2.60794419761 * (1+np.cos(4*np.pi*0.4*(xs-4.5)*0.15*100)*np.exp(-(2*np.pi*0.15*0.4*(xs-4.5))**2))
    ma = 2.60794419761 * (1+np.exp(-(2*np.pi*0.15*0.4*(xs-4.5))**2))
    mb = 2.60794419761 * (1-np.exp(-(2*np.pi*0.15*0.4*(xs-4.5))**2))
    en_max = np.maximum(ma, mb)
    en_min = np.minimum(ma, mb)
    ax.plot(xs, mod, color="#d7827e")
    ax.plot(xs, en_max, color="#286983")
    ax.plot(xs, en_min, color="#286983")

plot1()
plt.xticks([], [])
plt.yticks([], [])
ax.spines['left'].set_position('zero')
ax.spines['right'].set_visible(False)
ax.spines['bottom'].set_position('zero')
ax.spines['top'].set_visible(False)
ax.xaxis.set_ticks_position('bottom')
ax.yaxis.set_ticks_position('left')

# make arrows
ax.plot((1), (0), ls="", marker=">", ms=10, color="k",
        transform=ax.get_yaxis_transform(), clip_on=False)
ax.plot((0), (1), ls="", marker="^", ms=10, color="k",
        transform=ax.get_xaxis_transform(), clip_on=False)
fig.savefig('plot3.pgf')
ax.clear()
plot2()
plt.xticks([], [])
plt.yticks([], [])
ax.spines['left'].set_position('zero')
ax.spines['right'].set_visible(False)
ax.spines['bottom'].set_position('zero')
ax.spines['top'].set_visible(False)
ax.xaxis.set_ticks_position('bottom')
ax.yaxis.set_ticks_position('left')

# make arrows
ax.plot((1), (0), ls="", marker=">", ms=10, color="k",
        transform=ax.get_yaxis_transform(), clip_on=False)
ax.plot((0), (1), ls="", marker="^", ms=10, color="k",
        transform=ax.get_xaxis_transform(), clip_on=False)
fig.savefig('plot4.pgf')
ax.clear()
