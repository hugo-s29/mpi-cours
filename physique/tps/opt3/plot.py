import matplotlib.pyplot as plt
import matplotlib
import numpy as np

_data = np.genfromtxt('data.csv', delimiter=',')
xs, data = _data[:,1], _data[:,2]

sinc = lambda x : np.sin(x) / x


matplotlib.use("pgf")
matplotlib.rcParams.update({
    "pgf.texsystem": "pdflatex",
    'font.family': 'serif',
    'text.usetex': True,
    'pgf.rcfonts': False,
})

fig, ax = plt.subplots()

def plot1():
    m  = 1647
    b  = 799
    d  = 49273
    x0 = 23.598
    v  = 5.56e-7

    i1 = m + b * sinc(2*np.pi * v * (xs - x0) * d)
    i2 = m - b * sinc(2*np.pi * v * (xs - x0) * d)
    mod = np.maximum(i1, i2)

    ax.plot(xs, mod, label="$I_{\\mathrm{mod}}(t)$", color="#d7827e")


def plot2():
    m  = 1644
    b  = 810
    a  = 22139
    x0 = 24.212
    v  = 5.56e-7

    mod = m + b * np.exp(-(2*np.pi*v*(xs-x0)*a)**2)

    ax.plot(xs, mod, label="$I_{\\mathrm{mod}}(t)$",  color="#d7827e")

ax.plot(xs, data, label="$I_{\\mathrm{mes}}(t)$", color="#907aa9")
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
ax.legend()
fig.savefig('plot0.pgf')
ax.clear()
ax.plot(xs, data, label="$I_{\\mathrm{mes}}(t)$", color="#907aa9")
plot1()
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
ax.legend()
fig.savefig('plot1.pgf')
ax.clear()
ax.plot(xs, data, label="$I_{\\mathrm{mes}}(t)$", color="#907aa9")
ax.plot(xs, data, label="$I_{\\mathrm{mes}}(t)$", color="#907aa9")
plot2()
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
ax.legend()
fig.savefig('plot2.pgf')
