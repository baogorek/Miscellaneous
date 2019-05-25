import numpy as np
import pywt
import matplotlib.pyplot as plt


x = np.array([1, 0, -3, 2, 1, 0, 1, 2])

haar = pywt.Wavelet('haar')
db = pywt.Wavelet('db2')

# returns [decomposition, reconstruction] x [low, high]
# reconstruction seem to be the ones i'm familiar with
haar.filter_bank

scale_vals, wavelet_vals, x_vals = haar.wavefun()
plt.plot(x_vals, scale_vals)
plt.show()
plt.plot(x_vals, wavelet_vals)
plt.show()

# length of the coefficients 
print pywt.dwt_coeff_len(x.shape[0], haar,"sym")
print pywt.dwt_coeff_len(8, 2,"sym")

# Maximum useful level of decomposition for 
# This is currently logged as a bug
print pywt.dwt_max_level(x.shape[0], haar.dec_len)

coeffs3 = pywt.wavedec(x, haar, mode = 'sym', level = 3)

# Let's see if I can recover the c(1) = (0, 2)
coeffs2 = pywt.wavedec(x, haar, mode = 'sym', level = 2)
decimated = coeffs[0]

# http://wavelets.pybytes.com/wavelet/db2/
scale_vals, wavelet_vals, x_vals = db.wavefun()
plt.plot(x_vals, scale_vals)
plt.show()
plt.plot(x_vals, wavelet_vals)
plt.show()

db_coefs3 = pywt.wavedec(x, db, mode = 'sym', level = 3)

### TODO: Do the decomposition with np.correlate
db_coefs1 = pywt.wavedec(x, db, mode = 'sym', level = 1)
# since I'm applying the filter going forward, I can use rec_hi and rec_lo
# I should turn it around and go back in time in accordance with filter theory
psi = db.rec_lo
phi = db.rec_hi

x_padded = np.concatenate(([0, 1], x, [2, 1]))
np.dot(psi, x_padded[0:4])

np.correlate(x_padded, psi, "valid")[[0, 2, 4, 6, 8]]
np.correlate(x_padded, phi, "valid")[[0, 2, 4, 6, 8]]


cA, cD = db_coefs1

## One at a time: Reconcile with upsampling
## bn = sum_{k = 0 to 4} h(n - 2 * k)

g9 = db.rec_hi[1] * cD[4] + db.rec_hi[3] * cD[3]
h9 = db.rec_lo[1] * cA[4] + db.rec_lo[3] * cA[3]
print g9 + h9

g8 = db.rec_hi[0] * cD[4] + db.rec_hi[2] * cD[3]
h8 = db.rec_lo[0] * cA[4] + db.rec_lo[2] * cA[3]
print g8 + h8

g7 = db.rec_hi[1] * cD[3] + db.rec_hi[3] * cD[2]
h7 = db.rec_lo[1] * cA[3] + db.rec_lo[3] * cA[2]
print g7 + h7 

g6 = db.rec_hi[0] * cD[3] + db.rec_hi[2] * cD[2]
h6 = db.rec_lo[0] * cA[3] + db.rec_lo[2] * cA[2]
print g6 + h6 

g5 = db.rec_hi[1] * cD[2] + db.rec_hi[3] * cD[1]
h5 = db.rec_lo[1] * cA[2] + db.rec_lo[3] * cA[1]
print g5 + h5 

g4 = db.rec_hi[0] * cD[2] + db.rec_hi[2] * cD[1]
h4 = db.rec_lo[0] * cA[2] + db.rec_lo[2] * cA[1]
print g4 + h4 

g3 = db.rec_hi[1] * cD[1] + db.rec_hi[3] * cD[0]
h3 = db.rec_lo[1] * cA[1] + db.rec_lo[3] * cA[0]
print g3 + h3 

g2 = db.rec_hi[0] * cD[1] + db.rec_hi[2] * cD[0]
h2 = db.rec_lo[0] * cA[1] + db.rec_lo[2] * cA[0]
print g2 + h2 

import numpy as np
import pywt
from matplotlib import pyplot as plt

x = np.arange(1024)
y = np.sin(2 * np.pi * x / 8) +  np.sin(2 * np.pi * x / 32)
plt.plot(x[0:200], y[0:200])
plt.show

def get_power_spectrum(y, wavelet):
  print("Using", wavelet, "wavelet")
  coefs = pywt.wavedec(y, wavelet, mode = 'sym', level = 10)
  
  for i in range(10):
    detail_i = coefs[i + 1]
    octave = 10 - i 
    period = 2 ** octave
    frequency = 1 / period
    detail_norm = np.sqrt(detail_i.dot(detail_i)) / np.sqrt(len(detail_i))
    print("frequency is 1 / ", str(period), "=", round(frequency, 2), 
          "has detail coef norm", str(round(detail_norm, 2)))

get_power_spectrum(y, 'haar')
get_power_spectrum(y, 'db2')
get_power_spectrum(np.random.normal(size = 1024), 'haar')
get_power_spectrum(np.random.normal(size = 1024), 'db2')



