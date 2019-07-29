lambda <- 0.2548 # GPS L5 wavelength (m)
delta.F <- 24 # Frequency Bandwidth (MHz)
T.Q5 <- 0.02 # Period of the Q5 component of the GPS L5 signal 

P.t <- -157.9 # Transmitter Power at Surface of Earth(dBW)
G.r <- 25 # Receiver Antenna Gain (dB)
G.t <- 15 # Transmitter Antenna Gain (dB)
G.sp <- delta.F * T.Q5 # Processing Gain of the Cross-Correlator

N.r <- -131 # Noise level for GPS L5 receiver (dB) == k * T * delta.F

R.tg <- 300:800 # Distance between Target and Receiver (m)
R.tg <- R.tg/100
R.2 <- 22000


h <- 2.72 # 1st dimension of Cessna 172 (m)
l <- 8.3 # 2nd dimension of Cessna 172 (m)
sigma.fsr <- (4 * pi * (h * l)^2) / (lambda ^2)

SNR.rx <- (P.t * G.r * (h * l)^2 * G.sp) / (lambda^2 * R.tg^2 * N.r)

SNR.db <- (10 * log(SNR.rx))/log(10)
plot(R.tg, SNR.db)

SNR.min <- 10^(20/10) # 20dB SNR minimum
R.max <- sqrt((P.t * G.r * G.sp * sigma.fsr) / (4* pi * N.r * SNR.min)) # Maximum Range for min SNR
R.max <- ((h * l) / lambda) * sqrt((P.t * G.r * G.sp) / (N.r * SNR.min))


a = 11 / 2 # Radius of sphere
alpha = a / lambda

d = 11
theta = (lambda / d) * (180 / pi)
half_angle <- 5 * theta / 2
