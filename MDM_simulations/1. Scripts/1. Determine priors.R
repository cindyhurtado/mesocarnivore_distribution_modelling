# Burgar's et al. 2019 SCR analysis ####

# Priors ###

# 1. from Chandler and royle 2013 supplementary material ####

# PART 4
# Commented R code describing method used to obtain
# informative prior used in the analysis of the parula dataset.
# Moldenhauer and Regelski (1996) state that home range size typically
# ranges from 0.08-0.65 ha, which equates to a radius of

sqrt(0.08/pi*10000) # 15.96 m or
sqrt(0.65/pi*10000) # 45.49 m

# Since parulas were detected by song, let's be safe and add a minimum and maximum distance at which they could be heard, 50 and 250 m, based upon Simons et al. (2009).
# So now we have an area between

(15.96+50)^2*pi / 10000  # 1.37 ha, and
(45.49+250)^2*pi / 10000 # 27.4 ha

# We note that this is a very large range.

# Following Royle et. al (2011), and assuming a
# chi-squared distribution with 2 degrees of freedom,
# the range of sigma is given by

sqrt(1.37*10000/pi)/sqrt(5.99)   # 27 m
sqrt(27.4*10000/pi)/sqrt(5.99)   # 120 m

# In our grid spacing, 1 unit = 50m, so our we want a prior with most
# of the density between:

27/50  # 0.54
121/50 # 2.42

# Gamma(13, 10) covers this nicely
#Cindy: so needs data values for gamma distribution that fit within 0.54 and 2.42?

qgamma(c(0.001, 0.5, 0.999), 13, 10)

plot(function(x) dgamma(x, 13, 10), 0, 5, xlim=c(0, 3), ylim=c(0, 1.5))

#2.  Jo Burgars et al 2019 #### 

#home range data to determine gamma values for HR varying form 4 to 72, 16km2 and 40km2 ##

# For home ranges sizes between 4-72 km2, the range of σ in model units of 1 km is:
# 5.99 is the chi square value from a significance level of 0.05 with 2 degrees of freedom. 

(sqrt(4/pi)/1)/sqrt(5.99) # 0.46

(sqrt(16/pi)/1)/sqrt(5.99) # 0.92

(sqrt(40/pi)/1)/sqrt(5.99) # 1.46

(sqrt(72/pi)/1)/sqrt(5.99) # 1.95

# Resulting in prior distributions with most of the density between 0.46-1.95

qgamma(c(0.05,0.5,0.95),60,60) # 0.79754 (min) 0.99445(med) 1.22139 (max) - 16 km2. Values need to be from 0.92 to 1.95

qgamma(c(0.05,0.5,0.95),75,50) # 1.2269 1.4933 1.7958 - 40 km2.  Values need to be from 1.46 to 1.95

qgamma(c(0.05,0.5,0.95),4,4) # 0.34158 0.91802 1.93841 - 4-72 km2. Values need to be from 0.46 to 1.95

# 3. Columbian populations ####

# Furnas et al. 2017 mean home range between 21-128 km2
# the home estimated home range for each population is: 

#priors used by Sun et al. 2019 for bears

qgamma(c(0.001,0.5,0.999),1, 0.001)  # 1.005 , 693.14, 6907.76


(sqrt(5/pi)/1)/sqrt(5.99) #  core home range, for female central interior # 0.52

(sqrt(30/pi)/1)/sqrt(5.99) #  average female home range 30km (Davis 2009, Weir et al. 2009) in central interior # 1.26

(sqrt(50/pi)/1)/sqrt(5.99) # expected home range in Columbian populations?  # 1.63

(sqrt(128/pi)/1)/sqrt(5.99) # maixmun average home range in US Furnas et al. 2017 # 2.60

(sqrt(141/pi)/1)/sqrt(5.99) # puntzi lake sigma 2730 m #2.737

(sqrt(300/pi)/1)/sqrt(5.99) # assuming one male can have up to 5 female home ranges of 50km2, maximum HR could be 300km2 but unlikely # 3.99

qgamma(c(0.001,0.5,0.999),6, 4) #0.2767762 1.4175403 4.1136863

# plot distribution 
x <- seq(0.29,4.1, by =0.01)
y <- dgamma(x, 6,4) # 
plot(y)
