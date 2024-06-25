# Burgar's et al. 2019 SCR analysis ####

#  Columbian fisher populations ####

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
