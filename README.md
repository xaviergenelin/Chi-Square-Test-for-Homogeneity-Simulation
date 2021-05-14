# Chi-Sqare-Test-for-Homogeneity-Simulation

This project will conduct a Monte Carlo simulation to determine the properites of a chi-square test for homoegeity. A Pearson chi-square test will be used to see how well alpha and power are controlled for two multinomials.

Each multinomial will be one of four sample sizes: 20, 30, 50, 100 and there are 3 different probabilities that will be used in the multinomials:
* p1 = 1/3, 1/3, 1/3 (equal)
* p2 = 1/10, 3/10, 6/10 (mixed 1)
* p3 = 1/10, 1/10, 8/10 (mixed 2)

Any expected counts that are 0 will be changed to 0.5 so that we avoid the case where we divide by 0.

To determine alpha control, two multinomials will be generated from the same p vector and all sample size combinations. When inspecting power, we'll compare the 3 following cases:
* Equal vs Mixed 1
* Equal vs Mixed 2
* Mixed 1 vs Mixed 2
