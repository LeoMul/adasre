# adasre
Post processes the (un)formatted oic files of Nigel Badnell's `AUTOSTRUCTURE` [code](https://amdpp.phys.strath.ac.uk/autos/) to generate resonant-excitation data, that can be added to direct-excitation runs to produce an isolated resonant approximation total Maxwellian-Averaged collision strengths. 

This code can process both `RUN='DR'` and `RUN='RE'` runs from the `AUTOSTRUCTURE` package. In either case, the radiative rates are ignored and the resonant-branching-ratios assume only Auger decays. This is valid for excitation, but not for DR. 

The code expects a file called `input` file - containing the number of bound states in the $N$ electron system
