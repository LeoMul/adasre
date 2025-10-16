# adasre
Post processes the (un)formatted oic files of Nigel Badnell's `AUTOSTRUCTURE` [code](https://amdpp.phys.strath.ac.uk/autos/) to generate resonant-excitation data, that can be added to direct-excitation runs to produce an isolated resonant approximation for the total Maxwellian-Averaged collision strengths. 

This code can process both `RUN='DR'` and `RUN='RE'` runs from the `AUTOSTRUCTURE` package. In either case, the radiative rates are ignored if they are present and the resonant-branching-ratios assume only Auger decays. This is valid for excitation, but not for DR. 

The code expects a file called `input` - containing the number of bound states in the $N$ electron system, along with the corresponding output of the `LEVELS` files from the structure run. Eventually this will be namelist-ed. Additionally, at this moment all inelastic transitions between $N$ electron bound states are calculated. The main bottleneck is looping over a large amount of resonances.

Unlike many atomic codes, this program is entirely dynamically allocated. The largest memory bottleneck is the lack of foreknowledge in the `AUTOSTRUCTURE` output for the number of Auger rates per block in the oic files. For this reason, three arrays are dynamically reallocated if they run over. 

Much like [adasdr](https://amdpp.phys.strath.ac.uk/autos/default/misc/), this code can take a mixture of formatted and unformatted input. The oic files should be symbolic linked to say o1,o2, o3u etc - with the suffix 'u' for unformatted files. 
