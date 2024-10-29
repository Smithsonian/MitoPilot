# How to use MitoPilot on the Smithsonian Hydra computing cluster

You will need an account to access the Hyrda computing cluster. Instructions are available [here](https://confluence.si.edu/display/HPC/Hydra+Policies).

DJM has submitted a request for a system-wide Nextflow module. But for now, you will need to install Nextflow on Hydra. Also, for some reason, Nextflow does not work with the Java modules on Hydra, so you also need to install Java via SDKMAN.

```
# Nextflow installation instructions
# from https://www.nextflow.io/docs/latest/install.html
cd ~
curl -s https://get.sdkman.io | bash # install SDKMAN
source ~/.bashrc
sdk install java 17.0.10-tem # install java
curl -s https://get.nextflow.io | bash # install Nextflow
chmod +x nextflow # make Nextflow executable
```

There will now be an executable `nextflow` file in your home directory. You should move it to a location that is in your PATH. For example:

```
mkdir ~/bin
mv ~/nextflow ~/bin/nextflow
```
 
You can now call `nextflow` from anywhere on the cluster. Now you're ready to start using MitoPilot on Hydra!

- login to Hydra
- start an interactive session by running `qrsh`
- run `module load tools/R/RStudio/server`
- run `start-rstudio-server` (you may be asked to run a different command if this is your first time)
- leave this cluster terminal window open 
- in a new terminal window on your local computer:
	- start an ssh tunnel by running something like `ssh -N -L 8787:compute-64-16:8787 sylvain@hydra-login01.si.edu`, exact command available in your cluster terminal window
	- leave this terminal window open
	- open a web browser and enter `http://localhost:8787` in the URL bar
	- enter your cluster login credentials to access the RStudio server
	- any commands run in this new window will execute on the cluster in the interactive session
- to install MitoPilot, use the RStudio server window
	- run `remotes::install_github("JonahVentures/MitoPilot", auth_token = gh_token)` (this will not work for now b/c repo is private)
	- check that installation was successful by loading the library with `library(MitoPilot)`
- you can now follow instructions on the [MitoPilot GitHub page](https://github.com/JonahVentures/MitoPilot) to run the test dataset