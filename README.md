# Haskell-FunctionOffloading
Proof of concept for a thesis on the offloading of Haskell functions onto an FPGA

Note: In-depth User manual is still a work in progress.

[Add link to thesis]

## Haskell Co-design requirements
...

## Semi-automated function offloading guide
...

## Manual function offloading guide
...

## Reproducing the thesis' SoCKit setup
The configuration of the SoCKit used in this proof of concept has been extensively modified from its original state as proposed by the Xillybus IP core and Linux image \cite{ref:xillybus}. The main purpose of these modifications was to allow for compiling and running Haskell code (version 7.10.3) on the ARM processor itself, as a cross-compiler was not capable of dynamic-loading and Template Haskell \cite{ref:TH_prob}. There are essentially two options that will result in an exact replicate of the SoCKit set-up used in this thesis. These options are:
+ Making an exact copy of the 32GB SD-card image used during this thesis.
+ Re-create the SD-card image by following the steps described in \autoref{sec:prepimage}.

Additionally the default Xillybus FPGA project has also been modified. A copy of the complete FPGA project is stored on the SD-card image, which may be downloaded to a host computer. The complete project will not be made available online on \todo{github} as it requires a license by Xillybus\cite{ref:licenses}. Instead, the steps to recreate it are provided in \autoref{sec:prepfpga}.\footnote{Or contact the owner of the github repository for a temporary download link}\todo{github} . 

### Re-creating the SD-card Linux image
This guide for preparing the SoCKit SD-card image starts off with the assumption that the Xillybus demonstration set-up has been completed, which may be achieved by downloading the associated files and following the guide(s) on \cite{ref:xillinuxguide}.

Now that the demonstration application set-up is completed and appears to be operating correctly, we may proceed with applying the modifications to the Xillinux image as enumerated below. To summarize the process, we attempt to install GHC version 7.10.3 and CλaSH on the ARM. We require this specific version because CλaSH does not work on older versions. As GHC builds upon specific libraries, we will have to update Ubuntu itself too. Each step can take a considerable amount of time and it is therefore recommended to make a backup after each successful Ubuntu upgrade to reduce time-loss. GHC 7.10.3 is a standard package available in the universal repository of Ubuntu 16.04 LTS (_Xenial_), but upgrading the complete system to Xenial can (and in our initial case will) break the OS. The solution we used was to only upgrade to Ubuntu 14.04 (_Trusty_) and to install GHC 7.10.3 and its dependencies from the universal repository of Ubuntu 16.04 (_Xenial_).

1. Provide the SoCKit with internet access such that the package manager can be used.

2. Fully update the standard Ubuntu version 12.04 LTS (_precise_).

3. Upgrade the Ubuntu image to version 14.04 LTS (_trusty_) through the package manager. This can be achieved by first changing the default Xillinux '_precise_' repository codename to the '_trusty_' codename in the '_/etc/apt/sources.list_' file.

4. Temporarily add the '_universal_' Xenial repository (possibly also the '_main_' and '_restricted_' repos) to the '_/etc/apt/sources.list_' and subsequently install GHC 7.10.3 and its dependencies. Afterwards the Xenial repositories can be removed again. (It is not recommended to upgrade to Ubuntu 16.04 LTS (_Xenial_) because it breaks the OS!)

5. Use cabal to install the '_ghc-paths_' (used for the Core plugin) and the most recent '_CλaSH_' Haskell packages (_CλaSH_  version 0.6.19 was used in this thesis).

After this process Haskell and CλaSH  should be fully functioning on the ARM. If the FPGA project from the thesis is available, then an (almost) identical copy of the SoCKit set-up is prepared. In the next section a guide is written on how to recreate the FPGA project with merely downloading the top-level entity file found on \todo{refer to github}.

### Re-creating the FPGA project
The template FPGA project that is used for this thesis can also be recreated. It starts with the default demonstration project provided by Xillybus \cite{ref:xillinuxguide}, which is only compatible with Quartus II version 13.0sp1. It is assumed that the standard process of generating a FPGA programmable bit-stream as proposed by Xillybus is performed at least once such that all required files are generated. The following steps are necessary to recreate the project as used in this thesis:

+ Generate the correct Xillybus IP core on the IP factory website \cite{ref:ipfactory} (Requires registering on the website). The IP core should be configured similarly to the configuration displayed in \autoref{fig:coreconfig}.
+ Follow the \textit{README.TXT} file provided with the generated core to apply the IP core changes into the default Quartus project. The step to update the top-level entity file with the generated template HDL may be skipped.
+ Replace the top-level '\textit{xillydemo.vhd}' file with the top-level entity file as provided on \todo{url github}. 
+ Use the pin assignment file on \todo{github} to add the correct pin assignments for the push-buttons and 50MHz clock. This may also be done manually if desired.
