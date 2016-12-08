This readme is not finished!! Some references are broken and the manual offloading tutorial is not finished.

# Haskell-FunctionOffloading
Proof of concept for a thesis on the offloading of Haskell functions onto an FPGA. Thesis link: http://essay.utwente.nl/71486/

Contents
+ Semi-automated offloading guide
+ Manual offloading guide
+ General tasks
+ Reproducing thesis setup


## Semi-automated function offloading guide
This guide describes how to use the thesis' implementation for semi-automated offloading of annotated Haskell functions. Refer to the thesis for more detail. First the requirements are described and subsequently the tutorial is given. An example can be found in the folder _/examples/auto/_ of this repository.

### Requirements
+ All Haskell functions that are to be offloaded should adhere to the following requirements:
  + The offloadable functions are completely compilable to a hardware description language by CLaSH (In this implementation only a VHDL Quartus project was prepared, but Verilog should be possible too with some additional work).
  + An offloadable function should have the following pure function: **_i -> o_**
  + A composition of the previously mentioned offloadable function and the higher-order data-flow function of CLaSH has to be exist, i.e. the function can be passed as an argument to the **_pureDF_**
  + The argument and result types should both be able to have an instance of the class **_BitPack_** of CLaSH such that the functions **_pack_** and **_unpack_** can be applied to them.
  + The function type should not contain any separately defined types, because the Core plugin does not have access to them in the intermediate files. 
+ In order to compile the FPGA architecture with the listed representation of offloadable functions, all the targeted functions have to be within one or more Haskell modules that exclusively consists of CLaSH compilable functions.
+ It is required that the user only designs a set of offloaded functions that, in combination with the rest of the FPGA architecture and Xillybus IP core, will fit within the resource budget of the FPGA.
+ The user is required to fulfil the task of preventing non-deterministic behaviour by assigning the correct clock frequency to the FPGA architecture such that, in all possible system states, the propagation delays of the logic will not exceed the clock period.
+ The software partition of a Haskell co-design should be thread-safe when calling upon offloaded functions, such that no race conditions can occur.
+ All the offloadable functions should be in a **single** Haskell module that exclusively consists of CLaSH compilable functions (This occurs due to that this proof of concept does not take into account that multiple instances of the Core plugin can be active. In that case it would then result in duplicates of function identifiers and an incorrectly generated top-entity module for the hardware partition).

### Tutorial
1. Start with a Haskell software-only design in **_module A_**.

2. Re-write the functions that will be offloaded according to the previously listed requirements for automated offloading and transfer them from **_module A_** into **_module B_**, which now will become an imported module of **_module A_**. Subsequently, simulation should verify the desired functional behaviour before proceeding.

3. The following code is to be added to **_module B_** such that the Core plugin can be correctly invoked.

```
{-# OPTIONS_GHC -fplugin=Offload.Plugin.OffloadPlugin #-} -- requires ghc dynamic option '-package ghc' 

import Offload.Plugin.OffloadAnnotations -- required for annotations

import Offload.Haskell.OffloadFunctions
makeClassesInScope :: (BitPack Bit, KnownNat 0) => () -- required for the offloadplugin
makeClassesInScope = ()
```

4. The desired offloadable functions in **_module B_** should now be annotated using the following annotation whereas 'functionname' should be replaced with the actual offloadable function name. Using the 'OffloadMealy' or 'OffloadMoore' annotations does not offload a function in this proof of concept.
``` 
{-# ANN functionname OffloadPure #-}
```

5. Compile **_module A_** (with **_module B_** as an import) to an executable object file with GHC on the ARM, which results in the software partition of the co-design. The Core plugin will also automatically generate the top-entity module file that is used by CLaSH to generate the FPGA architecture that contains the hardware partition of the co-design.

6. The resulting top-entity module file has to be compiled with CLaSH (on the faster x86-based host PC), which will parametrize and generate the desired FPGA architecture in VHDL. The annotations and additional Core plugin code in **_module B_** should be removed again or commented out (actually, creating a copy of **_module B_** in step 2 and manually importing it instead is also possible) in order to allow compilation with CLaSH. 

7. The resulting VHDL files are to be added to the Quartus template project and synthesized to a programmable FPGA bitstream (on the faster x86-based host PC).
+ The FPGA bitstream should now be uploaded to the ARM in order to program the FPGA at boot-time. Once the FPGA has been configured, then the in step 5 generated executable object file may be used to start the HW/SW co-designed Haskell program.

## Manual function offloading
### Requirements
+ All Haskell functions that are to be offloaded should adhere to the following requirements:
  + The offloadable functions are completely compilable to a hardware description language\footnote{In this implementation only a VHDL Quartus project was prepared, but Verilog should be possible too with some additional work.} by CLaSH.
  + By default an offloadable function should be in the form of either one of the following abstract types \cite{ref:clashgentypes}:
    + Pure function: **_i -> o_**
    + Mealy function: **_s -> i -> (s, o)_**
    + Moore function: **_(s -> i -> s) -> (s -> o)_**
  + A composition of the previously mentioned offloadable functions and a higher-order data-flow function of CLaSH \cite{ref:dataflowclash} has to be exist, i.e. the function can be passed as an argument to either the **_pureDF_**, **_mealyDF_**, or **_mooreDF_** function.
  + The argument and result types should both be able to have an instance of the class **_BitPack_**\cite{ref:bitpackclash}, such that the functions **_pack_** and **_unpack_** can be applied to them.
+ In order to compile the FPGA architecture with the listed representation of offloadable functions, all the targeted functions have to be within one or more Haskell modules that exclusively consists of CLaSH compilable functions.
+ It is required that the user only designs a set of offloaded functions that, in combination with the rest of the FPGA architecture and Xillybus IP core, will fit within the resource budget of the FPGA.
+ The user is required to fulfil the task of preventing non-deterministic behaviour by assigning the correct clock frequency to the FPGA architecture such that, in all possible system states, the propagation delays of the logic will not exceed the clock period.
+ The software partition of a Haskell co-design should be thread-safe when calling upon offloaded functions, such that no race conditions can occur.
+ In addition to the three allowed basic offloadable function types (i.e. pure, mealy, and moore), it is also allowed to manually create sequential combinations of them, which are to be connected using the dataflow composition combinator function **_seqDF_** from CLaSH \cite{ref:dataflowclash}.
+ It is also allowed to manually offload a function that is **_Signal_** type based \cite{ref:clashgentypes}. It must be liftable to the dataflow type using the **_listDF_** function from CLaSH \cite{ref:dataflowclash}.
+ It is the user's task to apply the protocol parameters correctly to the polymorphic functions and FPGA architecture. This implies that the function identifiers should be unique and that they are matching between the hardware and software partitions. Additionally, the **_serialization naturals} required by the functions should be correctly given (either calculation using Template Haskell or manually).
+ It is the task of the user to modify the main program to allow for the sequential **_Mealy_** or **_Moore_** functions. This means removing the process of folding the states 
+ The user should use the pipelined optimization approach with only a single reading thread, unless the in \autoref{sec:pipelined} mentioned solutions are used. \\

### Tutorial
+ Start with a Haskell software-only design in \textit{module C}.

+ Re-write the functions that will be offloaded according to the previously listed requirements for manual offloading and move them into one or more separate modules, which we will call the \textit{module set D}. Subsequently, simulation should verify the desired functional behaviour before proceeding.

+ Create the top-entity module for the CLaSH compilation process of the hardware partition. A template file is available on \cite{ref:githubrepo}, which includes practical examples. Import the \textit{module set D} and correctly fill in the argument vector of offloadable functions of the \textit{topWrapper} function in the template file. 

+ The resulting top-entity module file has to be compiled with CLaSH, which will parametrize and generate desired FPGA architecture in VHDL.

+ The resulting VHDL files are to be added to the Quartus template project and synthesized to a programmable FPGA bitstream. 

+ The desired offloadable functions in the \textit{module set D} should now be manually replaced\footnote[6]{It is recommended to make a copy of \textit{module set D} here to retain the old functions} with the \textit{offloadTemplate} function or in the case of a pipelined design with the \textit{offloadWriteTemplate} and \textit{offloadReadTemplate} functions. This includes the correct assignment of \textit{function identifiers} and \textit{serialisation naturals}. The \textit{module C}, which calls upon the replaced offloadable functions, should now be modified accordingly. In the case of a pipelined utilization of the offloadable function, the \textit{module C} should be converted into a design with separate write and read threads.

+ Compile \textit{module C} (with the \textit{module set D} as an import) to an executable object file with GHC on the ARM, which results in the software partition of the co-design.

+ The FPGA bitstream is uploaded to the ARM in order to program the FPGA at boot-time. Once the FPGA has been configured, then the in step 7 generated executable object file may be used to start the HW/SW co-designed Haskell program.

add code examples


## General tasks
Guide is written with a linux host pc in mind and SD-card image used in this thesis. Any reproductions of the SD-card will most likely have different configurations.

### Connecting with the SoCKit
In the SoCKit setup of the proof of concept we can connect to the SoCKit by means of an SSH network connection, which can be achieved with the following information:

+ The static IP address: **_192.168.1.178_**
+ The default user: **_root_**
+ The default password: **_clash_**

For example, a linux operating system can connect to the SoCKit with the following command:
**_ssh root@192.168.1.178_**, and subsequently entering the password.

ref: https://linux.die.net/man/1/ssh

**backup method: serial usb-uart connection (see xillybus guide)**

### File transferring to (or from) the SoCKit
**_scp [-r] [source] [destination]_**

example: **_scp -r ./* root@192.168.1.178:/root/Haskell_**
or: **_scp soc\_system.rbf root@192.168.1.178:/root/UploadToFPGA_**

ref: https://linux.die.net/man/1/scp

### Compiling Haskell programs on the SoCKit
Compiling with ghc on ARM gives some warnings which may be ignored (?)

Use the command: **_ghc [Mainmodule.hs] [-o objectname] [-package ghc] [-main-is MainModuleName]}_**

The option **_-package ghc_** is required when you want to use the Core plugin/automated offloading

e.g.: **_ ghc MainModule.hs -o testMain -package ghc -main-is MainModule

ref: https://linux.die.net/man/1/ghc

### Compiling the FPGA architecture

How to generate a bit-file:

+ clash --vhdl "topentityfile"
+ Start Quartus II (13.0sp1)
+ Open the template offloading project (see below)
+ Add all *.vhdl files from vhdl/topentityfile/ to the project
+ Compile (Ctrl+L)

### Programming the FPGA
Refer to xillybus document or use the steps below with the thesis' SD-card image:

+ Convert the generated **_xillydemo.sof_** to **_soc_system.rbf_** using the 'File>convert programming files...' menu in Quartus
+ upload your 'soc_system.rbf' file to **_/root/UploadToFPGA/_**
+ execute **_./root/Upload/ToFPGA/UploadToFPGA_** and follow the text. This will place the file on the vfat partition of the  SD-card.
+ reboot the ARM to apply the new bitstream

### FPGA reset and error LEDs
error leds on the fpga

## Reproducing the thesis' SoCKit setup
The configuration of the SoCKit used in this proof of concept has been extensively modified from its original state as proposed by the Xillybus IP core and Linux image \cite{ref:xillybus}. The main purpose of these modifications was to allow for compiling and running Haskell code (version 7.10.3) on the ARM processor itself, as a cross-compiler was not capable of dynamic-loading and Template Haskell \cite{ref:TH_prob}. There are essentially two options that will result in an exact replicate of the SoCKit set-up used in this thesis. These options are:
+ Making an exact copy of the 32GB SD-card image used during this thesis.
+ Re-create the SD-card image by following the steps described in \autoref{sec:prepimage}.

Additionally the default Xillybus FPGA project has also been modified. A copy of the complete FPGA project is stored on the SD-card image, which may be downloaded to a host computer. The complete project will not be made available online on \todo{github} as it requires a license by Xillybus\cite{ref:licenses}. Instead, the steps to recreate it are provided in \autoref{sec:prepfpga}.\footnote{Or contact the owner of the github repository for a temporary download link}\todo{github} . 

### Re-creating the SD-card Linux image
This guide for preparing the SoCKit SD-card image starts off with the assumption that the Xillybus demonstration set-up has been completed, which may be achieved by downloading the associated files and following the guide(s) on \cite{ref:xillinuxguide}.

Now that the demonstration application set-up is completed and appears to be operating correctly, we may proceed with applying the modifications to the Xillinux image as enumerated below. To summarize the process, we attempt to install GHC version 7.10.3 and CλaSH on the ARM. We require this specific version because CλaSH does not work on older versions. As GHC builds upon specific libraries, we will have to update Ubuntu itself too. Each step can take a considerable amount of time and it is therefore recommended to make a backup after each successful Ubuntu upgrade to reduce time-loss. GHC 7.10.3 is a standard package available in the universal repository of Ubuntu 16.04 LTS (_Xenial_), but upgrading the complete system to Xenial can (and in our initial case will) break the OS. The solution we used was to only upgrade to Ubuntu 14.04 (_Trusty_) and to install GHC 7.10.3 and its dependencies from the universal repository of Ubuntu 16.04 (_Xenial_).

1. Provide the SoCKit with internet access such that the package manager can be used. It heavily recommended to add a swap partition on the SD-card (1GB+).

3. Fully update the standard Ubuntu version 12.04 LTS (_precise_).

4. Upgrade the Ubuntu image to version 14.04 LTS (_trusty_) through the package manager. This can be achieved by first changing the default Xillinux '_precise_' repository codename to the '_trusty_' codename in the '_/etc/apt/sources.list_' file.

5. Temporarily add the '_universal_' Xenial repository (possibly also the '_main_' and '_restricted_' repos) to the '_/etc/apt/sources.list_' and subsequently install GHC 7.10.3 and its dependencies. Afterwards the Xenial repositories can be removed again. (It is not recommended to upgrade to Ubuntu 16.04 LTS (_Xenial_) because it breaks the OS!)

6. Use cabal to install the '_ghc-paths_' (used for the Core plugin) and the most recent '_CλaSH_' Haskell packages (_CλaSH_  version 0.6.19 was used in this thesis).

After this process Haskell and CλaSH  should be fully functioning on the ARM. If the FPGA project from the thesis is available, then an (almost) identical copy of the SoCKit set-up is prepared. In the next section a guide is written on how to recreate the FPGA project with merely downloading the top-level entity file found on \todo{refer to github}.

### Re-creating the FPGA project
The template FPGA project that is used for this thesis can also be recreated. It starts with the default demonstration project provided by Xillybus \cite{ref:xillinuxguide}, which is only compatible with Quartus II version 13.0sp1. It is assumed that the standard process of generating a FPGA programmable bit-stream as proposed by Xillybus is performed at least once such that all required files are generated. The following steps are necessary to recreate the project as used in this thesis:

+ Generate the correct Xillybus IP core on the IP factory website \cite{ref:ipfactory} (Requires registering on the website). The IP core should be configured similarly to the configuration displayed in \autoref{fig:coreconfig}.
+ Follow the **_README.TXT_** file provided with the generated core to apply the IP core changes into the default Quartus project. The step to update the top-level entity file with the generated template HDL may be skipped.
+ Replace the top-level '**_xillydemo.vhd_**' file with the top-level entity file as provided on \todo{url github}. 
+ Use the pin assignment file on \todo{github} to add the correct pin assignments for the push-buttons and 50MHz clock. This may also be done manually if desired.
