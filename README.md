This readme is not completely finished!! Some references are broken and the manual offloading tutorial is not completely finished.

# Haskell-FunctionOffloading
Proof of concept for a thesis on the offloading of Haskell functions onto an FPGA. Thesis link: http://essay.utwente.nl/71486/

This readme file contains a more in-depth use guide as opposed to the thesis report.

Guide contents:
+ [Semi-automated offloading guide](https://github.com/jjvanvossen/Haskell-FunctionOffloading/#semi-automated-function-offloading-guide)
+ [Manual offloading guide](https://github.com/jjvanvossen/Haskell-FunctionOffloading#manual-function-offloading)
+ [General tasks](https://github.com/jjvanvossen/Haskell-FunctionOffloading#general-tasks)
+ [Reproducing thesis setup](https://github.com/jjvanvossen/Haskell-FunctionOffloading#reproducing-the-thesis-sockit-setup)


## Semi-automated function offloading guide
This guide describes how to use the thesis' implementation for semi-automated offloading of annotated Haskell functions. Refer to the thesis and the code for more design and implementation detail. First the requirements are described and subsequently the tutorial is given. An example can be found in the folder _/examples/auto/_ of this repository.

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

5. Compile **_module A_** (with **_module B_** as an import) to an executable object file (with ghc option **_-package ghc_**) with GHC on the ARM, which results in the software partition of the co-design. The Core plugin will also automatically generate the top-entity module file that is used by CLaSH to generate the FPGA architecture that contains the hardware partition of the co-design.

6. The resulting top-entity module file has to be compiled with CLaSH (on the faster x86-based host PC), which will parametrize and generate the desired FPGA architecture in VHDL. The annotations and additional Core plugin code in **_module B_** should be removed again or commented out (actually, creating a copy of **_module B_** in step 2 and manually importing it instead is also possible) in order to allow compilation with CLaSH. 

7. The resulting VHDL files are to be added to the Quartus template project and synthesized to a programmable FPGA bitstream (on the faster x86-based host PC).
+ The FPGA bitstream should now be uploaded to the ARM in order to program the FPGA at boot-time. Once the FPGA has been configured, then the in step 5 generated executable object file may be used to start the HW/SW co-designed Haskell program.

## Manual function offloading
### Requirements
+ All Haskell functions that are to be offloaded should adhere to the following requirements:
  + The offloadable functions are completely compilable to a hardware description language (in this implementation only a VHDL Quartus project was prepared, but Verilog should be possible too with some additional work) by CLaSH.
  + By default an offloadable function should be in the form of either one of the following abstract types:
    + Pure function: **_i -> o_**
    + Mealy function: **_s -> i -> (s, o)_**
    + Moore function: **_(s -> i -> s) -> (s -> o)_**
  + A composition of the previously mentioned offloadable functions and a higher-order data-flow function of CLaSH has to be exist, i.e. the function can be passed as an argument to either the **_pureDF_**, **_mealyDF_**, or **_mooreDF_** function.
  + The argument and result types should both be able to have an instance of the class **_BitPack_** from CLaSH, such that the functions **_pack_** and **_unpack_** can be applied to them.
+ In order to compile the FPGA architecture with the listed representation of offloadable functions, all the targeted functions have to be within one or more Haskell modules that exclusively consists of CLaSH compilable functions.
+ It is required that the user only designs a set of offloaded functions that, in combination with the rest of the FPGA architecture and Xillybus IP core, will fit within the resource budget of the FPGA.
+ The user is required to fulfil the task of preventing non-deterministic behaviour by assigning the correct clock frequency to the FPGA architecture such that, in all possible system states, the propagation delays of the logic will not exceed the clock period.
+ The software partition of a Haskell co-design should be thread-safe when calling upon offloaded functions, such that no race conditions can occur.
+ In addition to the three allowed basic offloadable function types (i.e. pure, mealy, and moore), it is also allowed to manually create sequential combinations of them, which are to be connected using the dataflow composition combinator function **_seqDF_** from CLaSH.
+ It is also allowed to manually offload a function that is **_Signal_** type based. It must be liftable to the dataflow type using the **_listDF_** function from CLaSH \cite{ref:dataflowclash}.
+ It is the user's task to apply the protocol parameters correctly to the polymorphic functions and FPGA architecture. This implies that the function identifiers should be unique and that they are matching between the hardware and software partitions. Additionally, the **_serialization naturals_** required by the functions should be correctly given (either calculation using Template Haskell or manually).
+ It is the task of the user to modify the main program to allow for the sequential **_Mealy_** or **_Moore_** functions. This means removing the process of folding the states 
+ The user should use the pipelined optimization approach with only a single reading thread, unless the in \autoref{sec:pipelined} mentioned solutions are used. \\

### Tutorial
1. Start with a Haskell software-only design in **_module 1_**.

2. Re-write the functions that will be offloaded according to the previously listed requirements for manual offloading and move them into one or more separate modules, which we will call the **_module set 2_**. Subsequently, simulation should verify the desired functional behaviour before proceeding.

3. Create the top-entity module for the CLaSH compilation process of the hardware partition. A template file is available in this repository, which includes practical examples. Import the **_module set 2_** and correctly fill in the argument vector of offloadable functions of the **_topWrapper_** function in the template file. 

todo code examples

4. The resulting top-entity module file has to be compiled with CLaSH, which will parametrize and generate desired FPGA architecture in VHDL. 

5. The resulting VHDL files are to be added to the Quartus template project and synthesized to a programmable FPGA bitstream. 

6. The desired offloadable functions in the **_module set 2_** should now be manually replaced (it is recommended to make a copy of **_module set 2_** here to retain the old functions) with the **_offloadTemplate_** function or in the case of a pipelined design with the **_offloadWriteTemplate_** and **_offloadReadTemplate_** functions. This includes the correct assignment of **_function identifiers_** and **_serialisation naturals_**. The **_module 1_**, which calls upon the replaced offloadable functions, should now be modified accordingly. In the case of a pipelined utilization of the offloadable function, the **_module 1_** should be converted into a design with separate write and read threads.

7. Compile **_module 1_** (with the **_module set 2_** as an import) to an executable object file with GHC on the ARM, which results in the software partition of the co-design.

8. The FPGA bitstream is uploaded to the ARM in order to program the FPGA at boot-time. Once the FPGA has been configured, then the in step 7 generated executable object file may be used to start the HW/SW co-designed Haskell program.

add code examples


## General tasks
This guide is written with a linux host pc in mind and with the SD-card image used in this thesis. Any reproductions of the SD-card will most likely have different configurations. In most cases the Xillybus IP core documents can also be used for information.

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
To exchange data between the SoCKit and a development PC the following command can be used: **_scp [-r] [source] [destination]_**

example: **_scp -r ./* root@192.168.1.178:/root/Haskell_**
or: **_scp soc\_system.rbf root@192.168.1.178:/root/UploadToFPGA_**

ref: https://linux.die.net/man/1/scp

### Compiling Haskell programs on the SoCKit

Use the command: **_ghc [Mainmodule.hs] [-o objectname] [-package ghc] [-main-is MainModuleName]}_**
The option **_-package ghc_** is only required when you want to use the Core plugin/automated offloading
Note: compiling with ghc on ARM gives linker warnings which may be ignored.

e.g.: **_ ghc MainModule.hs -o testMain -package ghc -main-is MainModule_**

ref: https://linux.die.net/man/1/ghc

### Compiling the FPGA architecture

How to generate a FPGA programmable bit-stream:

+ Compile the CLaSH 'topentityfile' to VHDL with: **_clash --vhdl "topentityfile"_**
+ Start Quartus II (13.0sp1)
+ Open the template offloading project (see below)
+ Add all *.vhdl files from vhdl/topentityfile/ to the project
+ Compile (Ctrl+L)

### Programming the FPGA
Refer to xillybus document or use the steps below with the thesis' SD-card image:

+ Convert the generated **_xillydemo.sof_** to **_soc_system.rbf_** using the 'File>convert programming files...' menu in Quartus
+ upload your 'soc_system.rbf' file to **_/root/UploadToFPGA/_**
+ execute **_./root/Upload/ToFPGA/UploadToFPGA_** and follow the text. This will place the file on the vfat partition of the SD-card.
+ reboot the ARM to apply the new bitstream

### FPGA reset and error LEDs
The pin assignment in the template Quartus project are configured to have the following functions:
+ KEY0 = reset of FPGA architecture and FIFO IP cores
+ LED0 = Error LED for incorrect received message meaning either:

  + Wrong header
  + Wrong function ID (i.e. less than 0 or higher than max amount of functions)
  + Wrong message length (i.e. less than 1)

+ LED1 = Error LED for incorrect data message length as determined in the deserialization logic
+ LED2 = Unused error LED
+ LED3 = Unused error LED

## Reproducing the thesis' SoCKit setup
The configuration of the SoCKit used in this proof of concept has been extensively modified from its original state as proposed by the [Xillybus IP core and Linux image](http://xillybus.com/). The main purpose of these modifications was to allow for compiling and running Haskell code (version 7.10.3) on the ARM processor itself, as a cross-compiler was not capable of dynamic-loading and Template Haskell. There are essentially two options that will result in an exact replicate of the SoCKit set-up used in this thesis. These options are:
+ Making an exact copy of the 32GB SD-card image used during this thesis.
+ Re-create the SD-card image by following the steps described in the next section.

Additionally the default Xillybus FPGA project has also been modified. A copy of the complete FPGA project is stored on the SD-card image, which may be downloaded to a host computer. The complete project will not be made available in this repository as it requires a license by Xillybus. Instead, the steps to recreate it are provided in the last section of this readme file (Or contact the owner of this repository for a temporary download link). 

### Re-creating the SD-card Linux image
This guide for preparing the SoCKit SD-card image starts off with the assumption that the Xillybus demonstration set-up has been completed, which may be achieved by downloading the associated files and following the [guide(s)](http://xillybus.com/doc).

Now that the demonstration application set-up is completed and appears to be operating correctly, we may proceed with applying the modifications to the Xillinux image as enumerated below. To summarize the process, we attempt to install GHC version 7.10.3 and CλaSH on the ARM. We require this specific version because CλaSH does not work on older versions. As GHC builds upon specific libraries, we will have to update Ubuntu itself too. Each step can take a considerable amount of time and it is therefore recommended to make a backup after each successful Ubuntu upgrade to reduce time-loss. GHC 7.10.3 is a standard package available in the universal repository of Ubuntu 16.04 LTS (_Xenial_), but upgrading the complete system to Xenial can (and in our initial case will) break the OS. The solution we used was to only upgrade to Ubuntu 14.04 (_Trusty_) and to install GHC 7.10.3 and its dependencies from the universal repository of Ubuntu 16.04 (_Xenial_).

1. Provide the SoCKit with internet access such that the package manager can be used. It heavily recommended to add a swap partition on the SD-card (1GB+).

3. Fully update the standard Ubuntu version 12.04 LTS (_precise_).

4. Upgrade the Ubuntu image to version 14.04 LTS (_trusty_) through the package manager. This can be achieved by first changing the default Xillinux '_precise_' repository codename to the '_trusty_' codename in the '_/etc/apt/sources.list_' file.

5. Temporarily add the '_universal_' Xenial repository (possibly also the '_main_' and '_restricted_' repos) to the '_/etc/apt/sources.list_' and subsequently install GHC 7.10.3 and its dependencies. Afterwards the Xenial repositories can be removed again. (It is not recommended to upgrade to Ubuntu 16.04 LTS (_Xenial_) because it breaks the OS!)

6. Use cabal to install the '_ghc-paths_' (used for the Core plugin) and the most recent '_CλaSH_' Haskell packages (_CλaSH_  version 0.6.19 was used in this thesis).

After this process Haskell and CλaSH  should be fully functioning on the ARM. If the FPGA project from the thesis is available, then an (almost) identical copy of the SoCKit set-up is prepared. In the next section a guide is written on how to recreate the FPGA project with merely downloading the top-level entity file found in this repository at **_FPGAtemplate/offloadHaskellArch.vhd_**.

### Re-creating the FPGA project
The template FPGA project that is used for this thesis can also be recreated. It starts with the [default demonstration project provided by Xillybus](http://xillybus.com/xillinux), which is only compatible with Quartus II version 13.0sp1. It is assumed that the standard process of generating a FPGA programmable bit-stream as proposed by Xillybus is performed at least once such that all required files are generated. The following steps are necessary to recreate the project as used in this thesis:

+ Generate the correct Xillybus IP core on the [Xillybus IP factory webpage](http://xillybus.com/custom-ip-factory) (Requires registering on the website). The IP core should be configured as an 32 bits up- and downstream FIFO pair similarly to the following configuration:
  + Upstream, 32 bits, (and for example: 200 MB/s, autoset, general exchange with coprocessor)
  + Downstream, 32 bits, (and for example: 200 MB/s, autoset, general exchange with coprocessor)
+ Follow the **_README.TXT_** file provided with the generated core to apply the IP core changes into the default Quartus project. The step to update the top-level entity file with the generated template HDL may be skipped.
+ Replace the top-level '**_xillydemo.vhd_**' file with the top-level entity file as provided in this repository at **_FPGAtemplate/offloadHaskellArch.vhd_**. 
+ Use the importable pin assignment file in this repository (**_FPGAtemplate/offloadHaskellArch.qsf_**) to add the correct pin assignments for the push-buttons and 50MHz clock. This may also be done manually, if desired.
