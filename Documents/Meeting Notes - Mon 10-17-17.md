# Priorities
## Stuff Eli thinks is really important to sort out before we continue
1. ### Have a `master` branch that runs on everyone's computer...
	* #### ideally self-contained
2. ### Code commentary for the next generation of developers on this code

## Long-term Goals
1. ### filetype-to-binary data conversion
	* #### visual image to binary
	* #### audial image to binary
	* #### etc.
2. ### Graphical User Interface (GUI)

## Concrete Improvements
1. ### Read/write files in binary mode
2. ### Redefine `ambiguousData()` to implement a threshold value
3. ### Save file path for training/expected data files
4. ### Introduce additional cost and activation functions (set up menu of defaults)
	* #### Updating initialization to allow for these selections
5. ### Running the program from console (i.e. making a `main()` with `argc` and `argv[]`)
6. ### Expand testing
	* #### Add noise to training/validation data
	* #### Try to feed the network "incorrect" data
	* #### Test networks with different numbers of input/output neurons
		* ##### Need to generate this data somehow (may be handled by Data class already)
