Tabs; 4; No Wrap

## Member Summary
Type | Name | Description
----- | ------- | -------------
`double` | `learning_rate` | Multiplier for updating weights and biases // was eta
`int` | `batch_size` | Size of mini batches to take from test data
`int` | `epochs` | Number of times to "train" the network // consider changing name to more descriptive one
`int*` | `layer_sizes` | Array containing sizes of each layer
`int` | `num_Layers` | Number of layers in the network // was L
`int` | `num_correct_outputs` | Number of times network output matches truth values for test data 
`vector<matrix>` | `weights` | Vector of (j x k) matrices containing all the weights for the network
`vector<matrix>` | `biases` | Vector of (j x 1) matrices containing all the biases for the network
`vector<matrix>` | `activations` | Vector of (j x 1) matrices containing all the activation values of the network
`vector<matrix>` | `weighted_sums` | Vector of (j x 1) matrices containing all the weighted sums of the network
`vector<matrix>` | `errors` | Vector of (j x 1) matrices containing all errors at each node of the network // was delta
`vector<matrix>` | `sum_nabla_b` | Vector of (j x 1) matrices containing the sum of the dC/db at each node of the network
`vector<matrix>` | `sum_nabla_w` | Vector of (j x k) matrices containing the sum of the dC/dw at each node of the network
`vector<int>` | `test_data_indices` | Array of all indices corresponding to the test data
`vector<int>` | `mini_batch_indices` | Array of indices of size batch_size corresponding to the test data
`string` | `test_data_file` | Name of file of test data inputs
`string` | `expected_values_file` | Name of file of expected values that correspond to the test data inputs

## Constructor Summary
Constructor | Description
------------- | -------------
`Network (istream& in = cin)` | Constructs an untrained Network object that represents the data read from the "in" istream
`Network (const string& network_filename, const string& data_filename)` | Reconstructs a [trained] network and classifies it
`~Network ()` | Deallocates the Network object's dynamic memory and closes all files
	
## Method Summary
Return type | Name | Description
-------------|------|-------------
`string&` | `ReadInit (string& hyperparam_filename)` | Reads the required hyperparameters of the class from a file provided by the user
`string&` | `ReadInit ()` | Reads the required hyperparameters from the console `cin`
`void` | `ForwardPropagation ()` | Sets all activation values and weighted inputs for a single test data input
`void` | `BackPropagation (const matrix& Expected_values)` | Backpropagates through network to compute error at each node
`void` | `SGD()` | Stochastic Gradient Descent: performs forward and back propagation once for each input in the mini batch then updates the weights and biases accordingly
`void` | `Train()` | Trains network by repeatedly performing SGD on randomized mini batches of the test data for as many epochs as specified
`void` | `Update()` | Updates weights and biases based on data from SGD
`void` | `Classify ()` | Classifies the data file provided by the user and displays the efficiency based on the classified data 
`void` | `Randomize_Matrix(matrix& M)` | Assigns Gaussian normally distributed set of random numbers for each element in a matrix
`bool` | `Create_Network_File(string& network_filename)` | Writes a file to store the network	
`matrix&` | `Hadamard_Product (const matrix& M1, const matrix& M2)` | A method to perform the Hadamard Product operation on any two matrices 
`void` | `Shuffle(vector<T> data)` | Applies Fisher-Yates shuffle to randomize elements in a vector

## Member Details
hello
### learning_rate
	`hello`
	this is descriptive
	more description
### batch_size
### epochs
### layer_sizes
### num_Layers
### num_correct_output
### weights
### biases
### activations
### weighted_sums
### errors
### sum_nabla_b
### sum_nabla_w
### test_data_indices
### mini_batch_indices
### test_data_file
### expected_values_file

## Constructor Details

## Method Details
