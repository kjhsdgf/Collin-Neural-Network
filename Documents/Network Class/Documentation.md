Tabs; 4; No Wrap

## Member Summary
Type | Name | Description
----- | ------- | -------------
`double` | `learning_rate` | Multiplier for updating weights and biases 
`int` | `batch_size` | Size of mini batches to take from test data
`int` | `epochs` | Number of times to "train" the network // consider changing name to more descriptive one
`vector<int>` | `layer_sizes` | Array containing sizes of each layer
`int` | `num_Layers` | Number of layers in the network 
`int` | `num_correct_outputs` | Number of times network output matches truth values for test data 
`vector<matrix>` | `weights` | Vector of matrices containing all the weights for the network
`vector<matrix>` | `biases` | Vector of matrices containing all the biases for the network
`vector<matrix>` | `activations` | Vector of matrices containing all the activation values of the network
`vector<matrix>` | `weighted_sums` | Vector of matrices containing all the weighted sums of the network
`vector<matrix>` | `errors` | Vector of matrices containing all errors at each node of the network 
`vector<matrix>` | `sum_nabla_b` | Vector of matrices containing the sum of the dC/db at each node of the network
`vector<matrix>` | `sum_nabla_w` | Vector of matrices containing the sum of the dC/dw at each node of the network
`vector<matrix>` | `Expected_values` | Vector of matrices containing the expected values for each training data input 
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
`matrix&` | `Hadamard_Product (const matrix& M1, const matrix& M2)` |Performs the Hadamard Product operation on any two given matrices 
`void` | `Shuffle(vector<T> data)` | Applies Fisher-Yates shuffle to randomize elements in a vector

## Member Details
### learning_rate
	double learning_rate;  
Positive multiplier for updating weights and biases.  
Generally, the learning rate is less than 1

### batch_size
	int batch_size;
Size of the small samples of randomly chosen training inputs from the test data file.
Provided, the sample size should be large enough to speedup the learning process of the network.

### epochs
	int epochs;
A hyperparameter that denotes the number of times the samples of finite size needs to be chosen randomly, such that the training inputs are exhausted. 
This number is usually provided by the user rather than a randomly selected value by the learning algorithm.	
	
### layer_sizes
	vector<int> layer_sizes;
Records the size of every layer in the network, once given a source by the user. Every required matrix gets its size using this parameter.
	
### num_Layers
	int num_Layers;
Stores information about the depth of the network. This helps in randomly accessing any particular layer in tbe network. 
	
### num_correct_output
	int num_correct_output;
A 
	
### weights
	vector<matrix> weights;
	
### biases
	vector<matrix> biases;
	
### activations
	vector<matrix> activations;
	
### weighted_sums
	vector<matrix> weighted_sums;
	
### errors
	vector<matrix> errors;
	
### sum_nabla_b
	vector<matrix> sum_nabla_b;

### sum_nabla_w
	vector<matrix> sum_nabla_w;
	
### Expected_Values
	vector<matrix> Expected_values;
	
### test_data_indices
	vector<int> test_data_indices;
	
### mini_batch_indices
	vector<int> mini_batch_indices;
	
### test_data_file
	string test_data_file;
	
### expected_values_file
	string expected_values_file;

## Constructor Details

## Method Details
