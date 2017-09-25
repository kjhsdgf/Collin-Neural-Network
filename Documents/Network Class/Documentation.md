Tabs; 4; No Wrap

## Member Summary
Type | Name | Description
----- | ------- | -------------
`double` | `learning_rate` | multiplier for updating weights and biases // was eta
`int` | `batch_size` | size of mini batches to take from test data
`int` | `epochs` | number of times to "train" the network
`int*` | `layer_sizes` | int array containing sizes of each layer
`int` | `num_Layers` | number of layers in the network // was L
`int` | `num_correct_outputs` | number of times network output matches truth values for test data 
`vector<matrix>` | `weights` | vector of (j x k) matrices containing all the weights for the network
`vector<matrix>` | `biases` | vector of (j x 1) matrices containing all the biases for the network
`vector<matrix>` | `activations` | vector of (j x 1) matrices containing all the activation values of the network
`vector<matrix>` | `weighted_sums` | vector of (j x 1) matrices containing all the weighted sums of the network
`vector<matrix>` | `errors` | vector of (j x 1) matrices containing all errors at each node of the network // was delta
`vector<matrix>` | `sum_nabla_b` | vector of (j x 1) matrices containing the sum of the dC/db at each node of the network
`vector<matrix>` | `sum_nabla_w` | vector of (j x k) matrices containing the sum of the dC/dw at each node of the network
`vector<int>` | `test_data_indices` | array of all indices corresponding to the test data
`vector<int>` | `mini_batch_indices` | array of indices of size batch_size corresponding to the test data
`string` | `test_data_file` | name of file of test data inputs
`string` | `expected_values_file` | name of file of expected values that correspond to the test data inputs

## Constructor Summary
Constructor | Description
------------- | -------------
`Network (istream& in = cin)` | constructs an untrained Network object that represents the data read from the "in" istream
`Network (const string& network_file, const string& classification_data_file)` | reconstructs a [trained] network and classifies it
`~Network ()` | deallocates the Network object's dynamic memory and closes all files
	
## Method Summary
Return type | Name | Description
-------------|------|-------------
`string&` | `ReadInit (istream& in)` | A method to read the required hyperparameters of the class from the file provided by the user
`string&` | `ReadInit ()` | An overloaded method to read the required parameters from the console
`void` | `ForwardPass()` | A method that sets all activation values and weighted inputs for a single test data input
`void` | `BackProp (const matrix& Expected_values)` | A method to backpropagate the error and to get the sum of cost partials
`void` | `SGD (const int batch_size)` | Stochastic Gradient Descent method to perform forward pass and backpropagation once for the mini batch and update the weights and biases
`void` | `Train()` | A method which loops through the epochs performing SGD for all the mini batches in the test data for each epoch
`void` | `Update()` | A method called by SGD() to update the weights and biases in the network
`void` | `Classify ()` | A method to classify the data file provided by the user and display the efficiency based on the classified data 
`void` | `Randomize_Weights()` | A method called by the default constructor to randomly assign the weights to the weight matrices
`bool` | `Create_Network_File()` | A method to create a file for storing in the required values of the network built	
`matrix&` | `Hadamard_Product (const matrix& M1, const matrix& M2)` | A method to perform the Hadamard Product operation on any two matrices 
`void` | `Shuffle(vector<int> data)` | A method to shuffle the indices at the beginning of every epoch

## Member Details

## Constructor Details

## Method Details
