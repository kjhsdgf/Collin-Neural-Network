Tabs; 4; No Wrap

## Member Summary
Type | Name | Description
----- | ------- | -------------
`double` | `learning_rate` | multiplier for updating weights and biases // was eta
`int` | `batch_size` | size of mini batches to take from test data
`int` | `epochs` | number of times to "train" the network
`int` | `layer_sizes[]` | int array containing sizes of each layer
`int` | `num_Layers` | number of layers in the network // was L
`int` | `num_correct_outputs` | number of times netowrk output matches truth values for test data 

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
	Network	(istream& in = cin)		// constructs an untrained Network object that represents the data read from the "in" istream
	Network	(ifstream& network_file, ifstream& classification_data_file)
									// reconstructs a [trained] network from network file and calls classify on classification data
	~Network ()						// destructs a Network objects dynamic memory and closes all files
	
## Method Summary
	string&		ReadInit (istream& in)
	string&		ReadInit ()
	
	// main algorithm methods	
	void		ForwardPass()
	void		BackwardPass()
	void		SGD()
	void		Train
	void 		updata
	
	// helper methods
	


## Member Details

## Constructor Details

## Method Details
