Class Network
    Data Members:
        double eta
        int batch size
        int epochs
        int array layer sizes
        vector<matrix> weights
        vector<matrix> biases
        vector<matrix> activations
        vector<matrix> weighted inputs
        vector<matrix> nabla_b
        vector<matrix> nabla_w
        string training data filename
        string expected values filename
    Methods:
        Constructor (requires eta, batch size, layer sizes, file names, and epochs provided by the ReadIn static member function)
            Generate L,which is the number of layers. This is given by sizeof(layer sizes). 
            Weights, biases, activations, weighted inputs, nabla_w, nabla_b resized to size L.
                Everything but the activation vector will have an effective size of L-1, as their first element will be left unused.
            Looping an index i from 1 to L
                resize the ith element of the weights vector to be layer sizes at i by layer sizes at i-1
                resize the ith element of the nabla_w vector to be layer sizes at i by layer sizes at i-1, fill with zeros
                resize the ith element of the biases vector to be layer sizes at i by 1, fill with random numbers, mean 0, st. dev. 1
                resize the ith element of the nabla_b vector to be layer sizes at i by 1, fill with zeros
                resize the ith element of the activations vector to be layer sizes at i by 1, fill with zeros
                resize the ith element of the weighted inputs vector to be layer sizes at i by 1, fill with zeros
            resize the 0th member of the activations vector to be layer sizes at 0 by 1, fill with zeros
            Populate ith element in the array of weights vector with pseudorandom numbers, mean 0, st. dev. 1/Sqrt[layer sizes at i-1]
            Set object's hyperparameters to values passed to constructor
       
        forwardPass is a function that sets all activation values for a single test data input. 
        It needs the layer of activations at 0 to be assigned values from the test data.
        void forwardPass (no parameters).
            Looping an index i from 1 to L
                activations at i is equal to the sigmoid function of the weighted inputs at i,
                    where the weighted inputs at i is set to be equal to (weights at i * activations at i-1)+biases at i
                    
        backProp is a function that calculates the nabla_b and nabla_w vectors.
        backProp requires sigmoid_prime function, cost derivative function, activations and weighted inputs must have already been set
        void backProp (no paramaters)
            nabla_b at L = calculated error of the output layer
            Looping an index i from L-1 to 1 (the remaining layers)
                nabla_b at i = ((weights at i + 1 transposed)*(nabla_b at i + 1)) hadamard product with sigmoid_prime at i
