#include<iostream>
#include<fstream>
#include<string.h>
#include<vector.h>
#include<dlib/matrixabstract.h>

Class Network
{
    Data Members:
        double eta;
        int batch_size;
        int epochs;
        int layer_sizes[];          //array of layer sizes
        int L;                      //number of layers i.e. sizeof(layer_sizes)
        vector<matrix> weights;
        vector<matrix> biases;
        vector<matrix> activations;
        vector<matrix> weighted_inputs;
        vector<matrix> nabla_b;
        vector<matrix> nabla_w;
        string training_data_filename;
        string expected values_filename;
    Methods:
        Constructor(requires eta, batch size, layer sizes, file names, and epochs provided by the ReadIn static member function)
        {
            //Assign L, which is the number of layers. This is given by sizeof(layer sizes). 
            //Weights, biases, activations, weighted inputs, nabla_w, nabla_b vectors resized to size L.
            //Everything but the activation vector will have an effective size of L-1, as their first element will be left unused.
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
        }
       
        //forwardPass() is a function that sets all activation values for a single test data input. 
        //It needs the layer of activations at 0 to be assigned values from the test data.
        void forwardPass()
        {
            Looping an index i from 1 to L
                activations at i is equal to the sigmoid function of the weighted inputs at i,
                    where the weighted inputs at i is set to be equal to (weights at i * activations at i-1) + biases at i
        }
                    
        //backProp() is a function that calculates the nabla_b and nabla_w vectors.
        //backProp requires sigmoid_prime function, cost derivative function, activations and weighted inputs already been set
        void backProp()
        {
            nabla_b at L = calculated error of the output layer using cost derivative function
            Looping an index i from L-1 to 1 (the remaining layers, save for the zeroth)
                nabla_b at i += ((weights at i + 1 transposed)*(nabla_b at i + 1)) hadamard product with sigmoid_prime at i
                nabla_w at i += (nabla_b at i)(activations at i-1 transposed)
        }
        
        //SGD() is a function that loads a test sample, runs forwardPass() and backProp(), and repeats a number of times equal to the
        //mini-batch size. Once this is completed, SGD will call update() to update the weights and biases, reset the nabla_b and
        //nabla_w vectors, and continue until the entirety of the test data from training_data_filename is exhausted. This process
        //is repeated a number of times equal to 
        string SGD()
        
        
        
        static void ReadIn();
};

//Just realized we can make ReadIn() to be a static member function of the class Network. It can be called directly in the main(), 
//before even declaring any object of the class because it is a static function. And, we can read in the required parameters and assign
//the values before calling the constructor. So, the constructor doesn't need any parameter if we make ReadIn() a static member function 
//of the class Network. //@Yon- YESS :D that's why i really wanted to make it a static method. It's a separate entity than any specific 
                        //      instantiation but still retains it's class membership
void main()
{
    Network::ReadIn();      //that's how we can call it before declaring the object of the class
}
