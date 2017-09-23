#include<iostream>
#include<fstream>
#include<string.h>
#include<vector.h>
#include<dlib/matrixabstract.h>

using namespace std;
using namespace dlib;

Class Network
{
    Data Members:
        double eta;
        int batch_size;
        int epochs;
        int layer_sizes[];          //array of layer sizes
        vector<matrix> weights;
        vector<matrix> biases;
        vector<matrix> activations;
        vector<matrix> weighted_inputs;
        vector<matrix> nabla_b;
        vector<matrix> nabla_w;
        vector<matrix> errors;
        string training_data_filename;
        string expected_output_filename;
    Methods:
    
        //Constructor that initializes the neural network using values provided by ReadIn() 
        //Weights, biases, activations, weighted inputs, nabla_w, nabla_b vectors resized to size L.
        //Everything but the activation vector will have an effective size of L-1, as their first element will be left unused.
        Network(requires eta, batch size, layer sizes, file names, and epochs provided by the ReadIn static member function)
        {
            //Generate L, which is the number of layers. This is given by sizeof(layer sizes).
            Assign L, which is the number of layers. This is given by sizeof(layer sizes)
            Weights, biases, activations, weighted inputs, errors, nabla_w, nabla_b vectors resized to size L.
            //Everything but the activation vector will have an effective size of L-1, as their first element will be left unused.
            Looping an index i from 1 to L
                resize the ith element of the weights vector to be layer sizes at i by layer sizes at i-1
                resize the ith element of the nabla_w vector to be layer sizes at i by layer sizes at i-1, fill with zeros
                resize the ith element of the biases vector to be layer sizes at i by 1, fill with random numbers, mean 0, st. dev. 1
                resize the ith element of the nabla_b vector to be layer sizes at i by 1, fill with zeros
                resize the ith element of the activations vector to be layer sizes at i by 1, fill with zeros
                resize the ith element of the weighted inputs vector to be layer sizes at i by 1, fill with zeros
                resize the ith element of the errors vector to be layer sizes at i by 1, fill with zeros
            resize the 0th member of the activations vector to be layer sizes at 0 by 1, fill with zeros
            Populate ith element in the array of weights vector with pseudorandom numbers, mean 0, st. dev. 1/Sqrt[layer sizes at i-1]
            Set object's hyperparameters to values passed to constructor by ReadIn()
        }
        
        //Default Constructor that makes an empty Network
        Network()
        {
            Set all vector<matrix> sizes to 0 by 0;
            Set _filename strings to empty strings ("")
            Set remaining int and double member attributes to 0
        }
        
        //Because we are using quadratic cost C(w, b) = [1/(2n)]Sum(x = 1, n, ||y(x) - a||^2), the first derivative of our cost function
        //will be a vector of the scalar distances between the output layer and the expected output, evaluated elementwise
        //Cost_prime returns a matrix of doubles
        matrix<double> cost_prime(expected output matrix object, activations at L) 
        {
            return absolute value of (activations at L - expected output matrix object)
        }
        
        //Activation function we chose to start with, uses dlib's vectorized sigmoid function
        matrix<double> sigmoid(matrix<double> object) 
        {
            return the output of sigmoid(input matrix)
        }
        
        //First derivative of sigmoid function, takes advantage of the fact that the derivative can be expressed in terms of the
        //original function, uses dlib's vectorized sigmoid function
        matrix<double> sigmoid_prime(matrix<double> object) 
        {
            return sigmoid(input)*(1 - sigmoid(input))
        }
        
        //Function updates the weights and biases, as per Nielsen's basic algorithm in Ch.1
        void update() 
        {
            Looping an index i from 1 to L
                weights at i -= (eta/batch_size)*(nabla_w at i)
                biases at i -= (eta/batch_size)*(nabla_b at i)
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
        //backProp requires sigmoid_prime function, cost_prime function, activations and weighted inputs already been set
        void backProp(expected output for a given training input)
        {
            errors at L = hadamard product of cost_prime(expected output for the given training sample, activations at L) with sigmoid_prime of weighted inputs at L 
            nabla_b at L = calculated error of the output layer using cost_prime function
            Looping an index i from L-1 to 1 (moving backward through the remaining layers, save for the zeroth)
                errors at i = ((weights at i + 1 transposed)*(errors at i + 1)) hadamard product with sigmoid_prime at i
                nabla_b at i += errors at i
                nabla_w at i += (errors at i)(activations at i-1 transposed)
        }
        
        void update() {}
        
        //SGD() is a function that loads a test sample, runs forwardPass() and backProp(), and repeats a number of times equal to the
        //mini-batch size. Once this is completed, SGD will call update() to update the weights and biases, reset the nabla_b and
        //nabla_w vectors, and continue until the entirety of the test data from training_data_filename is exhausted.
        //SGD() will also populate a data table of progress (data points included TBD) as the mini-batches are completed
        //SGD() requires backProp(), feedForward(), update() functions.
        void SGD()
        {
            create an array of boolean values equal in size to your training data set, all set to the same value
            Open training_data_filename
            Open expected_output_filename
            
            Looping an index k from 0 to epochs
                Seed the random number generator
                Looping an index j from 0 to [(number of training samples / batch_size) - 1]
                    Looping an index i from 0 to (batch_size - 1)
                        Generate a random number between 0 and size of (training data set - 1)
                        while that element in the array of boolean values is false
                            Generate a new random number
                            Check the value in the boolean array
                        set the random number`th element in the boolean array to false      
                        Load the random number`th training input from training_data_filename and assign its value to activations at 0
                        Load the random number`th expected output from expected_output_filename and assign to a temporary matrix
                        Call forwardPass()
                        Call backProp(temp matrix that holds the expected output for [(batch_size * j) + i]th training input)
                    Call update()
                    Reset nabla_w and nabla_b vector matrices to zero vectors/zero matrices
                //All data should be exhausted by this point
                Set all values in the boolean array to true
            
            Close files
        }
        
        //Instantiates a Network object by file as input to class constructor
        static Network ReadIn()
        {
            Prompt the user to choose between an existing network file or to make a new network
            If the user opts to choose a file
                Read in file name
                Input validation (TBD)
                Extract Network architecture and hyperparameters from file to temporary variables
            Else
                Prompt the user to enter details through the command prompt, storing them in temporary variables
            Call class constructor, passing temporary variables as arguments
        }
};

//Just realized we can make ReadIn() to be a static member function of the class Network. It can be called directly in the main(), 
//before even declaring any object of the class because it is a static function. And, we can read in the required parameters and assign
//the values before calling the constructor. So, the constructor doesn't need any parameter if we make ReadIn() a static member function 
//of the class Network. //@Yon- YESS :D that's why i really wanted to make it a static method. It's a separate entity than any specific 
                        //      instantiation but still retains it's class membership
void main()
{
    Network newNetwork = Network::ReadIn();      //that's how we can call it before declaring the object of the class
}
