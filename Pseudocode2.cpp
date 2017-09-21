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
            //Generate L, which is the number of layers. This is given by sizeof(layer sizes). 
            //Weights, biases, activations, weighted inputs, nabla_w, nabla_b resized to size L.
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
            nabla_b at L = calculated error of the output layer
            Looping an index i from L-1 to 1 (the remaining layers)
                nabla_b at i = ((weights at i + 1 transposed)*(nabla_b at i + 1)) hadamard product with sigmoid_prime at i
            Loop starts from i = 1 to i = L
                inner loop which will go from 0 to all the nodes in the layer
                  element in nabla_w matrix=(activations of the node from which the weight is coming from the (i-1) th layer)*(nabla_b value for that node)
              //doubtful about this (Please chaeck)
        }
        
        static void ReadIn(istream &in)  //accepts input stream as a parameter
        {
            //accepts the values of all the hyperparameters, an integer array, name of the training datafile and also, name of the 
            //truthdata file
            //assigns the values to the respected parameters in the class
        }
        
        //Stochastic_Gradient_Descent() or SGD() will be the function which will complete the required epochs and update the values of 
        //weights and biases in the network. This will keep the average of nabla_w and nabla_b or in other words, the cost partials.
        //An epoch will be said to complete once the complete dataset goes through the forwardpass() and then, the backprop()
        void Stochastic_Gradient_Descent()
        {
            double Total_nabla_w;  //to store the total of cost partials of weights
            double Total_nabla_b;  //to store the total of cost partials of biases 
            //A loop starts for one epoch and goes till the total number of epochs entered by the user
                //Total_nabla_w and Total_nabla_b are set to 0
                //the mini_batch size from the dataset, as entered by the user, will be chosen
                //Usually, we will need to randomize the data after choosing our batch. But, randomization is not needed now, because  
                //the data is generated randomly.
                
                //An inner loop starts, which will go from the first data in the minibatch to the last dataof that minibatch
                    //will call the fowardpass() and backprop() methods for each training data
                    //updates the Total_nabla_w and Total_nabla_b values
                
                //Update Weights, according to the equation:
                            //w += (eta/n)nabla_w
                //Update biases
                            //b += (eta/n)nabla_b
        }
};


