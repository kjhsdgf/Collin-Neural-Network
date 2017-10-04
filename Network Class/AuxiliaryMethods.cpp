//PLEASE FOLLOW THIS: Tabs, 4, No wrap

#include <dlib\matrix\matrix_math_functions_abstract.h>	//used for sigmoid() in activationFunction()
#include <dlib\matrix.h>								//used for overload of - for matrices in costPrime()
#include <time.h>										//used to seed srand() with time(NULL)
#include <math.h>										//used for pow(), sqrt(), pi in distribution()

//Fisher Yates shuffle
template <class T>
void FYShuffle(vector<T>& v)
{
	//below is another method using shuffle defined in <algorithm> but it is potentially much slower and not much more random
	//unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	//shuffle(v.begin(), v.end(), default_random_engine(seed));

	for (int i = v.size()-1; i > 0; i--)
	{
		int randI = rand() % i;
		T temp = v[i];
		v[i] = v[randI];
		v[randI] = temp;
	}
}

//Activation function: can be changed later by the programmer, however as we discussed reading a function from the file can be a little risky
//So, I need to think about it before implementing it
const Matrix& activationFunction(const Matrix& weighted_inputs)
{
	return sigmoid(weighted_inputs);
}

//Cost derivative with respect to activations (for the quadratic cost function, in this case).
//Removed & (reference operator) from return time, as it was giving me compiler errors
const Matrix costPrime(const Matrix &activations_vector, const Matrix &expected_vals_vector)
{
	return (activations_vector - expected_vals_vector);
}

//Sample function for a random distribution that takes information about the network to generate intial values
//for weights and biases within the network. Seeing as the user may write any manner of function, with who knows
//what parameters, creating class members that can handle a wide variety of input may prove to be a challenge.
//Moreover, the user may desire access to information about the network in order to produce initial values, namely
//the layer sizes (seeing that Nielsen uses it, providing accessors for this information doesn't seem too far-fetched).
//must srand(time(NULL)) before running (calling srand() from within the function will produce the same number)
const long double distribution(int *layer_sizes, int current_layer)
{
	long double variance = (1 / sqrt(layer_sizes[current_layer - 1]));
	return pow(variance * sqrt(2 * pi), -1) * exp(-(std::rand() % 51) / (2 * pow(variance, 2)));
}
