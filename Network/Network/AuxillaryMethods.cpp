#include "Network.h"

///
//Auxillary Methods
///

const Matrix activationFunction(const Matrix &weighted_inputs)
{
	return sigmoid(weighted_inputs);
};

const Matrix activationPrime(const Matrix &input_matrix)
{
	return pointwise_multiply(sigmoid(input_matrix), ones_matrix(input_matrix) - sigmoid(input_matrix));
};

const Matrix costPrime(const Matrix &activations_vector, const Matrix &expected_vals_vector)
{
	return (activations_vector - expected_vals_vector);
};

//must srand(time(NULL)) before running
//Uses polar form of Box-Muller transform
const double distribution(const int num_neurons_in)
{
	long double v1, v2, w, z1;
	long double variance;

	if (num_neurons_in < 0)
		variance = 1;
	else
		variance = (1 / sqrt(num_neurons_in));

	do {
		v1 = 2 * (std::rand() / (double)RAND_MAX) - 1;
		v2 = 2 * (std::rand() / (double)RAND_MAX) - 1;
		w = v1 * v1 + v2 * v2;
	} while (w >= 1);
	w = sqrt((-2 * log(w)) / w);
	z1 = v1 * w;
	return (z1 * variance);
};

//Fisher Yates shuffle
void FYShuffle(Vector &v)
{
	//below is another method using shuffle defined in <algorithm> but it is potentially much slower and not much more random
	//unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	//shuffle(v.begin(), v.end(), default_random_engine(seed));

	for (int i = v.size() - 1; i > 0; i--)
	{
		int randI = std::rand() % i;
		int temp = v[i];
		v[i] = v[randI];
		v[randI] = temp;
	}
};