#include"Network.h"

void randomizeMatrix(Matrix &input_matrix)
{
	if (input_matrix.nc() == 1)
	{
		for (int i = 0; i < input_matrix.nr(); i++)
			input_matrix(i) = distribution(-1);
	}
	else
	{
		for (int i = 0; i < input_matrix.nr(); i++)
			for (int j = 0; j < input_matrix.nc(); j++)
				input_matrix(i, j) = distribution(input_matrix.nc());
	}
}

/*const Matrix activationFunction(const Matrix& weighted_inputs)
{
	return sigmoid(weighted_inputs);
}

const Matrix activationPrime(const Matrix &input_matrix)
{
	return pointwise_multiply(sigmoid(input_matrix), ones_matrix(input_matrix) - sigmoid(input_matrix));
}*/

const Matrix costPrime(const Matrix &activations_vector, const Matrix &expected_vals_vector)
{
	return (activations_vector - expected_vals_vector);
}

const double distribution(const int num_neurons_in)
{
	long double v1, v2, w, z1;
	long double variance;
	if (num_neurons_in < 0)
		variance = 1;
	else
		variance = (1 / sqrt(num_neurons_in));

	do {
		v1 = std::rand() + 1;
		v2 = std::rand() + 1;
		v1 /= RAND_MAX + 2;
		v2 /= RAND_MAX + 2;
		v1 = 2 * v1 - 1;
		v2 = 2 * v2 - 1;
		w = v1 * v1 + v2 * v2;
	} while (w >= 1);
	w = sqrt((-2 * log(w)) / w);
	z1 = v1 * w;

	return (z1 * variance);
}

