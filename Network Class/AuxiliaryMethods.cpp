//PLEASE FOLLOW THIS: Tabs, 4, No wrap

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

const Matrix activationFunction(const Matrix& weighted_inputs)
{
	return sigmoid(weighted_inputs);
}

//must srand(time(NULL)) before running
//Uses polar form of Box-Muller transform of uniformly distributed variable in [0,1] to normally distributed 
//variable in (-inf, inf). May end up simplified to exclude v2, but tbh I don't yet understand the transform.
//Requires #include <math>
const long double distribution(const int numNeuronsIn)
{
	//we end up getting pretty small numbers here, so long doubles are used
	long double v1, v2, w, z1;							//temp variables to hold 
	long double variance = (1 / sqrt(numNeuronsIn));	//standard deviation as per Nielsen's suggestion

	//For some reason, I can't get the assignment to work right unless the operation is split into phases as shown.
	//Should be something like v# = 2 * (std::rand() / RAND_MAX) - 1, but that causes stack overflow...
	do {
		v1 = std::rand();
		v2 = std::rand();
		v1 /= RAND_MAX;
		v2 /= RAND_MAX;
		v1 = 2 * v1 - 1;
		v2 = 2 * v2 - 1;
		w = v1 * v1 + v2 * v2;
	} while (w >= 1);
	w = sqrt((-2 * log(w)) / w);
	z1 = v1 * w;

	return (z1 * variance); //mean 0, standard deviation = variance
}

//Overwrites an input matrix's elements with random values produced by a distribution function
//Loops over individual elements, since I haven't thought of a better way to accomplish this.
//Requires #include <dlib/matrix.h>
void randomizeMatrix(Matrix &inputMatrix)
{
		for (int i = 0; i < inputMatrix.nr(); i++)
			for (int j = 0; j < inputMatrix.nc(); j++)
				inputMatrix(i, j) = distribution(inputMatrix.nc());
}
