#ifndef NETWORK_H
#define NETWORK_H

#include <iostream>
#include <math.h>
#include <fstream>
#include <dlib\matrix.h>
#include <string>
#include <ctype.h>
#include <time.h>
#include <vector>
#include <dlib\matrix\matrix_math_functions.h>

using namespace std;
using namespace dlib;

typedef matrix<double>		Matrix;
typedef std::vector<Matrix>	VMatrix;
typedef std::vector<int>	Vector;

	//---------Functions outside the class (Auxiliary functions):->
//const Matrix	activationFunction(const Matrix& weighted_inputs);
//const Matrix	activationPrime(const Matrix &input_matrix);
const Matrix	costPrime(const Matrix &activations_vector, const Matrix &expected_vals_vector);
const double	distribution(const int num_neurons_in);
void			randomizeMatrix(Matrix &);

class Network
{
public:

	//---------Constructors and Destructors:->
						Network();
						Network(const string&, const string&);
						Network(const string&);
						~Network();

	//---------Publicly Accessible Methods:->
	std::vector<double>	train();
	void				classify(const string&);
	bool				readInit(const string&);
	void				readInit();

	//---------Setter/Mutator methods (input validation done here as well):->
	void				checkLearningRate(int = 1);		//the highest value for the learning rate is set by default to 1 but can be changed while calling the function
	void				checkEpochs();
	void				checkBatchSize();
	void				checkLayersString(string&);
	void				checkNumLayers();

	//---------Methods to output activation values to a txt file:->
	void				displayActivations(const Matrix& , ostream&  = cout);
	void				displayActivationPrimes(ostream& = cout);
	void				createActivationsFile(const Matrix&);

	//---------Enums:->
	enum Inputs {
		inputLinear,
		inputSigmoid,
		inputComplementaryLog_Log,
		inputBipolarSigmoid,
		inputTanh,
		inputLeCun_stanh,
		inputRectifier,
		inputSmoothRectifier,
		inputLogit,
		inputSoftmax,
		inputRadialGaussian,
		inputMaxout,
		inputLeakyRelu,
		inputCosine,

		numActivations
	};

private:

	//---------Private Data Members:->
	double				learningRate;
	int					batchSize;
	int					epochs;
	int 				numLayers;

	VMatrix				weights;
	VMatrix				sumNablaW;
	VMatrix				biases;
	VMatrix				activations;
	VMatrix				weightedInputs;
	VMatrix				errors;
	VMatrix				sumNablaB;

	Vector				miniBatchIndices;
	Vector				layerSizes;
	VMatrix				activationPrime;
	std::vector<string>	wrongInputs;
	string				trainingDataFilename;
	string				expectedValuesFilename;
	ifstream			trainingDataInfile;
	ifstream			expectedValuesInfile;

	struct 				layerReport { bool isAmbiguous; Matrix cleanOutput; };


	//---------Private Functions:->
	bool				writeToFile() const;
	bool				backProp(int);
	void				forwardProp(int, ifstream &);
	int					SGD();
	layerReport			outputLayerReport();
	void				updateWeightsAndBiases();
	int					filesize(istream&);
	bool				compareOutput(const Matrix&);
	const Matrix		hadamardProduct(const Matrix&, const Matrix&);

	template <class T>
	void				FYShuffle(std::vector<T>& v);

	template<class T>
	const matrix<T>		getM(ifstream&, int);	//A function to return a column matrix at any position i in the given file

	template<class T>
	std::vector<T>		getV(ifstream&, int);

	template <class T>
	std::vector<T>		Strtok(const string&, char[]);

	//---------Activation Methods:->
	 void				linear(int);
	 void				Sigmoid(int);
	 void				logLog(int);
	 void				bipolarSigmoid(int);
	 void				Tanh(int);
	 void				LeCun_stanh(int);
	 void				rectifier(int);
	 void				smoothRectifier(int);
	 void				logit(int);
	 void				softmax(int);
	 void				radialGaussian(int);
	 void				maxout(int);
	 void				leakyRelu(int);
	 void				cosine(int);

	//---------Declarations related to state table:->

		//Required Attributes:->
	matrix<unsigned char>	stateTable;
	std::vector<string>		setActivations;

		//Required Methods:->
	void				activationFuncSwitch(unsigned char, int);
	void				initStateTable();
	void				activationFuncSelect(int);
};

inline const Matrix Network::hadamardProduct(const Matrix &input_matrix_L, const Matrix &input_matrix_R)
{
	return pointwise_multiply(input_matrix_L, input_matrix_R);
}

template <class T>
std::vector<T> Network::Strtok(const string& str, char Separator[])
{
	char * pN;
	std::vector<T> v;
	char *p = new char[str.size() + 1];
	strcpy(p, str.c_str());
	pN = strtok(p, Separator);
	while (pN != NULL)
	{
		v.push_back(atof(pN));
		pN = strtok(NULL, Separator);
	}
	delete[] p;
	return v;
}

template <class T>
void Network::FYShuffle(std::vector<T>& v)
{
	for (int i = v.size() - 1; i > 0; i--)
	{
		int randI = std::rand() % i;
		T temp = v[i];
		v[i] = v[randI];
		v[randI] = temp;
	}
}


template<class T>
const matrix<T> Network::getM(ifstream& fin, int i)
{
	std::vector<T> v2;
	matrix<T> m;
	if (i >= 0)
	{
		fin.seekg(0);
		string str;
		for (int j = 0; j <= i; j++)
			getline(fin, str);
		v2 = Strtok<T>(str, ",");
		m.set_size(v2.size(), 1);
		for (int i = 0; i < v2.size(); i++)
		{
			m(i, 0) = v2[i];
		}
		return m;
	}
	else
	{
		cout << "\n Server error 403: Found Invalid Index" << endl;
		m.set_size(0, 0);
		return m;
	}
}

template<class T>
std::vector<T> Network::getV(ifstream& fin, int i)
{
	std::vector<T> v;
	if (i >= 0)
	{
		fin.seekg(0);
		string str;
		for (int j = 0; j <= i; j++)
			getline(fin, str);
		v = Strtok<T>(str, ",");
		return v;
	}
	else
	{
		cout << "\n Server error 404: Found Invalid Index" << endl;
		v.resize(0);
		return v;
	}
}

#endif