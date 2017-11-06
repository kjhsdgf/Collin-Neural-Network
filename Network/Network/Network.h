#ifndef NETWORK_H

#define NETWORK_H

#include <iostream>
#include <fstream>
#include <dlib\matrix.h>
#include <string>
#include <ctype.h>
#include <time.h>
#include <vector>
#include <dlib\matrix\matrix_math_functions.h>


using namespace std;
using namespace dlib;

typedef matrix<double> Matrix;
typedef std::vector<Matrix> VMatrix;
typedef std::vector<int> Vector;

class Network
{
public:

	//**********Constructors and Destructors*******************
						Network();
						Network(const string&, const string&);
						Network(const string&);
						~Network();

	//**********Public Accessible Methods**********************
	std::vector<double>	train();
	void				classify(const string&);

	//***********Methods that read size of each layer, number of epochs, learning rate and the batch size for the network*********
	//---------------------------Also, the number of layers is calculated in these methods----------------------------------------
	bool				readInit(const string&);
	void				readInit();

	//---------------------Methods to check the typo errors by the users--------------------
	void				checkLearningRate(int = 1);		//the highest value for the learning rate is set by default to 1 but can be changed while calling the function
	void				checkEpochs();
	void				checkBatchSize();
	void				checkLayersString(string&);
	void				checkNumLayers();

	//---------------------Methods to output the patterns generated--------------------------
	void				displayActivations(const Matrix& , ostream&  = cout);
	void				displayActivationPrimes(ostream& = cout);
	void				createActivationsFile(const Matrix&);

	//----Enums
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

	//Private Data Members:->
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
	std::vector<std::vector<double>>	trainingData;
	std::vector<std::vector<double>>	expectedValues;

	struct 				layerReport { bool isAmbiguous; Matrix cleanOutput; };


	//Private Functions:->
	bool				writeToFile() const;
	bool				backProp(int);
	void				forwardProp(int, ifstream &);
	int					SGD();
	layerReport			outputLayerReport();
	void				updateWeightsAndBiases();
	int					filesize(istream&);
	bool				compareOutput(const Matrix&);
	const Matrix		hadamardProduct(const Matrix&, const Matrix&);
	void				initTrainingData();
	
	template <class T>
	void				FYShuffle(std::vector<T>& v);

	template<class T>
	const matrix<T>		getM(bool, int);	//A function to return a column matrix at any position i in the given file

	template<class T>
	std::vector<T>		getV(bool, int);

	bool				getNext(std::vector<double>&, std::vector<double>&);

	template <class T>
	std::vector<T>		Strtok(const string&, char[]);
	std::vector<double>	tokenize(const string&, char[] = ", ");


	//Activation Methods:
	 void		linear(int);
	 void		Sigmoid(int);
	 void		logLog(int);
	 void		bipolarSigmoid(int);
	 void		Tanh(int);
	 void		LeCun_stanh(int);
	 void		rectifier(int);
	 void		smoothRectifier(int);
	 void		logit(int);
	 void		softmax(int);
	 void		radialGaussian(int);
	 void		maxout(int);
	 void		leakyRelu(int);
	 void		cosine(int);

	//--------------------Declarations related to state table------------------------------------

	//Required Attritubes ->
	matrix<unsigned char>	stateTable;
	std::vector<string>		setActivations;

	//Required Methods ->
	void				Switch(unsigned char, int);
	void				initStateTable();
	void				takeInput(int);
};

//---------Functions outside the class (Auxiliary functions)----------------------------------------
const Matrix	activationFunction(const Matrix& weighted_inputs);
const Matrix	activationPrime(const Matrix &input_matrix);
const Matrix	costPrime(const Matrix &activations_vector, const Matrix &expected_vals_vector);
const double	distribution(const int num_neurons_in);
void			randomizeMatrix(Matrix &);

//---------------------------------------------------------------------------------------------------


inline const Matrix Network::hadamardProduct(const Matrix &input_matrix_L, const Matrix &input_matrix_R)
{
	return pointwise_multiply(input_matrix_L, input_matrix_R);
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



template<class T>
const matrix<T> Network::getM(bool exp, int i)
{
	std::vector<T> v = getV<T>(exp, i);
	matrix<T> m; m.set_size(v.size(), 1);
	for (int i = 0; i < v.size(); i++)
		m(i, 0) = v[i];
	return m;
}

template<class T>
std::vector<T> Network::getV(bool exp, int i)
{
	if (exp)
		return expectedValues[i];
	else
		return trainingData[i];
}



#endif


