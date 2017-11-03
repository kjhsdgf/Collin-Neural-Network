#ifndef NETWORK_H
#define NETWORK_H

#include <iostream>
#include <fstream>
#include <dlib\matrix.h>
#include <dlib\matrix\matrix_math_functions.h>
#include <string>
#include <time.h>
#include <vector>
#include <math.h>

using namespace std;
using namespace dlib;

typedef matrix<double>			Matrix;
typedef std::vector<Matrix> 	VMatrix;
typedef std::vector<int>		Vector;

	/**********Prototypes for Auxillary Methods***************/
const Matrix		activationFunction(const Matrix&);
const Matrix		activationPrime(const Matrix&);
const Matrix		costPrime(const Matrix&, const Matrix&);
const double		distribution(const int num_neurons_in);
void				FYShuffle(Vector&);
void				randomizeMatrix(Matrix &);


class Network
{
public:
	/**********Constructors and Destructors*******************/
											Network();
											Network(const string&);
											Network(const string&, const string&);
											~Network();

	/**********Public Methods*********************************/
void										checkBatchSize();
void										checkEpochs();
void										checkLayersString(string&);
void										checkLearningRate(int = 1);
void										checkNumLayers();
void										classify(const string&);
void										readInit();
bool										readInit(const string&);
std::vector<double>							train();

	//----Enums
	enum Inputs {
											inputBipolarSigmoid,
											inputComplementaryLog_Log,
											inputCosine,
											inputLeakyRelu,
											inputLeCun_stanh,
											inputLinear,
											inputLogit,
											inputMaxout,
											inputRadialGaussian,
											inputRectifier,
											inputSigmoid,
											inputSmoothRectifier,
											inputSoftmax,
											inputTanh,

											numActivations
	};

private:
	/**********Private Data Members***************************/
	double								learningRate;
	int									batchSize;
	int									epochs;
	int 								numLayers;
	struct layerReport				 	{ bool isAmbiguous; Matrix cleanOutput; };
	std::vector<string>					wrongInputs;
	std::vector<double>					trainingData;
	std::vector<double>					truthValues;

	VMatrix								weights;
	VMatrix								sumNablaW;
	VMatrix								biases;
	VMatrix								activations;
	//VMatrix							activationPrime;
	VMatrix								weightedInputs;
	VMatrix								errors;
	VMatrix								sumNablaB;

	Vector								miniBatchIndices;
	Vector								layerSizes;

	string								trainingDataFilename;
	string								expectedValuesFilename;
	ifstream							trainingDataInfile;
	ifstream							expectedValuesInfile;

	/**********Private Methods******************************/
	bool								backProp(int);
	bool								writeToFile() const;
	const Matrix						hadamardProduct(const Matrix&, const Matrix&);
	int									compareOutput(const Matrix&);
	int									fileSize(istream&);
	int									SGD();
	layerReport							outputLayerReport();
	void								forwardProp(ifstream&, const int);
	void								updateWeightsAndBiases();
	
	template<class T>
	const matrix<T>						getM(ifstream&, const int);
	
	template <class T>	 
	std::vector<T> 						getV(ifstream&, const int);
	
	template <class T>
	std::vector<T>						Strtok(const string&, char[]);

	//Activation Functions:
	/*void								linear(int);
	void								Sigmoid(int);
	void								logLog(int);
	void								bipolarSigmoid(int);
	void								Tanh(int);
	void								LeCun_stanh(int);
	void								rectifier(int);
	void								smoothRectifier(int);
	void								logit(int);
	void								softmax(int);
	void								radialGaussian(int);
	void								maxout(int);
	void								leakyRelu(int);
	void								cosine(int);*/

	/*//--------------------Declarations related to state table------------------------------------

	//Required Attritubes ->
	matrix<unsigned char>				stateTable;
	std::vector<string>					setActivations;

	//Required Methods ->
	void								Switch(unsigned char, int);
	void								initStateTable();
	Matrix								takeInput(int);*/

};

//--------------------------------------------------------------------------------------------------------------------
//					Definitions of some inline and template functions related to the class
//--------------------------------------------------------------------------------------------------------------------

//hadamardProduct utilizes dlib's pointwise_multiply() to compute the element-by-element product.
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

//******************************
//
//MY CHANGES - YONATAN!!!!
//
//******************************
#endif
