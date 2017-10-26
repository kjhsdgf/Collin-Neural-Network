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

typedef matrix<double>		Matrix;
typedef std::vector<Matrix> VMatrix;
typedef std::vector<int>	Vector;
typedef Matrix(*activationsType) (int);

	/**********Prototypes for Auxillary Methods***************/
const Matrix						activationFunction(const Matrix&);
const Matrix						activationPrime(const Matrix&);
const Matrix						costPrime(const Matrix&, const Matrix&);
const double						distribution(const int num_neurons_in);
void								FYShuffle(Vector&);
void								randomizeMatrix(Matrix &);

class Network
{
public:
	/**********Constructors and Destructors*******************/
									Network();
									Network(const string&);
									Network(const string&, const string&);
									~Network();

	/**********Public Methods*********************************/
	void							checkBatchSize();
	void							checkEpochs();
	void							checkLayersString(string&);
	void							checkLearningRate(int = 1);
	void							checkNumLayers();
	//void							classify();
	void							classify(const string&);
	void							readInit();
	bool							readInit(const string&);
	std::vector<double>				train();

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
	double							learningRate;
	int								batchSize;
	int								epochs;
	int 							numLayers;
	struct layerReport				{ bool isAmbiguous; Matrix cleanOutput; };
	std::vector<string>				wrongInputs;

	VMatrix							weights;
	VMatrix							sumNablaW;
	VMatrix							biases;
	VMatrix							activations;
	VMatrix							weightedInputs;
	VMatrix							errors;
	VMatrix							sumNablaB;

	Vector							miniBatchIndices;
	Vector							layerSizes;

	string							trainingDataFilename;
	string							expectedValuesFilename;
	ifstream						trainingDataInfile;
	ifstream						expectedValuesInfile;

	/**********Private Methods******************************/
	bool							backProp(int);
	bool							writeToFile() const;
	const Matrix					hadamardProduct(const Matrix&, const Matrix&);
	int								compareOutput(const Matrix&);
	int								fileSize(istream&);
	int								SGD();
	layerReport						outputLayerReport();
	void							forwardProp(ifstream&, const int);
	void							updateWeightsAndBiases();
	
	template<class T>
	const matrix<T>					getM(ifstream&, const int);
	
	template <class T>	 
	std::vector<T> 					getV(ifstream&, const int);
	
	template <class T>
	std::vector<T>					Strtok(const string&, char[]);

	//Activation Functions:
	static Matrix					bipolarSigmoid(int);
	static Matrix					cosine(int);
	static Matrix					leakyRelu(int);
	static Matrix					LeCun_stanh(int);
	static Matrix					linear(int);
	static Matrix					log_Log(int);
	static Matrix					logit(int);
	static Matrix					maxout(int);
	static Matrix					rectifier(int);
	static Matrix					radialGaussian(int);
	static Matrix					sigmoid(int);
	static Matrix					smoothRectifier(int);
	static Matrix					softmax(int);
	static Matrix					tanh(int);

	//Declarations related to state table:
		//Required Attributes:
	static activationsType			activationFuncs[numActivations];	//array of functions with return type matrix and parameter of int
		   matrix<unsigned char>	StateTable;

		//Required Methods:
	void							initStateTable();
	Matrix							takeInput(int);
};
#endif
