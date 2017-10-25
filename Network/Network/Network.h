#ifndef NETWORK_H
#define NETWORK_H

#include <iostream>
#include <fstream>
#include <dlib\matrix.h>
#include <dlib\matrix\matrix_math_functions.h>
#include <string>
#include <time.h>
#include <vector>

using namespace std;
using namespace dlib;

typedef matrix<double>		Matrix;
typedef std::vector<Matrix> VMatrix;
typedef std::vector<int>	Vector;

	/**********Prototypes for Auxillary Methods***************/
const Matrix			activationFunction(const Matrix&);
const Matrix			activationPrime(const Matrix&);
const Matrix			costPrime(const Matrix&, const Matrix&);
const double			distribution(const int num_neurons_in);
void					FYShuffle(Vector&);

class Network
{
public:
	/**********Constructors and Destructors*******************/
						Network();
						Network(const string&);
						Network(const string&, const string&);
						~Network();

	/**********Public Methods*********************************/
	//void				classify();
	void				classify(const string&);
	void				readInit();
	bool				readInit(const string&);
	std::vector<double>	train();

private:

	/**********Private Data Members***************************/
	double				learningRate;
	int					batchSize;
	int					epochs;
	int 				numLayers;
	struct layerReport	{ bool isAmbiguous; Matrix cleanOutput; };

	VMatrix				weights;
	VMatrix				sumNablaW;
	VMatrix				biases;
	VMatrix				activations;
	VMatrix				weightedInputs;
	VMatrix				errors;
	VMatrix				sumNablaB;

	Vector				miniBatchIndices;
	Vector				layerSizes;

	string				trainingDataFilename;
	string				expectedValuesFilename;
	ifstream			trainingDataInfile;
	ifstream			expectedValuesInfile;

	/**********Private Methods******************************/
	bool				backProp(int);
	bool				writeToFile() const;
	const Matrix		hadamardProduct(const Matrix&, const Matrix&);
	int					compareOutput(const Matrix&);
	int					fileSize(istream&);
	int					SGD();
	layerReport			outputLayerReport();
	void				forwardProp(ifstream&, const int);
	void 				randomizeMatrix(Matrix&);
	void				updateWeightsAndBiases();
	
	template<class T>
	const matrix<T>		getM(ifstream&, const int);
	
	template <class T>	 
	std::vector<T> 		getV(ifstream&, const int);
	
	template <class T>
	std::vector<T>		Strtok(const string&, char[]);
};
#endif