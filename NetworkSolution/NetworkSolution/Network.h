#ifndef NETWORK_H
#define NETWORK_H

#include <iostream>
#include <fstream>
#include <dlib\matrix.h>
#include <dlib\matrix\matrix_math_functions.h>
#include <string>
#include <vector>
#include <time.h>

using namespace std;
using namespace dlib;

typedef matrix<double> Matrix;
typedef std::vector<Matrix> VMatrix;
typedef std::vector<int> Vector;

class Network
{
public:
	Network();
	Network(const string&, const string&);
	Network(const string&);
	~Network();
	std::vector<double>	train();
	//void				classify();
	void				classify(const string&);

private:
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

	//we do need the string names to be the class data members. Just realized, we need to write the names of the files in the Previous_Network_File.
	string				trainingDataFilename;
	string				expectedValuesFilename;

	ifstream			trainingDataInfile;
	ifstream			expectedValuesInfile;

	//functions that need to be private:
	int					fileSize(istream&);
	bool				writeToFile() const;
	const Matrix		hadamardProduct(const Matrix&, const Matrix&);
	void 				randomizeMatrix(Matrix&);		//parameter with a pointer to the function distribution is not needed
														//because distribution is outside the class and so, it can be called directly
														//without introducing a functor as a parameter
	bool				readInit(const string&);
	void				readInit();
	void				forwardProp(const int, ifstream&);
	bool				backProp(int);
	int					SGD();
	void				updateWeightsAndBiases();
	int					compareOutput(const Matrix&);
	template<class T>
	const matrix<T>		getM(ifstream&, const int);				//A function to return the vector at any position i  
	template <class T>											//in the file, whose ifstream object is passed as the 
	std::vector<T> 		getV(ifstream&, const int);				//parameter
	template <class T>
	std::vector<T>		Strtok(const string&, char[]);
};
#endif