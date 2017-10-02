#ifndef NETWORK_H

#define NETWORK_H

#include <iostream>
#include <fstream>
#include <dlib\matrix.h>
#include <string>
#include <vector>

using namespace std;
using namespace dlib;

typedef matrix<double> Matrix;
typedef std::vector<Matrix> Vector;

class Network
{
	public:
			Network			(istream& = cin);
			Network			(const string&, const string&);
			Network			(const string&);
			~Network		();
		void	train			();
		void	classify		(const string&);
		bool	writeToFile		()	const;
		Matrix&	hadamardProduct		(Matrix M1, Matrix M2);
		void	readInit		(const string&);
		void	readInit		();
		void	forwardProp		(int);
		bool	backProp		(int);
		int	SGD			();
		void	update			();

	private:
		double				learningRate;
		int				batchSize;
		int				epochs;
		int 				numLayers;
		std::vector<int>		layerSizes;
		Vector				weights;
		Vector				biases;
		Vector				activations;
		Vector				weightedInputs;
		Vector				errors;
		Vector				sumNablaB;
		Vector				sumNablaW;
		Vector				expectedValues;
		std::vector<int>		miniBatchIndices;
		string 				trainingDataFilename;
		string 				expectedValuesFilename;
		ifstream			trainingDataInfile;
		ifstream			expectedValuesInfile;
};
#endif