#ifndef NETWORK_H
#define NETWORK_H

#include <vector>
#include <dlib/matrixabstract.h>
#include <string>
using namespace std;
using namespace dlib;

class Network
{
	public:
		Network		(istream& = cin);
		Network		(const string&, const string&) //might not be necessary
		Network		(const string&)
		~Network	();
		
		void	readInit	(const string&);
		void	readInit	();
		
		void	forwardProp	(int);
		bool	backProp	(int);
		void	SGD			();
		void	update		();
		void	train		();
		
		void	classify	(const string&) // or maybe (ifstream&);
		bool	writeToFile	();
		matrix&	hadamardProduct (matrix 
	private:
		double			learningRate;
		int				batchSize;
		int				epochs;
		vector<int>		layerSizes;
		
		vector<matrix>	weights;
		vector<matrix>	biases;
		vector<matrix>	activations;
		
		vector<matrix>	errors;
		vector<matrix>	sumNablaB;
		vector<matrix>	sumNablaW;
		
		vector<int>		miniBatchIndices;
		
		ifstream		trainingDataInfile;
		ifstream		expectedValuesInfile;
};

#endif
