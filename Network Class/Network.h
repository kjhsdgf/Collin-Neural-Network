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
typedef std::vector<Matrix> VMatrix;
typedef std::vector<dlib::vector<double>> VVector;
typedef std::vector<int> Vector;

class Network
{
	public:
			Network			(istream& = cin);
			Network			(const string&, const string&);
			Network			(const string&);
			~Network		();
		void	train			();
		void	classify		(const string&);

	private:
		double				learningRate;
		int				batchSize;
		int				epochs;
		int 				numLayers;
		
		VMatrix				weights;
		VMatrix				sumNablaW;
	
	//As we decided, the n by 1 matrics are now vectors of dlib::vector
		VVector				biases;
		VVector				activations;
		VVector				weightedInputs;
		VVector				errors;
		VVector				sumNablaB;
		
		Vector				miniBatchIndices;
		Vector				layerSizes;
	
	//we don't need the string names to be the class data members. We are defining them inside the readInit()
	//and opening the required files in readInit() using the ifstream objects
	
		ifstream			trainingDataInfile;
		ifstream			expectedValuesInfile;
	
	//functions that need to be private:
		bool		writeToFile		();
		Matrix&		hadamardProduct		(const Matrix& ,const Matrix& );
		void		readInit		(const string&);
		void		readInit		();
		void		forwardProp		(int);
		bool		backProp		(int);
		int		SGD			();
		void		update			();
		template<class T>
		std::vector<T>& operator[]		(ifstream& inFile, const int i); //A function to return the vector at any position i  
											 //in the file, whose ifstream object is passed as the 
											 //parameter
};
#endif
