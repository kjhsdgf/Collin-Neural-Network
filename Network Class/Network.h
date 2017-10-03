#ifndef NETWORK_H

#define NETWORK_H

#include <iostream>
#include <fstream>
#include <dlib\matrix.h>
#include <dlib\matrix_math_functions.h>
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
	
	//we do need the string names to be the class data members. Just realized, we need to write the names of the files in the Previous_Network_File.
		string				trainingDataFilename;
		string				expectedValuesFilename;
	
		ifstream			trainingDataInfile;
		ifstream			expectedValuesInfile;
	
	//functions that need to be private:
		bool		writeToFile		();
		const Matrix	hadamardProduct		(const Matrix& ,const Matrix& );
		void 		randomizeMatrix		(const Matrix&);	//parameter with a pointer to the function distribution is not needed
										//because distribution is outside the class and so, it can be called directly
										//without introducing a functor as a parameter
		void		readInit		(const string&);
		void		readInit		();
		void		forwardProp		(int);
		bool		backProp		(int);
		int		SGD			();
		void		update			();
		template<class T>
		void 		shuffleDataIndices	(std::vector<T>& v);
		template<class T>
		std::vector<T>& getAt			(ifstream& inFile, const int i); //A function to return the vector at any position i  
											 //in the file, whose ifstream object is passed as the 
											 //parameter
};
#endif
