#ifndef NETWORK_H

#define NETWORK_H

#include <iostream>
#include <fstream>
#include <dlib\matrix.h>
#include <string>
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
	void		train();
	void				classify(const string&);

	//***********Methods that read size of each layer, number of epochs, learning rate and the batch size for the network*********
	//---------------------------Also, the number of layers is calculated in these methods----------------------------------------
	bool				readInit(const string&);
	void				readInit();
	
	//---------------------Methods to check the typo errors by the users--------------------
	void				checkLearningRate(int = 1);		//the highest value for the learning rate is set to 1 but can be changed while calling the function
	void				checkEpochs();
	void				checkBatchSize();
	void				checkLayersString(string&);
	void				checkNumLayers();

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
	std::vector<string>	wrongInputs;				//contains all the input errors
	string				trainingDataFilename;
	string				expectedValuesFilename;
	ifstream			trainingDataInfile;
	ifstream			expectedValuesInfile;
	struct layerReport	{ bool isAmbiguous; Matrix cleanOutput; };

	//Private Functions:->
	bool				writeToFile() const;
	bool				saveNetwork() const;

	bool				backProp(int);
	void				forwardProp(ifstream &, const int);
	int					SGD();
	void				updateWeightsAndBiases();
	int					fileSize(istream&);
	int					compareOutput(const Matrix&);
	const Matrix		hadamardProduct(const Matrix&, const Matrix&);
	layerReport			outputLayerReport();
	
	union double_longlong
	{
		double d;
		long long l;
	};

	union float_long
	{
		float f;
		long l;
	};

	template <class T>
	void				FYShuffle(std::vector<T>& v);

	template <class T>
	std::vector<T>		Strtok(const string& str, char Separator[]);

	template <class T>
	void				write(ofstream& out, T val) const;
	template <class T>
	void				writeHelper(ofstream& out, T val) const;

	template <class T>
	T					read(ifstream& in);
	template <class T>
	T					readHelper(ifstream& in);



	template<class T>
	const matrix<T>		getMBin(ifstream&, int);	//A function to return a column matrix at any position i in the given file

	template<class T>
	std::vector<T>		getVBin(ifstream&, int);

	template<class T>
	const matrix<T>		getM(ifstream&, int);	//A function to return a column matrix at any position i in the given binary file

	template<class T>
	std::vector<T>		getV(ifstream&, int);

	
};

//---------Functions outside the class (Auxiliary functions)----------------------------------------
const Matrix 	activationFunction(const Matrix& weighted_inputs);
const Matrix 	activationPrime(const Matrix &input_matrix);
const Matrix 	costPrime(const Matrix &activations_vector, const Matrix &expected_vals_vector);
const double 	distribution(const int num_neurons_in);
      void   	randomizeMatrix(Matrix &);

//---------------------------------------------------------------------------------------------------

template <class T>
void Network:: FYShuffle(std::vector<T>& v)
{
	for (int i = v.size() - 1; i > 0; i--)
	{
		int randI = std::rand() % i;
		T temp = v[i];
		v[i] = v[randI];
		v[randI] = temp;
	}
}

//A Strtok(), which can help us assigning any vector later on while reading from a file
//can be used in readInit() too
//Considering the fact that we don't want any user or programmer to use it, Strtok<T> can be a private member of the class.
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
	std::vector<T> v;
	std::vector<T> v2;
	matrix<T> m;
	if (i >= 0)
	{
		fin.seekg(0);
		string str;
		for (int j = 0; j < i; j++)
			getline(fin, str);
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
		for (int j = 0; j < i; j++)
			getline(fin, str);
		getline(fin, str);
		v = Strtok<T>(str, ",");
		return v;
	}
	else
	{
		cout << "\n Server error 403: Found Invalid Index" << endl;
		v.resize(0);
		return v;
	}
}

template<class T>
const matrix<T> Network::getMBin(ifstream& in, int i)
{
	std::vector<T> v = getVBin<T>(in, i);
	matrix<T> m(v.size(), 1);
	if (v.size() == 0)
		return m;

	for (int j = 0; j < v.size(); j++)
		m(j, 1) = v[j];
	return m;
}

template <class T>
std::vector<T> Network::getVBin(ifstream& in, int i)
{
	std::vector<T> v;
	int inputNodes = 4; //'4' is just for test. change to 'layerSizes[0]'
	int typeSize = sizeof(T);
	int lineSize = inputNodes * typeSize;
	in.seekg(0, in.end);
	int fileSize = in.tellg() / (lineSize);
	if (i < 0 || i >= fileSize)
	{
		cout << "\n Server error 403: Found Invalid Index" << endl;
		return v;
	}

	in.seekg(i * lineSize);
	for (int j = 0; j < inputNodes; j++)
	{
		v.push_back(read<T>(in));
	}
	return v;
}


// writes a single piece of data of type T to ostream out
// calls writeHelper to do actual writing; this method ensures the helper isn't called with erroneous data types
template <class T>
void Network::write(ofstream& out, T val) const
{
	// expand the if else chain to include more types or
	// assert that only these four are taken
	if (typeid(T) == typeid(double))
	{
		double_longlong tmp; tmp.d = val;
		writeHelper(out, tmp.l);
	}
	else if (typeid(T) == typeid(float))
	{
		float_long tmp; tmp.f = val;
		writeHelper(out, tmp.l);
	}
	else if (typeid(T) == typeid(int))
		writeHelper(out, (int)val);
	else
		writeHelper(out, (char)val);
}

template <class T>
void Network::writeHelper(ofstream& out, T val) const
{
	for (int i = 0; i < sizeof(T); i++)
	{
		out.put((char)(val & 255));
		val = val >> 8;
	}
}

// reads a single piece of data of type T from istream in
// calls readHelper to do actual reading; this method ensures the helper isn't called with erroneous data types
template <class T>
T Network::read(ifstream& in)
{
	// expand the if else chain to include more types or
	// assert that only these four are taken
	if (typeid(T) == typeid(double))
	{
		double_longlong tmp;
		tmp.l = readHelper<long long>(in);
		return tmp.d;
	}
	else if (typeid(T) == typeid(float))
	{
		float_long tmp;
		tmp.l = readHelper<long long>(in);
		return tmp.f;
	}
	else if (typeid(T) == typeid(int))
		return readHelper<int>(in);
	else
		return readHelper<char>(in);
}

template <class T>
T Network::readHelper(ifstream& in)
{
	T val = 0;
	for (int i = 0; i < sizeof(T); i++)
	{
		T tmp = in.get();
		val |= tmp << (8 * i);
	}
	return val;
}


#endif

