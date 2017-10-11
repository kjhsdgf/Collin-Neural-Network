#include "Network.h"
///
//Auxillary Methods
///
const Matrix activationFunction(const Matrix &weighted_inputs)
{
	return sigmoid(weighted_inputs);
};

const Matrix activationPrime(const Matrix &input_matrix)
{
	return pointwise_multiply(sigmoid(input_matrix), ones_matrix(input_matrix) - sigmoid(input_matrix));
};

const Matrix costPrime(const Matrix &activations_vector, const Matrix &expected_vals_vector)
{
	return (activations_vector - expected_vals_vector);
};

//must srand(time(NULL)) before running
//Uses polar form of Box-Muller transform
const long double distribution(const int num_neurons_in)
{
	long double v1, v2, w, z1;
	long double variance;

	if (num_neurons_in < 0)
		variance = 1;
	else
		variance = (1 / sqrt(num_neurons_in));

	do {
		v1 = 2 * (std::rand() / (double)RAND_MAX) - 1;
		v2 = 2 * (std::rand() / (double)RAND_MAX) - 1;
		w = v1 * v1 + v2 * v2;
	} while (w >= 1);
	w = sqrt((-2 * log(w)) / w);
	z1 = v1 * w;
	return (z1 * variance);
};

//Fisher Yates shuffle
template <class T>
void FYShuffle(std::vector<T>& v)
{
	//below is another method using shuffle defined in <algorithm> but it is potentially much slower and not much more random
	//unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	//shuffle(v.begin(), v.end(), default_random_engine(seed));

	for (int i = v.size() - 1; i > 0; i--)
	{
		int randI = std::rand() % i;
		T temp = v[i];
		v[i] = v[randI];
		v[randI] = temp;
	}
};

///
//Network Class Functions
///
void Network::randomizeMatrix(Matrix &input_matrix)
{
	if (input_matrix.nc() == 1)
	{
		for (int i = 0; i < input_matrix.nr(); i++)
			input_matrix(i) = distribution(-1);
	}
	else
	{
		for (int i = 0; i < input_matrix.nr(); i++)
			for (int j = 0; j < input_matrix.nc(); j++)
				input_matrix(i, j) = distribution(input_matrix.nc());
	}
}

bool Network::writeToFile() const
{
	/*This method creates a file named "Previous_Network_[Day][Time(hhmin)].txt" and
	writes all  the required parameters of the class network in the file.
	This file can be used later to train the existing network or to classify the file
	provided by the user, using the values from any previous network.

	This method writes to the file in the format as follows:

	trainingDataFilename
	expectedValuesFilename
	learningRate
	batchSize
	epochs
	numLayers
	vector of layerSizes
	(for all layers ->)
	w [index]
	weights matrix at that index
	b [index]
	biases matrix at that index  */

	ofstream outfile;
	std::vector<int>::const_iterator i1;
	int i;
	int j(0);
	string a("previousnetwork");
	time_t _tm = time(NULL);
	struct tm * curtime = localtime(&_tm);
	a += asctime(curtime);
	string fileName;

	fileName.resize(24);
	for (i = 0; i < 20; i++)			//gets the name "Previous_Network_[Day]"
		fileName[j++] = a[i];

	for (i = 28; i < 35; i++)			//appends the [time (hhmin)] to name of the file
		if (a[i] != ':')
			fileName[j++] = a[i];

	fileName += ".txt";				//appends ".txt" to the name of the file
	outfile.open(fileName, ios_base::out); 		//creates the file with name "Previous_Network_[Day][Time(hhmin)].txt"
	if (outfile.is_open())
	{
		cout << "\nCreating file " << fileName << '\n'; //because endl calls flush(), which forces the buffer to write to the file without being filled
		outfile << trainingDataFilename << '\n';
		outfile << expectedValuesFilename << '\n';
		outfile << learningRate << '\n';
		outfile << batchSize << '\n';
		outfile << epochs << '\n';
		outfile << numLayers << '\n';
		i1 = layerSizes.begin();
		for (; i1 != layerSizes.end(); i1++)
			outfile << (*i1) << " ";
		outfile << '\n';
		j = 1;
		while ((j) < numLayers)
		{
			outfile << "w " << j << '\n';
			outfile << weights[j];
			outfile << "b " << j << '\n';
			outfile << biases[j];
			j++;
		}
		outfile.close();
		return true;
	}
	else
	{
		cout << "Server error 401: Could not open the file requested! Try again later..";
		return false;
	}
}

void Network::readInit() // reading from console
{
	cout << "Welcome! Please follow the prompts to initialize and begin training your network." << endl;
	cout << "Enter a string of integers that correspond to the layers and desired nodes in each layer of your network:" << endl;
	string layers;  getline(cin, layers);

	char* cStrLayers = new char[layers.size() + 1];
	strcpy(cStrLayers, layers.c_str());

	for (char *p = strtok(cStrLayers, " ,"); p != NULL; p = strtok(NULL, " ,"))
	{
		layerSizes.push_back(atoi(p));
	}
	numLayers = layerSizes.size();

	cout << "\nPlease enter a double for the learning rate (usually in the range [x-y]):" << endl;
	cin >> learningRate;

	cout << "\nPlease enter an integer for the number of epochs (number of times to parse through test data):" << endl;
	cin >> epochs;

	cout << "\nPlease enter an integer for the mini batch size:" << endl;
	cin >> batchSize;

	cout << "\nThank you! You have created a network with these values:" << endl;

	cout << "Input layer with " << layerSizes[0] << " nodes." << endl <<
		"Output layer with " << layerSizes[numLayers - 1] << " nodes." << endl <<
		(numLayers - 2) << " hidden layers with ";

	for (int i = 1; i < numLayers - 1; i++)
		cout << layerSizes[i] << " ";

	cout << "nodes respectively." << endl <<
		"Learning rate := " << learningRate << endl <<
		"Epochs := " << epochs << endl <<
		"Mini batch size := " << batchSize << endl;
	delete[] cStrLayers;
}

//hadamardProduct utilizes dlib's pointwise_multiply() to compute the element-by-element product.
//pointwise_multiply() will assert that the input matrices must be of equal size, so the program will stop
//when that occurs. 
//Removed & (address operator) from the return type, as it was giving out compiler errors.
const Matrix Network::hadamardProduct(const Matrix &input_matrix_L, const Matrix &input_matrix_R)
{
	return pointwise_multiply(input_matrix_L, input_matrix_R);
}

//Updates weights and biases for the network by overwriting weights and biases.
//helps if sumNablaW and sumNablaB VMatrix's have been filled.
void Network::updateWeightsAndBiases()
{
	double k = (learningRate / batchSize); //saves (numLayers - 1) divisions
	for (int i = 1; i < numLayers; i++)
	{
		weights[i] -= k * sumNablaW[i];
		biases[i] -= k * sumNablaB[i];
	}

}

//moves backwards through Network calculating error at each level and using that to increment sumNablaB and sumNablaW
//requires the assumption that expected values has an int
bool Network::backProp(int index)
{
	int lastInd = numLayers - 1;
	Matrix expectedValues = getM<double>(expectedValuesInfile, index);

	bool correct = compareOutput(expectedValues) > 0;

	errors[lastInd] = hadamardProduct(costPrime(activations[lastInd], expectedValues), activationPrime(weightedInputs[lastInd]));
	sumNablaB[lastInd] += errors[lastInd];
	sumNablaW[lastInd] += (errors[lastInd]) * trans(activations[lastInd - 1]);

	for (int i = lastInd - 1; i > 0; i--)
	{
		errors[i] = hadamardProduct(trans(weights[i + 1]) * errors[i + 1],
			activationPrime(weightedInputs[i]));
		sumNablaB[i] += errors[i];
		sumNablaW[i] += errors[i] * trans(activations[i - 1]);
	}

	return true; //correct;
}

// checks if Network output matches expected
// @param - takes a matrix of expected vals (could be changed to vector though or process both if need be)
// returns <0 if correct, >0 if incorrect and a 0 if ambiguous
int Network::compareOutput(const Matrix& expectedValues)
{
	int lastInd = numLayers - 1;
	int lastSize = layerSizes[lastInd];

	// indices at which there is the biggest activation or a 1 for the network or expected vals respectively
	int biggestI = -1; int expectedI = -1;

	// biggest starts at 0 (not MIN) because any number < 0 might as well be 0 for these purposes
	// isAmbiguous is whether or not we encounter a biggest element more than once in output layer
	double biggest = 0;	bool isAmbiguous = false;

	// out of place random check to make sure it's being fed good data
	if (expectedValues.size() != lastSize)
	{
		cout << "Error in compareOutput: the number of neurons in the output layer != the number of elements in given validation datum\n";
		return 0;
	}

	for (int i = 0; i < lastSize; i++)
	{
		double output = activations[lastInd](i, 0);
		if (output > biggest)
		{
			biggestI = i;
			biggest = output;
			isAmbiguous = false; // we found a new biggest so the output is not ambiguous
		}
		else if (output == biggest)
			isAmbiguous = true;

		if (expectedValues(i, 0) == 1 && expectedI == -1)
		{
			if (expectedI == -1) // if it's not been set before..
				expectedI = i;
			else				 // in this case there's bad data
				expectedI = -2;
		}
	}

	// the return statements here can and should change depending on what we agree is good, bad, or ambiguous data
	if (biggestI == -1 || expectedI <= -1) // everything was smaller than 0 or expected is bad data (probably should throw excpetion instead actually)
		return 0;

	// now we can be sure biggestI, expectedI >=0

	if (isAmbiguous) // if there's more than 1 biggest, ambiguous data. don't even care if the indices match
		return 0;

	if (biggestI == expectedI)
		return 1;
	else
		return -1;
}


//Extracts the training data sample at batchIndex and runs forward propagation as commonly(?) defined for feedforward
//neural networks. Requires that a training data file be in place, and an infile object is instantiated for it.
//Added another parameter bc it made my life easier while writing classify()
void Network::forwardProp(const int batchIndex, ifstream &infile)
{
	//extract data point from training data file at input indice into first layer of activations
	activations[0] = getM<double>(infile, batchIndex); //<--untested!
													   //run forward propagation
	for (int i = 1; i < numLayers; i++)
	{
		weightedInputs[i] = ((weights[i] * activations[i - 1]) + biases[i]);
		activations[i] = activationFunction(weightedInputs[i]);
	}
}

// SGD performs a single step of stochastic gradient descent on a mini batch size
// It propagates forward to compute an output then backwards to compute the errors in the newtwork for each input in the mini batch size
// It then computes the average error for the mini batch size and updates the weights and biases accordingly
// Returns an int representing how many times the network produced expected output for a given mini batch size
int Network::SGD()
{
	int numCorrect = 0;
	for (int i = 0; i < miniBatchIndices.size(); i++)
	{
		forwardProp(miniBatchIndices[i], trainingDataInfile);
		if (backProp(miniBatchIndices[i]))
			numCorrect++;
	}

	updateWeightsAndBiases();

	for (int i = 1; i < numLayers; i++)
	{
		sumNablaB[i] = zeros_matrix(sumNablaB[i]);
		sumNablaW[i] = zeros_matrix(sumNablaW[i]);
	}
	return numCorrect;
}



//when passed a text file, will classify data therein and output to console as well as a file
void Network::classify(const string &validation_data_filename)
{
	//SETUP
	//create an infile object for validation data, and open it
	ifstream validationDataInfile(validation_data_filename, ios::ate);	//opened at end for calc of numClassifications
	if (validationDataInfile.fail())
	{
		cout << "File \'" << validation_data_filename << "\' not found!\n";
		cout << "Exiting classify()...\n";
		return;
	}

	//create outfile for output and open it
	string outputFilename = "classification";							//what filename starts with
	const int MAX_SIZE(80);												//buffer (maximum) size for the intermediary cstring
	time_t currentTime = time(NULL);									//returns current time
	struct tm * currentTimeInfo = localtime(&currentTime);				//stores current time into a struct
	char outputFileTime[MAX_SIZE];										//creates cstring intermediary
	strftime(outputFileTime, MAX_SIZE, "%a%H%m", currentTimeInfo);		//converts cstring intermediary into a string containing the current time
	outputFilename += outputFileTime;									//appends Day(of week)HourMinute
	ofstream classificationOutput(outputFilename, ios::trunc);			//creates and opens output file

																		//create an integer for biggest element indice
	int biggestElement(0); ///assumed to be the 0th element, initially
						   //create a counter for number of elements equal to the biggest
	int numBiggest(0);
	//create a counter for ambiguous data
	int ambiguousData(0);
	//make an integer for the total number of samples in the validation data file
	const int MAGIC_NUMBER_1(2);	///these are just what worked with the given data files!
	const int MAGIC_NUMBER_2(10);	///may need to change them later... no idea what they represent yet.
	int numClassifications = (static_cast<int>(validationDataInfile.tellg()) + MAGIC_NUMBER_1) / MAGIC_NUMBER_2;
	validationDataInfile.clear();				//clears eof bit
	validationDataInfile.seekg(0, ios::beg);	//resets the seekg head

												//CLASSIFY
												//Loop through samples
	for (int i = 0; i < numClassifications; i++)
	{
		forwardProp(i, validationDataInfile);				//Pass a sample to forwardProp
															//find the biggest element in output vector, or determine output as ambiguous
		for (int j = 0; j < activations[numLayers].nr(); j++)	//loop through the elements of the output
			if (activations[numLayers](j, 0) > activations[numLayers](biggestElement, 0))
				biggestElement = j;
		for (int j = 0; j < activations[numLayers].nr(); j++)
			if (activations[numLayers](j, 0) == activations[numLayers](biggestElement, 0))
				numBiggest++;
		if (numBiggest > 1)
			ambiguousData++;
		//print out the classification into a file
		classificationOutput << "Network Input:  " << dlib::trans(getM<double>(validationDataInfile, i));		//<-- possible problems here
		classificationOutput << "Network Output: " << dlib::trans(activations[numLayers]);
		classificationOutput << "Classification: ";
		if (numBiggest > 1)
			classificationOutput << "ambiguous data";
		else
			for (int j = 0; j < activations[numLayers].nr(); j++)
			{
				if (j == biggestElement)
					classificationOutput << "1 ";
				else
					classificationOutput << "0 ";
			}
		classificationOutput << "\n";
		//print out classification into cmd prompt
		cout << "Network Input:  " << dlib::trans(getM<double>(validationDataInfile, i));		//<-- possible problems here
		cout << "Network Output: " << dlib::trans(activations[numLayers]);
		cout << "Classification: ";
		if (numBiggest > 1)
			cout << "ambiguous data";
		else
			for (int j = 0; j < activations[numLayers].nr(); j++)
			{
				if (j == biggestElement)
					cout << "1 ";
				else
					cout << "0 ";
			}
		cout << "\n";
		//reset relevant counters for next classification
		biggestElement = 0;
		numBiggest = 0;
	}
	//Print out number classified, and % classified
	cout << numClassifications - ambiguousData << " out of " << numClassifications << " classified.";
	classificationOutput << numClassifications - ambiguousData << " out of " << numClassifications << " classified.";
	//CLOSING
	//close the files
	classificationOutput.close();
	validationDataInfile.close();

}

//Default constructor for the class
Network::Network()
{
	//Call readInit() to fill numLayers, layerSizes[], learningRate, epochs, batchSize
	readInit();

	//Ask for training data filename, then open it
	cout << "Please enter the location of your training file [C:\\...\\TrainingDataFilename.txt:\n";
	cin.ignore();
	getline(cin, trainingDataFilename);
	//trainingDataFilename = "C:\\Users\\...\\source\\repos\\Collin-Neural-Network\\Data\\UnsortedDataFile.txt";
	trainingDataInfile.open(trainingDataFilename);
	while (trainingDataInfile.fail())
	{
		trainingDataInfile.clear();
		trainingDataInfile.close();
		cout << "Could not open " << trainingDataFilename << ".\n" << "Please try again:\n";
		getline(cin, trainingDataFilename);
		trainingDataInfile.open(trainingDataFilename);
	}
	//Ask for expected values filename and open it
	cout << "Please enter the location of your truth data file [C:\\...\\ExpectedValuesFilename.txt:\n";
	getline(cin, expectedValuesFilename);
	//expectedValuesFilename = "C:\\Users\\...\\source\\repos\\Collin-Neural-Network\\Data\\UnsortedTruthFile.txt";
	expectedValuesInfile.open(expectedValuesFilename);
	while (expectedValuesInfile.fail())
	{
		expectedValuesInfile.clear();
		expectedValuesInfile.close();
		cout << "Could not open " << expectedValuesFilename << ".\n" << "Please try again:\n";
		getline(cin, expectedValuesFilename);
		expectedValuesInfile.open(expectedValuesFilename);
	}

	//resize the VMatrix's to match input
	weights.resize(numLayers);
	biases.resize(numLayers);
	activations.resize(numLayers);
	weightedInputs.resize(numLayers);
	errors.resize(numLayers);
	sumNablaB.resize(numLayers);
	sumNablaW.resize(numLayers);

	//fill out the 0th layer of activations, as they aren't covered
	//in the following for loop.
	cout << layerSizes[0] << endl;
	activations[0].set_size(layerSizes[0], 1);
	activations[0] = zeros_matrix(activations[0]);

	//set the sizes of each matrix, and fill with appropriate numbers
	for (int i = 1; i < numLayers; i++)
	{
		weights[i].set_size(layerSizes[i], layerSizes[i - 1]);
		randomizeMatrix(weights[i]);

		biases[i].set_size(layerSizes[i], 1);
		randomizeMatrix(biases[i]);

		activations[i].set_size(layerSizes[i], 1);
		activations[i] = zeros_matrix(activations[i]);

		weightedInputs[i].set_size(layerSizes[i], 1);
		weightedInputs[i] = zeros_matrix(weightedInputs[i]);

		errors[i].set_size(layerSizes[i], 1);
		errors[i] = zeros_matrix(errors[i]);

		sumNablaB[i].set_size(layerSizes[i], 1);
		sumNablaB[i] = zeros_matrix(sumNablaB[i]);

		sumNablaW[i].set_size(layerSizes[i], layerSizes[i - 1]);
		sumNablaW[i] = zeros_matrix(sumNablaW[i]);
	}

	//resize miniBatchIndices to be batchSize elements
	miniBatchIndices.resize(batchSize);
}

//Classify constructor. data initializing handled by readInit
Network::Network(const string& networkFilename, const string& validationDataFilename)
{
	readInit(networkFilename);
	classify(validationDataFilename);
}

//An overloaded readInit() to read the required values, to create or classify a network, from the given file
bool Network::readInit(const string & file)
{
	ifstream fin;
	fin.open(file, ios_base::in);
	if (!fin.is_open())
	{
		fin.clear();
		fin.open(file, ios_base::in);
	}
	else;

	if (fin.is_open())
	{
		int i;
		string cLayers;
		getline(fin, trainingDataFilename);
		getline(fin, expectedValuesFilename);
		fin >> learningRate;
		fin >> batchSize;
		fin >> epochs;
		fin >> numLayers;
		fin.seekg(2, ios::cur);
		getline(fin, cLayers);
		layerSizes = Strtok<int>(cLayers, " ");

		//Resize the vectors of the matrices to numLayers
		weights.resize(numLayers);
		biases.resize(numLayers);
		activations.resize(numLayers);
		weightedInputs.resize(numLayers);
		errors.resize(numLayers);
		sumNablaB.resize(numLayers);
		sumNablaW.resize(numLayers);

		//resize the 0th member of the activations vector: layer_sizes[0] by 1, fill with zeros
		activations[0].set_size(layerSizes[0], 1);
		activations.push_back(zeros_matrix(activations[0]));

		//Everything but the activations vector will have an effective size of num_layers-1, as their first element will be left unused.
		for (i = 1; i < numLayers; i++)
		{
			//weights matrix at index i created of size: layerSizes[i] by layer_sizes[i-1]
			weights[i].set_size(layerSizes[i], layerSizes[i - 1]);

			//biases matrix at index i created of size: layerSizes[i] by 1
			biases[i].set_size(layerSizes[i], 1);

			//activations matrix at index i created of size: layerSizes[i] by 1, filled with Zeroes
			activations[i].set_size(layerSizes[i], 1);
			activations[i] = zeros_matrix(activations[i]);

			//weightedInputs matrix at index i created of size: layerSizes[i] by 1, filled with Zeroes
			weightedInputs[i].set_size(layerSizes[i], 1);
			weightedInputs[i] = zeros_matrix(weightedInputs[i]);

			//errors matrix at index i created of size: layerSizes[i] by 1, filled with Zeroes
			errors[i].set_size(layerSizes[i], 1);
			errors[i] = zeros_matrix(errors[i]);

			//sumNablaB matrix at index i created of size: layerSizes[i] by 1, filled with Zeroes
			sumNablaB[i].set_size(layerSizes[i], 1);
			sumNablaB[i] = zeros_matrix(sumNablaB[i]);

			//sumNablaB matrix at index i created of size: layerSizes[i] by 1, filled with Zeroes
			sumNablaW[i].set_size(layerSizes[i], layerSizes[i - 1]);
			sumNablaW[i] = zeros_matrix(sumNablaW[i]);

			//end of the loop
		}

		//resize mini_batch_indices to batch_size
		miniBatchIndices.resize(batchSize);

		for (int i = 1; i < layerSizes.size(); i++)
		{
			fin.ignore(numeric_limits<streamsize>::max(), '\n');
			for (int j = 0; j < layerSizes[i]; j++)
				for (int k = 0; k < layerSizes[i - 1]; k++)
					fin >> weights[i](j, k);
			fin.ignore(numeric_limits<streamsize>::max(), '\n');

			fin.ignore(numeric_limits<streamsize>::max(), '\n');
			for (int j = 0; j < layerSizes[i]; j++)
				fin >> biases[i](j, 0);
			fin.ignore(numeric_limits<streamsize>::max(), '\n');
		}

		return true;
	}
	else
	{
		cout << "Server error 402: Could not open the file requested! Try again later..";
		return false;
	}
}

//This constructor will be called when the user wants to train an existing network. It will retrieve all the required information the given file
//and will then, prompt the user if user wants to change the values of hyperparameters. If so, it updates the values of hyperparameters
Network::Network(const string& previous_network_filename)
{
	readInit(previous_network_filename);
	char c;
	string str;
	cout << "\nWould you like to change the values of hyperparameters? Press Y/N:-> ";
	cin >> c;
	if ((c == 'y') || (c == 'Y'))
	{
		cout << "\nEnter a value for learning Rate (0 < x < 1): ";
		cin >> learningRate;
		cout << "\nEnter a value for batch size (x > 1): ";
		cin >> batchSize;
		cout << "\nEnter a value for epochs (x > 1): ";
		cin >> epochs;

		cout << "\nYou have successfully updated the hyparameters with following values:";
		cout << "\nLearning Rate: " << learningRate;
		cout << "\nBatch Size: " << batchSize;
		cout << "\nNumber of epochs: " << epochs;
	}
	else;
}

template<class T>
std::vector<T> Network::getV(ifstream& fin, const int i)
{
	std::vector<T> v;
	if (i >= 0)
	{
		fin.seekg(0);
		string str;
		getline(fin, str);
		fin.seekg(i * (str.size() + 2), ios_base::beg);
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

//A Strtok(), which can help us assigning any vector later on while reading from a file
//can be used in readInit() too
//Considering the fact that we don't want any user or programmer to use it, Strtok<T> can be a private member of the class.
template<class T>
std::vector<T> Network::Strtok(const string &str, char Separator[])
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
const matrix<T> Network::getM(ifstream& fin, const int i)
{
	std::vector<T> v;
	matrix<T> m;
	if (i >= 0)
	{
		fin.seekg(0);
		string str;
		getline(fin, str);
		fin.seekg(i * (str.size() + 2), ios_base::beg);
		getline(fin, str);
		v = Strtok<T>(str, ",");
		m.set_size(v.size(), 1);
		for (int i = 0; i < v.size(); i++)
		{
			m(i, 0) = v[i];
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

// thought it might be nice if train also returned a vector of the network's efficiency at each epoch
// but if we're having that info printed out to console (though i think it's better served storing somewhere for later access)
// this is not necessary and can be changed with juust a couple deletions

// train calls SGD on every mini batch in a training data set until the set has been exhausted as many times as epochs
// randomizing the set between each epoch
// no parameters
// returns a vector of doubles containing the percentage of outputs the network successfully classified for each iteration of an epoch
std::vector<double> Network::train()
{
	std::vector<double> efficiency(epochs);

	int trainingDataSize = fileSize(trainingDataInfile);

	Vector trainingDataIndices(trainingDataSize);
	for (int i = 0; i < trainingDataSize; i++)
		trainingDataIndices[i] = i;

	int batchSize = miniBatchIndices.size();
	int sgdCalls = trainingDataSize / batchSize;

	for (int i = 0; i < epochs; i++)
	{
		int numCorrect = 0;
		FYShuffle(trainingDataIndices);

		for (int j = 0; j < sgdCalls; j++)
		{
			for (int k = 0; k < batchSize; k++)
				miniBatchIndices[k] = trainingDataIndices[(j*batchSize) + k];
			numCorrect += SGD();
		}

		efficiency[i] = 100 * ((double)numCorrect) / (sgdCalls * batchSize);
		cout << "\nEfficiency at epoch: " << i << " = " << efficiency[i] << " %" << endl;
	}

	return efficiency;
}

// yes i know what you're thinking "parsing the whole file just for the size?!?!" but it's really NOT that slow
// this should be fine for what data we have now or in the near/far future
// i have a couple of benchmarks on a few pretty large files i generated so just ask me if you want to know the stats
//  - Yon
int Network::fileSize(istream& in)
{
	int count = 0;

	in.seekg(0, ios::beg);
	while (!in.eof())
	{
		count++;
		in.ignore(numeric_limits<streamsize>::max(), '\n');
	}
	in.seekg(0, ios::beg);

	return count;
}

//Destructor will be called at the end of the main(). It will be responsible to close all the files and deallocate the dynamic memory that was being used.
//Consider, having a close() to close the files, if there's going to be a loop in main()
Network:: ~Network()
{
	trainingDataInfile.close();
	expectedValuesInfile.close();
	//destructor of dlib and vector class called
}