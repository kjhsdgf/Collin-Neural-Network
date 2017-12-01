#include <math.h>
#include "Network.h"

std::vector<int> strings;
bool flag (false);

void Network::activationFuncSwitch(unsigned char e, int index)
{
	switch (e)
	{
	case inputLinear:
		linear(index);
		break;
	case inputSigmoid:
		Sigmoid(index);
		break;
	case inputComplementaryLog_Log:
		logLog(index);
		break;
	case inputBipolarSigmoid:
		bipolarSigmoid(index);
		break;
	case inputTanh:
		Tanh(index);
		break;
	case inputLeCun_stanh:
		LeCun_stanh(index);
		break;
	case inputSmoothRectifier:
		smoothRectifier(index);
		break;
	case inputRectifier:
		rectifier(index);
		break;
	/*case inputLogit:
		logit(index);
		break;*/
	case inputSoftmax:
		softmax(index);
		break;
	case inputRadialGaussian:
		radialGaussian(index);
		break;
	case inputMaxout:
		maxout(index);
		break;
	case inputLeakyRelu:
		leakyRelu(index);
		break;
	case inputCosine:
		cosine(index);
		break;
	default:
	{
		cout << "Unknown input..Try again";
		strings[index] = 0;
		activationFuncSelect(index);
	}
	}
}

void	Network::initStateTable()
{
	cout << "-------Here's the list of all the valid activation functions that can be used for the network-------";
	cout << "\n1) Linear Function" << endl;
	cout << "\n2) Sigmoid Function" << endl;
	cout << "\n3) Complementary Log-Log Function" << endl;
	cout << "\n4) Bipolar Sigmoid Function" << endl;
	cout << "\n5) Tanh Function " << endl;
	cout << "\n6) LeCun's Tanh Function " << endl;
	cout << "\n7) Rectifier Linear Function" << endl;
	cout << "\n8) Smooth Rectifier Function " << endl;
	//cout << "\n9) Logit Function " << endl;
	cout << "\n9) Softmax Function" << endl;
	cout << "\n10) RadialGaussian Function" << endl;
	cout << "\n11) Maxout Function " << endl;
	cout << "\n12) Leaky Rectifier Linear Function" << endl;
	cout << "\n13) Cosine Function " << endl;

	stateTable.set_size(numActivations + 1, numLayers);
	int i = 0, j = 0;
	while (i < stateTable.nr())
	{
		j = 0;
		while (j < stateTable.nc())
		{
			stateTable(i, j) = i;
			j++;
		}
		i++;
	}
	strings.resize(numLayers);
	for (i = 0; i < numLayers; i++)
		strings[i] = -1;
}

void Network::activationFuncSelect(int index)
{
	int j;
	if (strings[index] != -1)
		activationFuncSwitch(stateTable(strings[index], index), index);
	else
	{
		cout << "Enter the number of the activation function to be used for layer " << index << " -> ";
		cin >> j;
		strings[index] = (static_cast<int>(j) - (1));
		activationFuncSwitch(stateTable(strings[index], index), index);
	}
}

void Network::logLog(int index)
{
	//1 − exp(−exp(weightedInputs(i, j)))
	activations[index] = (ones_matrix<double>(layerSizes[index], 1) - exp(zeros_matrix<double>(layerSizes[index], 1) - exp(weightedInputs[index])));
	activationPrime[index] = pointwise_multiply(activations[index] - ones_matrix(activations[index]), zeros_matrix<double>(layerSizes[index], 1) - exp(weightedInputs[index]));
}

void Network::bipolarSigmoid(int index)
{
	//	(1 - exp(-(weightedInputs(i,j))) / (1 + exp(-weightedInputs(i,j)))
	int i;
	for (i = 0; i < layerSizes[index]; i++)
	{
		activations[index](i) = (1.00 - exp(0.00 - weightedInputs[index](i))) / (1.00 + exp(0.000 - weightedInputs[index](i)));
		activationPrime[index](i) = (2.00 * exp(-weightedInputs[index](i))) / (pow(1.00 + exp(-weightedInputs[index](i)), 2));
	}
}

void Network::rectifier(int index)
{
	int i;
	for (i = 0; i < layerSizes[index]; i++)
	{
		activations[index](i) = (weightedInputs[index](i) > 0) ? (weightedInputs[index](i)) : (0);
		activationPrime[index](i) = (activations[index](i)) ? (1) : (0);
	}
}

void Network::smoothRectifier(int index)
{
	//	log(1 + exp(weightedInputs(i,j))
	int i;
	activations[index] = log(ones_matrix<double>(layerSizes[index], 1) - exp(weightedInputs[index]));
	for (i = 0; i < layerSizes[index]; i++)
		activationPrime[index](i) = (1.00) / (1.00 + exp(-weightedInputs[index](i)));
}

/*void Network::logit(int index)
{
	//  log(weightedInputs(i,j) / (1 - weightedInputs(i,j)))
	int i;
	for (i = 0; i < layerSizes[index]; i++)
	{
		activations[index](i) = log(weightedInputs[index](i) / (1.00 - weightedInputs[index](i)));
		activationPrime[index](i) = (1.00) / (weightedInputs[index](i) * (1.00 - weightedInputs[index](i)));
	}
}*/

void Network::softmax(int index)
{
	//	exp(weightedInputs(i,j)) / sum of exp(weightedInputs(i,j)) for the last l
	int i;
	double sum(0.00);
	for (i = 0; i < layerSizes[index]; i++)
		sum += exp(weightedInputs[index](i));
	activations[index] = (1.0 / sum) * (exp(weightedInputs[index]));
	activationPrime[index] = (activations[index] - squared(activations[index]));
}

void Network::maxout(int index)
{
	//	max of activations[i-1].weights[i] + biases[i]
	double biggest = weightedInputs[index](0);
	int  i;
	int biggestIndex(0);
	activations[index](0) = weightedInputs[index](0);
	for (i = 1; i < layerSizes[index]; i++)
		if (weightedInputs[index](i) > biggest)
		{
			biggest = weightedInputs[index](i);
			activations[index](i) = biggest;
			activationPrime[index](i) = 1.0;
		}
		else
		{
			activations[index](i) = biggest;
			activationPrime[index](i) = 0;
		}
}

void Network::leakyRelu(int index)
{
	//  alpha * weightedInputs(i,j)  z < 0, alpha = 0 < x < 1
	//	weightedInputs(i,j)			 z > 0

	float alpha(0.000718);
	int i;
	for (i = 0; i < layerSizes[index]; i++)
	{
		activations[index](i) = (weightedInputs[index](i) > 0) ? (weightedInputs[index](i)) : ((alpha)* weightedInputs[index](i));
		activationPrime[index](i) = (activations[index](i)) ? (1.0) : (-1.0 * alpha);
	}
}

bool Network::setActivationFunc(int activationFuncIndex)
{
	if ((activationFuncIndex >= 0) && (activationFuncIndex < numActivations) && (!flag))
	{
		int i;
		for (i = 0; i < numLayers; i++)
			strings[i] = activationFuncIndex;
		cout << "\nThe activation function set to " << activationFunc[activationFuncIndex] << " function successfully.." << endl;
		return true;
	}
	else
	{
		cout << "Error 411: Cannot set the activation function!" << endl;
		return false;
	}
}

Network::Network()
{
	//Call readInit() to fill numLayers, layerSizes[], learningRate, epochs, batchSize
	readInit();

	//Ask for training data filename, then open it
	cout << "Please enter the location of your training file [C:\\...\\TrainingDataFilename.txt:";
	cin >> trainingDataFilename;
	cout << endl;
	trainingDataInfile.open(trainingDataFilename, ios_base::in);
	while (trainingDataInfile.fail())
	{
		trainingDataInfile.clear();
		trainingDataInfile.close();
		cout << "Could not open " << trainingDataFilename << ".\n" << "Please try again:" << endl;
		getline(cin, trainingDataFilename);
		trainingDataInfile.open(trainingDataFilename);
	}
	//Ask for expected values filename and open it
	cout << "Please enter the location of your truth data file [C:\\...\\ExpectedValuesFilename.txt:" << endl;
	cin >> expectedValuesFilename;
	expectedValuesInfile.open(expectedValuesFilename);
	while (expectedValuesInfile.fail())
	{
		expectedValuesInfile.clear();
		expectedValuesInfile.close();
		cout << "Could not open " << expectedValuesFilename << ".\n" << "Please try again:" << endl;
		getline(cin, expectedValuesFilename);
		expectedValuesInfile.open(expectedValuesFilename);
	}

	initStateTable();
	initTrainingData();
	checkBatchSize();
	//resize the VMatrix's to match input
	weights.resize(numLayers);
	biases.resize(numLayers);
	activations.resize(numLayers);
	activationPrime.resize(numLayers);
	weightedInputs.resize(numLayers);
	errors.resize(numLayers);
	sumNablaB.resize(numLayers);
	sumNablaW.resize(numLayers);

	//fill out the 0th layer of activations, as they aren't covered
	//in the following for loop.
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

		activationPrime[i].set_size(layerSizes[i], 1);
		activationPrime[i] = zeros_matrix(activationPrime[i]);

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

Network:: ~Network()							//CHANGE!!!
{
	if (wrongInputs.size())
	{
		cout << "\tPossible Errors:->" << endl;
		std::vector<string> ::const_iterator i1;
		i1 = wrongInputs.begin();
		for (; i1 != wrongInputs.end(); i1++)
			cout << (*i1) << '\n';
	}
	
	trainingDataInfile.close();
	expectedValuesInfile.close();
	//destructor of dlib and vector class called
}


bool  Network::writeToFile() const
{
	/*This method creates a file named "Previous_Network_[Day][Time(hhmin)].txt" and
	writes all the required parameters of the class network in the file.
	This file can be used later to train the existing network or to classify the file
	provided by the user, using the values from any previous network.
	*/

	ofstream outfile;
	std::vector<int>::const_iterator i1;
	int i;
	int j(0);
	int k;
	string a("Previous_Network_");
	time_t _tm = time(NULL);
	struct tm * curtime = localtime(&_tm);
	a += asctime(curtime);
	string fileName;

	fileName.resize(24);
	for (i = 0; i < 20; i++)		//gets the name "Previous_Network_[Day]"
		fileName[j++] = a[i];

	for (i = 28; i < 35; i++)	//appends the [time (hhmin)] to name of the file
		if (a[i] != ':')
			fileName[j++] = a[i];

	fileName += ".txt";				//appends ".txt" to the name of the file
	outfile.open(fileName, ios_base::out); //creates the file with name "Previous_Network_[Day][Time(hhmin)].txt"
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
			outfile << weights[j];			//there is overloaded operator for '<<' to display the matrices inside dlib
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
Network::Network(const string& networkFilename, const string& validationDataFilename)
{
	readInit(networkFilename);
	initTrainingData();
	classify(validationDataFilename);
}
//lr_highest can be decided by us later and till then the default value is set to 1 (-Ami)
void Network::checkLearningRate(int lr_highest)
{
	bool result;
	string temp;
	while (!(result = ((learningRate > 0) && (learningRate < lr_highest))))	//loop continues until the learning rate is between 0 and given end point
	{
		//If learning rate is not in range, displays an error message
		cout << "\nError: The learning rate you entered is wrong.." << endl;

		//also, it prompts the user if the user wants to continue or change the value entered
		cout << "Press 'end' to continue with the value entered or 'change' to change the value" << endl;
		cin >> temp;
		if (temp == "end")
		{
			string wrong_data;
			wrong_data = "Incorrect learning rate: ";
			wrong_data += static_cast<int> (learningRate + '0');
			wrongInputs.push_back(wrong_data);
			break;
		}
		else
		{
			cout << "Enter the value of learning rate in range ( 0 x < " << lr_highest << "): " << endl;
			if (!(cin >> learningRate))
				continue;
		}
	}

}
void Network::checkEpochs()
{
	//--------------------------------------------------------------------------------------------------------
	//checks if the number of epochs is a positive integer
	//consider using size_t instead of int
	while (epochs < 1)
	{
		cout << "\nInvalid number of epochs..";
		cout << "\nCannot proceed..Enter a valid number of epochs (x > 0): " << endl;
		if (!(cin >> epochs))
			continue;
	}
}

void Network::checkNumLayers()
{
	//--------------------------------------------------------------------------------------------------------
	//checks if the number of layers read from the file is a valid positive integer
	//consider using size_t instead of int
	if (numLayers < 2)
	{
		cout << "\nInvalid number of layers..";
		numLayers = layerSizes.size();
	}
}

void Network::checkBatchSize()
{
	//checks if the batchSize is not greater than the size of file
	//use this piece of code only if training data file is opened before you call this function
	//------------------------------------------------------------------------------------------------------
	int end_of_file = trainingData.size();					//CHANGEEEE!!!
	while ((batchSize > end_of_file) || (batchSize < 0))
	{
		cout << "\nInvalid batch size..";
		cout << "\nCannot proceed..Enter a valid number for batch size (0 < x < " << end_of_file << "): " << endl;
		if (!(cin >> batchSize))
			continue;
	}
}
void Network::checkLayersString(string& layer_string)
{
	//checks the string of layer sizes
	//erases all the unwanted characters and records the errors in the vector of wrong_inputs
	int go_ahead(0);
	int j(0);
	string temp;
	do
	{
		j = 0;
		temp.resize(layer_string.size());
		for (int i = 0; i < layer_string.size(); i++)
		{
			string wrong_data;
			if ((!isdigit(layer_string[i])) && (layer_string[i] != ' '))
			{
				wrong_data = "layer size with ";
				wrong_data += layer_string[i];
				wrongInputs.push_back(wrong_data);
			}
			else
			{
				temp[j++] = layer_string[i];
				if (layer_string[i] == ' ') go_ahead++;
			}
		}
		layer_string.resize(j);
		layer_string = temp;
		if (!go_ahead)
		{
			string s;
			cout << "\nCannot proceed..Enter at least two layers for the network: " << endl;
			cin.ignore();
			getline(cin, s);
			layer_string.resize(s.size());
			layer_string = s;
		}
	} while (go_ahead < 1);
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
		checkLearningRate();
		fin >> batchSize;
		fin >> epochs;
		checkEpochs();
		fin >> numLayers;
		fin.seekg(2, ios::cur);
		getline(fin, cLayers);
		checkLayersString(cLayers);
		layerSizes = Strtok<int>(cLayers, " ");
		checkNumLayers();
		//Resize the vectors of the matrices to numLayers
		weights.resize(numLayers);
		biases.resize(numLayers);
		activations.resize(numLayers);
		activationPrime.resize(numLayers);
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

			activationPrime[i].set_size(layerSizes[i], 1);
			activationPrime[i] = zeros_matrix(activationPrime[i]);

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

Network::Network(const string& previous_network_filename)
{
	readInit(previous_network_filename);
	//srand(time(0));
	initStateTable();
	trainingDataInfile.open(trainingDataFilename);
	if (!trainingDataInfile.is_open())
	{
		trainingDataInfile.clear();
		trainingDataInfile.close();
		trainingDataInfile.open(trainingDataFilename);
		if (!trainingDataInfile.is_open())
			cout << "\nServer Error 406: Could not open the requested training data file" << endl;
		else;
	}
	expectedValuesInfile.open(expectedValuesFilename);
	if (!expectedValuesInfile.is_open())
	{
		expectedValuesInfile.clear();
		expectedValuesInfile.close();
		expectedValuesInfile.open(expectedValuesFilename);
		if (!expectedValuesInfile.is_open())
			cout << "\nServer Error 407: Could not open the requested expected values file" << endl;
		else;
	}
	initTrainingData();
	char c;
	string str;
	cout << "\nWould you like to change the values of hyperparameters? Press Y/N:-> ";
	cin >> c;
	if ((c == 'y') || (c == 'Y'))
	{
		cout << "\nEnter a value for learning Rate (0 < x < 1): ";
		cin >> learningRate;
		checkLearningRate();
		cout << "\nEnter a value for batch size (x > 1): ";
		cin >> batchSize;
		cout << "\nEnter a value for epochs (x > 1): ";
		cin >> epochs;
		checkEpochs();

		cout << "\nYou have successfully updated the hyparameters with following values:";
		cout << "\nLearning Rate: " << learningRate;
		cout << "\nBatch Size: " << batchSize;
		cout << "\nNumber of epochs: " << epochs << endl;
	}
	else;
	checkBatchSize();
	miniBatchIndices.resize(batchSize);
}

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

void Network::forwardProp(const int batchIndex, ifstream& infile)
{
	//extract data point from training data file at input indice into first layer of activations
	activations[0] = getM<double>(false, batchIndex);
	for (int i = 1; i < numLayers; i++)
	{
		weightedInputs[i] = ((weights[i] * activations[i - 1]) + biases[i]);
		activationFuncSelect(i);
	}
		//activations[i] = activationFunction(weightedInputs[i]);
	//for (int j = 1; j < numLayers; j++)		//required, especially when we use softmax function
		//activationFuncSelect(j);
}

void Network::updateWeightsAndBiases()
{
	double k = (learningRate / batchSize); //saves (numLayers - 1) divisions
	for (int i = 1; i < numLayers; i++)
	{
		weights[i] -= k * sumNablaW[i];
		biases[i] -= k * sumNablaB[i];
	}
}

bool Network::backProp(int index)
{
	int lastInd = numLayers - 1;
	int lastSize = layerSizes[lastInd];
	Matrix expectedValues = getM<double>(true, index);

	bool correct = compareOutput(expectedValues);


	errors[lastInd] = hadamardProduct(costPrime(activations[lastInd], expectedValues), activationPrime[lastInd]);
	sumNablaB[lastInd] += errors[lastInd];
	sumNablaW[lastInd] += (errors[lastInd]) * trans(activations[lastInd - 1]);

	for (int i = lastInd - 1; i > 0; i--)
	{
		errors[i] = hadamardProduct(trans(weights[i + 1]) * errors[i + 1],
			activationPrime[i]);
		sumNablaB[i] += errors[i];
		sumNablaW[i] += errors[i] * trans(activations[i - 1]);
	}

	/*displayActivations(expectedValues);
	createActivationsFile(expectedValues);*/
	return correct;
}

bool Network::compareOutput(const Matrix& expectedValues)
{
	int lastInd = numLayers - 1;
	int lastSize = layerSizes[lastInd];
	int biggestI = -1; int expectedI = -1;
	double biggest = 0;	int numBiggest = 0;
	if (expectedValues.size() != lastSize)
	{
		cout << "Error in CompareOutput" << endl;
		return 0;
	}

	for (int i = 0; i < lastSize; i++)
	{
		double output = activations[lastInd](i, 0);
		if (output > biggest)
		{
			biggestI = i;
			biggest = output;
			numBiggest = 0; // we found a new biggest so numBiggest's previous data is invalid
		}
		else if (output == biggest)
			numBiggest++;

		if (expectedValues(i, 0) == 1 && expectedI == -1)
		{
			if (expectedI == -1) // if it's not been set before..
				expectedI = i;
			else				 // it it has it's bad data
				expectedI = -2;
		}
	}

	if (biggestI == -1 || expectedI <= -1) // everything was smaller than 0 or expected is bad data (probably should throw excpetion instead actually)
		return 0;

	if (numBiggest > 0) // if there's more than 1 biggest, ambiguous data. don't even care if the indices match
		return 0;

	if (biggestI == expectedI)
		return true;
	else
		return false;
}

std::vector<double> Network::tokenize(const string& line, char delims[])
{
	char* cStrLine = new char[line.size() + 1];
	strcpy(cStrLine, line.c_str());
	std::vector<double> tokens;
	for (char *p = strtok(cStrLine, delims); p != NULL; p = strtok(NULL, delims))
		tokens.push_back(atof(p));
	delete[] cStrLine;
	return tokens;
}

bool Network::getNext(std::vector<double>& nextData, std::vector<double>& nextTruth)
{
	/* for 'our' data
	if (!trainingDataInfile.is_open() || !expectedValuesInfile.is_open())
		return false;
	string dataLine, truthLine;
	getline(trainingDataInfile, dataLine); getline(expectedValuesInfile, truthLine);
	if (dataLine.length() == 0 || truthLine.length() == 0)
		return false;
	nextData = tokenize(dataLine);
	nextTruth = tokenize(truthLine);
	return true;
	*/

	//for mnist data
	if (!trainingDataInfile.is_open())
		return false;
	string line;
	getline(trainingDataInfile, line);
	if (line.length() == 0)
		return false;
	nextData.resize(784); nextTruth.resize(10);
	nextData = tokenize(line.substr(2, line.length()));
	for (int i = 0; i < nextTruth.size(); i++)
		nextTruth[i] = 0;
	nextTruth[(int)line[0] - 48] = 1; cout << line[0];
	return true;
}

void Network::initTrainingData()
{
	std::vector<double> nextData;
	std::vector<double> nextTruth;
	while (getNext(nextData, nextTruth))
	{
		trainingData.push_back(nextData);
		expectedValues.push_back(nextTruth);
	}
}

void Network::readInit() // reading from console
{
	cout << "Welcome! Please follow the prompts to initialize and begin training your network." << endl;
	cout << "Enter a string of integers that correspond to the layers and desired nodes in each layer of your network:" << endl;
	string layers;  getline(cin, layers);
	checkLayersString(layers);
	char* cStrLayers = new char[layers.size() + 1];
	strcpy(cStrLayers, layers.c_str());

	for (char *p = strtok(cStrLayers, " ,"); p != NULL; p = strtok(NULL, " ,"))
		layerSizes.push_back(atoi(p));

	numLayers = layerSizes.size();

	cout << "\nPlease enter a double for the learning rate (usually in the range [x-y]):" << endl;
	cin >> learningRate;
	checkLearningRate();

	cout << "\nPlease enter an integer for the number of epochs (number of times to parse through test data):" << endl;
	cin >> epochs;
	checkEpochs();

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

std::vector<double> Network::train()
{
	if (!writeToFile())
		cout << "\n Server error 405: Could not write network to file." << endl;

	std::vector<double> efficiency(epochs);
	int trainingDataSize = trainingData.size();
	cout << "trainingdatasize: " << trainingDataSize << "\nexpectedvaluesize: " << expectedValues.size() << endl;

	flag = true;

	Vector trainingDataIndices(trainingDataSize);
	for (int i = 0; i < trainingDataSize; i++)
		trainingDataIndices[i] = i;

	int sgdCalls = trainingDataSize / batchSize;
	int numCorrect;
	for (int i = 0; i < epochs; i++)
	{
		numCorrect = 0;
		FYShuffle<int>(trainingDataIndices);
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

/*int Network::filesize(istream& in)

{
	int count(0);
	in.seekg(0, ios::beg);
	while (!in.eof())
	{
		count++;
		in.ignore(numeric_limits<streamsize>::max(), '\n');
	}
	in.seekg(0, ios::beg);
	return count;
}*/

Network::layerReport Network::outputLayerReport()
{
	layerReport output;
	output.cleanOutput.set_size(activations[numLayers - 1].nr(), 1);
	output.isAmbiguous = false;
	int biggestElementIndice = 0;
	double biggestElementValue = -DBL_MAX;
	double currentNeuronOutput = 0;

	for (int i = 0; i < activations[numLayers - 1].nr(); i++)
	{
		currentNeuronOutput = activations[numLayers - 1](i, 0);
		if (currentNeuronOutput > biggestElementValue)
		{
			biggestElementIndice = i;
			biggestElementValue = activations[numLayers - 1](i, 0);
			output.isAmbiguous = false;
		}
		else if (currentNeuronOutput == biggestElementValue)
			output.isAmbiguous = true;
	}

	for (int i = 0; i < activations[numLayers - 1].nr(); i++)
	{
		if (activations[numLayers - 1](i, 0) == biggestElementValue)
			output.cleanOutput(i, 0) = 1;
		else
			output.cleanOutput(i, 0) = 0;
	}

	return output;
}

void Network::classify(const string &validation_data_filename)
{
	//SETUP
	//create an infile object for validation data, and open it
	ifstream validationDataInfile(validation_data_filename, ios::ate);	//opened at end for calc of numClassifications
	if (validationDataInfile.fail())
	{
		std::cout << "File \'" << validation_data_filename << "\' not found!\n";
		std::cout << "Exiting classify()...\n";
		return;
	}

	//create outfile for output and open it
	string outputFilename = "classification";							//what filename starts with
	const int MAX_SIZE(80);												//buffer (maximum) size for the intermediary cstring
	time_t currentTime = time(NULL);									//returns current time
	struct tm * currentTimeInfo = localtime(&currentTime);				//stores current time into a struct
	char outputFileTime[MAX_SIZE];										//creates cstring intermediary
	strftime(outputFileTime, MAX_SIZE, "%a%H%M", currentTimeInfo);		//converts cstring intermediary into a string containing the current time
	string filetype = ".txt";											//small filetype appendage
	outputFilename += outputFileTime + filetype;						//appends Day(of week)HourMinute
	ofstream classificationOutput(outputFilename, ios::trunc);			//creates and opens output file

																		//variables used in the classification loop
	layerReport report;													//stores generated "clean" output and ambiguity state
	int i = 0;															//index for the while loop
	bool isAmbiguous = report.isAmbiguous;								//stores state of ambiguity in network output
	Matrix cleanedOutput = report.cleanOutput;							//stores "cleaned" output of network
	int numAmbiguousData = 0;											//counts number of data in an ambiguous state
	int numClassifications = expectedValues.size();			//counts number of data in validation data file
	Matrix currentSample(layerSizes[numLayers - 1], 1);

	//CLASSIFY
	//Loop through samples
	while (!validationDataInfile.eof())
	{
		currentSample = getM<double>(true, i);//CHHAAAAAAAAAANGE!!!!!
		forwardProp(i, validationDataInfile);
		report = outputLayerReport();
		isAmbiguous = report.isAmbiguous;
		cleanedOutput = report.cleanOutput;

		if (isAmbiguous)
			numAmbiguousData++;

		//print out the classification into a file
		classificationOutput << "Network Input:  \"" << dlib::trans(getM<double>(true, i));//CHHAAAAAAAAAANGE!!!!!
		classificationOutput << "Network Output: " << dlib::trans(activations[numLayers - 1]);
		classificationOutput << "Classification: " << dlib::trans(cleanedOutput) << '\n';

		//print out classification into cmd prompt
		std::cout << "Network Input:  " << dlib::trans(getM<double>(true, i++));	//NOTICE, the i++ is in this line! //CHHAAAAAAAAAANGE!!!!!
		std::cout << "Network Output: " << dlib::trans(activations[numLayers - 1]);
		std::cout << "Classification: " << dlib::trans(cleanedOutput) << '\n';
	}
	//Print out number classified, and % classified
	std::cout << numClassifications - numAmbiguousData << " out of " << numClassifications << " classified, " <<
		(numClassifications - numAmbiguousData) / (double)numClassifications * 100 << " %";
	classificationOutput << numClassifications - numAmbiguousData << " out of " << numClassifications << " classified, " <<
		(numClassifications - numAmbiguousData) / (double)numClassifications * 100 << " %";
	//CLOSING
	//close the files
	classificationOutput.close();
	validationDataInfile.close();
}

void	Network::displayActivations(const Matrix& expectedValues, ostream& out)
{
	out << "--------------------------------------------------" << endl;
	out << "Input: " << trans(activations[0]) << endl;
	for (int i = 1; i < (numLayers - 1); i++)
		out << "Hidden Layer " << i << ":" << trans(activations[i]) << endl;
	out << "Output: " << trans(activations[numLayers - 1]) << endl;
	out << "Clean Output: " << trans(outputLayerReport().cleanOutput) << endl;
	out << "Expected: " << trans(expectedValues);
	out << "--------------------------------------------------" << endl;
}

void	Network::createActivationsFile(const Matrix& expectedValues)
{
	ofstream outfile;
	string a("Activations");
	time_t currentTime = time(NULL);
	struct tm * t = localtime(&currentTime);
	char Time[8];
	strftime(Time, 8, "%a%H%M", t);
	a += Time;
	a += ".txt";
	outfile.open(a, ios_base::out | ios_base::app);
	displayActivations(expectedValues, outfile);
	outfile.close();
}

bool Network :: makeGraphFile(int index, const string& graphFileName, double threshold)
{
	/*******************************************************************************
	Follow the instructions to create the image:
	1. Open the Command Prompt
	2. Go to the address- ...\graphviz-2.38\release\bin>
	3. Give the command: dot.exe -c
	4. Write the command: dot.exe -Tjpg(can be any other file type) -O graphFile(with index).txt
	5. This will create the desired graph inside the bin folder of graphviz
	*******************************************************************************/

	string fileName;
	int k = index;
	string str2;
	int i;

	if (graphFileName != "None")
		fileName = graphFileName;
	else
	{
		std::vector<int> digits;
		while (index)
		{
			digits.push_back(index % 10);
			index /= 10;
		}
		std::reverse(digits.begin(), digits.end());
		str2.resize(digits.size());
		for (i = 0; i < digits.size(); i++)
			str2[i] = digits[i] + '0';
		fileName = "C:/graphviz-2.38/release/bin/graphFile" + str2 + ".txt"; //Attention!!!! Address here needs to be the path ..../graphviz-2.38/release/bin
	}

	string shape; shape = "circle";
	string style; style = "filled";
	string rank; rank = "same";
	std::vector <matrix<string>> color;
	std::vector<string> label;
	fstream outfile;
	int j;

	outfile.open(fileName, ios::out);
	if (outfile.is_open())
	{
		cout << "\nCreating graph file " << fileName << endl;
		cout << threshold;
		forwardProp(k, trainingDataInfile);

		color.resize(numLayers);
		//initialize the colors matrix

		//Input layer with value bigger than threshold will be displayed with black and remaining will be green in color
		color[0].set_size(layerSizes[0], 1);
		for (j = 0; j < layerSizes[0]; j++)
			if (activations[0](j, 0) > threshold)
				color[0](j, 0) = "black";
			else
				color[0](j, 0) = "green";

		//Hidden layer with firing nodes will be displayed with yellow color and remaining in dodger blue
		for (i = 1; i < (numLayers - 1); i++)
		{
			color[i].set_size(layerSizes[i], 1);
			for (j = 0; j < layerSizes[i]; j++)
				if (activations[i](j, 0) > threshold)
 					color[i](j, 0) = "yellow";
				else
					color[i](j, 0) = "dodgerblue";
		}

		//Output layer will contain the neuron fired in black and other in white
		activations[i] = outputLayerReport().cleanOutput;
		color[i].set_size(layerSizes[i], 1);
		for (j = 0; j < layerSizes[i]; j++)
			if (activations[i](j, 0))
				color[i](j, 0) = "black";
			else
				color[i](j, 0) = "white";


		
		//Writing the commands required by graphviz in the file
		outfile << "digraph G {\n";
		outfile << "\trankdir = LR;\n";
		outfile << "\tsplines=false;\n";
		outfile << "\tedge[style=invis];\n";
		outfile << "\tranksep= 1.4;\n";

		//the commands for shape, labels and color of nodes
		i = 0;
		for (j = 0; j < layerSizes[i]; j++)
		{
			outfile << "\t{\n";
			outfile << "\tnode [shape=" << shape << ", color=" << color[i](j, 0) << ", style=" << style << ", fillcolor=" << color[i](j, 0) << "];\n";
			outfile << "\tx" << j << " [label=<x<sub>" << j << "</sub>>];\n";
			outfile << "\t}\n";
		}

		for (i = 1; i < (numLayers-1); i++)
			for (j = 0; j < layerSizes[i]; j++)
			{
				outfile << "\t{\n";
				outfile << "\tnode [shape=" << shape << ", color=" << color[i](j, 0) << ", style=" << style << ", fillcolor=" << color[i](j, 0) << "];\n";
				outfile << "\ta" << (j) << (i+1) << " [label=<a<sub>" << (j) << "</sub><sup>(" << (i + 1) << ")</sup>>];\n";
				outfile << "\t}\n";
			}

		for (j = 0; j < layerSizes[i]; j++)
		{
			outfile << "\t{\n";
			outfile << "\tnode [shape=" << shape << ", color=" << color[i](j, 0) << ", style=" << style << ", fillcolor=" << color[i](j, 0) << "];\n";
			outfile << "\tO" << j+1 << " [label=<O<sub>" << j+1 << "</sub>>];\n";
			outfile << "\t}\n";
		}

		//the commands for rank of the nodes
		i = 0;
		outfile << "\t{\n";
		outfile << "\trank=" << rank << ";\n";
		outfile << "\tx0";
		for (j = 1; j < layerSizes[i]; j++)
			outfile << "->x" << j;
		outfile << ";\n\t}\n";

		for (i = 1; i < (numLayers - 1); i++)
		{
			outfile << "\t{\n";
			outfile << "\trank=" << rank << ";\n";
			outfile << "\ta0"<<i+1;
			for (j = 1; j < layerSizes[i]; j++)
				outfile << "->a" << j << i + 1;
			outfile << ";\n\t}\n";
		}
		outfile << "\t{\n";
		outfile << "\trank=" << rank << ";\n";
		outfile << "\tO1";
		for (j = 1; j < layerSizes[i]; j++)
			outfile << "->O" << j+1;
		outfile << ";\n\t}\n";

		//labels
		shape = "plaintext";
		for (i = 1; i < (numLayers - 1); i++)
		{
			outfile << "\tl" << i << " [shape=" << shape << ",label=\"" << activationFunc[strings[i]] << "\"];\n";
			outfile << "\tl" << i << "->a0" << i + 1 << ";\n";
			outfile << "\t{rank=" << rank << "; l" << i << ";a0" << i + 1;
			outfile << "};\n";
		}

		outfile << "\tl" << i << " [shape=" << shape << ",label=\"" << activationFunc[strings[i]] << "\"];\n";
		outfile << "\tl" << i << "->O1;\n";
		outfile << "\t{rank=" << rank << "; l" << i << ";O1";
		outfile << "};\n";

		//edges
		style = "solid";
		outfile << "edge[style=" << style << ", tailport=e, headport=w];\n";
		i = 0;
		outfile << "\t{";
		outfile << "x0";
		for (j = 1; j < layerSizes[i]; j++)
			outfile << ";x" << j;
		outfile << "} -> ";

		for (i = 1; i < (numLayers - 1); i++)
		{
			outfile << "{";
			outfile << "a0" << i + 1 ;
			for (j = 1; j < layerSizes[i]; j++)
				outfile << ";a" << j << i + 1;
			outfile << "};\n";

			outfile << "\t{";
			outfile << "a0" << i + 1;
			for (j = 1; j < layerSizes[i]; j++)
				outfile << ";a" << j << i + 1;
			outfile << "} -> ";
			\
		}
		outfile << "{";
		outfile << "O1";
		for (j = 1; j < layerSizes[i]; j++)
			outfile << ",O" << j + 1;
		outfile << "};\n";
		outfile << "}";
		outfile.close();
		return true;
	}
	else
	{
		cout << "Error 410: Cannot write to the file " << fileName << endl;
		return false;
	}	
}
