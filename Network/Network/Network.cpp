#include "Network.h"
///
//State Machine Assets
///

/*void Network::Switch(unsigned char e, int index)
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
	case inputLogit:
		logit(index);
		break;
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
			takeInput(index);
		}
	}
}	 

void Network::initStateTable()
{
	Network::activations.resize(1);
	Network::weightedInputs.resize(1);
	Network::layerSizes.resize(1);

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
}*/

///
//Activation Functions for State Machine
///
//Need to properly scope the Network members, or else make them nonstatic.
/*
 void Network::linear(int index)
{
	activations[index] = weightedInputs[index];
	activationPrime[index] = ones_matrix<double>(layerSizes[index], 1);
}

void Network::Sigmoid(int index)
{
	activations[index] = sigmoid(weightedInputs[index]);
	activationPrime[index] = pointwise_multiply(activations[index], ones_matrix(activations[index]) - activations[index]);
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
		activations[index](i) = (1 - exp(0 - weightedInputs[index](i))) / (1 + exp(0 - weightedInputs[index](i)));
		activationPrime[index](i) = (2 * exp(-weightedInputs[index](i))) / (pow(1 + exp(-weightedInputs[index](i)), 2));
	}
}

void Network::Tanh(int index)
{
	activations[index] = tanh(weightedInputs[index]);
	activationPrime[index] = ones_matrix<double>(layerSizes[index], 1) - squared(activations[index]);
}

void Network::LeCun_stanh(int index)
{
	//	1.7159 tanh((2/3) * weightedInputs(i,j)) 
	activations[index] = (1.7159) * tanh((2 / 3) * weightedInputs[index]);
	activationPrime[index] = 0.98143 * (ones_matrix<double>(layerSizes[index], 1) - (0.33964 * squared(activations[index])));
}

void Network::rectifier(int index)
{
	int i;
	for (i = 0; i < layerSizes[index]; i++)
	{
		activations[index](i) = (weightedInputs[index](i) > 0) ? (weightedInputs[index](i)) : (0);
		activationPrime[index](i) = (activations[index](i) != 0) ? (1) : (0);
	}
}

void Network::smoothRectifier(int index)
{
	//	log(1 + exp(weightedInputs(i,j))
	int i;
	activations[index] = log(ones_matrix<double>(layerSizes[index], 1) - exp(weightedInputs[index]));
	for (i = 0; i < layerSizes[index]; i++)
		activationPrime[index](i) = 1 / (1 + exp(-weightedInputs[index](i)));
}

void Network::logit(int index)
{
	//  log(weightedInputs(i,j) / (1 - weightedInputs(i,j)))
	int i;
	for (i = 0; i < layerSizes[index]; i++)
	{
		activations[index](i) = log(weightedInputs[index](i) / (1 - weightedInputs[index](i)));
		activationPrime[index](i) = 1 / (weightedInputs[index](i) * (1 - weightedInputs[index](i)));
	}
}

void Network::softmax(int index)
{
	//	exp(weightedInputs(i,j)) / sum of exp(weightedInputs(i,j)) for the last l
	int i;
	double sum(0);
	for (i = 0; i < layerSizes[index]; i++)
		sum += exp(weightedInputs[index](i));
	activations[index] = (1 / sum) * (exp(weightedInputs[index]));
	activationPrime[index] = (activations[index] - squared(activations[index]));
}

void Network::radialGaussian(int index)
{
	//	exp( -(1/2)*((weightedInputs(i,j)^2))
	activations[index] = exp((-1 / 2) * (squared(weightedInputs[index])));
	activationPrime[index] = pointwise_multiply(zeros_matrix<double>(layerSizes[index], 1) - weightedInputs[index], activations[index]);
}

void Network::maxout(int index)
{
	//	max of activations[i-1].weights[i] + biases[i]
	double biggest = weightedInputs[index](1, 1);
	int  i;
	int biggestIndex(0);
	for (i = 0; i < layerSizes[index]; i++)
		if (weightedInputs[index](i) > biggest)
		{
			biggest = weightedInputs[index](i);
			biggestIndex = i;
		}
	for (i = 0; i < layerSizes[index]; i++)
	{
		activations[index](i) = biggest;
		activationPrime[index](i) = ((i == biggestIndex) ? (1) : (0));
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
		activationPrime[index](i) = (activations[index](i) != 0) ? (1) : (-1 * alpha);
	}
}

void Network::cosine(int index)
{
	activations[index] = cos(weightedInputs[index]);
	activationPrime[index] = (zeros_matrix<double>(layerSizes[index], 1) - sin(activations[index]));
}*/

//Belongs in Documentation.md
/*-----------------------------------------------------	~ AN EXAMPLE ~ ----------------------------------------------------------
unsigned char Network::stateTable[numActivations + 1][(if numLayers =) 5] = {

								//Layers:	0		1		2		3		4		
	/* inputLinear				 	{		0,		0,		0,		0,		0,		},
	/* inputSigmoid				 	{		1,		1,		1,		1,		1,		},
	/* inputComplementaryLog_Log 	{		2,		2,		2, 		2, 		2, 		},
	/* inputBipolarSigmoid		 	{		3,		3,		3, 		3, 		3, 		},
	/* inputTanh				 	{		4,		4,		4, 		4, 		4, 		},
	/* inputLeCun_stanh			 	{		5,		5,		5, 		5, 		5, 		},	
	/* inputRectifier			 	{		6,		6,		6, 		6, 		6, 		},
	/* inputSmoothRectifier		 	{		7,		7,		7, 		7, 		7, 		},
	/* inputLogit				 	{		8,		8, 		8, 		8, 		8, 		},
	/* inputSoftmax				 	{		9,		9,		9,		9,		9,		},
	/* inputRadialGaussian		 	{		10,		10, 	10,		10,		10,		},
	/* inputMaxout				 	{		11,		11,		11,		11,		11,		},
	/* inputLeakyRelu			 	{		12,		12,		12,		12,		12,		},
	/* inputCosine				 	{		13,		13,		13,		13,		13,		}					
};*/

/*Matrix Network::takeInput(int index)
{
	int j;
	Matrix prime;
	std::vector<string> strings;
	strings.resize(numLayers);
	cout << "Enter the number of the activation function to be used for layer " << index << " -> ";
	cin >> j;
	cin.ignore();
	Switch(stateTable(j, index), index);
	return prime;
}*/

/*----------------------------------------------------------------------------------------------------------
TBD:
- Also, trying to make a setActivation function by which the user can set an activation function for the complete network
  if the user wants
-----------------------------------------------------------------------------------------------------------*/

///
//Network Class Functions
///

/**********Constructors and Destructors*******************/

//Default constructor for the class
Network::Network()
{
	//Call readInit() to fill numLayers, layerSizes[], learningRate, epochs, batchSize
	readInit();
	//initStateTable();

	//Ask for training data filename, then open it
	std::cout << "Please enter the location of your training file [C:\\...\\TrainingDataFilename.txt:\n";
	cin.ignore();
	getline(cin, trainingDataFilename);
	trainingDataInfile.open(trainingDataFilename);
	while (trainingDataInfile.fail())
	{
		trainingDataInfile.clear();
		trainingDataInfile.close();
		std::cout << "Could not open " << trainingDataFilename << ".\n" << "Please try again:\n";
		getline(cin, trainingDataFilename);
		trainingDataInfile.open(trainingDataFilename);
	}

	checkBatchSize();
	//Ask for expected values filename and open it
	std::cout << "Please enter the location of your truth data file [C:\\...\\ExpectedValuesFilename.txt:\n";
	getline(cin, expectedValuesFilename);
	expectedValuesInfile.open(expectedValuesFilename);
	while (expectedValuesInfile.fail())
	{
		expectedValuesInfile.clear();
		expectedValuesInfile.close();
		std::cout << "Could not open " << expectedValuesFilename << ".\n" << "Please try again:\n";
		getline(cin, expectedValuesFilename);
		expectedValuesInfile.open(expectedValuesFilename);
	}

	//resize the VMatrix's to match input
	weights.resize(numLayers);
	biases.resize(numLayers);
	activations.resize(numLayers);
	//activationPrime.resize(numLayers);
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

		/*activationPrime[i].set_size(layerSizes[i], 1);
		activationPrime[i] = zeros_matrix(activationPrime[i]);*/

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

//This constructor will be called when the user wants to train an existing network. It will retrieve all the required information the given file
//and will then, prompt the user if user wants to change the values of hyperparameters. If so, it updates the values of hyperparameters
Network::Network(const string& previous_network_filename)
{
	readInit(previous_network_filename);
	//initStateTable();
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
	checkBatchSize();
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
		checkBatchSize();
		cout << "\nEnter a value for epochs (x > 1): ";
		cin >> epochs;
		checkEpochs();

		cout << "\nYou have successfully updated the hyparameters with following values:";
		cout << "\nLearning Rate: " << learningRate;
		cout << "\nBatch Size: " << batchSize;
		cout << "\nNumber of epochs: " << epochs;
	}
	else;
}

//Classify constructor. data initializing handled by readInit
Network::Network(const string& networkFilename, const string& validationDataFilename)
{
	readInit(networkFilename);
	classify(validationDataFilename);
}

//Destructor will be called at the end of the main(). It will be responsible to close all the files and deallocate the dynamic memory that was being used.
//Consider, having a close() to close the files, if there's going to be a loop in main()
Network::~Network()
{
	if (writeToFile())
		std::cout << "Write successful\n";

	trainingDataInfile.close();
	expectedValuesInfile.close();
	//destructor of dlib and vector class called
}

/**********Public Methods*******************************/

//user can set the highest value to the learning rate by passing it as a parameter to the checkLearningRate()
//In case if user doesn't set any highest learning rate, the lr_highest is by default set to 1
//It asks user if the user wants to continue with the entered learning rate if it finds anything wrong
//if,
		//the user agrees to the learning rate entered, the loop breaks and the program continues
//else,
		//user is asked to enter a value within the range

void Network::checkLearningRate(int lr_highest)
{
	bool result;
	string temp;
	while (!(result = ((learningRate > 0) && (learningRate < lr_highest))))	//loop continues until the learning rate is between 0 and given end point
	{
		//If learning rate is not in range, displays an error message
		cout << "\nError: The learning rate you entered is too high.." << endl;

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
	int end_of_file = fileSize(trainingDataInfile);
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

//when passed a text file, will classify data therein and output to console as well as a file
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
	int numClassifications = fileSize(validationDataInfile);			//counts number of data in validation data file
	Matrix currentSample(layerSizes[numLayers - 1], 1);

	//CLASSIFY
	//Loop through samples
	while (!validationDataInfile.eof())
	{
		currentSample = getM<double>(validationDataInfile, i);
		forwardProp(validationDataInfile, i);
		report = outputLayerReport();
		isAmbiguous = report.isAmbiguous;
		cleanedOutput = report.cleanOutput;

		if (isAmbiguous)
			numAmbiguousData++;

		//print out the classification into a file
		classificationOutput << "Network Input:  " << dlib::trans(getM<double>(validationDataInfile, i));
		classificationOutput << "Network Output: " << dlib::trans(activations[numLayers - 1]);
		classificationOutput << "Classification: " << dlib::trans(cleanedOutput) << '\n';

		//print out classification into cmd prompt
		std::cout << "Network Input:  " << dlib::trans(getM<double>(validationDataInfile, i++));	//NOTICE, the i++ is in this line!
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

void Network::readInit() // reading from console
{
	std::cout << "Welcome! Please follow the prompts to initialize and begin training your network." << endl;
	std::cout << "Enter a string of integers that correspond to the layers and desired nodes in each layer of your network:" << endl;
	string layers;  getline(cin, layers);
	checkLayersString(layers);

	char* cStrLayers = new char[layers.size() + 1];
	strcpy(cStrLayers, layers.c_str());

	for (char *p = strtok(cStrLayers, " ,"); p != NULL; p = strtok(NULL, " ,"))
	{
		layerSizes.push_back(atoi(p));
	}
	numLayers = layerSizes.size();

	std::cout << "\nPlease enter a double for the learning rate (usually in the range [x-y]):" << endl;
	cin >> learningRate;
	checkLearningRate();

	std::cout << "\nPlease enter an integer for the number of epochs (number of times to parse through test data):" << endl;
	cin >> epochs;
	checkEpochs();

	std::cout << "\nPlease enter an integer for the mini batch size:" << endl;
	cin >> batchSize;

	std::cout << "\nThank you! You have created a network with these values:" << endl;

	std::cout << "Input layer with " << layerSizes[0] << " nodes." << endl <<
		"Output layer with " << layerSizes[numLayers - 1] << " nodes." << endl <<
		(numLayers - 2) << " hidden layers with ";

	for (int i = 1; i < numLayers - 1; i++)
		std::cout << layerSizes[i] << " ";

	std::cout << "nodes respectively." << endl <<
		"Learning rate := " << learningRate << endl <<
		"Epochs := " << epochs << endl <<
		"Mini batch size := " << batchSize << endl;
	delete[] cStrLayers;
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
		//activationPrime.resize(numLayers);
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

			/*activationPrime[i].set_size(layerSizes[i], 1);
			activationPrime[i] = zeros_matrix(activationPrime[i]);*/

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
		std::cout << "Server error 402: Could not open the file requested! Try again later..";
		return false;
	}
}

// train calls SGD on every mini batch in a training data set until the set has been exhausted as many times as epochs
// randomizing the set between each epoch
// no parameters
// returns a vector of doubles containing the percentage of outputs the network successfully classified for each iteration of an epoch
std::vector<double> Network::train()
{
	std::vector<double> efficiency(epochs);
	int trainingDataSize = 1000;//fileSize(trainingDataInfile);
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
		std::cout << "\nEfficiency at epoch: " << i << " = " << efficiency[i] << " %" << endl;
	}

	if (writeToFile())
		std::cout << "Write successful\n";

	return efficiency;
}

/**********Private Methods******************************/

//moves backwards through Network calculating error at each level and using that to increment sumNablaB and sumNablaW
//requires the assumption that expected values has an int
bool Network::backProp(int index)
{
	int lastInd = numLayers - 1;
	Matrix expectedValues = getM<double>(expectedValuesInfile, index);

	bool isCorrect = compareOutput(expectedValues) > 0;

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

	return isCorrect; //correct
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

	std::vector<int>::const_iterator i1;
	int j(0);

	string fileName = "previousnetwork";								//what filename starts with
	const int MAX_SIZE(80);												//buffer (maximum) size for the intermediary cstring
	time_t currentTime = time(NULL);									//returns current time
	struct tm * currentTimeInfo = localtime(&currentTime);				//stores current time into a struct
	char outputFileTime[MAX_SIZE];										//creates cstring intermediary
	strftime(outputFileTime, MAX_SIZE, "%a%H%M", currentTimeInfo);		//converts cstring intermediary into a string containing the current time
	string filetype = ".txt";											//small filetype appendage
	fileName += outputFileTime + filetype;								//appends Day(of week)HourMinute
	ofstream outfile(fileName, ios::trunc);								//creates and opens output file

	if (outfile.is_open())
	{
		std::cout << "\nCreating file " << fileName << '\n'; //because endl calls flush(), which forces the buffer to write to the file without being filled
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
		std::cout << "Server error 401: Could not open the file requested! Try again later..";
		return false;
	}
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
	double biggest = -DBL_MAX;	bool isAmbiguous = false;

	// out of place random check to make sure it's being fed good data
	if (expectedValues.size() != lastSize)
	{
		std::cout << "Error in compareOutput: the number of neurons in the output layer != the number of elements in given validation datum\n";
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
	in.clear();		//clears eof flag

	return count;
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
		forwardProp(trainingDataInfile, miniBatchIndices[i]);
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

//Extracts the training data sample at batchIndex and runs forward propagation as commonly(?) defined for feedforward
//neural networks. Requires that a training data file be in place, and an infile object is instantiated for it.
//Added another parameter bc it made my life easier while writing classify()
void Network::forwardProp(ifstream &infile, const int batchIndex)
{
	//extract data point from training data file at input indice into first layer of activations
	activations[0] = getM<double>(infile, batchIndex);
	
	//run forward propagation
	for (int i = 1; i < numLayers; i++)
	{
		weightedInputs[i] = ((weights[i] * activations[i - 1]) + biases[i]);
		activations[i] = activationFunction(weightedInputs[i]); //takeInput(i);
	}
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

