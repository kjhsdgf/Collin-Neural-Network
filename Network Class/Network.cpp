#include "Network.h"

bool Network::writeToFile() 	
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
	std::vector<int>::iterator i1;
	int i;
	int j (0);
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
		if(a[i] != ':')
			fileName[j++] = a[i];
	
	fileName += ".txt";				//appends ".txt" to the name of the file
	outfile.open(fileName, ios_base::out); //creates the file with name "Previous_Network_[Day][Time(hhmin)].txt"
	if (outfile.is_open())
	{
		cout << "\nCreating file " << fileName << endl;
		outfile << trainingDataFilename << endl;
		outfile << expectedValuesFilename << endl;
		outfile << learningRate << endl;
		outfile << batchSize << endl;				
		outfile << epochs << endl;
		outfile << numLayers << endl;
		i1 = layerSizes.begin();
		for (; i1 != layerSizes.end(); i1++)
			outfile << (*i1) << " ";
		outfile << endl;
		j = 1;
		while ((j) < numLayers)
		{
			outfile << "w " << j << endl;
			outfile << weights[j];			//there is overloaded operator for '<<' to display the matrices inside dlib
			outfile << "b " << j << endl;
			outfile << biases[j];
			j++;
		}
		outfile.close();
		return true;
	}
	else
	{
		cout<<"Server error 401: Could not open the file requested! Try again later..";
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

	vector<int> layerSizes;
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
	cin >> miniBatchSize;

	cout << "\nThank you! You have created a network with these values:" << endl;
	
	cout << "Input layer with " << layerSizes[0] << " nodes." << endl <<
		 	"Output layer with " << layerSizes[numLayers-1] << " nodes." << endl <<
			(numLayers - 2) << " hidden layers with "; 
	
	for (int i=1; i < numLayers - 1; i++)
		cout << layerSizes[i] << " ";
	
	cout 										 << "nodes respectively." << endl <<
			"Learning rate := " << learingRate << endl <<
			"Epochs := " << epochs << endl <<
			"Mini batch size := " << miniBatchSize << endl;
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
		biases[i] -=  k * sumNablaB[i];
	}

}

//Extracts the training data sample at batchIndex and runs forward propagation as commonly(?) defined for feedforward
//neural networks. Requires that a training data file be in place, and an infile object is instantiated for it.
//Added another parameter bc it made my life easier while writing classify()
void Network::forwardProp(const int batchIndex, const string &filename)
{
	//extract data point from training data file at input indice into first layer of activations
	activations[0] = getAt<double>(filename, batchIndex); //<--untested!
	//run forward propagation
	for (int i = 1; i < numLayers; i++)
	{
		weightedInputs[i] = ((weights[i] * activations[i - 1]) + biases[i]);
		activations[i] = activationFunction(weightedInputs[i]);
	}
}

//when passed a text file, will classify data therein and output to console as well as a file
void classify(const string &validation_data_filename)
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
	string outputFilename = "Classification_";							//what filename starts with
	const int MAX_SIZE(80);												//buffer (maximum) size for the intermediary cstring
	time_t currentTime = time(NULL);									//returns current time
	struct tm * currentTimeInfo = localtime(&currentTime);				//stores current time into a struct
	char outputFileTime[MAX_SIZE];										//creates cstring intermediary
	strftime(outputFileTime, MAX_SIZE, "%a-%b-%d-%T", currentTimeInfo);	//cinverts cstring intermediary into a string containing the current time
	outputFilename += outputFileTime;									//appends Day(of week)-Month-Day(of month)-Current Hour:Minute:Second to outputFilename
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
		forwardProp(i, validation_data_filename);				//Pass a sample to forwardProp
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
		classificationOutput << "Network Input:  " << dlib::trans(getAt<Matrix>(validationDataInfile, i)[0]);		//<-- possible problems here
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
		cout << "Network Input:  " << dlib::trans(getAt<Matrix>(validationDataInfile, i)[0]);		//<-- possible problems here
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

//in the case they want to classify whatever's already in the training file data member
void classify()
{
//SETUP
	//move class training data infile to the start
	trainingDataInfile.clear();
	trainingDataInfile.seekg(0, ios::beg);

	//create outfile for output and open it
	string outputFilename = "Classification_";							//what filename starts with
	const int MAX_SIZE(80);												//buffer (maximum) size for the intermediary cstring
	time_t currentTime = time(NULL);									//returns current time
	struct tm * currentTimeInfo = localtime(&currentTime);				//stores current time into a struct
	char outputFileTime[MAX_SIZE];										//creates cstring intermediary
	strftime(outputFileTime, MAX_SIZE, "%a-%b-%d-%T", currentTimeInfo);	//cinverts cstring intermediary into a string containing the current time
	outputFilename += outputFileTime;									//appends Day(of week)-Month-Day(of month)-Current Hour:Minute:Second to outputFilename
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
	int numClassifications = (static_cast<int>(trainingDataInfile.tellg()) + MAGIC_NUMBER_1) / MAGIC_NUMBER_2;
	trainingDataInfile.clear();				//clears eof bit
	trainingDataInfile.seekg(0, ios::beg);	//resets the seekg head

//CLASSIFY
	//Loop through samples
	for (int i = 0; i < numClassifications; i++)
	{
		forwardProp(i, validation_data_filename);				//Pass a sample to forwardProp
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
		classificationOutput << "Network Input:  " << dlib::trans(getAt<Matrix>(trainingDataInfile, i)[0]);		//<-- possible problems here
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
		cout << "Network Input:  " << dlib::trans(getAt<Matrix>(trainingDataInfile, i)[0]);		//<-- possible problems here
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
	//close the files (not closing trainingDataInfile, because destructor should take care of that).
	classificationOutput.close();
}
