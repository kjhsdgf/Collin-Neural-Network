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
	for (i = 0; i < 20; i++)			//gets the name "Previous_Network_[Day]"
		fileName[j++] = a[i];
	
	for (i = 28; i < 35; i++)			//appends the [time (hhmin)] to name of the file
		if(a[i] != ':')
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
