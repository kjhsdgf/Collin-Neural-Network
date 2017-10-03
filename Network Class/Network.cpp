#include "Network.h"

bool Network::writeToFile() 	
{
	ofstream outfile;
	std::vector<int>::iterator i1;
	int i;
	int j;
	int k;
	string fileName("Previous_Network_");
	time_t _tm = time(NULL);
	struct tm * curtime = localtime(&_tm);
	fileName += asctime(curtime);
	outfile.open(fileName, ios_base::out);
	if (outfile.is_open())
	{
		cout << "\nCreating file " << fileName << ".txt" << endl;
		outfile << trainingDataFilename << endl;
		outfile << expectedValuesFilename << endl;
		outfile << learningRate << endl;
		outfile << batchSize << endl;				
		outfile << epochs << endl;
		outfile << numLayers << endl;
		i1 = layerSizes.begin();
		for (; i1 != layerSizes.end(); i1++)
		{
			outfile << (*i1) << " ";
		}
		outfile << endl;
		j = 0;
		while ((j++) < numLayers)
		{
			outfile << "w " << j << endl;
			for (i = 0; i < weights[j].nr(); i++)
			{
				for (k = 0; k < weights[j].nc(); k++)
					outfile << weights[j](i, k) << " ";
				outfile << endl;
			}
			outfile << "b " << j << endl;
			for (i = 0; i < biases[j].nr(); i++)
			{
				outfile << biases[j](i, 1);
				outfile << endl;
			}
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

void Network::readIn() // reading from console
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


