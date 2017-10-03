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
	// replace all ambiguous variable names with the real ones from the class
	
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

	cout << "\nPlease enter a double for the learning rate (usually in the range [x-y]):" << endl;
	double lr; cin >> lr;

	cout << "\nPlease enter an integer for the number of epochs (number of times to parse through test data):" << endl;
	int ep; cin >> ep;

	cout << "\nPlease enter an integer for the mini batch size:" << endl;
	int mbs; cin >> mbs;

	cout << "\nThank you! You have created a network with these values:" << endl;

	// continue here

	/*for (int i=0; i < layerSizes.size(); i++)
		cout << layerSizes[i] << endl;*/
}

//hadamardProduct utilizes dlib's pointwise_multiply() to compute the element-by-element product.
//pointwise_multiply() will assert that the input matrices must be of equal size, so the program will stop
//when that occurs. 
//Removed & (address operator) from the return type, as it was giving out compiler errors.
const Matrix hadamardProduct(const Matrix &input_matrix_L, const Matrix &input_matrix_R)
{
	return pointwise_multiply(input_matrix_L, input_matrix_R);
}
