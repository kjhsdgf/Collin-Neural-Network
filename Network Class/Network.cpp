#include "Network.h"

bool Network::createNetworkFile() 	
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
		j = 1;
		while (j < numLayers)
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
		return false;
}


