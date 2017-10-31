#include "Network.h"

int main()
{
	/*string previousNetworkName = "C:\\Users\\Elijah\\source\\repos\\Collin-Neural-Network\\Network\\Network\\previousnetworkThu1305.txt";
	Network brain(previousNetworkName);
	string validationFileName = "C:\\Users\\Elijah\\source\\repos\\Collin-Neural-Network\\Data\\validationData.txt";
	
	//brain.train();
	brain.classify(validationFileName);*/

	Network N;
	std::vector<double> v = N.train();
	ofstream outfile;
	outfile.open("Efficiency.txt", ios_base::out);
	std::vector<double> ::const_iterator i1;
	i1 = v.begin();
	for (; i1 != v.end(); i1++)
		outfile << (*i1) << '\n';
	outfile.close();


	return 0;
}