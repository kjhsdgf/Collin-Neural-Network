#include "Network.h"

int main()
{
	string previousNetworkName = "C:\\Users\\Elijah\\source\\repos\\Collin-Neural-Network\\Network\\Network\\previousnetworkThu1305.txt";
	Network brain(previousNetworkName);
	string validationFileName = "C:\\Users\\Elijah\\source\\repos\\Collin-Neural-Network\\Data\\validationData.txt";
	
	//brain.train();
	brain.classify(validationFileName);

	return 0;
}