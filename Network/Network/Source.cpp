#include <iostream>
#include "Network.h"

int main()
{
	Network brain;
	//string prevNetwork = "C:\\Users\\Elijah\\source\\repos\\Collin-Neural-Network\\Network\\Network\\Previous_Network_Fri0301.txt";
	//string valFile = "C:\\Users\\Elijah\\source\\repos\\Collin-Neural-Network\\Data\\D100.txt";
	//Network brain(prevNetwork);
	brain.train();
	
	//brain.classify(valFile);

	return 0;
}