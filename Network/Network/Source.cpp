#include<iostream>
#include "Network.h"

using namespace std;

void main()
{
	Network N ("Previous_Network_Thu1343.txt");
	//Network N;
	//N.setActivationFunc(3);
	std::vector<double> v = N.train();

	N.makeGraphFile(1000);
	/*ofstream outfile;
	outfile.open("Efficiency.txt", ios_base::out);
	std::vector<double> ::const_iterator i1;
	i1 = v.begin();
	for (; i1 != v.end(); i1++)
		outfile << (*i1) << '\n';
	outfile.close();*/
}