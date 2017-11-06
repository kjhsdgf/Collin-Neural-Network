#include<iostream>
#include "Network.h"

using namespace std;

void main()
{
	Network N ("Previous_Network_Fri1005.txt");
	//Network N;
	std::vector<double> v = N.train();
	ofstream outfile;
	outfile.open("Efficiency.txt", ios_base::out);
	std::vector<double> ::const_iterator i1;
	i1 = v.begin();
	for (; i1 != v.end(); i1++)
		outfile << (*i1) << '\n';
	outfile.close();
}