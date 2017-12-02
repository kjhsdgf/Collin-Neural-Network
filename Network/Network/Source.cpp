#include<iostream>
#include "Network.h"

using namespace std;

void main()
{
	//Network N ("Previous_Network_Thu2353.txt");
	///string str = "C:\\Users\\Elijah\\Documents\\Visual Studio 2015\\External Libraries\\GraphViz-2.38\\My Graphs\\By Activation Function\\Efficiencies\\";
	string stamp;
	std::vector<double> v;
	std::vector<double>::const_iterator i1;
	for (int i = 0; i < 1728; i++)
	{
		Network * nPtr = new Network;
		v = nPtr->train();

		nPtr->makeGraphFile(3, "None", 0.5);
		ofstream * outfile = new ofstream;
		stamp = str + "efficiency" + to_string(int(Network::get_AFC() / 3)) + ".csv";
		outfile->open(stamp, ios_base::out);
		i1 = v.begin();
		for (; i1 != v.end(); i1++)
			*outfile << (*i1) << ',';
		outfile->close();

		v.clear();
		delete nPtr;
		delete outfile;
	}

	cout << "done." << endl;
}