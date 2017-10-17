#include<iostream>
#include "Network.h"

using namespace std;

void main()
{
	Matrix m;
	Network N;
	/*ifstream fin;
	fin.open("DataFile.txt", ios_base::in);
	m.set_size(4, 1);
	m = N.getM<double>(fin, 7);
	cout << m;*/
	

	std::vector<double> v = N.train();
	ofstream outfile;
	outfile.open("Efficiency.txt", ios_base::out);
	std::vector<double> :: const_iterator i1;
	i1 = v.begin();
	for (; i1 != v.end(); i1++)
		outfile << (*i1) << '\n';
	outfile.close();
}