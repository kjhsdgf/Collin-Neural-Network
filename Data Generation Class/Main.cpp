#include<iostream>

#include "Data.h"

using namespace std;

void main()
{
	try
	{
		Data<int> d;
		d.CreateDataFile();
		d.CreateTruthFile();
		d.Display();
		cout << d.GetTruth(0) << endl; //Shows truth value for the first 4-pixel image generated in the file
		cout << d[1];
	}
	catch (Data<int> ::Exceptions e)
	{
		switch (e)
		{
		case Data<int> :: FileNotCreated :
			cout << "\nUnable to open the file. Try again later..";
		case Data<int> :: InvalidIndex:
			cout << "\nInvalid Index! Permission denied.";
		default:
			cout << "\nError 402: Internal server error.";
		}
	}
}
