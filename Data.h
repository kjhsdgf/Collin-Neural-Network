#ifndef DATA_H
#define DATA_H

#include <time.h>
#include <iostream>
#include <fstream>
#include<string.h>

using namespace std;

const long int DataSet(10000);

template <class DATA>
class Data
{
	private:
		DATA data[DataSet][4];
		long int Horizontal;
		long int Vertical;
		long int Diagonal;
		int truth[DataSet][3];

		void Initialize();
		Data(const Data <DATA> &);
	
	public:
		
		enum Exceptions {FileNotCreated, InvalidIndex};
		Data();
		Data(const DATA[]);
		~Data();

	//Functions to Generate the matrices
		const DATA  & GenerateValue();
		const void GenerateArrays();

	//Functions to write into the File
		void CreateUnsortedFile(int);
		void CreateDataFile();
		void CreateTruthFile();

	//Functions to count the things and generate the truth values
		void Count(const DATA [DataSet][4]);					//generates truth values
		void Display() const;

	//Functions to get the Truth Value
		const DATA * GetTruth(const int &) const;	//Displays the truth value of any 4-pixel image

	//Operators
		const DATA * operator []	(const int &) const;
};

template <class DATA>
inline Data <DATA> :: ~Data()
{
}

template <class DATA>
Data <DATA> ::Data (const DATA d[])
{
	for (int i = 0; i < DataSet; i++)
		for (int j = 0; j < 4; j++)
			data[i][j] = d[j];		
}

template <class DATA>
Data <DATA>::Data()
{
	Horizontal = 0;
	Vertical = 0;
	Diagonal = 0;
	GenerateArrays();
	Count(data);
}

template <class DATA>
void Data <DATA>::Initialize()
{
	srand(time(0));
}

template <class DATA>
const DATA & Data <DATA> ::GenerateValue()
{
	DATA  value;
	value = rand();
	value = value % 2;
	return value;
}

template <class DATA>
const void Data <DATA> ::GenerateArrays()
{
	Initialize();
	DATA sum  (0);
	int j;
	for (int i = 0; i < DataSet; i++)
	{
		sum = 0;
		j = 0;
		do
		{
			data[i][j] = GenerateValue();
			sum += data[i][j];  //sum = sum + data[i][j];
			j++;
		} while (j < 4);
		if (sum != 2)
			i--;
	}
}

template <class DATA>
void Data <DATA> ::Count(const DATA d[DataSet][4])
{
	int k;
	for (int i = 0; i < DataSet; i++)
	{
		if (((d[i][0] == 1) && (d[i][1] == 1)) || ((d[i][2] == 1) && (d[i][3] == 1)))
		{
			Horizontal++;
			truth[i][0] = 1;
			truth[i][1] = 0;
			truth[i][2] = 0;
		}
		else if (((d[i][0] == 1) && (d[i][2] == 1)) || ((d[i][1] == 1) && (d[i][3] == 1)))
		{
			Vertical++;
			truth[i][0] = 0;
			truth[i][1] = 1;
			truth[i][2] = 0;
		}
		else
		{
			Diagonal++;
			truth[i][0] = 0;
			truth[i][1] = 0;
			truth[i][2] = 1;
		}
	}
}

template <class DATA>
void Data <DATA> ::Display() const
{
	cout << "\nHorizontals:" << Horizontal;
	cout << "\nVerticals:" << Vertical;
	cout << "\nDiagonals:" << Diagonal;
}

template <class DATA>
void Data <DATA> ::CreateUnsortedFile(int k)
{
	fstream _file;
	int i, j;
	if (k == 4)
	{
		_file.open("UnsortedDataFile", ios_base::out | ios_base::trunc);
		if (_file.is_open())
		{

			for (i = 0; i < DataSet; i++)
			{
				for (j = 0; j < k; j++)
					_file << data[i][j] << ",";
				_file << endl;
			}
			_file.close();
		}
		else
			throw FileNotCreated;
			
	}
	else if (k == 3)
	{
		_file.open("UnsortedTruthFile", ios_base::out | ios_base::trunc);
		if (_file.is_open())
		{

			for (i = 0; i < DataSet; i++)
			{
				for (j = 0; j < k; j++)
					_file << truth[i][j] << ",";
				_file << endl;
			}
			_file.close();
		}
		else
			throw FileNotCreated;
	}
	else
		cout << "\nError 404: Unable to find the data requested.";
}

template <class DATA>
inline void Data <DATA> ::CreateDataFile()
{
	CreateUnsortedFile(4);
}

template <class DATA>
inline void Data <DATA> ::CreateTruthFile()
{
	CreateUnsortedFile(3);
}

template <class DATA>
const DATA * Data <DATA> :: operator[] (const int & i) const
{
	DATA * ptemp;
	if ((i >= 0) && (i < DataSet))
	{
		ptemp = new DATA[4];
		for (int j = 0; j < 4; j++)
		{
			ptemp[j] = data[i][j];
		}
		return ptemp;
	}
	else
	{
		throw InvalidIndex;
		return NULL;
	}
}

template <class DATA>
const DATA * Data <DATA> :: GetTruth (const int & i) const
{
	DATA * ptemp;
	if ((i >= 0) && (i < DataSet))
	{
		ptemp = new DATA[3];
		for (int j = 0; j < 3; j++)
		{
			ptemp[j] = truth[i][j];
		}
		return ptemp;
	}
	else
	{
		throw InvalidIndex;
		return NULL;
	}
}

ostream & operator << (ostream & out, const int * p)
{
	for (int i = 0; i < 4; i++)
	{
		if ((p[i] == 0) || (p[i] == 1))
			out << p[i] << " ";
		else
			break;
	}
	return out;
}

ostream & operator << (ostream & out, const float * p)
{
	for (int i = 0; i < 4; i++)
	{
		out << p[i] << " ";
	}
	delete[] p;
	return out;
}
#endif