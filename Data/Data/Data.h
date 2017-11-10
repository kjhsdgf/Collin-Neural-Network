#ifndef DATA_H
#define DATA_H

#include <time.h>
#include <iostream>
#include <fstream>
#include <vector>
#include<string.h>

using namespace std;

const long int DataSet(50000);

template <class DATA>
class Data
{
	private:
		DATA			data[DataSet][4];
		long int		Horizontal;
		long int		Vertical;
		long int		Diagonal;
		int				truth[DataSet][3];

		void			Initialize();
						Data(const Data <DATA> &);
	
	public:
		
		enum Exceptions {FileNotCreated, InvalidIndex};
						Data();
						Data(const DATA[]);
						~Data();

	//Functions to Generate the matrices
		const DATA&		GenerateValue();
		void			GenerateArrays();

	//Functions to write into the File
		void			CreateUnsortedFile(int);
		void			CreateDataFile();
		void			CreateTruthFile();

	//Functions to count the things and generate the truth values
		void			Count(DATA [][4]);					//generates truth values
		void			Display() const;

	//Functions to get the Truth Value
		//const DATA*		GetTruth(const int &) const;	//Displays the truth value of any 4-pixel image

	//Operators
		const DATA*		operator []	(const int &) const;

		vector<DATA>	cleanOutput(int);
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
	value = static_cast<DATA> (rand());
	/*value = static_cast<float>(value + 1) / RAND_MAX;*/		value = value % 2;
	return value;
}

template <class DATA>
void Data <DATA> ::GenerateArrays()
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
void Data <DATA> ::Count(DATA d[DataSet][4])
{
	int k;
	//vector<DATA> v;
	//v.resize(4);
	for (int i = 0; i < DataSet; i++)
	{
		//v = cleanOutput(i);
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

/*template <class DATA>
vector<DATA> Data <DATA> ::cleanOutput(int index)
{
	vector<DATA> v;
	int i;
	v.resize(4);
	DATA Biggestvalue = data[index][0];
	int currBiggestIndex1 (0);
	int currBiggestIndex2 (0);
	for (i = 1; i < 4; i++)
	{
		if (data[index][i] >= Biggestvalue)
		{
			Biggestvalue = data[index][i];
			currBiggestIndex1 = i;
		}
		else;
	}
	v[currBiggestIndex1] = 1;
	Biggestvalue = 0;
	for (i = 0; i < 4; i++)
	{
		if (i != currBiggestIndex1)
		{
			v[i] = 0;
			if (data[index][i] > Biggestvalue)
			{
				Biggestvalue = data[index][i];
				currBiggestIndex2 = i;
			}
			else;
		}
		else;
	}
	v[currBiggestIndex2] = 1;
	return v;
}*/

template <class DATA>
void Data <DATA> ::Display() const
{
	cout << "\nHorizontals:" << Horizontal;
	cout << "\nVerticals:" << Vertical;
	cout << "\nDiagonals:" << Diagonal << endl;
}

template <class DATA>
void Data <DATA> ::CreateUnsortedFile(int k)
{
	fstream _file;
	int i, j;
	if (k == 4)
	{
		_file.open("D50000.txt", ios_base::out | ios_base::trunc);
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
		_file.open("T50000.txt", ios_base::out | ios_base::trunc);
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

/*template <class DATA>
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
}*/

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