
//PLEASE FOLLOW THIS: Tabs, 4, No wrap

//Fisher Yates shuffle
template <class T>
void FYShuffle(vector<T>& v)
{
	//below is another method using shuffle defined in <algorithm> but it is potentially much slower and not much more random
	//unsigned seed = chrono::system_clock::now().time_since_epoch().count();
	//shuffle(v.begin(), v.end(), default_random_engine(seed));

	for (int i = v.size()-1; i > 0; i--)
	{
		int randI = rand() % i;
		T temp = v[i];
		v[i] = v[randI];
		v[randI] = temp;
	}
}

//Activation function: can be changed later by the programmer, however as we discussed reading a function from the file can be a little risky
//So, I need to think about it before implementing it
const Matrix& activationFunction(const Matrix& weighted_inputs)
{
	return sigmoid(weighted_inputs);
}
