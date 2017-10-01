//PLEASE FOLLOW THIS: Tabs, 4, No wrap

template <class T>
void Network::FYShuffle(vector<T>& v)
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
