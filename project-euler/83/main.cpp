
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <utility>

using namespace std;

int read_matrix(map<int, map<int, int> >& matrix) {
	int size = -1;

   	for (int i = 0; size == -1 || size * size > i; i++) {
		int n;
		char d;
		
		cin >> n;
		d = cin.get();

		matrix[size == -1 ? 0 : i / size][size == -1 ? i : i % size] = n;

		if (d == '\n') {
			if (size == -1) {
				size = i + 1;
			}
		}
	}
	return size;
}

void print_matrix(map<int, map<int, int> > matrix, int size) {
	for (int i = 0; i < size; i++) {	
	    for (int j = 0; j < size; j++) {
			cout << matrix[i][j] << '\t';
		}
		cout << endl;
	}
}

void solve_iterate(map<int, map<int, int> > matrix,
				   int size,
				   map<int, map<int, int> >& sums,
				   set<pair<int, int> >& changeds) {
	
	set<pair<int, int> > newchangeds;
	
	for (set<pair<int, int> >::iterator cit = changeds.begin();
		 cit != changeds.end();
		 cit++) {

		int x = cit->first;
		int y = cit->second;

		//		cout << "Changed: " << x << ' ' << y << endl;

		/* . 3 .
		   4 @ 2
		   . 1 .
		*/
		
		if (x < size - 1) {
			//	cout << "try " << x + 1 << ' ' << y;
			int newsum = sums[x][y] + matrix[x + 1][y];
			
			if (sums[x + 1][y] == 0 || sums[x + 1][y] > newsum) {
				sums[x + 1][y] = newsum;
				//	cout << " ok! " << newsum << endl;
				newchangeds.insert(pair<int, int>(x + 1, y));
			}
		}
		if (x > 0) {
			//			cout << "\ntry " << x - 1 << ' ' << y;
			
			int newsum = sums[x][y] + matrix[x - 1][y];
			
			if (sums[x - 1][y] == 0 || sums[x - 1][y] > newsum) {
				sums[x - 1][y] = newsum;
				//	cout << " ok! " << newsum << endl;
				newchangeds.insert(pair<int, int>(x - 1, y));
			}
		}
		if (y < size - 1) {
			//			cout << "try " << x << ' ' << y + 1;
			int newsum = sums[x][y] + matrix[x][y + 1];
			
			if (sums[x][y + 1] == 0 || sums[x][y + 1] > newsum) {
				sums[x][y + 1] = newsum;
				//	cout << " ok! " << newsum << endl;
				newchangeds.insert(pair<int, int>(x, y + 1));
			}
		}
		if (y > 0) {
			//			cout << "\ntry " << x << ' ' << y - 1;
			
			int newsum = sums[x][y] + matrix[x][y - 1];
			
			if (sums[x][y - 1] == 0 || sums[x][y - 1] > newsum) {
				sums[x][y - 1] = newsum;
				//	cout << " ok! " << newsum << endl;
				newchangeds.insert(pair<int, int>(x, y - 1));
			}
		}
		
	}

	newchangeds.swap(changeds);
}

int solve(map<int, map<int, int> > matrix, int size) {
	
	map<int, map<int, int> > sums;
	set<pair<int, int> > changed;

	// the simplest solution
	sums[size - 1][size - 1] = matrix[size - 1][size - 1];

	changed.insert(pair<int, int>(size - 1, size - 1));

	for (int i = 0; changed.size() != 0; i++) {
		cout << "\niteration " << i << ' ' << changed.size() << " to try" << endl;
		solve_iterate(matrix, size, sums, changed);
		//		print_matrix(sums, size);
	}

	cout << endl;


	return sums[0][0];
}

int main() {
	map<int, map<int, int> > matrix;

	const int size = read_matrix(matrix);

	print_matrix(matrix, size);

	cout << "solving..." << endl;

	cout << "result: " << solve(matrix, size) << endl;
	
	return 0;
}
