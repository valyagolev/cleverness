
#include <iostream>

#include <string.h>

#include <set>

const int NO_VARIANTS = 1;

using namespace std;

void print_set(set<int>);

class Puzzle {
public:
  void read();
  void write();
  bool solve();
  int solve_iter();
  int solve_try_variants(int i, int j, set<int> vs);
  int unsolved();
  void fill_variants(set<int> &vs, int i, int j);
  int the_sum() {
	return numbers[0][0]*100  + numbers[0][1]*10 + numbers[0][2];
  }


  int numbers[9][9];
};

int Puzzle::solve_try_variants(int i, int j, set<int> vs) {
  for (set<int>::iterator it = vs.begin(); it != vs.end(); it++) {
	Puzzle tryme;
	memcpy(tryme.numbers, this->numbers, sizeof(int)*81);
	tryme.numbers[i][j] = *it;
	cout << "Variant:" << endl;
	//	tryme.write();

	bool solved = false;
	
	try {
	  if (tryme.solve()) {
		//		cout << "Solved fine!!!" << endl;
		//		tryme.write();
		memcpy(this->numbers, tryme.numbers, sizeof(int)*81);
		return 81;
	  }
	} catch (int a) {
	  if (a == NO_VARIANTS) {
		// ok
	  } else {
		throw;
	  }
	}

  }
  return 0;
}

int Puzzle::unsolved() {
  int ct = 0;
  for (int i = 0; i < 9; i++) {
	for (int j = 0; j < 9; j++) {
	  if (this->numbers[i][j] == 0) {
		ct++;
	  }
	}
  }

  return ct;
}

void Puzzle::fill_variants(set<int> &vs, int i, int j) {
  if (this->numbers[i][j] != 0) {
	set<int> ret;
	ret.insert(this->numbers[i][j]);
	vs.swap(ret);
	return;
  }
  
  int all_values[]= {1,2,3,4,5,6,7,8,9};
  int _i, _j;
  set<int> ret(all_values, all_values + 9);

  for (_i = 0; _i < 9; _i++) {
	if (_i != i) {
	  ret.erase(this->numbers[_i][j]);
	}
	if (_i != j) {
	  ret.erase(this->numbers[i][_i]);
	}
  }

  int celli, cellj, offi, offj;

  offi = i % 3;
  celli = i - offi;
  offj = j % 3;
  cellj = j - offj;
  
  for (_i = celli; _i < celli + 3; _i++) {
	for (_j = cellj; _j < cellj + 3; _j++) {
	  if (_i != i || _j != j) {
		ret.erase(this->numbers[_i][_j]);
	  }
	}
  }
  

  vs.swap(ret);
}

int Puzzle::solve_iter() {
  int solved = 0;
  set<int> variants[9][9];
  
  for (int i = 0; i < 9; i++) {
	for (int j = 0; j < 9; j++) {
	  if (this->numbers[i][j] == 0) {
		this->fill_variants(variants[i][j], i, j);

		/*
		
		cout << i << ", " << j << '|'; 
		print_set(variants[i][j]);
		cout << " (" << this->numbers[i][j] << ')' << endl;

		//*/
		
		if (variants[i][j].size() < 1)
		  throw NO_VARIANTS;


		if (variants[i][j].size() == 1) {
		  solved += 1;
		  this->numbers[i][j] = *variants[i][j].begin();
		}
		
	  }
	}
  }

  if (!solved) {
	// let's try something more cool
	for (int i = 0; i < 9; i++) {
	  for (int j = 0; j < 9; j++) {
		if (variants[i][j].size() == 2) {
		  return this->solve_try_variants(i, j, variants[i][j]);
		}
	  }
	}
  }

  return solved;
}

bool Puzzle::solve() {
  for (int i = 0; i < 10; i++) {
	if (!this->unsolved()) {
	  return true;
	}
	cout << "Iteration " << i << ": " << endl;
	int result = this->solve_iter();
	cout << result << endl;
	if (!result) {
	  cout << "No idea!" << endl;
	  return false;
	}
  }
  return (this->unsolved() == 0);
}


void Puzzle::read() {
    char ch;

    for (int i = 0; i < 9; i++) {
        for (int j = 0; j < 9; j++) {
            std::cin >> ch;
            this->numbers[i][j] = ch - '0';
        }
    }
}

void Puzzle::write() {
    int i, j;
	for (i = 0; i < 19; i++) {
        std::cout << '-';
    }

    std::cout << std::endl;

    for (i = 0; i < 9; i++) {
        std::cout << '|';
        for (j = 0; j < 9; j++) {
            std::cout << this->numbers[i][j];
            if (j % 3 == 2) {
                std::cout << '|';
            } else {
                std::cout << ' ';
            }
        }
        if (i % 3 == 2) {
            std::cout << std::endl;
            for (j = 0; j < 19; j++) {
                std::cout << '-';
            }
        }
        std::cout << std::endl;
    }
    std::cout << std::endl;
}


void print_set(set<int> s) {
  for (set<int>::iterator it = s.begin(); it != s.end(); it++) {
	cout << " " << *it;
  }
}


int next_sum() {
    Puzzle p;

    p.read();	
    p.write();
	p.solve();
    p.write();
	std::cout << "The sum is " << p.the_sum() << std::endl;

    return p.the_sum();
}

int main() {
  char title[20];
  int result = 0;

  for (int i = 0; i < 50; i++) {
	std::cin >> title >> title;
	std::cout << "!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << endl;
	std::cout << title << std::endl;

	int v = next_sum();
	std::cout << v << std::endl;
	result += v;
  }

  std::cout << "Result: " << result << endl;
  
  return 0;
}
