
#include <iostream>

#include <math.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

#define MAX_DIG 5000

using namespace std;

int dig_cnt(unsigned int n) {
  return log10(n) + 1;
}

class FNumber {

public:
  FNumber(const unsigned int digs[MAX_DIG], int digit_count)
	: digit_count(digit_count) {
	memcpy(digits, digs, MAX_DIG*sizeof(unsigned int));
  }
  
  unsigned int digits[100000];
  int digit_count;

  void to_s(char* buf);

  FNumber& operator+=(const FNumber other);

  unsigned int last();
  unsigned int first();
  
};

unsigned int FNumber::last() {
  return this->digits[0];
}

unsigned int FNumber::first() {
  int fc = dig_cnt(this->digits[this->digit_count - 1]);

  int need = 9 - fc;

  if (this->digit_count <= 1 || !need) {
	return this->digits[this->digit_count - 1];
  }

  int fsts = this->digits[this->digit_count - 1] * pow(10, need);

  fsts += this->digits[this->digit_count - 2] / pow(10, fc);
  
  return fsts;
  
}

void FNumber::to_s(char* buf) {
  sprintf(buf, "%u...%u", this->first(), this->last());
}

FNumber& FNumber::operator+=(const FNumber other) {
  int rem = 0;
  int i;
  
  for (i = 0; i < digit_count || rem != 0; i++) {
	  
	this->digits[i] += other.digits[i] + rem;

	rem = 0;
	  
	int ln = dig_cnt(this->digits[i]);

	if (ln > 9) {
	  rem = this->digits[i] / 1000000000;
	  this->digits[i] %= 1000000000;
	}

	
  }
  
  this->digit_count = i;
	
  return (*this);
}


bool find_digit(int d, char buf[10]) {
  char need = d + '0';
  for (int i = 0; i < 9; i++) {
	if (buf[i] == need) {
	  return true;
	}
  }
  return false;
}
bool is_pan(unsigned int a) {
  if (dig_cnt(a) != 9) {
	//	cout << "size" << endl;
	return false;
  }

  char buf[10];

  sprintf(buf, "%u", a);

  for (int c = 1; c <= 9; c++) {
	if (!find_digit(c, buf)) {
	  return false;
	}
  }

  return true;
}

int main() {
  unsigned int one[MAX_DIG] = {1};
  FNumber a(one, 1);
  FNumber b(one, 1);
  FNumber tmp = b;
  char buf[25];
  bool pf, pl;

  for (int i = 2; i < 1000000; i++) {
    // b.to_s(buf);
	
	//	cout << i << " " << buf << endl;

	pf = is_pan(b.first());
	
	if (pf) {
	  cout << "Pandigitals first! " << i << endl;
	}

	pl = is_pan(b.last());
	
	if (pl) {
	  cout << "Pandigitals last! " << i << endl;
	}

	if (pf && pl) {
	  cout << "Found! " << i << endl;
	  return 0;
	}
	
	tmp = b;
	b += a;
	a = tmp;
  }
  

}
