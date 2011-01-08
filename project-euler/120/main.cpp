
#include <iostream>
#include <assert.h>

int max_r(int a) {
	int r2 = a % 2;
	int m = a / 2;

	if (r2) {
		return 2*m*a;
	} else {
		return 2*(m - 1)*a;
	}
}



int main() {
	// simple as cake: see problem.txt for reasoning

	assert(max_r(7) == 42);
	
	assert(max_r(6) == 24);

	assert(max_r(3) == 6);

    int result = 0;
	
	for (int i = 3; i <= 1000; i++) {
		result += max_r(i);
	}

	std::cout << "result: " << result << std::endl;
	
	return 0;
}
