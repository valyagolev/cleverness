
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MAX_N  100000

typedef struct {
  int* primes;
  int count;
} prime_array;

typedef struct {
  int n;
  int ra;
} rad_pair;

prime_array get_prime_factors(int n) {
  int factors[100];

  int c = 0, i;
  
  for (i = 2; n != 1; i++) {
	while (n % i == 0) {
	  
	  if (!c || factors[c - 1] != i) {

		factors[c] = i;
		c++;
	  }

	  n /= i;
	  
	}
  }

  prime_array p = {NULL, 0};

  if (!c) {
	return p;
  }
  
  int* ret = (int*)malloc(c*sizeof(int));

  memcpy(ret, factors, c*sizeof(int));

  p.primes = ret;
  p.count = c;
  
  return p;
}

int rad(int n) {
  prime_array ps = get_prime_factors(n);
  int i, ra = 1;

  for (i = 0; i < ps.count; i++) {
	ra *= ps.primes[i];
  }

  free(ps.primes);

  return ra;
}

int compare_rad(const void* a, const void* b) {
  rad_pair* p1 = (rad_pair*)a;
  rad_pair* p2 = (rad_pair*)b;

  if (p1->ra < p2->ra) {
	return -1;
  }

  if (p1->ra > p2->ra) {
	return 1;
  }
  
  return p1->n - p2->n;
}

int main() {
  assert(rad(504) == 42);

  int i;
  rad_pair pairs[MAX_N];

  for (i = 1; i <= MAX_N; i++) {
	rad_pair pa = { i, rad(i) };
	pairs[i - 1] = pa;
  }


  puts("generated, sorting...");


  qsort(pairs, MAX_N, sizeof(rad_pair), compare_rad);

  for (i = 1; i <= MAX_N; i++) {
	printf("%i\t%i\n", pairs[i - 1].n, pairs[i - 1].ra);
  }

  //  assert(pairs[4 - 1].n == 8);
  //  assert(pairs[6 - 1].n == 9);

  printf("result: %i\n", pairs[10000 - 1].n);

  return 0;
}


