
#define MAX_N 100001

int* make_ar(int n) {
    return malloc(sizeof(int) * (n + 1));
}

int main() {
    int* answers[MAX_N];

    answers[0] = make_ar(0);
    answers[1] = make_ar(1);
    
    answers[0][0] = 1;
    answers[1][1] = 1;

    for (int n = 2; n < MAX_N; n++) {
        answers[n] = make_ar(n);
        int an = 0;
        //        int s = (n + 1)/2;

        for (int rm = 1; rm < n; rm++) {
            int rm_m = (n - rm > rm ? rm : n - rm);
            an = (an + answers[n - rm][rm_m]) % 10000000;
            answers[n][rm] = an;
        }

        an++;
        answers[n][n] = an;

        if (an % 1000 == 0)
            printf("%i -> %i\n", n, an);

        if (an % 1000000 == 0)
            printf("wow!!!!!!!!!!!!!1\n");
    }
}
