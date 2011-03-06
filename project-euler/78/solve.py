"""
>>> p(5)
7
>>> p(5, 1) # only 1+1+1+1+1
1
>>> p(5, 2)
3

Okay, I've tried to solve it for soooo long,
and then I've decided to give up and open
Wikipedia: http://en.wikipedia.org/wiki/Partition_(number_theory)



OOOOO = 5
OOOO O = 41
OOO OO = 32
OOO O O = 311
OO OO O = 221
OO O O O = 2111
O O O O O = 11111



OOOO 0
OOO| 1
OO|O 2
OO|| 3
O|O| 5
O||| 7
|||| 15

11111
2111
311 221
41 32 41 32
5 5
"""
from collections import defaultdict

MAX_N = 100000

def pent(n):
    return (3*n*n - n)/2

def gen_pent():
    for i in xrange(1, MAX_N):
        yield pent(i)
        yield pent(-i)

koeffs = list(gen_pent())

answers = {}

def strange_formula(k):
    if k in answers:
        return answers[k]
    
    if k < 0:
        return 0
    
    if k == 0:
        return 1

    r = 0
    i = 0
    while k >= koeffs[i]:
        if (int(i / 2) % 2 == 0):
            r += strange_formula(k - koeffs[i])
        else:
            r -= strange_formula(k - koeffs[i])
        i += 1

    answers[k] = r
    return r
        
def main():
#    print koeffs

    for i in xrange(MAX_N):
        r = strange_formula(i)
        if i % 1000 == 0:
            print i
        if r % 1000000 == 0:
            print i, r
            return
    
    
# OLD    






answers = {}
big_answers = {}

def collect(max_n):
    big_answers[0] = 1
    big_answers[1] = 1
    
    for n in xrange(2, max_n + 1):
        an = 0
        s = int((n + 1) / 2)
        for real_max in xrange(1, s):
            an += answers[(n - real_max, real_max)]
            an %= 10000000
            del answers[(n - real_max, real_max)]
            answers[(n, real_max)] = an

        for real_max in xrange(s, n):
            an += big_answers[n - real_max]
            an %= 10000000
            answers[(n, real_max)] = an

        an += 1
        big_answers[n] = an


#        print n, an
            
        if an % 1000 == 0:
            print n, an

#    print hits
#    print answers

def p(n, mx):
    # reference impl?
    if n < 2:
        return 1
    
    if mx > n:
        mx = n
        
    if (n, mx) in answers:
        return answers[(n, mx)]

    assert n == mx, (n, mx)
    
    an = 0
    for real_max in xrange(1, int(n/2 + 1)):# mx + 1):
        print n, real_max
        r = p(n - real_max, real_max)
        if n - real_max < real_max:            
            print n, real_max, n - real_max, r
        an += r
        answers[(n, real_max)] = an

    for real_max in xrange(int(n/2 + 1), mx + 1):
        r = p(n - real_max, n - real_max)
        an += r
#        answers[(n, real_max)] = an
        
    answers[(n, mx)] = an

    return an

def old_main():

    strange_formula(5)
    
    return
    collect(10000)
    return
    [p(i, i) for i in xrange(1,20)]
    print p(5, 5)
    print answers
    return
    MAX_N = 20
    perc = 0
    for i in xrange(1, MAX_N + 1):

        new_perc = int(100 * (float(i) / MAX_N))
        if perc != new_perc:
            print '%i%%' % new_perc

        perc = new_perc

        r = p(i, i)
        if r % 10000 == 0:
            print i, r
        
#    for (n, mx), v in answers.iteritems():
#        print (n - mx), v

if __name__ == '__main__':
    main()
