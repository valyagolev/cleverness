

# [1/3, 2/3]
# [1/(3^3), 2/(3^3)], [7/(3^2), 8/(3^2)]

from fractions import Fraction



def from_float(s):
    b, a = s.split('.')
    n = len(a)
    m = int(b) * 10 ** n + int(a)
    return Fraction(m, n)


cache = {}

F13 = Fraction(1, 3)
F23 = Fraction(2, 3)

def removed_on_iteration(xs):
    x = Fraction(xs)
    
    if x in cache:
        return cache[x]

    #already = set()
    #    print x
    for i in xrange(1, 100):
#        already.add(x)
        
        x *= 3
        
#        print "try {0}: ".format(i)
#        print "x{{0}} = {1}".format(i, x)

#        if x in already:
#            print "Error! ", x, xs
#            return 1000000 + x
#        else:
#            print x, already
        
        if x > 2:
            x -= 2
        elif x >= 1:
#            print x, ":", i
            return i + float(xs)
    return 101 + float(xs)



    
#    print x, "error!"
    raise Exception(x)


def solve_case(case_n):

    n = int(raw_input())

    numbers = [raw_input() for i in xrange(n)]

    print "Case #%i:" % case_n

    numbers.sort(key=removed_on_iteration)

    for n in numbers:
        print float(n)
    

def main():
#    print removed_on_iteration(Fraction('0.9'))
#    print removed_on_iteration(Fraction('0.1'))
#    return
    t = int(raw_input())

    for i in xrange(t):
        solve_case(i + 1)

if __name__ == '__main__':
    main()
