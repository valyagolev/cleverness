from __future__ import division

from collections import deque

from fractions import gcd
from math import sqrt, ceil


def combine(t1, t2, d):
    a1, b1, c1 = t1
    a2, b2, c2 = t2

    return (a1 * a2 + d * b1 * b2, a1*b2 + b1*a2, c1*c2)

def divides(a, b):
    return a % b == 0

def is_quad(n):
    return is_int(sqrt(n))

def is_int(n):
    return n == int(n)

def ring_inv(a, m):
    print "inverting %d %d" % (a, m)

    for i in xrange(int(m)):
        if (a * i) % m == 1:
            return i

    assert False, "No inverse element for %i, %i" % (a, m)

def super_mod(a, b):
    m = a % b
    if m < 0:
        print "super mod:", m, b, m - b
        return m - b
    else:
        return m

    
def ring_div(a, b, m):   
    if b == 0:
        return 0
    
    d = gcd(a, m)

    assert divides(b, d), "%f %f" % (b, d)

    a1 = a//d
    b1 = b//d
    m1 = m//d

    c = ring_inv(a1, m1)

    x = (c * b1) % m

    assert (a * x) % m == b % m

    return x

def get_m(d, a, b, k):
    mpr = int(sqrt(d))

    l = super_mod(mpr*b + a, k)
    
    if l < 0:
        l += abs(k)

    df = ring_div(b, l, k)

    var1 = mpr - df

    assert super_mod((mpr - var1) * b, k) == l

#    assert var1 == int(var1), "%d, %d, %d" % (d, var1, int(var1))

    u = ceil((mpr - var1) / k)
    var2 = u*k + var1
    
#    assert var2 == int(var2),  "%d, %d, %d" % (d, var2, int(var2))

    if var1 > var2:
        var1, var2 = var2, var1

    assert divides(var1 * b + a, k), locals()
    assert var1 <= mpr <= var2, locals()

    if mpr - var1 > var2 - mpr:
        return var2
    else:
        return var1

queue = deque()
    
def normalize(a, b, k, d, init=None):
    print "normalizing: ", a, b, k, d

    if not init:
        init = (a, b, k)
    
    if is_int(a) and is_int(b) and a > 0 and b > 0 and k > 0 and k == 1:
        return a, b, k

    a, b, k = combine((a, b, k), init, d)

    if is_quad(abs(k)):
        dd = sqrt(abs(k))
        if divides(a, dd) and divides(b, dd):
            a, b, k = a / dd, b / dd, k / abs(k)
    
    if is_int(a) and is_int(b) and a > 0 and b > 0 and k > 0 and k == 1:
        return a, b, k
    
    queue.append((a, b, k, d, init))
    queue.append((a, b, k, d))

#    print queue

    return normalize(*queue.popleft())

    
def solve(d):   
    a = int(sqrt(d)) + 1
    b = 1
    k = a*a - b*b*d

    step = 0
    
    while True:
        if abs(k) in (1, 2, 4):
            if abs(k) == 4:
                a, b, k = a/2, b/2, k/4
                
            return normalize(a, b, k, d)

        step += 1
        print "Step: ", step, "for", d
        
        m = get_m(d, a, b, k)
        print "  m =", m

        print "  Old:", a, b, k
#        print (a, b, k), (m, 1, m*m - d)
        a, b, k = ((a*m + d*b) / abs(k),
                   (a + b*m) / abs(k),
                   (m*m - d) / k)
        print "  New:", a, b, k
        
#        if a > 1000000000:
#            print "OMGGGGG"
#            return None

    

def main():

    nones = 0
    max_i = 0
    max_v = 0
    for i in xrange(1001):
        if i in (721, 769, 889):
            continue
        if sqrt(i) == int(sqrt(i)):
            continue

        
        r = solve(i)
        print r, i
        if not r:
            nones += 1
            print i, "!"
        else:
            if max_v < r[0]:
                max_i = i
                max_v = r[0]


    print locals()
#    for d in xrange(2, 1001):
#        print solve(d)
        
if __name__ == '__main__':
    main()
