from __future__ import division

from fractions import gcd

import math
import sys

def nok(*args):
    return reduce(gcd, args)

class TheNum(object):
    rational_part = 0
    under_root_part = 0
    denumerator = 1
    root_quofficient = 1

    def __init__(self, under_root_part=0, rational_part=0, denumerator=1, root_quofficient=1):
        self.under_root_part = under_root_part
        self.rational_part = rational_part
        self.denumerator = denumerator
        self.root_quofficient = root_quofficient

        self.normalize()

    def __eq__(self, other):
        return self.vals() == other.vals()

    def __hash__(self):
        return hash(self.vals())

    def vals(self):
        return (self.under_root_part, self.rational_part, self.denumerator, self.root_quofficient)

    def normalize(self):
        if self.denumerator < 0:
            self.rational_part *= -1
            self.root_quofficient *= -1
            self.denumerator *= -1

        snok = nok(self.rational_part, self.root_quofficient, self.denumerator)

        self.rational_part /= snok
        self.root_quofficient /= snok
        self.denumerator /= snok

    def recip(self):
#        if self.under_root_part == 0:
#            return TheNum(rational_part=1/self.rat_part())
        if self.rational_part < 0:
            return TheNum(rational_part= (-1)*self.rational_part * self.denumerator,
                          under_root_part=self.under_root_part,
                          root_quofficient=self.root_quofficient * self.denumerator,
                          denumerator=(self.under_root_part - self.rational_part**2))

        raise Exception(self)

    def __sub__(self, oth):
        if isinstance(oth, int):
            return TheNum(rational_part=self.rational_part - oth * self.denumerator,
                          under_root_part=self.under_root_part,
                          denumerator=self.denumerator)
        
        raise Exception(self)

    def divide_num(self):
        int_p = self.int_part()
        return (int_p, self - int_p)
    
    def rat_part(self):
        assert self.rational_part == int(self.rational_part)
        assert self.root_quofficient == int(self.root_quofficient)
        assert self.under_root_part == int(self.under_root_part)
        return (self.rational_part + self.root_quofficient * math.sqrt(self.under_root_part)) / self.denumerator
    
    def int_part(self):
        return int(self.rat_part())
            

    def __str__(self):
        s = str(self.rational_part)
        
        if self.under_root_part:
            if self.root_quofficient != 1:
                s = '{0}v\'{1}\' - {2}'.format(self.root_quofficient, self.under_root_part, -self.rational_part)
            else:
                s = 'v\'{0}\' - {1}'.format(self.under_root_part, -self.rational_part)

        if self.denumerator != 1:
            s = '( {0} ) / {1}'.format(s, self.denumerator)

        return s
    
def sqrt(n):
    return TheNum(under_root_part=n)
    

def next_step(num):
#    print "    1 / << %s >> = << %s >>" % (num, num.recip())
    return num.recip()


def get_period(n):
    already = set()
    fst, cn = sqrt(n).divide_num()

    for i in xrange(1000):
        also, cn = next_step(cn).divide_num()
#        print i, also, cn, already
        if cn in already:
            return len(already)
        
        already.add(cn)

def is_period(ns, k):
    fst = ns[0:k]
    for b in xrange(5):
        if fst != ns[b*k:(b+1)*k]:
            return False

    return True
        
#def get_period(ns):
#    ns = list(ns)
#    for k in xrange(1, MAX_PER):
#        if is_period(ns, k):
#            return k

#    return None#raise Exception("omg, %s" % ns)

#MAX_PER = 20        

PERS = (23,)# 40, 60, 100, 300, 500, 1000, 10000)# (20, 50, 100, 300)

def get_itr(_itr):
    return ((i, get_period(i)) for i in _itr if int(math.sqrt(i)) != math.sqrt(i))


MAX_PER = 2

#sys.exit(0)



result = 0
unknowns = set()

itr = xrange(10001)




for MAX_PER in PERS:

    print 'Going with MAX_PER = %i' % MAX_PER

    last = -1000
    for i, val in get_itr(itr):
        if i - last > 300:
            print i, val
            last = i
            
        if val is None:
            unknowns.add(i)
        elif val % 2 == 1:
            result += 1
            unknowns.discard(i)
        else:
            unknowns.discard(i)
            

    print result, len(unknowns)
    
    itr = unknowns.copy()
