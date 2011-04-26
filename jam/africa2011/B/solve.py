import itertools
import sys

class Edge(object):
    def __init__(self, a, b):
        self.a = a
        self.b = b
        self.frz = frozenset([a, b])

    def __contains__(self, t):
        return t in self.frz

    def __repr__(self):
        return 'Edge(%i, %i)' % (self.a, self.b)
        
    def connected_edges(self, edges):

        found = set()
        to_try = set()
        
        for edge in edges:
            if self.a in edge or self.b in edge:
                found.add(edge)
                to_try.add(edge)

        notyet = edges
                
        while to_try:
            notyet -= found
            ed = to_try.pop()

            for edge in notyet:
                if ed.a in edge or ed.b in edge:
                    found.add(edge)
                    to_try.add(edge)
            

        return found

        
class Town(object):
    def __init__(self, n, edges):
        self.n = n
        self.edges = [e for e in edges if n in e]

    def __repr__(self):
        return 'Town(%i, ...)' % self.n

    def degree(self):
        return len(self.edges)
    
    def odd(self):
        return self.degree() % 2

        
class Group(object):
    def __init__(self, edges, _towns_collection):
        self.edges = edges
        self.reset_towns(_towns_collection)

    def reset_towns(self, _towns_collection):
        self.towns = [t for t in _towns_collection
                      if any(t.n in e for e in self.edges)]

    def __repr__(self):
        return 'Group(%s)' % str(self.edges)

    def oddness(self):
        return sum(1 for t in self.towns if t.odd())

        
def divide_groups(_edges, towns):
    edges = set(_edges)

    while edges:
        current_edge = edges.pop()

        connected = set(current_edge.connected_edges(set(edges)))

        yield Group([current_edge] + list(connected), towns)

        edges -= connected

        print >>sys.stderr, '%i/%i' % (len(_edges) - len(edges), len(_edges))


def select_fix_groups(groups):
    """O(groups)"""
    return sorted(groups, key=Group.oddness)[:2]


def select_fix_town(group):
    for t in group.towns:
        if t.odd():
            return t

        
    return group.towns[0]

        
def select_fix_towns(groups):
    g1, g2 = select_fix_groups(groups)
    
    t1 = select_fix_town(g1)
    t2 = select_fix_town(g2)

    return (t1, g1), (t2, g2)
    
        
def fix_groups(groups, edges, towns):
    total_new_edges = 0
    
    while len(groups) > 1:
        (t1, g1), (t2, g2) = select_fix_towns(groups)

        e = Edge(t1.n, t2.n)
        edges.append(e)

        t1.edges.append(e)
        t2.edges.append(e)

        g1.edges += g2.edges
        g1.edges.append(e)

        g1.reset_towns(towns)
        
        groups.remove(g2)

        total_new_edges += 1

    return total_new_edges


def fix_degrees(edges, towns):
    """O(towns)"""
    
    total_new_edges2 = sum(1 for t in towns if t.odd())

    assert total_new_edges2 % 2 == 0

    return total_new_edges2 / 2
    
    
def solve_case(case_n):
    print >>sys.stderr, "========================================"
    print >>sys.stderr, "========================================"
    print >>sys.stderr, "========================================"
    print >>sys.stderr, "Case #%i:" % case_n
    print ("Case #%i:" % case_n),

    N = int(raw_input())
    R = int(raw_input())

    print >>sys.stderr, N, R

    edges = []
    towns = []
    groups = []

    new_roads = 0
    
    for i in xrange(R):
        a, b = raw_input().split(' ')

        edges.append(Edge(int(a), int(b)))
        
    for i in xrange(N):
        towns.append(Town(i, edges))

    print >>sys.stderr, 'dividing...'
        
    groups = list(divide_groups(edges, towns))

    print >>sys.stderr, ''
    print >>sys.stderr, 'Edges: ', edges
    print >>sys.stderr, 'Towns: ', towns
    print >>sys.stderr, 'Groups: ', groups
    
    # make graph connected
    new_roads += fix_groups(groups, edges, towns)

    # smooth all degrees
    new_roads += fix_degrees(edges, towns)
    


    
    print new_roads


        

    
def main():
    print 'GO'
    T = int(raw_input())

    for i in xrange(T):
        solve_case(i + 1)

if __name__ == '__main__':
    main()

    
