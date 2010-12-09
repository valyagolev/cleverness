#!/usr/bin/python

import sys

def main():
    arg = int(sys.argv[1])

    for i in xrange(1, arg + 1):
        if i % 3 == 0:
            if i % 5 == 0:
                print "Hop"
            else:
                print "Hoppity"
        elif i % 5 == 0:
            print "Hophop"

    

if __name__ == '__main__':
    main()
