#!env python3
import sys

a = []
for line in sys.stdin:
    line = line.splitlines()[0]
    a += line.split(' ')

#print(list("'{}'".format(x) for x in a))
print(a)
    
"""
['var', 'x', ';', 'x',':=', '(', 5, '*', 2, ')', ';', 'return', '(', 'x', '+', 1, ')', '.']
"""
