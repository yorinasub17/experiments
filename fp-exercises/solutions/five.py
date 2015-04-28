import math

def fsum(f):
    def inner(a, b):
        return sum(map(f, xrange(a, b)))
    return fsum

segment_sum = fsum(lambda x: x)
square_sum = fsum(lambda x: x*x)
log_sum = fsum(math.log)
