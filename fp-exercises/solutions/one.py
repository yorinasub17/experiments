from cytoolz.itertoolz import drop, take

def integers(start=0):
    while True:
        yield start
        start += 1

def range_(a, b):
    return take(b-a, integers(a))
