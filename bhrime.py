# bhrime.py
# some prime number generators
# python 3.8
# updated 2020-11-14 by Steven Bhardwaj

def bhrime(n):
    # github: bhrdj/bhack/bhrime.py
    # output list of prime numbers from 2 up to and including n

    b = [True]*(n+1)
    b[:2] = [False]*2    
    for i in range(2,n):
        if b[i]:
            for j in range(2,n//i+1):
                b[i*j] = False
                print(i,j,b[i*j])
    return b
