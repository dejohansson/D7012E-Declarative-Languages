n = 1024
b = 2
m = 1
s = 0
p = 1
while n > 0:
    q = n//b
    r = n-q*b
    print(r)
    s = p*r+s              
    p = p*10
    n = q
print(s)