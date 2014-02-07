import random

def random_vector(mag):
    
    
    x = random.normalvariate(0,1)
    y = random.normalvariate(0,1)
    z = random.normalvariate(0,1)

    denom = (x*x + y*y + z*z)**0.5

    x = x/denom * mag
    y = y/denom * mag
    z = z/denom * mag

    return x,y,z

def write_random_coils(coil_num, mag):

    f = open('coil_rand','w')
    for i in xrange(coil_num):
        x,y,z = random_vector(mag)
        s = '%9.6f  %9.6f  %9.6f \n' %(x,y,z)
        f.write(s)

    f.close()

write_random_coils(48, 0.000001)
