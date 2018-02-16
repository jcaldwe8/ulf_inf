
from subprocess import call

for i in range(1, 21):
  call(["wget", 
    "http://www.cs.rochester.edu/research/cisd/resources/monroe/s{}/s{}.transcript".format(i + 1, i + 1)])

