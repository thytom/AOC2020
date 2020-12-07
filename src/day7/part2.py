import re

file = open("input", "r")
lines = [line.strip() for line in  file.readlines()]

colours = {}

def parse(line):
    line = re.sub('\.| bags| bag', '', line)
    bag, *links = re.split(' contain |, ', line)
    if "no other" in links:
        links = []
    else:
        links = [link.split(' ', 1) for link in links]
    links = [(int(a), b) for (a, b) in links]
    colours[bag] = links

def countbags(colour):
    count = 0
    if colours[colour] != []:
        for (num, bag) in colours[colour]:
            count += num + (num * countbags(bag))
    else:
        return 0
    print(str(colour) + ": " + str(count))
    return count

[parse(line) for line in lines]

print(countbags("shiny gold"))
