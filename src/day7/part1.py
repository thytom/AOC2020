import re

file = open("input", "r")
lines = [line.strip() for line in  file.readlines()]

colours = {}

def parse(line):
    line = re.sub('[0-9] |\.| bags| bag', '', line)
    bag, *links = re.split(' contain |, ', line)
    colours[bag] = links

[parse(line) for line in lines]

def containsColour(colour, target):
    if colour == target:
        return False

    contains = False
    for link in colours[colour]:
        if link == target:
            contains = True
            break
        elif link in colours:
            contains = contains or containsColour(link, target)
    return contains

# [print(colour + ": " + str(colours[colour])) for colour in colours]
print(len([col for col in colours if containsColour(col, "shiny gold")]))
