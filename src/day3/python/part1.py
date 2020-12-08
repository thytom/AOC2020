file = open("input", 'r')
filelines = [line.rstrip() for line in file.readlines()]

count = 0
i = 0
currentxpos = 0
while i < len(filelines):
    if filelines[i][currentxpos] == '#':
        count += 1

    currentxpos = (currentxpos + 3) % len(filelines[0])
    i += 1

print(count)
