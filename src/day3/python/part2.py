file = open("input", 'r')
filelines = [line.rstrip() for line in file.readlines()]


def checkslope(across, down):
    count = 0
    currentxpos = 0
    i = 0
    while i < len(filelines):
        if filelines[i][currentxpos] == '#':
            count += 1

        currentxpos = (currentxpos + across) % len(filelines[0])
        i += down
    return count

slopestocheck = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

prod = 1
for (a, b) in slopestocheck:
    prod *= checkslope(a, b)

print(prod)
