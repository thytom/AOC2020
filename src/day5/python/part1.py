f = open("input", "r")
lines = f.readlines()

def seatid(seat):
    return (row(seat[0:8]) * 8) + col(seat[7:len(seat)])

def row(s):
    return bsp(s, range(128))

def col(s):
    return bsp(s, range(8))

def bsp(string, arr):
    head, *rest = string
    array = arr
    if rest == []:
        return array[0]
    if head == 'F' or head == 'L':
        return bsp(rest, array[0:int(len(array)/2)])
    elif head == 'B' or head == 'R':
        return bsp(rest, array[int(len(array)/2):len(array)])

print(max([seatid(line) for line in lines]))
