def isPasswordValid(passwordArr):
    min = int(passwordArr[0])
    max = int(passwordArr[1])
    char = passwordArr[2]
    password = passwordArr[3]

    count = 0
    for pchar in password:
        if pchar == char:
            count += 1
    if count <= max and count >= min:
        return True
    else:
        return False

input = open("input", 'r')
inputs = input.readlines()

parsedinput = []

import re

for input in inputs:
    parsedinput.append(re.split('-| |: ', input.rstrip()))

count = 0
for password in parsedinput:
    if isPasswordValid(password):
        count += 1

print(count)
