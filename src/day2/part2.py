def isPasswordValid(passwordArr):
    pos1 = int(passwordArr[0]) - 1
    pos2 = int(passwordArr[1]) - 1
    char = passwordArr[2]
    password = passwordArr[3]

    count = 0
    if password[pos1] == char:
        count += 1
    if password[pos2] == char:
        count += 1

    if count != 1:
        return False
    else:
        return True


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
