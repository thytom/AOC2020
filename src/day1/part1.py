input = open('input', 'r')
inputs = input.readlines()

for i in range(0, len(inputs)):
    for j in range(0, len(inputs)):
        if int(inputs[i]) + int(inputs[j]) == 2020:
            print(int(inputs[i]) * int(inputs[j]))
            break
