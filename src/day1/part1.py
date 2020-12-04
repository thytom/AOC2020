input = open('input', 'r')
inputs = input.readlines()

for i in range(0, len(inputs)):
    search = 2020 - int(inputs[i])
    if str(search)+"\n" in inputs:
        print(int(inputs[i])* search)
        break;
