import copy

inputFile = open("input", "r")
lines = inputFile.readlines()
lines = [l.strip().split(" ") for l in lines]

# [instruction, value, previously executed]
instructions = [[inst, int(val), 0] for [inst, val] in lines]

accumulator = 0

def run(instructionset, pc):
    program_counter = pc
    global accumulator 
    accumulator = 0
    while(True):
        if program_counter == len(instructionset):
            return True
        instruction = instructionset[program_counter]
        if(instruction[2] == 1):
            return False
        else:
            # print(str(program_counter).zfill(3) + " " + str(instruction[2]) + " \t" + instruction[0] 
            #         + " " + str(instruction[1]).rjust(5, ' ') + "\t| " + str(accumulator))
            if instruction[0] == "jmp":
                program_counter += instruction[1]-1
                if program_counter >= len(instructionset):
                    return False
            elif instruction[0] == "acc":
                accumulator += instruction[1]
            program_counter+=1
            instruction[2] += 1

for i in range(0, len(instructions)):
    instructions = [[x, y, 0] for [x, y, _] in instructions]
    remember = instructions[i][0]
    if instructions[i][0] == "nop":
        instructions[i][0] = "jmp"
    elif instructions[i][0] == "jmp":
        instructions[i][0] = "nop"
    else:
        continue
    if run(instructions, 0):
        print(accumulator)
        break
    else:
        instructions[i][0] = remember
