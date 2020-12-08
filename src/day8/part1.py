import copy

inputFile = open("input", "r")
lines = inputFile.readlines()
lines = [l.strip().split(" ") for l in lines]

# [instruction, value, previously executed]
instructions = [[inst, int(val), 0] for [inst, val] in lines]

accumulator = 0

def run(instructionset):
    program_counter = 0
    global accumulator
    while(True):
        instruction = instructionset[program_counter]
        if(instruction[2] == 1):
            return False
        else:
            # print(str(program_counter).zfill(3) + " " + str(instruction[2]) + " \t" + instruction[0] 
            #         + " " + str(instruction[1]).rjust(5, ' ') + "\t| " + str(accumulator))
            if instruction[0] == "jmp":
                program_counter += instruction[1]-1
            elif instruction[0] == "acc":
                accumulator += instruction[1]
            program_counter+=1
            instruction[2] += 1
    return True


run(instructions)
print(accumulator)
