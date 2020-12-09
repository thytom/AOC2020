f = open("input", "r")
lines = f.read().split("\n\n")

parsed = [set(x.replace("\n", "")) for x in lines]

print(sum([len(x) for x in parsed]))
