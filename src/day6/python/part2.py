f = open("input", "r")
lines = f.read().split("\n\n")

parsed = [x.split("\n") for x in lines]

noempty = [[x for x in l if x != ''] for l in parsed]

def getCommon(arr):
    head, *rest = arr
    ret = head
    for r in rest:
        ret = list(set(ret).intersection(set(r)))
    return ret

uniq = [len(getCommon(x)) for x in noempty]

print(sum(uniq))
