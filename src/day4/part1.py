import re

file = open("input", 'r')
lines = file.readlines()

passports = [passport.replace("\n", "") for passport in ''.join(lines).split("\n\n")]

validentries = ['byr:', 'iyr:', 'eyr:', 'hgt:', 'hcl:', 'ecl:', 'pid:'] #cid:

def passportvalid(passport):
    entries=re.findall('...:', passport)
    return all([x in entries for x in validentries])


print([passportvalid(passport) for passport in passports].count(True))
