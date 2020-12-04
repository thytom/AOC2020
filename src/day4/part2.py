import re

file = open("input", 'r')
lines = file.readlines()

passports = [passport.replace("\n", " ") for passport in ''.join(lines).split("\n\n")]

validentries = ['byr:', 'iyr:', 'eyr:', 'hgt:', 'hcl:', 'ecl:', 'pid:'] #cid:

def passportvalid(passport):
    entries=re.findall('...:', passport)
    return all([x in entries for x in validentries])

def deepvalidation(passport):
    if passportvalid(passport):
        keys =[s.replace(":", "") for s in re.findall('...:', passport)]
        vals =[s.replace(":", "") for s in re.findall(':[^ ]*', passport)]
        passdict = dict(zip(keys, vals))
        heightCheck = False
        if "in" in passdict['hgt']:
            heightCheck = (int(passdict['hgt'].replace("in","")) >= 59 and 
                    int(passdict['hgt'].replace("in", "")) <= 76)
        elif "cm" in passdict['hgt']:
            heightCheck= (int(passdict['hgt'].replace("cm","")) >= 150 and 
                    int(passdict['hgt'].replace("cm", "")) <= 193)
        else:
            False
        return all([
            int(passdict['byr']) >= 1920 and int(passdict['byr']) <= 2002,
            int(passdict['iyr']) >= 2010 and int(passdict['iyr']) <= 2020,
            int(passdict['eyr']) >= 2020 and int(passdict['eyr']) <= 2030,
            re.search('#[0-9,a-f]{6}', passdict['hcl']) != None and len(passdict['hcl']) == 7,
            heightCheck,
            [x == passdict['ecl'] for x in ['amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth']].count(True) == 1,
            re.search('^[0-9]{9}$', passdict['pid']) != None
            ])
    else:
        return False


print([deepvalidation(passport) for passport in passports].count(True))
