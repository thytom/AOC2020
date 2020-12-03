input = open('input', 'r')
inputs = input.readlines()

nums = list(map(int, inputs))

for i in range(0, len(nums)):
    for j in range(0, len(nums)):
        for k in range(0, len(nums)):
            if nums[i] + nums[j]+ nums[k] == 2020 and (nums[i] != 1 or nums[j] != 1 or nums[k] != 1):
                print(nums[i] * nums[j] * nums[k])
                break
