
median_75_v1 = [1,24,76,99]

median_75_dist = [0,1,14,16,20,50,75,76,77,78,79,81,83]

# LESS THAN THE MEDIAN
sum=0
for i in range(1000000):
    index = random.randint(0,12)
    sample = median_75_dist[index]
    if sample<75:
        index = random.randint(0,12)
        sample = median_75_dist[index]
    sum += sample

sum/1000000



# LESS THAN THE MEAN
sum=0
for i in range(1000000):
    index = random.randint(0,12)
    sample = median_75_dist[index]
    if sample<50:
        index = random.randint(0,12)
        sample = median_75_dist[index]
    sum += sample

sum/1000000


median_25_dist = [1,2,3,4,5,24,26,49,51,74,76,85]

# NOW RUN SOME TESTS


