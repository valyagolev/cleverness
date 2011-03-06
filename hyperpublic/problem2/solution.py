
prices = [98, 42, 23, 17, 3, 2]
points = [2349, 2102, 2001, 1747]

def get_task_count(points, tasks=None):
    if tasks == []:
        if points == 0:
            return 0
        else:
            raise Exception()

    if not tasks:
        tasks = prices
    
    the_task = tasks[0]

    if the_task == 98 and points > the_task * 3:
        prime_task = (points - 100) / the_task
        return prime_task + get_task_count(points - prime_task * 98, tasks)

    max_t = points / the_task


    print "max t is ", max_t, "for", points, tasks

    variants = []

    for i in xrange(max_t + 1):
        points_yet = points - (the_task * i)
        print points_yet, the_task, i
        try:
            variants.append(i + get_task_count(points_yet, tasks[1:]))
        except Exception, e:
            print e

    print variants

    return min(variants)

def main():

    r = 1
    for p in points:
        t = get_task_count(p)
        print t, " for ", p
        r *= t

    print "result: ", r

if __name__ == '__main__':
    main()
