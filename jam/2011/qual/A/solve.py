

def solve_case():
    bts_s = raw_input()
    bts = bts_s.split(' ')

    btn_n = int(bts[0])

    buttons = []
    rbuttons = {'B': [], 'O': []}

    steps_needed = 0
    
    for i in xrange(btn_n):
        r, bn = bts[i*2+1:i*2+3]

        bn = int(bn)
        
        buttons.append((r, bn))
        rbuttons[r].append(bn)

    position = {'B': 1, 'O': 1}

    cur = None
    
    while buttons or cur:
        acts = {'B': None, 'O': None}

        if not cur:
            cur = buttons.pop(0)
        
        
        # if should press, press
        if position[cur[0]] == cur[1]:
            acts[cur[0]] = ('press', 1)
            assert cur[1] == rbuttons[cur[0]].pop(0)
            cur = None
            
        # if should go, go
        for k, v in acts.iteritems():
            if not v:
                if rbuttons[k]:
                    next_b = rbuttons[k][0]

                    if next_b != position[k]:
                        acts[k] = ('go', position[k] - next_b)
                    else:
                        acts[k] = ('stay', 0)
                else:
                    acts[k] = ('stay', 0)

        # now we know how to act
        max_dx = min(abs(a[1]) for a in acts.itervalues() if a[1])

        print 'step', steps_needed, ':', max_dx
        print position
        print acts
        
        steps_needed += max_dx

        for k, (t, v) in acts.iteritems():
            if t == 'go':
                if v > 0:
                    position[k] -= max_dx
                else:
                    position[k] += max_dx

    return steps_needed
    
    
def main():
    case_n = int(raw_input())

    for i in xrange(case_n):
        print '#%i:' % i, solve_case()

if __name__ == '__main__':
    main()
