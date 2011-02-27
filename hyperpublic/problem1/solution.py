


class User(object):
    influence = None

    def __init__(self, id, infls):
        self.infls = infls
        self.id = id

    def __str__(self):
        return "User %i, infls: %s, influence: %i" % (self.id, self.infls, self.get_influence())

    def __repr__(self):
        return "<%s>" % str(self)

    def get_influence(self):
        if not self.influence:
            self.influence = sum(1 + users[i].get_influence() for i in self.infls)

        return self.influence

users = {}

def main():
    f = file('big.txt')

    for i, s in enumerate([s.strip() for s in f if s.strip()]):
        users[i] = User(i, [j for (j, v) in enumerate(s) if v == 'X'])

    user_list = list(users.values())
    user_list.sort(key=User.get_influence, reverse=True)

    print user_list[:-10]

if __name__ == '__main__':
    main()
