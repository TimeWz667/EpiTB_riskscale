__author__ = 'Chu-Chang Ku'
__all__ = ['Group']


class Group:
    def __init__(self, age, sex, n):
        self.Age = age
        self.Sex = sex
        self.Tags = list()
        self.N = n

    def divide(self, pr=1.0, tag='new feature'):
        g0, g1 = Group(self.Age, self.Sex, self.N * (1 - pr)), Group(self.Age, self.Sex, self.N * pr)
        g0.Tags = list(self.Tags)
        g1.Tags = list(self.Tags)
        g1.Tags.append(tag)
        return g0, g1

    def to_dict(self):
        d = {
            'Age': self.Age,
            'Sex': self.Sex,
            'N': self.N
        }

        for tag in self.Tags:
            d[tag] = 1
        return d

    def __str__(self):
        tags = '|' + ','.join(self.Tags) if len(self.Tags) else ''
        return f'g(Age={self.Age}, Sex={self.Sex}, N={self.N:.1f}{tags})'

    __repr__ = __str__


if __name__ == '__main__':

    gp = Group(30, 'f', 1000)

    print(gp)

    g0, g1 = gp.divide(0.6)

    print(g0)
    print(g1)

    print(g1.to_dict())
