from risk.group import Group

__author__ = 'Chu-Chang Ku'
__all__ = ['fill_tags']


def fill_tags(pop0, sel_risk, fn_rank):
    pop = list(pop0)

    for key, tag, p_targets, fil in sel_risk:
        eligible = [gp for gp in pop if fil(gp)]
        n_eligible = sum([gp.N for gp in eligible])

        new_pop = [gp for gp in pop if not fil(gp)]

        eli_rnk = [(gp, fn_rank(gp)) for gp in eligible]
        range_rnk = list(set(rnk for _, rnk in eli_rnk))
        range_rnk.sort(reverse=True)

        n_targets = n_eligible * p_targets

        for rnk in range_rnk:
            at_risk = [gp for gp, r in eli_rnk if r >= rnk]

            n_at_risk = sum([gp.N for gp in at_risk])
            pr = n_targets / n_at_risk if n_at_risk > n_targets else 1

            for gp in at_risk:
                new_pop += list(gp.divide(pr, tag))

            eli_rnk = [(gp, r) for gp, r in eli_rnk if r < rnk]
            n_targets -= pr * n_at_risk

            if n_targets <= 0:
                break

        for gp, _ in eli_rnk:
            new_pop.append(gp)

        pop = [gp for gp in new_pop if gp.N > 1]
    return pop


if __name__ == '__main__':

    ps0 = [Group(50, 'b', 1000)]

    risks = [
        ('f1', 'f1', 0.1, lambda gp: gp.Sex == 'b'),
        ('f2', 'f2', 0.04, lambda gp: gp.Sex == 'b'),
        ('f3', 'f3', 0.2, lambda gp: gp.Sex == 'b')
    ]

    # Clustering
    print('Clustered')
    ps1 = fill_tags(ps0, risks, fn_rank=lambda gp: len(gp.Tags))

    for p in ps1:
        print(p)

    # Exclusive
    print('Exclusive')
    ps2 = fill_tags(ps0, risks, fn_rank=lambda gp: -len(gp.Tags))

    for p in ps2:
        print(p)

    # Independent
    print('Independent')
    ps3 = fill_tags(ps0, risks, fn_rank=lambda gp: 1)

    for p in ps3:
        print(p)
