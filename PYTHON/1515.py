class Solution:
    '''
    A delivery company wants to build a new service centre in a new city.
    The company knows the positions of all the customers in this city on
    a 2D-Map and wants to build the new centre in a position such that
    the sum of the euclidean distances to all customers is minimum.

    Given an array positions where positions[i] = [xi, yi] is the position
    of the ith customer on the map, return the minimum sum of the euclidean
    distances to all customers.

    In other words, you need to choose the position of the service centre
    [xcentre, ycentre] such that the following formula is minimized:

    Answers within 10^-5 of the actual value will be accepted.
    '''
    def getMinDistSum(self, positions: List[List[int]]) -> float:
        from math import dist
        import random

        def sgd(c0, d0, nmin=10, iters=1000, eta=0.01):
            for i in range(iters):
                random_choices = random.sample(positions, nmin)
                for p in random_choices:
                    dp2c = dist(c0, p)
                    if not dp2c: break
                    dx = (p[0] - c0[0]) / dp2c
                    dy = (p[1] - c0[1]) / dp2c
                    c1 = c0[0] + eta * dx, c0[1] + eta * dy
                    d0 = sum([dist(c1, p) for p in positions])
                    if d0 < self.min_dist:
                        self.min_dist = d0
                        c0 = c1
                if d0 < self.min_dist:
                    self.min_dist = d0
            return c0, d0

        xmax, ymax = max([p[0] for p in positions]), max([p[1] for p in positions])
        xmin, ymin = min([p[0] for p in positions]), min([p[1] for p in positions])

        norm_positions = [[(p[0]-xmin), (p[1]-ymin)] for p in positions]
        n = len(positions)

        cx = (sum([p[0]+xmin for p in norm_positions])/n)
        cy = (sum([p[1]+ymin for p in norm_positions])/n)
        centroid = (cx, cy)
        self.min_dist = sum([dist(centroid, p) for p in positions])

        c0 = centroid
        d0 = sum([dist(c0, p) for p in positions])
        nmin = 1 if n < 10 else 2 if n  < 100 else 3

        learning_rates = [100, 10, 1, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7]
        for eps in learning_rates:
            c0, d0 = sgd(c0, d0, nmin=nmin, iters=100,  eta=eps)

        return self.min_dist


