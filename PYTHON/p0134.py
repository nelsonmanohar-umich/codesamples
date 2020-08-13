class Solution:
    '''
    There are N gas stations along a circular route, where the amount of gas at station i is gas[i].
    You have a car with an unlimited gas tank and it costs cost[i] of gas to travel from station i
    to its next station (i+1). You begin the journey with an empty tank at one of the gas stations.
    Return the starting gas station's index if you can travel around the circuit once in the
    clockwise direction, otherwise return -1.
    Note:
        If there exists a solution, it is guaranteed to be unique.
        Both input arrays are non-empty and have the same length.
        Each element in the input arrays is a non-negative integer.
    '''
    def canCompleteCircuit(self, gas: List[int], cost: List[int]) -> int:
        n = len(gas)
        if n == 0: return -1
        if n == 1: return 0 if gas[0] >= cost[0] else -1

        path, visited, self.doable = [], {}, -1

        def visit(start, tot, cur,visited={}):
            key = cur
            if key in visited and visited[key] >= tot: return
            visited[key] = tot

            tot = tot + gas[cur] - cost[cur]
            nxt = (cur + 1) % n if tot >= 0 else -1

            if nxt == start:
                if tot >= 0:
                    self.doable = start
                return
            else:
                if nxt != -1:
                    visit(start, tot, nxt, visited)
            return

        for i in range(n):
            start, cur, tot = i, i, 0
            visit(start, tot, cur, visited=visited)
            if self.doable != -1: return start

        return self.doable

