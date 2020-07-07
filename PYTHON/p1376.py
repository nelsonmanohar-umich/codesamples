class Solution:
    '''
    A company has n employees with a unique ID for each employee from 0 to n - 1.
    The head of the company has is the one with headID.
    Each employee has one direct manager given in the manager array where manager[i]
    is the direct manager of the i-th employee, manager[headID] = -1. Also it's
    guaranteed that the subordination relationships have a tree structure.

    The head of the company wants to inform all the employees of the company of an
    urgent piece of news. He will inform his direct subordinates and they will inform
    their subordinates and so on until all employees know about the urgent news.

    The i-th employee needs informTime[i] minutes to inform all of his direct
    subordinates (i.e After informTime[i] minutes, all his direct subordinates can
    start spreading the news).

    Return the number of minutes needed to inform all the employees about the urgent news.
    '''
    def numOfMinutes(self, n: int, headID: int, manager: List[int], informTime: List[int]) -> int:

        def sol1():
            def dfs(emp_id, total):
                total += informTime[emp_id]
                if total > self.total:
                    self.total = total
                if emp_id in subs:
                    for sub_id in subs[emp_id]:
                        dfs(sub_id, total)
                return

            self.total = 0
            subs = {}
            for e, m in enumerate(manager):
                if m not in subs: subs[m] = []
                subs[m].append(e)

            dfs(headID, 0)

            return self.total


        def sol2():
            self.total = 0
            m2e = {}
            for e, m in enumerate(manager):
                if m not in m2e: m2e[m] = []
                m2e[m].append(e)

            stack, visited, times, parent = [], {}, {}, headID
            stack.append((None, parent))
            times[None] = 0

            while stack:
                parent, child = stack.pop()
                times[child] = times[parent] + informTime[child]
                if times[child] > self.total:
                    self.total = times[child]
                if child in m2e:
                    for e in m2e[child]:
                        stack.append((child, e))
            return self.total

        return sol1()


