class Solution:
    '''
    Given an array of meeting time intervals consisting of start and end times
    [[s1,e1],[s2,e2],...] (si < ei), find the minimum number of conference rooms
    required.
    '''
    def minMeetingRooms(self, intervals: List[List[int]]) -> int:

        intervals.sort(reverse=True)

        n = len(intervals)
        if n <= 1: return n

        cnt = 0
        interval = intervals.pop()
        s0, e0 = interval
        rooms = {}
        rooms[cnt] = [interval]
        while intervals:
            interval = intervals.pop()
            s1, e1 = interval
            found = False
            for i in rooms:
                s0, e0 = rooms[i][-1]
                if s1 < e0:
                    continue
                else:
                    found = True
                    rooms[i].append(interval)
                    break
            if not found:
                cnt += 1
                rooms[cnt] = [interval]

        return len(rooms)

