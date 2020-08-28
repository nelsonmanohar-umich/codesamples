class Solution:
    '''
    On a single threaded CPU, we execute some functions.  Each function has a unique
    id between 0 and N-1.  We store logs in timestamp order that describe when a
    function is entered or exited.

    Each log is a string with this format: "{function_id}:{"start" | "end"}:{timestamp}".
    For example, "0:start:3" means the function with id 0 started at the beginning of
    timestamp 3.  "1:end:2" means the function with id 1 ended at the end of timestamp 2.
    A function's exclusive time is the number of units of time spent in this function.
    Note that this does not include any recursive calls to child functions.

    The CPU is single threaded which means that only one function is being executed
    at a given time unit.

    Return the exclusive time of each function, sorted by their function id.
    '''
    def exclusiveTime(self, n: int, logs: List[str]) -> List[int]:
        from collections import OrderedDict
        fids = OrderedDict()
        times = []
        pending = []

        def gettime(t1, t2):
            return int(t1)-int(t2)+1

        for log in logs:
            fid, mode, ts = log.split(":")

            if len(times) == 0:
                times.append( [fid, mode, ts] )
                continue

            if mode == "start":
                f0, m0, t0 = times.pop()

                if fid != f0:
                    # nested function
                    tot = gettime(ts, t0) - 1
                    if f0 not in fids: fids[f0] = 0
                    fids[f0] += tot
                    pending.append([f0, m0, t0])
                    times.append( [fid, mode, ts])
                else:
                    # recursion
                    tot = gettime(ts, t0) - 1
                    if f0 not in fids: fids[f0] = 0
                    fids[f0] += tot
                    pending.append([f0, m0, t0])
                    times.append( [fid, mode, ts])

            elif mode == "end":
                f0, m0, t0 = times.pop()
                #assert fid == f0
                tot = gettime(ts, t0)
                if f0 not in fids: fids[f0] = 0
                fids[f0] += tot
                if pending:
                    f1, m1, t1 = pending.pop()
                    #assert m1 == "start"
                    times.append([f1, m1, str(int(ts)+1)])

        return [fids[f] for f in fids][:n]


