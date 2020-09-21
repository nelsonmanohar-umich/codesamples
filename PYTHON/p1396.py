class UndergroundSystem:
    '''
    Implement the class UndergroundSystem that supports three methods:
     1. checkIn(int id, string stationName, int t)
        A customer with id card equal to id, gets in the station stationName at time t.
        A customer can only be checked into one place at a time.
     2. checkOut(int id, string stationName, int t)
       A customer with id card equal to id, gets out from the station stationName at time t.
     3. getAverageTime(string startStation, string endStation)

     Returns the average time to travel between the startStation and the endStation.
     The average time is computed from all the previous traveling from startStation to
     endStation that happened directly.  Call to getAverageTime is always valid.
     You can assume all calls to checkIn and checkOut methods are consistent. That is,
     if a customer gets in at time t1 at some station, then it gets out at time t2 with
     t2 > t1. All events happen in chronological order.
    '''
    def __init__(self):
        self.customers = {}
        self.avg_times = {}
        return


    def checkIn(self, id: int, stationName: str, t: int) -> None:
        self.customers[id] = (stationName, t)


    def checkOut(self, id: int, stationName: str, t: int) -> None:
        station_out, t_out = stationName, t
        station_in, t_in = self.customers[id]
        key = (station_in, station_out)
        if key not in self.avg_times:
            self.avg_times[key] = [0, 0]
        prev_sum, prev_cnt = self.avg_times[key]
        self.avg_times[key] = [prev_sum + (t_out-t_in), prev_cnt+1]
        return

    def getAverageTime(self, startStation: str, endStation: str) -> float:
        key = (startStation, endStation)
        if key in self.avg_times:
            curr_sum, curr_cnt = self.avg_times[key]
            return curr_sum/curr_cnt
        return 0.


# Your UndergroundSystem object will be instantiated and called as such:
# obj = UndergroundSystem()
# obj.checkIn(id,stationName,t)
# obj.checkOut(id,stationName,t)
# param_3 = obj.getAverageTime(startStation,endStation)
