class Solution:
    '''
    A transaction is possibly invalid if:
        the amount exceeds $1000, or;
        if it occurs within (and including) 60 minutes of another
        transaction with the same name in a different city.

    Each transaction string transactions[i] consists of comma separated
    values representing the name, time (in minutes), amount, and city of
    the transaction.  Given a list of transactions, return a list of
    transactions that are possibly invalid.  You may return the answer
    in any order.

    Example 1:
        Input: transactions = ["alice,20,800,mtv","alice,50,100,beijing"]
        Output: ["alice,20,800,mtv","alice,50,100,beijing"]
        Explanation: The first transaction is invalid because the second transaction
        occurs within a difference of 60 minutes, have the same name and is in a
        different city. Similarly the second one is invalid too.
    '''
    def invalidTransactions(self, transactions: List[str]) -> List[str]:
        n = len(transactions)
        if n <= 1: return []

        users = {}
        invalids = {}
        for transaction in transactions:
            user, time, amount, city = transaction.split(',')
            if int(amount) > 1000:
                invalids[transaction] = True
            if user not in users:
                users[user] = {}
            if city not in users[user]:
                users[user][city] = []
            users[user][city].append((int(time), transaction))

        for transaction in transactions:
            user, time, amount, city = transaction.split(',')
            time = int(time)
            cities = list([c for c in users[user].keys() if c!=city])
            if not cities: continue
            for c in cities:
                times = [(i, t, conflict) for i, (t, conflict) in enumerate(users[user][c]) if time-60 <= t <= time]
                if not times: continue
                invalids[transaction] = True
                for ix, t, conflict in times:
                     invalids[conflict] = True

        return invalids.keys()

