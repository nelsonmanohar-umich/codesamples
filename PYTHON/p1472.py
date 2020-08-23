class BrowserHistory:
    '''
    You have a browser of one tab where you start on the homepage and you can visit
    another url, get back in the history number of steps or move forward in the
    history number of steps.  Implement the BrowserHistory class:

    BrowserHistory(string homepage)
       Initializes the object with the homepage of the browser.
    void visit(string url)
       visits url from the current page. It clears up all the forward history.
    string back(int steps)
       Move steps back in history. If you can only return x steps in the history and
       steps > x, you will return only x steps. Return the current url after moving
       back in history at most steps.
    string forward(int steps)
       Move steps forward in history. If you can only forward x steps in the history
       and steps > x, you will forward only x steps. Return the current url after
       forwarding in history at most steps.
    '''
    def __init__(self, homepage: str):
        self.history = [homepage]
        self.curr = len(self.history) - 1
        return


    def visit(self, url: str) -> None:
        self.history = self.history[:(self.curr + 1)]
        self.history.append(url)
        self.curr = len(self.history) - 1
        return


    def back(self, steps: int) -> str:
        self.curr -= steps
        if self.curr < 0: self.curr = 0
        return self.history[self.curr]


    def forward(self, steps: int) -> str:
        n = len(self.history)
        self.curr += steps
        if self.curr >= n: self.curr = n - 1
        return self.history[self.curr]



# Your BrowserHistory object will be instantiated and called as such:
# obj = BrowserHistory(homepage)
# obj.visit(url)
# param_2 = obj.back(steps)
# param_3 = obj.forward(steps)
