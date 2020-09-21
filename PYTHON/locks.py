
class FizzBuzz:
    def __init__(self, n: int):
        import threading
        from threading import Thread, Lock
        from multiprocessing import Queue

        self.lock = Lock()
        self.oq = Queue()
        self.iq = Queue()
        self.n = n

        iq = [Queue(), Queue(), Queue(), Queue()]
        oq = Queue(n+1)

        t = [None, None, None, None]
        t1 = threading.Thread(target=self.fizz, args=(iq[0], oq)).start()
        t2 = threading.Thread(target=self.buzz, args=(iq[1], oq)).start()
        t3 = threading.Thread(target=self.fizzbuzz, args=(iq[2], oq)).start()
        t4 = threading.Thread(target=self.number, args=(iq[3], oq)).start()

        def printFizz(i): print(i, "fizz")
        def printBuzz(i): print(i, "buzz")
        def printFizzBuzz(i): print(i, "Fizzbuzz")
        def printNumber(i,o): print(i, o)

        def printer(out):
            out.sort()
            for i, o in out:
                if o == 'fizz': printFizz(i)
                elif o == 'buzz': printBuzz(i)
                elif o == 'fizzbuzz': printFizzBuzz(i)
                else: printNumber(i,o)

        for i in range(1,n+1):
            if i % 3 == 0 and not i % 5 == 0: iq[0].put(i)
            elif not i % 3 == 0 and i % 5 == 0: iq[1].put(i)
            elif i % 3 == 0 and i % 5 == 0: iq[2].put(i)
            else: iq[3].put(i)

            if i and i % 4 == 0:
                out = [oq.get(), oq.get(), oq.get(), oq.get()]
                printer(out)

        out = []
        for i in range(n % 4): out.append(oq.get())
        printer(out)
        iq[0].put(-1), iq[1].put(-1), iq[2].put(-1), iq[3].put(-1)
        return


    # printFizz() outputs "fizz"
    def fizz(self, iq, oq) -> None:
        while True:
            i = iq.get()
            if i < 0: break
            if i % 3 == 0 and not i % 5 == 0: oq.put((i, "fizz"))
            # iq.task_done()

    # printBuzz() outputs "buzz"
    def buzz(self, iq, oq) -> None:
        while True:
            i = iq.get()
            if i < 0: break
            if not i % 3 == 0 and i % 5 == 0: oq.put((i, "buzz"))
            # iq.task_done()

    # printFizzBuzz() outputs "fizzbuzz"
    def fizzbuzz(self, iq, oq) -> None:
        while True:
            i = iq.get()
            if i < 0: break
            if i % 3 == 0 and i % 5 == 0: oq.put((i, "fizzbuzz"))
            # iq.task_done()

    # printNumber(x) outputs "x", where x is an integer.
    def number(self, iq, oq) -> None:
        while True:
            i = iq.get()
            if i < 0: break
            if not i % 3 == 0 and not i % 5 == 0: oq.put((i, str(i)))
            # iq.task_done()



c = FizzBuzz(15)

