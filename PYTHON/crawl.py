from bs4 import BeautifulSoup
import requests
import urllib
import time
from multiprocessing import Process, Queue, Pool
from multiprocessing import Manager
import multiprocessing as mp
import os



def get_url_netloc(http):
    res = urllib.parse.urlparse(http)
    # SplitResult( # scheme='http', # netloc='foo.bar', # path='/path;', # query='', # fragment='')
    its_domain = res.netloc
    return its_domain


def correct_url_netloc(http, its_domain, focus_domain):
    if its_domain == "":
        http = "https://%s/%s" % (focus_domain , http)
        print("********* correcting by prepending https://", http)
    return http


def validate_url_focus(http, focus_domain):
    its_domain = get_url_netloc(http)
    print("********* validating %s wrt %s, %s" % (http, focus_domain, its_domain))
    if focus_domain in its_domain or its_domain in focus_domain or its_domain == "":
        http = correct_url_netloc(http, its_domain, focus_domain)
        return True, http
    else:
        return False, http


def get_urikey(http):
    http = http.lstrip('https://')
    http = http.lstrip('http://')
    http = http.lstrip('ftp://')
    urikey = http
    return urikey


def been_visited(visited_lock, http, visited_urls):
    http_key = get_urikey(http)
    visited_lock.acquire()
    print("***********", http_key)
    print("**************", visited_urls)
    visited = http_key in visited_urls
    visited_lock.release()
    return visited


def set_visited(visited_lock, http, visited_urls, pid):
    http_key = get_urikey(http)
    visited_lock.acquire()
    visited_urls[http_key] = pid
    visited_lock.release()
    return


def crawl(i, pid, url, focus_domain, visited_lock, visited_urls, visitation_queue, politeness=1, debug=False):
    print()
    print("****** CRAWLER %s:%s received %s" % (i, pid, url))
    within, url = validate_url_focus(url, focus_domain)
    if not within: return
    if been_visited(visited_lock, url, visited_urls): return
    set_visited(visited_lock, url, visited_urls, pid)
    print("****** CRAWLER %s:%s visiting %s" % (i, pid, url))

    f = requests.get(url)
    text = f.text
    if debug:
        print('-' * 32)
        print("*** crawling %s" % url)
        print("retrieved", text)
        print('-' * 32)

    print("********* CRAWLER %s:%s crawling %s" % (i, pid, url))
    soup = BeautifulSoup(text, 'html.parser')
    for link in soup.find_all('a', href=True):
        http = link['href']
        within, http = validate_url_focus(http, focus_domain)
        if not within: continue
        if been_visited(visited_lock, http, visited_urls): continue
        set_visited(visited_lock, url, visited_urls, pid)
        visitation_queue.put(http)
        print("********* scheduled %s" % http)
        time.sleep(politeness)
        print('-' * 32)
    time.sleep(politeness)
    return


def crawler(*args, **kargs):
    i, focus_domain, visited_lock, visited_urls, visitation_queue = args
    pid = os.getpid()
    while True:
        url = visitation_queue.get()
        if "END" == url:
            print("*** CRAWLER %s:%s shutting down exiting %s" % (i, pid, url))
            exit()
        crawl(i, pid, url, focus_domain, visited_lock, visited_urls, visitation_queue)
    return



if __name__ == '__main__':
    # mp.set_start_method('spawn')
    url = "http://google.com/"
    res = urllib.parse.urlparse(url)
    focus_domain = res.netloc
    N = 4
    with mp.Manager() as manager:
        visited_urls = manager.dict()
        visited_lock = manager.Lock()
        visitation_queue = manager.Queue()

        # crawl(url, focus_domain, visited_lock, visited_urls, visitation_queue)
        # focus_domain, visited_lock, visited_urls, visitation_queue = args
        procs = []
        for i in range(N):
            p = mp.Process(target=crawler, args=(i, focus_domain, visited_lock, visited_urls, visitation_queue), daemon=True)
            p.start()
            print("Lauched %i crawler daemon" % i, p)
            procs.append(p)


        print("START", focus_domain)
        print("*** scheduling %s crawl" % url)
        visitation_queue.put(url)
        while True:
            try:
                time.sleep(1)
            except:
                visitation_queue.put("END")
                time.sleep(1)
                exit()


