# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    '''
    Given a singly linked list, group all odd nodes together followed
    by the even nodes. Please note here we are talking about the node
    number and not the value in the nodes.

    You should try to do it in place. The program should run in O(1)
    space complexity and O(nodes) time complexity.

    Example 1:
        Input: 1->2->3->4->5->NULL
        Output: 1->3->5->2->4->NULL
    Example 2:
        Input: 2->1->3->5->6->4->7->NULL
        Output: 2->3->6->7->1->5->4->NULL

    Constraints:
    The relative order inside both the even and odd groups should remain as it was in the input.
    The first node is considered odd, the second node even and so on ...
    The length of the linked list is between [0, 10^4].
    '''
    def oddEvenList(self, head: ListNode) -> ListNode:
        p = head
        def print_list(p):
            i = 0
            while p:
                print( p.val, end=", ")
                i = i + 1
                p = p.next
                if i > 8:
                    break
            print("")
            print('-'*32)
            return

        if not head:
            return head

        odds_head = None
        odds_tail = odds_head
        evens_head = None
        evens_tail = evens_head

        i = 1
        while p:
            pp = p
            if i % 2 == 0:
                if i == 2:
                    evens_head = p
                    evens_tail = p
                else:
                    evens_tail.next = p
                    evens_tail = p

            if i % 2 == 1:
                if i == 1:
                    odds_head = p
                    odds_tail = p
                else:
                    odds_tail.next = p
                    odds_tail = p

            i = i + 1
            p = pp.next

        if odds_tail:
             odds_tail.next = None
        if evens_tail:
             evens_tail.next = None

        head = odds_head if odds_head else evens_head if evens_head else None

        if odds_tail:
            odds_tail.next = evens_head

        return head



        def brute_force():
            odds = []
            evens = []
            p = root
            i = 0
            head = None
            while p:
                if i % 2:
                    odds.append(p)
                else:
                    evens.append(p)
            if odds:
                head = odds[0]
            for i, p in enumerate(odds):
                p.next = odds[i+1] if i+1<len(odds) else (evens[0] if evens else None)

            for i, p in enumerate(evens):
                p.next = evens[i+1] if i+1<len(evens) else None
            return head




