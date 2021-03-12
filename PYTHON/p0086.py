# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    """ https://leetcode.com/problems/partition-list/
    """
    def partition(self, head: ListNode, x: int) -> ListNode:
        pl = hless = ListNode() # dummy
        pm = hmore = ListNode() # dummy
        if not head: return head

        p = head
        while p:
            if p.val < x:
                node = ListNode(val=p.val)
                pl.next = node
                pl = node
            else:
                node = ListNode(val=p.val)
                pm.next = node
                pm = node
            p = p.next

        hless = hless.next
        hmore = hmore.next
        if hless and hmore:
            head = hless
            pl.next = hmore
        elif hless and not hmore:
            head = hless
        elif not hless and hmore:
            head = hmore
        else:
            head = None
        return head

