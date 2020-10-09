# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    '''
    Given the head of a linked list, remove the nth node from the end of the list and
    return its head.  Follow up: Could you do this in one pass?
    '''
    def removeNthFromEnd(self, head: ListNode, n: int) -> ListNode:
        if n == 0:
            return head

        nodes = []
        p = head
        while p:
            nodes.append(p)
            p = p.next

        N = len(nodes)

        if n > N:
            return head
        elif n == N:
            prev, to_remove = None, 0
            head = nodes[to_remove].next
        else:
            prev, to_remove = -n-1, -n
            after = nodes[to_remove].next
            nodes[prev].next = after
        return head


