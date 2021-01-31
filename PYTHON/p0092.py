# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    """
    https://leetcode.com/problems/reverse-linked-list-ii/
    Reverse a linked list from position m to n. Do it in one-pass.
    Note: 1 ≤ m ≤ n ≤ length of list.
    """
    def reverseBetween(self, head: ListNode, m: int, n: int) -> ListNode:
        if not head: return head

        p, i, stack = head, 0, {}
        while p:
            i += 1
            stack[i] = p
            p = p.next

        N = len(stack)
        if N <= 1: return head

        for pos in range(n, m, -1):
            stack[pos].next = stack[pos-1]

        precedent = stack[m-1] if m-1 > 0 else None
        subsequent = stack[n+1] if n+1 <= N else None
        sub_head, tail = stack[n], stack[m]

        if precedent:
            precedent.next = sub_head
        else:
            head = sub_head

        if subsequent:
            tail.next = subsequent
        else:
            tail.next = None
        return head


