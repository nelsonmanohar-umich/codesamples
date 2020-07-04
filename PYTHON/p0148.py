# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    '''
    Sort a linked list in O(n log n) time using constant space complexity.
    Example 1:
        Input: 4->2->1->3
        Output: 1->2->3->4
    Example 2:
        Input: -1->5->3->4->0
        Output: -1->0->3->4->5
    '''
    def sortList(self, head: ListNode) -> ListNode:
        # using sort from auxiliary array
        p2v = {}
        p = head
        while p:
            p2v[p] = p.val
            p = p.next

        keys = sorted(list(p2v.keys()), key=lambda k: p2v[k])
        p, i = head, 0
        while p:
            p.val = p2v[keys[i]]
            p = p.next
            i += 1
        return head


    def print_list(self, p):
        while p:
            print(p.val, end="-> ")
        print("")


    def sortList_mergesort(self, head: ListNode) -> ListNode:
        # divide until n-2, the sort O(1)
        # merge the sorted halves (O(1) pointer movements
        def mergesort(nodes):
            n = len(nodes)
            if n == 0:
                return []
            elif n == 1:
                return nodes
            elif n == 2:
                p1, p2 = nodes
                return [p1, p2] if p1.val <= p2.val else [p2, p1]
            else:
                mi = n//2
                left = mergesort(nodes[:mi])
                right = mergesort(nodes[mi:])
                left.reverse()
                right.reverse()
                l, r = left.pop(), right.pop()
                merged = []
                while l and r:
                    if l.val == r.val:
                        merged.append(l)
                        merged.append(r)
                        l = left.pop() if left else None
                        r = right.pop() if right else None
                    elif l.val < r.val:
                        merged.append(l)
                        l = left.pop() if left else None
                    else: # l.val > r.val
                        merged.append(r)
                        r = right.pop() if right else None

                if r: merged.append(r)
                if l: merged.append(l)
                while right: merged.append(right.pop())
                while left: merged.append(left.pop())
                return merged

        p, nodes = head, []
        while p:
            nodes.append(p)
            p = p.next

        if nodes:
            nodes = mergesort(nodes)
            nodes.reverse()
            head = nodes.pop()
            p = head
            while nodes:
                pp = nodes.pop()
                p.next = pp
                pp.next = None
                p = pp

        return head


    def sortList_n2(self, head: ListNode) -> ListNode:
        # mege sort to be done after this selection sort
        p = head
        while p:
            pmin = p
            q = p.next
            while q:
                if q.val < pmin.val:
                    pmin = q
                q = q.next
            if pmin is not None:
                p.val, pmin.val = pmin.val, p.val
            p = p.next
        return head
