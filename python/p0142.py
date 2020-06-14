# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, x):
#         self.val = x
#         self.next = None

class Solution:
    '''
    Given a linked list, return the node where the cycle
    begins. If there is no cycle, return null.
    To represent a cycle in the given linked list, we use
    an integer pos which represents the position (0-indexed)
    in the linked list where tail connects to. If pos is -1,
    then there is no cycle in the linked list.

    Note: Do not modify the linked list.

    Example 1:
    Input: head = [3,2,0,-4], pos = 1
    Output: tail connects to node index 1
    Explanation: There is a cycle in the linked list, where tail connects to the second node.
    '''
    def detectCycle(self, head: ListNode) -> ListNode:
        # print('-'*32)
        pos = -1
        cnt = 0
        visited = {}
        self.cycle = None
        def dfs(p, cnt, visited={}):
            if not p:
                return None
            # print(p.val, cnt)
            if p in visited:
                # print( "cycle", p.val, cnt, [p.val for p in visited] + [p.val])
                self.cycle = p
                return p
            visited[p] = cnt

            dfs(p.next, cnt+1, visited)
            return None

        dfs(head, 0, visited)
        if self.cycle is None:
            print("no cycle")
        return self.cycle

