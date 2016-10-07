import sys

class TreeNode():
  def __init__(self, child_list, this_value):
    self.children = child_list
    self.value = this_value
    return

def dfs(node):
  print node.value
  for child in node.children:
    dfs(child)

node1 = TreeNode([], 1)
node2 = TreeNode([], 2)
node3 = TreeNode([], 3)
node4 = TreeNode([], 4)
node5 = TreeNode([], 5)

node6 = TreeNode([node1, node2], 6)
node7 = TreeNode([node6, node3], 7)

node8 = TreeNode([node4], 8)
node9 = TreeNode([node7, node5], 9)

node10 = TreeNode([node9, node8], 0)

dfs(node10)