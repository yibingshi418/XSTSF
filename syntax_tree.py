import nltk
from nltk import Tree

# Define your syntactic structure
tree = Tree('S', 
            [Tree('NP', ['LaTeX']),
             Tree('VP', [Tree('V', ['is']),
                         Tree('NP', ['fun'])])])

# Draw the tree
tree.draw()
