# chemlambda-hask

## Description
chemlambda-hask is an implementation of the
[Chemlambda](https://chorasimilarity.github.io/chemlambda-gui/index.html)
graph rewriting system based on lambda calculus. Check out that link above to
learn about Chemlambda itself.

The original creator's, Marius Buliga, repository can be found
[here](https://github.com/chorasimilarity/chemlambda-gui).

chemlambda-hask is a set of tools for working with the Chemlambda system along
with an implemetation of it. It is comprised of a core library upon which a
graph rewrite system can be built, a chemistry implementing a small subset of
what is possible with the core library, following the standard Chemlambda rules
of graph rewrites, and a set of "language" modules for parsing .mol files (the
barebones syntax for Chemlambda molecules). The project is still under constant
development, so changes are to be expected along with new additions to what
Chemlambda is and how it is implemented.  

## The modules
### chemlambda-core
This is the core library/API for building rewrite systems. Using this module
along with chemlambda-chemistry, full graph rewriting systems, based on and
possibly deviating from the Chemlambda standard chemistry are possible. This is
to say that Core is a foundational, abstract library that can be used to construct
a multitude of rewrite systems, not just the standard Chemlambda one. 

The only thing hindering Core's ability to be completely abstract is the module
Chemlambda.Core.Node which specifies the vertices of graphs to be tethered to
the Chemlambda standard rather than being completely abstract and
user-definable. Core.Node may in the future be generalized beyond the vertices
(atoms) of Chemlambda to avoid this tethering. 

### chemlambda-chemistry
Because the chemlambda-hask project is mostly specific to Chemlambda, and not
rewrite systems in general, chemlambda-chemistry connects the abstract world
defined in chemlambda-core to a concrete implementation of a graph rewriting
system. In this module, the rewrite rules -- the chemistry -- of Chemlambda are
defined by specifying the patterns of vertices to look for in a graph (called
left patterns), the vertices with which to replace left patterns
(called right patterns), and the enzymes that find left patterns in a graph and
replace them with right patterns. There are other files in this module, but this
is the conceputal bulk of it.

### chemlambda-langauge
This component defines a parser for mol files and one for an enhanced mol syntax
that is currently a work in progress. In Chemlambda, molecules are encoded in text
files. Each line in a mol file represents an elementary molecule (a node) and its 
incoming and outgoing connections to other nodes in a molecule. Here's an example:

    L 1 1 2
    L 3 3 4
    A 2 4 5

You can learn more about the molecule encoding using the links above.

The MolParser interprets mol files, reading them into the chemlambda-hask evaluator
so that they can be computed.

There is some interest in developing an enhanced mol syntax that enables one to
abstract over repetitive, or otherwise meaningful, parts of a molecule. Perhaps you
have a section in a molecule that needs to be used in various places. This syntax
would allow you to create molecule abstractions that can be parameterized to "hotswap"
into multiple locations. Basically implementing molecule constructors. 


## TODO
### Immediate
- Make a testing suite for each set of modules
- Develop on the mol parser (Completed)

### Soon
- Generalize the rewrite system by making a Rewritable class that Actors and
  Graphs and maybe Graphs of Actors are instances

### Long Run
- Make chemlambda-hask-repl executable subproject
- Figure out what other subprojects would be helpful
- Build a parallel version using CUDA C.
