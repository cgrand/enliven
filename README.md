# enliven

Enliven: a continuous templating system

## Manifesto

Templates as we know them are part of a continuum. A template is at the intersection of presentation and data.

Usually once the template is rendered, the relationship between the state of an application 
and its UI is governed by a different piece of code.

A way to deal with this problem is to not server-side template and to do everything on the client.

Both solutions focus on only one end of the continuum: server-only and client-only.

Templates are easy because they don't carry state of their own, all state is explicit: 
it's the piece of data being rendered!

Enliven's proposition is to make writing HTML UI as simple as writing templates. No not just as simple but 
the same.

An Enliven template can be used both server-side and client-side without modification.

A page could be initialized server-side (no more loading artifacts or progress bars) and updated client-side with no code duplication.

A page could use the history API automatically.

## Concepts

### Transformations

Transformations are the basic building block of templates.

A transformation conceptually takes two arguments: a node and some data and returns an updated node.

A transformation is applied through the `transform` function which takes 3 args: the transformation, the node and the data.

Transformations are mashed together with `compose`. Composition is associative, commutative and idempotent.

Commutativity means that the order of composition is not important.

Associativity means that the nesting of composition is not important.

Idempotency means that repeating a transformation is not important.

`(content)` is a transformation which sets the content of the current node with the current data.

`(content :foo)` is a transformation which sets the content of the current node with whatever is under `:foo` in the current data.

`(content "p.foo" :foo)` is a transformation which sets the content of all descendants matching `p.foo` of the current node with whatever is under `:foo` in the current data.

FIXME

## License

Copyright © 2013 FIXME

Distributed under the Eclipse Public License, the same as Clojure.
