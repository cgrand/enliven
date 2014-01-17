# Enliven

Enliven: a (not yet) continuous templating system.

*WARNING* very wet paint

## Enliven is the successor to Enlive

Even dictionaries say so:

> Enlive: To Enliven (Obs.)

Enliven also stands for Enlive N(ext).

### Not tied to HTML

Currently Enliven can template plain text (`enliven.text`) and html (`enliven.html`).
The HTML "domain" is even composite since it uses the text "domain" to template text nodes.

### Simpler selectors

Selectors are now functions from loc to locs. Most domains should expose a `sel` function to coerce
a domain specific selector (eg a CSS-selector in string or a regex) to a selector fn.

### Parallel execution of transformations

All transformations occur at once for maximum declarativeness (and it enables good performance). It follows that two transformations can't work on the same node
or on a node and one of its ancestors.

This constraint is heavily mitigated by infinite-resolution selectors and transformation-refined selectors.

### Infinite-resolution selectors

Selectors don't stop at nodes as specified in the DOM. Any node can be subdivided at will!

For example classes in a `class` attribute can be targeted independently. Each character of a text node can be transformed independently.
You can append/conflict/prepend on an element without conflicts!

However this happens under the hood, see: 

```clj
(at 
  "div" (class "important" :important)
  "div" (class "footnote" :foot-note))
```

Those two transformations won't conflict because they refine their selector.

### Point-free

Templates take a single argument which is the data model to render.

Transformations don't take as arguments the actual values but keys or paths into the model.

```clj
=> ((static-template
     (enliven.html.jsoup/parse "<div>")
     "div" (content :sentence))
     {:sentence "Hello world"})
"<html><head></head><body><div>Hello world</div></body></html>"
```

Some transformations like `dup` introduces a new scope so that sub-transformation can only see
the current item.

```clj
=> ((static-template
     (enliven.html.jsoup/parse "<ul><li>")
     "li" (dup :todos 
            (content []))) ; [] is the empty path, so points to the whole value
     {:todos ["Laundry" "Walk the dog"]})
"<html><head></head><body><ul><li>Laundry</li><li>Walk the dog</li></ul></body></html>"
```

### It's fast

Generating a 5x5 table. (pr-str just dumps the 5*5 vector):
* pr-str (24.5µs), 
* enliven (24.8µs), 
* hiccup (44.8µs), 
* enlive (130µs), 
* laser (1280µs).

https://gist.github.com/cgrand/8471718

## Dev details

### Processing model

1. Nodes are selected
2. For each selected node, associated transformations are grounded -- this is where the nodes are refined to avoid conflicts. The result is a set of rules.
3. A hierarchical plan is created from the rules.
4. The plan can either be executed as is or "compiled".

## License

Copyright © 2014 Christophe Grand

Distributed under the Eclipse Public License, the same as Clojure.
