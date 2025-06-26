# Declensia

## A library for pluralizing and singularizing

The api is very simple. `declensia.core/pluralize` changes a singular to a
plural, and `declensia.core/singularize` changes a plural to a singular.

For now, there are no guarantees about what happens when you pass a singular word to `singularize`; same for `pluralize`.

Everything is meant to be extensible, and there's a few options based on your needs. 

To do a one-off word change, you can define a new multimethod on `singularize` and `pluralize`, which both dispatch off the word itself. 

```clj
  (defmethod pluralize   "foobaria" [_] "foobar")
  (defmethod singularize "foobar"   [_] "foobaria")
```

You can also add rules with `add-rule`.  There are 3 varieties of rules, `:uncountable` `:irregular` and `:regular`.

These are named for how they are intended to be used, but only programmatic
difference is that `:uncountable` rules are considered first, `:irregular`
second, and `:regular` last. There are some warnings printed if you try to use
something else.

Here are some examples of rules 

```clj
(add-rule :plural   :irregular (rule "ox"     "oxen"))
(add-rule :singular :irregular (rule "oxen"   "ox"))
(add-rule :plural   :irregular (rule "person" "people"))
(add-rule :singular :irregular (rule "people" "person"))
```

