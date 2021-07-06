

# Build
```
npm run build
```

# Test

```
make test
```

Some tests are cribbed from the chialisp website and other info about
chialisp.  This isn't the authentic experience I hoped it'd be.  It
doesn't do any optimization just yet and takes a different approach
to a few things.  Still it's much more strict than the original and
is written with a stricter separation between preprocessing, frontend
and code generation in mind.  It should be able to barf on many more
errors up front than the main chialisp compiler.

Scopes are respected much more in this implementation than the original
so that accidentally using f or a as the name of a defun or argument
will work as expected.  Very few names are taken as special forms, but
"q", "qq" and "quote" definitely are.

This code does not share the original code's sharing of quoted string
and atom namespace.

I had hoped to implement let forms by this point, but it might wait
for another day.

This is very lightly tested code and might not produce correct output
for every valid chialisp program.

What is tested and working is in tests/test.ml

