To run the scripts, I use the following executable. I have installed `rlwrap`,
`clojure`, and `clojure-contrib` using [homebrew](https://github.com/mxcl/homebrew).

```sh
#!/bin/sh

CLOJURE=$CLASSPATH:/usr/local/Cellar/clojure/1.3.0/clojure-1.3.0.jar
CLOJURE=$CLOJURE:/usr/local/Cellar/clojure-contrib/1.2.0/clojure-contrib.jar:${PWD}

BREAK_CHARS="\(\){}[],^%$#@\"\";:''|\\"

rlwrap --remember -c -b $BREAK_CHARS java -cp $CLOJURE clojure.main "$@"
```

