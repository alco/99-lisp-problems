To the scripts, I use the following executable:

```shell
#!/bin/sh

CLOJURE=$CLASSPATH:/usr/local/Cellar/clojure/1.3.0/clojure-1.3.0.jar:/usr/local/Cellar/clojure-contrib/1.2.0/clojure-contrib.jar:${PWD}

BREAK_CHARS="\(\){}[],^%$#@\"\";:''|\\"

rlwrap --remember -c -b $BREAK_CHARS java -cp $CLOJURE clojure.main "$@"
```

`rlwrap`, `clojure`, and `clojure-contrib` can be installed with [homebrew](https://github.com/mxcl/homebrew).
