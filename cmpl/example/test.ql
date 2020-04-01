; Here is a comment too
(def select-by-domain ; This is a comment
        (core-select-activity (where (pair "domain" :arg1)))
        (if (< 1 10)
            (block
                (print "Always"))
            (block 
                (print "never"))))

; Try a second command
(let varName .5)