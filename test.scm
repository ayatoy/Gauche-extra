(use gauche.test)

(test-start "extra")

(use extra.list)
(test-module 'extra.list)

(use extra.string)
(test-module 'extra.string)

(use extra.condition)
(test-module 'extra.condition)

(use extra.dev)
(test-module 'extra.dev)

(test-end)
