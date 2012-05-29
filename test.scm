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

(use extra.daemon)
(test-module 'extra.daemon)

(test-end)
