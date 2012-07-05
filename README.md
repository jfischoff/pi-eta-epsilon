
## Test Status:
     $ cabal test
     Running 2 test suites...
     Test suite doctests: RUNNING...
     Test suite doctests: PASS
     Test suite logged to: dist\test\pi-eta-epsilon-0.0.1-doctests.log
     Test suite tests: RUNNING...
     Test suite tests: PASS
     Test suite logged to: dist\test\pi-eta-epsilon-0.0.1-tests.log
     2 of 2 test suites (2 of 2 test cases) passed.


## Forward Isomorphisms

 d   |  a,b,c,d                | d  | I> | I---> | d   | a,b,c,d                          | d  | I> | letwhere          | pattern
 --- | ----------------------- | ---| ---| ----  | --- | ---------------------------      | ---| ---| ----              | ----
 <   |  iso        , v , C, s  | >  | I> |       | [   | iso, v' C, s'                    | ]  | I> | where iso s v     | (v', s')
 <   |  (:\+:) c1 c2, v', C, s | >  | I> |       | <   | c1, v , L\+ C c2    , s          | >  | I> |                   | [v' ~ left v]
 <   |  (:\+:) c1 c2, v', C, s | >  | I> |       | <   | c2, v , R\+ c1 C    , s          | >  | I> |                   | [v' ~ right v]
 <   |  (:\*:) c1 c2, v', C, s | >  | I> |       | <   | c1, v1, L\* C c2 v2, s'          | >  | I> | where s' = s      | [v  ~ (v1,v2)]
 [   |  c1, v1, L\* C c2, s    | ]  | I> |       | <   | c2, v2, R\* c1 v1 C, s           | >  | I> |                   | 
 <   |  (:\::) c1 c2, v , C, s | >  | I> |       | <   | c1, v , Fst C c2    , s          | >  | I> |                   |
 [   |  c1, v, Fst C c2, s     | ]  | I> |       | <   | c2, v , Snd c1 C    , s          | >  | I> |                   |
 [   |  c2, v, Snd c1 C, s     | ]  | I> |       | [   | (:\::) c1 c2, v, C  , s          | ]  | I> |                   |
 [   |  c1, v, L\+ C c2, s     | ]  | I> |       | [   | (:\+:) c1 c2, left v, C  , s     | ]  | I> |                   |
 [   |  c2, v, R\+ c1 C, s     | ]  | I> |       | [   | (:\+:) c1 c2, right v    , C , s | ]  | I> |                   |
 [   |  c2, v2, R\* c1 C, s    | ]  | I> |       | [   | (:\*:) c1 c2, tuple v1 v2, C , s | ]  | I> |                   |
 --- | ----------------------- | ---| ---| ----  | --- | ---------------------------      | ---| ---| ----              | ----
 d   |  a,b,c,d                | d  | I>  | I---> | d   | a,b,c,d                    | d  | I> | letwhere     | pattern
 --- | ----------------------- | ---| --- | ----  | --- | -------------------------- | ---| ---| ----         | ----
 <   | eps+, v, C, s           | >  | I>  |       | <   | eps+, left (-v') , C, s    | >  | < I|              | [v  ~ right  v'   ]
 <   | eps+, v, C, s           | >  | I>  |       | <   | eps+, right v'   , C, s    | >  | < I|              | [v  ~ left (-v')  ]
 <   | eta+, v, C, s           | >  | < I |       | <   | eta+, left (-v') , C, s    | >  | I> |              | [v  ~ right  v'   ]
 <   | eta+, v, C, s           | >  | < I |       | <   | eta+, right v'   , C, s    | >  | I> |              | [v  ~ left (-v')  ]

